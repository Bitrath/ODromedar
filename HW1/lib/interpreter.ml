open Ast
open Environment
(* open Utils *)

(* --- INTERPRETER  --- *)

let rec eval (e: exp) (env: evT env) (taint: bool) (sec_lev: trust) : evT * bool = 
  match e with
  | CstInt n -> (Int n, taint)            
  | CstFlt n -> (Float n, taint)
  | CstBool b -> (Int (if b then 1 else 0), taint)
  | CstStr s -> (String s, taint)
  | Den id -> (lookup env id, t_lookup env id)
  (* IL BODY E' UN LISTA DI ESPRESSIONI *)
(*
  | LetSec (x, eRhs, letBody) ->(
      
      let xVal, t1 = eval eRhs env t in (* xVal: evT * bool != xVal, t1: evT, bool *)
          match t1 with

                          (* CONTROLLO SE IL VALORE È UNTAINED PRIMA DI AGGIUNGERLO ALL'ENVIRONMENT *)
            | false -> let letEnv = (x, xVal, t1)::env in
                      eval letBody letEnv t1
            | true -> failwith "Cannot add tainted data to TrustedBlock"
    
  )
  *)
  | Let (x, conf, eRhs, letBody) -> ( 
      match sec_lev, conf with
        | Untrusted, Private -> failwith "Can't make a private declaration in a unstrusted enviroment"    
        | Trusted, Public ->failwith "Can't make a public declaration in a trusted enviroment "
        | _ -> let xVal, t1 = eval eRhs env taint sec_lev in
            let letEnv = (x, xVal, t1)::env in
              eval letBody letEnv t1 sec_lev  
            )
  | Prim (ope, e1, e2) -> (
      let v1, t1 = eval e1 env taint sec_lev in
        let v2, t2 = eval e2 env taint sec_lev in
          match (ope, v1, v2) with
            | "*", Int i1, Int i2 -> (Int (i1 * i2), t1 || t2)
            | "+", Int i1, Int i2 -> (Int (i1 + i2), t1 || t2)
            | "-", Int i1, Int i2 -> (Int (i1 - i2), t1 || t2)
            | "=", Int i1, Int i2 -> (Int (if i1 = i2 then 1 else 0), t1 || t2)
            | "<", Int i1, Int i2 -> (Int (if i1 < i2 then 1 else 0), t1 || t2)
            | ">", Int i1, Int i2 -> (Int (if i1 > i2 then 1 else 0), t1 || t2)
            | "&", Bool b1, Bool b2 -> (Bool (b1 && b2), t1 || t2)
            | "|", Bool b1, Bool b2 -> (Bool (b1 || b2), t1 || t2)
            (* e le stringhe?? manca il compare e le altre operazioni... *)
            (* ops for float *)
            | "*", Float i1, Float i2 -> (Float (i1 *. i2), t1 || t2)
            | "+", Float i1, Float i2 -> (Float (i1 +. i2), t1 || t2)
            | "-", Float i1, Float i2 -> (Float (i1 -. i2), t1 || t2)
            | "=", Float i1, Float i2 -> (Int (if i1 = i2 then 1 else 0), t1 || t2)
            | "<", Float i1, Float i2 -> (Int (if i1 < i2 then 1 else 0), t1 || t2)
            | ">", Float i1, Float i2 -> (Int (if i1 > i2 then 1 else 0), t1 || t2)
            | _ -> failwith "unknown primitive or wrong type"  
    )
  | If (cond, e2, e3) -> (
      let v1, t1 = eval cond env taint sec_lev in
        match v1 with
          | Bool true -> let v2, t2 = eval e2 env  t1 sec_lev  in (v2, t1 || t2)
          | Bool false -> let v3, t3 = eval e3 env  t1 sec_lev  in (v3, t1 || t3)
          | _ -> failwith "eval if" 
    )
  | Fun (f_param, f_body) -> (Closure (f_param, f_body, env), taint)
(* --- [ >:( ] --- *)
  | Call (f, param) -> ( (* Call -> chiamata a funzione f *)

      let fClosure, f_t = eval f env taint sec_lev in 
      (* Questa riga valuta l'espressione f (che rappresenta la funzione da chiamare)
        nell'ambiente corrente env con il flag di taintedness t.
        Restituisce due valori: fClosure -> rappresenta la closure della 
        funzione (se f è una funzione) e f_t -> rappresenta il flag di 
        taintedness associato a f. *)

        match fClosure, f_t  with (* fClosure -> funzione insieme all'ambiente in 
                                    cui e' stata definita *)

            (* f_param -> parametro formale funzione, f_body -> corpo della funzione, 
              fDeclEnv -> ambiente in cui la funzione e' stata definita *)
          | Closure (f_param, f_body, fDeclEnv),  false  -> 
          (* CLOSURE: la funzione viene eseguita solo se il valore di f_t è UNTAINTED *) 
            (* xVal is evaluated in the current stack *)
              let xVal, t1 = eval param env taint sec_lev in (* param (che rappresenta gli argomenti 
                della funzione) viene valutato nell'ambiente corrente env con il flag di
                taintedness t, restituendo xVal come valore dell'argomento e t1 come flag 
                di taintedness associato all'argomento. *)
                let fBodyEnv = (f_param, xVal, t1)::fDeclEnv in
                (* costruzione di un nuovo ambiente fBodyEnv estendendo l'ambiente 
                  (concatenazione) dichiarativo fDeclEnv con il valore dell'argomento 
                  xVal associato al parametro formale f_param. Il flag di taintedness 
                  associato all'argomento t1 viene mantenuto per tenere traccia della 
                  "contaminazione" dei valori. *)
                  let f_res, t_res = eval f_body fBodyEnv taint sec_lev in 
                    (f_res, t_res) (* NON HO PIU' BISOGNO DI OR TRA I TAINTED VALUES *)
          | _ -> failwith "eval Call: not a function"
    )
  | GetInput e -> eval e env true sec_lev
  | Abort msg -> failwith msg
  | TrustedBlock(id, body) -> 
    (* We search for the Block Identifier *)
    (* If available, exit the Block, it already exists!!! *)
    (* -> Not available: Then insert it into the current env and continue with the evaluation of the body *)
        if (clean_lookup env id) = Int 1 then 
            failwith (id ^ " is already declared as an trustedBlock")
        else if sec_lev = BlockLvl then
            failwith ("can't declare a trustedBlock inside another one")
        else if sec_lev = Untrusted then
            failwith ("can't declare a trustedBlock inside of untrusted code")
        else (
            let newBlockEnv = [] in 
              let bodyRes, evalTaint = eval body newBlockEnv taint BlockLvl in 
              (* valutazione del corpo del trusted block (body) all'interno del
               nuovo ambiente (newBlockEnv). La funzione eval e' utilizzata per 
               valutare il corpo del trusted block. Viene passato l'ambiente 
               newBlockEnv, il livello di contaminazione attuale taint e il livello 
               di sicurezza BlockLvl.
               bodyRes -> valutazione del body del trusted block
               evalTaint -> taint status del body del trusted block *)
                match bodyRes with 
                  | ClosureTrustedBlock(_) -> ( if evalTaint = true then failwith "tainted." 
                      else 
                        let extEnv = (id, bodyRes, evalTaint)::env in 
                          (* Se il risultato del corpo del blocco è una closure associata a un blocco fidato 
                          e il livello di taintedness non e' true, allora viene creato un nuovo environment
                          extEnv aggiungendo il trusted block corrente all'ambiente esistente env. *)
                          eval (EndTrustedBlock(id)) extEnv evalTaint BlockLvl
                          (* Viene chiamata ricorsivamente la funzione eval con EndTrustedBlock(id)
                           per segnalare la fine del blocco fidato corrente, passando il nuovo ambiente
                           extEnv, il taint status risultante evalTaint e il livello di sicurezza BlockLvl. *)
                    )
                  | _ -> failwith "The Block ended with an expression different than an Handle. Error."
          ) 
    | Handle (ideFun) -> (
      (* cerchiamo il nome della handle fun *)
        if sec_lev != BlockLvl then failwith "Cannot declare a Handle Function outside the Trusted Block"
        else
        let res, resTaint = eval (Den(ideFun)) env taint sec_lev in (* guardiamo se l'ide della handle function è presente nell'env del block*)
          match res with 
            | Closure(_) -> if resTaint = true then failwith "tainted." 
                 else (ClosureTrustedBlock(env), resTaint)
            | _ -> failwith "no handle fun name..."
      ) 
    | EndTrustedBlock(i) -> (
       (* estraiamo il trusted block... *)
       let res, resT = eval (Den(i)) env taint sec_lev in
        match res with
         | ClosureTrustedBlock(envBlock) -> (
            let rec extractHandle env : ide = (
              (* Viene definita una funzione extractHandle che ricorsivamente 
                esamina l'ambiente del blocco fidato per estrarre il nome della
                funzione di gestione. Questo viene fatto cercando tra le coppie di
                variabili e valori dell'ambiente e selezionando il nome della funzione
                di gestione quando viene trovata una chiusura associata ad essa. *)
              match env with 
                | [] -> "None"
                | (fIde, fEvt, _) :: tail -> ( match fEvt with 
                (* fIde -> identificatore della variabile e fEvt -> valore. *)
                  | Closure(_) -> (fIde)
                  (*  Se fEvt = closure, la funzione restituirà l'ide della variabile fIde,
                   che rappresenta il nome della funzione di gestione associata a quella closure. *)
                  | _ -> extractHandle tail )
            ) in 
              let hName = extractHandle envBlock in
                 if hName = "None" then failwith "No Handle in TrustedBlock. Error." 
                    else
                      let newEnv = (hName, HandleFlag(i), resT)::env in 
                      (* creato un nuovo ambiente newEnv aggiungendo una nuova entry 
                        all'ambiente corrente env (tramite la concatenazione ::). 
                        Questa nuova entry rappresenta l'associazione tra il nome della
                        funzione di gestione hName, un flag HandleFlag(i) che indica 
                        che questa è una funzione di gestione e il taint status 
                        risultante resT. *)
                        (Env(newEnv), resT)
         )
         | _ -> failwith "No ClosureTrustedBlock. Error."
    )
    | Include(trst, id, pluginCode, incBody) -> (Int 0, true)
        (* match sec_lev with 
          | BlockLvl -> failwith "can't include a plugin inside of a trusted block"
          | _ -> let pluginResult, t1 = eval pluginCode env taint sec_lev in
                let env' =
                  match id with
                    | "" -> env
                    | _ -> (id, pluginResult, t1) :: env
                in eval incBody env' t1 sec_lev *)
    | Exec(exName, exBody) -> (Int 0, true)
    | _ -> failwith "fail: error include"

(* 
  La nostra handle e' implicitamente definita quando chiamiamo il trustedBlock. 
  In questo modo abbiamo risolto il bisogno di definire la keyword "Handle"
*)

(* let newBlockEnv = [] in
              
                (*funzione ausiliaria per valutare il contenuto del trust block*)
  
                let rec evalBody bodyContent accBlockEnv =

                  match bodyContent with
                    | [] -> accBlockEnv (*Quando ri esauriscono i contenuti, ritorna l'mbiente accumulato*)
                    | exp1 :: tail -> (

                        let ret, t1 = eval exp1 accBlockEnv t sec_lev in
                        match  ret, t1 with

                          | _ , false  -> evalBody tail accBlockEnv 
                          | _ -> failwith "TRUSTED BLOCK HAS BEEN  DECLARED WRONG"
                        
                         ) in 

                    let handleEnv = evalBody body  newBlockEnv in

                    let handleClosure =
                      let result = lookup handleEnv handle_name in
                        match result with 
                          | Closure( _ , handleExp, handleEnv ) -> (handleExp, handleEnv)

                          | _ -> failwith "not found"

                        in
                        
                        let handleRes, t_res = eval (fst handleClosure) (snd handleClosure) t sec_lev in
                          (handleRes, t_res) *)

(* TrustedBlock (ide * expr * expr) -> Fun(...) -> Closure(ide, ...) -> ... -> Handle(ideFun) -> (..)::envPriv
                                                                      (ideHandle, FlagHandle(ideRefEnv), taint)::(ideTrustB, ClosureEnvPriv(envPriv), taint)::env
   let trustBlockOne(:ide) = 
      trust {
        body(:exp) 
      }

body(:exp) =
    let exp1 = in 
          (...) in 
    handle funName;; -> Handle( ide, exp )


    ENV Principale: (ide * evT * taint) list
                    (nomeHandle * evT: Handle * taint)

    ENV 
*)

(*
   
Enclave ide * exp(tutto)* exp(include)







*)