open Ast
open Environment
(* open Utils *)

(* --- INTERPRETER  --- *)

let rec eval (e: exp) (env: evT env) (t : bool) (* (sec_lev : trust) *) : evT * bool = 
  match e with
  | CstInt n -> (Int n, t)            
  | CstFlt n -> (Float n, t)
  | CstBool b -> (Int (if b then 1 else 0), t)
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

  | Let (x, eRhs, letBody) -> 
     let xVal, t1 = eval eRhs env t in (* xVal: evT * bool != xVal, t1: evT, bool *)
      let letEnv = (x, xVal, t1)::env in
        eval letBody letEnv t1  
  | Prim (ope, e1, e2) -> (
      let v1, t1 = eval e1 env t in
        let v2, t2 = eval e2 env t in
          match (ope, v1, v2) with
            | "*", Int i1, Int i2 -> (Int (i1 * i2), t1 || t2)
            | "+", Int i1, Int i2 -> (Int (i1 + i2), t1 || t2)
            | "-", Int i1, Int i2 -> (Int (i1 - i2), t1 || t2)
            | "=", Int i1, Int i2 -> (Int (if i1 = i2 then 1 else 0), t1 || t2)
            | "<", Int i1, Int i2 -> (Int (if i1 < i2 then 1 else 0), t1 || t2)
            | ">", Int i1, Int i2 -> (Int (if i1 > i2 then 1 else 0), t1 || t2)
            | "&", Bool b1, Bool b2 -> (Bool (b1 && b2), t1 || t2)
            | "|", Bool b1, Bool b2 -> (Bool (b1 || b2), t1 || t2)
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
      let v1, t1 = eval cond env t in
        match v1 with
          | Bool true -> let v2, t2 = eval e2 env  t1  in (v2, t1 || t2)
          | Bool false -> let v3, t3 = eval e3 env  t1  in (v3, t1 || t3)
          | _ -> failwith "eval if" 
    )
  | Fun (f_param, f_body) -> (Closure (f_param, f_body, env), t)
(* --- [ >:( ] --- *)
  | Call (f, param) -> ( (* Call -> chiamata a funzione f *)

      let fClosure, f_t = eval f env t in 
      (* Questa riga valuta l'espressione f (che rappresenta la funzione da chiamare)
        nell'ambiente corrente env con il flag di taintedness t.
        Restituisce due valori: fClosure -> rappresenta la chiusura della 
        funzione (se f è una funzione) e f_t -> rappresenta il flag di 
        taintedness associato a f. *)

        match fClosure, f_t  with (* fClosure -> funzione insieme all'ambiente in 
                                    cui e' stata definita *)

            (* f_param -> parametro formale funzione, f_body -> corpo della funzione, 
              fDeclEnv -> ambiente in cui la funzione e' stata definita *)
          | Closure (f_param, f_body, fDeclEnv),  false  -> 
          (* CLOSURE: la funzione viene eseguita solo se il valore di f_t è UNTAINTED *) 
            (* xVal is evaluated in the current stack *)
              let xVal, t1 = eval param env t in (* param (che rappresenta gli argomenti 
                della funzione) viene valutato nell'ambiente corrente env con il flag di
                taintedness t, restituendo xVal come valore dell'argomento e t1 come flag 
                di taintedness associato all'argomento. *)
                let fBodyEnv = (f_param, xVal, t1)::fDeclEnv in
                (* costruzione di un nuovo ambiente fBodyEnv estendendo l'ambiente 
                  (concatenazione) dichiarativo fDeclEnv con il valore dell'argomento 
                  xVal associato al parametro formale f_param. Il flag di taintedness 
                  associato all'argomento t1 viene mantenuto per tenere traccia della 
                  "contaminazione" dei valori. *)
                  let f_res, t_res = eval f_body fBodyEnv t in 
                    (f_res, t_res) (* NON HO PIU' BISOGNO DI OR TRA I TAINTED VALUES *)
          | _ -> failwith "eval Call: not a function"
    )
  | GetInput e -> eval e env true 
  | Abort msg -> failwith msg
  | TrustedBlock(id, body) -> 
    (* We search for the Block Identifier *)
    (* If available, exit the Block, it already exists!!! *)
    (* -> Not available: Then insert it into the current env and continue with the evaluation of the body *)
    let resultId = clean_lookup env id in 
      match resultId, t with
                                    (* I TRUSTED BLOCK SONO SEMPRE UNTAINTED -> FALSE *)           
        | (_ , _) -> failwith "NO"
  
  | _ -> failwith "Pattern not matched"