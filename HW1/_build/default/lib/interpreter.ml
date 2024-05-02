open Ast
open Environment
open Security
(* open Utils *)

(* --- INTERPRETER  --- *)

let rec eval (e: exp) (env: evT env) (stack : pstack) (t : bool) (block: block_level): evT * bool = 
  match e with
  | CstInt n -> (Int n, t)
  | CstFlt n -> (Float n, t)
  | CstBool b -> (Int (if b then 1 else 0), t)
  | Den e1 -> (lookup env e1, t_lookup env e1)
  | Let (x, conf_lev, eRhs, letBody) -> (
      match conf_lev, block with 
        | Secret, Untrusted -> failwith "Can't declare a secret in an Untrusted env"
        | Public, Trusted  -> failwith "A trusted block accepts only trusted data and trusted functions "
        | _ ->  let xVal, t1 = eval eRhs env stack t block in (* xVal: evT * bool != xVal, t1: evT, bool *)
                  let letEnv = (x, xVal, t1)::env in
                    eval letBody letEnv stack t1 block 
    )
  | SecLet (x, conf_lev, eRhs, secSet, letBody) -> (
      match conf_lev, block with 
        | Secret, Untrusted -> failwith "Can't declare a secret in an Untrusted env"
        | Public, Trusted  -> failwith "A trusted block accepts only trusted data and trusted functions "
        | _ ->  let xVal, t1 = eval eRhs env stack t block in (* xVal is evaluated in the current stack *)
                  let letEnv = (x, xVal, t1)::env in
                    let letStack = (Grant secSet)::stack in (* letBody is evaluated in the updated stack *)
                      eval letBody letEnv letStack t1 block 
    )
  | Prim (ope, e1, e2) -> (
      let v1, t1 = eval e1 env stack t block in
        let v2, t2 = eval e2 env stack t block in
          match (ope, v1, v2) with
            | "*", Int i1, Int i2 -> (Int (i1 * i2), t1 || t2)
            | "+", Int i1, Int i2 -> (Int (i1 + i2), t1 || t2)
            | "-", Int i1, Int i2 -> (Int (i1 - i2), t1 || t2)
            | "=", Int i1, Int i2 -> (Int (if i1 = i2 then 1 else 0), t1 || t2)
            | "<", Int i1, Int i2 -> (Int (if i1 < i2 then 1 else 0), t1 || t2)
            | ">", Int i1, Int i2 -> (Int (if i1 > i2 then 1 else 0), t1 || t2)
            | "&", Bool b1, Bool b2 -> (Bool (b1 && b2), t1 || t2)
            | "|", Bool b1, Bool b2 -> (Bool (b1 || b2), t1 || t2)
            (*ops for foat*)
            | "*", Float i1, Float i2 -> (Float (i1 *. i2), t1 || t2)
            | "+", Float i1, Float i2 -> (Float (i1 +. i2), t1 || t2)
            | "-", Float i1, Float i2 -> (Float (i1 -. i2), t1 || t2)
            | "=", Float i1, Float i2 -> (Int (if i1 = i2 then 1 else 0), t1 || t2)
            | "<", Float i1, Float i2 -> (Int (if i1 < i2 then 1 else 0), t1 || t2)
            | ">", Float i1, Float i2 -> (Int (if i1 > i2 then 1 else 0), t1 || t2)
            | _ -> failwith "unknown primitive or wrong type"  
    )
  | If (cond, e2, e3) -> (
      let v1, t1 = eval cond env stack t block in
        match v1 with
          | Bool true -> let v2, t2 = eval e2 env stack t1 block in (v2, t1 || t2)
          | Bool false -> let v3, t3 = eval e3 env stack t1 block in (v3, t1 || t3)
          | _ -> failwith "eval if" 
    )
  | Fun (f_param, f_body, secSet) -> (Closure (f_param, f_body, secSet, env), t)
  | Call (f, param) -> (
      let fClosure, f_t = eval f env stack t block in
        match fClosure, f_t  with (*CLOSURE: la funzione viene valutata solo se il valore di f_t è UNTAINED ,cioè FALSE*)
          | Closure (f_param, f_body, secSet, fDeclEnv),  false  -> (* xVal is evaluated in the current stack *)
              let xVal, t1 = eval param env stack t block in
                let fBodyEnv = (f_param, xVal, t1)::fDeclEnv in
                  let fBodyStack = (Grant secSet)::stack in (* fBody is evaluated in the updated stack *)
                    let f_res, t_res = eval f_body fBodyEnv fBodyStack t block in 
                      (f_res, t_res) (* NON HO PIÙ BISOGNO DI OR TRA I TAINTED VALUES *)
          | _ -> failwith "eval Call: not a function"
     )
    (*DemandPermission richiede solo di analizzare lo stack alla ricerca di quella permission, torna 1 o 0*)
  | DemandPermission p -> (Int (stackInspection inspectFun stack p), t)
    (*OnPermission associa il return di DemandPermission ad un'espressione: se true esegue l'exp se false non esegue *)
  | OnPermission (p, e) -> (
      let a1, t1 = eval (DemandPermission p) env stack t block in
        match a1 with
          | Int 1 -> eval e env stack t1 block
          | _ -> (Int 0, t1)
    )
    (*CheckPermission associa DemandPermission ad un errore di tipo espressione Abort*)   
  | CheckPermission p -> (
      let a1, t1 = eval (DemandPermission p) env stack t block in
        match a1 with
          | Int 1 -> (Int 1, t1)
          | _ -> eval (Abort "CheckPermission failed") env stack t1 block
    )
    (*Enable abilita il permesso prima di valutare l'espressione*)
  | Enable( p, e) -> eval e env (Enable p :: stack) t block
    (*Disable disabilita il permesso prima di valutare l'espressione*)
  | Disable (p, e) -> eval e env (Disable p :: stack) t block
    (* Evaluates the expression pushing the secAction on top of the stack *)
  | SecBlock (sec, e) -> eval e env (sec :: stack) t block
  | ReadFile f -> (
      let a1, t1 = eval (DemandPermission (Permission ("File", f, ["r"]))) env stack t block in
        match a1 with
          | Int 1 -> Int 1, t1
          |  _ -> eval (Abort ("No Read Permission for " ^ f)) env stack t1 block
    ) 
  | SendFile (e, f) -> (
      let a1, t1 = eval (DemandPermission (Permission ("File", f, [ "w" ]))) env stack t block in
        match a1 with
          | Int 1 -> eval e env stack t1 block(* do write *)
          | _ -> eval (Abort ("No Write Permission for " ^ f)) env stack t block
    )
  | GetInput(e) -> eval e env stack true block
  | Abort msg -> failwith msg

  (* 
                  --- (OLD FUNCTIONS) ---
  | IsZeroFloat(e1) -> is_zero_float(eval e1 env stack) (*<--*)
  | IsZeroInt(e1) -> is_zero_int(eval e1 env stack) (*<--*)
  | Times(e1, e2) -> int_mul((eval e1 env), (eval e2 env))
  | TimesF(e1, e2) -> float_mul((eval e1 env), (eval e2 env))
  | Sum(e1, e2) -> int_plus((eval e1 env), (eval e2 env))
  | SumF(e1, e2) -> float_plus((eval e1 env), (eval e2 env))
  | Sub(e1, e2) -> int_minus((eval e1 env), (eval e2 env))
  | SubF(e1, e2) -> float_minus((eval e1 env), (eval e2 env))
  | Div(e1, e2) -> int_div((eval e1 env), (eval e2 env))
  | DivF(e1, e2) -> float_div((eval e1 env), (eval e2 env))
  | Eq(e1, e2) -> int_eq((eval e1 env), (eval e2 env))
  | EqF(e1, e2) -> float_eq((eval e1 env), (eval e2 env))
  | And(e1, e2) -> bool_and((eval e1 env), (eval e2 env))
  | Or(e1, e2) -> bool_or((eval e1 env), (eval e2 env))
  | Not(e1) -> bool_not(eval e1 env)
  | Ifthenelse(cond, e2, e3) -> 
      (let g = eval cond env in
      match (typecheck("bool", g), g) with
        | (true, Bool(true)) -> eval e2 env
        | (true, Bool(false)) -> eval e3 env
        | (_, _) -> failwith ("Non-boolean guard"))
  *)