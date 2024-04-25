open Ast
open Environment
open Utils
open Security

(* --- INTERPRETER  --- *)


(*eval ritorna un valore evT (: evT)*)
let rec eval (e: exp) (env: evT env) (stack : pstack) : evT = 
  match e with
  | CstInt(n) -> Int(n)
  | CstFlt(n) -> Float(n)
  | CstBool(b) -> Int(if b then 1 else 0)
  | Den(e1) -> lookup env e1
  | Let (x, eRhs, letBody) ->
      let xVal = eval eRhs env stack in
      let letEnv = (x, xVal) :: env in
      eval letBody letEnv stack
  | SecLet (x, eRhs, secSet, letBody) ->
      (* xVal is evaluated in the current stack *)
      let xVal = eval eRhs env stack in
      let letEnv = (x, xVal) :: env in
      let letStack = Grant secSet :: stack in
      (* letBody is evaluated in the updated stack *)
      eval letBody letEnv letStack

  | Prim (ope, e1, e2) -> (
      let v1 = eval e1 env stack in
      let v2 = eval e2 env stack in
      match (ope, v1, v2) with
      | "*", Int i1, Int i2 -> Int (i1 * i2)
      | "+", Int i1, Int i2 -> Int (i1 + i2)
      | "-", Int i1, Int i2 -> Int (i1 - i2)
      | "=", Int i1, Int i2 -> Int (if i1 = i2 then 1 else 0)
      | "<", Int i1, Int i2 -> Int (if i1 < i2 then 1 else 0)
      | "&", Bool b1, Bool b2 -> Bool (b1 && b2)
      | "|", Bool b1, Bool b2 -> Bool (b1 || b2)
        (*ops for foat*)
      | "*", Float i1, Float i2 -> Float (i1 *. i2)
      | "+", Float i1, Float i2 -> Float (i1 +. i2)
      | "-", Float i1, Float i2 -> Float (i1 -. i2)
      | "=", Float i1, Float i2 -> Int (if i1 = i2 then 1 else 0)
      | "<", Float i1, Float i2 -> Int (if i1 < i2 then 1 else 0)
      | _ -> failwith "unknown primitive or wrong type")
  | If (cond, e2, e3) -> (
      match eval cond env stack with
      | Int 0 -> eval e3 env stack
      | Int _ -> eval e2 env stack
      | _ -> failwith "eval if")
  | Fun (x, fBody, secSet) -> Closure (x, fBody, secSet, env)
  | Call (eFun, eArg) -> (
      let fClosure = eval eFun env stack in
         match fClosure with
          | Closure (x, fBody, secSet, fDeclEnv) ->
              (* xVal is evaluated in the current stack *)
              let xVal = eval eArg env stack in
              let fBodyEnv = (x, xVal) :: fDeclEnv in
              let fBodyStack = Grant secSet :: stack in

              (* fBody is evaluated in the updated stack *)
              eval fBody fBodyEnv fBodyStack
          | _ -> failwith "eval Call: not a function")

  (*DemandPermission richiede solo di analizzare lo stack alla ricerca di quella permission, torna 1 o 0*)
  | DemandPermission p -> Int (stackInspection inspectFun stack p) 

  (*OnPermission associa il return di DemandPermission ad un'espressione: se true esegue l'exp se false non esegue *)
  | OnPermission (p, e) ->
      if eval (DemandPermission p) env stack = Int 1 then eval e env stack
      else Int 0

   (*CheckPermission associa DemandPermission ad un errore di tipo espressione Abort*)   
  | CheckPermission p ->
      if eval (DemandPermission p) env stack = Int 1 then Int 1
      else eval (Abort "CheckPermission failed") env stack 


  | Abort msg -> failwith msg

(*Enable abilita il permesso prima di valutare l'espressione*)
  | Enable( p, e) -> eval e env ( Enable p :: stack)

(*Disable disabilita il permesso prima di valutare l'espressione*)
  | Disable (p, e) -> eval e env (Disable p :: stack)

(* Evaluates the expression pushing the secAction on top of the stack *)
  | SecBlock (sec, e) -> eval e env (sec :: stack)

  | ReadFile f ->
    if
      eval (DemandPermission (Permission ("File", f, [ "r" ]))) env stack
      = Int 1
    then Int 1 (* do read *)
    else eval (Abort ("No Read Permission for " ^ f)) env stack
  | SendFile (e, f) ->
      if
        eval (DemandPermission (Permission ("File", f, [ "w" ]))) env stack
        = Int 1
      then eval e env stack (* do write *)
      else eval (Abort ("No Write Permission for " ^ f)) env stack
  | Abort msg -> failwith msg

  (*
  | IsZeroFloat(e1) -> is_zero_float(eval e1 env stack) (*<--*)
  | IsZeroInt(e1) -> is_zero_int(eval e1 env stack) (*<--*)
*)
(*
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
 *)

 (*
 | Ifthenelse(cond, e2, e3) -> 
  (let g = eval cond env in
   match (typecheck("bool", g), g) with
    | (true, Bool(true)) -> eval e2 env
    | (true, Bool(false)) -> eval e3 env
    | (_, _) -> failwith ("Non-boolean guard"))
  *)