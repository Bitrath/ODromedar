open Ast
open Environment
open Utils

(* --- INTERPRETER  --- *)

let rec eval(e:exp) (env: evT env ) =
  match e with
  | CstInt(n) -> Int(n)
  | CstFlt(n) -> Float(n)
  | CstTrue -> Bool(true)
  | CstFalse -> Bool(false)
  | IsZeroFloat(e1) -> is_zero_float(eval e1 env)
  | IsZeroInt(e1) -> is_zero_int(eval e1 env)
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
  | Den(e1) -> lookup env e1
  | Ifthenelse(cond, e2, e3) -> 
          (let g = eval cond env in
           match (typecheck("bool", g), g) with
            | (true, Bool(true)) -> eval e2 env
            | (true, Bool(false)) -> eval e3 env
            | (_, _) -> failwith("Non-boolean guard"))
  | Let(i, e, ebody) -> eval ebody(bind env i (eval e env))
  | Fun(i, a) -> Closure(i, a, env)
  | Apply(Den(f), eArg) ->
    (let fclosure = lookup env f in
      match fclosure with
        | Closure(arg, fbody, fDecEnv) ->
          let aVal = eval eArg env in
          let aenv = bind fDecEnv arg aVal in
            eval fbody aenv
        | _ -> failwith("Non functional value"))
  | Apply(_, _) -> failwith("Application: not first order function")