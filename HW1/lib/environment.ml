open Ast
open Security
(* --- ENVIRONMENT --- *)

(* Empty Environment *)
let emptyenv = []

(* Non-Empty Environment *)
(*The environment maps variables to
pairs consisting of a value and taint status*)
type 'a env = (ide * 'a * bool )  list

(* Types handled from the environment *)
type evT = Int of int
        | Float of float
        | Bool of bool
        | Closure of ide * exp * pdomain * evT env
        | Unbound

(* lookup: exhaustive search of an element into the environment *)
let rec lookup env x =
  match env with
    | [] -> failwith("Not Found")
    | (y, v,_)::r -> if x = y then v else lookup r x

(*taintnes check, if x y binding it returns the taint status*)
let rec t_lookup env x =
  match env with
    | [] -> failwith("Not Found")
    | (y,_,t)::r -> if y=x then t else t_lookup r x


(* bind: pushes a new tuple (string, element) into the environment *)
let bind env (x: string) (v: 'a) = (x, v)::env