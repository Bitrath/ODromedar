open Ast

(* --- ENVIRONMENT --- *)

(* Empty Environment *)
let emptyenv = []

(* Non-Empty Environment *)
type 'a env = (ide * 'a) list

(* Types handled from the environment *)
type evT = Int of int
        | Float of float
        | Bool of bool
        | Closure of ide * exp * evT env
        | Unbound

(* lookup: exhaustive search of an element into the environment *)
let rec lookup env x =
  match env with
    | [] -> failwith("Not Found")
    | (y, v)::r -> if x = y then v else lookup r x

(* bind: pushes a new tuple (string, element) into the environment *)
let bind env (x:string) (v:'a) = (x,v)::env