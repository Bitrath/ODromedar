open Ast

(* --- ENVIRONMENT --- *)

(* Empty Environment *)
let emptyenv = []

(* Non-Empty Environment *)
type 'a env = (ide * 'a * bool) list

type evT = Int of int
    | Float of float
    | Bool of bool
    | String of string
    | Closure of ide * exp * evT env
    | ClosureTrustedBlock of evT env
    | HandleFlag of ide 
    | ClosureInclude of trust * exp
    | ExecuteCheck of evT env
    | Env of evT env
    | Unbound

(* lookup: Cerca un identificatore nell'ambiente e restituisce il valore associato. *)
let rec lookup env x = 
  match env with
    | [] -> failwith ("LOOKUP Error: (" ^ x ^ ") not found.")
    | (y, v, _)::r -> if x = y then v else lookup r x

(* clean_lookup: Cerca se c'è un trusted block già definito con quel nome *)
let rec clean_lookup env x =
  match env with
    | [] -> Int 0
    | (y, _, _)::r -> if x = y then (Int 1) else clean_lookup r x

(* t_lookup: Restituisce lo status di taint di un identificatore nell'ambiente *)
let rec taint_lookup env x =
  match env with
    | [] -> failwith ("TAINT-LOOKUP Error: (" ^ x ^ ") not found.")
    | (y, _, t)::r -> if y = x then t else taint_lookup r x



(* DA TOGLIERE*)

