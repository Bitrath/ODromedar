open Ast

(* --- ENVIRONMENT --- *)

(* Empty Environment *)
let emptyenv = []

(* Non-Empty Environment *)
  (* The environment maps variables to
    pairs consisting of a value and taint status *)
type 'a env = (ide * 'a * bool)  list

(* Types handled from the environment *)
type evT = Int of int
    | Float of float
    | Bool of bool
    | String of string
    | Closure of ide * exp * evT env
    | Unbound

(* lookup: exhaustive search of an element into the environment *)
let rec lookup env x =
  match env with
    | [] -> failwith "Not Found"
    | (y, v, _)::r -> if x = y then v else lookup r x


    (*Vede se c'è un trusted block già definito con quel nome*)
let rec clean_lookup env x =
  match env with
    | [] -> Int 0
    | (y, _, _)::r -> if x = y then Int 1 else clean_lookup r x

(*taintnes check, if x y binding it returns the taint status*)
let rec t_lookup env x =
  match env with
    | [] -> failwith "Not Found"
    | (y, _, t)::r -> if y = x then t else t_lookup r x

(* bind: pushes a new tuple (string, element) into the environment *)
let bind env (x: string) (v: 'a) = (x, v)::env



(* le funzioni di un blocco trusted non sono connesse al nome specifico del blocco
   trusted, di blocco trusted ne eesiste uno SOLO*)

let rec checker body env t eval = 
  match body with
  | [] -> Int 1
  | exp1::tail-> match exp1 with 
                      | Let(_,_,_,_) -> = eval exp1 env t Trusted :: checker tail env t eval

                      | _ ->  failwith "ONLY LET FUNCTIONS ARE ALLOWED"







(*
let rec block_lookup body blockEnv t eval = 
  match body with 
    | [] -> blockEnv,t
    | exp1::tail ->  match exp1 with
                | Let(x, eRhs, letbody ) ->   let xVal, t1 = eval eRhs blockEnv t Private in 
                                                let letEnv = (x, xVal, t1)::blockEnv in
                                                  let funVal, t2 = eval letbody letEnv t   Private  in
                                                    
                                                    block_lookup tail blockEnv t eval


                | _ -> failwith "NOTHING ELSE, JUST Let"  

let rec checker b blockEnv taintValue = 
                                    match b with 
                                      | [] -> blockEnv
                                      | exp1::tail -> match exp1 with
                                            | Let(letIde, letArgs, letBody) ->  (
                                                let ideVal, ideTaint = eval letArgs blockEnv taintValue in 
                                                let updEnv = (letIde, ideVal, ideTaint)::blockEnv in 
                                                let bodyVal, bodyTaint = eval letBody updEnv ideTaint in 
                                                checker tail updEnv bodyTaint )
                                            | _ -> []*)