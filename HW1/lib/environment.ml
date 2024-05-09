open Ast

(* --- ENVIRONMENT --- *)

(* Empty Environment *)
let emptyenv = []

(* Non-Empty Environment *)
  (* env è un tipo parametrico che rappresenta un ambiente. Ogni elemento 
    dell'ambiente è una tripla contenente un identificatore (ide), un 
    valore generico ('a) e uno status di taint (booleano). *)
type 'a env = (ide * 'a * bool) list

(* Types handled from the environment *)
type evT = Int of int
    | Float of float
    | Bool of bool
    | String of string
    | Closure of ide * exp * evT env (* le Closure rappresentano funzioni:
       ide: rappresenta il nome dell'identificatore associato alla chiusura,
            e' l'identificatore della funzione.
       exp: e' il corpo della funzione, l'espressione che verra' valutata quando 
            la funzione viene chiamata.
       evT env: e' l'ambiente in cui la funzione e' stata definita. 
            Contiene i legami tra gli identificatori e i loro valori nel momento 
            della creazione della chiusura. *)
    | Unbound

(* lookup: Cerca un identificatore nell'ambiente e restituisce il valore associato. *)
let rec lookup env x = 
  match env with
    | [] -> failwith "Not Found"
    | (y, v, _)::r -> if x = y then v else lookup r x
  (* y -> ide, v -> valore associato a y, _ -> taintness (non importante ora) *)

(* clean_lookup: Cerca se c'è un trusted block già definito con quel nome *)
let rec clean_lookup env x =
  match env with
    | [] -> Int 0
    | (y, _, _)::r -> if x = y then Int 1 else clean_lookup r x (*????????*)

(* t_lookup: Restituisce lo status di taint di un identificatore nell'ambiente *)
let rec t_lookup env x =
  match env with
    | [] -> failwith "Not Found"
    | (y, _, t)::r -> if y = x then t else t_lookup r x

(* bind: Aggiunge un nuovo identificatore con valore nell'ambiente 
   ---> (adds a tuple = (string, element) ) *)
let bind env (x: string) (v: 'a) = (x, v)::env
(* env -> L'ambiente esistente, rappresentato come una lista di tuple.
   x -> L'identificatore da associare al valore nella nuova tupla.
   v -> Il valore da associare all'identificatore x nella nuova tupla.
   (x, v) -> Questo crea una nuova tupla contenente l'ide x e il valore v.
   ::env -> Utilizzando l'operatore ::, la nuova tupla viene concatenata alla testa 
      dell'ambiente esistente, creando cosi' un nuovo ambiente con il nuovo legame 
      identificatore-valore. *)

(* let rec block_lookup body blockEnv t eval = 
  match body with 
    | [] -> blockEnv,t
    | exp1::tail ->  match exp1 with
                | Let(x, eRhs, letbody ) ->   let xVal, t1 = eval eRhs blockEnv t Private in 
                                                let letEnv = (x, xVal, t1)::blockEnv in
                                                  let funVal, t2 = eval letbody letEnv t   Private  in
                                                    
                                                    block_lookup tail blockEnv t eval


                | _ -> failwith "NOTHING ELSE, JUST Let"  *)
