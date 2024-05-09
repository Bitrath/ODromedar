
(* --- ABSTRACT SYNTAX TREE --- *) 

type ide = string (* Questa riga definisce un nuovo tipo chiamato ide, 
   che rappresenta gli identificatori nel linguaggio. 
   Gli identificatori sono semplicemente delle stringhe. *)

(*
type trust = (* Qui viene definito un altro tipo, trust, che rappresenta 
   i livelli di fiducia nelle operazioni del programma. *)
    | Private
    | Public
*)
type trust = 
    | Trusted (* Trusted *)
    | Untrusted  (* Untrusted *)
    | BlockLvl


type conf =
    | Public
    | Private

type exp = CstInt of int
    | CstBool of bool
    | CstFlt of float
    | Prim of ide * exp * exp (* Prim: Rappresenta operazioni primitive con due 
                                operandi e un identificatore dell'operatore.*)
    | Den of ide
    | If of exp * exp * exp
    | Let of  ide  *  exp * exp 
            (* (n.b.) this interpreter won't handle recursion *)
    | Fun of ide (* list *) * exp 
    | Call of exp * exp (* list *) (* Call: Chiama una funzione con argomenti. *)
    | Abort of string
    | GetInput of exp (* taint source *)
    | TrustedBlock of ide * (exp) list * ide (* Definisce un blocco di espressioni 
       con un livello di fiducia specificato e una lista di espressioni, aggiunto ad una ide-> handle *)
(*
        (* Int Exps *)
    | Times of exp * exp
    | Div of exp * exp
    | Sum of exp * exp
    | Sub of exp * exp
    | Eq of exp * exp
    | IsZeroInt of exp
    (* Float Exps *)
    | TimesF of exp * exp
    | DivF of exp * exp
    | SumF of exp * exp
    | SubF of exp * exp
    | EqF of exp * exp
    | IsZeroFloat of exp
    (* Other Exps *)
    | Or of exp * exp
    | And of exp * exp
    | Not of exp
*)