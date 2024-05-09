(* --- ABSTRACT SYNTAX TREE --- *) 

type ide = string



type trust = 
    | Private
    | Public

type exp = CstInt of int
    | CstBool of bool
    | CstFlt of float
    | Prim of ide * exp * exp
    | Den of ide
    | If of exp * exp * exp
    | Let of  ide  *  exp * exp 
            (* (n.b.) this interpreter won't handle recursion *)
    | Fun of ide (* list *) * exp 
    | Call of exp * exp (* list *) 
    | Abort of string
    | GetInput of exp (* tain source*)
    | TrustedBlock of ide * (exp) list 
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