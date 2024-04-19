(* --- ABSTRACT SYNTAX TREE --- *) 

type ide = string

type exp = CstInt of int
    | CstFlt of float
    | CstTrue
    | CstFalse
    | TimesF of exp * exp
    | Times of exp * exp
    | DivF of exp * exp
    | Div of exp * exp
    | SumF of exp * exp
    | Sum of exp * exp
    | SubF of exp * exp
    | Sub of exp * exp
    | EqF of exp*exp
    | Eq of exp * exp
    | IsZeroInt of exp
    | IsZeroFloat of exp
    | Or of exp * exp
    | And of exp * exp
    | Not of exp
    | Den of ide
    | Ifthenelse of exp * exp * exp
    | Let of ide * exp * exp
    (* (n.b.) this interpreter won't handle recursion *)
    | Fun of ide (* list *) * exp
    | Apply of exp * exp (* list *)