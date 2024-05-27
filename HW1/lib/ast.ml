
(* --- ABSTRACT SYNTAX TREE --- *) 

type ide = string

type trust = 
    | Trusted
    | Untrusted
    | BlockLvl

type conf =
    | Public
    | Private
    
type exp = CstInt of int
    | CstBool of bool
    | CstFlt of float
    | CstStr of string
    | Prim of ide * exp * exp
    | Den of ide
    | If of exp * exp * exp
    | Let of ide * conf * exp * exp 
    | Fun of ide * exp 
    | Call of exp * exp
    | Abort of string
    | GetInput of exp
    | TrustedBlock of ide * exp
    | EndTrustedBlock of ide
    | Handle of ide
    | Include of trust * ide * exp 
    | EndInclude
    | Execute of ide * exp
    | HandleCall of ide * exp
    | Empty