
open Security

(* --- ABSTRACT SYNTAX TREE --- *) 

type ide = string

(*implementazione blocco TRUSTED e UNTRUSTED*)

type block_level =
    | Trusted 

    | Untrusted


type confidentiality =
    | Secret
 
    | Public
 

type exp = CstInt of int
    | CstBool of bool
    | CstFlt of float
    (* Int Exps *)
(*
    | Times of exp * exp
    | Div of exp * exp
    | Sum of exp * exp
    | Sub of exp * exp
    | Eq of exp * exp
    | IsZeroInt of exp
*)


    (* Float Exps *)
(*
    | TimesF of exp * exp
    | DivF of exp * exp
    | SumF of exp * exp
    | SubF of exp * exp
    | EqF of exp * exp
    | IsZeroFloat of exp
*)
    (* Other Exps *)

(*
    | Or of exp * exp
    | And of exp * exp
    | Not of exp
*)
    | Prim of ide * exp * exp
    | Den of ide
    | If of exp * exp * exp

    | Let of ide * confidentiality *  exp * exp

    | SecLet of ide * confidentiality * exp * pdomain * exp
    (* (n.b.) this interpreter won't handle recursion *)
    | Fun of ide (* list *) * exp * pdomain
    | Call of exp * exp (* list *)
    | DemandPermission of permission
    | OnPermission of permission * exp
    | CheckPermission of permission
    | Abort of string
    | Enable of permission * exp
    | Disable of permission * exp
    (* Evaluates the expression pushing the secAction on top of the stack *)
    | SecBlock of secAction * exp
    (* Reads a file iff is allowed otherwise aborts *)
    | ReadFile of string
     (* Send and evaluates expr to a file iff is allowed otherwise aborts *)
    | SendFile of exp * string
    | GetInput of exp (* tain source*)