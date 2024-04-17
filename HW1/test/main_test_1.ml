type evT = Int of int
        | Float of float
        | Bool of bool
        | Unbound

type ide = string

type exp = 
      CstInt of int
    | CstFlt of float
    | CstTrue
    | CstFalse
    | Times of exp * exp
    | Sum of exp * exp
    | Sub of exp * exp
    | Eq of exp * exp
    | Iszero of exp
    | Or of exp * exp
    | And of exp * exp
    | Not of exp
    | Den of ide
    | Ifthenelse of exp * exp * exp
    | Let of ide * exp * exp
    | Fun of ide list * exp
    | Apply of exp * exp list


let typecheck(tipo, typeDescriptor) = 
    match tipo with
    | "int" -> (match typeDescriptor with
              | Int(u) -> true
              | _ -> false)
    | "bool" -> (match typeDescriptor with
              | Bool(u) -> true
              | _ -> false)
    | "float" -> (match typeDescriptor with
              | Float(u) -> true
              | _ -> false)
    | _ -> failwith("not a valid type")

(*controllo uguale a zero per int*)
let is_zero_int x = 
    match(typecheck("int",x),x)  with
      | (true,Int(y)) -> Bool (y=0)
      | (_,_) -> failwith ("run-time error")


(*controllo uguale a zero per float*)
let is_zero_float x = 
    match(typecheck("float",x),x) with
      | (true,Float(y)) -> Bool (y=0.0)
      | (_,_) -> failwith ("run-time error")



(*eguaglianza tra interi*)
let int_eq(x,y) =
    match (typecheck("int",x),typecheck("int",y),x,y) with
      |(true,true,Int(x),Int(y)) -> Bool(x=y)
      |(_,_,_,_)->failwith("run-time error")


(*eguaglianza tra float*)
let float_eq(x,y) =
  match (typecheck("float",x),typecheck("float",y),x,y) with
      |(true,true,Float(x),Float(y)) -> Bool(abs_float(x-.y)<0.0001)
      |(_,_,_,_)->failwith("run-time error")



(*somma tra int*)
let int_plus(x,y) =
    match (typecheck("int",x),typecheck("int",y),x,y) with
      |(true,true,Int(x),Int(y)) -> Int(x+y)
      |(_,_,_,_)->failwith("run-time error")

  (*somma tra float*)
let float_plus(x,y) =
    match (typecheck("float",x),typecheck("float",y),x,y) with
      |(true,true,Float(x),Float(y)) -> Float(x+.y)
      |(_,_,_,_)->failwith("run-time error")


(*sottrazione tra int*)
let int_minus(x,y) =
    match (typecheck("int",x),typecheck("int",y),x,y) with
      |(true,true,Int(x),Int(y)) -> Int(x-y)
      |(_,_,_,_)->failwith("run-time error")


(*sottrazione tra float*)
let float_minus(x,y) =
    match (typecheck("float",x),typecheck("float",y),x,y) with
      |(true,true,Float(x),Float(y)) -> Float(x-.y)
      |(_,_,_,_)->failwith("run-time error")


(*moltiplicazione tra int*)
let int_mul(x,y) =
    match (typecheck("int",x),typecheck("int",y),x,y) with
      |(true,true,Int(x),Int(y)) -> Int(x*y)
      |(_,_,_,_)->failwith("run-time error")


(*moltiplicazione tra float*)
let float_mul(x,y) =
    match (typecheck("float",x),typecheck("float",y),x,y) with
      |(true,true,Float(x),Float(y)) -> Float(x*.y)
      |(_,_,_,_)->failwith("run-time error")



(*divisione tra int*)
let int_div(x,y) =
    match (typecheck("int",x),typecheck("int",y),x,y) with
      |(_,_,_,Int(0)) -> failwith("denominator must not be zero ")
      |(true,true,Int(x),Int(y)) -> Int(x/y)
      |(_,_,_,_)->failwith("run-time error")


(*divisione tra float*)
let float_div(x,y) =
    match (typecheck("float",x),typecheck("float",y),x,y) with
      |(_,_,_,Float(0.0)) -> failwith("denominator must not be zero ")
      |(true,true,Float(x),Float(y)) -> Float(x/.y)
      |(_,_,_,_)->failwith("run-time error")