open Environment

(* UTILS MODULE: Helper functions for the Interpreter *)

(* typecheck: checks the identifier t matches with a constructor of type evT *)
let typecheck (t, typeDescriptor) = 
    match t with
    | "int" -> (match typeDescriptor with
              | Int(_) -> true
              | _ -> false)
    | "bool" -> (match typeDescriptor with
              | Bool(_) -> true
              | _ -> false)
    | "float" -> (match typeDescriptor with
              | Float(_) -> true
              | _ -> false)
    | _ -> failwith("Not a valid type")

(* BOOL Type Utils *)

let bool_and (x, y) =
  match (typecheck("bool", x), typecheck("bool", y), x, y) with
    | (true, true, Bool(x), Bool(y)) -> Bool(x && y)
    | (_,_,_,_) -> failwith ("Not valid")


let bool_or (x, y) =
  match (typecheck("bool", x), typecheck("bool", y), x, y) with
    | (true, true, Bool(x), Bool(y)) -> Bool(x || y)
    | (_,_,_,_) -> failwith ("Not valid")  
    
let bool_not x =
  match (typecheck("bool", x), x) with
    | (true, Bool(x)) -> Bool(not x)
    | (_,_) -> failwith ("Not valid")

(* IS_ZERO Checks *)

let is_zero_int x = 
    match (typecheck("int", x), x)  with
      | (true, Int(y)) -> Bool (y = 0)
      | (_,_) -> failwith ("Run-time error")

let is_zero_float x = 
    match (typecheck("float", x), x) with
      | (true, Float(y)) -> Bool (y = 0.0)
      | (_,_) -> failwith ("Run-time error")

(* INT Type Utils *)

let int_eq (x, y) =
    match (typecheck("int", x), typecheck("int", y), x, y) with
      |(true, true, Int(x), Int(y)) -> Bool(x = y)
      |(_,_,_,_) -> failwith ("Run-time error")

let int_plus (x, y) =
  match (typecheck("int", x), typecheck("int" ,y), x, y) with
    |(true, true, Int(x), Int(y)) -> Int(x + y)
    |(_,_,_,_) -> failwith ("Run-time error")

let int_minus (x, y) =
  match (typecheck("int", x), typecheck("int", y), x, y) with
    |(true, true, Int(x), Int(y)) -> Int(x - y)
    |(_,_,_,_) -> failwith ("Run-time error")

let int_mul (x, y) =
  match (typecheck("int", x), typecheck("int", y), x, y) with
    |(true, true, Int(x), Int(y)) -> Int(x * y)
    |(_,_,_,_) -> failwith ("Run-time error")

let int_div (x, y) =
    match (typecheck("int", x), typecheck("int", y), x, y) with
      |(_,_,_,Int(0)) -> failwith ("Denominator must not be zero ")
      |(true, true, Int(x), Int(y)) -> Int(x / y)
      |(_,_,_,_) -> failwith ("Run-time error")

(* FLOAT Type Utils *)

let float_eq (x, y) =
  match (typecheck("float", x), typecheck("float", y), x, y) with
      |(true, true, Float(x), Float(y)) -> Bool(abs_float(x -. y) < 0.0001)
      |(_,_,_,_) -> failwith ("Run-time error")

let float_plus (x, y) =
    match (typecheck("float", x), typecheck("float", y), x, y) with
      |(true, true, Float(x), Float(y)) -> Float(x +. y)
      |(_,_,_,_) -> failwith ("Run-time error")

let float_minus(x,y) =
    match (typecheck("float",x),typecheck("float",y),x,y) with
      |(true,true,Float(x),Float(y)) -> Float(x-.y)
      |(_,_,_,_)->failwith("run-time error")

let float_mul (x, y) =
    match (typecheck("float", x), typecheck("float", y), x, y) with
      |(true, true, Float(x), Float(y)) -> Float(x *. y)
      |(_,_,_,_) -> failwith ("Run-time error")

let float_div (x, y) =
    match (typecheck("float", x), typecheck("float", y), x, y) with
      |(_,_,_,Float(0.0)) -> failwith ("Denominator must not be zero ")
      |(true, true, Float(x), Float(y)) -> Float(x /. y)
      |(_,_,_,_) -> failwith ("Run-time error")