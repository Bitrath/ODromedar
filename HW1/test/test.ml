open  OUnit2
(*open  HW1.Interpreter*)
(*open  HW1.Ast*)
open  HW1.Environment
(*open  HW1.Utils*)

(*
let rec lookup env x = 
  match env with
    | [] -> failwith "Not Found"
    | (y, v, _)::r -> if x = y then v else lookup r x

*)
let x = "x"

let env = [("x", (Int(6)) , true)] 
let env1 = [] 




let tests = "test suite for sum" >::: [
  "env vuoto" >:: (fun _ -> assert_equal ( "Not Found") (lookup env1 x) ); 
  "intero" >:: (fun _ -> assert_equal (Int(5)) (lookup env x)  ~msg: "Expected 5" );
  

  (*"singleton" >:: (fun _ -> assert_equal 1 (sum [1]) );
  "two_elements" >:: (fun _ -> assert_equal 3 (sum [1; 2]) ~printer:string_of_int);*)
]


let _ = run_test_tt_main tests