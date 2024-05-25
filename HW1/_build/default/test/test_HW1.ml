open OUnit2
open HW1.Ast
open HW1.Environment
open HW1.Interpreter
(*
let rec sum = function
  | [] -> 0
  | x :: xs -> x + sum xs

  let tests = "test suite for sum" >::: [
    "empty" >:: (fun _ -> assert_equal 0 (sum []));
    "singleton" >:: (fun _ -> assert_equal 1 (sum [1]));
    "two_elements" >:: (fun _ -> assert_equal 3 (sum [1; 2]));

    eval_test "eval_Prim" (Int 1, false) (CstInt 1) envTest false Trusted;
  let envTest = [("x", Int 1, false), ("y", Int 2, false)]
  *)

(* --- Test Expressions --- *)
let testTrustedBlock = (
  TrustedBlock(
    "trustB1",
    (
      Let(
        "x",
        Private, 
        (CstInt 1),
        ( 
          Let(
            "sum",
            Private,
            (
              Fun(
                "y",
                ((Prim("+", Den("x"), Den("y"))))
              )
            ),
            Handle("sum")
          )
        )
      )
    )
  )
)
let expInclude_1 = Include(Untrusted, "plugin", (Let("xIn", Public, (CstInt 1), (Let ("yIn", Public, (CstInt 3), (Prim("+", Den "xIn", Den "yIn")))))))
let expExec_mul = Execute("pluginMul", (Let("n1", Public, (CstInt 3), (Call(Den("mul"), Den("n1"))))))
let expHandleCall_Den = (Let("n", Private, (CstInt 10), (HandleCall("sum", Den("n")))))
let expHandleCall_CstInt = (HandleCall("sum", (CstInt 7)))
let expExec_Psw = Execute("myFilter", (HandleCall("checkPassword", Den("ext"))))
(*
let expTrustedBlockPsw = (
  TrustedBlock(
    "trustB2",
    (
      Let(
        "password",
        Private, 
        (CstStr "abcd"),
        ( 
          Let(
            "checkPassword",
            Private,
            (
              Fun(
                "guess",
                ((Prim("cmp", Den("password"), Den("guess"))))
              )
            ),
            Handle("checkPassword")
          )
        )
      )
    )
  )
)*)
(*
let expIncludePsw = Include(Untrusted, "myFilter", (Let("ext", Public, (CstStr "acbd"), (EndInclude))))*)

(* --- Test Environments --- *)
let envTrustedBlock1 = [("sum", HandleFlag "trustB1", false); 
  ("trustB1", ClosureTrustedBlock [("sum", Closure ("y", Prim ("+", Den "x", Den "y"), [("x", Int 1, false)]), false);
  ("x", Int 1, false)], false)]
(*
let envTrustedBlock2 = [("checkPassword", HandleFlag "trustB2", false); 
  ("trustB2", ClosureTrustedBlock [("checkPassword", Closure ("guess", Prim("cmp", Den "password", Den "guess"), [("password", String "abcd", false)]), false);
  ("password", String "abcd", false)], false)]*)

let envHandle_1 = [("sum", Closure ("y", Prim ("+", Den "x", Den "y"), [("x", Int 1, false)]), false)]
(*
let envHandle_2 = [("checkPassword", Closure ("guess", Prim ("cmp", Den "password", Den "guess"), [("password", String "abcd", false)]), false)]*)

let envInclude_1 = [("plugin", ClosureInclude(Untrusted, (Let("xIn", Public, (CstInt 1), (Let ("yIn", Public, (CstInt 3), (Prim("+", Den "xIn", Den "yIn"))))))), false)]

let envInclude_Mul = [("pluginMul", ClosureInclude(Untrusted, (Let("mul", Public, (Fun("n1", Prim("*", Den("n1"), CstInt 2))), (EndInclude)))), false)]

let envExecCheckPSW = [
  ("myFilter", ClosureInclude (Untrusted, Let ("ext", Public, CstStr "abcd", EndInclude)), false);
  ("checkPassword", HandleFlag "trustB2", false);
  ("trustB2", ClosureTrustedBlock[
    ("checkPassword", Closure ("guess", Prim ("cmp", Den "password", Den "guess"), [("password", String "abcd", false)]), false);
    ("password", String "abcd", false)
    ], false)
  ]
(*
(* 1) Trusted Block Exp *)
let envOne, taintOne = eval expTrustedBlockPsw [] false Trusted 

(* 2) Plugin Exp *)
let envTwo, taintTwo = eval expIncludePsw envOne taintOne Trusted
(* 3) Exec Plugin with Handle Call *)
*)
(* --- Test Functions --- *)
let lookup_test name expected_output (env_i: evT env) (ide_i: ide) = 
  name >:: (fun _ -> assert_equal expected_output (lookup env_i ide_i))

let eval_test name expected_output (exp_i: exp) (env_i: evT env) (taint_i: bool) (sec_i: trust) =
  name >:: (fun _ -> assert_equal expected_output (eval exp_i env_i taint_i sec_i) ~msg:"Failed at Eval")

(* --- Tests --- *)
let tests = "Test Suite for Interpreter" >::: [
  lookup_test "lookup_env" (Int 1) [("x", Int 1, false)] ("x": ide);
  eval_test "eval_Int" (Int 1, false) (CstInt 1) [] false Trusted;
  eval_test "eval_Prim" (Int 3, false) (Prim("+", Den "x", Den "y")) [("x", Int 1, false); ("y", Int 2, false)] false Trusted;
  eval_test "eval_TrustedBlock1" (Env envTrustedBlock1, false) testTrustedBlock [] false Trusted;
  eval_test "eval_Handle" (ClosureTrustedBlock envHandle_1, false) (Handle "sum") envHandle_1 false BlockLvl;
  eval_test "eval_Include_1" (Env envInclude_1, false) expInclude_1 [] false Trusted;
  eval_test "eval_Exec_mul" (Int 6, false) expExec_mul envInclude_Mul false Untrusted;
  eval_test "eval_HandleCall_TB1_Den" (Int 11, false) expHandleCall_Den envTrustedBlock1 false Trusted;
  eval_test "eval_HandleCall_TB1_CstInt" (Int 8, false) expHandleCall_CstInt envTrustedBlock1 false Trusted;
  eval_test "eval_HandleCall_TB2_Psw" (Bool true, false) expExec_Psw envExecCheckPSW false Trusted;
]

let _ = run_test_tt_main tests 