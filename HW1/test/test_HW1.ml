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

  let testTrustBlock = (
    TrustedBlock(
      "a",
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
  
  let envTrustedBlock_1 = [("sum", HandleFlag "a", false); 
    ("a", ClosureTrustedBlock [("sum", Closure ("y", Prim ("+", Den "x", Den "y"), [("x", Int 1, false)]), false);
    ("x", Int 1, false)], false)]

  let envHandle_1 = [("sum", Closure ("y", Prim ("+", Den "x", Den "y"), [("x", Int 1, false)]), false)]

  (*
let envInclude_1 = [("y", Int 1, false); ("x", Int 1, false)]
  *)
  let expInclude_1 = Include(Untrusted, "plugin", (Let("x", Public, (CstInt 1), (Let ("y", Public, (CstInt 3), (Prim("+", Den "x", Den "y")))))))
  
  let envInclude_1 = [("plugin", ClosureInclude(Untrusted, (Let("x", Public, (CstInt 1), (Let ("y", Public, (CstInt 3), (Prim("+", Den "x", Den "y"))))))), false)]

  let lookup_test name expected_output (env_i: evT env) (ide_i: ide) = 
    name >:: (fun _ -> assert_equal expected_output (lookup env_i ide_i) ~msg: "Failed at Lookup")

  let eval_test name expected_output (exp_i: exp) (env_i: evT env) (taint_i: bool) (sec_i: trust) =
    name >:: (fun _ -> assert_equal expected_output (eval exp_i env_i taint_i sec_i) ~msg:("Failed at (" ^ name ^ ")"))
  
  let tests = "Test Suite for Interpreter" >::: [
    lookup_test "lookup_env" (Int 1) [("x", Int 1, false)] ("x": ide);
    eval_test "eval_Int" (Int 1, false) (CstInt 1) [] false Trusted;
    eval_test "eval_Prim" (Int 3, false) (Prim("+", Den "x", Den "y")) [("x", Int 1, false); ("y", Int 2, false)] false Trusted;
    eval_test "eval_TrustedBlock_1" (Env envTrustedBlock_1, false) (testTrustBlock) [] false Trusted;
    eval_test "eval_Handle" (ClosureTrustedBlock envHandle_1, false) (Handle "sum") envHandle_1 false BlockLvl;
    eval_test "eval_Include_1" (Env envInclude_1, false) (expInclude_1) [] false Trusted;
  ]

  let _ = run_test_tt_main tests 