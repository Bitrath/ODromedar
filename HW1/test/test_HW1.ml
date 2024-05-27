open OUnit2
open HW1.Ast
open HW1.Environment
open HW1.Interpreter

(* --- Test (lookup_env) Setup ---------------------- *)
(* --- Test (eval_Int) Setup ------------------------ *)
(* --- Test (eval_Prim) Setup ----------------------- *)
(* --- Test (eval_Let) Setup ------------------------ *)
let expLet = Let("y", Private, (CstInt 2), (Prim("+", Den("x"), Den("y"))));;
let envLet = [("x", Int 1, false)];;

(* --- Test (eval_If) Setup ------------------------- *)
let expIF = If((Prim("=", Den("x"), Den("y"))), (Prim("-", Den("x"), Den("y"))), (Prim("+", Den("x"), Den("y"))));;
let envIF_e1 = [("x", Int 2, false); ("y", Int 2, false)];;
let envIF_e2 = [("x", Int 1, false); ("y", Int 2, false)];;

(* --- Test (eval_Fun) Setup ------------------------ *)
let expFun = Fun("x1", (Prim("+", Den("x1"), (CstInt 1))));;
let returnFun = Closure("x1", (Prim("+", Den("x1"), (CstInt 1))), [])
let expCall = Let("sum", Public, (expFun), (Call(Den "sum", Den "x")));;
let envCall = [("x", Int 3, false); ("y", Int 2, false)];;

(* --- Test (eval_TrustedBlock1) Setup -------------- *)
let expTrustedBlock = (
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
);;
let envTrustedBlock = [("sum", HandleFlag "trustB1", false); 
  ("trustB1", ClosureTrustedBlock [("sum", Closure ("y", Prim ("+", Den "x", Den "y"), [("x", Int 1, false)]), false);
  ("x", Int 1, false)], false)];;
  
(* --- Test (eval_Handle) Setup --------------------- *)
let envHandle = [("sum", Closure ("y", Prim ("+", Den "x", Den "y"), [("x", Int 1, false)]), false)];;

(* --- Test (eval_Include) Setup ------------------ *)
let expInclude = Include(Untrusted, "plugin", (Let("xIn", Public, (CstInt 1), (Let ("yIn", Public, (CstInt 3), (Prim("+", Den "xIn", Den "yIn")))))));;
let envInclude = [("plugin", ClosureInclude(Untrusted, (Let("xIn", Public, (CstInt 1), (Let ("yIn", Public, (CstInt 3), (Prim("+", Den "xIn", Den "yIn"))))))), false)];;

(* --- Test (eval_Exec_mul) Setup ------------------- *)
let expExec_mul = Execute("pluginMul", (Let("n1", Public, (CstInt 3), (Call(Den("mul"), Den("n1"))))));;
let envInclude_Mul = [("pluginMul", ClosureInclude(Untrusted, (Let("mul", Public, (Fun("n1", Prim("*", Den("n1"), CstInt 2))), (EndInclude)))), false)];;

(* --- Test (eval_HandleCall_TB1_Den) Setup --------- *)
let expHandleCall_Den = (Let("n", Private, (CstInt 10), (HandleCall("sum", Den("n")))));;

(* --- Test (eval_HandleCall_TB1_CstInt) Setup ------ *)
let expHandleCall_CstInt = (HandleCall("sum", (CstInt 7)));;

(* --- Test (eval_HandleCall_TB2_Psw) Setup --------- *)
let expExec_Psw = Execute("myFilter", (HandleCall("checkPassword", Den("ext"))));;
let envExecCheckPSW = [
  ("myFilter", ClosureInclude (Untrusted, Let ("ext", Public, CstStr "abcd", EndInclude)), false);
  ("checkPassword", HandleFlag "trustB2", false);
  ("trustB2", ClosureTrustedBlock[
    ("checkPassword", Closure ("guess", Prim ("cmp", Den "password", Den "guess"), [("password", String "abcd", false)]), false);
    ("password", String "abcd", false)
    ], false)
  ];;

(* --- Test Functions --- *)
let lookup_test name expected_output (env_i: evT env) (ide_i: ide) = 
  name >:: (fun _ -> assert_equal expected_output (lookup env_i ide_i))

let taint_lookup_test name expected_output (env_i: evT env) (ide_i: ide) = 
  name >:: (fun _ -> assert_equal expected_output (taint_lookup env_i ide_i))
let clean_lookup_test name expected_output (env_i: evT env) (ide_i: ide) = 
  name >:: (fun _ -> assert_equal expected_output (clean_lookup env_i ide_i))

let eval_test name expected_output (exp_i: exp) (env_i: evT env) (taint_i: bool) (sec_i: trust) =
  name >:: (fun _ -> assert_equal expected_output (eval exp_i env_i taint_i sec_i) ~msg:"Failed at Eval")

(* ----------------------------------------- *)
(* -------------- (Tests) ------------------ *)
(* ----------------------------------------- *)
let tests = "Test Suite for Interpreter" >::: [
  (* --- Lookup Tests -------------------- *)
  lookup_test "lookup_x" (Float 1.) ([("x", Float 1., false); ("y", Int 2, false)]) ("x": ide);
  taint_lookup_test "taint_lookup_x" (false) ([("x", Int 1, false); ("y", Int 2, true)]) ("x": ide);
  clean_lookup_test "clean_lookup_x" (Int 1) ([("x", Float 1., false); ("y", Int 2, false)]) ("x": ide);
  (* --- Eval (Type) Tests --------------- *)
  eval_test "eval_type_int" (Int 1, false) (CstInt 1) [] false Trusted;
  eval_test "eval_type_float" (Float 1., false) (CstFlt 1.) [] false Trusted;
  eval_test "eval_type_bool" (Bool true, false) (CstBool true) [] false Trusted;
  eval_test "eval_type_string" (String "Ok", false) (CstStr "Ok") [] false Trusted;
  eval_test "eval_type_den" (Int 0, false) (Den "x") ([("x", Int 0, false); ("y", Int 2, false)]) false Trusted;
  (* --- Eval (Let) Tests ---------------- *)
  eval_test "eval_let" (Int 3, false) expLet envLet false Trusted;
  (* --- Eval (Prim) Tests --------------- *)
  eval_test "eval_prim_int_mul" (Int 2, false) (Prim("*", Den "x", Den "y")) [("x", Int 2, false); ("y", Int 1, false)] false Trusted;
  eval_test "eval_prim_int_sum" (Int 3, false) (Prim("+", Den "x", Den "y")) [("x", Int 2, false); ("y", Int 1, false)] false Trusted;
  eval_test "eval_prim_int_sub" (Int 1, false) (Prim("-", Den "x", Den "y")) [("x", Int 2, false); ("y", Int 1, false)] false Trusted;
  eval_test "eval_prim_int_cmp" (Bool true, false) (Prim("=", Den "x", Den "y")) [("x", Int 1, false); ("y", Int 1, false)] false Trusted;
  eval_test "eval_prim_int_>" (Bool true, false) (Prim(">", Den "x", Den "y")) [("x", Int 2, false); ("y", Int 1, false)] false Trusted;
  eval_test "eval_prim_int_<" (Bool true, false) (Prim("<", Den "x", Den "y")) [("x", Int 1, false); ("y", Int 2, false)] false Trusted;
  eval_test "eval_prim_float_mul" (Float 2., false) (Prim("*", Den "x", Den "y")) [("x", Float 2., false); ("y", Float 1., false)] false Trusted;
  eval_test "eval_prim_float_sum" (Float 3., false) (Prim("+", Den "x", Den "y")) [("x", Float 2., false); ("y", Float 1., false)] false Trusted;
  eval_test "eval_prim_float_sub" (Float 1., false) (Prim("-", Den "x", Den "y")) [("x", Float 2., false); ("y", Float 1., false)] false Trusted;
  eval_test "eval_prim_float_cmp" (Bool true, false) (Prim("=", Den "x", Den "y")) [("x", Float 1., false); ("y", Float 1., false)] false Trusted;
  eval_test "eval_prim_float_>" (Bool true, false) (Prim(">", Den "x", Den "y")) [("x", Float 2., false); ("y", Float 1., false)] false Trusted;
  eval_test "eval_prim_float_<" (Bool true, false) (Prim("<", Den "x", Den "y")) [("x", Float 1., false); ("y", Float 2., false)] false Trusted;
  eval_test "eval_prim_bool_&" (Bool false, false) (Prim("&", Den "x", Den "y")) [("x", Bool true, false); ("y", Bool false, false)] false Trusted;
  eval_test "eval_prim_bool_|" (Bool true, false) (Prim("|", Den "x", Den "y")) [("x", Bool true, false); ("y", Bool false, false)] false Trusted;
  eval_test "eval_prim_string_cmp" (Bool true, false) (Prim("cmp", Den "x", Den "y")) [("x", String "test", false); ("y", String "test", false)] false Trusted;
  (* --- Eval (If) Tests ----------------- *)
  eval_test "eval_if_e1" (Int 0, false) expIF envIF_e1 false Trusted;
  eval_test "eval_if_e2" (Int 3, false) expIF envIF_e2 false Trusted;
  (* --- Eval (Fun) Tests ---------------- *)
  eval_test "eval_fun" (returnFun, false) expFun [] false Trusted;
  eval_test "eval_fun_call" (Int 4, false) expCall envCall false Untrusted;
  (* --- Eval (GetInput) Tests ----------- *)
  (* --- Eval (Trusted Block) Tests ------ *)
  eval_test "eval_trusted_block" (Env envTrustedBlock, false) expTrustedBlock [] false Trusted;
  (* --- Eval (Handle) Tests ------------- *)
  eval_test "eval_handle" (ClosureTrustedBlock envHandle, false) (Handle "sum") envHandle false BlockLvl;
  (* --- Eval (Include) Tests ------------ *)
  eval_test "eval_include" (Env envInclude, false) expInclude [] false Trusted;
  (* --- Eval (Execute) Tests ------------ *)
  eval_test "eval_exec_mul" (Int 6, false) expExec_mul envInclude_Mul false Untrusted;
  (* --- Eval (HandleCall) Tests --------- *)
  eval_test "eval_handleCall_den" (Int 11, false) expHandleCall_Den envTrustedBlock false Trusted;
  eval_test "eval_handleCall_int" (Int 8, false) expHandleCall_CstInt envTrustedBlock false Trusted;
  eval_test "eval_handleCall_psw" (Bool true, false) expExec_Psw envExecCheckPSW false Trusted;
];;

let _ = run_test_tt_main tests;;


(* --- SETUPS UNUSED --- *)
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
)
let expIncludePsw = Include(Untrusted, "myFilter", (Let("ext", Public, (CstStr "acbd"), (EndInclude))))

let envTrustedBlock2 = [("checkPassword", HandleFlag "trustB2", false); 
  ("trustB2", ClosureTrustedBlock [("checkPassword", Closure ("guess", Prim("cmp", Den "password", Den "guess"), [("password", String "abcd", false)]), false);
  ("password", String "abcd", false)], false)]

let envHandle_2 = [("checkPassword", Closure ("guess", Prim ("cmp", Den "password", Den "guess"), [("password", String "abcd", false)]), false)]

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
*)