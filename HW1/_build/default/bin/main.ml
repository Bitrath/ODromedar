open HW1.Ast
open HW1.Environment
open HW1.Interpreter
open OUnit2

let extractEnv (env: evT): evT env = (
    match env with 
      | Env(e) -> e
      | _ -> []
);;

let emptyEnv (env: evT env): evT = (
  match env with 
    | [] -> Bool true 
    | _ -> Bool false 
)

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
);;

let expIncludePsw = Include(Untrusted, "myFilter", (Let("ext", Public, (CstStr "abcd"), (EndInclude))));;
let expExec_Psw = Execute("myFilter", (HandleCall("checkPassword", Den("ext"))))

let test = ( (* 1) Trusted Block Exp *)
  let envOne, taintOne = eval expTrustedBlockPsw [] false Trusted in (*  Env(e) *)
    let envOneR = extractEnv envOne in 
      let envOneB = emptyEnv envOneR in 
        match envOneB with 
          | Bool true -> failwith "Env One Error"
          | Bool false -> ( (* 2) Plugin Exp *)
              let envTwo, taintTwo = eval expIncludePsw envOneR taintOne Trusted in 
                let envTwoR = extractEnv envTwo in 
                  let envTwoB = emptyEnv envTwoR in 
                    match envTwoB with 
                      | Bool true -> failwith "Env Two Error"
                      | Bool false -> ( (* 3) Exec Plugin with Handle Call *)
                          let result, taintResult = eval expExec_Psw envTwoR taintTwo Trusted in 
                            if taintResult = true then failwith "exec Error" 
                            else (result)
                        )
                      | _ -> failwith "Env Two Error 2"
            )
          | _ -> failwith "Env One Error 2"
);;

let testing = "--- test suite for boh ---" >::: [
  "checkPsw" >:: (fun _ -> assert_equal (Bool true) (test));
]

let _ = run_test_tt_main testing

(*let () = print_endline "Hello, World!";;*)

(* let x = 1 in 
    let sum (y: int) = x + y in
      handle sum;;
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
) *)


(*
[("myFilter", ClosureInclude (Untrusted, Let ("ext", Public, CstStr "acbd", EndInclude)), false);
  ("checkPassword", HandleFlag "trustB2", false);
  ("trustB2", ClosureTrustedBlock[
      ("checkPassword", Closure ("guess", Prim ("cmp", Den "password", Den "guess"), [("password", String "abcd", false)]), false);
      ("password", String "abcd", false)
    ], false)]
*)

(*
let test1 = ( (* 1) Trusted Block Exp *)
  let envOne, taintOne = eval expTrustedBlockPsw [] false Trusted in (*  Env(e) *)
    let envOneR = extractEnv envOne in 
      let envOneB = emptyEnv envOneR in 
        match envOneB with 
          | Bool true -> failwith "Env One Error"
          | Bool false -> ( (* 2) Plugin Exp *)
              let envTwo, taintTwo = eval expIncludePsw envOneR taintOne Trusted in 
                let envTwoR = extractEnv envTwo in 
                  let envTwoB = emptyEnv envTwoR in 
                    match envTwoB with 
                      | Bool true -> failwith "Env Two Error"
                      | Bool false -> (envTwoR)
                      | _ -> failwith "Env Two Error 2"
            )
          | _ -> failwith "Env One Error 2"
);;   
*)