open HW1.Ast
open HW1.Interpreter

(*let () = print_endline "Hello, World!";;*)

(* let x = 1 in 
    let sum (y: int) = x + y in
      handle sum;; *)
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

let _ = eval testTrustBlock [] false Trusted;;
