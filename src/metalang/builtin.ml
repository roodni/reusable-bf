(*
  ビルトイン変数を含む環境を用意する。
  使いにくい名前をつけておいて標準ライブラリで改名して使うことを想定している。
  bfml使用者がビルトイン変数を認識する必要はなく、標準ライブラリだけを把握すればいい。
*)

open Syntax
open Value

let envs_with_builtin_values =
  let add_builtin name fn env =
    VE.extend (Var.of_string name) (VaBuiltin fn) env
  in
  { Envs.empty with
    va_env = Envs.empty.va_env
      |> add_builtin "__string_length__" (fun trace s ->
          let l = 
            Va.to_string trace s.i s.v
            |> String.length
          in
          VaInt l
        )
      |> add_builtin "__string_get__" (fun trace s ->
          let s = Va.to_string trace s.i s.v in
          VaBuiltin (fun trace i ->
            let i = Va.to_int trace i.i i.v in
            let c =
              try int_of_char s.[i] with
              | Invalid_argument _ ->
                  Error.at trace (Eval_Exception "index out of bounds")
            in
            VaInt c
          )
        )
      |> add_builtin "__failwith__" (fun trace msg ->
          let msg = Va.to_string trace msg.i msg.v in
          Error.at trace (Eval_Exception msg)
        )
    ;
  }