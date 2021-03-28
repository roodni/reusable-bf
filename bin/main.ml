open Batteries
open Printf
open Bfhoge
open Bfhoge.Named

let svar v = Sel.Cell v
let slst ?p ?(i=0) lst sel =
  match p with
  | None -> Sel.Lst (lst, i, sel)
  | Some p -> Sel.LstPtr (lst, p, i, sel)


let add ?(n=1) s = Cmd.Add (n, s)
let sub ?(n=1) s = Cmd.Add (-n, s)
let loop s l = Cmd.Loop (s, l)
let add2 ?(times=1) s_from s_dest =
  loop s_from [
    sub s_from;
    add ~n:times s_dest;
  ]
let sub2 ?(times=1) s_from s_dest =
  loop s_from [
    sub s_from;
    sub ~n:times s_dest
  ]

let ipt_base10 ed ~dest ~tmp:(ipt, tmp) =
  let open Cmd in
    [
      add ipt;
      loop ipt [
        Get ipt;
        sub ~n:(int_of_char ed) ipt;
        add tmp;
        (* if *)
        loop ipt [
          sub tmp;
          add2 dest tmp;
          sub ~n:(int_of_char '0' - int_of_char ed) ipt;
          add2 ipt dest;
          add2 ~times:10 tmp dest;
        ];
        add ipt;
        (* else *)
        loop tmp [ sub tmp;
          sub ipt;
        ];
      ];
    ]  

let dfn, program =
  let ipt = Var.gen_named "ipt" in
  let tmp = Var.gen_named "tmp" in
  let a = Var.gen_named "a" in
  let b = Var.gen_named "b" in
  let arr = Var.gen_named "arr" in
  let n = Var.gen_named "n" in
  let up = Var.gen_named "up" in
  let flg = Var.gen_named "flg" in
  let p = Var.gen_named "p" in
  let dfn =
    let open Dfn in
    of_list [
      (ipt, Cell);
      (tmp, Cell);
      (a, Cell);
      (b, Cell);
      (arr, Lst {
        length = Some 4;
        mem = of_list [
          (n, Cell);
          (up, Cell);
          (tmp, Cell);
          (p, Ptr);
          (flg, Cell);
        ];
      })
    ]
  in
  let program =
    let open Cmd in
    ipt_base10 ' ' ~dest:(svar a) ~tmp:(svar ipt, svar tmp) @
    ipt_base10 '\n' ~dest:(svar b) ~tmp:(svar ipt, svar tmp) @
    [
      sub (svar a);
      sub (svar b);
      loop (svar a) [ sub (svar a);
        loop (svar b) [ sub (svar b); add (svar tmp);
          (* Comment "loop"; *)
          (* Dump; *)
          add (slst ~i:0 arr @@ svar up);
          loop (slst ~p arr @@ svar up) begin
            let p_up = (slst ~p arr @@ svar up) in
            let p_n = (slst ~p arr @@ svar n) in
            let p_tmp = (slst ~p arr @@ svar tmp) in
            let p_flg = (slst ~p arr @@ svar flg) in
            [
              add ~n:9 p_tmp;
              sub2 p_n p_tmp;
              loop p_tmp [
                sub p_up;
                add ~n:10 p_n;
                sub2 p_tmp p_n;
              ];
              loop p_up [
                sub p_up;
                add (slst ~p ~i:1 arr @@ svar up);
              ];
              loop p_flg [ sub p_flg ]; add p_flg;
              Shift (1, svar arr, p);
            ]
          end;
          loop (slst ~p ~i:(-1) arr @@ svar p) [
            Shift (-1, svar arr, p);
          ];
        ];
        loop (svar tmp) [
          sub (svar tmp); add (svar b);
        ];
      ];
      loop (slst ~p arr @@ svar flg) [
        Shift (1, svar arr, p);
      ];
      LoopPtr (svar arr, p, [
        Shift (-1, svar arr, p);
        add ~n:(int_of_char '0') (slst ~p arr @@ svar n);
        Put (slst ~p arr @@ svar n);
      ]);
    ]
  in
  (dfn, program)

let layout = Layout.of_dfn dfn

let bf_code = codegen layout program

(* let bf_code =
  let open Bf.Cmd in [
    Move 1;
    Add 45;
    Loop [
      Comment "test";
      Loop [
        Move (-1); Add 1;
        Move 2; Add 1;
        Move (-1); Add (-1);
      ];
      Move 1;Add (-1);
    ];
  ] *)

let main () =
  Layout.print layout;
  bf_code |> Bf.Cmd.list_to_string |> print_endline;
  print_endline "--- start ---";
  let state = Bf.run_stdio bf_code in
  print_newline ();
  Bf.State.dump state

let () = main ()