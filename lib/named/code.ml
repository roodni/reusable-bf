(** 中間言語のコード *)
type t = cmd list
and cmd =
  | Add of int * Sel.t
  | Put of Sel.t
  | Get of Sel.t
  | Loop of Sel.t * t
  | LoopPtr of (Sel.t * Id.t * t)
  | Shift of int * Sel.t * Id.t
  | If of Sel.t * t * t