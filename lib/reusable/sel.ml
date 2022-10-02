open Support.Error

(** Ir.Selをラップする *)
type t =
  | Base of Ir.Id.t
  | LstMem of t * int * Ir.Id.t
  | LstPtr of t * Ir.Id.t
type nsel_or_nptr =
  | NSel of Ir.Sel.t
  | NPtr of Ir.Sel.t * Ir.Id.t

let base_or_mem (parent: t option) (v: Ir.Id.t) =
  match parent with
  | None -> Base v
  | Some sel -> LstMem (sel, 0, v)

let rec to_nsel_or_nptr sel =
  let rec to_nsel_lst index child sel =
    match sel with
    | Base nv ->  Ir.Sel.Array { name=nv; index_opt=None; offset=index; member=child }
    | LstMem (LstPtr (sel, p), i, nv) ->
        let child = Ir.Sel.Array { name=nv; index_opt=None; offset=index; member=child } in
        to_nsel_lst_ptr p i child sel
    | LstMem (sel, i, nv) ->
        let child = Ir.Sel.Array { name=nv; index_opt=None; offset=index; member=child } in
        to_nsel_lst i child sel
    | LstPtr _ -> assert false
  and to_nsel_lst_ptr ptr index child sel =
    match sel with
    | Base nv ->  Ir.Sel.Array { name=nv; index_opt=Some ptr; offset=index; member=child }
    | LstMem (LstPtr (sel, p), i, nv) ->
        let child = Ir.Sel.Array { name=nv; index_opt=Some ptr; offset=index; member=child } in
        to_nsel_lst_ptr p i child sel
    | LstMem (sel, i, nv) ->
        let child = Ir.Sel.Array { name=nv; index_opt=Some ptr; offset=index; member=child } in
        to_nsel_lst i child sel
    | LstPtr _ -> assert false
  in
  match sel with
  | Base nv -> NSel (Ir.Sel.Member nv)
  | LstMem (LstPtr (sel, ptr), index, nv) -> NSel (to_nsel_lst_ptr ptr index (Ir.Sel.Member nv) sel)
  | LstMem (sel, index, nv) -> NSel (to_nsel_lst index (Ir.Sel.Member nv) sel)
  | LstPtr (sel, ptr) -> begin
      let nsel = to_nsel_or_nptr sel in
      match nsel with
      | NSel nsel -> NPtr (nsel, ptr)
      | NPtr _ -> assert false
    end

(* to_nsel や to_nptr は例外を発生させるべきではなくて、infoも必要ない
    Irのリファクタリングに合わせて取り除く *)
let to_nsel info sel =
  match to_nsel_or_nptr sel with
  | NSel nsel -> nsel
  | NPtr _ -> error_at info "selector(cell) or selector(array) expected"
let to_nptr info sel =
  match to_nsel_or_nptr sel with
  | NSel _ -> error_at info "selector(index) expected"
  | NPtr (nsel, ptr) -> (nsel, ptr)

(** ポインタ[ptr]をセレクタ[sel]が経由するかどうか
    [has_ptr ptr sel] *)
let rec has_ptr ptr sel =
  match sel with
  | Base _ -> false
  | LstMem (sel, _, _) -> has_ptr ptr sel
  | LstPtr (sel, p) -> if ptr = p then true else has_ptr ptr sel