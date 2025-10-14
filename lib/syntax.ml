open Core
open Angstrom
open Lex

type graph_dec = Graph_dec of string * string list * string list

type literal =
  | Num of float
  | Str of string
  | Bool of bool

type lhs =
  | LIden of string
  | LArr of lhs list
  | LTupl of lhs list

type rhs =
  | RIden of string
  | RLit of literal
  | RArr of rhs list
  | RTupl of rhs list

let sep_by2 a b =
  sep_by1 a b >>= function
  | x :: y :: s -> return (x :: y :: s)
  | _ -> fail "expected 2 values"

let identifier_list =
  sep_by1 (morpheme ',') identifier

let graph_declaration =
  let name =
    lexeme "graph" *> identifier
  in
  let inp =
    morpheme '(' *> identifier_list <* morpheme ')'
  in
  let out =
    lexeme "->" *>
    morpheme '(' *> identifier_list <* morpheme ')'
  in
  lift3 (fun n i o -> Graph_dec (n, i, o)) name inp out

let rec lhs_expr () =
  choice [
    identifier >>| (fun s -> LIden s);
    array_lhs_expr () >>| (fun ls -> LArr ls);
    tuple_lhs_expr () >>| (fun ls -> LTupl ls)
  ]
and array_lhs_expr () =
  morpheme '[' *>
  sep_by1 (morpheme ',') (lhs_expr ())
  <* morpheme ']'
and tuple_lhs_expr () =
  let lhs_list =
    sep_by2 (morpheme ',') (lhs_expr ())
  in
  choice [
    morpheme '(' *> lhs_list <* morpheme ')';
    lhs_list
  ]

let literal =
  choice [
    numeric_literal >>| (fun n -> Num n);
    string_literal >>| (fun s -> Str s);
    logical_literal >>| (fun b -> Bool b)
  ]

let rec rhs_expr () =
  choice [
    literal >>| (fun l -> RLit l);
    identifier >>| (fun s -> RIden s);
    array_rhs_expr () >>| (fun ls -> RArr ls);
    tuple_rhs_expr () >>| (fun ls -> RTupl ls)
  ]
and array_rhs_expr () =
  morpheme '[' *>
  sep_by1 (morpheme ',') (rhs_expr ())
  <* morpheme ']'
and tuple_rhs_expr () =
  morpheme '(' *>
  sep_by2 (morpheme ',') (rhs_expr ())
  <* morpheme ')'