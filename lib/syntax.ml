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

type arg =
  | ASolo of rhs
  | AIden of string * rhs

type invocation = Invoc of string * arg list

type assignment = Ass of lhs * invocation

type graph = Graph of graph_dec * assignment list

type document = Doc of float * string list * graph

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

let argument =
  let rh =
    rhs_expr () >>| (fun r -> ASolo r)
  in
  let iden =
    identifier <* morpheme '='
  in
  let ass =
    lift2 (fun i r -> AIden (i,r)) iden (rhs_expr ())
  in
  choice [
    rh;
    ass
  ]

let arglist =
  sep_by1 (morpheme ',') argument

let invocation =
  let args =
    morpheme '(' *> arglist <* morpheme ')'
  in
  lift2 (fun i a -> Invoc (i,a)) identifier args

let assign =
  let invoc =
    morpheme '=' *> invocation <* morpheme ';'
  in
  lift2 (fun l i -> Ass (l,i)) (lhs_expr ()) invoc

let body =
  morpheme '{' *> many assign <* morpheme '}'

let graph_def =
  lift2 (fun d b -> Graph (d,b)) graph_declaration body

let version =
  lexeme "version" *> numeric_literal <* morpheme ';'

let extension =
  lexeme "extension" *> many1 identifier <* morpheme ';'

let document =
  lift3 (fun v e g -> Doc (v,e,g)) version extension graph_def