open Core
open Angstrom

let keywords =
  ["version"; "extension"; "graph"; "fragment";
  "tensor"; "integer"; "scalar"; "logical"; "string";
  "shape_of"; "length_of"; "range_of"; "for"; "in";
  "yield"; "if"; "else"] |> Set.of_list (module String);;

let is_alpha_under = function
  | '_' | 'A' .. 'Z' | 'a' .. 'z' -> true
  | _ -> false

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

let is_alnum_under c = is_digit c || is_alpha_under c

let is_ws = function
  | '\x09' .. '\x0d' | '\x20' -> true
  | _ -> false

let is_eol = function
  | '\x0a' | '\x0d' -> true
  | _ -> false

let ident =
  lift2
    (fun x xs -> String.make 1 x ^ xs)
    (satisfy is_alpha_under)
    (take_while is_alnum_under)

let not_keyword kws s =
  if Set.mem kws s then fail "keyword" else return s

let identifier =
  ident >>= not_keyword keywords

let comment = char '#' *> take_till is_eol <* end_of_line
