open Core
open Angstrom

let keywords = [
  "version"; "extension"; "graph"; "fragment";
  "tensor"; "integer"; "scalar"; "logical"; "string";
  "shape_of"; "length_of"; "range_of"; "for"; "in";
  "yield"; "if"; "else"]
  |> Set.of_list (module String);;

let is_alpha_under = function
  | '_' | 'A' .. 'Z' | 'a' .. 'z' -> true
  | _ -> false

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

let is_ws = function
  | '\x09' .. '\x0d' | '\x20' -> true
  | _ -> false

let is_eol = function
  | '\x0a' | '\x0d' -> true
  | _ -> false

let is_printable = function
  | '\x20' .. '\x7e' -> true
  | _ -> false

let ws = take_while1 is_ws

let morpheme c = char c <* ws

let lexeme s = string s <* ws

let ident =
  lift2
    (fun x xs -> String.make 1 x ^ xs)
    (satisfy is_alpha_under)
    (take_while (fun c ->
      is_digit c || is_alpha_under c))

let not_keyword kws s =
  if Set.mem kws s then fail "keyword" else return s

let identifier = ident >>= not_keyword keywords <* ws

let logical_literal =
  lexeme "true" *> return true
  <|> lexeme "false" *> return false

let string_literal =
  (char '"' <|> char '\'') >>= fun q ->
  let chunk =
    take_while1 (fun c ->
      is_printable c && Char.(c <> '\\' && c <> q))
  in
  let escaped_q =
    char '\\' *> char q >>| fun _ -> String.make 1 q
  in
  let escaped_bs =
    string "\\\\" >>| fun _ -> "\\"
  in
  many (choice [chunk; escaped_q; escaped_bs])
  <* char q
  >>| String.concat <* ws

let digits = take_while1 is_digit

let numeric_literal =
  let integer = digits in
  let frac =
    option "" (char '.' *> digits
      >>| fun f -> "." ^ f)
  in
  let exp =
    (char 'e' <|> char 'E') *>
    option '+' (char '+' <|> char '-')
    >>= fun sgn -> digits
    >>| fun e -> let sgn_str =
      match sgn with '-' -> "-" | _ -> "+"
    in "e" ^ sgn_str ^ e
  in
  lift3 (fun i f e -> i ^ f ^ e) integer frac exp
  >>| (fun s -> float_of_string s)
  <* ws

let comment = char '#' *> take_till is_eol <* end_of_line