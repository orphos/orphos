(* Copyright (C) 2018-2019 Takezoe,Tomoaki <tomoaki@res.ac>
 *
 * SPDX-Identifier: LGPL-3.0-or-later
 *)
open Mullos_parser
open Mullos_syntax

let is_auto_semi_followed = function
  | IF | MATCH | FN | LET | LBRACKET | NUMBERSIGN | LCBRACKET | GRAVE_ACCENT
   |PLUS | HYPHEN | EXCLAMATION | AMPERSAND | ASTERISK | BIG_PLUS
   |BIG_HYPHEN | RAISE | LAZY | TILDE | IDENTIFIER _ | LPAREN | TEXT _
   |NUMBER _ | BOOL _ | TYPE | VAL | EXCEPTION ->
      true
  | _ -> false

let is_followed_by_auto_semi = function
  | END | RBRACKET | RCBRACKET | BIG_PLUS | BIG_HYPHEN | IDENTIFIER _
   |RPAREN | TEXT _ | NUMBER _ | BOOL _ ->
      true
  | _ -> false

module Buf = CamomileLibrary.UTF8.Buf
module UChar = CamomileLibrary.UChar

let new_reader () =
  let rollback = Sedlexing.rollback in
  let lexeme = Sedlexing.Utf8.lexeme in
  let lexeme_char = Sedlexing.lexeme_char in
  let rec read_newline lexbuf =
    match%sedlex lexbuf with
    | '\n' -> Sedlexing.new_line lexbuf ; read_newline lexbuf
    | ' ' | '\t' -> read_newline lexbuf
    | _ -> rollback lexbuf ; NL
  in
  let read_quoted_identifier lexbuf mark =
    let buf = Buf.create 1024 in
    let rec loop () =
      match%sedlex lexbuf with
      | any ->
          let c = lexeme_char lexbuf 0 in
          if c = Uchar.of_char mark then Buf.contents buf
          else (
            Buf.add_string buf (lexeme lexbuf) ;
            loop () )
      | _ -> failwith "unreachable"
    in
    loop ()
  in
  let rec read_unicode_escape lexbuf limit =
    let rec loop acc = function
      | i when i = limit -> UChar.of_int i
      | i -> (
          let ret base_char base_value =
            loop
              ( (acc * 16)
              + ( Uchar.to_int (Sedlexing.lexeme_char lexbuf 0)
                - Char.code base_char )
              + base_value )
              (i + 1)
          in
          match%sedlex lexbuf with
          | '0' .. '9' -> ret '0' 0
          | 'a' .. 'f' -> ret 'a' 10
          | 'A' .. 'F' -> ret 'A' 10
          | _ -> failwith "unexpecd end of unicode escape" )
    in
    loop 0 0
  in
  let read_escape lexbuf =
    match%sedlex lexbuf with
    | 'u' -> read_unicode_escape lexbuf 4
    | 'U' -> read_unicode_escape lexbuf 8
    | '\'' -> UChar.of_char '\''
    | '"' -> UChar.of_char '"'
    | '\\' -> UChar.of_char '\\'
    | 'a' -> UChar.of_int 0x07
    | 'b' -> UChar.of_char '\b'
    | 'f' -> UChar.of_int 0x0c
    | 'n' -> UChar.of_char '\n'
    | 'r' -> UChar.of_char '\r'
    | 't' -> UChar.of_char '\t'
    | 'v' -> UChar.of_int 0x0b
    | _ -> failwith "Invalid escape sequence"
  in
  let rec read_text lexbuf =
    let buf = Buf.create 1024 in
    let rec aux () =
      match%sedlex lexbuf with
      | '"' -> Buffer.contents buf
      | eof -> failwith "unexpeted EOF"
      | '\\' ->
          Buf.add_char buf (read_escape lexbuf) ;
          aux ()
      | any ->
          Buf.add_string buf (lexeme lexbuf) ;
          aux ()
      | _ -> failwith "unreachable"
    in
    aux ()
  in
  let read_precision lexbuf =
    let rec aux acc =
      match%sedlex lexbuf with
      | '0' .. '9' ->
          aux ((acc * 10) + Uchar.to_int (lexeme_char lexbuf 0) - Char.code '0')
      | _ -> acc
    in
    aux 0
  in
  let read_number lexbuf radix =
    let ret m e n = (Q.make (Z.of_int m) (Z.pow (Z.of_int 10) e), n) in
    let read_digit_char base_char base_value =
      Uchar.to_int (lexeme_char lexbuf 0) - Char.code base_char + base_value
    in
    let rec proceed m e n base_char base_value reading_exponent =
      let digit_value = read_digit_char base_char base_value in
      if radix < digit_value then ( rollback lexbuf ; ret m e n )
      else
        aux
          ((m * radix) + digit_value)
          (if reading_exponent then e + 1 else e)
          n reading_exponent
    and aux m e n reading_exponent =
      match%sedlex lexbuf with
      | '0' .. '9' -> proceed m e n '0' 0 reading_exponent
      | 'a' .. 'e' -> proceed m e n 'a' 10 reading_exponent
      | 'A' .. 'F' -> proceed m e n 'A' 10 reading_exponent
      | '.' ->
          if reading_exponent then ( rollback lexbuf ; ret m e n )
          else aux m e QType true
      | 'i' -> ret m e (IntType (read_precision lexbuf))
      | 'u' -> ret m e (IntType (read_precision lexbuf))
      | 'f' ->
          if radix = 10 then ret m e (FloatType (read_precision lexbuf))
          else ( rollback lexbuf ; ret m e n )
      | _ -> ret m e n
    in
    aux 0 0 ZType false
  in
  let rec read_raw_token lexbuf =
    match%sedlex lexbuf with
    | Plus (' ' | '\t') -> read_raw_token lexbuf
    | Plus '\n' -> Sedlexing.new_line lexbuf ; read_newline lexbuf
    (* three character symbols *)
    | ":+:" -> COLON_PLUS_COLON
    | ":-:" -> COLON_HYPHEN_COLON
    (* two character symbols *)
    | "!=" -> EXCLAMATION_EQ
    | "&&" -> BIG_AMPERSAND
    | "++" -> BIG_PLUS
    | "+:" -> PLUS_COLON
    | "+=" -> PLUS_EQ
    | "," -> COMMA
    | "--" -> BIG_HYPHEN
    | "-:" -> HYPHEN_COLON
    | "-=" -> HYPHEN_EQ
    | "->" -> HYPHEN_GREATER
    | ":+" -> COLON_PLUS
    | ":-" -> COLON_HYPHEN
    | "::" -> BIG_COLON
    | ":=" -> COLON_EQ
    | "<<" -> BIG_LESS
    | "==" -> BIG_EQ
    | "=>" -> EQ_GREATER
    | ">>" -> BIG_GREATER
    | "[" -> LBRACKET
    | "]" -> RBRACKET
    | "|>" -> VERTICAL_GREATER
    | "||" -> BIG_VERTICAL
    (* raw identifier or keyword *)
    | ( ( 'a' .. 'z'
        | 'A' .. 'Z'
        | '_', ('_' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9') )
      , Star ('a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9') ) -> (
      match lexeme lexbuf with
      | "and" -> AND
      | "case" -> CASE
      | "effect" -> EFFECT
      | "else" -> ELSE
      | "end" -> END
      | "exception" -> EXCEPTION
      | "false" -> BOOL false
      | "fn" -> FN
      | "functor" -> FUNCTOR
      | "given" -> GIVEN
      | "handle" -> HANDLE
      | "if" -> IF
      | "lazy" -> LAZY
      | "let" -> LET
      | "match" -> MATCH
      | "of" -> OF
      | "raise" -> RAISE
      | "rec" -> REC
      | "sig" -> SIG
      | "signature" -> SIGNATURE
      | "struct" -> STRUCT
      | "structure" -> STRUCTURE
      | "then" -> THEN
      | "true" -> BOOL true
      | "type" -> TYPE
      | "val" -> VAL
      | "when" -> WHEN
      | "where" -> WHERE
      | "with" -> WITH
      | "without" -> WITHOUT
      | id -> IDENTIFIER id )
    (* one character symbols *)
    | "-" -> HYPHEN
    | '!' -> EXCLAMATION
    | '#' -> NUMBERSIGN
    | '$' -> DOLLAR
    | '%' -> PERCENT
    | '&' -> AMPERSAND
    | '(' -> LPAREN
    | ')' -> RPAREN
    | '*' -> ASTERISK
    | '+' -> PLUS
    | '.' -> DOT
    | '/' -> SOLIDUS
    | ':' -> COLON
    | ';' -> SEMI
    | '<' -> LESS
    | '=' -> EQ
    | '>' -> GREATER
    | '@' -> AT
    | '\'' -> SINGLE_QUOTE
    | '^' -> CIRCUMFLEX
    | '_' -> LOWLINE
    | '_' -> LOWLINE
    | '`' -> GRAVE_ACCENT
    | '{' -> LCBRACKET
    | '|' -> VERTICAL
    | '}' -> RCBRACKET
    | '~' -> TILDE
    | '"' -> TEXT (read_text lexbuf)
    | "0", '0' .. '7' ->
        rollback lexbuf ;
        NUMBER (read_number lexbuf 8)
    | "0x" -> NUMBER (read_number lexbuf 16)
    | "0b" -> NUMBER (read_number lexbuf 2)
    | '0' .. '9' ->
        rollback lexbuf ;
        NUMBER (read_number lexbuf 10)
    | _ -> EOF
  in
  let read_raw_tokens lexbuf =
    (* read all tokens *)
    let aux acc =
      match read_raw_token lexbuf with
      | EOF -> List.rev (EOF :: acc)
      | t -> t :: acc
    in
    aux []
  in
  let filter_nl tokens =
    (* Remove NL inside auto semicolon disabled region *)
    let rec aux acc = function
      | stack, LCBRACKET :: tl -> aux (LCBRACKET :: acc) (true :: stack, tl)
      | stack, (LPAREN | LBRACKET) :: tl ->
          aux (LCBRACKET :: acc) (false :: stack, tl)
      | _ :: stack, (RCBRACKET | RPAREN | RBRACKET) :: tl ->
          aux (LCBRACKET :: acc) (stack, tl)
      | (false :: _ as stack), NL :: tl -> aux acc (stack, tl)
      | stack, hd :: tl -> aux (hd :: acc) (stack, tl)
      | _, [] -> List.rev acc
    in
    aux [] ([true], tokens)
  in
  let autoinsert_semicolon tokens =
    (* Remove NL unless semicolon should be auto inserted  *)
    let rec aux acc = function
      | NL :: tl -> aux acc tl
      | t1 :: (NL as t2) :: t3 :: tl
        when is_auto_semi_followed t1 && is_followed_by_auto_semi t3 ->
          aux (t3 :: t2 :: t1 :: acc) tl
      | hd :: tl -> aux (hd :: acc) tl
      | [] -> List.rev acc
    in
    aux [] tokens
  in
  fun lexbuf -> read_raw_tokens lexbuf |> filter_nl |> autoinsert_semicolon
