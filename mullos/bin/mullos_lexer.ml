(* Copyright (C) 2018-2019 Takezoe,Tomoaki <tomoaki@res.ac>
 *
 * SPDX-Identifier: LGPL-3.0-or-later
 *)
open Mullos_parser
open Mullos_syntax

let is_expression_start = function
  | AMPERSAND | ASTERISK | BOOL _ | EXCLAMATION | FN | HYPHEN | IF | LAZY
   |LCBRACKET | LET | IDENTIFIER _ | LPAREN | NUMBER _ | PLUS | RAISE
   |TEXT _ | VAL ->
      true
  | _ -> false

let is_expression_end = function
  | BOOL _ | IDENTIFIER _ | NUMBER _ | RCBRACKET | RPAREN | TEXT _ -> true
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
    | "!=" -> EXCLAMATION_EQ
    | "&&" -> BIG_AMPERSAND
    | "++" -> BIG_PLUS
    | "+=" -> PLUS_EQ
    | "," -> COMMA
    | "-" -> HYPHEN
    | "--" -> BIG_HYPHEN
    | "-=" -> HYPHEN_EQ
    | "->" -> HYPHEN_GREATER
    | "::" -> BIG_COLON
    | ":=" -> COLON_EQ
    | "==" -> BIG_EQ
    | "=>" -> EQ_GREATER
    | "[" -> LBRACKET
    | "]" -> RBRACKET
    | "case" -> CASE
    | "effect" -> EFFECT
    | "else" -> ELSE
    | "exception" -> EXCEPTION
    | "false" -> BOOL false
    | "fn" -> FN
    | "if" -> IF
    | "lazy" -> LAZY
    | "let" -> LET
    | "match" -> MATCH
    | "raise" -> RAISE
    | "then" -> THEN
    | "true" -> BOOL true
    | "type" -> TYPE
    | "val" -> VAL
    | "where" -> WHERE
    | "||" -> BIG_VERTICAL
    | '!' -> EXCLAMATION
    | '#' -> NUMBERSIGN
    | '$' -> DOLLAR
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
    | '^' -> CIRCUMFLEX
    | '_' -> LOWLINE
    | '{' -> LCBRACKET
    | '|' -> VERTICAL
    | '}' -> RCBRACKET
    | '~' -> TILDE
    | ( ( 'a' .. 'z'
        | 'A' .. 'Z'
        | '_', ('_' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9') )
      , Star ('a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9') ) ->
        IDENTIFIER (lexeme lexbuf)
    | '_' -> LOWLINE
    | "``" -> failwith "empty identifier"
    | '`' -> IDENTIFIER (read_quoted_identifier lexbuf '`')
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
    let aux acc =
      match read_raw_token lexbuf with
      | EOF -> List.rev (EOF :: acc)
      | t -> t :: acc
    in
    aux []
  in
  let filter_nl tokens =
    let rec aux stack acc = function
      | LCBRACKET :: tl -> aux (true :: stack) (LCBRACKET :: acc) tl
      | (LPAREN | LBRACKET) :: tl -> aux (false :: stack) (LCBRACKET :: acc) tl
      | (RCBRACKET | RPAREN | RBRACKET) :: tl ->
          aux (List.tl stack) (LCBRACKET :: acc) tl
      | NL :: tl when not (List.hd stack) -> aux stack acc tl
      | hd :: tl -> aux stack (hd :: acc) tl
      | [] -> List.rev acc
    in
    aux [true] [] tokens
  in
  let autoinsert_semicolon tokens =
    let rec aux acc = function
      | t1 :: (NL as t2) :: t3 :: tl
        when is_expression_end t1 && is_expression_start t3 ->
          aux (t3 :: t2 :: t1 :: acc) tl
      | t1 :: NL :: t3 :: tl -> aux (t3 :: t1 :: acc) tl
      | NL :: tl -> aux acc tl
      | hd :: tl -> aux (hd :: acc) tl
      | [] -> List.rev acc
    in
    aux [] tokens
  in
  fun lexbuf -> read_raw_tokens lexbuf |> filter_nl |> autoinsert_semicolon
