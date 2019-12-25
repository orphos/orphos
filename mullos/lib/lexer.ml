(* Copyright (C) 2018-2019 Takezoe,Tomoaki <tomoaki3478@res.ac>
 *
 * SPDX-Identifier: Apache-2.0 WITH LLVM-exception
 *)
open Syntax

module Make (Data : Syntax.Data) = struct
  module Parser = Parser.Make (Data)
  open Parser

  let is_auto_semi_followed = function
    | IF _ | MATCH _ | FN _ | LET _ | LBRACKET _ | NUMBERSIGN _ | LCBRACKET _ | GRAVE_ACCENT _ | PLUS _ | HYPHEN _ | EXCLAMATION _
    | AMPERSAND _ | ASTERISK _ | BIG_PLUS _ | BIG_HYPHEN _ | RAISE _ | LAZY _ | TILDE _ | UPPER_IDENTIFIER _ | LOWER_IDENTIFIER _
    | LPAREN _ | TEXT _ | NUMBER _ | BOOL _ | TYPE _ | VAL _ | EXCEPTION _ ->
        true
    | _ -> false

  let is_followed_by_auto_semi = function
    | END _ | RBRACKET _ | RCBRACKET _ | BIG_PLUS _ | BIG_HYPHEN _ | UPPER_IDENTIFIER _ | LOWER_IDENTIFIER _ | RPAREN _ | TEXT _
    | NUMBER _ | BOOL _ ->
        true
    | _ -> false

  module Buf = CamomileLibrary.UTF8.Buf
  module UChar = CamomileLibrary.UChar

  let new_reader () =
    let rollback = Sedlexing.rollback in
    let lexeme = Sedlexing.Utf8.lexeme in
    let lexeme_char = Sedlexing.lexeme_char in
    let loc lexbuf = let start, _ = Sedlexing.lexing_positions lexbuf in start in
    let rec read_newline lexbuf =
      match%sedlex lexbuf with
      | '\n' ->
          Sedlexing.new_line lexbuf;
          read_newline lexbuf
      | ' ' | '\t' -> read_newline lexbuf
      | _ ->
          rollback lexbuf;
          NL (loc lexbuf)
    in
    let read_unicode_escape lexbuf limit =
      let rec loop acc = function
        | i when i = limit -> UChar.of_int i
        | i -> (
            let ret base_char base_value =
              loop
                ((acc * 16) + (Uchar.to_int (Sedlexing.lexeme_char lexbuf 0) - Char.code base_char) + base_value)
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
    let read_text lexbuf =
      let buf = Buf.create 1024 in
      let rec aux () =
        match%sedlex lexbuf with
        | '"' -> Buffer.contents buf
        | eof -> failwith "unexpeted EOF"
        | '\\' ->
            Buf.add_char buf (read_escape lexbuf);
            aux ()
        | any ->
            Buf.add_string buf (lexeme lexbuf);
            aux ()
        | _ -> failwith "unreachable"
      in
      aux ()
    in
    let read_precision lexbuf =
      let rec aux acc =
        match%sedlex lexbuf with
        | '0' .. '9' -> aux ((acc * 10) + Uchar.to_int (lexeme_char lexbuf 0) - Char.code '0')
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
        if radix < digit_value then (
          rollback lexbuf;
          ret m e n )
        else aux ((m * radix) + digit_value) (if reading_exponent then e + 1 else e) n reading_exponent
      and aux m e n reading_exponent =
        match%sedlex lexbuf with
        | '0' .. '9' -> proceed m e n '0' 0 reading_exponent
        | 'a' .. 'e' -> proceed m e n 'a' 10 reading_exponent
        | 'A' .. 'F' -> proceed m e n 'A' 10 reading_exponent
        | '.' ->
            if reading_exponent then (
              rollback lexbuf;
              ret m e n )
            else aux m e QType true
        | 'i' -> ret m e (IntType (read_precision lexbuf))
        | 'u' -> ret m e (IntType (read_precision lexbuf))
        | 'f' ->
            if radix = 10 then ret m e (FloatType (read_precision lexbuf))
            else (
              rollback lexbuf;
              ret m e n )
        | _ -> ret m e n
      in
      aux 0 0 ZType false
    in
    let rec read_raw_token lexbuf =
      let loc () = loc lexbuf in
      match%sedlex lexbuf with
      | Plus (' ' | '\t') -> read_raw_token lexbuf
      |  '\n' ->
          Sedlexing.new_line lexbuf;
          read_newline lexbuf
      (* three character symbols *)
      | ":+:" -> COLON_PLUS_COLON (loc ())
      | ":-:" -> COLON_HYPHEN_COLON (loc ())
      (* two character symbols *)
      | "!=" -> EXCLAMATION_EQ (loc ())
      | "&&" -> BIG_AMPERSAND (loc ())
      | "++" -> BIG_PLUS (loc ())
      | "+:" -> PLUS_COLON (loc ())
      | "+=" -> PLUS_EQ (loc ())
      | "," -> COMMA (loc ())
      | "--" -> BIG_HYPHEN (loc ())
      | "-:" -> HYPHEN_COLON (loc ())
      | "-=" -> HYPHEN_EQ (loc ())
      | "->" -> HYPHEN_GREATER (loc ())
      | ":+" -> COLON_PLUS (loc ())
      | ":-" -> COLON_HYPHEN (loc ())
      | "::" -> BIG_COLON (loc ())
      | ":=" -> COLON_EQ (loc ())
      | "<<" -> BIG_LESS (loc ())
      | "==" -> BIG_EQ (loc ())
      | "=>" -> EQ_GREATER (loc ())
      | ">>" -> BIG_GREATER (loc ())
      | "[" -> LBRACKET (loc ())
      | "]" -> RBRACKET (loc ())
      | "|>" -> VERTICAL_GREATER (loc ())
      | "||" -> BIG_VERTICAL (loc ())
      (* raw identifiers starting with uppercase character *)
      | 'A' .. 'Z', Star ('a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9') -> UPPER_IDENTIFIER (loc (), lexeme lexbuf)
      (* raw identifiers starting with lowercase or lowline character, or keyword *)
      | ('a' .. 'z' | '_'), Star ('a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9') -> (
          match lexeme lexbuf with
          | "and" -> AND (loc ())
          | "as" -> AS (loc ())
          | "case" -> CASE (loc ())
          | "effect" -> EFFECT (loc ())
          | "else" -> ELSE (loc ())
          | "end" -> END (loc ())
          | "exception" -> EXCEPTION (loc ())
          | "false" -> BOOL (loc (), false)
          | "fn" -> FN (loc ())
          | "given" -> GIVEN (loc ())
          | "handle" -> HANDLE (loc ())
          | "if" -> IF (loc ())
          | "interface" -> INTERFACE (loc ())
          | "lazy" -> LAZY (loc ())
          | "let" -> LET (loc ())
          | "match" -> MATCH (loc ())
          | "module" -> MODULE (loc ())
          | "of" -> OF (loc ())
          | "raise" -> RAISE (loc ())
          | "rec" -> REC (loc ())
          | "then" -> THEN (loc ())
          | "true" -> BOOL (loc (), true)
          | "type" -> TYPE (loc ())
          | "val" -> VAL (loc ())
          | "when" -> WHEN (loc ())
          | "where" -> WHERE (loc ())
          | "with" -> WITH (loc ())
          | "without" -> WITHOUT (loc ())
          | id -> LOWER_IDENTIFIER (loc (), id) )
      (* quoted lower-identifier *)
      | "${", Sub (any, '}'), '}' ->
          let s = lexeme lexbuf in
          LOWER_IDENTIFIER (loc (), String.sub s 2 (String.length s - 3))
      (* quoted upper-identifier *)
      | "$[", Sub (any, ']'), ']' ->
          let s = lexeme lexbuf in
          UPPER_IDENTIFIER (loc (), String.sub s 2 (String.length s - 3))
      (* lower identifier prefixed with $ *)
      | '$', ('a' .. 'z' | '_'), Star ('a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9') ->
          let s = lexeme lexbuf in
          LOWER_IDENTIFIER (loc (), String.sub s 1 (String.length s - 1))
      (* upper identifier prefixed with $ *)
      | '$', ('A' .. 'Z'), Star ('a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9') ->
        let s = lexeme lexbuf in
        UPPER_IDENTIFIER (loc (), String.sub s 1 (String.length s - 1))
      (* one character symbols *)
      | "-" -> HYPHEN (loc ())
      | '!' -> EXCLAMATION (loc ())
      | '#' -> NUMBERSIGN (loc ())
      | '%' -> PERCENT (loc ())
      | '&' -> AMPERSAND (loc ())
      | '(' -> LPAREN (loc ())
      | ')' -> RPAREN (loc ())
      | '*' -> ASTERISK (loc ())
      | '+' -> PLUS (loc ())
      | '.' -> DOT (loc ())
      | '/' -> SOLIDUS (loc ())
      | ':' -> COLON (loc ())
      | ';' -> SEMI (loc ())
      | '<' -> LESS (loc ())
      | '=' -> EQ (loc ())
      | '>' -> GREATER (loc ())
      | '@' -> AT (loc ())
      | '\'' -> SINGLE_QUOTE (loc ())
      | '^' -> CIRCUMFLEX (loc ())
      | '_' -> LOWLINE (loc ())
      | '_' -> LOWLINE (loc ())
      | '`' -> GRAVE_ACCENT (loc ())
      | '{' -> LCBRACKET (loc ())
      | '|' -> VERTICAL (loc ())
      | '}' -> RCBRACKET (loc ())
      | '~' -> TILDE (loc ())
      | '"' -> TEXT (loc (), read_text lexbuf)
      | "0", '0' .. '7' ->
          rollback lexbuf;
          NUMBER (loc (), read_number lexbuf 8)
      | "0x" -> NUMBER (loc (), read_number lexbuf 16)
      | "0b" -> NUMBER (loc (), read_number lexbuf 2)
      | '0' .. '9' ->
          rollback lexbuf;
          NUMBER (loc (), read_number lexbuf 10)
      | _ -> EOF (loc ())
    in
    let read_raw_tokens lexbuf =
      (* read all tokens *)
      let aux acc = match read_raw_token lexbuf with EOF _ as eof -> List.rev (eof :: acc) | t -> t :: acc in
      aux []
    in
    let filter_nl tokens =
      (* Remove NL inside auto semicolon disabled region *)
      let rec aux acc = function
        | stack, (LCBRACKET _ as sym) :: tl -> aux (sym :: acc) (true :: stack, tl)
        | stack, ((LPAREN  _ | LBRACKET _) as sym) :: tl -> aux (sym :: acc) (false :: stack, tl)
        | _ :: stack, ((RCBRACKET  _ | RPAREN  _ | RBRACKET _) as sym) :: tl -> aux (sym :: acc) (stack, tl)
        | (false :: _ as stack), NL _ :: tl -> aux acc (stack, tl)
        | stack, hd :: tl -> aux (hd :: acc) (stack, tl)
        | _, [] -> List.rev acc
      in
      aux [] ([ true ], tokens)
    in
    let autoinsert_semicolon tokens =
      (* Remove NL unless semicolon should be auto inserted *)
      let rec aux acc = function
        | NL _ :: tl -> aux acc tl
        | t1 :: (NL _ as t2) :: t3 :: tl when is_auto_semi_followed t1 && is_followed_by_auto_semi t3 ->
            aux (t3 :: t2 :: t1 :: acc) tl
        | hd :: tl -> aux (hd :: acc) tl
        | [] -> List.rev acc
      in
      aux [] tokens
    in
    fun lexbuf -> read_raw_tokens lexbuf |> filter_nl |> autoinsert_semicolon
end
