(* Copyright (C) 2018-2019 Takezoe,Tomoaki <tomoaki@res.ac>
 *
 * SPDX-Identifier: LGPL-3.0-or-later
 *)
open Mullos_parser

type context = {
  mutable newline_region_stack: bool list;
  mutable token_buf: token list;
  mutable offside_stack: int list;
}

let is_expression_start = function
  | AMPERSAND
  | ASTERISK
  | BOOL _
  | CIRCUMFLEX
  | EXCLAMATION
  | FN
  | GOTO
  | HYPHEN
  | IDENTIFIER
  | IF
  | LABEL
  | LCBRACKET
  | LET
  | LPAREN
  | NUMBER
  | PLUS
  | RAISE
  | TEXT _ -> true
  | _ -> false

let is_expression_end = function
  | BOOL _
  | IDENTIFIER
  | NUMBER
  | RCBRACKET
  | RPAREN
  | TEXT _
  | TYPEVAR_IDENTIFIER
  | TYPE_IDENTIFIER -> true
  | _ -> false

let new_reader () =
  let context = { newline_region_stack = [true]; token_buf= []; offside_stack = [] } in
  let push_newline_region enabled = context.newline_region_stack <- enabled :: context.newline_region_stack in
  let pop_newline_region enabled =
    match context.newline_region_stack with
    | h :: t when h = enabled -> context.newline_region_stack <- t
    | _ -> failwith "inconsistent lexer context" in
  let rec read lexbuf =
    match context.token_buf with
    | h :: t ->
      context.token_buf <- t;
      h
    | [] ->
      begin match read_raw lexbuf with
      | t1 when is_expression_end t1 ->
        begin match read_raw lexbuf with
        | NL _ as t2 ->
          let t3 = read_raw lexbuf in
          if is_expression_start t3 then
            begin
              context.token_buf <- [t2; t3];
              t1
            end
          else
            begin
              context.token_buf <- [t3];
              t1
            end
        | t2 ->
          context.token_buf <- [t2];
          t1
        end
      | WHERE ->
        begin match read_raw lexbuf with
        | NL offside ->
          begin match read_raw lexbuf with
          | LCBRACKET ->
            context.token_buf <- [LCBRACKET];
            WHERE
          | t3 ->
            context.offside_stack <- offside :: context.offside_stack;
            context.token_buf <- [LCBRACKET; t3];
            WHERE
          end
        | t2 ->
          context.token_buf <- [t2];
          WHERE
        end
      | NL indentation ->
        let rec aux  = function
          | buf, h :: t when h > indentation -> aux (RCBRACKET :: buf,  t)
          | x -> x in
        let (buf, stack) = aux ([], context.offside_stack) in
        context.offside_stack <- stack;
        context.token_buf <- List.append buf context.token_buf;
        read lexbuf
      | t1 -> t1
      end
  and read_newline lexbuf =
    match%sedlex lexbuf with
    | '\n' ->
      Sedlexing.new_line lexbuf;
      read_newline lexbuf
    | _ -> read_indentation lexbuf
  and read_indentation lexbuf =
    let i = { contents = 0 } in
    match%sedlex lexbuf with
    | ' ' ->
      i := !i + 1;
      read_indentation lexbuf
    | '\t' ->
      i := !i + 4;
      read_indentation lexbuf
    | _ -> NL !i
  and read_raw lexbuf =
    match%sedlex lexbuf with
    | Plus (' ' | '\t') -> read lexbuf
    | Plus '\n' ->
      Sedlexing.new_line lexbuf;
      if (List.hd context.newline_region_stack) then
        read_newline lexbuf
      else
        read_raw lexbuf
    | "!=" -> EXCLAMATION_EQ
    | "&&" -> BIG_AMPERSAND
    | "++" -> BIG_PLUS
    | "+=" -> PLUS_EQ
    | "," -> COMMA
    | "-" -> HYPHEN
    | "--" -> BIG_HYPHEN
    | "-=" -> HYPHEN_EQ
    | "->" -> HYPHEN_GREATER
    | ".." -> BIG_DOT
    | "::" -> BIG_COLON
    | ":=" -> COLON_EQ
    | "==" -> BIG_EQ
    | "=>" ->
      pop_newline_region false;
      EQ_GREATER
    | "case" ->
      push_newline_region false;
      CASE
    | "catch" -> CATCH
    | "class" -> CLASS
    | "data" -> DATA
    | "def" -> DEF
    | "deriving" -> DERIVING
    | "else" -> ELSE
    | "external" -> EXTERNAL
    | "false" -> BOOL false
    | "fn" -> FN
    | "goto" -> GOTO
    | "if" -> IF
    | "instance" -> INSTANCE
    | "internal" -> INTERNAL
    | "label" -> LABEL
    | "let" -> LET
    | "match" -> MATCH
    | "raise" -> RAISE
    | "then" -> THEN
    | "true" -> BOOL true
    | "type" -> TYPE
    | "unsafe" -> UNSAFE
    | "where" -> WHERE
    | "||" -> BIG_VERTICAL
    | '!' -> EXCLAMATION
    | '#' -> NUMBERSIGN
    | '$' -> DOLLAR
    | '&' -> AMPERSAND
    | '(' ->
      push_newline_region false;
      LPAREN
    | ')' ->
      pop_newline_region false;
      RPAREN
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
    | '{' ->
      push_newline_region true;
      LCBRACKET
    | '|' -> VERTICAL
    | '}' ->
      pop_newline_region true;
      RCBRACKET
    | Plus (('a' .. 'z') | '_') -> IDENTIFIER
    | ('A' .. 'Z'), Star ('a' .. 'z') -> TYPE_IDENTIFIER
    | '`', Plus (('a' .. 'z') | '_') -> TYPEVAR_IDENTIFIER
    | 0x03b1 .. 0x03c9 -> TYPEVAR_IDENTIFIER
    | '"' -> lex_text lexbuf
    | ('1' .. '9'), Star ('0' .. '9'), Opt (('i' | 'u'), ('1' .. '9'), Star ('0' .. '9')) -> NUMBER
    | ('1' .. '9'), Star ('0' .. '9'), 'u', ('1' .. '9'), Star ('0' .. '9') -> NUMBER
    | ('1' .. '9'), Star ('0' .. '9') -> NUMBER
    | "0x", Plus (('0' .. '9') | ('a' .. 'f')) , Opt (('i' | 'u'), ('1' .. '9'), Star ('0' .. '9')) -> NUMBER
    | "0b", Plus ('0' | '1') , Opt (('i' | 'u'), ('1' .. '9'), Star ('0' .. '9')) -> NUMBER
    | '0', Star ('0' .. '7') , Opt (('i' | 'u'), ('1' .. '9'), Star ('0' .. '9')) -> NUMBER
    | Plus ('0' .. '9'), '.', Plus ('0' .. '9'), Opt ('E', Opt ('+' | '-'), Plus ('0' .. '9')), Opt ('f', Plus ('0' .. '9')) -> NUMBER
    | '.', Plus ('0' .. '9'), Opt ('E', Opt ('+' | '-'), Plus ('0' .. '9')), Opt ('f', Plus ('0' .. '9')) -> NUMBER
    | Plus ('0' .. '9'), '.', Opt ('E', Opt ('+' | '-'), Plus ('0' .. '9')), Opt ('f', Plus ('0' .. '9')) -> NUMBER
    | _ -> failwith ""
  and lex_text lexbuf =
    let buf = CamomileLibrary.UTF8.Buf.create 1024 in
    let rec aux () =
      match%sedlex lexbuf with
      | '"' -> TEXT (Buffer.contents buf)
      | eof -> failwith "unexpeted EOF"
      | '\\' ->
        CamomileLibrary.UTF8.Buf.add_char buf (lex_escape lexbuf);
        aux ()
      | any ->
        CamomileLibrary.UTF8.Buf.add_string buf (Sedlexing.Utf8.lexeme lexbuf);
        aux ()
      | _ -> failwith "unreachable" in
    aux ()
  and lex_escape lexbuf =
    match%sedlex lexbuf with
    | 'u' -> lex_unicode_escape lexbuf 4
    | 'U' -> lex_unicode_escape lexbuf 8
    | '\'' -> CamomileLibrary.UChar.of_char '\''
    | '"' -> CamomileLibrary.UChar.of_char '"'
    | '\\' -> CamomileLibrary.UChar.of_char '\\'
    | 'a' -> CamomileLibrary.UChar.of_int 0x07
    | 'b' -> CamomileLibrary.UChar.of_char '\b'
    | 'f' -> CamomileLibrary.UChar.of_int 0x0c
    | 'n' -> CamomileLibrary.UChar.of_char '\n'
    | 'r' -> CamomileLibrary.UChar.of_char '\r'
    | 't' -> CamomileLibrary.UChar.of_char '\t'
    | 'v' -> CamomileLibrary.UChar.of_int 0x0b
    | _ -> failwith "Invalid escape sequence"
  and lex_unicode_escape lexbuf limit =
    let v = ref 0 in
    let i = ref 0 in
    if !i = limit then
      CamomileLibrary.UChar.of_int !i
    else
      let aux base =  Uchar.to_int (Sedlexing.lexeme_char lexbuf 0) - Char.code base in
      match%sedlex lexbuf with
      | '0' .. '9' ->
        v := !v * 16 + aux '0';
        lex_unicode_escape lexbuf limit
      | 'a' .. 'f' ->
        v := !v * 16 + aux 'a' + 10;
        lex_unicode_escape lexbuf limit
      | 'A' .. 'F' ->
        v := !v * 16 + aux 'A' + 10;
        lex_unicode_escape lexbuf limit
      | _ -> failwith "unexpecd end of unicode escape"
    in
  read

