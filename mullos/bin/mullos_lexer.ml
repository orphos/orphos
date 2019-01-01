(* Copyright (C) 2018 Takezoe,Tomoaki <tomoaki@res.ac>
 *
 * SPDX-Identifier: LGPL-3.0-or-later
 *)
open Mullos_parser

type context = {
  mutable newline_region_stack: bool list;
  mutable token_buf: token list;
}

let expression_start = [
  LPAREN;
  LCBRACKET;
  LET;
  EXCLAMATION;
  PLUS;
  HYPHEN;
  CIRCUMFLEX;
  AMPERSAND;
  ASTERISK;
  IF;
  TEXT;
  NUMBER;
  BOOL;
  FN;
  RAISE;
  IDENTIFIER;
  GOTO;
  LABEL;
]

let expression_end = [
  RPAREN;
  RCBRACKET;
  TYPE_IDENTIFIER;
  TYPEVAR_IDENTIFIER;
  TEXT;
  NUMBER;
  BOOL;
  IDENTIFIER;
]

let new_lexer () =
  let context = { newline_region_stack = [true]; token_buf= [] } in
  let push_newline_region enabled = context.newline_region_stack <- enabled :: context.newline_region_stack in
  let pop_newline_region enabled =
    match context.newline_region_stack with
    | h :: t when h = enabled -> context.newline_region_stack <- t
    | _ -> failwith "inconsistent lexer context" in
  let rec lex lexbuf =
    match context.token_buf with
    | h :: t ->
      context.token_buf <- t;
      h
    | [] ->
      let t1 = lex_impl lexbuf in
      if List.mem t1 expression_end then
        begin match lex_impl lexbuf with
        | SEMI true ->
          let t3 = lex_impl lexbuf in
          if List.mem t3 expression_start then
            begin
              context.token_buf <- [SEMI true; t3];
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
      else
        t1
  and lex_impl lexbuf =
    match%sedlex lexbuf with
    | Plus (' ' | '\t') -> lex lexbuf
    | '\n', Plus (' ' | '\t') ->
      if (List.hd context.newline_region_stack) then
        SEMI true
      else
        lex_impl lexbuf
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
    | "false" -> BOOL
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
    | "true" -> BOOL
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
    | ';' -> SEMI false
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
    | '"', Sub (any, '"'), '"' -> TEXT
    | ('1' .. '9'), Star ('0' .. '9'), Opt (('i' | 'u'), ('1' .. '9'), Star ('0' .. '9')) -> NUMBER
    | ('1' .. '9'), Star ('0' .. '9'), 'u', ('1' .. '9'), Star ('0' .. '9') -> NUMBER
    | ('1' .. '9'), Star ('0' .. '9') -> NUMBER
    | "0x", Plus (('0' .. '9') | ('a' .. 'f')) , Opt (('i' | 'u'), ('1' .. '9'), Star ('0' .. '9')) -> NUMBER
    | "0b", Plus ('0' | '1') , Opt (('i' | 'u'), ('1' .. '9'), Star ('0' .. '9')) -> NUMBER
    | '0', Star ('0' .. '7') , Opt (('i' | 'u'), ('1' .. '9'), Star ('0' .. '9')) -> NUMBER
    | Plus ('0' .. '9'), '.', Plus ('0' .. '9'), Opt ('E', Opt ('+' | '-'), Plus ('0' .. '9')), Opt ('f', Plus ('0' .. '9')) -> NUMBER
    | '.', Plus ('0' .. '9'), Opt ('E', Opt ('+' | '-'), Plus ('0' .. '9')), Opt ('f', Plus ('0' .. '9')) -> NUMBER
    | Plus ('0' .. '9'), '.', Opt ('E', Opt ('+' | '-'), Plus ('0' .. '9')), Opt ('f', Plus ('0' .. '9')) -> NUMBER
    | _ -> failwith "" in
  lex

