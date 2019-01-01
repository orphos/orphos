(* Copyright (C) 2018 Takezoe,Tomoaki <tomoaki@res.ac>
 *
 * SPDX-Identifier: LGPL-3.0-or-later
 *)
open Mullos_parser

type context = {
  mutable newline_region_stack: bool list;
  mutable token_buf: token list;
  mutable offside_stack: int list;
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
  let context = { newline_region_stack = [true]; token_buf= []; offside_stack = [] } in
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
      begin match lex_impl lexbuf with
      | t1 when List.mem t1 expression_end ->
        begin match lex_impl lexbuf with
        | NL _ as t2 ->
          let t3 = lex_impl lexbuf in
          if List.mem t3 expression_start then
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
        begin match lex_impl lexbuf with
        | NL offside ->
          begin match lex_impl lexbuf with
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
        lex lexbuf
      | t1 -> t1
      end
  and lex_newline lexbuf =
    match%sedlex lexbuf with
    | '\n' ->
      Sedlexing.new_line lexbuf;
      lex_newline lexbuf
    | _ -> lex_indentation lexbuf
  and lex_indentation lexbuf =
    let i = { contents = 0 } in
    match%sedlex lexbuf with
    | ' ' ->
      i := !i + 1;
      lex_indentation lexbuf
    | '\t' ->
      i := !i + 4;
      lex_indentation lexbuf
    | _ -> NL !i
  and lex_impl lexbuf =
    match%sedlex lexbuf with
    | Plus (' ' | '\t') -> lex lexbuf
    | Plus '\n' ->
      Sedlexing.new_line lexbuf;
      if (List.hd context.newline_region_stack) then
        lex_newline lexbuf
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

