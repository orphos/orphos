(* Copyright (C) 2018-2019 Takezoe,Tomoaki <tomoaki@res.ac>
 *
 * SPDX-Identifier: LGPL-3.0-or-later
 *)
open Mullos_parser
open Mullos_syntax

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
  | HYPHEN
  | IDENTIFIER _
  | IF
  | LAZY
  | LCBRACKET
  | LET
  | LPAREN
  | NUMBER _
  | PLUS
  | QUESTION
  | RAISE
  | TEXT _
  | TILDE -> true
  | _ -> false

let is_expression_end = function
  | BOOL _
  | IDENTIFIER _
  | NUMBER _
  | RCBRACKET
  | RPAREN
  | TEXT _
  | TYPEVAR_IDENTIFIER _
  | TYPE_IDENTIFIER _ -> true
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
    | "#![" -> NUMBERSIGN_EXCLAMATION_LBRACKET
    | "#[" -> NUMBERSIGN_LBRACKET
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
    | "[" -> LBRACKET
    | "]" -> RBRACKET
    | "case" ->
      push_newline_region false;
      CASE
    | "catch" -> CATCH
    | "class" -> CLASS
    | "def" -> DEF
    | "deriving" -> DERIVING
    | "else" -> ELSE
    | "external" -> EXTERNAL
    | "false" -> BOOL false
    | "fn" -> FN
    | "if" -> IF
    | "instance" -> INSTANCE
    | "internal" -> INTERNAL
    | "lazy" -> LAZY
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
    | '?' -> QUESTION
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
    | '~' -> TILDE
    | ('a' .. 'z'), Star (('a' .. 'z') | '_') -> IDENTIFIER (Sedlexing.Utf8.lexeme lexbuf)
    | '_' , Plus (('a' .. 'z') | '_') -> IDENTIFIER (Sedlexing.Utf8.lexeme lexbuf)
    | '_' -> LOWLINE
    | "``" -> failwith "empty identifier"
    | '`' -> IDENTIFIER (read_quoted_identifier lexbuf '`')
    | "\'\'", Plus (('a' .. 'z') | '_') -> TYPEVAR_IDENTIFIER (Sedlexing.Utf8.lexeme lexbuf)
    | 0x03b1 .. 0x03c9 (* α .. ω *) -> TYPEVAR_IDENTIFIER (Sedlexing.Utf8.lexeme lexbuf)
    | ('A' .. 'Z'), Star ('a' .. 'z') -> TYPE_IDENTIFIER (Sedlexing.Utf8.lexeme lexbuf)
    | '\'' -> TYPE_IDENTIFIER (read_quoted_identifier lexbuf '\'')
    | Star '_', ('A' .. 'Z'), Plus ('A' .. 'Z' | '_') -> CTOR_IDENTIFIER (Sedlexing.Utf8.lexeme lexbuf)
    | "#(" -> CTOR_IDENTIFIER (read_quoted_identifier lexbuf ')')
    | '"' -> TEXT (read_text lexbuf)
    | ('1' .. '9') ->
      Sedlexing.rollback lexbuf;
      NUMBER (read_number lexbuf 10)
    | "0x" -> NUMBER (read_number lexbuf 16)
    | "0b" -> NUMBER (read_number lexbuf 2)
    | "0" ->
      Sedlexing.rollback lexbuf;
      NUMBER (read_number lexbuf 8)
    | "#\"" -> TEXTTYPE (read_text lexbuf)
    | "#true" -> BOOLTYPE true
    | "#false" -> BOOLTYPE false
    | '#', ('1' .. '9') ->
      Sedlexing.rollback lexbuf;
      NUMBERTYPE (read_number lexbuf 10)
    | "#0x" -> NUMBERTYPE (read_number lexbuf 16)
    | "#0b" -> NUMBERTYPE (read_number lexbuf 2)
    | "#0" ->
      Sedlexing.rollback lexbuf;
      NUMBERTYPE (read_number lexbuf 8)
    | _ -> failwith ""
  and read_text lexbuf =
    let buf = CamomileLibrary.UTF8.Buf.create 1024 in
    let rec aux () =
      match%sedlex lexbuf with
      | '"' -> Buffer.contents buf
      | eof -> failwith "unexpeted EOF"
      | '\\' ->
        CamomileLibrary.UTF8.Buf.add_char buf (read_escape lexbuf);
        aux ()
      | any ->
        CamomileLibrary.UTF8.Buf.add_string buf (Sedlexing.Utf8.lexeme lexbuf);
        aux ()
      | _ -> failwith "unreachable" in
    aux ()
  and read_escape lexbuf =
    match%sedlex lexbuf with
    | 'u' -> read_unicode_escape lexbuf 4
    | 'U' -> read_unicode_escape lexbuf 8
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
  and read_unicode_escape lexbuf limit =
    let v = ref 0 in
    let i = ref 0 in
    if !i = limit then
      CamomileLibrary.UChar.of_int !i
    else
      let aux base =  Uchar.to_int (Sedlexing.lexeme_char lexbuf 0) - Char.code base in
      match%sedlex lexbuf with
      | '0' .. '9' ->
        v := !v * 16 + aux '0';
        read_unicode_escape lexbuf limit
      | 'a' .. 'f' ->
        v := !v * 16 + aux 'a' + 10;
        read_unicode_escape lexbuf limit
      | 'A' .. 'F' ->
        v := !v * 16 + aux 'A' + 10;
        read_unicode_escape lexbuf limit
      | _ -> failwith "unexpecd end of unicode escape"
  and read_number lexbuf radix =
    let i = ref 0 in
    let e = ref 0 in
    let reading_exponent = ref false in
    let number_literal_type = ref ZType in
    let create_token () = (Q.make (Z.of_int !i) (Z.mul (Z.of_int !e) (Z.of_int 10)), !number_literal_type) in
    let rec read_digit base_char base_num =
      Sedlexing.rollback lexbuf;
      i := !i * radix + (Uchar.to_int (Sedlexing.lexeme_char lexbuf 0) - Char.code base_char) + base_num;
      if !reading_exponent then
        e := !e + 1;
      loop ()
    and loop () =
      match%sedlex lexbuf with
      | '0' .. '1' ->
        read_digit '0' 0
      | '2' .. '7' ->
        if radix < 8 then
          begin
            Sedlexing.rollback lexbuf;
            create_token ()
          end
        else read_digit '0' 0
      | '2' .. '9' ->
        if radix < 10 then
          begin
            Sedlexing.rollback lexbuf;
            create_token ()
          end
        else read_digit '0' 0
      | 'a' .. 'e' ->
        if radix < 16 then
          begin
            Sedlexing.rollback lexbuf;
            create_token ()
          end
        else read_digit 'a' 10
      | 'A' .. 'F' ->
        if radix < 16 then
          begin
            Sedlexing.rollback lexbuf;
            create_token ()
          end
        else read_digit 'A' 10
      | '.' ->
        if !reading_exponent then
          begin
            Sedlexing.rollback lexbuf;
            create_token ()
          end
        else
          begin
            reading_exponent := true;
            number_literal_type := QType;
            loop ()
          end
      | 'i' ->
        number_literal_type := IntType (read_precision lexbuf);
        create_token ()
      | 'u' ->
        number_literal_type := IntType (read_precision lexbuf);
        create_token ()
      | 'f' ->
        if radix = 10 then
          begin
            number_literal_type := FloatType (read_precision lexbuf);
            create_token ()
          end
        else if radix < 16 then
          begin
            Sedlexing.rollback lexbuf;
            create_token ()
          end
        else read_digit 'a' 10
      | _ -> create_token () in
    loop ()
  and read_precision lexbuf =
    let i = ref 0 in
    let rec loop () =
      match%sedlex lexbuf with
      | '0' .. '9' ->
        i := !i * 10 + Uchar.to_int (Sedlexing.lexeme_char lexbuf 0) - Char.code '0';
        loop ()
      | _ -> !i in
    loop ()
  and read_quoted_identifier lexbuf mark =
    let buf = CamomileLibrary.UTF8.Buf.create 1024 in
    let rec loop () =
      match%sedlex lexbuf with
      | any ->
        let c = Sedlexing.lexeme_char lexbuf 0 in
        if c = Uchar.of_char mark then
          CamomileLibrary.UTF8.Buf.contents buf
        else
          begin
            CamomileLibrary.UTF8.Buf.add_string buf (Sedlexing.Utf8.lexeme lexbuf);
            loop ()
          end
      | _ -> failwith "unreachable" in
    loop ()
    in
  read

