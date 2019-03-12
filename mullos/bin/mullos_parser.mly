(* Copyright (C) 2018-2019 Takezoe,Tomoaki <tomoaki@res.ac>
 *
 * SPDX-Identifier: LGPL-3.0-or-later
 *)
%{

open Mullos_aux
open Mullos_syntax

let noimpl () = failwith "not implemented"

%}

%token AMPERSAND
%token AND
%token ASTERISK
%token AT
%token BIGBIG_COLON
%token BIG_AMPERSAND
%token BIG_COLON
%token BIG_DOT
%token BIG_EQ
%token BIG_GREATER
%token BIG_HYPHEN
%token BIG_LESS
%token BIG_PLUS
%token BIG_VERTICAL
%token <bool> BOOL
%token CASE
%token CATCH
%token CIRCUMFLEX
%token CLASS
%token COLON
%token COLON_EQ
%token COLON_HYPHEN
%token COLON_HYPHEN_COLON
%token COLON_PLUS
%token COLON_PLUS_COLON
%token COMMA
%token DEF
%token DERIVING
%token DOLLAR
%token DOT
%token EFFECT
%token ELSE
%token END
%token EOF
%token EQ
%token EQ_GREATER
%token EXCEPTION
%token EXCLAMATION
%token EXCLAMATION_EQ
%token EXTERNAL
%token FN
%token FUNCTOR
%token GRAVE_ACCENT
%token GREATER
%token HANDLE
%token HYPHEN
%token HYPHEN_COLON
%token HYPHEN_EQ
%token HYPHEN_GREATER
%token <string> IDENTIFIER
%token IF
%token INSTANCE
%token INTERNAL
%token LAZY
%token LCBRACKET
%token LBRACKET
%token LBRACKET_VERTICAL
%token LESS
%token LET
%token LOWLINE
%token LPAREN
%token MATCH
%token MODULE
%token MUTABLE
%token NL
%token <Mullos_syntax.number> NUMBER
%token NUMBERSIGN
%token NUMBERSIGN_EXCLAMATION_LBRACKET
%token NUMBERSIGN_LBRACKET
%token OF
%token PERCENT
%token PLUS
%token PLUS_COLON
%token PLUS_EQ
%token QUESTION
%token RAISE
%token RBRACKET
%token RCBRACKET
%token REC
%token RPAREN
%token SEMI
%token SIG
%token SIGNATURE
%token SINGLETON
%token SOLIDUS
%token STRUCT
%token <string> TEXT
%token THEN
%token TILDE
%token TRAIT
%token TYPE
%token TYPE_VARIABLE_ID
%token UNSAFE
%token VAL
%token VERTICAL
%token VERTICAL_GREATER
%token VERTICAL_RBRACKET
%token WHEN
%token WHERE
%token WITH

%start<unit> compilation_unit

%%

compilation_unit: definition_list  EOF { () }

%inline
semi:
  | NL { () }
  | SEMI { () }

long_id:
  IDENTIFIER BIG_COLON long_id { $1 :: $3 }
  | IDENTIFIER { [$1] }

seq:
  | expression semi seq { $1 :: $3 }
  | expression { [$1] }

expression:
  | IF expression THEN expression ELSE expression { IfThenElse ($2, $4, Some $6) }
  | IF expression THEN expression END { IfThenElse ($2, $4, None) }
  | MATCH lhs=expression WITH rhs=pattern_clause+ END { Match (lhs, rhs) }
  | FN pattern HYPHEN_GREATER expression { Lambda ($2, $4) }
  | LET simple_pattern EQ expression semi expression { Let ($2, [], $4, $6) }
  | LET REC simple_pattern EQ expression list(AND simple_pattern EQ expression{ noimpl () }) semi expression { noimpl () }
  | assignment_expression HANDLE pattern_clause+ END { noimpl () }
  | assignment_expression { $1 }
  | LBRACKET separated_list(SEMI, expression) RBRACKET { noimpl () }
  | LBRACKET_VERTICAL separated_list(SEMI, expression) VERTICAL_RBRACKET { noimpl () }
  | MUTABLE LBRACKET_VERTICAL separated_list(SEMI, expression) VERTICAL_RBRACKET { noimpl () }
  | LCBRACKET ioption(expression WITH{}) separated_list(SEMI, DOT IDENTIFIER EQ expression { noimpl () }) RCBRACKET { noimpl () }
  | LCBRACKET DOT IDENTIFIER HYPHEN expression RCBRACKET { noimpl () }
  | GRAVE_ACCENT IDENTIFIER expression { noimpl () }

assignment_expression: assignment_expression binop_assignment pipeline_expression { noimpl () } | pipeline_expression { $1 }
%inline
binop_assignment:
  | PLUS_EQ { `AddAsign }
  | HYPHEN_EQ { `SubstractAsign }
  | COLON_EQ { `Asign }

pipeline_expression: dollar_expression VERTICAL_GREATER pipeline_expression { noimpl () } | dollar_expression { $1 }

dollar_expression: dollar_expression DOLLAR tuple_expression { noimpl () } | tuple_expression { $1 }

tuple_expression: lor_expression nonempty_list(COMMA lor_expression { noimpl () }) { noimpl () } | lor_expression { $1 }

lor_expression: lor_expression BIG_VERTICAL land_expression { noimpl () } | land_expression { $1 }

land_expression: land_expression BIG_AMPERSAND bitwise_or_expression { noimpl () } | bitwise_or_expression { $1 }

bitwise_or_expression: bitwise_or_expression VERTICAL xor_expression { noimpl () } | xor_expression { $1 }

xor_expression: xor_expression CIRCUMFLEX bitwise_and_expression { noimpl () } | bitwise_and_expression { $1 }

bitwise_and_expression: bitwise_and_expression AMPERSAND equal_expression { noimpl () } | equal_expression { $1 }

equal_expression: equal_expression binop_equal greater_expression { noimpl () } | greater_expression { $1 }
%inline
binop_equal:
  | EXCLAMATION_EQ { `NotEqual }
  | BIG_EQ { `Equal }

greater_expression: greater_expression binop_greater shift_expression { noimpl () } | shift_expression { $1 }
%inline
binop_greater:
  | LESS { `Less }
  | GREATER { `Greater }

shift_expression: shift_expression binop_shift cons_expression { noimpl () } | cons_expression { $1 }
%inline
binop_shift:
  | BIG_LESS { `BitwiseLeftShift }
  | BIG_GREATER { `BitwiseRightShift }

cons_expression: add_expression binop_cons cons_expression { noimpl () } | add_expression { $1 }
%inline
binop_cons:
   | COLON_PLUS { noimpl () }
   | COLON_HYPHEN { noimpl () }
   | COLON_PLUS_COLON { noimpl () }
   | COLON_HYPHEN_COLON { noimpl () }

add_expression: add_expression binop_add multiply_expression { noimpl () } | multiply_expression { $1 }
%inline
binop_add:
  | PLUS { `Add }
  | HYPHEN { `Substract }
  | PLUS_COLON { noimpl () }
  | HYPHEN_COLON { noimpl () }

multiply_expression: multiply_expression binop_multiply prefix_expression { noimpl () } | prefix_expression { $1 }
%inline
binop_multiply:
  | ASTERISK { `Multiply }
  | SOLIDUS  { `Division }
  | PERCENT { `Reminder }

prefix_expression: prefix_op postfix_expression { noimpl () } | postfix_expression { $1 }
%inline
prefix_op:
  | PLUS { noimpl () }
  | HYPHEN { noimpl () }
  | EXCLAMATION { noimpl () }
  | AMPERSAND { noimpl () }
  | ASTERISK { noimpl () }
  | BIG_PLUS { noimpl () }
  | BIG_HYPHEN { noimpl () }
  | RAISE { noimpl() }
  | LAZY { noimpl() }

postfix_expression: application_expression postfix_op { noimpl () } | application_expression { $1 }
%inline
postfix_op:
  | BIG_PLUS { noimpl () }
  | BIG_HYPHEN { noimpl () }

application_expression: application_expression dot_expression { Apply ($1, $2) } | dot_expression { $1 }

dot_expression: dot_expression DOT simple_expression { BinOp ($1, `Dot, $3, []) } | simple_expression { $1 }

simple_expression:
  | IDENTIFIER { Identifier $1 }
  | LPAREN RPAREN { Unit }
  | LCBRACKET seq RCBRACKET { Seq $2 }
  | TEXT { Text $1 }
  | NUMBER { Number $1 }
  | BOOL { Bool $1 }
  | LPAREN expression RPAREN { $2 }

pattern_clause:
  | CASE pat=pattern_or_clause cond=pattern_condition? EQ_GREATER exp=expression { MatchPat (pat, cond, exp) }
  | CASE EXCEPTION pat=pattern_or_clause cond=pattern_condition? EQ_GREATER exp=expression { MatchException (pat, cond, exp) }
  | CASE EFFECT pat=pattern_or_clause cond=pattern_condition? EQ_GREATER exp=expression { MatchEffect (pat, cond, exp) }

pattern_or_clause:
  | pattern { $1 }
  | pattern VERTICAL pattern_or_clause { POr ($1, $3) }

pattern_condition: WHEN expression { $2 }

%inline
pattern_op:
  | COMMA { `Comma }
  | AT { `At }

pattern:
  | LBRACKET separated_list(SEMI, pattern) RBRACKET { noimpl () }
  | LBRACKET_VERTICAL separated_list(SEMI, pattern) VERTICAL_RBRACKET { noimpl () }
  | tuple_pattern { $1 }
  | GRAVE_ACCENT IDENTIFIER pattern { noimpl () }

tuple_pattern: cons_pattern list(COMMA cons_pattern { noimpl () }) { noimpl() }

cons_pattern:
  | append_pattern cons_pattern_op cons_pattern { noimpl () } | append_pattern { $1 }
%inline
cons_pattern_op:
  | PLUS_COLON { noimpl () }
  | HYPHEN_COLON { noimpl () }
  | COLON_PLUS_COLON { noimpl () }
  | COLON_HYPHEN_COLON { noimpl () }

append_pattern: append_pattern append_pattern_op prefix_pattern { noimpl () } | prefix_pattern { $1 }
%inline
append_pattern_op:
  | COLON_PLUS { noimpl () }
  | COLON_HYPHEN { noimpl () }

prefix_pattern: prefix_pattern_op capture_pattern { noimpl () } | capture_pattern { $1 }
%inline
prefix_pattern_op:
   | LAZY { noimpl () }

capture_pattern:
  | IDENTIFIER EQ ctor_pattern { noimpl () } | ctor_pattern { $1 }

ctor_pattern:
  | IDENTIFIER simple_pattern { noimpl () } | simple_pattern { $1 }

simple_pattern:
  | IDENTIFIER { PIdent $1 }
  | LPAREN RPAREN { PUnit }
  | LPAREN pattern RPAREN { $2 }
  | LOWLINE { PWildcard }
  | TEXT { PText $1 }
  | NUMBER { PNumber $1 }
  | BOOL { PBool $1 }

ty:
  | refinement_ty { $1 }
  | LCBRACKET option(ty WITH { noimpl () }) separated_list(SEMI, DOT IDENTIFIER COLON ty { noimpl () }) RCBRACKET { noimpl () }
  | GRAVE_ACCENT IDENTIFIER OF ty { noimpl () }
  | LBRACKET separated_list(VERTICAL, ty { $1 }) RBRACKET { noimpl () }

refinement_ty:
  | refinement_ty WHERE refinement_body END { noimpl () } | fun_ty { $1 }

refinement_body:
  | expression { [$1] }
  | { [] }
  | expression SEMI refinement_body { $1 :: $3 }
fun_ty:
  | fun_ty HYPHEN_GREATER tuple_ty { noimpl () } | tuple_ty { $1 }

tuple_ty:
  | tuple_ty ASTERISK ty_with_effect { noimpl () } | ty_with_effect { $1 }

ty_with_effect:
  | application_ty AT long_id { noimpl () } | application_ty { $1 }

application_ty:
  | simple_ty+ { noimpl() }

simple_ty:
  | long_id { TIdent $1 }
  | TYPE_VARIABLE_ID { noimpl () }
  | LPAREN ty RPAREN { $2 }

type_definition:
  | TYPE name=IDENTIFIER params=IDENTIFIER* deriving=deriving_clause? EQ body=type_definition_body { VariantDef (name, List.map (fun x -> TVar x) params, deriving, body) }

type_definition_body:
  | nonempty_list(VERTICAL variant_constructor { $2 }) { Variant $1 }
  | BIG_DOT { ExtensibleVariant }

variant_constructor:
  | name=IDENTIFIER COLON param=ty { name, Some param }
  | name=IDENTIFIER { name, None }

deriving_clause: DERIVING deriving_clause_body { $2 : ty list }

deriving_clause_body:
  | long_id { [TIdent $1] }
  | long_id COMMA deriving_clause_body { TIdent $1 :: $3 }

extensible_variant_definition: TYPE name=IDENTIFIER PLUS_EQ ctor=variant_constructor { ExtensibleVariantDef (name, ctor) }

val_definition: VAL name=IDENTIFIER COLON ty=ty { ValDef (name, ty) }

top_level_let_body: IDENTIFIER simple_pattern* EQ expression { noimpl () }
top_level_let: LET top_level_let_body { noimpl () }
top_level_letrec: LET REC top_level_let_body top_level_and* { noimpl () }
top_level_and: AND top_level_let_body { noimpl () }

definition:
  | val_definition { $1 }
  | type_definition { $1 }
  | extensible_variant_definition { $1 }
  | top_level_let { $1 }
  | top_level_letrec { $1 }

definition_list:
  | definition definition_list { $1 :: $2 }
  | definition { [$1] }

let_definition:
  | LET simple_pattern EQ expression { noimpl () }
  | LET REC simple_pattern EQ expression list(AND simple_pattern EQ expression{ noimpl () }) { noimpl () }

exception_decl:
  | EXCEPTION IDENTIFIER option(OF ty { noimpl () }) { noimpl () }

signature_body_part:
  | val_definition { noimpl () }
  | type_definition { noimpl () }
  | exception_decl { noimpl () }

signature_body: separated_list(semi, signature_body_part) { $1 }

signature:
  | SIG signature_body END { noimpl () }

signature_decl:
  | SIGNATURE IDENTIFIER EQ signature { noimpl () }

signature_ref: | signature  { noimpl () } | long_id { noimpl () }

structure_body_part:
  | type_definition { noimpl () }
  | let_definition { noimpl () }

structure:
  | STRUCT separated_list(semi, structure_body_part) END option(COLON signature_ref { noimpl () }) { noimpl () }

structure_decl:
  | MODULE IDENTIFIER separated_list(COMMA, IDENTIFIER COLON signature_ref { noimpl () }) EQ structure { noimpl () }

functor_decl:
  | FUNCTOR IDENTIFIER separated_list(COMMA, IDENTIFIER COLON signature_ref { noimpl () }) EQ structure { noimpl () }
