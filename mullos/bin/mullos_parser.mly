(* Copyright (C) 2018-2019 Takezoe,Tomoaki <tomoaki@res.ac>
 *
 * SPDX-Identifier: LGPL-3.0-or-later
 *)
%{

open Mullos_syntax

%}

%token AMPERSAND
%token ASTERISK
%token AT
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
%token COMMA
%token DATA
%token DEF
%token DERIVING
%token DOLLAR
%token DOT
%token ELSE
%token EOF
%token EQ
%token EQ_GREATER
%token EXCLAMATION
%token EXCLAMATION_EQ
%token EXTERNAL
%token FN
%token GOTO
%token GREATER
%token HYPHEN
%token HYPHEN_EQ
%token HYPHEN_GREATER
%token <string> IDENTIFIER
%token IF
%token INSTANCE
%token INTERNAL
%token LABEL
%token LCBRACKET
%token LESS
%token LET
%token LOWLINE
%token LPAREN
%token MATCH
%token <int> NL
%token <Q.t * Mullos_syntax.number_literal_type> NUMBER
%token NUMBERSIGN
%token PERCENT
%token PLUS
%token PLUS_EQ
%token RAISE
%token RCBRACKET
%token RPAREN
%token SEMI
%token SOLIDUS
%token <string> TEXT
%token THEN
%token TYPE
%token <string> TYPEVAR_IDENTIFIER
%token <string> TYPE_IDENTIFIER
%token UNSAFE
%token VERTICAL
%token WHERE

%right DOLLAR
%right COLON_EQ PLUS_EQ HYPHEN_EQ
%left COMMA
%right VERTICAL BIG_VERTICAL
%right AMPERSAND BIG_AMPERSAND
%left BIG_EQ LESS GREATER EXCLAMATION_EQ
%left BIG_LESS BIG_GREATER
%left PLUS HYPHEN BIG_PLUS BIG_HYPHEN
%left ASTERISK SOLIDUS PERCENT
%nonassoc MATCH
%left NUMBERSIGN

%start<unit> compilation_unit

%%

compilation_unit: top_level_definition_list  EOF { () }

linkage:
  INTERNAL { () }
  | EXTERNAL { () }

annotation: AT IDENTIFIER LPAREN argument_list? RPAREN { () }

annotation_list:
  annotation { () }
  | annotation annotation_list { () }

definition: annotation_list? linkage? UNSAFE? DEF pattern parameter_list? EQ expression where_clause? { () }

value_name:
  IDENTIFIER DOT value_name { () }
  | IDENTIFIER { () }

type_name:
  TYPE_IDENTIFIER DOT type_name { () }
  | TYPE_IDENTIFIER { () }
  | TYPEVAR_IDENTIFIER { () }

expression:
  | IDENTIFIER { Identifier $1 }
  | LPAREN RPAREN { Unit }
  | LPAREN expression RPAREN { $2 }
  | LCBRACKET expression RCBRACKET { $2 }
  | expression LPAREN expression RPAREN { Apply($1, $3) }
  | LET pattern parameter_list? EQ expression SEMI expression { failwith "not implemented" }
  | LET pattern parameter_list? EQ expression NL expression { failwith "not implemented" }
  | expression PLUS expression { failwith "not implemented" }
  | expression HYPHEN expression { failwith "not implemented" }
  | expression ASTERISK expression { failwith "not implemented" }
  | expression SOLIDUS expression { failwith "not implemented" }
  | expression PERCENT expression { failwith "not implemented" }
  | expression BIG_PLUS expression { failwith "not implemented" }
  | expression BIG_HYPHEN expression { failwith "not implemented" }
  | expression BIG_LESS expression { failwith "not implemented" }
  | expression BIG_GREATER expression { failwith "not implemented" }
  | expression LESS expression { failwith "not implemented" }
  | expression GREATER expression { failwith "not implemented" }
  | expression BIG_EQ expression { failwith "not implemented" }
  | expression EXCLAMATION_EQ expression { failwith "not implemented" }
  | expression AMPERSAND expression { failwith "not implemented" }
  | expression BIG_AMPERSAND expression { failwith "not implemented" }
  | expression VERTICAL expression { failwith "not implemented" }
  | expression BIG_VERTICAL expression { failwith "not implemented" }
  | expression COMMA tuple_tail { Tuple ($1 :: $3) }
  | expression MATCH LCBRACKET pattern_clause_list RCBRACKET { failwith "not implemented" }
  | expression PLUS_EQ expression { failwith "not implemented" }
  | expression HYPHEN_EQ expression { failwith "not implemented" }
  | expression COLON_EQ expression { failwith "not implemented" }
  | expression DOLLAR expression { failwith "not implemented" }
  | EXCLAMATION expression { failwith "not implemented" }
  | PLUS expression { failwith "not implemented" }
  | HYPHEN expression { failwith "not implemented" }
  | CIRCUMFLEX expression { failwith "not implemented" }
  | AMPERSAND expression { failwith "not implemented" }
  | ASTERISK expression { failwith "not implemented" }
  | IF expression THEN expression ELSE expression { failwith "not implemented" }
  | IF expression THEN expression { failwith "not implemented" }
  | expression COLON type_expression { failwith "not implemented" }
  | expression SEMI expression { failwith "not implemented" }
  | expression NL expression { failwith "not implemented" }
  | TEXT { Text $1 }
  | NUMBER {
      let v, t = $1 in
      Number(v, t)
    }
  | BOOL { Bool $1 }
  | FN parameter_list EQ_GREATER expression { failwith "not implemented" }
  | RAISE expression { failwith "not implemented" }
  | IDENTIFIER HYPHEN_GREATER expression { failwith "not implemented" }
  | expression NUMBERSIGN IDENTIFIER { failwith "not implemented" }
  | GOTO expression { failwith "not implemented" }
  | label_clause expression { failwith "not implemented" }

tuple_tail:
  | expression COMMA tuple_tail { $1 :: $3 }
  | expression { [$1] }

label_clause: LABEL IDENTIFIER COLON { () }

pattern_clause_list:
  pattern_clause { () }
  | pattern_clause pattern_clause_list { () }

pattern_clause:
  CASE pattern pattern_condition? EQ_GREATER expression { () }
  | CATCH type_name pattern pattern_condition? EQ_GREATER expression { () }

pattern_condition: IF expression { () }

pattern:
  value_name { () }
  | LPAREN RPAREN { () }
  | LPAREN pattern RPAREN { () }
  | IDENTIFIER AT pattern { () }
  | IDENTIFIER LPAREN pattern RPAREN { () }
  | pattern COMMA pattern { () }
  | pattern BIG_COLON pattern { () }
  | pattern COLON type_expression { () }
  | LOWLINE { () }
  | TEXT { () }
  | NUMBER { () }
  | BOOL { () }
  | IDENTIFIER HYPHEN_GREATER pattern { () }

argument_list:
  expression { () }
  | expression argument_list { () }

parameter_list:
  pattern { () }
  | pattern parameter_list { () }

where_clause: WHERE LCBRACKET definition_list RCBRACKET { () }

definition_list: definition { () }
  | definition definition_list { () }

type_expression:
  type_name { () }
  | LPAREN type_expression RPAREN { () }
  | type_name type_argument_list { () }
  | type_expression COMMA type_expression { () }
  | ASTERISK type_expression { () }
  | IDENTIFIER HYPHEN_GREATER type_expression { () }
  | type_expression LCBRACKET effect_expression RCBRACKET { () }
  | NUMBER { () }
  | TEXT { () }
  | BOOL { () }
  | type_expression EQ_GREATER type_expression { () }

type_argument_list:
  type_expression { () }
  | type_expression type_argument_list { () }

effect_expression:
  type_expression { () }
  | type_expression BIG_PLUS effect_expression { () }
  | LOWLINE { () }

variant:
  DATA TYPE_IDENTIFIER variant_parameter_list? deriving_clause_body? EQ variant_constructor_list { () }
  | DATA TYPE_IDENTIFIER variant_parameter_list? deriving_clause_body? EQ type_expression { () }

variant_parameter_list:
  TYPEVAR_IDENTIFIER { () }
  | TYPEVAR_IDENTIFIER variant_parameter_list { () }

variant_constructor_list:
  variant_constructor { () }
  | variant_constructor VERTICAL variant_constructor_list { () }

variant_constructor:
  TYPE_IDENTIFIER variant_constructor_parameter_and_result { () }
  | TYPE_IDENTIFIER { () }

variant_constructor_parameter_and_result:
  COLON type_expression { () }
  | COLON type_expression variant_constructor_result { () }

variant_constructor_result: EQ_GREATER type_expression { () }

deriving_clause: DERIVING deriving_clause_body { () }

deriving_clause_body:
  type_name { () }
  | type_name COMMA deriving_clause_body { () }

type_class: CLASS TYPE_IDENTIFIER type_parameter_list? EQ type_class_parameter? LCBRACKET type_class_body RCBRACKET { () }

type_class_parameter: type_name type_parameter_list? EQ_GREATER { () }

type_parameter_list:
  type_name { () }
  | type_name type_parameter_list { () }

type_class_body: declaration_list { () }

declaration_list:
  declaration { () }
  | declaration declaration_list { () }

declaration: DEF IDENTIFIER COLON type_expression { () }

instance: INSTANCE TYPE_IDENTIFIER type_argument_list EQ LCBRACKET definition_list RCBRACKET { () }

extensible_variant_declaration: TYPE TYPE_IDENTIFIER EQ BIG_DOT { () }

extensible_variant_definition: TYPE TYPE_IDENTIFIER PLUS_EQ variant_constructor { () }

top_level_definition:
  definition { () }
  | type_class { () }
  | variant { () }
  | declaration { () }
  | extensible_variant_declaration { () }
  | extensible_variant_definition { () }

top_level_definition_list:
  top_level_definition { () }
  | top_level_definition top_level_definition_list { () }
