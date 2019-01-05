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
%token GREATER
%token HYPHEN
%token HYPHEN_EQ
%token HYPHEN_GREATER
%token <string> IDENTIFIER
%token IF
%token INSTANCE
%token INTERNAL
%token LAZY
%token LCBRACKET
%token LESS
%token LET
%token LOWLINE
%token LPAREN
%token MATCH
%token <int> NL
%token <Q.t * Mullos_syntax.number_literal_type> NUMBER
%token NUMBERSIGN
%token NUMBERSIGN_EXCLAMATION_LBRACKET
%token NUMBERSIGN_LBRACKET
%token PERCENT
%token PLUS
%token PLUS_EQ
%token QUESTION
%token RAISE
%token RBRACKET
%token RCBRACKET
%token RPAREN
%token SEMI
%token SOLIDUS
%token <string> TEXT
%token THEN
%token TILDE
%token TYPE
%token <string> TYPEVAR_IDENTIFIER
%token <string> TYPE_IDENTIFIER
%token UNSAFE
%token VERTICAL
%token WHERE

%nonassoc NL SEMI
%nonassoc LET
%nonassoc FN
%nonassoc THEN
%nonassoc ELSE
%right DOLLAR
%right COLON
%right RAISE
%right COLON_EQ PLUS_EQ HYPHEN_EQ
%left COMMA
%right VERTICAL BIG_VERTICAL
%right AMPERSAND BIG_AMPERSAND
%left BIG_EQ LESS GREATER EXCLAMATION_EQ
%left BIG_LESS BIG_GREATER
%right BIG_COLON
%left PLUS HYPHEN BIG_PLUS BIG_HYPHEN
%left ASTERISK SOLIDUS PERCENT
%right unary_op
%nonassoc MATCH
%right LAZY
%left NUMBERSIGN
%left AT

%start<unit> compilation_unit

%%

compilation_unit: top_level_definition_list  EOF { () }

linkage:
  INTERNAL { () }
  | EXTERNAL { () }

attribute: NUMBERSIGN_LBRACKET expression RBRACKET { () }

attribute_list:
  attribute { () }
  | attribute attribute_list { () }

definition:
  | attribute_list? linkage? UNSAFE? DEF pattern parameter_list? EQ expression where_clause? { () }
  | attribute_list? linkage? UNSAFE? DEF pattern parameter_list? { () }

value_name:
  IDENTIFIER DOT value_name { $1 :: $3 }
  | IDENTIFIER { [$1] }

type_ident:
  | TYPE_IDENTIFIER DOT type_ident { $1 :: $3 }
  | TYPE_IDENTIFIER { [$1] }

type_name:
  | type_ident { TIdent $1 }
  | TYPEVAR_IDENTIFIER { TVar $1 }

simple_expression:
  | IDENTIFIER { Identifier $1 }
  | LPAREN RPAREN { Unit }
  | LPAREN expression RPAREN { $2 }
  | LCBRACKET seq RCBRACKET { Seq $2 }
  | TEXT { Text $1 }
  | NUMBER {
        let v, t = $1 in
        Number(v, t)
      }
  | BOOL { Bool $1 }

seq:
  | expression SEMI seq { $1 :: $3 }
  | expression NL seq  { $1 :: $3 }
  | expression { [$1] }

application_expression:
  | simple_expression { $1 }
  | application_expression simple_expression { Apply($1, $2) }

expression:
  | application_expression { $1 }
  | LET pattern parameter_list? EQ expression SEMI expression { failwith "not implemented" }
  | LET pattern parameter_list? EQ expression NL expression { failwith "not implemented" }
  | expression PLUS expression { Apply(Identifier "+", Tuple [$1; $3]) }
  | expression HYPHEN expression { Apply(Identifier "-", Tuple [$1; $3]) }
  | expression ASTERISK expression { Apply(Identifier "*", Tuple [$1; $3]) }
  | expression SOLIDUS expression { Apply(Identifier "/", Tuple [$1; $3]) }
  | expression PERCENT expression { Apply(Identifier "%", Tuple [$1; $3]) }
  | expression BIG_PLUS expression { Apply(Identifier "++", Tuple [$1; $3]) }
  | expression BIG_HYPHEN expression { Apply(Identifier "--", Tuple [$1; $3]) }
  | expression BIG_LESS expression { Apply(Identifier "<<", Tuple [$1; $3]) }
  | expression BIG_GREATER expression { Apply(Identifier ">>", Tuple [$1; $3]) }
  | expression LESS expression { Apply(Identifier "<", Tuple [$1; $3]) }
  | expression GREATER expression { Apply(Identifier ">", Tuple [$1; $3]) }
  | expression BIG_EQ expression { Apply(Identifier "==", Tuple [$1; $3]) }
  | expression EXCLAMATION_EQ expression { Apply(Identifier "!=", Tuple [$1; $3]) }
  | expression AMPERSAND expression { Apply(Identifier "&", Tuple [$1; $3]) }
  | expression BIG_AMPERSAND expression { Apply(Identifier "&&", Tuple [$1; $3]) }
  | expression VERTICAL expression { Apply(Identifier "|", Tuple [$1; $3]) }
  | expression BIG_VERTICAL expression { Apply(Identifier "||", Tuple [$1; $3]) }
  | expression COMMA expression {
        let rhs = $3 in
        begin match rhs with
        | Tuple xs -> Tuple ($1 :: xs)
        | x -> Tuple ($1 :: [x])
        end
      }
  | expression MATCH LCBRACKET pattern_clause_list RCBRACKET { failwith "not implemented" }
  | expression PLUS_EQ expression { failwith "not implemented" }
  | expression HYPHEN_EQ expression { failwith "not implemented" }
  | expression COLON_EQ expression { failwith "not implemented" }
  | expression DOLLAR expression { Apply(Identifier "$", Tuple [$1; $3]) }
  | EXCLAMATION expression %prec unary_op { Apply(Identifier "!", $2) }
  | PLUS expression %prec unary_op { Apply(Identifier "+", $2) }
  | HYPHEN expression %prec unary_op { Apply(Identifier "-", $2) }
  | CIRCUMFLEX expression %prec unary_op { Apply(Identifier "^", $2) }
  | AMPERSAND expression %prec unary_op { Apply(Identifier "&", $2) }
  | ASTERISK expression %prec unary_op { Apply(Identifier "*", $2) }
  | IF expression THEN expression ELSE expression { IfThenElse ($2, $4, Some $6) }
  | IF expression THEN expression { IfThenElse ($2, $4, None) }
  | expression COLON type_expression { failwith "not implemented" }
  | FN pattern HYPHEN_GREATER expression %prec FN { Lambda ($2, $4) }
  | RAISE expression { failwith "not implemented" }
  | TILDE IDENTIFIER COLON expression { failwith "not implemented" }
  | QUESTION IDENTIFIER COLON expression { failwith "not implemented" }
  | expression NUMBERSIGN IDENTIFIER { failwith "not implemented" }
  | LAZY expression { Lazy $2 }

pattern_clause_list:
  pattern_clause { () }
  | pattern_clause pattern_clause_list { () }

pattern_clause:
  CASE pattern pattern_condition? EQ_GREATER expression { () }
  | CATCH type_name pattern pattern_condition? EQ_GREATER expression { () }

pattern_condition: IF expression { () }

pattern:
  value_name { PIdent $1 }
  | LPAREN RPAREN { PUnit }
  | LPAREN pattern RPAREN { $2 }
  | IDENTIFIER AT pattern { PBind ($1, $3) }
  | IDENTIFIER LPAREN pattern RPAREN { PCtor ($1, $3) }
  | pattern COMMA tuple_pat_tail { PTuple ($1 :: $3) }
  | pattern BIG_COLON pattern { PCons ($1, $3) }
  | pattern COLON type_expression { failwith "not implemented" }
  | LOWLINE { PWildcard }
  | TEXT { PText $1 }
  | NUMBER {
      let v, s = $1 in
      PNumber (v, s)
    }
  | BOOL { PBool $1 }
  | IDENTIFIER HYPHEN_GREATER pattern { failwith "not implemented" }
  | LAZY pattern { PLazy $2 }

tuple_pat_tail:
  | pattern COMMA tuple_pat_tail { $1 :: $3 }
  | pattern { [$1] }

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
  | type_name { $1 }
  | LPAREN type_expression RPAREN { $2 }
  | type_name type_argument_list { TApply ($1, $2) }
  | type_expression COMMA type_tuple_tail { TTuple ($1 :: $3) }
  | ASTERISK type_expression { TPointer $2 }
  | IDENTIFIER HYPHEN_GREATER type_expression { failwith "not implemented" }
  | type_expression LCBRACKET effect_expression RCBRACKET { TEff ($1, $3) }
  | NUMBER { let v, s = $1 in TNumber (v, s) }
  | TEXT { TText $1 }
  | BOOL { TBool $1 }
  | type_expression HYPHEN_GREATER type_expression { TLambda ($1, $3) }
  | LAZY type_expression { TLazy $2 }

type_argument_list:
  type_expression { [$1] }
  | type_expression type_argument_list { $1 :: $2 }

type_tuple_tail:
  | type_expression COMMA type_tuple_tail { $1 :: $3 }
  | type_expression { [$1] }

effect_expression:
  type_expression { ETy $1 }
  | effect_expression BIG_PLUS effect_expression { ECombine ($1, $3) }
  | LOWLINE { EWildcard }

type_definition:
  | TYPE TYPE_IDENTIFIER variant_parameter_list? deriving_clause_body? EQ variant_constructor_list { () }

type_definition_body:
  | variant_constructor_list { () }
  | type_expression { () }

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

type_class_body: top_level_definition_list { () }

instance: INSTANCE TYPE_IDENTIFIER type_argument_list EQ LCBRACKET definition_list RCBRACKET { () }

extensible_variant_declaration: TYPE TYPE_IDENTIFIER EQ BIG_DOT { () }

extensible_variant_definition: TYPE TYPE_IDENTIFIER PLUS_EQ variant_constructor { () }

top_level_definition:
  definition { () }
  | type_class { () }
  | type_definition { () }
  | extensible_variant_declaration { () }
  | extensible_variant_definition { () }

top_level_definition_list:
  top_level_definition { () }
  | top_level_definition top_level_definition_list { () }
