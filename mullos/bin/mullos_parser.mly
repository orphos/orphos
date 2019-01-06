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
%token <bool> BOOLTYPE
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
%token LBRACKET
%token LESS
%token LET
%token LOWLINE
%token LPAREN
%token MATCH
%token <int> NL
%token <Mullos_syntax.number> NUMBER
%token <Mullos_syntax.number> NUMBERTYPE
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
%token <string> TEXTTYPE
%token THEN
%token TILDE
%token TYPE
%token <string> TYPEVAR_IDENTIFIER
%token <string> TYPE_IDENTIFIER
%token UNSAFE
%token VERTICAL
%token WHERE

%nonassoc NL SEMI
%nonassoc FN
%nonassoc IF THEN
%nonassoc ELSE
%nonassoc LET
%right DOLLAR
%right COLON
%right RAISE
%right TILDE QUESTION
%right COLON_EQ PLUS_EQ HYPHEN_EQ
%left COMMA
%right HYPHEN_GREATER
%right VERTICAL BIG_VERTICAL
%right AMPERSAND BIG_AMPERSAND
%left BIG_EQ LESS GREATER EXCLAMATION_EQ
%left BIG_LESS BIG_GREATER
%right BIG_COLON
%left PLUS HYPHEN BIG_PLUS BIG_HYPHEN
%left ASTERISK SOLIDUS PERCENT
%right unary EXCLAMATION CIRCUMFLEX
%nonassoc MATCH
%right LAZY
%left NUMBERSIGN
%nonassoc LBRACKET LCBRACKET
%left AT
%nonassoc IDENTIFIER TEXT NUMBER BOOL LPAREN TYPE_IDENTIFIER TYPEVAR_IDENTIFIER TEXTTYPE BOOLTYPE NUMBERTYPE
%nonassoc type_constraint
%nonassoc application


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
  | attribute_list? linkage? UNSAFE? DEF pattern EQ expression where_clause? { () }
  | attribute_list? linkage? UNSAFE? DEF IDENTIFIER parameter_list EQ expression where_clause? { () }
  | attribute_list? linkage? UNSAFE? DEF pattern { () }
  | attribute_list? linkage? UNSAFE? DEF IDENTIFIER parameter_list { () }

value_name:
  IDENTIFIER DOT value_name { $1 :: $3 }
  | IDENTIFIER { [$1] }

type_ident:
  | TYPE_IDENTIFIER DOT type_ident { $1 :: $3 }
  | TYPE_IDENTIFIER { [$1] }

type_name:
  | type_ident { TIdent $1 }
  | TYPEVAR_IDENTIFIER { TVar $1 }

seq:
  | expression SEMI seq { $1 :: $3 }
  | expression NL seq  { $1 :: $3 }
  | expression { [$1] }

%inline
bin_op:
   | PLUS { Add }
   | HYPHEN { Substract }
   | ASTERISK { Multiply }
   | SOLIDUS { Division }
   | CIRCUMFLEX { Xor }
   | PERCENT { Reminder }
   | BIG_LESS { BitwiseLeftShift }
   | BIG_GREATER { BitwiseRightShift }
   | LESS { Less }
   | GREATER { Greater }
   | EXCLAMATION_EQ { NotEqual }
   | AMPERSAND { BitwiseAnd }
   | BIG_AMPERSAND { And }
   | VERTICAL { BitwiseOr }
   | BIG_VERTICAL { Or }
   | BIG_EQ { Equal }
   | PLUS_EQ { AddAsign }
   | HYPHEN_EQ { SubstractAsign }
   | COLON_EQ { Asign }
   | BIG_PLUS { Combine }
   | BIG_HYPHEN { Remove }

unary_op:
   | EXCLAMATION { Not }
   | PLUS { Positive }
   | HYPHEN { Negative }
   | ASTERISK { Deref }
   | AMPERSAND { Ref }
   | RAISE { Raise }
   | LAZY { Lazy }

expression:
  | IDENTIFIER { Identifier $1 }
  | LPAREN RPAREN { Unit }
  | LPAREN expression RPAREN { $2 }
  | LCBRACKET seq RCBRACKET { Seq $2 }
  | TEXT { Text $1 }
  | NUMBER { Number $1 }
  | BOOL { Bool $1 }
  | expression expression %prec application { Apply($1, $2) }
  | LET pattern EQ expression NL expression { failwith "not implemented" }
  | LET IDENTIFIER parameter_list EQ expression SEMI expression { failwith "not implemented" }
  | LET pattern EQ expression SEMI expression { failwith "not implemented" }
  | LET IDENTIFIER parameter_list EQ expression NL expression { failwith "not implemented" }
  | expression bin_op expression { BinOp ($1, $2, $3) }
  | expression COMMA expression {
        let rhs = $3 in
        begin match rhs with
        | Tuple xs -> Tuple ($1 :: xs)
        | x -> Tuple ($1 :: [x])
        end
      }
  | expression MATCH LCBRACKET pattern_clause_list RCBRACKET { failwith "not implemented" }
  | expression DOLLAR expression { Apply($1, $3) }
  | unary_op expression %prec unary { UnaryOp ($1, $2) }
  | IF expression THEN expression ELSE expression { IfThenElse ($2, $4, Some $6) }
  | IF expression THEN expression { IfThenElse ($2, $4, None) }
  | expression COLON type_expression %prec type_constraint { failwith "not implemented" }
  | FN pattern HYPHEN_GREATER expression %prec FN { Lambda ($2, $4) }
  | TILDE IDENTIFIER COLON expression { failwith "not implemented" }
  | QUESTION IDENTIFIER COLON expression { failwith "not implemented" }
  | expression NUMBERSIGN IDENTIFIER { failwith "not implemented" }

pattern_clause_list:
  pattern_clause { () }
  | pattern_clause pattern_clause_list { () }

pattern_clause:
  CASE pattern pattern_condition? EQ_GREATER expression { () }
  | CATCH type_name pattern pattern_condition? EQ_GREATER expression { () }

pattern_condition: IF expression { () }

simple_pattern:
  | value_name { PIdent $1 }
  | LPAREN RPAREN { PUnit }
  | LPAREN pattern RPAREN { $2 }
  | IDENTIFIER AT pattern { PBind ($1, $3) }
  | LOWLINE { PWildcard }
  | TEXT { PText $1 }
  | NUMBER { PNumber $1 }
  | BOOL { PBool $1 }
  | TILDE IDENTIFIER COLON pattern { failwith "not implemented" }
  | LAZY pattern { PLazy $2 }

pattern:
  | TYPE_IDENTIFIER LPAREN pattern RPAREN { PCtor ($1, $3) }
  | pattern COMMA pattern {
        let rhs = $3 in
        begin match rhs with
        | PTuple xs -> PTuple ($1 :: xs)
        | x -> PTuple ($1 :: [x])
        end
      }
  | pattern COLON type_expression { failwith "not implemented" }
  | pattern BIG_COLON pattern { PCons ($1, $3) }
  | simple_pattern { $1 }

parameter_list:
  simple_pattern { () }
  | simple_pattern parameter_list { () }

where_clause: WHERE LCBRACKET definition_list RCBRACKET { () }

definition_list: definition { () }
  | definition definition_list { () }

type_expression:
  | type_name { $1 }
  | NUMBERTYPE { TNumber $1 }
  | TEXTTYPE { TText $1 }
  | BOOLTYPE { TBool $1 }
  | LPAREN type_expression RPAREN { $2 }
  | type_expression type_expression %prec application { TApply ($1, $2) }
  | type_expression COMMA type_expression {
        let x = $3 in
        begin match x with
        | TTuple xs -> TTuple ($1 :: xs)
        | x -> TTuple ($1 :: [x])
        end
      }
  | ASTERISK type_expression { TPointer $2 }
  | IDENTIFIER HYPHEN_GREATER type_expression { failwith "not implemented" }
  | type_expression LBRACKET effect_expression RBRACKET { TEff ($1, $3) }
  | type_expression HYPHEN_GREATER type_expression { TLambda ($1, $3) }
  | LAZY type_expression { TLazy $2 }

effect_expression:
  type_expression { ETy $1 }
  | effect_expression BIG_PLUS effect_expression { ECombine ($1, $3) }
  | LOWLINE { EWildcard }

type_definition:
  | TYPE TYPE_IDENTIFIER variant_parameter_list? deriving_clause? EQ type_definition_body { () }

type_definition_body:
  | variant_constructor_list { () }
  | BIG_DOT { () }

variant_parameter_list:
  TYPEVAR_IDENTIFIER { () }
  | TYPEVAR_IDENTIFIER variant_parameter_list { () }

variant_constructor_list:
  | VERTICAL variant_constructor { () }
  | VERTICAL variant_constructor VERTICAL variant_constructor_list { () }

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

instance: INSTANCE type_expression EQ LCBRACKET definition_list RCBRACKET { () }

extensible_variant_definition: TYPE TYPE_IDENTIFIER PLUS_EQ variant_constructor { () }

top_level_definition:
  definition { () }
  | type_class { () }
  | type_definition { () }
  | extensible_variant_definition { () }
  | instance { () }

top_level_definition_list:
  top_level_definition { () }
  | top_level_definition top_level_definition_list { () }
