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
%token IF
%token INSTANCE
%token INTERNAL
%token LAZY
%token LCBRACKET
%token LBRACKET
%token LESS
%token LET
%token <string> LOWER_SNAKECASE
%token LOWLINE
%token LPAREN
%token MATCH
%token <int> NL
%token <Mullos_syntax.number> NUMBER
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
%token UNSAFE
%token <string> UPPER_CAMELCASE
%token <string> UPPER_SNAKECASE
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
%nonassoc LBRACKET LCBRACKET
%left AT
%nonassoc LOWER_SNAKECASE TEXT NUMBER BOOL LPAREN TYPEVAR_IDENTIFIER
%nonassoc type_constraint
%nonassoc application
%left DOT


%start<unit> compilation_unit

%%

compilation_unit: definition_list  EOF { () }

%inline
semi:
  | NL { () }
  | SEMI { () }

linkage:
  INTERNAL { () }
  | EXTERNAL { () }

attribute: NUMBERSIGN_LBRACKET expression RBRACKET { () }

attribute_list:
  attribute { () }
  | attribute attribute_list { () }

value_definition:
  | attribute_list? linkage? UNSAFE? DEF pattern_list EQ expression where_clause? { () }
  | attribute_list? linkage? UNSAFE? DEF pattern_list { () }

value_name:
  LOWER_SNAKECASE DOT value_name { $1 :: $3 }
  | LOWER_SNAKECASE { [$1] }

type_ident:
  | LOWER_SNAKECASE DOT type_ident { $1 :: $3 }
  | LOWER_SNAKECASE { [$1] }

type_name:
  | type_ident { TIdent $1 }
  | TYPEVAR_IDENTIFIER { TVar $1 }

seq:
  | expression semi seq { $1 :: $3 }
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
   | BIG_COLON { Cons }

unary_op:
   | EXCLAMATION { Not }
   | PLUS { Positive }
   | HYPHEN { Negative }
   | ASTERISK { Deref }
   | AMPERSAND { Ref }
   | RAISE { Raise }
   | LAZY { Lazy }

expression:
  | LOWER_SNAKECASE { Identifier $1 }
  | LPAREN RPAREN { Unit }
  | LPAREN expression RPAREN { $2 }
  | LCBRACKET seq RCBRACKET { Seq $2 }
  | TEXT { Text $1 }
  | NUMBER { Number $1 }
  | BOOL { Bool $1 }
  | expression expression %prec application { Apply($1, $2) }
  | LET pattern_list EQ expression semi expression {
        match $2 with
        | hd :: tl -> Let (hd, tl, $4, $6)
        | _ -> failwith "unreachable"
      }
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
  | expression DOT LOWER_SNAKECASE { failwith "not implemented" }

pattern_clause_list:
  pattern_clause { () }
  | pattern_clause pattern_clause_list { () }

pattern_clause:
  | VERTICAL pattern pattern_condition? EQ_GREATER LBRACKET expression RBRACKET { () }
  (*| CATCH type_name pattern pattern_condition? EQ_GREATER expression { () }*)

pattern_condition: IF expression { () }

pattern:
  | value_name { PIdent $1 }
  | LPAREN RPAREN { PUnit }
  | LPAREN pattern RPAREN { $2 }
  | LOWER_SNAKECASE AT pattern { PBind ($1, $3) }
  | LOWLINE { PWildcard }
  | TEXT { PText $1 }
  | NUMBER { PNumber $1 }
  | BOOL { PBool $1 }
  | LAZY pattern { PLazy $2 }
  | UPPER_SNAKECASE LPAREN pattern RPAREN { PCtor ($1, $3) }
  | pattern COMMA pattern {
        let rhs = $3 in
        begin match rhs with
        | PTuple xs -> PTuple ($1 :: xs)
        | x -> PTuple ($1 :: [x])
        end
      }
  | pattern COLON type_expression { failwith "not implemented" }
  | pattern BIG_COLON pattern { PCons ($1, $3) }

pattern_list:
  pattern { [$1] }
  | pattern pattern_list { $1 :: $2 }

where_clause: WHERE LCBRACKET definition_list RCBRACKET { () }

type_expression:
  | type_name { $1 }
  | NUMBER { TNumber $1 }
  | TEXT { TText $1 }
  | BOOL { TBool $1 }
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
  | type_expression LBRACKET effect_expression RBRACKET { TEff ($1, $3) }
  | type_expression HYPHEN_GREATER type_expression { TLambda ($1, $3) }
  | LAZY type_expression { TLazy $2 }

effect_expression:
  type_expression { ETy $1 }
  | effect_expression BIG_PLUS effect_expression { ECombine ($1, $3) }
  | LOWLINE { EWildcard }

type_definition:
  | TYPE name=LOWER_SNAKECASE params=variant_parameter_list? deriving=deriving_clause? EQ body=type_definition_body { name, params, deriving, body }

type_definition_body:
  | variant_constructor_list { Variant $1 }
  | BIG_DOT { ExtensibleVariant }

variant_parameter_list:
  | hd=TYPEVAR_IDENTIFIER { [hd] }
  | hd=TYPEVAR_IDENTIFIER tl=variant_parameter_list { hd :: tl }

variant_constructor_list:
  | VERTICAL hd=variant_constructor { [hd] }
  | VERTICAL hd=variant_constructor VERTICAL tl=variant_constructor_list { hd :: tl }

variant_constructor:
  | name=UPPER_SNAKECASE param_ret=variant_constructor_parameter_and_result { name, Some param_ret }
  | name=UPPER_SNAKECASE { name, None }

variant_constructor_parameter_and_result:
  | COLON param=type_expression { param, None }
  | COLON param=type_expression ret=variant_constructor_result { param, Some ret }

variant_constructor_result: EQ_GREATER ret=type_expression { ret }

deriving_clause: DERIVING deriving_clause_body { $1 }

deriving_clause_body:
  type_name { [$1] }
  | type_name COMMA deriving_clause_body { $1 :: $3 }

class_definition: CLASS UPPER_CAMELCASE params=class_parameter_list? where_clause { () }

class_parameter_list:
  | class_parameter { () }
  | class_parameter COMMA class_parameter_list { () }

class_parameter:
  | TYPEVAR_IDENTIFIER { () }
  | type_name type_parameter_list { () }

type_parameter_list:
  type_name { () }
  | type_name type_parameter_list { () }

instance: INSTANCE type_expression where_clause { () }

extensible_variant_definition: TYPE LOWER_SNAKECASE PLUS_EQ variant_constructor { () }

definition:
  | value_definition { () }
  | type_definition { () }
  | extensible_variant_definition { () }
  | class_definition { () }
  | instance { () }

definition_list:
  | definition { () }
  | definition definition_list { () }
