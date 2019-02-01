(* Copyright (C) 2018-2019 Takezoe,Tomoaki <tomoaki@res.ac>
 *
 * SPDX-Identifier: LGPL-3.0-or-later
 *)
%{

open Mullos_aux
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
%token EFFECT
%token ELSE
%token EOF
%token EQ
%token EQ_GREATER
%token EXCEPTION
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
%token NL
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
%token UNSAFE
%token VERTICAL
%token WHERE

%right RAISE
%right AMPERSAND
%left PLUS HYPHEN
%left ASTERISK
%right EXCLAMATION
%right LAZY
%nonassoc LCBRACKET
%nonassoc IDENTIFIER TEXT NUMBER BOOL LPAREN

%start<unit> compilation_unit

%%

compilation_unit: definition_list  EOF { () }

%inline
semi:
  | NL { () }
  | SEMI { () }

qual_ident:
  IDENTIFIER DOT qual_ident { $1 :: $3 }
  | IDENTIFIER { [$1] }

seq:
  | expression semi seq { $1 :: $3 }
  | expression { [$1] }

%inline
bin_op:
   | PLUS { `Add }
   | HYPHEN { `Substract }
   | ASTERISK { `Multiply }
   | SOLIDUS { `Division }
   | CIRCUMFLEX { `Xor }
   | PERCENT { `Reminder }
   | BIG_LESS { `BitwiseLeftShift }
   | BIG_GREATER { `BitwiseRightShift }
   | LESS { `Less }
   | GREATER { `Greater }
   | EXCLAMATION_EQ { `NotEqual }
   | AMPERSAND { `BitwiseAnd }
   | BIG_AMPERSAND { `And }
   | VERTICAL { `BitwiseOr }
   | BIG_VERTICAL { `Or }
   | BIG_EQ { `Equal }
   | PLUS_EQ { `AddAsign }
   | HYPHEN_EQ { `SubstractAsign }
   | COLON_EQ { `Asign }
   | BIG_PLUS { `Combine }
   | BIG_HYPHEN { `Remove }
   | BIG_COLON { `Cons }
   | DOT { `Dot }

unary_op:
   | EXCLAMATION { Not }
   | PLUS { Positive }
   | HYPHEN { Negative }
   | ASTERISK { Deref }
   | AMPERSAND { Ref }
   | RAISE { Raise }
   | LAZY { Lazy }

expression:
   | non_paren_expression { $1 }
   | paren_expression { $1 }

let_expression:
   | LET ident_pattern ident_pattern+ EQ expression semi expression { Let ($2, $3, $5, $7) }
   | LET simple_pattern EQ expression semi expression { Let ($2, [], $4, $6) }

non_paren_expression:
   | IF expression THEN expression ELSE expression { IfThenElse ($2, $4, Some $6) }
   | simple_expression bin_op non_paren_expression {
       match $3 with
       | BinOp (lhs, op, rhs, tail) -> BinOp ($1, $2, lhs, (op, rhs) :: tail)
       | _ -> BinOp($1, $2, $3, [])
     }
   | simple_expression bin_op paren_expression { BinOp ($1, $2, $3, []) }
   | lhs=simple_expression MATCH LCBRACKET rhs=pattern_clause+ RCBRACKET { Match (lhs, rhs) }
   | lhs=paren_expression MATCH LCBRACKET rhs=pattern_clause+ RCBRACKET { Match (lhs, rhs) }
   | FN pattern HYPHEN_GREATER expression { Lambda ($2, $4) }
   | let_expression { $1 }
   | simple_expression simple_expression { Apply ($1, $2) }
   | simple_expression paren_expression { Apply ($1, $2) }
   | simple_expression { $1 }

paren_expression:
  | LPAREN expression RPAREN { $2 }

simple_expression:
  | IDENTIFIER { Identifier $1 }
  | LPAREN RPAREN { Unit }
  | LCBRACKET seq RCBRACKET { Seq $2 }
  | TEXT { Text $1 }
  | NUMBER { Number $1 }
  | BOOL { Bool $1 }
  | unary_op simple_expression { UnaryOp ($1, $2) }
  | unary_op paren_expression { UnaryOp ($1, $2) }

pattern_clause:
  | CASE pat=pattern_or_clause cond=pattern_condition? EQ_GREATER LBRACKET exp=expression RBRACKET { MatchPat (pat, cond, exp) }
  | CASE EXCEPTION pat=pattern_or_clause cond=pattern_condition? EQ_GREATER LBRACKET exp=expression RBRACKET { MatchException (pat, cond, exp) }
  | CASE EFFECT pat=pattern_or_clause cond=pattern_condition? EQ_GREATER LBRACKET exp=expression RBRACKET { MatchEffect (pat, cond, exp) }

pattern_or_clause:
  | pattern { $1 }
  | pattern VERTICAL pattern_or_clause { POr ($1, $3) }

pattern_condition: IF expression { $2 }

%inline
pattern_op:
  | COMMA { `Comma }
  | BIG_COLON { `Cons }
  | AT { `At }

pattern:
  | IDENTIFIER pattern { PCtor ($1, $2) }
  | simple_pattern COLON simple_or_paren_type_expression { failwith "not implemented" }
  | simple_pattern pattern_op pattern { failwith "not implemented" }
  | simple_pattern { $1 }

ident_pattern:
  | IDENTIFIER { PIdent $1 }

simple_pattern:
  | ident_pattern { $1 }
  | LPAREN RPAREN { PUnit }
  | LPAREN pattern RPAREN { $2 }
  | LOWLINE { PWildcard }
  | TEXT { PText $1 }
  | NUMBER { PNumber $1 }
  | BOOL { PBool $1 }
  | LAZY simple_pattern { PLazy $2 }

type_binary_op:
  | COMMA { TComma }
  | HYPHEN_GREATER { TArrow }

type_expression:
  | most_type_expression { $1 }
  | paren_type_expression { $1 }

paren_type_expression:
  | LPAREN type_expression RPAREN { $2 }

 simple_or_paren_type_expression:
  | LPAREN type_expression RPAREN { $2 }
  | simple_type_expression { $1 }

most_type_expression:
  | simple_type_expression { $1 }
  | simple_or_paren_type_expression type_binary_op most_type_expression {
        match $3 with
        | TBinOp (lhs, op, rhs, tail) -> TBinOp ($1, $2, lhs, (op, rhs) :: tail)
        | _ -> TBinOp ($1, $2, $3, [])
      }
  | simple_or_paren_type_expression most_type_expression {
        match $2 with
        | TBinOp (lhs, op, rhs, tail) -> TBinOp ($1, TApply, lhs, (op, rhs) :: tail)
        | _ -> TBinOp ($1, TApply, $2, [])
      }
  | simple_or_paren_type_expression type_binary_op paren_type_expression { TBinOp ($1, $2, $3, []) }
  | simple_or_paren_type_expression paren_type_expression { TBinOp ($1, TApply, $2, []) }
  | simple_or_paren_type_expression LBRACKET effect_expression RBRACKET { TEff ($1, $3) }
  | LAZY type_expression { TLazy $2 }

simple_type_expression:
  | ident_type { $1 }
  | NUMBER { TNumber $1 }
  | TEXT { TText $1 }
  | BOOL { TBool $1 }

ident_type:
  | qual_ident { TIdent $1 }

effect_expression:
  ident_effect { ETy $1 }
  | LOWLINE { EWildcard }

ident_effect:
  ident_type { [$1] }
  | ident_type BIG_PLUS ident_effect { $1 :: $3 }

type_definition:
  | TYPE name=IDENTIFIER params=IDENTIFIER* deriving=deriving_clause? EQ body=type_definition_body { VariantDef (name, List.map (fun x -> TVar x) params, deriving, body) }

type_definition_body:
  | nonempty_list(VERTICAL variant_constructor { $2 }) { Variant $1 }
  | BIG_DOT { ExtensibleVariant }

variant_constructor:
  | name=IDENTIFIER COLON param=simple_or_paren_type_expression { name, Some param }
  | name=IDENTIFIER { name, None }

deriving_clause: DERIVING deriving_clause_body { $2 : ty list }

deriving_clause_body:
  qual_ident { [TIdent $1] }
  | qual_ident COMMA deriving_clause_body { TIdent $1 :: $3 }

extensible_variant_definition: TYPE name=IDENTIFIER PLUS_EQ ctor=variant_constructor { ExtensibleVariantDef (name, ctor) }

definition:
  | let_expression { () }
  | type_definition { () }
  | extensible_variant_definition { () }

definition_list:
  | definition semi definition_list { $1 :: $3 }
  | definition { [$1] }

