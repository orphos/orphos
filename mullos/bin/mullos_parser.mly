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
%token <string> TYPEVAR_IDENTIFIER
%token UNSAFE
%token <string> UPPER_CAMELCASE
%token <string> UPPER_SNAKECASE
%token VERTICAL
%token WHERE

%right COLON
%right RAISE
%left COMMA
%right AMPERSAND
%right BIG_COLON
%left PLUS HYPHEN BIG_PLUS
%left ASTERISK
%right EXCLAMATION
%right LAZY
%nonassoc LCBRACKET
%left AT
%nonassoc LOWER_SNAKECASE TEXT NUMBER BOOL LPAREN

%start<unit> compilation_unit

%%

compilation_unit: definition_list  EOF { () }

%inline
semi:
  | NL { () }
  | SEMI { () }

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
   | non_paren_expression { $1 }
   | paren_expression { $1 }

let_expression:
   | LET pattern+ EQ expression semi expression {
      match $2 with
      | hd :: tl -> Let (hd, tl, $4, $6)
      | _ -> failwith "unreachable"
    }

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
  | LOWER_SNAKECASE { Identifier $1 }
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
  | VERTICAL pattern { $2 }
  | VERTICAL pattern pattern_or_clause { POr ($2, $3) }

pattern_condition: IF expression { $2 }

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
  | UPPER_CAMELCASE LPAREN pattern RPAREN { PCtor ($1, $3) }
  | pattern COMMA pattern {
        let rhs = $3 in
        begin match rhs with
        | PTuple xs -> PTuple ($1 :: xs)
        | x -> PTuple ($1 :: [x])
        end
      }
  | pattern COLON simple_or_paren_type_expression { failwith "not implemented" }
  | pattern BIG_COLON pattern { PCons ($1, $3) }

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
  | type_name { $1 }
  | NUMBER { TNumber $1 }
  | TEXT { TText $1 }
  | BOOL { TBool $1 }

effect_expression:
  type_expression { ETy $1 }
  | effect_expression BIG_PLUS effect_expression { ECombine ($1, $3) }
  | LOWLINE { EWildcard }

type_definition:
  | TYPE name=LOWER_SNAKECASE params=TYPEVAR_IDENTIFIER* deriving=deriving_clause? EQ body=type_definition_body { VariantDef (name, List.map (fun x -> TVar x) params, deriving, body) }

type_definition_body:
  | nonempty_list(VERTICAL variant_constructor { $2 }) { Variant $1 }
  | BIG_DOT { ExtensibleVariant }

variant_constructor:
  | name=UPPER_CAMELCASE COLON param=simple_or_paren_type_expression { name, Some param }
  | name=UPPER_CAMELCASE { name, None }

deriving_clause: DERIVING deriving_clause_body { $2 }

deriving_clause_body:
  type_name { [$1] }
  | type_name COMMA deriving_clause_body { $1 :: $3 }

extensible_variant_definition: TYPE name=LOWER_SNAKECASE PLUS_EQ ctor=variant_constructor { ExtensibleVariantDef (name, ctor) }

definition:
  | let_expression { () }
  | type_definition { () }
  | extensible_variant_definition { () }

definition_list:
  | definition semi definition_list { $1 :: $3 }
  | definition { [$1] }

