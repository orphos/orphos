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
%token GIVEN
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
%token SINGLE_QUOTE
%token SOLIDUS
%token STRUCT
%token STRUCTURE
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
%token WITHOUT

%start<Mullos_syntax.compilation_unit> compilation_unit

%%

%inline
semi:
  | NL { () }
  | SEMI { () }

long_id:
  IDENTIFIER BIG_COLON long_id { let LongId tail = $3 in LongId  ($1 :: tail) }
  | IDENTIFIER { LongId [$1] }

seq:
  | expression semi seq { $1 :: $3 }
  | expression { [$1] }

list_nonauto_semi(X):
  | X SEMI list_nonauto_semi(X) { $1 :: $3 }
  | X SEMI { [$1] }
  | X { [$1] }

expression:
  | IF expression THEN expression ELSE expression { IfThenElse ($2, $4, Some $6) }
  | IF expression THEN expression END { IfThenElse ($2, $4, None) }
  | MATCH lhs=expression WITH rhs=pattern_clause+ END { Match (lhs, rhs) }
  | FN pattern HYPHEN_GREATER expression { Lambda ($2, $4) }
  | LET pat=simple_pattern params=simple_pattern* EQ body=expression semi exp=expression { Let (pat, params, body, exp) }
  | LET REC pat=simple_pattern params=simple_pattern* EQ body=expression ands=list(AND pat=simple_pattern params=simple_pattern* EQ body=expression { pat, params, body }) semi exp=expression { LetRec ((pat, params, body) :: ands, exp) }
  | assignment_expression HANDLE pattern_clause+ END { Handle ($1, $3) }
  | assignment_expression { $1 }
  | LBRACKET list_nonauto_semi(expression) RBRACKET { ListLiteral $2 }
  | LBRACKET_VERTICAL list_nonauto_semi(expression) VERTICAL_RBRACKET { ArrayLiteral $2 }
  | LCBRACKET row=ioption(expression WITH { $1 }) fields=list_nonauto_semi(DOT IDENTIFIER EQ expression { $2, $4 }) RCBRACKET { RecordLiteral (row, fields) }
  | LCBRACKET left=expression WITHOUT DOT right=IDENTIFIER RCBRACKET { RecordRestrictionLiteral (left, right) }
  | GRAVE_ACCENT IDENTIFIER expression { PolymorphicVariantConstruction ($2, $3) }

assignment_expression: assignment_expression binop_assignment pipeline_expression { BinOp ($1, $2, $3) } | pipeline_expression { $1 }
%inline
binop_assignment:
  | PLUS_EQ { `AddAsign }
  | HYPHEN_EQ { `SubstractAsign }
  | COLON_EQ { `Asign }

pipeline_expression: dollar_expression VERTICAL_GREATER pipeline_expression { BinOp ($1, `Pipeline, $3) } | dollar_expression { $1 }

dollar_expression: dollar_expression DOLLAR tuple_expression { Apply ($1, $3) } | tuple_expression { $1 }

tuple_expression: lor_expression nonempty_list(COMMA lor_expression { $2 }) { Tuple ($1 :: $2) } | lor_expression { $1 }

lor_expression: lor_expression BIG_VERTICAL land_expression { BinOp ($1, `Or, $3) } | land_expression { $1 }

land_expression: land_expression BIG_AMPERSAND bitwise_or_expression { BinOp ($1, `And, $3) } | bitwise_or_expression { $1 }

bitwise_or_expression: bitwise_or_expression VERTICAL xor_expression { BinOp ($1, `BitwiseOr, $3) } | xor_expression { $1 }

xor_expression: xor_expression CIRCUMFLEX bitwise_and_expression { BinOp ($1, `Xor, $3) } | bitwise_and_expression { $1 }

bitwise_and_expression: bitwise_and_expression AMPERSAND equal_expression { BinOp ($1, `BitwiseAnd, $3) } | equal_expression { $1 }

equal_expression: equal_expression binop_equal greater_expression { BinOp ($1, $2, $3) } | greater_expression { $1 }
%inline
binop_equal:
  | EXCLAMATION_EQ { `NotEqual }
  | BIG_EQ { `Equal }

greater_expression: greater_expression binop_greater shift_expression { BinOp ($1, $2, $3) } | shift_expression { $1 }
%inline
binop_greater:
  | LESS { `Less }
  | GREATER { `Greater }

shift_expression: shift_expression binop_shift cons_expression { BinOp ($1, $2, $3) } | cons_expression { $1 }
%inline
binop_shift:
  | BIG_LESS { `BitwiseLeftShift }
  | BIG_GREATER { `BitwiseRightShift }

cons_expression: add_expression binop_cons cons_expression { BinOp ($3, $2, $1) } | add_expression { $1 }
%inline
binop_cons:
   | PLUS_COLON { `Append }
   | HYPHEN_COLON { `Erase }

add_expression: add_expression binop_add multiply_expression { BinOp ($1, $2, $3) } | multiply_expression { $1 }
%inline
binop_add:
  | PLUS { `Add }
  | HYPHEN { `Substract }
  | COLON_PLUS { `Prepend }
  | COLON_HYPHEN { `Erase }
  | COLON_PLUS_COLON { `Combine }
  | COLON_HYPHEN_COLON { `Remove }

multiply_expression: multiply_expression binop_multiply prefix_expression { BinOp ($1, $2, $3) } | prefix_expression { $1 }
%inline
binop_multiply:
  | ASTERISK { `Multiply }
  | SOLIDUS  { `Division }
  | PERCENT { `Reminder }

prefix_expression: prefix_op postfix_expression { PrefixOp ($1, $2) } | postfix_expression { $1 }
%inline
prefix_op:
  | PLUS { `Positive }
  | HYPHEN { `Negative }
  | EXCLAMATION { `Not }
  | AMPERSAND { `Ref }
  | ASTERISK { `Deref }
  | BIG_PLUS { `Increment }
  | BIG_HYPHEN { `Decrement }
  | RAISE { `Raise }
  | LAZY { `Lazy }

postfix_expression: application_expression postfix_op { PostfixOp ($1, $2) } | application_expression { $1 }
%inline
postfix_op:
  | BIG_PLUS { `Increment }
  | BIG_HYPHEN { `Decrement }

application_expression: application_expression dot_expression { Apply ($1, $2) } | dot_expression { $1 }

dot_expression: dot_expression DOT IDENTIFIER { RecordSelection ($1, $3) } | simple_expression { $1 }

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

pattern:
  | LBRACKET separated_list(semi, pattern) RBRACKET { PListLiteral $2 }
  | LBRACKET_VERTICAL separated_list(semi, pattern) VERTICAL_RBRACKET { PArrayLiteral $2 }
  | tuple_pattern { $1 }
  | GRAVE_ACCENT IDENTIFIER pattern { PPolymorphicVariant ($2, $3) }

tuple_pattern: lazy_pattern COMMA separated_nonempty_list(COMMA, lazy_pattern) { PTuple ($1 :: $3) } | lazy_pattern { $1 }

lazy_pattern: LAZY capture_pattern { PLazy $2 } | capture_pattern { $1 }

capture_pattern:
  | IDENTIFIER EQ ctor_pattern { PCapture ($1, $3) } | ctor_pattern { $1 }

ctor_pattern:
  | IDENTIFIER simple_pattern { PCtor ($1, $2) } | simple_pattern { $1 }

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
  | LCBRACKET row=option(ty WITH { $1 }) fields=list_nonauto_semi(DOT IDENTIFIER COLON ty { $2, $4 }) RCBRACKET { TRecord (row, fields) }
  | GRAVE_ACCENT IDENTIFIER OF ty { TPolymorphicVariant ($2, $4) }
  | LBRACKET separated_list(VERTICAL, ty { $1 }) RBRACKET { TOr $2 }

refinement_ty:
  | refinement_ty WHERE refinement_body END { TRefinement ($1, $3) } | given_ty { $1 }

refinement_body:
  | expression { [$1] }
  | { [] }
  | expression semi refinement_body { $1 :: $3 }

given_ty:
  | given_ty GIVEN separated_nonempty_list(COMMA, long_id) { TGiven ($1, $3) } | fun_ty { $1 }

fun_ty:
  | fun_ty HYPHEN_GREATER tuple_ty { TArrow ($1, $3) } | tuple_ty { $1 }

tuple_ty:
  | ty_with_effect nonempty_list(ASTERISK ty_with_effect { $2 }) { TTuple ($1 :: $2) } | ty_with_effect { $1 }

ty_with_effect:
  | application_ty AT separated_nonempty_list(COMMA, long_id) { TEff ($1, $3) } | application_ty { $1 }

application_ty:
  | simple_ty application_ty {
      match $2 with
        | TApply (params, fn) -> TApply ($1 :: params, fn)
        | fn -> TApply ([$1], fn)
      }
  | simple_ty { $1 }

simple_ty:
  | long_id { TIdent $1 }
  | type_variable { TVar $1 }
  | LPAREN ty RPAREN { $2 }

type_variable: SINGLE_QUOTE IDENTIFIER { $2 }

type_definition:
  | TYPE type_variable* IDENTIFIER { `TypeDecl ($2, $3) }
  | TYPE type_variable* IDENTIFIER EQ ty { `TypeAlias ($2, $3, $5) }
  | TYPE params=type_variable* name=IDENTIFIER EQ ioption(VERTICAL) ctor=IDENTIFIER OF ty=ty tail=nonempty_list(VERTICAL IDENTIFIER OF ty { $2, $4 }) { `MonomorphicVariant (params, name, (ctor, ty) :: tail) }

val_definition: VAL name=IDENTIFIER COLON ty=ty { `ValDef (name, ty) }

let_definition:
  | LET name=IDENTIFIER EQ exp=expression { `LetDef (name, exp) }
  | LET REC name=IDENTIFIER EQ exp=expression ands=list(AND name=IDENTIFIER EQ exp=expression{ name, exp }) { `LetRecDef ((name, exp) :: ands) }

exception_definition:
  | EXCEPTION IDENTIFIER option(OF ty { $2 }) { `ExceptionDef ($2, $3) }

signature_body_part:
  | val_definition { $1 }
  | type_definition { $1 }
  | exception_definition { $1 }

signature_body: separated_list(semi, signature_body_part) { $1 }

signature:
  | SIG signature_body END WHERE separated_list(semi, signature_where_part) END { $2, $5 }
  | SIG signature_body END { $2, [] }

signature_definition:
  | SIGNATURE IDENTIFIER EQ signature { `SignatureDecl ($2, false, $4)  }
  | GIVEN SIGNATURE IDENTIFIER EQ signature { `SignatureDecl ($3, true, $5)  }

signature_ref: | signature  { Signature $1 } | long_id { SignatureId $1  }

signature_where_part: type_variable* IDENTIFIER EQ ty { $1, $2, $4 }

structure_body_part:
  | val_definition { `SignatureInStruct $1 }
  | type_definition { `SignatureInStruct $1 }
  | exception_definition { `SignatureInStruct $1 }
  | let_definition { $1 }

structure:
  | STRUCT separated_list(semi, structure_body_part) END { $2 }
structure_with_constraint:
  | structure structure_signature_constraint { $1, $2 }

structure_definition:
  | GIVEN STRUCTURE IDENTIFIER structure_signature_constraint EQ structure  {
        `StructDecl (Some $3, true, (($6, $4): structure))
      }
  | GIVEN structure_with_constraint {
        `StructDecl (None, false, $2)
      }
  | STRUCTURE IDENTIFIER structure_signature_constraint EQ structure {
        `StructDecl (Some $2, false, ($5, $3))
      }

structure_signature_constraint: | { [] } | COLON separated_nonempty_list(COMMA, signature_ref) { $2 }

functor_definition:
  | FUNCTOR IDENTIFIER separated_list(COMMA, IDENTIFIER COLON signature_ref { $1, $3 }) structure_signature_constraint EQ structure { `FunctorDecl ($2, ($3 : (string * signature_ref) list), (($6, $4): structure)) }

definition:
  | signature_definition { $1 }
  | structure_definition { $1 }
  | functor_definition { $1 }

compilation_unit: definition*  EOF { CompilationUnit $1 }

