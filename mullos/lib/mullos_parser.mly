(* Copyright (C) 2018-2019 Takezoe,Tomoaki <tomoaki@res.ac>
 *
 * SPDX-Identifier: Apache-2.0 WITH LLVM-exception
 *)
%parameter <Data : Mullos_syntax.Data>
%{

open Mullos_aux
open Mullos_syntax
module Tree = Mullos_syntax.Make(Data)
open Tree

let noimpl () = failwith "not implemented"

let new_tree x = Data.allocate (),  x

%}

%token AMPERSAND
%token AND
%token AS
%token ASTERISK
%token AT
%token BIG_AMPERSAND
%token BIG_COLON
%token BIG_EQ
%token BIG_GREATER
%token BIG_HYPHEN
%token BIG_LESS
%token BIG_PLUS
%token BIG_VERTICAL
%token <bool> BOOL
%token CASE
%token CIRCUMFLEX
%token COLON
%token COLON_EQ
%token COLON_HYPHEN
%token COLON_HYPHEN_COLON
%token COLON_PLUS
%token COLON_PLUS_COLON
%token COMMA
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
%token FN
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
%token INTERFACE
%token LAZY
%token LCBRACKET
%token LBRACKET
%token LESS
%token LET
%token LOWLINE
%token LPAREN
%token MATCH
%token MODULE
%token NL
%token <Mullos_syntax.number> NUMBER
%token NUMBERSIGN
%token OF
%token PERCENT
%token PLUS
%token PLUS_COLON
%token PLUS_EQ
%token RAISE
%token RBRACKET
%token RCBRACKET
%token REC
%token RPAREN
%token SEMI
%token SINGLE_QUOTE
%token SOLIDUS
%token <string> TEXT
%token THEN
%token TILDE
%token TYPE
%token VAL
%token VERTICAL
%token VERTICAL_GREATER
%token WHEN
%token WHERE
%token WITH
%token WITHOUT

%start<Mullos_syntax.Make(Data).compilation_unit> compilation_unit

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
  | IF expression THEN expression ELSE expression { IfThenElse ($2, $4, Some $6) |> new_tree }
  | IF expression THEN expression END { IfThenElse ($2, $4, None) |> new_tree }
  | MATCH lhs=expression WITH rhs=pattern_clause+ END { Match (lhs, rhs) |> new_tree }
  | FN pattern HYPHEN_GREATER expression { Lambda ($2, $4) |> new_tree }
  | LET name=IDENTIFIER params=list(IDENTIFIER { PIdent $1 |> new_tree }) EQ body=expression semi exp=expression { Let (PIdent name |> new_tree , params, body, exp) |> new_tree }
  | LET REC name=IDENTIFIER params=list(IDENTIFIER { PIdent $1 |> new_tree }) EQ body=expression ands=list(AND name=IDENTIFIER params=list(IDENTIFIER { PIdent $1 |> new_tree }) EQ body=expression { PIdent name |> new_tree, params, body }) semi exp=expression {
      LetRec ((PIdent name |> new_tree, params, body) :: ands, exp) |> new_tree
    }
  | assignment_expression HANDLE pattern_clause+ END { Handle ($1, $3) |> new_tree }
  | assignment_expression { $1 }
  | LBRACKET list_nonauto_semi(expression) RBRACKET { ListLiteral $2 |> new_tree }
  | NUMBERSIGN LBRACKET list_nonauto_semi(expression) RBRACKET { ArrayLiteral $3 |> new_tree }
  | LCBRACKET row=ioption(expression WITH { $1 }) fields=list_nonauto_semi(DOT IDENTIFIER EQ expression { $2, $4 }) RCBRACKET { RecordLiteral (row, fields) |> new_tree }
  | LCBRACKET left=expression WITHOUT DOT right=IDENTIFIER RCBRACKET { RecordRestrictionLiteral (left, right) |> new_tree }
  | GRAVE_ACCENT IDENTIFIER expression { PolymorphicVariantConstruction ($2, $3) |> new_tree }

assignment_expression: assignment_expression binop_assignment pipeline_expression { BinOp ($1, $2, $3) |> new_tree } | pipeline_expression { $1 }
%inline
binop_assignment:
  | PLUS_EQ { AddAsign }
  | HYPHEN_EQ { SubstractAsign }
  | COLON_EQ { Asign }

pipeline_expression: tuple_expression VERTICAL_GREATER pipeline_expression { BinOp ($1, Pipeline, $3) |> new_tree } | tuple_expression { $1 }

tuple_expression: lor_expression nonempty_list(COMMA lor_expression { $2 }) { Tuple ($1 :: $2) |> new_tree } | lor_expression { $1 }

lor_expression: lor_expression BIG_VERTICAL land_expression { BinOp ($1, Or, $3) |> new_tree } | land_expression { $1 }

land_expression: land_expression BIG_AMPERSAND bitwise_or_expression { BinOp ($1, And, $3) |> new_tree } | bitwise_or_expression { $1 }

bitwise_or_expression: bitwise_or_expression VERTICAL xor_expression { BinOp ($1, BitwiseOr, $3) |> new_tree } | xor_expression { $1 }

xor_expression: xor_expression CIRCUMFLEX bitwise_and_expression { BinOp ($1, Xor, $3) |> new_tree } | bitwise_and_expression { $1 }

bitwise_and_expression: bitwise_and_expression AMPERSAND equal_expression { BinOp ($1, BitwiseAnd, $3) |> new_tree } | equal_expression { $1 }

equal_expression: equal_expression binop_equal greater_expression { BinOp ($1, $2, $3) |> new_tree } | greater_expression { $1 }
%inline
binop_equal:
  | EXCLAMATION_EQ { NotEqual }
  | BIG_EQ { Equal }

greater_expression: greater_expression binop_greater shift_expression { BinOp ($1, $2, $3) |> new_tree } | shift_expression { $1 }
%inline
binop_greater:
  | LESS { Less }
  | GREATER { Greater }

shift_expression: shift_expression binop_shift cons_expression { BinOp ($1, $2, $3) |> new_tree } | cons_expression { $1 }
%inline
binop_shift:
  | BIG_LESS { BitwiseLeftShift }
  | BIG_GREATER { BitwiseRightShift }

cons_expression: add_expression binop_cons cons_expression { BinOp ($3, $2, $1) |> new_tree } | add_expression { $1 }
%inline
binop_cons:
   | PLUS_COLON { Append }
   | HYPHEN_COLON { Erase }

add_expression: add_expression binop_add multiply_expression { BinOp ($1, $2, $3) |> new_tree } | multiply_expression { $1 }
%inline
binop_add:
  | PLUS { Add }
  | HYPHEN { Substract }
  | COLON_PLUS { Prepend }
  | COLON_HYPHEN { Erase }
  | COLON_PLUS_COLON { Combine }
  | COLON_HYPHEN_COLON { Remove }

multiply_expression: multiply_expression binop_multiply prefix_expression { BinOp ($1, $2, $3) |> new_tree } | prefix_expression { $1 }
%inline
binop_multiply:
  | ASTERISK { Multiply }
  | SOLIDUS  { Division }
  | PERCENT { Reminder }

prefix_expression: prefix_op postfix_expression { PrefixOp ($1, $2) |> new_tree } | postfix_expression { $1 }
%inline
prefix_op:
  | PLUS { Positive }
  | HYPHEN { Negative }
  | EXCLAMATION { Not }
  | AMPERSAND { Ref }
  | ASTERISK { Deref }
  | BIG_PLUS { PrefixIncrement }
  | BIG_HYPHEN { PrefixDecrement }
  | RAISE { Raise }
  | LAZY { Lazy }
  | TILDE { BitwiseNot }

postfix_expression: application_expression postfix_op { PostfixOp ($1, $2) |> new_tree } | application_expression { $1 }
%inline
postfix_op:
  | BIG_PLUS { PostfixIncrement }
  | BIG_HYPHEN { PostfixDecrement }

application_expression: application_expression dot_expression { Apply ($1, $2) |> new_tree } | dot_expression { $1 }

dot_expression: dot_expression DOT IDENTIFIER { RecordSelection ($1, $3) |> new_tree } | simple_expression { $1 }

simple_expression:
  | long_id { Ident $1 |> new_tree }
  | LPAREN RPAREN { Unit |> new_tree }
  | LCBRACKET seq RCBRACKET { Seq $2 |> new_tree }
  | TEXT { Text $1 |> new_tree }
  | NUMBER { Number $1 |> new_tree }
  | BOOL { Bool $1 |> new_tree }
  | LPAREN expression RPAREN { $2 }

pattern_clause:
  | CASE pat=pattern_or_clause cond=pattern_condition? EQ_GREATER exp=expression { MatchPat (pat, cond, exp) |> new_tree }
  | CASE EXCEPTION pat=pattern_or_clause cond=pattern_condition? EQ_GREATER exp=expression { MatchException (pat, cond, exp) |> new_tree }
  | CASE EFFECT pat=pattern_or_clause cond=pattern_condition? EQ_GREATER exp=expression { MatchEffect (pat, cond, exp) |> new_tree }

pattern_or_clause:
  | pattern { $1 }
  | pattern VERTICAL pattern_or_clause { POr ($1, $3) |> new_tree }

pattern_condition: WHEN expression { $2 }

pattern:
  | LBRACKET list_nonauto_semi(pattern) RBRACKET { PListLiteral $2 |> new_tree }
  | NUMBERSIGN LBRACKET list_nonauto_semi(pattern) RBRACKET { PArrayLiteral $3 |> new_tree }
  | tuple_pattern { $1 }
  | GRAVE_ACCENT IDENTIFIER pattern { PPolymorphicVariant ($2, $3) |> new_tree }

tuple_pattern: lazy_pattern COMMA separated_nonempty_list(COMMA, lazy_pattern) { PTuple ($1 :: $3) |> new_tree } | lazy_pattern { $1 }

lazy_pattern: LAZY capture_pattern { PLazy $2 |> new_tree } | capture_pattern { $1 }

capture_pattern:
  | ctor_pattern AS IDENTIFIER { PCapture ($3, $1) |> new_tree } | ctor_pattern { $1 }

ctor_pattern:
  | IDENTIFIER simple_pattern { PCtor ($1, $2) |> new_tree } | simple_pattern { $1 }

simple_pattern:
  | IDENTIFIER { PIdent $1 |> new_tree }
  | LPAREN RPAREN { PUnit |> new_tree }
  | LPAREN pattern RPAREN { $2 }
  | LOWLINE { PWildcard |> new_tree }
  | TEXT { PText $1 |> new_tree }
  | NUMBER { PNumber $1 |> new_tree }
  | BOOL { PBool $1 |> new_tree }

ty:
  | refinement_ty { $1 }
  | LCBRACKET row=option(ty WITH { $1 }) fields=list_nonauto_semi(DOT IDENTIFIER COLON ty { $2, $4 }) RCBRACKET { TRecord (row, fields) |> new_tree }
  | GRAVE_ACCENT IDENTIFIER OF ty { TPolymorphicVariant ($2, $4) |> new_tree }
  | LBRACKET separated_list(VERTICAL, ty { $1 }) RBRACKET { TOr $2 |> new_tree }

refinement_ty:
  | refinement_ty WHERE refinement_body END { TRefinement ($1, $3) |> new_tree } | given_ty { $1 }

refinement_body:
  | expression { [$1] }
  | semi { [] }
  | { [] }
  | expression semi refinement_body { $1 :: $3 }

given_ty:
  | given_ty GIVEN separated_nonempty_list(COMMA, long_id) { TGiven ($1, $3) |> new_tree } | fun_ty { $1 }

fun_ty:
  | fun_ty HYPHEN_GREATER tuple_ty { TArrow ($1, $3) |> new_tree } | tuple_ty { $1 }

tuple_ty:
  | ty_with_effect nonempty_list(ASTERISK ty_with_effect { $2 }) { TTuple ($1 :: $2) |> new_tree } | ty_with_effect { $1 }

ty_with_effect:
  | application_ty AT separated_nonempty_list(COMMA, long_id) { TEff ($1, $3) |> new_tree } | application_ty { $1 }

application_ty:
  | simple_ty application_ty {
      match $2 with
        | _, TApply (params, fn) -> TApply ($1 :: params, fn) |> new_tree
        | fn -> TApply ([$1], fn) |> new_tree
      }
  | simple_ty { $1 }

simple_ty:
  | long_id { TIdent $1 |> new_tree }
  | type_variable { TGeneric $1 |> new_tree }
  | LPAREN ty RPAREN { $2 }

type_variable: SINGLE_QUOTE IDENTIFIER { $2 }

type_definition:
  | TYPE type_variable* IDENTIFIER { TypeDecl ($2, $3) |> new_tree }
  | TYPE type_variable* IDENTIFIER EQ ty { TypeAlias ($2, $3, $5) |> new_tree }
  | TYPE params=type_variable* name=IDENTIFIER EQ ioption(VERTICAL) ctor=IDENTIFIER OF ty=ty tail=nonempty_list(VERTICAL IDENTIFIER OF ty { Ctor ($2, $4) |> new_tree }) { MonomorphicVariant (params, name, (Ctor (ctor, ty) |> new_tree) :: tail) |> new_tree }

val_definition: VAL name=IDENTIFIER COLON ty=ty { ValDef (name, ty) |> new_tree }

let_definition:
  | LET bindant=pattern EQ exp=expression { LetDef (bindant, exp) |> new_tree }
  | LET REC bindant=pattern EQ exp=expression ands=list(AND bindant=pattern EQ exp=expression{ LetRecDefPart (bindant, exp) |> new_tree }) { LetRecDef ((LetRecDefPart (bindant, exp) |> new_tree) :: ands) |> new_tree }

exception_definition:
  | EXCEPTION IDENTIFIER option(OF ty { $2 }) { ExceptionDef ($2, $3) |> new_tree }

interface_body_part:
  | val_definition { $1 }
  | type_definition { $1 }
  | exception_definition { $1 }

interface_body: separated_list(semi, interface_body_part) { $1 }

interface_exp_tail:
  | interface_body END WHERE list_nonauto_semi(interface_where_part) END { $1, $4 }
  | interface_body END { $1, [] }
interface_exp: INTERFACE interface_exp_tail { $2 }

interface_decl:
  | INTERFACE IDENTIFIER EQ interface_exp_tail { InterfaceDecl ($2, false, $4)  |> new_tree }
  | GIVEN INTERFACE IDENTIFIER EQ interface_exp_tail { InterfaceDecl ($3, true, $5)  |> new_tree }

interface_ref: interface_exp  { InterfaceExp $1 |> new_tree } | long_id { InterfaceId $1  |> new_tree }

interface_where_part: type_variable* IDENTIFIER EQ ty { $1, $2, $4 }

module_body_part:
  | val_definition { TypeDeclInModule $1 |> new_tree }
  | type_definition { TypeDeclInModule $1 |> new_tree }
  | exception_definition { TypeDeclInModule $1 |> new_tree }
  | let_definition { $1 }

module_exp:
  | MODULE separated_list(semi, module_body_part) END { $2 }
module_with_constraint:
  | module_exp module_interface_constraint { $1, $2 }

module_or_functor_decl:
  | GIVEN MODULE IDENTIFIER module_interface_constraint EQ module_exp  {
        ModuleDecl (Some $3, true, (($6, $4): module_exp)) |> new_tree
      }
  | GIVEN module_with_constraint {
        ModuleDecl (None, false, $2) |> new_tree
      }
  | MODULE IDENTIFIER module_interface_constraint EQ module_exp {
        ModuleDecl (Some $2, false, ($5, $3)) |> new_tree
      }
  | MODULE IDENTIFIER separated_nonempty_list(COMMA, IDENTIFIER COLON interface_ref { $1, $3 }) module_interface_constraint EQ module_exp { FunctorDecl ($2, ($3 : (string * interface_ref) list), (($6, $4): module_exp)) |> new_tree }

module_interface_constraint: | { [] } | COLON separated_nonempty_list(COMMA, interface_ref) { $2 }

decl:
  | interface_decl { $1 }
  | module_or_functor_decl { $1 }

compilation_unit: decl*  EOF { CompilationUnit $1 |> new_tree }

