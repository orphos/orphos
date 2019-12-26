(* Copyright (C) 2018-2019 Takezoe,Tomoaki <tomoaki3478@res.ac>
 *
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 *)
%parameter <Data : Syntax.Data>
%{

open Syntax
module Tree = Syntax.Make(Data)
open Tree

let new_tree x = Data.allocate (),  x

let without_loc (_, v) = v

%}

%token <Location.t> AMPERSAND
%token <Location.t> AND
%token <Location.t> AS
%token <Location.t> ASTERISK
%token <Location.t> AT
%token <Location.t> BIG_AMPERSAND
%token <Location.t> BIG_COLON
%token <Location.t> BIG_EQ
%token <Location.t> BIG_GREATER
%token <Location.t> BIG_HYPHEN
%token <Location.t> BIG_LESS
%token <Location.t> BIG_PLUS
%token <Location.t> BIG_VERTICAL
%token <Location.t * bool> BOOL
%token <Location.t> CASE
%token <Location.t> CIRCUMFLEX
%token <Location.t> COLON
%token <Location.t> COLON_EQ
%token <Location.t> COLON_HYPHEN
%token <Location.t> COLON_HYPHEN_COLON
%token <Location.t> COLON_PLUS
%token <Location.t> COLON_PLUS_COLON
%token <Location.t> COMMA
%token <Location.t> DOT
%token <Location.t> EFFECT
%token <Location.t> ELSE
%token <Location.t> END
%token <Location.t> EOF
%token <Location.t> EQ
%token <Location.t> EQ_GREATER
%token <Location.t> EXCEPTION
%token <Location.t> EXCLAMATION
%token <Location.t> EXCLAMATION_EQ
%token <Location.t> FN
%token <Location.t> GIVEN
%token <Location.t> GRAVE_ACCENT
%token <Location.t> GREATER
%token <Location.t> HANDLE
%token <Location.t> HYPHEN
%token <Location.t> HYPHEN_COLON
%token <Location.t> HYPHEN_EQ
%token <Location.t> HYPHEN_GREATER
%token <Location.t> IF
%token <Location.t> INTERFACE
%token <Location.t> LAZY
%token <Location.t> LCBRACKET
%token <Location.t> LBRACKET
%token <Location.t> LESS
%token <Location.t> LET
%token <Location.t * string> LOWER_IDENTIFIER
%token <Location.t> LOWLINE
%token <Location.t> LPAREN
%token <Location.t> MATCH
%token <Location.t> MODULE
%token <Location.t> NL
%token <Location.t * Syntax.number> NUMBER
%token <Location.t> NUMBERSIGN
%token <Location.t> OF
%token <Location.t> PERCENT
%token <Location.t> PLUS
%token <Location.t> PLUS_COLON
%token <Location.t> PLUS_EQ
%token <Location.t> RAISE
%token <Location.t> RBRACKET
%token <Location.t> RCBRACKET
%token <Location.t> REC
%token <Location.t> RPAREN
%token <Location.t> SEMI
%token <Location.t> SINGLE_QUOTE
%token <Location.t> SOLIDUS
%token <Location.t * string> TEXT
%token <Location.t> THEN
%token <Location.t> TILDE
%token <Location.t> TYPE
%token <Location.t * string> UPPER_IDENTIFIER
%token <Location.t> VAL
%token <Location.t> VERTICAL
%token <Location.t> VERTICAL_GREATER
%token <Location.t> WHEN
%token <Location.t> WHERE
%token <Location.t> WITH

%start<Syntax.Make(Data).compilation_unit> compilation_unit

%%

%inline
semi:
  | NL { () }
  | SEMI { () }

lower_id:
  | LOWER_IDENTIFIER { without_loc $1 }
  (* contextual keywords *)
  | AS { "as" }
  | INTERFACE { "interface" }
  | MODULE { "module" }
  | OF { "of" }
  | TYPE { "type" }
  | VAL { "val" }
  | WHEN { "when" }

upper_id: UPPER_IDENTIFIER { without_loc $1 }

lower_long_id: lower_id { LongId [$1] }
  | upper_id BIG_COLON lower_long_id { let LongId tail = $3 in LongId ($1 :: tail) }

upper_long_id: upper_id { LongId [$1] }
  | upper_id BIG_COLON upper_long_id { let LongId tail = $3 in LongId ($1 :: tail) }

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
  | LET name=lower_id params=list(lower_id { PIdent $1 |> new_tree }) EQ body=expression semi exp=expression { Let (PIdent name |> new_tree , params, body, exp) |> new_tree }
  | LET REC name=lower_id params=list(lower_id { PIdent $1 |> new_tree }) EQ body=expression
      ands=list(AND name=lower_id params=list(lower_id { PIdent $1 |> new_tree }) EQ body=expression { PIdent name |> new_tree, params, body }) semi exp=expression {
      LetRec ((PIdent name |> new_tree, params, body) :: ands, exp) |> new_tree
    }
  | assignment_expression HANDLE pattern_clause+ END { Handle ($1, $3) |> new_tree }
  | assignment_expression { $1 }
  | LBRACKET list_nonauto_semi(expression) RBRACKET { ListLiteral $2 |> new_tree }
  | NUMBERSIGN LBRACKET list_nonauto_semi(expression) RBRACKET { ArrayLiteral $3 |> new_tree }
  | LCBRACKET row=ioption(expression WITH { $1 }) fields=list_nonauto_semi(DOT lower_id EQ expression { $2, $4 }) RCBRACKET {
      let row = match row with
      | Some row -> row
      | None -> RecordEmpty |> new_tree in
      fields |> List.fold_left (fun acc -> function key, value -> RecordExtend (acc, key, value) |> new_tree) row
    }
  | GRAVE_ACCENT upper_id expression { PolymorphicVariantConstruction ($2, $3) |> new_tree }

assignment_expression: assignment_expression binop_assignment pipeline_expression { BinOp ($1, $2, $3) |> new_tree } | pipeline_expression { $1 }
%inline
binop_assignment:
  | PLUS_EQ { AddAsign }
  | HYPHEN_EQ { SubstractAsign }
  | COLON_EQ { Asign }

pipeline_expression: tuple_expression VERTICAL_GREATER pipeline_expression {
  (match $3 with
  | _, Construct (name, None) -> Construct (name, Some $1) |> new_tree
  | _ -> Apply ($3, $1) |> new_tree)
} | tuple_expression { $1 }

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

add_expression:
  | add_expression binop_add multiply_expression { BinOp ($1, $2, $3) |> new_tree }
  | left=add_expression HYPHEN DOT right=lower_id { RecordRestrictionLiteral (left, right) |> new_tree }
  | multiply_expression { $1 }
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

dot_expression: dot_expression DOT lower_id { RecordSelection ($1, $3) |> new_tree } | simple_expression { $1 }

simple_expression:
  | lower_long_id { Ident $1 |> new_tree }
  | upper_long_id { Construct ($1, None) |> new_tree}
  | LPAREN RPAREN { Unit |> new_tree }
  | LCBRACKET seq RCBRACKET { Seq $2 |> new_tree }
  | TEXT { Text (without_loc $1) |> new_tree }
  | NUMBER { Number (without_loc $1) |> new_tree }
  | BOOL { Bool (without_loc $1) |> new_tree }
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
  | GRAVE_ACCENT upper_id pattern { PPolymorphicVariant ($2, $3) |> new_tree }

tuple_pattern: lazy_pattern COMMA separated_nonempty_list(COMMA, lazy_pattern) { PTuple ($1 :: $3) |> new_tree } | lazy_pattern { $1 }

lazy_pattern: LAZY capture_pattern { PLazy $2 |> new_tree } | capture_pattern { $1 }

capture_pattern:
  | ctor_pattern AS lower_id { PCapture ($3, $1) |> new_tree } | ctor_pattern { $1 }

ctor_pattern:
  | upper_id simple_pattern { PCtor ($1, $2) |> new_tree } | simple_pattern { $1 }

simple_pattern:
  | lower_id { PIdent $1 |> new_tree }
  | LPAREN RPAREN { PUnit |> new_tree }
  | LPAREN pattern RPAREN { $2 }
  | LOWLINE { PWildcard |> new_tree }
  | TEXT { PText (without_loc $1) |> new_tree }
  | NUMBER { PNumber (without_loc $1) |> new_tree }
  | BOOL { PBool (without_loc $1) |> new_tree }

ty:
  | refinement_ty { $1 }
  | LCBRACKET row=option(ty WITH { $1 }) fields=list_nonauto_semi(DOT lower_id COLON ty { $2, $4 }) RCBRACKET { ERecord (row, fields) |> new_tree }
  | GRAVE_ACCENT upper_id OF ty { EPolymorphicVariant ($2, $4) |> new_tree }
  | LBRACKET separated_list(VERTICAL, ty { $1 }) RBRACKET { EOr $2 |> new_tree }

refinement_ty:
  | refinement_ty WHERE refinement_body END { ERefinement ($1, $3) |> new_tree } | given_ty { $1 }

refinement_body:
  | expression { [$1] }
  | semi { [] }
  | { [] }
  | expression semi refinement_body { $1 :: $3 }

given_ty:
  | given_ty GIVEN separated_nonempty_list(COMMA, lower_long_id) { EGiven ($1, $3) |> new_tree } | fun_ty { $1 }

fun_ty:
  | fun_ty HYPHEN_GREATER tuple_ty { EArrow ($1, $3) |> new_tree } | tuple_ty { $1 }

tuple_ty:
  | ty_with_effect nonempty_list(ASTERISK ty_with_effect { $2 }) { ETuple ($1 :: $2) |> new_tree } | ty_with_effect { $1 }

ty_with_effect:
  | application_ty AT separated_nonempty_list(COMMA, lower_long_id) { EEff ($1, $3) |> new_tree } | application_ty { $1 }

application_ty:
  | simple_ty application_ty {
      match $2 with
        | _, EApply (params, fn) -> EApply ($1 :: params, fn) |> new_tree
        | fn -> EApply ([$1], fn) |> new_tree
      }
  | simple_ty { $1 }

simple_ty:
  | lower_long_id { EIdent $1 |> new_tree }
  | type_variable { EGeneric $1 |> new_tree }
  | LPAREN ty RPAREN { $2 }

type_variable: SINGLE_QUOTE lower_id { $2 }

type_definition:
  | TYPE type_variable* lower_id { TypeDecl ($2, $3) |> new_tree }
  | TYPE type_variable* lower_id EQ ty { TypeAlias ($2, $3, $5) |> new_tree }
  | TYPE params=type_variable* name=lower_id EQ ioption(VERTICAL) ctor=upper_id OF ty=ty tail=nonempty_list(VERTICAL upper_id OF ty {
    Ctor ($2, $4) |> new_tree }) {
      MonomorphicVariant (params, name, (Ctor (ctor, ty) |> new_tree) :: tail) |> new_tree }

val_definition: VAL name=lower_id COLON ty=ty { ValDef (name, ty) |> new_tree }

let_definition:
  | LET bindant=pattern EQ exp=expression { LetDef (bindant, exp) |> new_tree }
  | LET REC bindant=pattern EQ exp=expression ands=list(AND bindant=pattern EQ exp=expression{ LetRecDefPart (bindant, exp) |> new_tree }) { LetRecDef ((LetRecDefPart (bindant, exp) |> new_tree) :: ands) |> new_tree }

exception_definition:
  | EXCEPTION upper_id option(OF ty { $2 }) { ExceptionDef ($2, $3) |> new_tree }

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
  | INTERFACE upper_id EQ interface_exp_tail { InterfaceDecl ($2, false, $4)  |> new_tree }
  | GIVEN INTERFACE upper_id EQ interface_exp_tail { InterfaceDecl ($3, true, $5)  |> new_tree }

interface_ref: interface_exp  { InterfaceExp $1 |> new_tree } | upper_long_id { InterfaceId $1  |> new_tree }

interface_where_part: type_variable* lower_id EQ ty { $1, $2, $4 }

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
  | GIVEN MODULE upper_id module_interface_constraint EQ module_exp  {
        ModuleDecl (Some ($3), true, (($6, $4): module_exp)) |> new_tree
      }
  | GIVEN module_with_constraint {
        ModuleDecl (None, false, $2) |> new_tree
      }
  | MODULE upper_id module_interface_constraint EQ module_exp {
        ModuleDecl (Some ($2), false, ($5, $3)) |> new_tree
      }
  | MODULE upper_id separated_nonempty_list(COMMA, upper_id COLON interface_ref { $1, $3 }) module_interface_constraint EQ module_exp {
    FunctorDecl ($2, ($3 : (string * interface_ref) list), (($6, $4): module_exp)) |> new_tree }

module_interface_constraint: | { [] } | COLON separated_nonempty_list(COMMA, interface_ref) { $2 }

decl:
  | interface_decl { $1 }
  | module_or_functor_decl { $1 }

compilation_unit: decl*  EOF { CompilationUnit $1 |> new_tree }

