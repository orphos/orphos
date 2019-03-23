(* Copyright (C) 2019 Takezoe,Tomoaki <tomoaki@res.ac>
 *
 * SPDX-Identifier: LGPL-3.0-or-later
 *)

type long_id = LongId of string list

type number_literal_type = ZType | QType | IntType of int | FloatType of int

type number = Q.t * number_literal_type

type bin_op =
  [ `Add
  | `Substract
  | `Multiply
  | `Division
  | `Xor
  | `Reminder
  | `BitwiseLeftShift
  | `BitwiseRightShift
  | `Less
  | `Greater
  | `Equal
  | `NotEqual
  | `BitwiseAnd
  | `And
  | `BitwiseOr
  | `Or
  | `AddAsign
  | `SubstractAsign
  | `Asign
  | `Combine
  | `Remove
  | `Cons
  | `Pipeline
  | `Append
  | `Prepend
  | `Erase
  | `Dot ]

type prefix_op = [ `Positive | `Negative | `Not | `Deref | `Ref | `Raise | `Lazy | `Increment | `Decrement | `BitwiseNot ]

type postfix_op = [ `Increment | `Decrement ]

type pat_bin_op = [`Colon | `Comma | `At]

type exp =
  | Bool of bool
  | Number of number
  | Text of string
  | Identifier of string
  | Unit
  | Apply of exp * exp
  | BinOp of exp * bin_op * exp
  | PrefixOp of prefix_op * exp
  | PostfixOp of exp * postfix_op
  | Tuple of exp list
  | IfThenElse of exp * exp * exp option
  | Seq of exp list
  | Lambda of pat * exp
  | Let of pat * pat list * exp * exp
  | LetRec of (pat * pat list * exp) list * exp
  | Label of string * exp
  | Match of exp * pat_clause list
  | ListLiteral of exp list
  | ArrayLiteral of exp list
  | RecordLiteral of exp option * (string * exp) list
  | RecordRestrictionLiteral of exp * string
  | RecordSelection of exp * string
  | PolymorphicVariantConstruction of string * exp
  | Handle of exp * pat_clause list

and pat_clause =
  | MatchPat of pat * exp option * exp
  | MatchException of pat * exp option * exp
  | MatchEffect of pat * exp option * exp

and pat =
  | PIdent of string
  | PUnit
  | PCapture of string * pat
  | PCtor of string * pat
  | PTuple of pat list
  | PWildcard
  | PText of string
  | PNumber of number
  | PBool of bool
  | PLazy of pat
  | POr of pat * pat
  | PListLiteral of pat list
  | PArrayLiteral of pat list
  | PPolymorphicVariant of string * pat

type ty_bin_op = TComma | TArrow | TApply

type ty =
  | TIdent of long_id
  | TVar of string
  | TPointer of ty
  | TNumber of number
  | TText of string
  | TBool of bool
  | TLazy of ty
  | TLabel of string * ty
  | TEff of ty * long_id list
  | TBinOp of ty * ty_bin_op * ty * (ty_bin_op * ty) list
  | TRecord of ty option * (string * ty) list
  | TPolymorphicVariant of string * ty
  | TOr of ty list
  | TRefinement of ty * exp list
  | TGiven of ty * long_id list
  | TArrow of ty * ty
  | TTuple of ty list
  | TApply of ty list * ty

type signature_part = [
  | `TypeAlias of string list * string * ty
  | `MonomorphicVariant of string list * string * (string * ty) list
  | `TypeDecl of string list * string
  | `ValDef of string * ty
  | `ExceptionDef of string * ty option
]
type struct_part = [
  | `SignatureInStruct of signature_part
  | `LetDef of string * exp
  | `LetRecDef of (string * exp) list
]

type signature = signature_part list * (string list * string * ty) list

type signature_decl = [`SignatureDecl of string * bool * signature]

type signature_ref = Signature of signature | SignatureId of long_id

type structure = struct_part list * signature_ref list

type struct_decl = [`StructDecl of string option * bool * structure]

type functor_decl = [`FunctorDecl of string * (string * signature_ref) list * structure]

type compilation_unit = CompilationUnit of [signature_decl | struct_decl | functor_decl] list

