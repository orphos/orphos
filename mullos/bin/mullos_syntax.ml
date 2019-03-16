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

type prefix_op = [ `Positive | `Negative | `Not | `Deref | `Ref | `Raise | `Lazy | `Increment | `Decrement ]

type postfix_op = [ `Positive | `Negative | `Not | `Deref | `Ref | `Raise | `Lazy | `Increment | `Decrement ]

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

type definition =
  | VariantDef of string * ty list * deriving option * typedef_body
  | ExtensibleVariantDef of string * ctor
  | ModuleDef of bool * string * ty list list * definition list
  | TraitDef of string * string list * ty list list * definition list
  | LetDef of exp
  | ValDef of string * ty

and typedef_body = Variant of ctor list | ExtensibleVariant

and ctor = string * ty option

and deriving = ty list
