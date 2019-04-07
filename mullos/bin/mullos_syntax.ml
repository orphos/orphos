(* Copyright (C) 2019 Takezoe,Tomoaki <tomoaki@res.ac>
 *
 * SPDX-Identifier: LGPL-3.0-or-later
 *)
open Mullos_aux

type long_id = LongId of string list

let long_id components = LongId components

module Type = struct
  type level = int

  type ty =
    | TLongId of long_id
    | TApply of ty list * ty
    | TArrow of ty * ty
    | TTuple of ty list
    | TLazy of ty
    | TVar of tvar ref

  and tvar = Unbound of int * level | Link of ty | Generic of int

  let new_var level = TVar (ref (Unbound (new_oid (), level)))

  let new_gen_var () = TVar (ref (Generic (new_oid ())))

  let i1 = TLongId (long_id ["Orphos"; "Bool"; "t"])

  let i8 = TLongId (long_id ["Orphos"; "Int8"; "t"])

  let i16 = TLongId (long_id ["Orphos"; "Int16"; "t"])

  let i32 = TLongId (long_id ["Orphos"; "Int32"; "t"])

  let i64 = TLongId (long_id ["Orphos"; "Int64"; "t"])

  let u8 = TLongId (long_id ["Orphos"; "UInt8"; "t"])

  let u16 = TLongId (long_id ["Orphos"; "UInt16"; "t"])

  let u32 = TLongId (long_id ["Orphos"; "UInt32"; "t"])

  let u64 = TLongId (long_id ["Orphos"; "UInt64"; "t"])

  let z = TLongId (long_id ["Oprhos"; "Z"; "t"])

  let text = TLongId (long_id ["Oprhos"; "Text"; "t"])

  let unit = TLongId (long_id ["Orphos"; "Unit"; "t"])
end

type number_literal_type = ZType | QType | IntType of int | FloatType of int

type number = Q.t * number_literal_type

type bin_op =
  | Add
  | Substract
  | Multiply
  | Division
  | Xor
  | Reminder
  | BitwiseLeftShift
  | BitwiseRightShift
  | Less
  | Greater
  | Equal
  | NotEqual
  | BitwiseAnd
  | And
  | BitwiseOr
  | Or
  | AddAsign
  | SubstractAsign
  | Asign
  | Combine
  | Remove
  | Cons
  | Pipeline
  | Append
  | Prepend
  | Erase
  | Dot

type prefix_op =
  | Positive
  | Negative
  | Not
  | Deref
  | Ref
  | Raise
  | Lazy
  | PrefixIncrement
  | PrefixDecrement
  | BitwiseNot

type postfix_op = PostfixIncrement | PostfixDecrement

type pat_bin_op = [`Colon | `Comma | `At]

type exp' =
  | Bool of bool
  | Number of number
  | Text of string
  | Ident of long_id
  | Unit
  | Apply of exp * exp
  | BinOp of exp * bin_op * exp
  | PrefixOp of prefix_op * exp
  | PostfixOp of exp * postfix_op
  | Tuple of exp list
  | IfThenElse of exp * exp * exp option
  | Seq of exp list
  | Lambda of string * exp
  | Let of pat * pat list * exp * exp
  | LetRec of (pat * pat list * exp) list * exp
  | Match of exp * pat_clause list
  | ListLiteral of exp list
  | ArrayLiteral of exp list
  | RecordLiteral of exp option * (string * exp) list
  | RecordRestrictionLiteral of exp * string
  | RecordSelection of exp * string
  | PolymorphicVariantConstruction of string * exp
  | Handle of exp * pat_clause list

and exp = oid * exp'

and pat_clause' =
  | MatchPat of pat * exp option * exp
  | MatchException of pat * exp option * exp
  | MatchEffect of pat * exp option * exp

and pat_clause = oid * pat_clause'

and pat' =
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

and pat = oid * pat'

type ty_bin_op = TComma | TArrow | TApply

type type_exp' =
  | TIdent of long_id
  | TVar of string
  | TPointer of type_exp
  | TNumber of number
  | TText of string
  | TBool of bool
  | TLazy of type_exp
  | TLabel of string * type_exp
  | TEff of type_exp * long_id list
  | TBinOp of type_exp * ty_bin_op * type_exp * (ty_bin_op * type_exp) list
  | TRecord of type_exp option * (string * type_exp) list
  | TPolymorphicVariant of string * type_exp
  | TOr of type_exp list
  | TRefinement of type_exp * exp list
  | TGiven of type_exp * long_id list
  | TArrow of type_exp * type_exp
  | TTuple of type_exp list
  | TApply of type_exp list * type_exp

and type_exp = oid * type_exp'

type signature_part' =
  | TypeAlias of string list * string * type_exp
  | MonomorphicVariant of string list * string * (string * type_exp) list
  | TypeDecl of string list * string
  | ValDef of string * type_exp
  | ExceptionDef of string * type_exp option

type signature_part = oid * signature_part'

type struct_part' =
  | SignatureInStruct of signature_part
  | LetDef of string * exp
  | LetRecDef of (string * exp) list

type struct_part = oid * struct_part'

type signature = signature_part list * (string list * string * type_exp) list

type signature_ref' = Signature of signature | SignatureId of long_id

type signature_ref = oid * signature_ref'

type structure = struct_part list * signature_ref list

type decl' =
  | SignatureDecl of string * bool * signature
  | StructDecl of string option * bool * structure
  | FunctorDecl of string * (string * signature_ref) list * structure

type decl = oid * decl'

type compilation_unit' = CompilationUnit of decl list

type compilation_unit = oid * compilation_unit'
