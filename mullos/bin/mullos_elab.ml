(* Copyright (C) 2019 Takezoe,Tomoaki <tomoaki3478@res.ac>
 *
 * SPDX-Identifier: LGPL-3.0-or-later
 *)
open Mullos_aux
open Mullos_syntax

type data = {mutable ty_field: Mullos_syntax.Type.ty option}

let get_ty = function
  | {ty_field= Some ty} -> ty
  | {ty_field= None} -> bug "ty is not allocated yet"

let set_ty data ty = data.ty_field <- Some ty

module ElabData = struct
  type t = data

  let allocate () = {ty_field= None}
end

open ElabData
module Tree = Mullos_syntax.Make (ElabData)
open Tree
open Mullos_syntax.Type

exception TypeError of string

let error msg = raise (TypeError ("type error: " ^ msg))

module LongIdIsOrdered = struct
  type t = long_id

  let compare left right =
    let rec aux = function
      | [], [] -> 0
      | h1 :: t1, h2 :: t2 ->
          let x = String.compare h1 h2 in
          if x == 0 then compare t1 t2 else x
      | h :: t, [] -> 1
      | [], h :: t -> -1
    in
    let LongId left, LongId right = (left, right) in
    aux (left, right)
end

module IdMap = Map.Make (LongIdIsOrdered)

type env = ty IdMap.t

let empty = IdMap.empty

let extend env name ty = IdMap.add name ty env

let lookup env name = IdMap.find name env

let occurs_check_adjust_levels tvar_id tvar_level ty =
  let rec f = function
    | TVar {contents= Link ty} -> f ty
    | TVar {contents= Generic _} -> bug "occurs check to generic variable"
    | TVar ({contents= Unbound (other_id, other_level)} as other_tvar) ->
        if other_id = tvar_id then error "recursive types"
        else if other_level > tvar_level then
          other_tvar := Unbound (other_id, tvar_level)
        else ()
    | TApply (ty_args, ty) -> f ty ; List.iter f ty_args
    | TArrow (param_ty, return_ty) -> f param_ty ; f return_ty
    | TLongId _ -> ()
    | TLazy ty -> f ty
    | TTuple tys -> List.iter f tys
  in
  f ty

let rec unify ty1 ty2 =
  if ty1 == ty2 then ()
  else
    match (ty1, ty2) with
    | TLongId long_id1, TLongId long_id2 when long_id1 = long_id2 -> ()
    | TApply (ty_args1, ty1), TApply (ty_args2, ty2) ->
        unify ty1 ty2 ;
        List.iter2 unify ty_args1 ty_args2
    | TArrow (param_ty1, return_ty1), TArrow (param_ty2, return_ty2) ->
        unify param_ty1 param_ty2 ;
        unify return_ty1 return_ty2
    | TVar {contents= Link ty1}, ty2 | ty1, TVar {contents= Link ty2} ->
        unify ty1 ty2
    | TVar {contents= Unbound (id1, _)}, TVar {contents= Unbound (id2, _)}
      when id1 = id2 ->
        bug "multiple instance of a particular type variable."
    | TVar ({contents= Unbound (id, level)} as tvar), ty
     |ty, TVar ({contents= Unbound (id, level)} as tvar) ->
        occurs_check_adjust_levels id level ty ;
        tvar := Link ty
    | TLazy _, _ -> noimpl "lazy"
    | TTuple xs, TTuple ys -> List.iter2 unify xs ys
    | _, _ -> error "cannot unify types "

let rec generalize level = function
  | TVar {contents= Unbound (id, other_level)} when other_level > level ->
      TVar (ref (Generic id))
  | TApply (ty_args, ty) ->
      TApply (List.map (generalize level) ty_args, generalize level ty)
  | TArrow (param_ty, return_ty) ->
      TArrow (generalize level param_ty, generalize level return_ty)
  | TTuple tys -> TTuple (List.map (generalize level) tys)
  | TLazy ty -> TLazy (generalize level ty)
  | TVar {contents= Link ty} -> generalize level ty
  | (TVar {contents= Generic _} | TVar {contents= Unbound _} | TLongId _) as ty
    ->
      ty

let instantiate level ty =
  let id_var_map = Hashtbl.create 10 in
  let rec f ty =
    match ty with
    | TLongId _ -> ty
    | TVar {contents= Link ty} -> f ty
    | TVar {contents= Generic id} -> (
      try Hashtbl.find id_var_map id with Not_found ->
        let var = new_var level in
        Hashtbl.add id_var_map id var ;
        var )
    | TVar {contents= Unbound _} -> ty
    | TApply (ty_args, ty) -> TApply (List.map f ty_args, f ty)
    | TArrow (param_ty, return_ty) -> TArrow (f param_ty, f return_ty)
    | TTuple tys -> TTuple (List.map f tys)
    | TLazy ty -> TLazy (f ty)
  in
  f ty

let rec elab_type type_env level = function
  | data, type_exp ->
      let elab = elab_type type_env level in
      let ty =
        match type_exp with
        | TIdent id -> lookup type_env id
        | TGeneric id -> lookup type_env (long_id [id])
        | TLazy _ -> noimpl "lazy"
        | TLabel _ -> noimpl "record"
        | TEff _ -> noimpl "effect"
        | TRecord _ -> noimpl "record"
        | TPolymorphicVariant _ -> noimpl "polymorphic variant"
        | TOr _ -> noimpl "union type"
        | TRefinement _ -> noimpl "refinement type"
        | TGiven _ -> noimpl "given type"
        | TArrow (param, ret) -> Type.TArrow (elab param, elab ret)
        | TTuple elems -> Type.TTuple (elems |> List.map elab)
        | TApply (params, applicant) ->
            Type.TApply (params |> List.map elab, elab applicant)
      in
      set_ty data ty ; ty

let rec elab_pat (env : env) (level : level) = function
  | data, pat' ->
      let ty =
        match pat' with
        | PIdent _ -> new_var level
        | PUnit -> TLongId (LongId ["unit"])
        | PCapture (_, pat) -> elab_pat env level pat
        | PCtor (ctor, pat) -> noimpl "variant constructor pattern"
        | PTuple pats ->
            TTuple (List.map (fun pat -> elab_pat env level pat) pats)
        | PWildcard -> new_var level
        | PText _ -> TLongId (LongId ["text"])
        | PNumber _ -> TLongId (LongId ["i32"])
        | PBool _ -> TLongId (LongId ["bool"])
        | PLazy pat -> TLazy (elab_pat env level pat)
        | POr (pat1, pat2) ->
            unify (elab_pat env level pat1) (elab_pat env level pat2) ;
            noimpl "union pattern"
        | PListLiteral pats -> noimpl "list pattern"
        | PArrayLiteral pats -> noimpl "array pattern"
        | PPolymorphicVariant (label, pat) ->
            noimpl "polymorphic variant pattern"
      in
      set_ty data ty ; ty

let rec elab_exp env level = function
  | data, exp' ->
      let elab = elab_exp env level in
      let ret =
        match exp' with
        | Ident name -> (
          try instantiate level (lookup env name) with Not_found ->
            error "variable not found" )
        | Lambda (param, body) ->
            let param_ty = new_var level in
            let env = extend env (LongId [param]) param_ty in
            let return_ty = elab_exp env level body in
            TArrow (param_ty, return_ty)
        | Let ((patId, PIdent name), params, value, body) ->
            let value_ty = elab_exp env (level + 1) value in
            let generalized_ty = generalize level value_ty in
            set_ty data generalized_ty ;
            elab_exp (extend env (LongId [name]) generalized_ty) level body
        | Let _ -> noimpl "binding to pattern"
        | LetRec (lets, body) ->
            let rec allocate_type_vars env = function
              | ((pat_data, PIdent name), params, value) :: t ->
                  let ty = new_var (level + 1) in
                  set_ty pat_data ty ;
                  allocate_type_vars (extend env (LongId [name]) ty) t
              | ((patId, _), params, value) :: t -> noimpl "binding to pattern"
              | [] -> env
            in
            let env = allocate_type_vars env lets in
            let rec elabBindees env = function
              | ((pat_data, PIdent name), params, value) :: t ->
                  let value_ty = elab_exp env (level + 1) value in
                  set_ty pat_data value_ty ;
                  elabBindees (extend env (LongId [name]) value_ty) t
              | [] -> env
              | ((_, _), _, _) :: _ -> noimpl "binding to pattern"
            in
            let env = elabBindees env lets in
            (* elab_exp body *)
            elab_exp env level body
        | Apply (fn, arg) -> (
          match elab_exp env level fn with
          | TArrow (param, ret) ->
              let arg = elab_exp env level arg in
              unify param arg ; ret
          | _ -> error "expected function" )
        | Bool _ -> i1
        | Number _ -> i64 (* TODO: implement integer suffix  *)
        | Text _ -> text
        | Unit -> unit
        | BinOp (left, op, right) -> (
          (* TODO: many of binary operators are planned to be desugared to a call to type class method *)
          match op with
          | Add | Substract | Multiply | Division | Xor | Reminder ->
              unify i64 (elab left) ;
              unify i64 (elab right) ;
              i64
          | BitwiseLeftShift | BitwiseRightShift | BitwiseAnd | BitwiseOr ->
              unify u64 (elab left) ;
              unify u64 (elab right) ;
              i64
          | Less | Greater ->
              unify i64 (elab left) ;
              unify i64 (elab right) ;
              i1
          | Equal | NotEqual ->
              unify (elab left) (elab right) ;
              i1
          | And | Or ->
              unify i1 (elab left) ;
              unify i1 (elab right) ;
              i1
          | Combine | Remove | Cons -> noimpl "sequence operators"
          | Pipeline -> noimpl "pipeline operator"
          | Append | Prepend | Erase ->
              noimpl "sequence-with-element operators"
          | Dot -> noimpl "dot operator"
          | AddAsign | SubstractAsign | Asign -> noimpl "assignment operators"
          )
        | PrefixOp (op, operand) -> (
            let operandType = elab operand in
            match op with
            | Positive | Negative -> unify i64 operandType ; i64
            | Not -> unify i1 operandType ; i1
            | BitwiseNot -> unify u64 operandType ; u64
            | Deref | Ref -> noimpl "ref/deref operators"
            | Raise -> noimpl "raise"
            | Lazy -> noimpl "lazy"
            | PrefixIncrement | PrefixDecrement ->
                noimpl "increment/decrement operators" )
        | PostfixOp (operand, op) -> noimpl "postfix operators"
        | Tuple values -> TTuple (List.map elab values)
        | ListLiteral values ->
            let types = List.map elab values in
            let elemType = new_var level in
            List.iter (unify elemType) types ;
            TApply ([elemType], listType)
        | ArrayLiteral values ->
            let types = List.map elab values in
            let elemType = new_var level in
            List.iter (unify elemType) types ;
            TApply ([elemType], arrayType)
        | IfThenElse (cond, body, Some fallback) ->
            elab cond |> unify i1 ;
            let bodyType = elab body in
            let fallbackType = elab fallback in
            unify bodyType fallbackType ;
            bodyType
        | IfThenElse (cond, body, None) ->
            elab cond |> unify i1 ;
            elab body |> ignore ;
            unit
        | Seq exps ->
            let rec aux = function
              | [last] -> elab last
              | exp :: t ->
                  elab exp |> ignore ;
                  aux t
              | [] -> unit
            in
            aux exps
        | Match (value, mrules) ->
            let valueType = elab_exp env level value in
            noimpl "pattern matching"
        | Handle _ -> noimpl "effect handler"
        | RecordLiteral _ | RecordSelection _ | RecordRestrictionLiteral _ ->
            noimpl "record"
        | PolymorphicVariantConstruction _ -> noimpl "polymorphic variant"
      in
      set_ty data ret ; ret

let elabModulePart (path : string list) (env : env) : module_part -> env =
  function
  | data, part -> (
      (* prepare *)
      let env = empty in
      let level = 0 in
      match part with
      | InterfaceInModule _ -> noimpl "interface"
      | LetDef (name, exp) ->
          let exp_type = elab_exp env (level + 1) exp in
          let generalized_type = generalize level exp_type in
          set_ty data generalized_type ;
          extend env (LongId (List.append path [name])) generalized_type
      | LetRecDef lets ->
          let rec allocate_type_vars env = function
            | (part_data, LetRecDefPart (name, value)) :: t ->
                let ty = new_var (level + 1) in
                set_ty part_data ty ;
                allocate_type_vars (extend env (LongId [name]) ty) t
            | [] -> env
          in
          let env = allocate_type_vars env lets in
          let rec elabParts env = function
            | (part_data, LetRecDefPart (name, exp)) :: t ->
                let exp_type = elab_exp env (level + 1) exp in
                elabParts (extend env (LongId [name]) exp_type) t
            | [] -> env
          in
          elabParts env lets )

let rec elabModule' path env module_id name = function
  | h :: t ->
      let env = elabModulePart path env h in
      elabModule' path env module_id name t
  | [] -> env

let elabModule module_id name module_parts =
  elabModule' [name] empty module_id name module_parts

let elabDecl = function
  | data, decl -> (
    match decl with
    | InterfaceDecl _ -> noimpl "interface"
    | FunctorDecl _ -> noimpl "functor"
    | ModuleDecl (Some name, false, (module_parts, [])) ->
        elabModule data name module_parts
    | ModuleDecl (Some _, false, (_, _ :: _)) -> noimpl "interface"
    | ModuleDecl (_, true, _) -> noimpl "given module"
    | ModuleDecl (None, _, _) -> noimpl "anonymous module" )
