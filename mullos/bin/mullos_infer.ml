(* Copyright (C) 2019 Takezoe,Tomoaki <tomoaki3478@res.ac>
 *
 * SPDX-Identifier: LGPL-3.0-or-later
 *)
open Mullos_syntax
open Mullos_syntax.Type
open Mullos_aux

exception Error of string

let error msg = raise (Error msg)

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

type env = type_exp IdMap.t

let empty = IdMap.empty

let extend env name ty = IdMap.add name ty env

let lookup env name = IdMap.find name env

let occurs_check_adjust_levels tvar_id tvar_level ty =
  let rec f = function
    | TVar {contents= Link ty} -> f ty
    | TVar {contents= Generic _} -> assert false
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
        assert false
        (* There is only a single instance of a particular type variable. *)
    | TVar ({contents= Unbound (id, level)} as tvar), ty
     |ty, TVar ({contents= Unbound (id, level)} as tvar) ->
        occurs_check_adjust_levels id level ty ;
        tvar := Link ty
    | TLazy _, _ -> assert false
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

let rec elabPat (env : env) (level : level) = function
  | id, pat' -> (
    match pat' with
    | PIdent _ -> new_var level
    | PUnit -> TLongId (LongId ["unit"])
    | PCapture (_, pat) -> elabPat env level pat
    | PCtor (ctor, pat) -> assert false
    | PTuple pats -> TTuple (List.map (fun pat -> elabPat env level pat) pats)
    | PWildcard -> new_var level
    | PText _ -> TLongId (LongId ["text"])
    | PNumber _ -> TLongId (LongId ["i32"])
    | PBool _ -> TLongId (LongId ["bool"])
    | PLazy pat -> TLazy (elabPat env level pat)
    | POr (pat1, pat2) ->
        unify (elabPat env level pat1) (elabPat env level pat2) ;
        assert false
    | PListLiteral pats -> assert false
    | PArrayLiteral pats -> assert false
    | PPolymorphicVariant (label, pat) -> assert false )

let rec infer env level = function
  | id, exp' -> (
    match exp' with
    | Ident name -> (
      try instantiate level (lookup env name) with Not_found ->
        error "variable not found" )
    | Lambda (param, body) ->
        let param_ty = new_var level in
        let env = extend env (LongId [param]) param_ty in
        let return_ty = infer env level body in
        TArrow (param_ty, return_ty)
    | Let (name, params, value, body) ->
        let value_ty = infer env (level + 1) value in
        let generalized_ty = generalize level value_ty in
        infer (extend env (LongId [name]) generalized_ty) level body
    | LetRec (lets, body) ->
        (* TODO *)
        let rec aux env = function
          | (name, params, value) :: t ->
              aux (extend env (LongId [name]) (new_var (level + 1))) t
          | [] -> env
        in
        let env = aux env lets in
        assert false
    | Apply (fn, arg) -> (
      match infer env level fn with
      | TArrow (param, ret) ->
          let arg = infer env level arg in
          unify param arg ; ret
      | _ -> failwith "type error: expected function" )
    | _ -> assert false )
