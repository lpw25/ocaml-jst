(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Substitutions *)

open Misc
open Path
open Types
open Btype

open Local_store

type type_replacement =
  | Path of Path.t
  | Type_function of { params : type_expr list; body : type_expr }

type t =
  { types: type_replacement Path.Map.t;
    modules: Path.t Path.Map.t;
    modtypes: module_type Ident.Map.t;
    for_saving: bool;
  }

let identity =
  { types = Path.Map.empty;
    modules = Path.Map.empty;
    modtypes = Ident.Map.empty;
    for_saving = false;
  }

let add_type_path id p s = { s with types = Path.Map.add id (Path p) s.types }
let add_type id p s = add_type_path (Pident id) p s

let add_type_function id ~params ~body s =
  { s with types = Path.Map.add id (Type_function { params; body }) s.types }

let add_module_path id p s = { s with modules = Path.Map.add id p s.modules }
let add_module id p s = add_module_path (Pident id) p s

let add_modtype id ty s = { s with modtypes = Ident.Map.add id ty s.modtypes }

let for_saving s = { s with for_saving = true }

let loc s x =
  if s.for_saving && not !Clflags.keep_locs then Location.none else x

let remove_loc =
  let open Ast_mapper in
  {default_mapper with location = (fun _this _loc -> Location.none)}

let is_not_doc = function
  | {Parsetree.attr_name = {Location.txt = "ocaml.doc"}; _} -> false
  | {Parsetree.attr_name = {Location.txt = "ocaml.text"}; _} -> false
  | {Parsetree.attr_name = {Location.txt = "doc"}; _} -> false
  | {Parsetree.attr_name = {Location.txt = "text"}; _} -> false
  | _ -> true

let attrs s x =
  let x =
    if s.for_saving && not !Clflags.keep_docs then
      List.filter is_not_doc x
    else x
  in
    if s.for_saving && not !Clflags.keep_locs
    then remove_loc.Ast_mapper.attributes remove_loc x
    else x

let rec module_path s path =
  try Path.Map.find path s.modules
  with Not_found ->
    match path with
    | Pident _ -> path
    | Pdot(p, n) ->
       Pdot(module_path s p, n)
    | Papply(p1, p2) ->
       Papply(module_path s p1, module_path s p2)

let modtype_path s = function
    Pident id as p ->
      begin try
        match Ident.Map.find id s.modtypes with
          | Mty_ident p -> p
          | _ -> fatal_error "Subst.modtype_path"
      with Not_found -> p end
  | Pdot(p, n) ->
      Pdot(module_path s p, n)
  | Papply _ ->
      fatal_error "Subst.modtype_path"

let type_path s path =
  match Path.Map.find path s.types with
  | Path p -> p
  | Type_function _ -> assert false
  | exception Not_found ->
     match path with
     | Pident _ -> path
     | Pdot(p, n) ->
        Pdot(module_path s p, n)
     | Papply _ ->
        fatal_error "Subst.type_path"

let type_path s p =
  match Path.constructor_typath p with
  | Regular p -> type_path s p
  | Cstr (ty_path, cstr) -> Pdot(type_path s ty_path, cstr)
  | LocalExt _ -> type_path s p
  | Ext (p, cstr) -> Pdot(module_path s p, cstr)

let to_subst_by_type_function s p =
  match Path.Map.find p s.types with
  | Path _ -> false
  | Type_function _ -> true
  | exception Not_found -> false

(* Special type ids for saved signatures *)

let new_id = s_ref (-1)
let reset_for_saving () = new_id := -1

let newpersty desc =
  decr new_id;
  { desc; level = generic_level; scope = Btype.lowest_level; id = !new_id }

(* ensure that all occurrences of 'Tvar None' are physically shared *)
let tvar_none = Tvar None
let tunivar_none = Tunivar None
let norm = function
  | Tvar None -> tvar_none
  | Tunivar None -> tunivar_none
  | d -> d

let ctype_apply_env_empty = ref (fun _ -> assert false)

(* Similar to [Ctype.nondep_type_rec]. *)
let rec typexp copy_scope s ty =
  let ty = repr ty in
  match ty.desc with
    Tvar _ | Tunivar _ as desc ->
      if s.for_saving || ty.id < 0 then
        let ty' =
          if s.for_saving then newpersty (norm desc)
          else newty2 ty.level desc
        in
        For_copy.save_desc copy_scope ty desc;
        ty.desc <- Tsubst ty';
        ty'
      else ty
  | Tsubst ty ->
      ty
  | Tfield (m, k, _t1, _t2) when not s.for_saving && m = dummy_method
      && field_kind_repr k <> Fabsent && (repr ty).level < generic_level ->
      (* do not copy the type of self when it is not generalized *)
      ty
(* cannot do it, since it would omit substitution
  | Tvariant row when not (static_row row) ->
      ty
*)
  | _ ->
    let desc = ty.desc in
    For_copy.save_desc copy_scope ty desc;
    let tm = row_of_type ty in
    let has_fixed_row =
      not (is_Tconstr ty) && is_constr_row ~allow_ident:false tm in
    (* Make a stub *)
    let ty' = if s.for_saving then newpersty (Tvar None) else newgenvar () in
    ty'.scope <- ty.scope;
    ty.desc <- Tsubst ty';
    ty'.desc <-
      begin if has_fixed_row then
        match tm.desc with (* PR#7348 *)
          Tconstr (Pdot(m,i), tl, _abbrev) ->
            let i' = String.sub i 0 (String.length i - 4) in
            Tconstr(type_path s (Pdot(m,i')), tl, ref Mnil)
        | _ -> assert false
      else match desc with
      | Tconstr (p, args, _abbrev) ->
         let args = List.map (typexp copy_scope s) args in
         begin match Path.Map.find p s.types with
         | exception Not_found -> Tconstr(type_path s p, args, ref Mnil)
         | Path _ -> Tconstr(type_path s p, args, ref Mnil)
         | Type_function { params; body } ->
            Tlink (!ctype_apply_env_empty params body args)
         end
      | Tpackage(p, n, tl) ->
          Tpackage(modtype_path s p, n, List.map (typexp copy_scope s) tl)
      | Tobject (t1, name) ->
          let t1' = typexp copy_scope s t1 in
          let name' =
            match !name with
            | None -> None
            | Some (p, tl) ->
                if to_subst_by_type_function s p
                then None
                else Some (type_path s p, List.map (typexp copy_scope s) tl)
          in
          Tobject (t1', ref name')
      | Tvariant row ->
          let row = row_repr row in
          let more = repr row.row_more in
          (* We must substitute in a subtle way *)
          (* Tsubst takes a tuple containing the row var and the variant *)
          begin match more.desc with
            Tsubst {desc = Ttuple [_;ty2]} ->
              (* This variant type has been already copied *)
              ty.desc <- Tsubst ty2; (* avoid Tlink in the new type *)
              Tlink ty2
          | _ ->
              let dup =
                s.for_saving || more.level = generic_level || static_row row ||
                match more.desc with Tconstr _ -> true | _ -> false in
              (* Various cases for the row variable *)
              let more' =
                match more.desc with
                  Tsubst ty -> ty
                | Tconstr _ | Tnil -> typexp copy_scope s more
                | Tunivar _ | Tvar _ ->
                    For_copy.save_desc copy_scope more more.desc;
                    if s.for_saving then newpersty (norm more.desc) else
                    if dup && is_Tvar more then newgenty more.desc else more
                | _ -> assert false
              in
              (* Register new type first for recursion *)
              more.desc <- Tsubst(newgenty(Ttuple[more';ty']));
              (* Return a new copy *)
              let row =
                copy_row (typexp copy_scope s) true row (not dup) more' in
              match row.row_name with
              | Some (p, tl) ->
                 Tvariant {row with row_name =
                                      if to_subst_by_type_function s p
                                      then None
                                      else Some (type_path s p, tl)}
              | None ->
                  Tvariant row
          end
      | Tfield(_label, kind, _t1, t2) when field_kind_repr kind = Fabsent ->
          Tlink (typexp copy_scope s t2)
      | _ -> copy_type_desc (typexp copy_scope s) desc
      end;
    ty'

let effect_context copy_scope s eff =
  copy_effect_context (typexp copy_scope s) eff

(*
   Always make a copy of the type. If this is not done, type levels
   might not be correct.
*)
let type_expr s ty =
  For_copy.with_scope (fun copy_scope -> typexp copy_scope s ty)

let type_scheme s sch eff =
  For_copy.with_scope
    (fun copy_scope ->
      let sch = typexp copy_scope s sch in
      let eff = effect_context copy_scope s eff in
      sch, eff)

let delayed_effect_context copy_scope s = function
  | Tuple(effs, eff) ->
      let effs = List.map (effect_context copy_scope s) effs in
      let eff = effect_context copy_scope s eff in
      Tuple(effs, eff)
  | Single eff -> Single (effect_context copy_scope s eff)

let expr_effect_context copy_scope s { current; delayed } =
  let current = effect_context copy_scope s current in
  let delayed = delayed_effect_context copy_scope s delayed in
  { current; delayed }

let expression_type s ty eff =
  For_copy.with_scope
    (fun copy_scope ->
      let ty = typexp copy_scope s ty in
      let eff = expr_effect_context copy_scope s eff in
      ty, eff)

let label_declaration copy_scope s l =
  {
    ld_id = l.ld_id;
    ld_mutable = l.ld_mutable;
    ld_global = l.ld_global;
    ld_type = typexp copy_scope s l.ld_type;
    ld_loc = loc s l.ld_loc;
    ld_attributes = attrs s l.ld_attributes;
    ld_uid = l.ld_uid;
  }

let constructor_arguments copy_scope s = function
  | Cstr_tuple l ->
      Cstr_tuple (List.map (typexp copy_scope s) l)
  | Cstr_record l ->
      Cstr_record (List.map (label_declaration copy_scope s) l)

let constructor_declaration copy_scope s c =
  {
    cd_id = c.cd_id;
    cd_args = constructor_arguments copy_scope s c.cd_args;
    cd_res = Option.map (typexp copy_scope s) c.cd_res;
    cd_loc = loc s c.cd_loc;
    cd_attributes = attrs s c.cd_attributes;
    cd_uid = c.cd_uid;
  }

let operation_declaration copy_scope s od =
  {
    od_id = od.od_id;
    od_args = List.map (typexp copy_scope s) od.od_args;
    od_res = Option.map (typexp copy_scope s) od.od_res;
    od_loc = loc s od.od_loc;
    od_attributes = attrs s od.od_attributes;
    od_uid = od.od_uid;
  }

let type_declaration' copy_scope s decl =
  { type_params = List.map (typexp copy_scope s) decl.type_params;
    type_arity = decl.type_arity;
    type_kind =
      begin match decl.type_kind with
        Type_abstract -> Type_abstract
      | Type_variant cstrs ->
          Type_variant (List.map (constructor_declaration copy_scope s) cstrs)
      | Type_record(lbls, rep) ->
          Type_record (List.map (label_declaration copy_scope s) lbls, rep)
      | Type_open -> Type_open
      | Type_effect ops ->
          Type_effect (List.map (operation_declaration copy_scope s) ops)
      end;
    type_manifest =
      begin
        match decl.type_manifest with
          None -> None
        | Some ty -> Some(typexp copy_scope s ty)
      end;
    type_private = decl.type_private;
    type_variance = decl.type_variance;
    type_separability = decl.type_separability;
    type_is_newtype = false;
    type_expansion_scope = Btype.lowest_level;
    type_loc = loc s decl.type_loc;
    type_attributes = attrs s decl.type_attributes;
    type_immediate = decl.type_immediate;
    type_unboxed = decl.type_unboxed;
    type_uid = decl.type_uid;
  }

let type_declaration s decl =
  For_copy.with_scope (fun copy_scope -> type_declaration' copy_scope s decl)

let class_signature copy_scope s sign =
  { csig_self = typexp copy_scope s sign.csig_self;
    csig_vars =
      Vars.map
        (function (m, v, t) -> (m, v, typexp copy_scope s t)) sign.csig_vars;
    csig_concr = sign.csig_concr;
    csig_inher =
      List.map
        (fun (p, tl) -> (type_path s p, List.map (typexp copy_scope s) tl))
        sign.csig_inher;
  }

let rec class_type copy_scope s = function
  | Cty_constr (p, tyl, cty) ->
      let p' = type_path s p in
      let tyl' = List.map (typexp copy_scope s) tyl in
      let cty' = class_type copy_scope s cty in
      Cty_constr (p', tyl', cty')
  | Cty_signature sign ->
      Cty_signature (class_signature copy_scope s sign)
  | Cty_arrow (l, ty, cty) ->
      Cty_arrow (l, typexp copy_scope s ty, class_type copy_scope s cty)

let class_declaration' copy_scope s decl =
  { cty_params = List.map (typexp copy_scope s) decl.cty_params;
    cty_variance = decl.cty_variance;
    cty_type = class_type copy_scope s decl.cty_type;
    cty_path = type_path s decl.cty_path;
    cty_new =
      begin match decl.cty_new with
      | None    -> None
      | Some ty -> Some (typexp copy_scope s ty)
      end;
    cty_loc = loc s decl.cty_loc;
    cty_attributes = attrs s decl.cty_attributes;
    cty_uid = decl.cty_uid;
  }

let class_declaration s decl =
  For_copy.with_scope (fun copy_scope -> class_declaration' copy_scope s decl)

let cltype_declaration' copy_scope s decl =
  { clty_params = List.map (typexp copy_scope s) decl.clty_params;
    clty_variance = decl.clty_variance;
    clty_type = class_type copy_scope s decl.clty_type;
    clty_path = type_path s decl.clty_path;
    clty_loc = loc s decl.clty_loc;
    clty_attributes = attrs s decl.clty_attributes;
    clty_uid = decl.clty_uid;
  }

let cltype_declaration s decl =
  For_copy.with_scope (fun copy_scope -> cltype_declaration' copy_scope s decl)

let class_type s cty =
  For_copy.with_scope (fun copy_scope -> class_type copy_scope s cty)

let value_description' copy_scope s descr =
  { val_type = typexp copy_scope s descr.val_type;
    val_eff = effect_context copy_scope s descr.val_eff;
    val_kind = descr.val_kind;
    val_loc = loc s descr.val_loc;
    val_attributes = attrs s descr.val_attributes;
    val_uid = descr.val_uid;
   }

let value_description s descr =
  For_copy.with_scope (fun copy_scope -> value_description' copy_scope s descr)

let extension_constructor' copy_scope s ext =
  { ext_type_path = type_path s ext.ext_type_path;
    ext_type_params = List.map (typexp copy_scope s) ext.ext_type_params;
    ext_args = constructor_arguments copy_scope s ext.ext_args;
    ext_ret_type = Option.map (typexp copy_scope s) ext.ext_ret_type;
    ext_private = ext.ext_private;
    ext_attributes = attrs s ext.ext_attributes;
    ext_loc = if s.for_saving then Location.none else ext.ext_loc;
    ext_uid = ext.ext_uid;
  }

let extension_constructor s ext =
  For_copy.with_scope
    (fun copy_scope -> extension_constructor' copy_scope s ext)


(* For every binding k |-> d of m1, add k |-> f d to m2
   and return resulting merged map. *)

let merge_tbls f m1 m2 =
  Ident.Map.fold (fun k d accu -> Ident.Map.add k (f d) accu) m1 m2

let merge_path_maps f m1 m2 =
  Path.Map.fold (fun k d accu -> Path.Map.add k (f d) accu) m1 m2

let type_replacement s = function
  | Path p -> Path (type_path s p)
  | Type_function { params; body } ->
    For_copy.with_scope (fun copy_scope ->
     let params = List.map (typexp copy_scope s) params in
     let body = typexp copy_scope s body in
     Type_function { params; body })

type scoping =
  | Keep
  | Make_local
  | Rescope of int

module Lazy_types = struct

  type module_decl =
    {
      mdl_type: modtype;
      mdl_attributes: Parsetree.attributes;
      mdl_loc: Location.t;
      mdl_uid: Uid.t;
    }

  and modtype =
    | MtyL_ident of Path.t
    | MtyL_signature of signature
    | MtyL_functor of functor_parameter * modtype
    | MtyL_alias of Path.t

  and modtype_declaration =
    {
      mtdl_type: modtype option;
      mtdl_attributes: Parsetree.attributes;
      mtdl_loc: Location.t;
      mtdl_uid: Uid.t;
    }

  and signature' =
    | S_eager of Types.signature
    | S_lazy of signature_item list

  and signature =
    (scoping * t * signature', signature') EnvLazy.t

  and signature_item =
      SigL_value of Ident.t * value_description * visibility
    | SigL_type of Ident.t * type_declaration * rec_status * visibility
    | SigL_typext of Ident.t * extension_constructor * ext_status * visibility
    | SigL_module of
        Ident.t * module_presence * module_decl * rec_status * visibility
    | SigL_modtype of Ident.t * modtype_declaration * visibility
    | SigL_class of Ident.t * class_declaration * rec_status * visibility
    | SigL_class_type of Ident.t * class_type_declaration *
                           rec_status * visibility

  and functor_parameter =
    | Unit
    | Named of Ident.t option * modtype

end
open Lazy_types

let rename_bound_idents scoping s sg =
  let rename =
    let open Ident in
    match scoping with
    | Keep -> (fun id -> create_scoped ~scope:(scope id) (name id))
    | Make_local -> Ident.rename
    | Rescope scope -> (fun id -> create_scoped ~scope (name id))
  in
  let rec rename_bound_idents s sg = function
    | [] -> sg, s
    | SigL_type(id, td, rs, vis) :: rest ->
        let id' = rename id in
        rename_bound_idents
          (add_type id (Pident id') s)
          (SigL_type(id', td, rs, vis) :: sg)
          rest
    | SigL_module(id, pres, md, rs, vis) :: rest ->
        let id' = rename id in
        rename_bound_idents
          (add_module id (Pident id') s)
          (SigL_module (id', pres, md, rs, vis) :: sg)
          rest
    | SigL_modtype(id, mtd, vis) :: rest ->
        let id' = rename id in
        rename_bound_idents
          (add_modtype id (Mty_ident(Pident id')) s)
          (SigL_modtype(id', mtd, vis) :: sg)
          rest
    | SigL_class(id, cd, rs, vis) :: rest ->
        (* cheat and pretend they are types cf. PR#6650 *)
        let id' = rename id in
        rename_bound_idents
          (add_type id (Pident id') s)
          (SigL_class(id', cd, rs, vis) :: sg)
          rest
    | SigL_class_type(id, ctd, rs, vis) :: rest ->
        (* cheat and pretend they are types cf. PR#6650 *)
        let id' = rename id in
        rename_bound_idents
          (add_type id (Pident id') s)
          (SigL_class_type(id', ctd, rs, vis) :: sg)
          rest
    | SigL_value(id, vd, vis) :: rest ->
        (* scope doesn't matter for value identifiers. *)
        let id' = Ident.rename id in
        rename_bound_idents s (SigL_value(id', vd, vis) :: sg) rest
    | SigL_typext(id, ec, es, vis) :: rest ->
        let id' = rename id in
        rename_bound_idents s (SigL_typext(id',ec,es,vis) :: sg) rest
  in
  rename_bound_idents s [] sg

let rec lazy_module_decl md =
  { mdl_type = lazy_modtype md.md_type;
    mdl_attributes = md.md_attributes;
    mdl_loc = md.md_loc;
    mdl_uid = md.md_uid }

and subst_lazy_module_decl scoping s md =
  let mdl_type = subst_lazy_modtype scoping s md.mdl_type in
  { mdl_type;
    mdl_attributes = attrs s md.mdl_attributes;
    mdl_loc = loc s md.mdl_loc;
    mdl_uid = md.mdl_uid }

and force_module_decl md =
  let md_type = force_modtype md.mdl_type in
  { md_type;
    md_attributes = md.mdl_attributes;
    md_loc = md.mdl_loc;
    md_uid = md.mdl_uid }

and lazy_modtype = function
  | Mty_ident p -> MtyL_ident p
  | Mty_signature sg ->
     MtyL_signature (EnvLazy.create_forced (S_eager sg))
  | Mty_functor (Unit, mty) -> MtyL_functor (Unit, lazy_modtype mty)
  | Mty_functor (Named (id, arg), res) ->
     MtyL_functor (Named (id, lazy_modtype arg), lazy_modtype res)
  | Mty_alias p -> MtyL_alias p

and subst_lazy_modtype scoping s = function
  | MtyL_ident p ->
      begin match p with
        Pident id ->
          begin match Ident.Map.find id s.modtypes with
          | mty -> lazy_modtype mty
          | exception Not_found -> MtyL_ident p end
      | Pdot(p, n) ->
          MtyL_ident(Pdot(module_path s p, n))
      | Papply _ ->
          fatal_error "Subst.modtype"
      end
  | MtyL_signature sg ->
      MtyL_signature(subst_lazy_signature scoping s sg)
  | MtyL_functor(Unit, res) ->
      MtyL_functor(Unit, subst_lazy_modtype scoping s res)
  | MtyL_functor(Named (None, arg), res) ->
      MtyL_functor(Named (None, (subst_lazy_modtype scoping s) arg),
                   subst_lazy_modtype scoping s res)
  | MtyL_functor(Named (Some id, arg), res) ->
      let id' = Ident.rename id in
      MtyL_functor(Named (Some id', (subst_lazy_modtype scoping s) arg),
                  subst_lazy_modtype scoping (add_module id (Pident id') s) res)
  | MtyL_alias p ->
      MtyL_alias (module_path s p)

and force_modtype = function
  | MtyL_ident p -> Mty_ident p
  | MtyL_signature sg -> Mty_signature (force_signature sg)
  | MtyL_functor (param, res) ->
     let param : Types.functor_parameter =
       match param with
       | Unit -> Unit
       | Named (id, mty) -> Named (id, force_modtype mty) in
     Mty_functor (param, force_modtype res)
  | MtyL_alias p -> Mty_alias p

and lazy_modtype_decl mtd =
  let mtdl_type = Option.map lazy_modtype mtd.mtd_type in
  { mtdl_type;
    mtdl_attributes = mtd.mtd_attributes;
    mtdl_loc = mtd.mtd_loc;
    mtdl_uid = mtd.mtd_uid }

and subst_lazy_modtype_decl scoping s mtd =
  { mtdl_type = Option.map (subst_lazy_modtype scoping s) mtd.mtdl_type;
    mtdl_attributes = attrs s mtd.mtdl_attributes;
    mtdl_loc = loc s mtd.mtdl_loc;
    mtdl_uid = mtd.mtdl_uid }

and force_modtype_decl mtd =
  let mtd_type = Option.map force_modtype mtd.mtdl_type in
  { mtd_type;
    mtd_attributes = mtd.mtdl_attributes;
    mtd_loc = mtd.mtdl_loc;
    mtd_uid = mtd.mtdl_uid }

and subst_lazy_signature scoping s sg =
  match EnvLazy.get_contents sg with
  | Left (scoping', s', sg) ->
     let scoping =
       match scoping', scoping with
       | sc, Keep -> sc
       | _, (Make_local|Rescope _) -> scoping
     in
     let s = compose s' s in
     EnvLazy.create (scoping, s, sg)
  | Right sg ->
     EnvLazy.create (scoping, s, sg)

and force_signature sg =
  List.map force_signature_item (force_signature_once sg)

and force_signature_once sg =
  lazy_signature' (EnvLazy.force force_signature_once' sg)

and lazy_signature' = function
  | S_lazy sg -> sg
  | S_eager sg -> List.map lazy_signature_item sg

and force_signature_once' (scoping, s, sg) =
  let sg = lazy_signature' sg in
  (* Components of signature may be mutually recursive (e.g. type declarations
     or class and type declarations), so first build global renaming
     substitution... *)
  let (sg', s') = rename_bound_idents scoping s sg in
  (* ... then apply it to each signature component in turn *)
  For_copy.with_scope (fun copy_scope ->
    S_lazy (List.rev_map (subst_lazy_signature_item' copy_scope scoping s') sg')
  )

and lazy_signature_item = function
  | Sig_value(id, d, vis) ->
     SigL_value(id, d, vis)
  | Sig_type(id, d, rs, vis) ->
     SigL_type(id, d, rs, vis)
  | Sig_typext(id, ext, es, vis) ->
     SigL_typext(id, ext, es, vis)
  | Sig_module(id, res, d, rs, vis) ->
     SigL_module(id, res, lazy_module_decl d, rs, vis)
  | Sig_modtype(id, d, vis) ->
     SigL_modtype(id, lazy_modtype_decl d, vis)
  | Sig_class(id, d, rs, vis) ->
     SigL_class(id, d, rs, vis)
  | Sig_class_type(id, d, rs, vis) ->
     SigL_class_type(id, d, rs, vis)

and subst_lazy_signature_item' copy_scope scoping s comp =
  match comp with
    SigL_value(id, d, vis) ->
      SigL_value(id, value_description' copy_scope s d, vis)
  | SigL_type(id, d, rs, vis) ->
      SigL_type(id, type_declaration' copy_scope s d, rs, vis)
  | SigL_typext(id, ext, es, vis) ->
      SigL_typext(id, extension_constructor' copy_scope s ext, es, vis)
  | SigL_module(id, pres, d, rs, vis) ->
      SigL_module(id, pres, subst_lazy_module_decl scoping s d, rs, vis)
  | SigL_modtype(id, d, vis) ->
      SigL_modtype(id, subst_lazy_modtype_decl scoping s d, vis)
  | SigL_class(id, d, rs, vis) ->
      SigL_class(id, class_declaration' copy_scope s d, rs, vis)
  | SigL_class_type(id, d, rs, vis) ->
      SigL_class_type(id, cltype_declaration' copy_scope s d, rs, vis)

and force_signature_item = function
  | SigL_value(id, vd, vis) -> Sig_value(id, vd, vis)
  | SigL_type(id, d, rs, vis) -> Sig_type(id, d, rs, vis)
  | SigL_typext(id, ext, es, vis) -> Sig_typext(id, ext, es, vis)
  | SigL_module(id, pres, d, rs, vis) ->
     Sig_module(id, pres, force_module_decl d, rs, vis)
  | SigL_modtype(id, d, vis) ->
     Sig_modtype (id, force_modtype_decl d, vis)
  | SigL_class(id, d, rs, vis) -> Sig_class(id, d, rs, vis)
  | SigL_class_type(id, d, rs, vis) -> Sig_class_type(id, d, rs, vis)

and modtype scoping s t =
  t |> lazy_modtype |> subst_lazy_modtype scoping s |> force_modtype

(* Composition of substitutions:
     apply (compose s1 s2) x = apply s2 (apply s1 x) *)

and compose s1 s2 =
  if s1 == identity then s2 else
  if s2 == identity then s1 else
  { types = merge_path_maps (type_replacement s2) s1.types s2.types;
    modules = merge_path_maps (module_path s2) s1.modules s2.modules;
    modtypes = merge_tbls (modtype Keep s2) s1.modtypes s2.modtypes;
    for_saving = s1.for_saving || s2.for_saving;
  }


let subst_lazy_signature_item scoping s comp =
  For_copy.with_scope
    (fun copy_scope -> subst_lazy_signature_item' copy_scope scoping s comp)

module Lazy = struct
  include Lazy_types

  let of_module_decl = lazy_module_decl
  let of_modtype = lazy_modtype
  let of_modtype_decl = lazy_modtype_decl
  let of_signature sg = EnvLazy.create_forced (S_eager sg)
  let of_signature_items sg = EnvLazy.create_forced (S_lazy sg)
  let of_signature_item = lazy_signature_item

  let module_decl = subst_lazy_module_decl
  let modtype = subst_lazy_modtype
  let modtype_decl = subst_lazy_modtype_decl
  let signature = subst_lazy_signature
  let signature_item = subst_lazy_signature_item

  let force_module_decl = force_module_decl
  let force_modtype = force_modtype
  let force_modtype_decl = force_modtype_decl
  let force_signature = force_signature
  let force_signature_once = force_signature_once
  let force_signature_item = force_signature_item
end

let signature sc s sg =
  Lazy.(sg |> of_signature |> signature sc s |> force_signature)

let signature_item sc s comp =
  Lazy.(comp|> of_signature_item |> signature_item sc s |> force_signature_item)

let modtype_declaration sc s decl =
  Lazy.(decl |> of_modtype_decl |> modtype_decl sc s |> force_modtype_decl)

let module_declaration scoping s decl =
  Lazy.(decl |> of_module_decl |> module_decl scoping s |> force_module_decl)
