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

(** {0 Representation of types and declarations} *)

(** [Types] defines the representation of types and declarations (that is, the
    content of module signatures).

    CMI files are made of marshalled types.
*)

(** Asttypes exposes basic definitions shared both by Parsetree and Types. *)
open Asttypes

(** Type expressions for the core language.

    The [type_desc] variant defines all the possible type expressions one can
    find in OCaml. [type_expr] wraps this with some annotations.

    The [level] field tracks the level of polymorphism associated to a type,
    guiding the generalization algorithm.
    Put shortly, when referring to a type in a given environment, both the type
    and the environment have a level. If the type has an higher level, then it
    can be considered fully polymorphic (type variables will be printed as
    ['a]), otherwise it'll be weakly polymorphic, or non generalized (type
    variables printed as ['_a]).
    See [http://okmij.org/ftp/ML/generalization.html] for more information.

    Note about [type_declaration]: one should not make the confusion between
    [type_expr] and [type_declaration].

    [type_declaration] refers specifically to the [type] construct in OCaml
    language, where you create and name a new type or type alias.

    [type_expr] is used when you refer to existing types, e.g. when annotating
    the expected type of a value.

    Also, as the type system of OCaml is generative, a [type_declaration] can
    have the side-effect of introducing a new type constructor, different from
    all other known types.
    Whereas [type_expr] is a pure construct which allows referring to existing
    types.

    Note on mutability: TBD.
 *)
type type_expr =
  { mutable desc: type_desc;
    mutable level: int;
    mutable scope: int;
    id: int }

and type_desc =
  | Tvar of string option
  (** [Tvar (Some "a")] ==> ['a] or ['_a]
      [Tvar None]       ==> [_] *)

  | Tarrow of arrow_desc * type_expr * type_expr * commutable
  (** [Tarrow (Nolabel,      e1, e2, c)] ==> [e1    -> e2]
      [Tarrow (Labelled "l", e1, e2, c)] ==> [l:e1  -> e2]
      [Tarrow (Optional "l", e1, e2, c)] ==> [?l:e1 -> e2]

      See [commutable] for the last argument. The argument
      type must be a [Tpoly] node *)

  | Ttuple of type_expr list
  (** [Ttuple [t1;...;tn]] ==> [(t1 * ... * tn)] *)

  | Tconstr of Path.t * type_expr list * abbrev_memo ref
  (** [Tconstr (`A.B.t', [t1;...;tn], _)] ==> [(t1,...,tn) A.B.t]
      The last parameter keep tracks of known expansions, see [abbrev_memo]. *)

  | Tobject of type_expr * (Path.t * type_expr list) option ref
  (** [Tobject (`f1:t1;...;fn: tn', `None')] ==> [< f1: t1; ...; fn: tn >]
      f1, fn are represented as a linked list of types using Tfield and Tnil
      constructors.

      [Tobject (_, `Some (`A.ct', [t1;...;tn]')] ==> [(t1, ..., tn) A.ct].
      where A.ct is the type of some class.

      There are also special cases for so-called "class-types", cf. [Typeclass]
      and [Ctype.set_object_name]:

        [Tobject (Tfield(_,_,...(Tfield(_,_,rv)...),
                         Some(`A.#ct`, [rv;t1;...;tn])]
             ==> [(t1, ..., tn) #A.ct]
        [Tobject (_, Some(`A.#ct`, [Tnil;t1;...;tn])] ==> [(t1, ..., tn) A.ct]

      where [rv] is the hidden row variable.
  *)

  | Tfield of string * field_kind * type_expr * type_expr
  (** [Tfield ("foo", Fpresent, t, ts)] ==> [<...; foo : t; ts>] *)

  | Tnil
  (** [Tnil] ==> [<...; >] *)

  | Tlink of type_expr
  (** Indirection used by unification engine. *)

  | Tsubst of type_expr         (* for copying *)
  (** [Tsubst] is used temporarily to store information in low-level
      functions manipulating representation of types, such as
      instantiation or copy.
      This constructor should not appear outside of these cases. *)

  | Tvariant of row_desc
  (** Representation of polymorphic variants, see [row_desc]. *)

  | Tunivar of string option
  (** Occurrence of a type variable introduced by a
      forall quantifier / [Tpoly]. *)

  | Tpoly of type_expr * type_expr list
  (** [Tpoly (ty,tyl)] ==> ['a1... 'an. ty],
      where 'a1 ... 'an are names given to types in tyl
      and occurrences of those types in ty. *)

  | Tpackage of Path.t * Longident.t list * type_expr list
  (** Type of a first-class module (a.k.a package). *)

and arrow_desc =
  arg_label * alloc_mode * alloc_mode

and alloc_mode_const = Global | Local

and alloc_mode_var = {
  mutable upper: alloc_mode_const;
  mutable lower: alloc_mode_const;
  mutable vlower: alloc_mode_var list;
  mutable mark: bool;
  mvid: int;
}

and alloc_mode =
  | Amode of alloc_mode_const
  | Amodevar of alloc_mode_var

(** [  `X | `Y ]       (row_closed = true)
    [< `X | `Y ]       (row_closed = true)
    [> `X | `Y ]       (row_closed = false)
    [< `X | `Y > `X ]  (row_closed = true)

    type t = [> `X ] as 'a      (row_more = Tvar a)
    type t = private [> `X ]    (row_more = Tconstr (t#row, [], ref Mnil))

    And for:

        let f = function `X -> `X -> | `Y -> `X

    the type of "f" will be a [Tarrow] whose lhs will (basically) be:

        Tvariant { row_fields = [("X", _)];
                   row_more   =
                     Tvariant { row_fields = [("Y", _)];
                                row_more   =
                                  Tvariant { row_fields = [];
                                             row_more   = _;
                                             _ };
                                _ };
                   _
                 }

*)
and row_desc =
    { row_fields: (label * row_field) list;
      row_more: type_expr;
      row_bound: unit; (* kept for compatibility *)
      row_closed: bool;
      row_fixed: fixed_explanation option;
      row_name: (Path.t * type_expr list) option }
and fixed_explanation =
  | Univar of type_expr (** The row type was bound to an univar *)
  | Fixed_private (** The row type is private *)
  | Reified of Path.t (** The row was reified *)
  | Rigid (** The row type was made rigid during constraint verification *)
and row_field =
    Rpresent of type_expr option
  | Reither of bool * type_expr list * bool * row_field option ref
        (* 1st true denotes a constant constructor *)
        (* 2nd true denotes a tag in a pattern matching, and
           is erased later *)
  | Rabsent

(** [abbrev_memo] allows one to keep track of different expansions of a type
    alias. This is done for performance purposes.

    For instance, when defining [type 'a pair = 'a * 'a], when one refers to an
    ['a pair], it is just a shortcut for the ['a * 'a] type.
    This expansion will be stored in the [abbrev_memo] of the corresponding
    [Tconstr] node.

    In practice, [abbrev_memo] behaves like list of expansions with a mutable
    tail.

    Note on marshalling: [abbrev_memo] must not appear in saved types.
    [Btype], with [cleanup_abbrev] and [memo], takes care of tracking and
    removing abbreviations.
*)
and abbrev_memo =
  | Mnil (** No known abbreviation *)

  | Mcons of private_flag * Path.t * type_expr * type_expr * abbrev_memo
  (** Found one abbreviation.
      A valid abbreviation should be at least as visible and reachable by the
      same path.
      The first expression is the abbreviation and the second the expansion. *)

  | Mlink of abbrev_memo ref
  (** Abbreviations can be found after this indirection *)

and field_kind =
    Fvar of field_kind option ref
  | Fpresent
  | Fabsent

(** [commutable] is a flag appended to every arrow type.

    When typing an application, if the type of the functional is
    known, its type is instantiated with [Cok] arrows, otherwise as
    [Clink (ref Cunknown)].

    When the type is not known, the application will be used to infer
    the actual type.  This is fragile in presence of labels where
    there is no principal type.

    Two incompatible applications relying on [Cunknown] arrows will
    trigger an error.

    let f g =
      g ~a:() ~b:();
      g ~b:() ~a:();

    Error: This function is applied to arguments
    in an order different from other calls.
    This is only allowed when the real type is known.
*)
and commutable =
    Cok
  | Cunknown
  | Clink of commutable ref

module TypeOps : sig
  type t = type_expr
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
end

(* *)

module Uid : sig
  type t

  val reinit : unit -> unit

  val mk : current_unit:string -> t
  val of_compilation_unit_id : Ident.t -> t
  val of_predef_id : Ident.t -> t
  val internal_not_actually_unique : t

  val for_actual_declaration : t -> bool

  include Identifiable.S with type t := t
end

(* Maps of methods and instance variables *)

module Meths : Map.S with type key = string
module Vars  : Map.S with type key = string

(* Value descriptions *)

type value_description =
  { val_type: type_expr;                (* Type of the value *)
    val_kind: value_kind;
    val_loc: Location.t;
    val_attributes: Parsetree.attributes;
    val_uid: Uid.t;
  }

and value_kind =
    Val_reg                             (* Regular value *)
  | Val_prim of Primitive.description   (* Primitive *)
  | Val_ivar of mutable_flag * string   (* Instance variable (mutable ?) *)
  | Val_self of (Ident.t * type_expr) Meths.t ref *
                (Ident.t * mutable_flag * virtual_flag * type_expr) Vars.t ref *
                string * type_expr
                                        (* Self *)
  | Val_anc of (string * Ident.t) list * string
                                        (* Ancestor *)

(* Variance *)

module Variance : sig
  type t
  type f =
      May_pos                (* allow positive occurrences *)
    | May_neg                (* allow negative occurrences *)
    | May_weak               (* allow occurrences under a negative position *)
    | Inj                    (* type is injective in this parameter *)
    | Pos                    (* there is a positive occurrence *)
    | Neg                    (* there is a negative occurrence *)
    | Inv                    (* both negative and positive occurrences *)
  val null : t               (* no occurrence *)
  val full : t               (* strictly invariant (all flags) *)
  val covariant : t          (* strictly covariant (May_pos, Pos and Inj) *)
  val unknown : t            (* allow everything, guarantee nothing *)
  val union  : t -> t -> t
  val inter  : t -> t -> t
  val subset : t -> t -> bool
  val eq : t -> t -> bool
  val set : f -> bool -> t -> t
  val mem : f -> t -> bool
  val conjugate : t -> t                (* exchange positive and negative *)
  val get_upper : t -> bool * bool                  (* may_pos, may_neg   *)
  val get_lower : t -> bool * bool * bool * bool    (* pos, neg, inv, inj *)
  val unknown_signature : injective:bool -> arity:int -> t list
  (** The most pessimistic variance for a completely unknown type. *)
end

module Separability : sig
  (** see {!Typedecl_separability} for an explanation of separability
      and separability modes.*)

  type t = Ind | Sep | Deepsep
  val eq : t -> t -> bool
  val print : Format.formatter -> t -> unit

  val rank : t -> int
  (** Modes are ordered from the least to the most demanding:
      Ind < Sep < Deepsep.
      'rank' maps them to integers in an order-respecting way:
      m1 < m2  <=>  rank m1 < rank m2 *)

  val compare : t -> t -> int
  (** Compare two mode according to their mode ordering. *)

  val max : t -> t -> t
  (** [max_mode m1 m2] returns the most demanding mode. It is used to
      express the conjunction of two parameter mode constraints. *)

  type signature = t list
  (** The 'separability signature' of a type assigns a mode for
      each of its parameters. [('a, 'b) t] has mode [(m1, m2)] if
      [(t1, t2) t] is separable whenever [t1, t2] have mode [m1, m2]. *)

  val print_signature : Format.formatter -> signature -> unit

  val default_signature : arity:int -> signature
  (** The most pessimistic separability for a completely unknown type. *)
end

(* Type definitions *)

type type_declaration =
  { type_params: type_expr list;
    type_arity: int;
    type_kind: type_kind;
    type_private: private_flag;
    type_manifest: type_expr option;
    type_variance: Variance.t list;
    (* covariant, contravariant, weakly contravariant, injective *)
    type_separability: Separability.t list;
    type_is_newtype: bool;
    type_expansion_scope: int;
    type_loc: Location.t;
    type_attributes: Parsetree.attributes;
    type_immediate: Type_immediacy.t;
    type_unboxed: unboxed_status;
    type_uid: Uid.t;
  }

and type_kind =
    Type_abstract
  | Type_record of label_declaration list  * record_representation
  | Type_variant of constructor_declaration list
  | Type_open

and record_representation =
    Record_regular                      (* All fields are boxed / tagged *)
  | Record_float                        (* All fields are floats *)
  | Record_unboxed of bool    (* Unboxed single-field record, inlined or not *)
  | Record_inlined of int               (* Inlined record *)
  | Record_extension of Path.t          (* Inlined record under extension *)

and global_flag =
  | Global
  | Nonlocal
  | Unrestricted

and label_declaration =
  {
    ld_id: Ident.t;
    ld_mutable: mutable_flag;
    ld_global: global_flag;
    ld_type: type_expr;
    ld_loc: Location.t;
    ld_attributes: Parsetree.attributes;
    ld_uid: Uid.t;
  }

and constructor_declaration =
  {
    cd_id: Ident.t;
    cd_args: constructor_arguments;
    cd_res: type_expr option;
    cd_loc: Location.t;
    cd_attributes: Parsetree.attributes;
    cd_uid: Uid.t;
  }

and constructor_arguments =
  | Cstr_tuple of type_expr list
  | Cstr_record of label_declaration list

and unboxed_status = private
  (* This type must be private in order to ensure perfect sharing of the
     four possible values. Otherwise, ocamlc.byte and ocamlc.opt produce
     different executables. *)
  {
    unboxed: bool;
    default: bool; (* True for unannotated unboxable types. *)
  }

val unboxed_false_default_false : unboxed_status
val unboxed_false_default_true : unboxed_status
val unboxed_true_default_false : unboxed_status
val unboxed_true_default_true : unboxed_status

type extension_constructor =
  {
    ext_type_path: Path.t;
    ext_type_params: type_expr list;
    ext_args: constructor_arguments;
    ext_ret_type: type_expr option;
    ext_private: private_flag;
    ext_loc: Location.t;
    ext_attributes: Parsetree.attributes;
    ext_uid: Uid.t;
  }

and type_transparence =
    Type_public      (* unrestricted expansion *)
  | Type_new         (* "new" type *)
  | Type_private     (* private type *)

(* Type expressions for the class language *)

module Concr : Set.S with type elt = string

type class_type =
    Cty_constr of Path.t * type_expr list * class_type
  | Cty_signature of class_signature
  | Cty_arrow of arg_label * type_expr * class_type

and class_signature =
  { csig_self: type_expr;
    csig_vars:
      (Asttypes.mutable_flag * Asttypes.virtual_flag * type_expr) Vars.t;
    csig_concr: Concr.t;
    csig_inher: (Path.t * type_expr list) list }

type class_declaration =
  { cty_params: type_expr list;
    mutable cty_type: class_type;
    cty_path: Path.t;
    cty_new: type_expr option;
    cty_variance: Variance.t list;
    cty_loc: Location.t;
    cty_attributes: Parsetree.attributes;
    cty_uid: Uid.t;
  }

type class_type_declaration =
  { clty_params: type_expr list;
    clty_type: class_type;
    clty_path: Path.t;
    clty_variance: Variance.t list;
    clty_loc: Location.t;
    clty_attributes: Parsetree.attributes;
    clty_uid: Uid.t;
  }

(* Type expressions for the module language *)

type visibility =
  | Exported
  | Hidden

type module_type =
    Mty_ident of Path.t
  | Mty_signature of signature
  | Mty_functor of functor_parameter * module_type
  | Mty_alias of Path.t

and functor_parameter =
  | Unit
  | Named of Ident.t option * module_type

and module_presence =
  | Mp_present
  | Mp_absent

and signature = signature_item list

and signature_item =
    Sig_value of Ident.t * value_description * visibility
  | Sig_type of Ident.t * type_declaration * rec_status * visibility
  | Sig_typext of Ident.t * extension_constructor * ext_status * visibility
  | Sig_module of
      Ident.t * module_presence * module_declaration * rec_status * visibility
  | Sig_modtype of Ident.t * modtype_declaration * visibility
  | Sig_class of Ident.t * class_declaration * rec_status * visibility
  | Sig_class_type of Ident.t * class_type_declaration * rec_status * visibility

and module_declaration =
  {
    md_type: module_type;
    md_attributes: Parsetree.attributes;
    md_loc: Location.t;
    md_uid: Uid.t;
  }

and modtype_declaration =
  {
    mtd_type: module_type option;  (* None: abstract *)
    mtd_attributes: Parsetree.attributes;
    mtd_loc: Location.t;
    mtd_uid: Uid.t;
  }

and rec_status =
    Trec_not                   (* first in a nonrecursive group *)
  | Trec_first                 (* first in a recursive group *)
  | Trec_next                  (* not first in a recursive/nonrecursive group *)

and ext_status =
    Text_first                     (* first constructor in an extension *)
  | Text_next                      (* not first constructor in an extension *)
  | Text_exception


(* Constructor and record label descriptions inserted held in typing
   environments *)

type constructor_description =
  { cstr_name: string;                  (* Constructor name *)
    cstr_res: type_expr;                (* Type of the result *)
    cstr_existentials: type_expr list;  (* list of existentials *)
    cstr_args: type_expr list;          (* Type of the arguments *)
    cstr_arity: int;                    (* Number of arguments *)
    cstr_tag: constructor_tag;          (* Tag for heap blocks *)
    cstr_consts: int;                   (* Number of constant constructors *)
    cstr_nonconsts: int;                (* Number of non-const constructors *)
    cstr_normal: int;                   (* Number of non generalized constrs *)
    cstr_generalized: bool;             (* Constrained return type? *)
    cstr_private: private_flag;         (* Read-only constructor? *)
    cstr_loc: Location.t;
    cstr_attributes: Parsetree.attributes;
    cstr_inlined: type_declaration option;
    cstr_uid: Uid.t;
   }

and constructor_tag =
    Cstr_constant of int                (* Constant constructor (an int) *)
  | Cstr_block of int                   (* Regular constructor (a block) *)
  | Cstr_unboxed                        (* Constructor of an unboxed type *)
  | Cstr_extension of Path.t * bool     (* Extension constructor
                                           true if a constant false if a block*)

(* Constructors are the same *)
val equal_tag :  constructor_tag -> constructor_tag -> bool

(* Constructors may be the same, given potential rebinding *)
val may_equal_constr :
    constructor_description ->  constructor_description -> bool

type label_description =
  { lbl_name: string;                   (* Short name *)
    lbl_res: type_expr;                 (* Type of the result *)
    lbl_arg: type_expr;                 (* Type of the argument *)
    lbl_mut: mutable_flag;              (* Is this a mutable field? *)
    lbl_global: global_flag;        (* Is this a nonlocal field? *)
    lbl_pos: int;                       (* Position in block *)
    lbl_all: label_description array;   (* All the labels in this type *)
    lbl_repres: record_representation;  (* Representation for this record *)
    lbl_private: private_flag;          (* Read-only field? *)
    lbl_loc: Location.t;
    lbl_attributes: Parsetree.attributes;
    lbl_uid: Uid.t;
  }

(** Extracts the list of "value" identifiers bound by a signature.
    "Value" identifiers are identifiers for signature components that
    correspond to a run-time value: values, extensions, modules, classes.
    Note: manifest primitives do not correspond to a run-time value! *)
val bound_value_identifiers: signature -> Ident.t list

val signature_item_id : signature_item -> Ident.t

type value_mode =
  (* See Btype.Value_mode *)
  { r_as_l : alloc_mode;
    r_as_g : alloc_mode; }
