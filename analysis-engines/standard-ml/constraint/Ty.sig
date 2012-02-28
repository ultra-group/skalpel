(* Copyright 2009 2010 2011 2012 Heriot-Watt University
 *
 * Skalpel is a free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Skalpel is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Skalpel. If not, see <http://www.gnu.org/licenses/>.
 *
 *  o Authors:     Vincent Rahli, John Pirie
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        24 May 2010
 *  o File name:   Ty.sig
 *  o Description: This file defines the signature TY specifying the
 *      internal types used by our slicer.
 *)


signature TY = sig

    (* ------ VARIABLES ------ *)
    type tyvar
    type seqvar
    type tynamevar
    type labvar
    type rowvar
    type tyfvar

    type tyname (* 1: arrow, 2: record, ... *)
    type idor
    type labcons    = string (* int for tyname but string for labcons because not in any environment *)
    type flex       = Label.label option
    type extv       = Id.labelledId option
    (* extv stands for EXplicit Type Variable *)
    type exttyvar   = tyvar ExtLab.extLab

    (* to hold type variables which are constrained to be equality types *)
    (* maybe we should be using CSTxxx for this....*)
    val eqTypeTyVars : tyvar list ref

    (*type assoc      = (tyvar * string) list*)
    datatype poly   = POLY | MONO (* polymorphic or monomorphic type variable *)
    datatype kcons  = DE of Id.id (* constructor from a DEclaration   *)
		    | PA          (* constructor from a PAttern       *)
		    | OT          (* OTher constructor                *)
		    | BB          (* constructor in the Builtin Basis *)
    (* We have the last constructor (BB) because for type constructor labeled by BB
     * we might want to re-label them to get end points. *)

    datatype orKind = VAL of Id.labelledId                       (* value    *)
		    | CST of string * Id.id * Label.label (* constant *)

    datatype labty  = LV  of labvar
		    | LC  of labcons * Label.label
		    | LD  of labty ExtLab.extLab
    datatype tnty   = NV  of tynamevar
		    | NC  of tyname * kcons * Label.label (* kcons is used to know if the constructor comes from a declaration (arrow type: datatype constructor with argument).  It is only used for the arrow type isn't it? *)
		    | ND  of tnty ExtLab.extLab
    datatype rowty  = RV  of rowvar
		    | RC  of labty * ty * Label.label
		    | RD  of rowty ExtLab.extLab
		    | RO (* marker to show that a row has been deleted from a sequence because it is cannot be an overloaded type while a RV can *)
	 and seqty  = SV  of seqvar
		    | SC  of rowty list * flex  * Label.label (* flex is SOME _ if the record is flexible *)
		    | SD  of seqty ExtLab.extLab
	 and tyfun  = TFV of tyfvar
		    | TFC of seqty * ty * Label.label
		    | TFD of tyfun ExtLab.extLab
	 and ty     = V   of tyvar  * extv  * poly                        (* Implicit Type variable    *)
                    | E   of Id.id  * tyvar                 * Label.label (* Explicit type variable    *)
                    | C   of tnty   * seqty                 * Label.label (* Type construction         *)
		    | A   of tyfun  * seqty                 * Label.label (* Type scheme instantiation *) (*(2010-06-23)Do we actually use that?*)
		    | OR  of seqty  * idor  * poly * orKind * Label.label (* Polymorphic type  *)
		    | GEN of ty list ref                                  (* intersection type *)
		    | TD  of ty ExtLab.extLab                             (* Type annotated with dependencies *)
    (* Concerning V:
     *   - The option (extv) is NONE if the variable does not come from an
     *     explicit type variable.
     * Concerning E:
     *   - Used for an explicit type variable.
     *   - The second variable is the generalisation.
     * Concerning C:
     *   - Used for a type constructor.
     * Concerning Or:
     *   - Used for a list of possible types: overloading.
     *   - The idor is so that an or type is unique even after freshning.
     *   - The identifier is so that we in case of an overloading type error,
     *     we can report the overloaded identifier. *)

    datatype names = TYNAME of tyname | DUMTYNAME of tyname | MAYTYNAME | NOTTYNAME

    (* list of top-level type constructors *)
    val tyNames           : string list

    val consTyNameVar     : Label.label -> ty

    val consV             : tyvar  -> ty
    val consSV            : seqvar -> seqty
    val consTFV           : tyfvar -> tyfun

    val newV              : unit -> ty
    val newRV             : unit -> rowty
    val newSV             : unit -> seqty
    val newTFV            : unit -> tyfun

    val tyvarToInt        : tyvar     -> int
    val tyfvarToInt       : tyfvar    -> int
    val tynamevarToInt    : tynamevar -> int
    val tynameToInt       : tyname    -> int
    val labvarToInt       : labvar    -> int
    val rowvarToInt       : rowvar    -> int
    val seqvarToInt       : seqvar    -> int
    val idorToInt         : idor      -> int

    val tynameFromInt     : int -> tyname

    val eqTyvar           : tyvar     -> tyvar     -> bool
    val eqSeqvar          : seqvar    -> seqvar    -> bool
    val eqLabvar          : labvar    -> labvar    -> bool
    val eqRowvar          : rowvar    -> rowvar    -> bool
    val eqTyname          : tyname    -> tyname    -> bool
    val eqTynamevar       : tynamevar -> tynamevar -> bool
    val eqIdor            : idor      -> idor      -> bool

    (*val eqSeqTy           : seqty     -> seqty     -> bool*)

    val isTyName          : tyfun -> names ExtLab.extLab

    (* extract the type name from a type function of the form:
     *   T.TFC (_, T.C (T.NC (name, _, _), _, _), _)
     * where we omit the dependencies. *)
    val getNameTyName     : tyfun -> tyname option

    val noflex            : unit -> flex
    val consflex          : Label.label -> flex
    val isflex            : flex -> bool
    val getflex           : flex -> Label.label
    val printflex         : flex -> string

    (*val emAssoc           : assoc*)

    val DUMMYTYNAME       : tyname
    (*val consrecord        : unit -> tyname
    val consarrow         : unit -> tyname
    val consint           : unit -> tyname
    val consword          : unit -> tyname*)
    val CONSREAL          : tyname
    (*val consbool          : unit -> tyname
    val consstring        : unit -> tyname
    val conschar          : unit -> tyname
    val consexception     : unit -> tyname
    val conslist          : unit -> tyname
    val consref           : unit -> tyname
    val conssubstring     : unit -> tyname
    val consarray         : unit -> tyname
    val consvector        : unit -> tyname
    val consoption        : unit -> tyname
    val consorder         : unit -> tyname
    val constypenamestart : unit -> tyname

    val conslabty         : int -> Label.label -> labty*)

    val constuple         : tyvar list -> Label.label -> rowty list

    (* constructors without kinds *)
    val constyint         : Label.label -> ty
    val constyword        : Label.label -> ty
    val constyreal        : Label.label -> ty
    val constybool        : Label.label -> ty
    val constystring      : Label.label -> ty
    val constychar        : Label.label -> ty
    val constyexception   : Label.label -> ty
    val constyunit        : Label.label -> ty
    val constynewcons     : Label.label -> ty
    val constysubstring   : Label.label -> ty
    val constyorder       : Label.label -> ty
    val constylist        : tyvar -> Label.label -> ty
    val constyref         : tyvar -> Label.label -> ty
    val constyarray       : tyvar -> Label.label -> ty
    val constyvector      : tyvar -> Label.label -> ty
    val constyoption      : tyvar -> Label.label -> ty
    val constyfrag        : tyvar -> Label.label -> ty
    val constyarrow       : tyvar -> tyvar -> Label.label -> ty
    val constytuple       : tyvar list -> Label.label -> ty
    val constyrecord      : rowvar list -> Label.label option -> Label.label -> ty
    (* the label option is for the flex *)

    (* constructors with kinds *)
    val constyint'        : Label.label -> kcons -> ty
    val constyword'       : Label.label -> kcons -> ty
    val constyreal'       : Label.label -> kcons -> ty
    val constybool'       : Label.label -> kcons -> ty
    val constystring'     : Label.label -> kcons -> ty
    val constychar'       : Label.label -> kcons -> ty
    val constyexception'  : Label.label -> kcons -> ty
    val constyunit'       : Label.label -> kcons -> ty
    val constynewcons'    : Label.label -> kcons -> ty
    val constysubstring'  : Label.label -> kcons -> ty
    val constyorder'      : Label.label -> kcons -> ty
    val constylist'       : tyvar -> Label.label -> kcons -> ty
    val constyref'        : tyvar -> Label.label -> kcons -> ty
    val constyarray'      : tyvar -> Label.label -> kcons -> ty
    val constyvector'     : tyvar -> Label.label -> kcons -> ty
    val constyoption'     : tyvar -> Label.label -> kcons -> ty
    val constyfrag'       : tyvar -> Label.label -> kcons -> ty
    val constyarrow'      : tyvar -> tyvar -> Label.label -> kcons -> ty
    val constytuple'      : tyvar list -> Label.label -> kcons -> ty
    val constyrecord'     : rowvar list -> Label.label option -> Label.label -> kcons -> ty

    (* constructors on types *)
    val consTyArrowTy     : ty -> ty -> Label.label -> kcons -> ty
    val consTyTupleTy     : ty list  -> Label.label -> kcons -> ty


    (* re-label a type if its top-level type constructor is from the builtin basis *)
    val labelBuiltinTy    : ty    -> Label.label -> ty
    val labelBuiltinTyf   : tyfun -> Label.label -> tyfun

    val isTyC             : ty -> bool
    val isTyV             : ty -> bool
    val getTyLab          : ty -> Label.label option
    val getTyName         : ty -> tnty option

    val isBase            : tyname -> bool
    val isBase'           : tyname -> bool
    val isBaseTy          : tnty   -> bool
    val isArrowTy         : tnty   -> bool
    val isExcTy           : tnty   -> bool
    val isDecTy           : tnty   -> bool
    val isPatTy           : tnty   -> bool
    val isVarTyName       : tnty   -> bool
    val isShallowSeq      : seqty  -> bool

    val freshtyvar        : unit -> tyvar
    val freshtyfvar       : unit -> tyfvar
    val freshseqvar       : unit -> seqvar
    val freshtynamevar    : unit -> tynamevar
    val freshlabvar       : unit -> labvar
    val freshrowvar       : unit -> rowvar
    val freshtyname       : unit -> tyname
    val freshidor         : unit -> idor

    val resetnexts        : unit -> unit

    val gettyvar          : unit -> tyvar
    val getseqvar         : unit -> seqvar
    val getrowvar         : unit -> rowvar
    val getlabvar         : unit -> labvar
    val gettynamevar      : unit -> seqvar
    val getidor           : unit -> idor

    val getTyNameString   : string -> tyname

    (* gettyvarsty is used by the unification algorithm to recompute
     * the monomorphic type variables.
     * This is at least used by enum/StateEnv.sml *)
    val getTyVarsTy       : ty -> exttyvar list

    (*(* Extract the tyvar from a type.  The type has to be a type variable. *)
    val tyToTyvar         : ty -> tyvar
    (* Extract the seqvar from a type sequence.  The type sequence has to be a sequence variable.*)
    val seqToSeqvar       : seqty -> seqvar*)

    (* strip the dependencies off a sequence type *)
    val stripDepsSq       : seqty -> seqty ExtLab.extLab

    (* Returns the actual type constructor of a tnty *)
    val tntyToTyCon       : tnty -> tyname

    val printTyNameAssoc  : tyname -> Id.assoc -> string

    (* true if a term of type poly is POLY, false if MONO *)
    val isPoly            : poly -> bool

    val printtyvar        : tyvar       -> string
    val printtyvarlist    : tyvar list  -> string
    val printseqvar       : seqvar      -> string
    val printseqvarlist   : seqvar list -> string
    val printtynamevar    : tynamevar   -> string
    val printtyname       : tyname      -> string
    val printtyname'      : tyname      -> string
    val printsmltn        : tyname      -> string
    val printseqty        : seqty       -> string
    val printseqty'       : seqty       -> string
    val printtnty         : tnty        -> string
    val printtnty'        : tnty        -> string
    val printtyf          : tyfun       -> string
    val printtyf'         : tyfun       -> string
    val printty           : ty          -> string
    val printty'          : ty          -> string
    val printtylist       : ty list     -> string
    val printlabcons      : labcons     -> string
    val printsmllc        : labcons     -> string
    val printrowty        : rowty       -> string
    val printrowty'       : rowty       -> string
    val printrowtylist    : rowty list  -> string
    val printrowvar       : rowvar      -> string
    val printlabty        : labty       -> string
    (*val printAssoc        : assoc       -> string
    val printAssoc'       : assoc       -> string*)

end
