(* Copyright 2009 2010 2011 2012 Heriot-Watt University
 *
 *
 * This file is part of the ULTRA SML Type Error Slicer (SMLTES) -
 * a Type Error Slicer for Standard ML written by the ULTRA Group of
 * Heriot-Watt University, Edinburgh.
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
 * along with Skalpel.  If not, see <http://www.gnu.org/licenses/>.
 *
 *  o Authors:     Vincent Rahli, John Pirie
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        24 May 2010
 *  o File name:   Ty.sml
 *  o Description: This file contains the definition of the internal
 *      types used by our slicer.  The file defines the structure Ty
 *      which has signature TY.
 *)


structure Ty :> TY = struct

(* shorten the name of structures *)
structure L  = Label
structure I  = Id
structure D  = Debug
structure CD = LongId
structure EL = ExtLab
structure EH = ErrorHandler
structure S  = BinaryMapFn(OrdKey) (* Used for type variable sets *)
structure D  = Debug

(* new exception, raised if a non-option value is passed to getflex function *)
exception unflex

(* type and datatype declarations *)
type typeVar            = int
type sequenceVar = int
type typenameVar        = int
type labelVar           = int
type rowVar           = int
type typeFunctionVar           = int
type eqType           = bool

(* int for typename but string for labcons because not in any environment *)
type typename     = int (* 1: arrow, 2: record, ... *)
type labcons    = string
type idor       = int
type flex       = L.label option
type extv       = I.labelledId option (* extv stands for EXplicit Type Variable*)
type assoc      = (int * string) list

type explicitTypeVar = typeVar ExtLab.extLab

    datatype poly   = POLY (* polymorphic type var *)
		    | MONO (* monomorphic type var *)

    (* --------------------------------------------------------------------------------
     * The constructorKind datatype:
     *
     * DECLARATION_CONS: constructor from a declaration
     * PATTERN_CONS: constructor from a pattern
     * OTHER_CONS: other constructor
     * BUILTIN_BASIS_CONS: We have this constructor because for type constructor labeled
     *                     by BUILTIN_BASIS. We might want to re-label them to get end
     *                     points. *)

    datatype constructorKind  = DECLARATION_CONS of Id.id
			      | PATTERN_CONS
			      | OTHER_CONS
			      | BUILTIN_BASIS_CONS

    (* ------------------------------------------------------------------------------*)

    datatype orKind = VALUE of Id.labelledId
		    | CONSTANT of string * Id.id * Label.label

    (* it is unclear why we need label vars (LABEL_VAR constructor) *)
    datatype labelType = LABEL_VAR  of labelVar
		       | LC  of labcons * Label.label
		       | LABEL_DEPENDANCY  of labelType ExtLab.extLab

    (* Joe speculates that typenameType and tyfun could be simply merged.
     * This means the C and A cases of ty could also be merged.
     * Probably nothing wrong with leaving them separate as they are now.
     *
     * constructorKind in the NC constructor is used to know if the constructor
     * comes from a declaration (arrow type: datatype constructor with argument).
     * It is only used for the arrow type isn't it? *)
    datatype typenameType = TYPENAME_VAR  of typenameVar
			  | NC  of typename * constructorKind * Label.label
			  | TYPENAME_DEPENDANCY  of typenameType ExtLab.extLab

    (* ROW_NO_OVERLOAD: marker to show that a row has been deleted from a sequence because
     * it is cannot be an overloaded type while a RV can *)
    datatype rowType = ROW_VAR  of rowVar
		     | RC  of labelType * ty * Label.label
		     | ROW_DEPENDANCY  of rowType ExtLab.extLab
		     | ROW_NO_OVERLOAD

	 and sequenceType = SEQUENCE_VAR  of sequenceVar
			  | SC  of rowType list * flex  * Label.label (* flex is SOME _ if the record is flexible *)
			  | SEQUENCE_DEPENDANCY  of sequenceType ExtLab.extLab

	 and typeFunction = TYPE_FUNCTION_VAR of typeFunctionVar
			  | TFC of sequenceType * ty * Label.label
			  | TYPE_FUNCTION_DEPENDANCY of typeFunction ExtLab.extLab

	 (*--------------------------------------------------------------------------
	  * The ty datatype:
	  *
	  * TYPE_VAR: implicit type variable. The option (extv) is  NONE if
	  *                the variable does not come from an explicit type varaible
	  * EXPLICIT_TYPE_VAR: explicit type variable. The second variable is
	  *                         the generalisation
	  * TYPE_CONSTRUCTOR: type constructor
	  * APPLICATION: type scheme instantiation (do we need this?)
	  * TYPE_POLY: polymorphic type. the idor is so that an or type is unique
	  *            even after freshening. The id is so that we can in case of
          *            an overloading type error, report the overloaded id.
	  * GEN: intersection type
	  * TYPE_DEPENDANCY: type annotated with dependancies *)

	 and ty = TYPE_VAR          of typeVar  * extv  * poly
                | EXPLICIT_TYPE_VAR of Id.id  * typeVar * Label.label
		| TYPE_CONSTRUCTOR       of typenameType   * sequenceType * Label.label
		| APPLICATION            of typeFunction  * sequenceType * Label.label
		| TYPE_POLY              of sequenceType  * idor  * poly * orKind * Label.label
		| GEN                    of ty list ref
		| TYPE_DEPENDANCY        of ty ExtLab.extLab

         (*------------------------------------------------------------------------*)

(* to hold type variables which are constrained to be equality types *)
(* maybe we should be using CSTxxx for this....*)
val eqTypeTyVars : typeVar list ref = ref []

datatype names = TYPENAME of typename | DUMTYPENAME of typename | MAYTYPENAME | NOTTYPENAME

(* List of top level type constructors (builtin type names).
 * When analysing the basis file, the first occurrence of such a type name
 * is the occurrence which is affected the internal type name as defined by
 * the getTypenameString function (see below). *)
val typenames = ["unit"     , "int"  , "word" , "real"  , "char", "string",
	       "substring", "exn"  , "array", "vector", "ref" , "bool"  ,
	       "option"   , "order", "list" , "frag"]

(* List of the overloading classes as they are called in the Definition
 * of SML.  These are hard coded in the parser. *)
val ovClasses = ["Int", "Word", "Real", "Char", "String"]

(*val emAssoc = []*)

fun noflex   _ = NONE
fun consflex l = SOME l
fun isflex   f = Option.isSome f
fun getflex  f = Option.valOf f handle Option => raise unflex

(* integer constants representing types *)
val DUMMYTYPENAME        = 0
val CONSARROW          = 1
val CONSRECORD         = 2
val CONSINT            = 3
val CONSWORD           = 4
val CONSREAL           = 5
val CONSBOOL           = 6
val CONSSTRING         = 7
val CONSLIST           = 8
val CONSREF            = 9
val CONSCHAR           = 10
val CONSEXCEPTION      = 11
val CONSSUBSTRING      = 12
val CONSARRAY          = 13
val CONSVECTOR         = 14
val CONSOPTION         = 15
val CONSORDER          = 16
val CONSFRAG           = 17
val CONSTYPENAMESTART  = 18

val nextTypenameVar        = ref 0 (* next type name variable *)
val nextTypeVar            = ref 0 (*  *)
val nextSequenceVar        = ref 0 (* next sequence variable *)
val nextLabelVar           = ref 0 (* next label variable *)
val nextRowVar             = ref 0 (* next row variable *)
val nextTypeFunctionVar    = ref 0 (* next ?? *)
val nextidor               = ref 0 (* next id variable *)
val nextTypename           = ref (CONSTYPENAMESTART)

(* sets the above ref values to a value n *)
fun setnexts n =
    let val _ = nextTypenameVar := n
	val _ = nextTypeVar     := n
	val _ = nextSequenceVar    := n
	val _ = nextRowVar    := n
	val _ = nextLabelVar    := n
	val _ = nextTypeFunctionVar    := n
	val _ = nextidor      := n
	val _ = nextTypename    := (CONSTYPENAMESTART)
    in ()
    end

(* resets all ref values above to 0 *)
fun resetnexts () = setnexts 0

(* accessor methods *)
fun getTypeVar     () = !nextTypeVar
fun getsequenceVar    () = !nextSequenceVar
fun getRowVar    () = !nextRowVar
fun getLabelVar    () = !nextLabelVar
fun getTypeFunctionVar    () = !nextTypeFunctionVar
fun getidor      () = !nextidor
fun getTypenameVar () = !nextTypenameVar

(* funtions to cast defined types to integers *)
fun typeVarToInt          typeVar         = typeVar
fun typeFunctionVarToInt  typeFunctionVar = typeFunctionVar
fun labelVarToInt         labelVar        = labelVar
fun rowVarToInt           rowVar          = rowVar
fun typenameVarToInt      typenameVar     = typenameVar
fun typenameToInt         typename          = typename
fun sequenceVarToInt      sequenceVar     = sequenceVar
fun idorToInt             idor            = idor

fun typenameFromInt typename = typename

(* equality testing for defined types *)
fun eqTypeVar     tv1 tv2 = (tv1 = (tv2 : typeVar))
fun eqTypeFunctionVar    fv1 fv2 = (fv1 = (fv2 : typeFunctionVar))
fun eqSequenceVar    sv1 sv2 = (sv1 = (sv2 : sequenceVar))
fun eqLabelVar    lv1 lv2 = (lv1 = (lv2 : labelVar))
fun eqRowVar    rv1 rv2 = (rv1 = (rv2 : rowVar))
fun eqIdor      id1 id2 = (id1 = (id2 : idor))
fun eqTypename    tn1 tn2 = (tn1 = (tn2 : typename))
fun eqTypenameVar nv1 nv2 = (nv1 = (nv2 : typenameVar))

(* Extract the name of a type name *)
fun getNameTypenameTn (NC (name, _, _)) = SOME name
  | getNameTypenameTn (TYPENAME_DEPENDANCY exttn) = getNameTypenameTn (EL.getExtLabT exttn)
  | getNameTypenameTn _ = NONE

fun getNameTypenameTy (TYPE_CONSTRUCTOR (tn, _, _)) = getNameTypenameTn tn
  | getNameTypenameTy (TYPE_DEPENDANCY extty) = getNameTypenameTy (EL.getExtLabT extty)
  | getNameTypenameTy _ = NONE


(* extract the type name from a type function of the form:
 *   T.TFC (_, T.C (T.NC (name, _, _), _, _), _)
 * where we omit the dependencies. *)
fun getTypename (TFC (_, ty, _)) = getNameTypenameTy ty
  | getTypename (TYPE_FUNCTION_DEPENDANCY exttf) = getTypename (EL.getExtLabT exttf)
  | getTypename _ = NONE


(* Strip dependencies off types *)

fun stripDepsTf (tf as TYPE_FUNCTION_DEPENDANCY (tf1, labs1, stts1, deps1)) =
    let val (tf2, labs2, stts2, deps2) = stripDepsTf tf1
    in (tf2, L.union labs1 labs2, L.union stts1 stts2, CD.union deps1 deps2)
    end
  | stripDepsTf (tf as TFC (sq, ty, lab)) =
    let val (sq', labs1, stts1, deps1) = stripDepsSq sq
	val (ty', labs2, stts2, deps2) = stripDepsTy ty
    in (TFC (sq', ty', lab),
	L.union  labs1 labs2,
	L.union  stts1 stts2,
	CD.union deps1 deps2)
    end
  | stripDepsTf (tf as TYPE_FUNCTION_VAR _) =
    (tf, L.empty, L.empty, CD.empty)

and stripDepsTy (ty as TYPE_VAR _) = (ty, L.empty, L.empty, CD.empty)
  | stripDepsTy (ty as EXPLICIT_TYPE_VAR _) = (ty, L.empty, L.empty, CD.empty)
  | stripDepsTy (ty as TYPE_CONSTRUCTOR(tn, sq, lab)) =
    let val (tn', labs1, stts1, deps1) = stripDepsTn tn
	val (sq', labs2, stts2, deps2) = stripDepsSq sq
    in (TYPE_CONSTRUCTOR (tn', sq', lab),
	L.union  labs1 labs2,
	L.union  stts1 stts2,
	CD.union deps1 deps2)
    end
  | stripDepsTy (ty as APPLICATION (tf, sq, lab)) =
    let val (tf', labs1, stts1, deps1) = stripDepsTf tf
	val (sq', labs2, stts2, deps2) = stripDepsSq sq
    in (APPLICATION (tf', sq', lab),
	L.union  labs1 labs2,
	L.union  stts1 stts2,
	CD.union deps1 deps2)
    end
  | stripDepsTy (ty as TYPE_POLY (sq, id, p, k, lab)) =
    let val (sq', labs, stts, deps) = stripDepsSq sq
    in (TYPE_POLY (sq', id, p, k, lab), labs, stts, deps)
    end
  | stripDepsTy (ty as GEN tyr) =
    let val (tys, labs, stts, deps) =
	    foldr (fn (ty, (tys, labs, stts, deps)) =>
		      let val (ty', labs', stts', deps') = stripDepsTy ty
		      in (ty' :: tys,
			  L.union  labs labs',
			  L.union  stts stts',
			  CD.union deps deps')
		      end)
		  ([], L.empty, L.empty, CD.empty)
		  (!tyr)
    in tyr := tys;
       (GEN tyr, labs, stts, deps)
    end
  | stripDepsTy (ty as TYPE_DEPENDANCY (ty1, labs1, stts1, deps1)) =
    let val (ty2, labs2, stts2, deps2) = stripDepsTy ty1
    in (ty2, L.union labs1 labs2, L.union stts1 stts2, CD.union deps1 deps2)
    end

(* strip the dependencies off a sequence type *)
and stripDepsSq (sq as SEQUENCE_VAR _) = (sq, L.empty, L.empty, CD.empty)
  | stripDepsSq (sq as SC (rows, flex, lab)) =
    let val (rows', labs, stts, deps) =
	    foldr (fn (row, (rows, labs, stts, deps)) =>
		      let val (row', labs', stts', deps') = stripDepsRt row
		      in (row' :: rows,
			  L.union  labs labs',
			  L.union  stts stts',
			  CD.union deps deps')
		      end)
		  ([], L.empty, L.empty, CD.empty)
		  rows
    in (SC (rows', flex, lab), labs, stts, deps)
    end
  | stripDepsSq (sq as SEQUENCE_DEPENDANCY (sq1, labs1, stts1, deps1)) =
    let val (sq2, labs2, stts2, deps2) = stripDepsSq sq1
    in (sq2, L.union labs1 labs2, L.union stts1 stts2, CD.union deps1 deps2)
    end

and stripDepsRt (rt as ROW_VAR _) = (rt, L.empty, L.empty, CD.empty)
  | stripDepsRt (rt as RC (lt, ty, lab)) =
    let val (lt', labs1, stts1, deps1) = stripDepsLt lt
	val (ty', labs2, stts2, deps2) = stripDepsTy ty
    in (RC (lt', ty', lab),
	L.union  labs1 labs2,
	L.union  stts1 stts2,
	CD.union deps1 deps2)
    end
  | stripDepsRt (rt as ROW_DEPENDANCY (rt1, labs1, stts1, deps1)) =
    let val (rt2, labs2, stts2, deps2) = stripDepsRt rt1
    in (rt2, L.union labs1 labs2, L.union stts1 stts2, CD.union deps1 deps2)
    end
  | stripDepsRt RO = (RO, L.empty, L.empty, CD.empty)

and stripDepsLt (lt as LABEL_VAR _) = (lt, L.empty, L.empty, CD.empty)
  | stripDepsLt (lt as LC _) = (lt, L.empty, L.empty, CD.empty)
  | stripDepsLt (lt as LABEL_DEPENDANCY (lt1, labs1, stts1, deps1)) =
    let val (lt2, labs2, stts2, deps2) = stripDepsLt lt1
    in (lt2, L.union labs1 labs2, L.union stts1 stts2, CD.union deps1 deps2)
    end

and stripDepsTn (tn as TYPENAME_VAR _) = (tn, L.empty, L.empty, CD.empty)
  | stripDepsTn (tn as NC _) = (tn, L.empty, L.empty, CD.empty)
  | stripDepsTn (tn as TYPENAME_DEPENDANCY (tn1, labs1, stts1, deps1)) =
    let val (tn2, labs2, stts2, deps2) = stripDepsTn tn1
    in (tn2, L.union labs1 labs2, L.union stts1 stts2, CD.union deps1 deps2)
    end


(* NONE means not a type name, SOME SOME tn means a type name tn, SOME NONE means we can't now.*)

fun isTypename tf =
    let val (tf', labs, stts, deps) = stripDepsTf tf
    in (isTypename' tf', labs, stts, deps)
    end

and isTypename' (TFC (sq1, TYPE_CONSTRUCTOR (NC (name, _, _), sq2, _), _)) =
    (case eqSeqTy sq1 sq2 of
	 SOME true  => TYPENAME name
       | SOME false => DUMTYPENAME name
       | NONE       => NOTTYPENAME)
  | isTypename' (TFC (sq1, TYPE_CONSTRUCTOR (_, sq2, _), _)) =
    (case eqSeqTy sq1 sq2 of
	 SOME _ => MAYTYPENAME
       | NONE   => NOTTYPENAME)
  | isTypename' (TFC (_, TYPE_VAR (_, SOME _, _), _)) = NOTTYPENAME
  | isTypename' _ = MAYTYPENAME

and eqSeqTy (SEQUENCE_VAR sv1) (SEQUENCE_VAR sv2) =
    if eqSequenceVar sv1 sv2
    then SOME true
    else SOME false
  | eqSeqTy (SC (rows1, _, _)) (SC (rows2, _, _)) = eqRowTys rows1 rows2
  | eqSeqTy _ _ = SOME false

and eqRowTys [] [] = SOME true
  | eqRowTys (row1 :: rows1) (row2 :: rows2) =
    (case (eqRowTy row1 row2, eqRowTys rows1 rows2) of
	 (SOME b1, SOME b2) => SOME (b1 andalso b2) (*if we can't know for one then we can't know*)
       | _ => NONE)
  | eqRowTys _ _ = NONE

and eqRowTy (ROW_VAR rv1) (ROW_VAR rv2) =
    if eqRowVar rv1 rv2
    then SOME true
    else SOME false
  | eqRowTy (RC (lab1, ty1, _)) (RC (lab2, ty2, _)) =
    (case (eqLabTy lab1 lab2, eqTy ty1 ty2) of
	 (SOME b1, SOME b2) => SOME (b1 andalso b2) (*if we can't know for one then we can't know*)
       | _ => NONE)
  | eqRowTy _ _ = SOME false

and eqLabTy (LABEL_VAR lv1) (LABEL_VAR lv2) =
    if eqLabelVar lv1 lv2
    then SOME true
    else SOME false
  | eqLabTy (LC (lc1, _)) (LC (lc2, _)) =
    if lc1 = lc2
    then SOME true
    else NONE
  | eqLabTy _ _ = SOME false

and eqTy (TYPE_VAR (tv1, _, _)) (TYPE_VAR (tv2, _, _)) =
    if eqTypeVar tv1 tv2
    then SOME true
    else SOME false
  | eqTy _ _ = NONE


(* Freshening *)

(* increment a ref value given as a parameter *)
fun freshAVar avar = let val x = !avar in (avar := !avar + 1; x) end

(* increments the ref associated with the function name *)
fun freshTypeVar     () = (D.printDebug 3 D.TY ("generating fresh AVar for typeVar ("^(Int.toString (!nextTypeVar))^")");         freshAVar nextTypeVar)
fun freshSequenceVar    () = (D.printDebug 3 D.TY ("generating fresh AVar for sequenceVar ("^(Int.toString (!nextSequenceVar))^")");       freshAVar nextSequenceVar)
fun freshTypenameVar () = (D.printDebug 3 D.TY ("generating fresh AVar for typenameVar ("^(Int.toString (!nextTypenameVar))^")"); freshAVar nextTypenameVar)
fun freshLabelVar    () = (D.printDebug 3 D.TY ("generating fresh AVar for labelVar ("^(Int.toString (!nextLabelVar))^")");       freshAVar nextLabelVar)
fun freshRowVar    () = (D.printDebug 3 D.TY ("generating fresh AVar for rowVar ("^(Int.toString (!nextRowVar))^")");       freshAVar nextRowVar)
fun freshTypeFunctionVar    () = (D.printDebug 3 D.TY ("generating fresh AVar for typeFunctionVar ("^(Int.toString (!nextTypeFunctionVar))^")");       freshAVar nextTypeFunctionVar)
fun freshTypename    () = (D.printDebug 3 D.TY ("generating fresh AVar for name ("^(Int.toString (!nextTypename))^")");         freshAVar nextTypename)
fun freshidor      () = (D.printDebug 3 D.TY ("generating fresh AVar for idor ("^(Int.toString (!nextidor))^")");           freshAVar nextidor)

fun getTypenameString "unit"      = CONSRECORD
  | getTypenameString "int"       = CONSINT
  | getTypenameString "word"      = CONSWORD
  | getTypenameString "real"      = CONSREAL
  | getTypenameString "bool"      = CONSBOOL
  | getTypenameString "string"    = CONSSTRING
  | getTypenameString "list"      = CONSLIST
  | getTypenameString "ref"       = CONSREF
  | getTypenameString "char"      = CONSCHAR
  | getTypenameString "exn"       = CONSEXCEPTION
  | getTypenameString "substring" = CONSSUBSTRING
  | getTypenameString "array"     = CONSARRAY
  | getTypenameString "vector"    = CONSVECTOR
  | getTypenameString "option"    = CONSOPTION
  | getTypenameString "order"     = CONSORDER
  | getTypenameString "frag"      = CONSFRAG
  | getTypenameString _           = freshTypename ()


(**)

(* type constructor for a label *)
fun consTypenameVar lab = TYPE_CONSTRUCTOR (TYPENAME_VAR (freshTypenameVar ()),
			   SEQUENCE_VAR (freshSequenceVar ()),
			   lab)

(* constructs an implicit type var *)
fun consTYPE_VAR   tv = TYPE_VAR (tv, NONE, POLY)

(* constructs a sequence var *)
fun consSEQUENCE_VAR  sv  = SEQUENCE_VAR sv

(* constructs a row variable *)
fun consROW_VAR  rv  = ROW_VAR rv

(* constructs a function variable *)
fun consTYPE_FUNCTION_VAR tfv = TYPE_FUNCTION_VAR tfv

fun newTYPE_VAR   () = consTYPE_VAR   (freshTypeVar  ())
fun newROW_VAR  () = consROW_VAR  (freshRowVar ())
fun newSEQUENCE_VAR  () = consSEQUENCE_VAR  (freshSequenceVar ())
fun newTYPE_FUNCTION_VAR () = consTYPE_FUNCTION_VAR (freshTypeFunctionVar ())

(* check if parameter is a type construction *)
fun isTyC (TYPE_CONSTRUCTOR _)    = true
  | isTyC _                        = false

(* check if the parameter is an implicit type variable *)
fun isTyV (TYPE_VAR _)    = true
  | isTyV _        = false

(* gets type name modelled by the type constructor *)
fun getTypenameType (TYPE_CONSTRUCTOR (tn, _, _)) = SOME tn
  | getTypenameType  _             = NONE

(* returns labels from TyLab*)
fun getTyLab (TYPE_VAR  _)               = NONE
  | getTyLab (EXPLICIT_TYPE_VAR (_, _, l))       = SOME l
  | getTyLab (TYPE_CONSTRUCTOR  (_, _, l))       = SOME l
  | getTyLab (APPLICATION  (_, _, l))       = SOME l
  | getTyLab (TYPE_POLY (_, _, _, _, l)) = SOME l
  | getTyLab (GEN _)              = NONE
  | getTyLab (TYPE_DEPENDANCY  _)              = NONE


(* Extract the tyvar from a type.  The type as to be a type variable. *)
fun tyToTypeVar (TYPE_VAR (tv, _, _)) = tv
  | tyToTypeVar _ = raise EH.DeadBranch "the type should a var"
(* Extract the sequenceVariable from a type sequence.  The type sequence as to be a sequence variable.*)
fun seqToSequenceVar (SEQUENCE_VAR sv) = sv
  | seqToSequenceVar _ = raise EH.DeadBranch "the type sequence should be a var"


(**)

(* constructor functions *)
fun conslabty n lab = LC (Int.toString n, lab)

fun constuple tvl lab =
    let fun cons []          _ = []
	  | cons (tv :: tvl) n = (RC (conslabty n lab, consTYPE_VAR tv, lab)) :: (cons tvl (n + 1))
    in cons tvl 1
    end

fun consTupleTy tyl lab =
    #1 (foldl (fn (x, (xs, n)) => ((RC (conslabty n lab, x, lab)) :: xs, n+1)) ([], 1) tyl)

(* constructors with kinds *)
fun constyarrow' tv1 tv2 lab k = TYPE_CONSTRUCTOR (NC (CONSARROW, k, lab), SC (constuple [tv1, tv2] lab, noflex (), lab), lab)
fun constyrecord'  tvl f lab k = TYPE_CONSTRUCTOR (NC (CONSRECORD, k, lab), SC (map (fn x => ROW_VAR x) tvl, f, lab), lab)
fun constybool'          lab k = TYPE_CONSTRUCTOR (NC (CONSBOOL, k, lab), SC ([], noflex (), lab), lab)
fun constyint'           lab k = TYPE_CONSTRUCTOR (NC (CONSINT, k, lab), SC ([], noflex (), lab), lab)
fun constyword'          lab k = TYPE_CONSTRUCTOR (NC (CONSWORD, k, lab), SC ([], noflex (), lab), lab)
fun constyreal'          lab k = TYPE_CONSTRUCTOR (NC (CONSREAL, k, lab), SC ([], noflex (), lab), lab)
fun constychar'          lab k = TYPE_CONSTRUCTOR (NC (CONSCHAR, k, lab), SC ([], noflex (), lab), lab)
fun constystring'        lab k = TYPE_CONSTRUCTOR (NC (CONSSTRING, k, lab), SC ([], noflex (), lab), lab)
fun constyexception'     lab k = TYPE_CONSTRUCTOR (NC (CONSEXCEPTION, k, lab), SC ([], noflex (), lab), lab)
fun constyunit'          lab k = TYPE_CONSTRUCTOR (NC (CONSRECORD, k, lab), SC ([], noflex (), lab), lab)
fun constylist'       tv lab k = TYPE_CONSTRUCTOR (NC (CONSLIST, k, lab), SC (constuple [tv] lab, noflex (), lab), lab)
fun constyref'        tv lab k = TYPE_CONSTRUCTOR (NC (CONSREF, k, lab), SC (constuple [tv] lab, noflex (), lab), lab)
fun constytuple'     tvl lab k = TYPE_CONSTRUCTOR (NC (CONSRECORD, k, lab), SC (constuple tvl lab, noflex (), lab), lab)
fun constysubstring'     lab k = TYPE_CONSTRUCTOR (NC (CONSSUBSTRING, k, lab), SC ([], noflex (), lab), lab)
fun constyarray'      tv lab k = TYPE_CONSTRUCTOR (NC (CONSARRAY, k, lab), SC (constuple [tv] lab, noflex (), lab), lab)
fun constyvector'     tv lab k = TYPE_CONSTRUCTOR (NC (CONSVECTOR, k, lab), SC (constuple [tv] lab, noflex (), lab), lab)
fun constyoption'     tv lab k = TYPE_CONSTRUCTOR (NC (CONSOPTION, k, lab), SC (constuple [tv] lab, noflex (), lab), lab)
fun constyorder'         lab k = TYPE_CONSTRUCTOR (NC (CONSORDER, k, lab), SC ([], noflex (), lab), lab)
fun constyfrag'       tv lab k = TYPE_CONSTRUCTOR (NC (CONSFRAG, k, lab), SC (constuple [tv] lab, noflex (), lab), lab)
fun constynewcons'       lab k = TYPE_CONSTRUCTOR (NC (freshTypename   (), k, lab), SC ([], noflex (), lab), lab) (* a new constant type *)

(* constructors on types *)
fun consTyArrowTy ty1 ty2 lab k = TYPE_CONSTRUCTOR (NC (CONSARROW, k, lab), SC (consTupleTy [ty1, ty2] lab, noflex (), lab), lab)
fun consTyTupleTy     tyl lab k = TYPE_CONSTRUCTOR (NC (CONSRECORD, k, lab), SC (consTupleTy tyl lab, noflex (), lab), lab)

(* constructors without kinds *)
fun constyarrow tv1 tv2 lab = constyarrow' tv1 tv2 lab OTHER_CONS
fun constyrecord  tvl f lab = constyrecord'  tvl f lab OTHER_CONS (* the label option is for the flex *)
fun constybool          lab = constybool'          lab OTHER_CONS
fun constyint           lab = constyint'           lab OTHER_CONS
fun constyword          lab = constyword'          lab OTHER_CONS
fun constyreal          lab = constyreal'          lab OTHER_CONS
fun constychar          lab = constychar'          lab OTHER_CONS
fun constystring        lab = constystring'        lab OTHER_CONS
fun constyexception     lab = constyexception'     lab OTHER_CONS
fun constyunit          lab = constyunit'          lab OTHER_CONS
fun constylist       tv lab = constylist'       tv lab OTHER_CONS
fun constyref        tv lab = constyref'        tv lab OTHER_CONS
fun constytuple     tvl lab = constytuple'     tvl lab OTHER_CONS
fun constysubstring     lab = constysubstring'     lab OTHER_CONS
fun constyarray      tv lab = constyarray'      tv lab OTHER_CONS
fun constyvector     tv lab = constyvector'     tv lab OTHER_CONS
fun constyoption     tv lab = constyoption'     tv lab OTHER_CONS
fun constyorder         lab = constyorder'         lab OTHER_CONS
fun constyfrag       tv lab = constyfrag'       tv lab OTHER_CONS
fun constynewcons       lab = constynewcons'       lab OTHER_CONS

fun isBase' tn = tn < CONSTYPENAMESTART andalso tn > DUMMYTYPENAME
fun isBase  tn = tn = CONSARROW         orelse  tn = CONSRECORD
(*orelse (tn = CONSINT)*)

fun isBaseTy (TYPENAME_VAR _)          = false
  | isBaseTy (NC (tn, _, _)) = isBase tn
  | isBaseTy (TYPENAME_DEPENDANCY etn)        = isBaseTy (EL.getExtLabT etn)

fun isArrowTy (TYPENAME_VAR _)          = false
  | isArrowTy (NC (tn, _, _)) = tn = CONSARROW
  | isArrowTy (TYPENAME_DEPENDANCY etn)        = isArrowTy (EL.getExtLabT etn)

fun isExcTy (TYPENAME_VAR _)          = false
  | isExcTy (NC (tn, _, _)) = tn = CONSEXCEPTION
  | isExcTy (TYPENAME_DEPENDANCY etn)        = isExcTy (EL.getExtLabT etn)

fun isDecTy (NC (_, DECLARATION_CONS _, _)) = true
  | isDecTy (TYPENAME_DEPENDANCY etn)          = isDecTy (EL.getExtLabT etn)
  | isDecTy _                 = false

fun isPatTy (NC (_, PA, _)) = true
  | isPatTy (TYPENAME_DEPENDANCY etn)        = isPatTy (EL.getExtLabT etn)
  | isPatTy _               = false


(* Check is the top level constructor of a type has been generated for a builtin identifier.
 * If it is the case then 3 labels are updated. *)
(* We might have to do that in the rtls if the top label is the dummy one. *)
fun labelBuiltinTy' (TYPE_CONSTRUCTOR (tn, seq, _)) lab =
    (case (labelBuiltinTn tn lab, labelBuiltinSq seq lab) of
	 (SOME tn', SOME seq') => SOME (TYPE_CONSTRUCTOR (tn', seq', lab))
       | _ => NONE)
  | labelBuiltinTy' (TYPE_DEPENDANCY (ty, labs, stts, deps)) lab =
    (case labelBuiltinTy' ty lab of
	 SOME ty' => SOME (TYPE_DEPENDANCY (ty', labs, stts, deps))
       | NONE => NONE)
  | labelBuiltinTy' ty _ = NONE

(* re-label a type if its top-level type constructor is from the builtin basis *)
and labelBuiltinTy ty lab =
    (case labelBuiltinTy' ty lab of
	 SOME ty' => ty'
       | NONE => ty)
and labelBuiltinTn (NC (tn, BUILTIN_BASIS_CONS, _)) lab = SOME (NC (tn, OTHER_CONS, lab))
  | labelBuiltinTn (TYPENAME_DEPENDANCY (tn, labs, stts, deps)) lab =
    (case labelBuiltinTn tn lab of
	 SOME tn' => SOME (TYPENAME_DEPENDANCY (tn', labs, stts, deps))
       | NONE => NONE)
  | labelBuiltinTn (NC _) _ = NONE
  | labelBuiltinTn (TYPENAME_VAR _) _ = NONE
and labelBuiltinSq (SC (rows, flex, _)) lab = SOME (SC (labelBuiltinRowList rows lab, flex, lab))
  | labelBuiltinSq (SEQUENCE_DEPENDANCY (seq, labs, stts, deps)) lab =
    (case labelBuiltinSq seq lab of
	 SOME seq' => SOME (SEQUENCE_DEPENDANCY (seq', labs, stts, deps))
       | NONE => NONE)
  | labelBuiltinSq (SEQUENCE_VAR _) _ = NONE
and labelBuiltinRowList xs lab = map (fn x => labelBuiltinRow x lab) xs
and labelBuiltinRow (ROW_VAR x)           _   = ROW_VAR x
  | labelBuiltinRow (RC (lt, ty, _)) lab = RC (lt, labelBuiltinTy ty lab, lab)
  | labelBuiltinRow (ROW_DEPENDANCY erow)        lab = ROW_DEPENDANCY (EL.mapExtLab erow (fn row => labelBuiltinRow row lab))
  | labelBuiltinRow RO               _   = RO
and labelBuiltinTyf (TFC (seq, ty, l)) lab =
    (case (labelBuiltinSq seq lab, labelBuiltinTy' ty lab) of
	 (SOME seq', SOME ty') => TFC (seq', ty', lab)
       | _ => TFC (seq, ty, l))
  | labelBuiltinTyf (TYPE_FUNCTION_DEPENDANCY etf) lab = TYPE_FUNCTION_DEPENDANCY (EL.mapExtLab etf (fn tf => labelBuiltinTyf tf lab))
  | labelBuiltinTyf (TYPE_FUNCTION_VAR x) _ = (TYPE_FUNCTION_VAR x)

fun isVarTypename (TYPENAME_VAR _) = true
  | isVarTypename _ = false

fun isShallowRow (ROW_VAR _) = true
  | isShallowRow _ = false

fun isShallowSeq (SEQUENCE_VAR _)            = true
  | isShallowSeq (SC (rows, _, _)) = List.all (fn row => isShallowRow row) rows
  | isShallowSeq (SEQUENCE_DEPENDANCY eseq)         = isShallowSeq (EL.getExtLabT eseq)

fun decorateExtTypeVars set labs stts deps =
    S.map (fn (labs0, stts0, deps0) =>
	      (L.union labs labs0, L.union stts stts0, CD.union deps deps0))
	  set

fun unionExtTypeVars (set1, set2) =
    S.unionWith (fn ((labs1, stts1, deps1), (labs2, stts2, deps2)) =>
		    (L.union labs1 labs2, L.union stts1 stts2, CD.union deps1 deps2))
		(set1, set2)

fun getTypeVarsrowty (ROW_VAR _)                       = S.empty
  | getTypeVarsrowty (RC (_, ty, _))              = getTypeVarsty ty
  | getTypeVarsrowty (ROW_DEPENDANCY (row, labs, stts, deps)) = decorateExtTypeVars (getTypeVarsrowty row) labs stts deps
  | getTypeVarsrowty RO                           = S.empty
and getTypeVarstyseq (SEQUENCE_VAR _)                       = S.empty
  | getTypeVarstyseq (SC (rows, _, _))            = foldr (fn (row, tvs) => unionExtTypeVars (getTypeVarsrowty row, tvs)) S.empty rows
  | getTypeVarstyseq (SEQUENCE_DEPENDANCY (seq, labs, stts, deps)) = decorateExtTypeVars (getTypeVarstyseq seq) labs stts deps
and getTypeVarstytf  (TYPE_FUNCTION_VAR _)                      = S.empty
  | getTypeVarstytf  (TFC (sq, ty, _))            = unionExtTypeVars (getTypeVarstyseq sq, getTypeVarsty ty)
  | getTypeVarstytf  (TYPE_FUNCTION_DEPENDANCY etf)                    = getTypeVarstytf (EL.getExtLabT etf)
and getTypeVarsty    (TYPE_VAR  (v, _, _))               = S.insert (S.empty, v, (L.empty, L.empty, CD.empty))
  | getTypeVarsty    (EXPLICIT_TYPE_VAR (_, v, _))               = S.insert (S.empty, v, (L.empty, L.empty, CD.empty)) (*??*)
  | getTypeVarsty    (TYPE_CONSTRUCTOR  (_,  sq, _))             = getTypeVarstyseq sq
  | getTypeVarsty    (APPLICATION  (tf, sq, _))             = unionExtTypeVars (getTypeVarstytf tf, getTypeVarstyseq sq)
  | getTypeVarsty    (TYPE_POLY (sq, _, _, _, _))        = getTypeVarstyseq sq
  | getTypeVarsty    (GEN ty)                     = S.empty
  | getTypeVarsty    (TYPE_DEPENDANCY (ty, labs, stts, deps))  = decorateExtTypeVars (getTypeVarsty ty) labs stts deps
(*and getTypeVarstylist xs = foldr (fn (x, y) => (getTypeVarsty x) @ y) [] xs
and getTypeVarstyseqlist xs = foldr (fn (x, y) => (getTypeVarstyseq x) @ y) [] xs*)

(* getTypeVarsty is used by the unification algorithm to recompute
 * the monomorphic type variables.
 * This is at least used by enum/StateEnv.sml *)
fun getTypeVarsTy ty =
    S.foldri (fn (tv, (labs, stts, deps), list) =>
		 (tv, labs, stts, deps) :: list)
	     []
	     (getTypeVarsty ty)


(* Returns the actual type constructor of a typenameType *)
fun tntyToTyCon (TYPENAME_VAR tnv)        = DUMMYTYPENAME (* we could also return "tnv" but we don't care about the variable really *)
  | tntyToTyCon (NC (tn, _, _)) = tn
  | tntyToTyCon (TYPENAME_DEPENDANCY etn)        = tntyToTyCon (EL.getExtLabT etn)

(* determines if a parameter is of monomorphic or polymorphic type *)
fun isPoly POLY = true
  | isPoly MONO = false

(********* PRINTING SECTION **********)

fun printTypeVar     tv  = "t"   ^ Int.toString tv
fun printetyvar    tv  = "e"   ^ Int.toString tv (*printetyvar for print explicit type variable *)
fun printSequenceVar    sv  = "s"   ^ Int.toString sv
fun printTypenameVar tnv = "tnv" ^ Int.toString tnv
fun printTypename    tn  = "n"   ^ Int.toString tn
fun printRowVar    rv  = "r"   ^ Int.toString rv
fun printLabelVar    lv  = "f"   ^ Int.toString lv
fun printTypeFunctionVar    tfv = "tfv" ^ Int.toString tfv
fun printlabcons   lc  = lc
fun printlabel     l   = "l"   ^ L.printLab l
fun printsmllc     lc  = "\""  ^ lc ^ "\""
fun printsmltn     tn  = Int.toString tn

fun printTypename'   0   = "a new type name"
  | printTypename'   1   = "arrow"
  | printTypename'   2   = "record"
  | printTypename'   3   = "int"
  | printTypename'   4   = "word"
  | printTypename'   5   = "real"
  | printTypename'   6   = "bool"
  | printTypename'   7   = "string"
  | printTypename'   8   = "list"
  | printTypename'   9   = "ref"
  | printTypename'   10  = "char"
  | printTypename'   11  = "exception"
  | printTypename'   12  = "substring"
  | printTypename'   13  = "array"
  | printTypename'   14  = "vector"
  | printTypename'   15  = "option"
  | printTypename'   16  = "order"
  | printTypename'   17  = "frag"
  | printTypename'   _   = "a user type" (* should be a raise DeadBranch *)

fun printTypenameAssoc typename assoc =
    if typename >= CONSTYPENAMESTART
    then case I.lookupId (I.fromInt typename) assoc of
	     NONE => printTypename' typename
	   | SOME str => "a user type named " ^ str
    else printTypename' typename

fun printlabty (LABEL_VAR lv)      = printLabelVar lv
  | printlabty (LC (lc, l)) = "(" ^ printlabcons lc ^ "," ^ printlabel l ^ ")"
  | printlabty (LABEL_DEPENDANCY elt)     = "LD" ^ EL.printExtLab' elt printlabty

fun printlistgen xs f = "[" ^ #1 (foldr (fn (t, (s, c)) => (f t ^ c ^ s, ",")) ("", "") xs) ^ "]"

fun printRowVarlist xs = printlistgen xs printRowVar
fun printTypeVarList  xs = printlistgen xs printTypeVar
fun printSequenceVarList xs = printlistgen xs printSequenceVar

fun printKCons (DECLARATION_CONS id) = "DE(" ^ I.printId id ^ ")"
  | printKCons PATTERN_CONS      = "PATTERN"
  | printKCons OTHER_CONS      = "OTHER_CONS"
  | printKCons BUILTIN_BASIS_CONS      = "BUILTIN_BASIS_CONS"

fun printtnty (TYPENAME_VAR tnv)        = printTypenameVar tnv
  | printtnty (NC (tn, k, l)) = "(" ^ printTypename tn ^ "," ^ printKCons k ^ "," ^ printlabel l ^ ")"
  | printtnty (TYPENAME_DEPENDANCY etn)        = "ND" ^ EL.printExtLab' etn printtnty

fun printOp NONE     _ = "-"
  | printOp (SOME x) f = f x

fun printflex     x = printOp x printlabel
fun printExplicit x = printOp x I.printIdL

fun printidor i = Int.toString i

fun printPoly POLY = "POLY"
  | printPoly MONO = "MONO"

fun printOrKind (VALUE labelledId) = "VALUE(" ^ I.printIdL labelledId ^ ")"
  | printOrKind (CONSTANT (st, id, lab)) = "CONSTANT(" ^ st ^ "," ^ I.printId id ^ "," ^ L.printLab lab ^ ")"


(* functions to print internal types *)

fun printRowType (ROW_VAR rv)            = "ROW_VAR(" ^ printRowVar rv ^ ")"
  | printRowType (RC (lt, tv, l))   = "RC(" ^ printlabty lt ^
				    ":"   ^ printty    tv ^
				    ","   ^ printlabel l  ^ ")"
  | printRowType (ROW_DEPENDANCY erow)          = "ROW_DEPENDANCY"  ^ EL.printExtLab' erow printRowType
  | printRowType RO                 = "RO"
and printrowtylist xs             = printlistgen xs printRowType
and printseqty (SEQUENCE_VAR sv)            = "SEQUENCE_VAR(" ^ printSequenceVar     sv ^ ")"
  | printseqty (SC (rl, b, l))    = "SC("  ^ printrowtylist rl ^
				    ","    ^ printflex      b  ^
				    ","    ^ printlabel     l  ^ ")"
  | printseqty (SEQUENCE_DEPENDANCY eseq)          = "SD"   ^ EL.printExtLab' eseq printseqty
and printtyf (TYPE_FUNCTION_VAR v)              = "TYPE_FUNCTION_VAR(" ^ printTypeFunctionVar   v   ^ ")"
  | printtyf (TFC (sq, ty, l))    = "TFC(" ^ printseqty    sq  ^
				    ","    ^ printty       ty  ^
				    ","    ^ printlabel    l   ^ ")"
  | printtyf (TYPE_FUNCTION_DEPENDANCY etf)            = "TYPE_FUNCTION_DEPENDANCY"  ^ EL.printExtLab' etf printtyf
and printty (TYPE_VAR (v, b, p))         = "TYPE_VAR("   ^ printTypeVar    v   ^
				    ","    ^ printExplicit b   ^
				    ","    ^ printPoly     p   ^ ")"
  | printty (EXPLICIT_TYPE_VAR (id, tv, l))       = "E("   ^ I.printId     id  ^
				    ","    ^ printTypeVar    tv  ^
				    ","    ^ printlabel    l   ^ ")"
  | printty (TYPE_CONSTRUCTOR (tn, sq, l))       = "TYPE_CONSTRUCTOR("   ^ printtnty     tn  ^
				    ","    ^ printseqty    sq  ^
				    ","    ^ printlabel    l   ^ ")"
  | printty (APPLICATION (tf, sq, l))       = "A("   ^ printtyf      tf  ^
				    ","    ^ printseqty    sq  ^
				    ","    ^ printlabel    l   ^ ")"
  | printty (TYPE_POLY (sq, i, p, k, l)) = "TYPE_POLY("  ^ printseqty    sq  ^
				    ","    ^ printidor     i   ^
				    ","    ^ printPoly     p   ^
				    ","    ^ printOrKind   k   ^
				    ","    ^ printlabel    l   ^ ")"
  | printty (GEN tys)             = "GEN(" ^ printTyGen    tys ^ ")"
  | printty (TYPE_DEPENDANCY ety)              = "TYPE_DEPENDANCY"   ^ EL.printExtLab' ety printty
and printtylist    xs = printlistgen xs printty
and printseqtylist xs = printlistgen xs printseqty
and printTyGen tys = printtylist (!tys)


(* this printty is user friendly *)


fun printtnty' (TYPENAME_VAR tnv)            = "NV(" ^ printTypenameVar tnv ^ ")"
  | printtnty' (NC (tn, k, l))     = "NC(" ^ printTypename' tn ^
				     ","   ^ printKCons   k  ^
				     ","   ^ printlabel   l  ^ ")"
  | printtnty' (TYPENAME_DEPENDANCY etn)            = "ND"  ^ EL.printExtLab' etn printtnty'

fun printrowty' (ROW_VAR rv)            = "RV(" ^ printRowVar rv ^ ")"
  | printrowty' (RC (lt, tv, l))   = "RC(" ^ printlabty  lt ^
				     ":"   ^ printty'    tv ^
				     ","   ^ printlabel  l  ^ ")"
  | printrowty' (ROW_DEPENDANCY erow)          = "ROW_DEPENDANCY"  ^ EL.printExtLab' erow printrowty'
  | printrowty' RO                 = "RO"
and printrowtylist' xs = printlistgen xs printrowty'
and printseqty' (SEQUENCE_VAR sv)            = "SEQUENCE_VAR(" ^ printSequenceVar sv ^ ")"
  | printseqty' (SC (rtl, b, lab)) = "SC(" ^ printrowtylist' rtl ^
				     ","   ^ printflex       b   ^
				     ","   ^ printlabel      lab ^ ")"
  | printseqty' (SEQUENCE_DEPENDANCY eseq)          = "SD"  ^ EL.printExtLab' eseq printseqty'
and printtyf' (TYPE_FUNCTION_VAR v)              = "TYPE_FUNCTION_VAR("  ^ printTypeFunctionVar   v   ^ ")"
  | printtyf' (TFC (sq, ty, l))    = "TFC("  ^ printseqty'   sq  ^
				     ","     ^ printty'      ty  ^
				     ","     ^ printlabel    l   ^ ")"
  | printtyf' (TYPE_FUNCTION_DEPENDANCY etf)            = "TYPE_FUNCTION_DEPENDANCY"   ^ EL.printExtLab' etf printtyf'
and printty' (TYPE_VAR (v, b, p))         = "TYPE_VAR("    ^ printTypeVar    v   ^
				     ","     ^ printExplicit b   ^
				     ","     ^ printPoly     p   ^ ")"
  | printty' (EXPLICIT_TYPE_VAR (id, tv, l))       = "E("    ^ I.printId     id  ^
				     ","     ^ printTypeVar    tv  ^
				     ","     ^ printlabel    l   ^ ")"
  | printty' (TYPE_CONSTRUCTOR (tn, sq, l))       = "TYPE_CONSTRUCTOR (" ^ printtnty'    tn  ^
				     ","     ^ printseqty'   sq  ^
				     ","     ^ printlabel    l   ^ ")"
  | printty' (APPLICATION (tf, sq, l))       = "A("    ^ printtyf'     tf  ^
				     ","     ^ printseqty'   sq  ^
				     ","     ^ printlabel    l   ^ ")"
  | printty' (TYPE_POLY (sq, i, p, k, l)) = "TYPE_POLY("   ^ printseqty'   sq  ^
				     ","     ^ printidor     i   ^
				     ","     ^ printPoly     p   ^
				     ","     ^ printOrKind   k   ^
				     ","     ^ printlabel    l   ^ ")"
  | printty' (GEN tys)             = "GEN("  ^ printTyGen'  tys  ^ ")"
  | printty' (TYPE_DEPENDANCY ety)              = "TYPE_DEPENDANCY"    ^ EL.printExtLab' ety printty'
and printtylist' tys = printlistgen tys printty'
and printTyGen'  tys = printtylist' (!tys)

fun printAssoc  xs = printlistgen xs (fn (tv, st) => "(" ^ Int.toString tv ^ "," ^ st ^ ")")
fun printAssoc' xs = printlistgen xs (fn (tv, st) => "(" ^ Int.toString tv ^ "," ^ "\"" ^ st ^ "\"" ^ ")")

end
