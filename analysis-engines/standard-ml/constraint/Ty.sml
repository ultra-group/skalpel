(* Copyright 2009 2010 2011 2012 Heriot-Watt University
 * Copyright 2018 Christian Gregg
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

(** A type variable. *)
type typeVar         = int
(** A row variable. *)
type rowVar     = int
(** An equality type variable variable. *)
type equalityTypeVar     = int
(** A typename variable. *)
type typenameVar     = int
(** A label variable. *)
type labelVar        = int
(** A field variable. *)
type fieldVar          = int
(** A type function variable. *)
type typeFunctionVar = int
type eqType          = bool

(** int for typename but string for labcons because not in any environment *)
type typename     = int (* 1: arrow, 2: record, ... *)
(** Type of a field name for a record, a string. *)
type fieldName    = string
type idor       = int
(** If we have ... in a record then that makes it flexible, that's called a
 * flexible record pattern. Sometimes just called flex records, it's equivalent
 * to having all the missing fields there with wildcard for pattern inside the
 * field. Change the name of this so that it makes sense *)
type flex       = L.label option
(** Explicit type variable. *)
type extv       = I.labelledId option
(** The type of an association. *)
type assoc      = (int * string) list

(** Explicit type variable. *)
type explicitTypeVar = typeVar ExtLab.extLab

(** Used to reporesent polymorphism (POLY) or monomorphism (MONO).*)
datatype poly   = POLY
		| MONO

(** A datatype holding representing kinds of constructors, has 4 constructors.
 *
 * \arg DECLARATION_CONS: constructor from a declaration
 * \arg PATTERN_CONS: constructor from a pattern
 * \arg OTHER_CONS: other constructor
 * \arg BUILTIN_BASIS_CONS: We have this constructor because for type constructor labeled
 *                          by BUILTIN_BASIS. We might want to re-label them to get final points. *)
datatype constructorKind  = DECLARATION_CONS of Id.id
			  | PATTERN_CONS
			  | OTHER_CONS
			  | BUILTIN_BASIS_CONS

(* jpirie: figure out what the 'or' means in 'orKind' *)
datatype orKind = VALUE of Id.labelledId
		| CONSTANT of string * Id.id * Label.label

(* The Cs in the below should be replaced by CONSTRUCTION. So for example,
 * LV should be become LABEL_CONSTRUCTION *)

(** It is unclear why we need label vars (LABEL_VAR constructor). *)
datatype labelType = LABEL_VAR  of labelVar
		   | LC  of fieldName * Label.label
		   | LABEL_DEPENDANCY  of labelType ExtLab.extLab

(** Joe speculates that typenameType and tyfun could be simply merged.
 * This means the C and A cases of ty could also be merged.
 * Probably nothing wrong with leaving them separate as they are now.
 *
 * constructorKind in the NC constructor is used to know if the constructor
 * comes from a declaration (arrow type: datatype constructor with argument).
 * It is only used for the arrow type isn't it? *)
datatype typenameType = TYPENAME_VAR  of typenameVar
		      | NC  of typename * constructorKind * Label.label
		      | TYPENAME_DEPENDANCY  of typenameType ExtLab.extLab

(** FIELD_NO_OVERLOAD: marker to show that a field has been deleted from a row because
 * it is cannot be an overloaded type while a FIELD_VAR can *)
datatype fieldType = FIELD_VAR  of fieldVar
		   | FC  of labelType * ty * Label.label
		   | FIELD_DEPENDANCY  of fieldType ExtLab.extLab
		   | FIELD_NO_OVERLOAD

     (** A row type, either a row variable, a row constructor, or a row dependency. *)
     and rowType = ROW_VAR  of rowVar
		 | ROW_C  of fieldType list * flex  * Label.label (* flex is SOME _ if the record is flexible *)
		 | ROW_DEPENDANCY  of rowType ExtLab.extLab

     (** A type function, either a type function variable, a type function constructor, or a type function dependency. *)
     and typeFunction = TYPE_FUNCTION_VAR of typeFunctionVar
		      | TFC of rowType * ty * Label.label
		      | TYPE_FUNCTION_DEPENDANCY of typeFunction ExtLab.extLab

	 (** The ty datatype, used to represent a type.
	  *
	  * \arg TYPE_VAR: implicit type variable. The option (extv) is  NONE if
	  *                the variable does not come from an explicit type varaible
	  * \arg EXPLICIT_TYPE_VAR: explicit type variable. The second variable is
	  *                         the generalisation
	  * \arg TYPE_CONSTRUCTOR: type constructor
	  * \arg APPLICATION: type scheme instantiation (do we need this?)
	  * \arg TYPE_POLY: polymorphic type. the idor is so that an
				   jpirie: HUH? "or type" is unique
	  *            even after freshening. The id is so that we can in case of
	  *            an overloading type error, report the overloaded id.
	  * \arg GEN: intersection type
	  * \arg TYPE_DEPENDANCY: type annotated with dependancies *)

	 and ty = TYPE_VAR          of typeVar  * extv  * poly * equalityType
		| EXPLICIT_TYPE_VAR of Id.id  * typeVar * Label.label * equalityType
		| TYPE_CONSTRUCTOR  of typenameType   * rowType * Label.label * equalityType
		| APPLICATION       of typeFunction  * rowType * Label.label
		| TYPE_POLY         of rowType  * idor  * poly * orKind * Label.label * equalityType
		| GEN               of ty list ref
		| TYPE_DEPENDANCY   of ty ExtLab.extLab

	 (** A datatype to give the different status that we can be in when checking if something is an equality type
	  * EQUALITY_TYPE: definitely an equality type
	  * NOT_EQUALITY_TYPE: definitely not an equality type *)
	 and equalityTypeStatus = EQUALITY_TYPE
				| NOT_EQUALITY_TYPE
				| SIG_TYPE
				| UNKNOWN


	 (** Datatype for equality type status tracking.
	  * \arg EQUALITY_TYPE_VAR: an equality type variable, just a number
	  * \arg EQUALITY_TYPE_VAR_LIST: a list of equality type variables
	  * \arg EQUALITY_TYPE_STATUS: for when we know the equality type status of an expression
	  * \arg EQUALITY_TYPE_DEPENDANCY: allows us to hold e.g. labels when creating constraints
	  * \arg EQUALITY_TYPE_ON_TYPE: holds any of the ty constructors (allows constraining ty's
          *                        equality type vars to another equality type var *)
 	 and equalityType = EQUALITY_TYPE_VAR of equalityTypeVar
			  | EQUALITY_TYPE_VAR_LIST of equalityTypeVar list
			  | EQUALITY_TYPE_STATUS of equalityTypeStatus
			  | EQUALITY_TYPE_TYPENAME of equalityTypeVar list
			  | EQUALITY_TYPE_DEPENDANCY of equalityType ExtLab.extLab
			  | EQUALITY_TYPE_ON_TYPE of ty

(** Different type name constructs (typename, dummy, mabye typename, not type name). *)
datatype names = TYPENAME of typename | DUMTYPENAME of typename | MAYTYPENAME | NOTTYPENAME

(** List of top level type constructors (builtin type names).
 * When analysing the basis file, the first occurrence of such a type name
 * is the occurrence which is affected the internal type name as defined by
 * the getTypenameString function (see below). *)
val typenames = ["unit"     , "int"  , "word" , "real"  , "char", "string",
		 "substring", "exn"  , "array", "vector", "ref" , "bool"  ,
		 "option"   , "order", "list" , "frag"]

(** List of the overloading classes as they are called in the Definition
 * of SML.  These are hard coded in the parser. *)
val ovClasses = ["Int", "Word", "Real", "Char", "String"]

(** Used if a record is not flexible, returns NONE. *)
fun noflex   _ = NONE
(** Used if a record is flexible, built with a label.
 * \param The label responsible for record flexibility. *)
fun consflex l = SOME l
(** Returns true if a record is flexible, false otherwise. *)
fun isflex   f = Option.isSome f
(** Gets the label reponsible for the flexibility of a record. *)
fun getflex  f = Option.valOf f handle Option => raise unflex

(** Integer constant representing dummy type names. *)
val DUMMYTYPENAME        = 0
(** Integer constant representing an arrow type. *)
val CONSARROW          = 1
(** Integer constant representing a record type. *)
val CONSRECORD         = 2
(** Integer constant representing an integer type. *)
val CONSINT            = 3
(** Integer constant representing a word type. *)
val CONSWORD           = 4
(** Integer constant representing a real type. *)
val CONSREAL           = 5
(** Integer constant representing a boolean type. *)
val CONSBOOL           = 6
(** Integer constant representing a string type. *)
val CONSSTRING         = 7
(** Integer constant representing a list type. *)
val CONSLIST           = 8
(** Integer constant representing a ref type. *)
val CONSREF            = 9
(** Integer constant representing a char type. *)
val CONSCHAR           = 10
(** Integer constant representing an exception type. *)
val CONSEXCEPTION      = 11
(** Integer constant representing a substring type. *)
val CONSSUBSTRING      = 12
(** Integer constant representing an array type. *)
val CONSARRAY          = 13
(** Integer constant representing a vector type. *)
val CONSVECTOR         = 14
(** Integer constant representing an option type. *)
val CONSOPTION         = 15
(** Integer constant representing an order type. *)
val CONSORDER          = 16
(** Integer constant representing a frag type. *)
val CONSFRAG           = 17
val CONSTYPENAMESTART  = 18

(** Next type name variable *)
val nextTypenameVar        = ref 0
(** Next type variable *)
val nextTypeVar            = ref 0
(** Next row variable *)
val nextRowVar             = ref 0
(** Next equality type variable *)
val nextEqualityTypeVar    = ref 0
(** Next label variable *)
val nextLabelVar           = ref 0
(** Next field variable *)
val nextFieldVar           = ref 0
(** Next type function variable *)
val nextTypeFunctionVar    = ref 0
(** Next id variable *)
val nextidor               = ref 0
val nextTypename           = ref (CONSTYPENAMESTART)

(** Sets the ref values for 'next' variables to a specified value set in the argument. *)
fun setnexts n =
    let val _ = nextTypenameVar     := n
	val _ = nextTypeVar         := n
	val _ = nextRowVar          := n
	val _ = nextEqualityTypeVar := n
	val _ = nextFieldVar        := n
	val _ = nextLabelVar        := n
	val _ = nextTypeFunctionVar := n
	val _ = nextidor            := n
	val _ = nextTypename        := (CONSTYPENAMESTART)
    in ()
    end

(** Resets all 'next' ref values to 0. *)
fun resetnexts () = setnexts 0

(** Gets the next type variable. *)
fun getTypeVar ()         = !nextTypeVar
(** Gets the next row variable. *)
fun getrowVar ()          = !nextRowVar
(** Gets the next equality type variable. *)
fun getEqualityTypeVar () = !nextEqualityTypeVar
(** Gets the next field variable. *)
fun getFieldVar ()        = !nextFieldVar
(** Gets the next label variable. *)
fun getLabelVar ()        = !nextLabelVar
(** Gets the next type function variable. *)
fun getTypeFunctionVar () = !nextTypeFunctionVar
(** Gets the next 'idor' variable. *)
fun getidor ()            = !nextidor
(** Gets the next typename variable. *)
fun getTypenameVar ()     = !nextTypenameVar

(** Converts a type variable to an integer. *)
fun typeVarToInt          typeVar         = typeVar
(** Converts a type function variable to an integer. *)
fun typeFunctionVarToInt  typeFunctionVar = typeFunctionVar
(** Converts a label variable to an integer. *)
fun labelVarToInt         labelVar        = labelVar
(** Converts a field variable to an integer. *)
fun fieldVarToInt           fieldVar          = fieldVar
(** Converts a typename variable to an integer. *)
fun typenameVarToInt      typenameVar     = typenameVar
(** Converts a typename to an integer. *)
fun typenameToInt         typename          = typename
(** Converts a row variable to an integer. *)
fun rowVarToInt           rowVar     = rowVar
(** Converts an equality type variable to an integer. *)
fun equalityTypeVarToInt  equalityTypeVar     = equalityTypeVar
(** Converts an idor variable to an integer. *)
fun idorToInt             idor            = idor
(** Converts a typename to an integer. *)
fun typenameFromInt typename = typename

(** Tests for equality between two equality type variables. *)
fun eqTypeVar     tv1 tv2 = (tv1 = (tv2 : typeVar))
(** Tests for equality between two equality type function variables. *)
fun eqTypeFunctionVar    fv1 fv2 = (fv1 = (fv2 : typeFunctionVar))
(** Tests for equality between two row variables. *)
fun eqRowVar    sv1 sv2 = (sv1 = (sv2 : rowVar))
(** Tests for equality between two equality type variables. *)
fun eqEqualityTypeVar    eqtv1 eqtv2 = (eqtv1 = (eqtv2 : equalityTypeVar))
(** Tests for equality between two labels variables. *)
fun eqLabelVar    lv1 lv2 = (lv1 = (lv2 : labelVar))
(** Tests for equality between two field variables. *)
fun eqFieldVar    rv1 rv2 = (rv1 = (rv2 : fieldVar))
(** Tests for equality between two idor variables. *)
fun eqIdor      id1 id2 = (id1 = (id2 : idor))
(** Tests for equality between two typenames. *)
fun eqTypename    tn1 tn2 = (tn1 = (tn2 : typename))
(** Tests for equality between two typename variables. *)
fun eqTypenameVar nv1 nv2 = (nv1 = (nv2 : typenameVar))

(** Extract the name of a type name. *)
fun getNameTypenameTn (NC (name, _, _)) = SOME name
  | getNameTypenameTn (TYPENAME_DEPENDANCY exttn) = getNameTypenameTn (EL.getExtLabT exttn)
  | getNameTypenameTn _ = NONE

(** Extracts the typename of a type constructor. *)
fun getNameTypenameTy (TYPE_CONSTRUCTOR (tn, _, _, _)) = getNameTypenameTn tn
  | getNameTypenameTy (TYPE_DEPENDANCY extty) = getNameTypenameTy (EL.getExtLabT extty)
  | getNameTypenameTy _ = NONE


(** Extract the type name from a type function of the form:
 *   T.TFC (_, T.C (T.NC (name, _, _), _, _), _)
 * where we omit the dependencies. *)
fun getTypename (TFC (_, ty, _)) = getNameTypenameTy ty
  | getTypename (TYPE_FUNCTION_DEPENDANCY exttf) = getTypename (EL.getExtLabT exttf)
  | getTypename _ = NONE


(** Strips the dependencies off type functions. *)
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

(** Strips the dependencies off #ty values. *)
and stripDepsTy (ty as TYPE_VAR _) = (ty, L.empty, L.empty, CD.empty)
  | stripDepsTy (ty as EXPLICIT_TYPE_VAR _) = (ty, L.empty, L.empty, CD.empty)
  | stripDepsTy (ty as TYPE_CONSTRUCTOR(tn, sq, lab, _)) =
    let val (tn', labs1, stts1, deps1) = stripDepsTn tn
	val (sq', labs2, stts2, deps2) = stripDepsSq sq
    in (TYPE_CONSTRUCTOR (tn', sq', lab, EQUALITY_TYPE_STATUS(UNKNOWN)),
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
  | stripDepsTy (ty as TYPE_POLY (sq, id, p, k, lab, eq)) =
    let val (sq', labs, stts, deps) = stripDepsSq sq
    in (TYPE_POLY (sq', id, p, k, lab, eq), labs, stts, deps)
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

(** Strips the dependencies off a row type. *)
and stripDepsSq (sq as ROW_VAR _) = (sq, L.empty, L.empty, CD.empty)
  | stripDepsSq (sq as ROW_C (fields, flex, lab)) =
    let val (fields', labs, stts, deps) =
	    foldr (fn (field, (fields, labs, stts, deps)) =>
		      let val (field', labs', stts', deps') = stripDepsRt field
		      in (field' :: fields,
			  L.union  labs labs',
			  L.union  stts stts',
			  CD.union deps deps')
		      end)
		  ([], L.empty, L.empty, CD.empty)
		  fields
    in (ROW_C (fields', flex, lab), labs, stts, deps)
    end
  | stripDepsSq (sq as ROW_DEPENDANCY (sq1, labs1, stts1, deps1)) =
    let val (sq2, labs2, stts2, deps2) = stripDepsSq sq1
    in (sq2, L.union labs1 labs2, L.union stts1 stts2, CD.union deps1 deps2)
    end

(** Strips the dependencies from a field type. *)
and stripDepsRt (rt as FIELD_VAR _) = (rt, L.empty, L.empty, CD.empty)
  | stripDepsRt (rt as FC (lt, ty, lab)) =
    let val (lt', labs1, stts1, deps1) = stripDepsLt lt
	val (ty', labs2, stts2, deps2) = stripDepsTy ty
    in (FC (lt', ty', lab),
	L.union  labs1 labs2,
	L.union  stts1 stts2,
	CD.union deps1 deps2)
    end
  | stripDepsRt (rt as FIELD_DEPENDANCY (rt1, labs1, stts1, deps1)) =
    let val (rt2, labs2, stts2, deps2) = stripDepsRt rt1
    in (rt2, L.union labs1 labs2, L.union stts1 stts2, CD.union deps1 deps2)
    end
  | stripDepsRt RO = (RO, L.empty, L.empty, CD.empty)

(** Strips dependencies from a label type. *)
and stripDepsLt (lt as LABEL_VAR _) = (lt, L.empty, L.empty, CD.empty)
  | stripDepsLt (lt as LC _) = (lt, L.empty, L.empty, CD.empty)
  | stripDepsLt (lt as LABEL_DEPENDANCY (lt1, labs1, stts1, deps1)) =
    let val (lt2, labs2, stts2, deps2) = stripDepsLt lt1
    in (lt2, L.union labs1 labs2, L.union stts1 stts2, CD.union deps1 deps2)
    end

(** Strips dependencies from a typename type. *)
and stripDepsTn (tn as TYPENAME_VAR _) = (tn, L.empty, L.empty, CD.empty)
  | stripDepsTn (tn as NC _) = (tn, L.empty, L.empty, CD.empty)
  | stripDepsTn (tn as TYPENAME_DEPENDANCY (tn1, labs1, stts1, deps1)) =
    let val (tn2, labs2, stts2, deps2) = stripDepsTn tn1
    in (tn2, L.union labs1 labs2, L.union stts1 stts2, CD.union deps1 deps2)
    end

(** NONE means not a type name, SOME SOME tn means a type name tn, SOME NONE means we can't now.*)
fun isTypename tf =
    let val (tf', labs, stts, deps) = stripDepsTf tf
    in (isTypename' tf', labs, stts, deps)
    end

and isTypename' (TFC (sq1, TYPE_CONSTRUCTOR (NC (name, _, _), sq2, _, _), _)) =
    (case eqSeqTy sq1 sq2 of
	 SOME true  => TYPENAME name
       | SOME false => DUMTYPENAME name
       | NONE       => NOTTYPENAME)
  | isTypename' (TFC (sq1, TYPE_CONSTRUCTOR (_, sq2, _, _), _)) =
    (case eqSeqTy sq1 sq2 of
	 SOME _ => MAYTYPENAME
       | NONE   => NOTTYPENAME)
  | isTypename' (TFC (_, TYPE_VAR (_, SOME _, _, _), _)) = NOTTYPENAME
  | isTypename' _ = MAYTYPENAME

(** Tests for equality between row types. *)
and eqSeqTy (ROW_VAR sv1) (ROW_VAR sv2) =
    if eqRowVar sv1 sv2
    then SOME true
    else SOME false
  | eqSeqTy (ROW_C (fields1, _, _)) (ROW_C (fields2, _, _)) = eqFieldTys fields1 fields2
  | eqSeqTy _ _ = SOME false

(** Tests for equality between field variables by calling #eqFieldTy. *)
and eqFieldTys [] [] = SOME true
  | eqFieldTys (field1 :: fields1) (field2 :: fields2) =
    (case (eqFieldTy field1 field2, eqFieldTys fields1 fields2) of
	 (SOME b1, SOME b2) => SOME (b1 andalso b2) (*if we can't know for one then we can't know*)
       | _ => NONE)
  | eqFieldTys _ _ = NONE

(** Tests for equalit ybetween field variables. *)
and eqFieldTy (FIELD_VAR fv1) (FIELD_VAR fv2) =
    if eqFieldVar fv1 fv2
    then SOME true
    else SOME false
  | eqFieldTy (FC (lab1, ty1, _)) (FC (lab2, ty2, _)) =
    (case (eqLabTy lab1 lab2, eqTy ty1 ty2) of
	 (SOME b1, SOME b2) => SOME (b1 andalso b2) (*if we can't know for one then we can't know*)
       | _ => NONE)
  | eqFieldTy _ _ = SOME false

(** Checks for equality between two label types. *)
and eqLabTy (LABEL_VAR lv1) (LABEL_VAR lv2) =
    if eqLabelVar lv1 lv2
    then SOME true
    else SOME false
  | eqLabTy (LC (lc1, _)) (LC (lc2, _)) =
    if lc1 = lc2
    then SOME true
    else NONE
  | eqLabTy _ _ = SOME false

(** Checks for equality between two type variables by calling #eqTypeVar. *)
and eqTy (TYPE_VAR (tv1, _, _, _)) (TYPE_VAR (tv2, _, _, _)) =
    if eqTypeVar tv1 tv2
    then SOME true
    else SOME false
  | eqTy _ _ = NONE


(* Freshening *)

(** Increment a ref value given as a parameter *)
fun freshAVar avar = let val x = !avar in (avar := !avar + 1; x) end

(** Generates a fresh type variable. *)
fun freshTypeVar         () = freshAVar nextTypeVar
(** Generates a fresh row variable. *)
fun freshRowVar          () = freshAVar nextRowVar
(** Generates a fresh equality type variable. *)
fun freshEqualityTypeVar () = freshAVar nextEqualityTypeVar
(** Generates a fresh typename variable. *)
fun freshTypenameVar     () = freshAVar nextTypenameVar
(** Generates a fresh label variable. *)
fun freshLabelVar        () = freshAVar nextLabelVar
(** Generates a fresh field variable. *)
fun freshFieldVar        () = freshAVar nextFieldVar
(** Generates a fresh type function variable. *)
fun freshTypeFunctionVar () = freshAVar nextTypeFunctionVar
(** Generates a fresh typename variable. *)
fun freshTypename        () = freshAVar nextTypename
(** Generates a fresh idor variable. *)
fun freshidor            () = freshAVar nextidor

(** Takes a typename string and returns a datatype constructor of #typename. *)
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


(** A type constructor for a label.
 * Used in the binding of datatypes (f_datbind (A.DatBind...)) in the constraint generator. *)
fun consTypenameVar lab = TYPE_CONSTRUCTOR (TYPENAME_VAR (freshTypenameVar ()),
					    ROW_VAR (freshRowVar ()),
					    lab,
					    EQUALITY_TYPE_STATUS(UNKNOWN))

(** Constructs an equality type variable *)
fun consEQUALITY_TYPE_VAR eqtv = EQUALITY_TYPE_VAR eqtv
(** Constructs an equality type variable list *)
fun consEQUALITY_TYPE_VAR_LIST eqtvs = EQUALITY_TYPE_VAR_LIST eqtvs
(** Prints a type variable. *)
fun printTypeVar     tv  = "t"   ^ Int.toString tv
(** Constructs an implicit type variable. *)
fun consTYPE_VAR   tv = (TYPE_VAR (tv, NONE, POLY, EQUALITY_TYPE_STATUS(UNKNOWN)))
(** Constructs an implicit type var. *)
fun consTYPE_VARwithEQ   tv eqtv = TYPE_VAR (tv, NONE, POLY, eqtv)
(** Constructs a row variable. *)
fun consROW_VAR  rv  = ROW_VAR rv
(** Constructs a field variable. *)
fun consFIELD_VAR  fv  = FIELD_VAR fv
(** Constructs a function variable. *)
fun consTYPE_FUNCTION_VAR tfv = TYPE_FUNCTION_VAR tfv
(** Builds a new type variable. *)
fun newTYPE_VAR   () = consTYPE_VAR   (freshTypeVar  ())
(** Builds a new field variable. *)
fun newFIELD_VAR  () = consFIELD_VAR  (freshFieldVar ())
(** Builds a new row variable. *)
fun newROW_VAR  () = consROW_VAR  (freshRowVar ())
(** Builds a new type function variable. *)
fun newTYPE_FUNCTION_VAR () = consTYPE_FUNCTION_VAR (freshTypeFunctionVar ())

(** True if parameter is a type constructor, false otherwise. *)
fun isTyC (TYPE_CONSTRUCTOR _)    = true
  | isTyC _                        = false
(** True if the parameter is an implicit type variable. *)
fun isTyV (TYPE_VAR _)    = true
  | isTyV _        = false

(** Gets type name inside the type constructor. *)
fun getTypenameType (TYPE_CONSTRUCTOR (tn, _, _, _)) = SOME tn
  | getTypenameType  _             = NONE

(** Returns labels from a #ty. *)
fun getTyLab (TYPE_VAR  _)               = NONE
  | getTyLab (EXPLICIT_TYPE_VAR (_, _, l, _))       = SOME l
  | getTyLab (TYPE_CONSTRUCTOR  (_, _, l, _))       = SOME l
  | getTyLab (APPLICATION  (_, _, l))       = SOME l
  | getTyLab (TYPE_POLY (_, _, _, _, l, _)) = SOME l
  | getTyLab (GEN _)              = NONE
  | getTyLab (TYPE_DEPENDANCY  _)              = NONE


(** Extracts the tyvar from a type. The type as to be a type variable. *)
fun tyToTypeVar (TYPE_VAR (tv, _, _, _)) = tv
  | tyToTypeVar _ = raise EH.DeadBranch "the type should a var"
(** Extract the rowVariable from a type row.  The type row as to be a row variable. *)
fun seqToRowVar (ROW_VAR sv) = sv
  | seqToRowVar _ = raise EH.DeadBranch "the type row should be a var"

fun conslabty n lab = LC (Int.toString n, lab)

fun constuple tvl lab =
    let fun cons []          _ = []
	  | cons (tv :: tvl) n = (FC (conslabty n lab, consTYPE_VAR tv, lab)) :: (cons tvl (n + 1))
    in cons tvl 1
    end

fun constupleTyped tvl lab =
    let fun cons []          _ = []
	  | cons (tv :: tvl) n = (FC (conslabty n lab, tv, lab)) :: (cons tvl (n + 1))
    in cons tvl 1
    end

fun consTupleTy tyl lab =
    #1 (foldl (fn (x, (xs, n)) => ((FC (conslabty n lab, x, lab)) :: xs, n+1)) ([], 1) tyl)


(** Construct an arow type where tv1 and tv2 are already a TYPE_VAR. *)
fun constyarrow'Typed tv1 tv2 lab k = TYPE_CONSTRUCTOR (NC (CONSARROW, k, lab), ROW_C (constupleTyped [tv1, tv2] lab, noflex (), lab), lab, EQUALITY_TYPE_STATUS(UNKNOWN))
(** Construct an arow type where tv1 and tv2 are of type #typeVar. *)
fun constyarrow' tv1 tv2 lab k = TYPE_CONSTRUCTOR (NC (CONSARROW, k, lab), ROW_C (constuple [tv1, tv2] lab, noflex (), lab), lab, EQUALITY_TYPE_STATUS(NOT_EQUALITY_TYPE))
(** Construct an arrow type with equality type information attached. *)
fun constyarrow'Eq tv1 tv2 lab k eq = TYPE_CONSTRUCTOR (NC (CONSARROW, k, lab), ROW_C (constuple [tv1, tv2] lab, noflex (), lab), lab, eq)
(** Construct a record type. *)
fun constyrecord'  tvl f lab k = TYPE_CONSTRUCTOR (NC (CONSRECORD, k, lab), ROW_C (map (fn x => FIELD_VAR x) tvl, f, lab), lab, EQUALITY_TYPE_STATUS(UNKNOWN))
(** Construct a boolean type. *)
fun constybool'          lab k = TYPE_CONSTRUCTOR (NC (CONSBOOL, k, lab), ROW_C ([], noflex (), lab), lab, EQUALITY_TYPE_STATUS(UNKNOWN))
(** Construct an integer type. *)
fun constyint'           lab k = TYPE_CONSTRUCTOR (NC (CONSINT, k, lab), ROW_C ([], noflex (), lab), lab, EQUALITY_TYPE_STATUS(UNKNOWN))
(** Construct a word type. *)
fun constyword'          lab k = TYPE_CONSTRUCTOR (NC (CONSWORD, k, lab), ROW_C ([], noflex (), lab), lab, EQUALITY_TYPE_STATUS(UNKNOWN))
(** Construct a 'real' type. *)
fun constyreal'          lab k = TYPE_CONSTRUCTOR (NC (CONSREAL, k, lab), ROW_C ([], noflex (), lab), lab, EQUALITY_TYPE_STATUS(UNKNOWN))
(** Construct a char type.*)
fun constychar'          lab k = TYPE_CONSTRUCTOR (NC (CONSCHAR, k, lab), ROW_C ([], noflex (), lab), lab, EQUALITY_TYPE_STATUS(UNKNOWN))
(** Construct a string type. *)
fun constystring'        lab k = TYPE_CONSTRUCTOR (NC (CONSSTRING, k, lab), ROW_C ([], noflex (), lab), lab, EQUALITY_TYPE_STATUS(UNKNOWN))
(** Construct an exception type. *)
fun constyexception'     lab k = TYPE_CONSTRUCTOR (NC (CONSEXCEPTION, k, lab), ROW_C ([], noflex (), lab), lab, EQUALITY_TYPE_STATUS(UNKNOWN))
(** Construct a unit type. *)
fun constyunit'          lab k = TYPE_CONSTRUCTOR (NC (CONSRECORD, k, lab), ROW_C ([], noflex (), lab), lab, EQUALITY_TYPE_STATUS(UNKNOWN))
(** Construct a list type. *)
fun constylist'       tv lab k = TYPE_CONSTRUCTOR (NC (CONSLIST, k, lab), ROW_C (constuple [tv] lab, noflex (), lab), lab, EQUALITY_TYPE_STATUS(UNKNOWN))
(** Construct a ref type. *)
fun constyref'        tv lab k = TYPE_CONSTRUCTOR (NC (CONSREF, k, lab), ROW_C (constuple [tv] lab, noflex (), lab), lab, EQUALITY_TYPE_STATUS(UNKNOWN))
(** Construct a tuple type. *)
fun constytuple'     tvl lab k = TYPE_CONSTRUCTOR (NC (CONSRECORD, k, lab), ROW_C (constuple tvl lab, noflex (), lab), lab, EQUALITY_TYPE_STATUS(UNKNOWN))
(** Construct a tuple type where the type variable as an argument is already a TYPE_VAR. *)
fun constytuple'WithTheta tvl lab k = TYPE_CONSTRUCTOR (NC (CONSRECORD, k, lab), ROW_C (constupleTyped tvl lab, noflex (), lab), lab, EQUALITY_TYPE_STATUS(UNKNOWN))
(** Construct a tuple with equality type information attached. *)
fun constytuple'WithEquality     tvl eqtv lab k = TYPE_CONSTRUCTOR (NC (CONSRECORD, k, lab), ROW_C (constuple tvl lab, noflex (), lab), lab, eqtv)
(** Construct a substring type. *)
fun constysubstring'     lab k = TYPE_CONSTRUCTOR (NC (CONSSUBSTRING, k, lab), ROW_C ([], noflex (), lab), lab, EQUALITY_TYPE_STATUS(UNKNOWN))
(* Construct an array type. *)
fun constyarray'      tv lab k = TYPE_CONSTRUCTOR (NC (CONSARRAY, k, lab), ROW_C (constuple [tv] lab, noflex (), lab), lab, EQUALITY_TYPE_STATUS(UNKNOWN))
(** Construct a vector type. *)
fun constyvector'     tv lab k = TYPE_CONSTRUCTOR (NC (CONSVECTOR, k, lab), ROW_C (constuple [tv] lab, noflex (), lab), lab, EQUALITY_TYPE_STATUS(UNKNOWN))
(** Construct an option type. *)
fun constyoption'     tv lab k = TYPE_CONSTRUCTOR (NC (CONSOPTION, k, lab), ROW_C (constuple [tv] lab, noflex (), lab), lab, EQUALITY_TYPE_STATUS(UNKNOWN))
(** Construct an order type. *)
fun constyorder'         lab k = TYPE_CONSTRUCTOR (NC (CONSORDER, k, lab), ROW_C ([], noflex (), lab), lab, EQUALITY_TYPE_STATUS(UNKNOWN))
(** Construct a frag type. *)
fun constyfrag'       tv lab k = TYPE_CONSTRUCTOR (NC (CONSFRAG, k, lab), ROW_C (constuple [tv] lab, noflex (), lab), lab, EQUALITY_TYPE_STATUS(UNKNOWN))
(** Construct a new constructor type. *)
fun constynewcons'       lab k = TYPE_CONSTRUCTOR (NC (freshTypename   (), k, lab), ROW_C ([], noflex (), lab), lab, EQUALITY_TYPE_STATUS(UNKNOWN)) (* a new constant type *)

(** Construct an arrow type. *)
fun consTyArrowTy ty1 ty2 lab k = TYPE_CONSTRUCTOR (NC (CONSARROW, k, lab), ROW_C (consTupleTy [ty1, ty2] lab, noflex (), lab), lab, EQUALITY_TYPE_STATUS(UNKNOWN))
(** Construct a tuple type (record type). *)
fun consTyTupleTy     tyl lab k = TYPE_CONSTRUCTOR (NC (CONSRECORD, k, lab), ROW_C (consTupleTy tyl lab, noflex (), lab), lab, EQUALITY_TYPE_STATUS(UNKNOWN))

(** Constructs an arrow type without a constructor kind (OTHER_CONS constructor kind). *)
fun constyarrow tv1 tv2 lab = constyarrow' tv1 tv2 lab OTHER_CONS
(** Constructs an arrow type without a constructor kind where the tv values are already TYPE_VARs (OTHER_CONS constructor kind). *)
fun constyarrowTyped tv1 tv2 lab = constyarrow'Typed tv1 tv2 lab OTHER_CONS
(** Constructs a record type without a constructor kind (OTHER_CONS constructor kind). *)
fun constyrecord  tvl f lab = constyrecord'  tvl f lab OTHER_CONS (* the label option is for the flex *)
(** Constructs a boolean type without a constructor kind (OTHER_CONS constructor kind). *)
fun constybool          lab = constybool'          lab OTHER_CONS
(** Constructs an integer type without a constructor kind (OTHER_CONS constructor kind). *)
fun constyint           lab = constyint'           lab OTHER_CONS
(** Constructs a word type without a constructor kind (OTHER_CONS constructor kind). *)
fun constyword          lab = constyword'          lab OTHER_CONS
(** Constructs a real type without a constructor kind (OTHER_CONS constructor kind). *)
fun constyreal          lab = constyreal'          lab OTHER_CONS
(** Constructs a char type without a constructor kind (OTHER_CONS constructor kind). *)
fun constychar          lab = constychar'          lab OTHER_CONS
(** Constructs a string type without a constructor kind (OTHER_CONS constructor kind). *)
fun constystring        lab = constystring'        lab OTHER_CONS
(** Constructs a exception type without a constructor kind (OTHER_CONS constructor kind). *)
fun constyexception     lab = constyexception'     lab OTHER_CONS
(** Constructs a unit type without a constructor kind (OTHER_CONS constructor kind). *)
fun constyunit          lab = constyunit'          lab OTHER_CONS
(** Constructs a list type without a constructor kind (OTHER_CONS constructor kind). *)
fun constylist       tv lab = constylist'       tv lab OTHER_CONS
(** Constructs a ref type without a constructor kind (OTHER_CONS constructor kind). *)
fun constyref        tv lab = constyref'        tv lab OTHER_CONS
(** Constructs a tuple type without a constructor kind (OTHER_CONS constructor kind). *)
fun constytuple     tvl lab = constytuple'     tvl lab OTHER_CONS
(** Constructs a tuple type without a constructor kind with equality type information (OTHER_CONS constructor kind). *)
fun constytupleWithTheta  tvl lab = constytuple'WithTheta     tvl lab OTHER_CONS
(** Constructs an arrow type without a constructor kind with equality type information (OTHER_CONS constructor kind). *)
fun constytupleWithEquality tvl eq lab = constytuple'WithEquality     tvl eq lab OTHER_CONS
(** Constructs an arrow type without a constructor kind (OTHER_CONS constructor kind). *)
fun constysubstring     lab = constysubstring'     lab OTHER_CONS
(** Constructs an array type without a constructor kind (OTHER_CONS constructor kind). *)
fun constyarray      tv lab = constyarray'      tv lab OTHER_CONS
(** Constructs a vector type without a constructor kind (OTHER_CONS constructor kind). *)
fun constyvector     tv lab = constyvector'     tv lab OTHER_CONS
(** Constructs an option type without a constructor kind (OTHER_CONS constructor kind). *)
fun constyoption     tv lab = constyoption'     tv lab OTHER_CONS
(** Constructs an order type without a constructor kind (OTHER_CONS constructor kind). *)
fun constyorder         lab = constyorder'         lab OTHER_CONS
(** Constructs a frag type without a constructor kind (OTHER_CONS constructor kind). *)
fun constyfrag       tv lab = constyfrag'       tv lab OTHER_CONS
(** Constructs a new constructor type without a constructor kind (OTHER_CONS constructor kind). *)
fun constynewcons       lab = constynewcons'       lab OTHER_CONS

(** Returns true if the typename given in the argument is a base type. *)
fun isBase' tn = tn < CONSTYPENAMESTART andalso tn > DUMMYTYPENAME

(** Returns true if typename in the argument is a base type, false otherwise. *)
fun isBase  tn = tn = CONSARROW         orelse  tn = CONSRECORD
(*orelse (tn = CONSINT)*)

(** Returns true if the typename value given in the argument is a base type. *)
fun isBaseTy (TYPENAME_VAR _)          = false
  | isBaseTy (NC (tn, _, _)) = isBase tn
  | isBaseTy (TYPENAME_DEPENDANCY etn)        = isBaseTy (EL.getExtLabT etn)

(** Returns true if the typename value argument is an arrow type. *)
fun isArrowTy (TYPENAME_VAR _)          = false
  | isArrowTy (NC (tn, _, _)) = tn = CONSARROW
  | isArrowTy (TYPENAME_DEPENDANCY etn)        = isArrowTy (EL.getExtLabT etn)

(** Returns true if the typename value argument is an exception type. *)
fun isExcTy (TYPENAME_VAR _)          = false
  | isExcTy (NC (tn, _, _)) = tn = CONSEXCEPTION
  | isExcTy (TYPENAME_DEPENDANCY etn)        = isExcTy (EL.getExtLabT etn)

(** Returns true if the typename value argument is an declaration. *)
fun isDecTy (NC (_, DECLARATION_CONS _, _)) = true
  | isDecTy (TYPENAME_DEPENDANCY etn)          = isDecTy (EL.getExtLabT etn)
  | isDecTy _                 = false

(** Returns true if the typename value argument is a pattern. *)
fun isPatTy (NC (_, PA, _)) = true
  | isPatTy (TYPENAME_DEPENDANCY etn)        = isPatTy (EL.getExtLabT etn)
  | isPatTy _               = false


(* Check is the top level constructor of a type has been generated for a builtin identifier.
 * If it is the case then 3 labels are updated. *)
(* We might have to do that in the rtls if the top label is the dummy one. *)
fun labelBuiltinTy' (TYPE_CONSTRUCTOR (tn, seq, _, eq)) lab =
    (case (labelBuiltinTn tn lab, labelBuiltinSq seq lab) of
	 (SOME tn', SOME seq') => SOME (TYPE_CONSTRUCTOR (tn', seq', lab, eq))
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
and labelBuiltinSq (ROW_C (fields, flex, _)) lab = SOME (ROW_C (labelBuiltinFieldList fields lab, flex, lab))
  | labelBuiltinSq (ROW_DEPENDANCY (seq, labs, stts, deps)) lab =
    (case labelBuiltinSq seq lab of
	 SOME seq' => SOME (ROW_DEPENDANCY (seq', labs, stts, deps))
       | NONE => NONE)
  | labelBuiltinSq (ROW_VAR _) _ = NONE
and labelBuiltinFieldList xs lab = map (fn x => labelBuiltinField x lab) xs
and labelBuiltinField (FIELD_VAR x)           _   = FIELD_VAR x
  | labelBuiltinField (FC (lt, ty, _)) lab = FC (lt, labelBuiltinTy ty lab, lab)
  | labelBuiltinField (FIELD_DEPENDANCY efield)        lab = FIELD_DEPENDANCY (EL.mapExtLab efield (fn field => labelBuiltinField field lab))
  | labelBuiltinField RO               _   = RO
and labelBuiltinTyf (TFC (seq, ty, l)) lab =
    (case (labelBuiltinSq seq lab, labelBuiltinTy' ty lab) of
	 (SOME seq', SOME ty') => TFC (seq', ty', lab)
       | _ => TFC (seq, ty, l))
  | labelBuiltinTyf (TYPE_FUNCTION_DEPENDANCY etf) lab = TYPE_FUNCTION_DEPENDANCY (EL.mapExtLab etf (fn tf => labelBuiltinTyf tf lab))
  | labelBuiltinTyf (TYPE_FUNCTION_VAR x) _ = (TYPE_FUNCTION_VAR x)

(** True if the argument is a TYPENAME_VAR, false otherwise. *)
fun isVarTypename (TYPENAME_VAR _) = true
  | isVarTypename _ = false

(** True if the argument is a FIELD_VAR, false otherwise. *)
fun isShallowField (FIELD_VAR _) = true
  | isShallowField _ = false

fun isShallowSeq (ROW_VAR _)            = true
  | isShallowSeq (ROW_C (fields, _, _)) = List.all (fn field => isShallowField field) fields
  | isShallowSeq (ROW_DEPENDANCY eseq)         = isShallowSeq (EL.getExtLabT eseq)

fun decorateExtTypeVars set labs stts deps =
    S.map (fn (labs0, stts0, deps0) =>
	      (L.union labs labs0, L.union stts stts0, CD.union deps deps0))
	  set

fun unionExtTypeVars (set1, set2) =
    S.unionWith (fn ((labs1, stts1, deps1), (labs2, stts2, deps2)) =>
		    (L.union labs1 labs2, L.union stts1 stts2, CD.union deps1 deps2))
		(set1, set2)


(** Extracts the type variables from a field type. *)
fun getTypeVarsfieldty (FIELD_VAR _)                       = S.empty
  | getTypeVarsfieldty (FC (_, ty, _))              = getTypeVarsty ty
  | getTypeVarsfieldty (FIELD_DEPENDANCY (field, labs, stts, deps)) = decorateExtTypeVars (getTypeVarsfieldty field) labs stts deps
  | getTypeVarsfieldty FIELD_NO_OVERLOAD                           = S.empty
(** Extracts the type variables from a sequence type. *)
and getTypeVarstyseq (ROW_VAR _)                       = S.empty
  | getTypeVarstyseq (ROW_C (fields, _, _))            = foldr (fn (field, tvs) => unionExtTypeVars (getTypeVarsfieldty field, tvs)) S.empty fields
  | getTypeVarstyseq (ROW_DEPENDANCY (seq, labs, stts, deps)) = decorateExtTypeVars (getTypeVarstyseq seq) labs stts deps
(** Extracts type variables from a type function type value. *)
and getTypeVarstytf  (TYPE_FUNCTION_VAR _)                      = S.empty
  | getTypeVarstytf  (TFC (sq, ty, _))            = unionExtTypeVars (getTypeVarstyseq sq, getTypeVarsty ty)
  | getTypeVarstytf  (TYPE_FUNCTION_DEPENDANCY etf)                    = getTypeVarstytf (EL.getExtLabT etf)
(** Extracts types variables from a #ty value. *)
and getTypeVarsty    (TYPE_VAR  (v, _, _, _))               = S.insert (S.empty, v, (L.empty, L.empty, CD.empty))
  | getTypeVarsty    (EXPLICIT_TYPE_VAR (_, v, _, _))               = S.insert (S.empty, v, (L.empty, L.empty, CD.empty)) (*??*)
  | getTypeVarsty    (TYPE_CONSTRUCTOR  (_,  sq, _, _))             = getTypeVarstyseq sq
  | getTypeVarsty    (APPLICATION  (tf, sq, _))             = unionExtTypeVars (getTypeVarstytf tf, getTypeVarstyseq sq)
  | getTypeVarsty    (TYPE_POLY (sq, _, _, _, _, _))        = getTypeVarstyseq sq
  | getTypeVarsty    (GEN ty)                     = S.empty
  | getTypeVarsty    (TYPE_DEPENDANCY (ty, labs, stts, deps))  = decorateExtTypeVars (getTypeVarsty ty) labs stts deps
(** Used by the unification algorithm to recompute * the monomorphic type variables.
 *  This is at least used by enum/StateEnv.sml. *)
fun getTypeVarsTy ty =
    S.foldri (fn (tv, (labs, stts, deps), list) =>
		 (tv, labs, stts, deps) :: list)
	     []
	     (getTypeVarsty ty)


(** Returns the actual type constructor of a typenameType *)
fun tntyToTyCon (TYPENAME_VAR tnv)        = DUMMYTYPENAME (* we could also return "tnv" but we don't care about the variable really *)
  | tntyToTyCon (NC (tn, _, _)) = tn
  | tntyToTyCon (TYPENAME_DEPENDANCY etn)        = tntyToTyCon (EL.getExtLabT etn)

(* Determines if a parameter is of monomorphic (false) or polymorphic (true) type *)
fun isPoly POLY = true
  | isPoly MONO = false

(********* PRINTING SECTION **********)

(** Prints an explicit type variable. *)
fun printetyvar    tv  = "e"   ^ Int.toString tv (*printetyvar for print explicit type variable *)
(** Prints a row variable. *)
fun printRowVar    sv  = "s"   ^ Int.toString sv
(** Prints a typename variable. *)
fun printTypenameVar tnv = "tnv" ^ Int.toString tnv
(** Prints a typename variable. *)
fun printTypename    tn  = "n"   ^ Int.toString tn
(** Prints a field variable. *)
fun printFieldVar    rv  = "r"   ^ Int.toString rv
(** Prints a label variable. *)
fun printLabelVar    lv  = "f"   ^ Int.toString lv
(** Prints a type function variable. *)
fun printTypeFunctionVar    tfv = "tfv" ^ Int.toString tfv
(** Prints an equality type variable. *)
fun printEqualityTypeVar        eqtv = "eqtv" ^ Int.toString eqtv
(** Prints a list of equality type variables. *)
fun printEqualityTypeVarList       [] = ""
  | printEqualityTypeVarList      [h] = "eqtv" ^ Int.toString h
  | printEqualityTypeVarList   (h::t) = ("eqtv" ^ Int.toString h ^ ", ")^(printEqualityTypeVarList t)
(** Prints an equality type status. *)
fun printEqualityTypeStatus status =
    case status of
	EQUALITY_TYPE => "EQUALITY_TYPE"
      | NOT_EQUALITY_TYPE => "NOT_EQUALITY_TYPE"
      | SIG_TYPE => "SIG_TYPE"
      | UNKNOWN => "UNKNOWN"
(** Prints a field name. *)
fun printFieldName   fieldName  = fieldName
(** Prints a label. *)
fun printlabel     l   = "l"   ^ L.printLab l
(** Prints a labelled constructor? *)
fun printsmllc     lc  = "\""  ^ lc ^ "\""
fun printsmltn     tn  = Int.toString tn

(** Prints a typename. *)
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

(** Looks up the typename argument in the association map and prints it. *)
fun printTypenameAssoc typename assoc =
    if typename >= CONSTYPENAMESTART
    then case I.lookupId (I.fromInt typename) assoc of
	     NONE => printTypename' typename
	   | SOME str => "a user type named " ^ str
    else printTypename' typename

(** Pritns out a #labelType value. *)
fun printlabty (LABEL_VAR lv)      = printLabelVar lv
  | printlabty (LC (lc, l)) = "(" ^ printFieldName lc ^ "," ^ printlabel l ^ ")"
  | printlabty (LABEL_DEPENDANCY elt)     = "LD" ^ EL.printExtLab' elt printlabty

fun printlistgen xs f = "[" ^ #1 (foldr (fn (t, (s, c)) => (f t ^ c ^ s, ",")) ("", "") xs) ^ "]"

(** Prints a list of field variables. *)
fun printFieldVarlist xs = printlistgen xs printFieldVar
(** Prints a list of type variables. *)
fun printTypeVarList  xs = printlistgen xs printTypeVar
(** Prints a list of row variables. *)
fun printRowVarList xs = printlistgen xs printRowVar

(** Prints the kind of a constructor.
 *  KCons means "Kind of constructor" - this name should be changed. *)
fun printKCons (DECLARATION_CONS id) = "DECLARATION_CONS(" ^ I.printId id ^ ")"
  | printKCons PATTERN_CONS      = "PATTERN"
  | printKCons OTHER_CONS      = "OTHER_CONS"
  | printKCons BUILTIN_BASIS_CONS      = "BUILTIN_BASIS_CONS"

(** Prints out a #typenameType value. *)
fun printtnty (TYPENAME_VAR tnv)        = printTypenameVar tnv
  | printtnty (NC (tn, k, l)) = "(" ^ printTypename tn ^ "," ^ printKCons k ^ "," ^ printlabel l ^ ")"
  | printtnty (TYPENAME_DEPENDANCY etn)        = "TYPENAME_DEPENDANCY" ^ EL.printExtLab' etn printtnty

fun printOp NONE     _ = "-"
  | printOp (SOME x) f = f x

fun printflex     x = printOp x printlabel
fun printExplicit x = printOp x I.printIdL

fun printidor i = Int.toString i

(** Prints out a #poly constructor as a string. *)
fun printPoly POLY = "POLY"
  | printPoly MONO = "MONO"

fun printOrKind (VALUE labelledId) = "VALUE(" ^ I.printIdL labelledId ^ ")"
  | printOrKind (CONSTANT (st, id, lab)) = "CONSTANT(" ^ st ^ "," ^ I.printId id ^ "," ^ L.printLab lab ^ ")"

(** Prints a field type value. *)
fun printFieldType (FIELD_VAR rv)            = "FIELD_VAR(" ^ printFieldVar rv ^ ")"
  | printFieldType (FC (lt, tv, l))   = "FIELD_CONSTRUCTION(" ^ printlabty lt ^
				    ":"   ^ printty    tv ^
				    ","   ^ printlabel l  ^ ")"
  | printFieldType (FIELD_DEPENDANCY efield)          = "FIELD_DEPENDANCY"  ^ EL.printExtLab' efield printFieldType
  | printFieldType FIELD_NO_OVERLOAD               = "FIELD_NO_OVERLOAD"
(** Prints a list of filed type values. *)
and printfieldtylist xs             = printlistgen xs printFieldType

(** Prints a sequence type as a string. *)
and printseqty (ROW_VAR sv)            = "ROW_VAR(" ^ printRowVar     sv ^ ")"
  | printseqty (ROW_C (rl, b, l))    = "ROW_CONSTRUCTION("  ^ printfieldtylist rl ^
				    ","    ^ printflex      b  ^
				    ","    ^ printlabel     l  ^ ")"
  | printseqty (ROW_DEPENDANCY eseq)          = "ROW_DEPENDANCY"   ^ EL.printExtLab' eseq printseqty

(** Prints an #equalityType value. *)
and printEqualityType (EQUALITY_TYPE_VAR eqtv)    = "EQUALITY_TYPE_VAR(" ^ printEqualityTypeVar eqtv ^ ")"
  | printEqualityType (EQUALITY_TYPE_VAR_LIST eqtvs)    = "EQUALITY_TYPE_LIST(" ^ printEqualityTypeVarList eqtvs ^ ")"
  | printEqualityType (EQUALITY_TYPE_STATUS status) = "EQUALITY_TYPE_STATUS("  ^ (printEqualityTypeStatus status) ^")"
  | printEqualityType (EQUALITY_TYPE_DEPENDANCY dep) = "EQUALITY_TYPE_DEPENDANCY("  ^ (EL.printExtLab' dep printEqualityType) ^")"
  | printEqualityType (EQUALITY_TYPE_TYPENAME eqtvs) = "EQUALITY_TYPE_TYPENAME("  ^ printEqualityTypeVarList eqtvs ^ ")"
  | printEqualityType (EQUALITY_TYPE_ON_TYPE ty) = "EQUALITY_TYPE_ON_TYPE("  ^ (printty ty) ^")"

(** Prints out a type function value. *)
and printtyf (TYPE_FUNCTION_VAR v)              = "TYPE_FUNCTION_VAR(" ^ printTypeFunctionVar   v   ^ ")"
  | printtyf (TFC (sq, ty, l))    = "TFC(" ^ printseqty    sq  ^
				    ","    ^ printty       ty  ^
				    ","    ^ printlabel    l   ^ ")"
  | printtyf (TYPE_FUNCTION_DEPENDANCY etf)            = "TYPE_FUNCTION_DEPENDANCY"  ^ EL.printExtLab' etf printtyf

(** Prints out a #ty value. *)
and printty (TYPE_VAR (v, b, p, eqtv))         = "TYPE_VAR("   ^ printTypeVar    v   ^
						 ","    ^ printExplicit b   ^
						 ","    ^ printPoly     p   ^
						 ","   ^ printEqualityType eqtv   ^ ")"
  | printty (EXPLICIT_TYPE_VAR (id, tv, l, eqtv))       = "EXPLICIT_TYPE_VAR("   ^ I.printId     id  ^
				    ","    ^ printTypeVar    tv  ^
				    ","    ^ printlabel    l   ^
				    ","    ^ printEqualityType    eqtv   ^ ")"
  | printty (TYPE_CONSTRUCTOR (tn, sq, l, eq))       = "TYPE_CONSTRUCTOR("   ^ printtnty     tn  ^
				    ","    ^ printseqty    sq  ^
				    ","    ^ printlabel    l   ^
				    ","    ^ printEqualityType    eq   ^ ")"
  | printty (APPLICATION (tf, sq, l))       = "APPLICATION("   ^ printtyf      tf  ^
				    ","    ^ printseqty    sq  ^
				    ","    ^ printlabel    l   ^ ")"
  | printty (TYPE_POLY (sq, i, p, k, l, eq)) = "TYPE_POLY("  ^ printseqty    sq  ^
				    ","    ^ printidor     i   ^
				    ","    ^ printPoly     p   ^
				    ","    ^ printOrKind   k   ^
				    ","    ^ printlabel    l   ^
				    ","    ^ printEqualityType    eq   ^ ")"
  | printty (GEN tys)             = "GEN(" ^ printTyGen    tys ^ ")"
  | printty (TYPE_DEPENDANCY ety)              = "TYPE_DEPENDANCY"   ^ EL.printExtLab' ety printty

(** Prints out a list of #ty values. *)
and printtylist    xs = printlistgen xs printty
(** Prints out a list of sequence types. *)
and printseqtylist xs = printlistgen xs printseqty
and printTyGen tys = printtylist (!tys)

(** Prints out a typename type value. *)
fun printtnty' (TYPENAME_VAR tnv)            = "TYPENAME_VAR(" ^ printTypenameVar tnv ^ ")"
  | printtnty' (NC (tn, k, l))     = "TYPENAME_CONSTRUCTION(" ^ printTypename' tn ^
				     ","   ^ printKCons   k  ^
				     ","   ^ printlabel   l  ^ ")"
  | printtnty' (TYPENAME_DEPENDANCY etn)     = "TYPENAME_DEPENDANCY"  ^ EL.printExtLab' etn printtnty'

fun printfieldty' (FIELD_VAR rv)     = "FIELD_VAR(" ^ printFieldVar rv ^ ")"
  | printfieldty' (FC (lt, tv, l))   = "FIELD_CONSTRUCTION(" ^ printlabty  lt ^
				     ":"   ^ printty'    tv ^
				     ","   ^ printlabel  l  ^ ")"
  | printfieldty' (FIELD_DEPENDANCY efield)          = "FIELD_DEPENDANCY"  ^ EL.printExtLab' efield printfieldty'
  | printfieldty' FIELD_NO_OVERLOAD                  = "FIELD_NO_OVERLOAD"
and printfieldtylist' xs = printlistgen xs printfieldty'
and printseqty' (ROW_VAR sv)            = "ROW_VAR(" ^ printRowVar sv ^ ")"
  | printseqty' (ROW_C (rtl, b, lab))   = "ROW_CONSTRUCTION(" ^ printfieldtylist' rtl ^
					  ","   ^ printflex       b   ^
					  ","   ^ printlabel      lab ^ ")"
  | printseqty' (ROW_DEPENDANCY eseq)   = "ROW_DEPENDANCY"  ^ EL.printExtLab' eseq printseqty'
and printtyf' (TYPE_FUNCTION_VAR v)     = "TYPE_FUNCTION_VAR("  ^ printTypeFunctionVar   v   ^ ")"
  | printtyf' (TFC (sq, ty, l))         = "TYPE_FUNCTION_CONSTRUCTION("  ^ printseqty'   sq  ^
					  ","     ^ printty'      ty  ^
					  ","     ^ printlabel    l   ^ ")"
  | printtyf' (TYPE_FUNCTION_DEPENDANCY etf)            = "TYPE_FUNCTION_DEPENDANCY"   ^ EL.printExtLab' etf printtyf'
and printty' (TYPE_VAR (v, b, p, eqtv))         = "TYPE_VAR("    ^ printTypeVar    v   ^
						  ","     ^ printExplicit b   ^
 						  ","     ^ printPoly     p   ^
 						  ","     ^ printEqualityType eqtv   ^ ")"
  | printty' (EXPLICIT_TYPE_VAR (id, tv, l, eqtv))       = "EXPLICIT_TYPE_VAR("    ^ I.printId     id  ^
							   ","     ^ printTypeVar    tv  ^
							   ","     ^ printlabel    l ^
 							   ","     ^ printEqualityType eqtv   ^ ")"
  | printty' (TYPE_CONSTRUCTOR (tn, sq, l, eq))       = "TYPE_CONSTRUCTOR (" ^ printtnty'    tn  ^
				     ","     ^ printseqty'   sq  ^
				     ","     ^ printlabel    l   ^
				    ","    ^ printEqualityType    eq   ^ ")"
  | printty' (APPLICATION (tf, sq, l))       = "APPLICATION("    ^ printtyf'     tf  ^
				     ","     ^ printseqty'   sq  ^
				     ","     ^ printlabel    l   ^ ")"
  | printty' (TYPE_POLY (sq, i, p, k, l, eq)) = "TYPE_POLY("   ^ printseqty'   sq  ^
				     ","     ^ printidor     i   ^
				     ","     ^ printPoly     p   ^
				     ","     ^ printOrKind   k   ^
				     ","     ^ printlabel    l   ^
				     ","     ^ printEqualityType    eq   ^ ")"
  | printty' (GEN tys)             = "GEN("  ^ printTyGen'  tys  ^ ")"
  | printty' (TYPE_DEPENDANCY ety)              = "TYPE_DEPENDANCY"    ^ EL.printExtLab' ety printty'
and printtylist' tys = printlistgen tys printty'
and printTyGen'  tys = printtylist' (!tys)

(** Maps a function f over a list of lists and then joins all the lists together using the @ operator. *)
fun striplistgen xs f = foldr (op @) [] (List.map f xs)

(** Strips equality type variables from a field type. *)
fun stripEqualityVariables_fieldType (FIELD_VAR rv) labels   = ([], labels)
  | stripEqualityVariables_fieldType (FC (_, tv, label)) labels  = stripEqualityVariables tv (L.cons label labels)
  | stripEqualityVariables_fieldType (FIELD_DEPENDANCY (term,labs,_,_)) labels = stripEqualityVariables_fieldType term (L.union labs labels)
  | stripEqualityVariables_fieldType FIELD_NO_OVERLOAD labels = ([], labels)
(** Strips equality type variables from a sequence type. *)
and stripEqualityVariables_sequenceType (ROW_VAR _) labels = ([], labels)
  | stripEqualityVariables_sequenceType (ROW_C ([],_,label)) labels = ([], L.cons label labels)
  | stripEqualityVariables_sequenceType (ROW_C ((h::t),x,label)) labels =
    let
	val (eqTypeStatuses, eqTypeLabels) = stripEqualityVariables_fieldType h (L.cons label labels)
	val (nextEqTypeStatuses, nextEqTypeLabels) = stripEqualityVariables_sequenceType (ROW_C (t,x,label)) (L.cons label labels)
    in
	(eqTypeStatuses@nextEqTypeStatuses, L.union eqTypeLabels nextEqTypeLabels)
    end
	(* val (eqTypeStatuses, eqTypeLabels) = striplistgen rtl stripEqualityVariables_fieldType (L.cons label labels) *)
  | stripEqualityVariables_sequenceType (ROW_DEPENDANCY (term, labs,_,_)) labels = stripEqualityVariables_sequenceType term (L.union labs labels)

(** Strips equality type variables from a #ty value. *)
and stripEqualityVariables (typeVar as TYPE_VAR (tv, x, monoOrPoly, EQUALITY_TYPE_VAR eqtv)) labels =
    (D.printDebug D.TY D.CONSTRAINT_GENERATION (fn _ => "Stripping an equality type var from a TYPE_VAR: " ^ (printty (typeVar)));
     ([eqtv], labels))
  | stripEqualityVariables (typeVar as TYPE_VAR (tv, x, monoOrPoly, _)) labels =
    (D.printDebug D.TY D.CONSTRAINT_GENERATION (fn _ => "WARNING: No code to strip equality status from TYPE_VAR: " ^ (printty (typeVar)));
     ([], labels))
  | stripEqualityVariables (EXPLICIT_TYPE_VAR(explicitTypeVar)) labels =
    (D.printDebug D.TY D.CONSTRAINT_GENERATION (fn _ => "WARNING: No code to strip equality type var from EXPLICIT_TYPE_VAR: " ^ (printty (EXPLICIT_TYPE_VAR(explicitTypeVar))));
     ([], labels))
  | stripEqualityVariables (TYPE_CONSTRUCTOR (typename, sequenceType, label, EQUALITY_TYPE_VAR(eq))) labels =
    (D.printDebug D.TY D.CONSTRAINT_GENERATION (fn _ => "Stripping an equality type var from a TYPE_POLY ("^(printEqualityTypeVar eq)^")");
     ([eq], L.cons label labels))
  | stripEqualityVariables (TYPE_CONSTRUCTOR (typename, sequenceType, label, eq)) labels =
    let
	val (eqTypeStatuses, eqTypeLabels) = stripEqualityVariables_sequenceType sequenceType (L.cons label labels)
    in
	(eqTypeStatuses, eqTypeLabels)
    end
  | stripEqualityVariables (APPLICATION(_)) labels =
    (D.printDebug D.TY D.CONSTRAINT_GENERATION (fn _ => "WARNING: No code to strip equality type status from APPLICATION!");
     ([], labels))
  | stripEqualityVariables (TYPE_POLY(_,_,_,_,label,EQUALITY_TYPE_VAR(eq))) labels =
    (D.printDebug D.TY D.CONSTRAINT_GENERATION (fn _ => "Stripping an equality var from a TYPE_POLY ("^(printEqualityTypeVar eq)^")");
     ([eq], L.cons label labels))
  | stripEqualityVariables (TYPE_POLY(_,_,_,_,label,eq)) labels =
    (D.printDebug D.TY D.CONSTRAINT_GENERATION (fn _ => "(Not) stripping an equality type from a TYPE_POLY ("^(printEqualityType eq)^")");
     ([], labels))
  | stripEqualityVariables (GEN _) labels =
    (D.printDebug D.TY D.CONSTRAINT_GENERATION (fn _ => "WARNING: No code to strip equality type status from GEN!");
     ([], labels))
  | stripEqualityVariables (TYPE_DEPENDANCY (term, depLabels, _, _)) labels = stripEqualityVariables term (L.union depLabels labels)

and stripEqualityVariablesList _ = raise EH.TODO ""

val tab = "     ";
fun ppTyPair f (x,y) ind = ((f x ind))^((f y ind))

fun ppList ppFun [] ind = ""
 |  ppList ppFun (h::t) ind = (ppFun h ind)^(ppList ppFun t ind);

fun ppTyCon (TYPE_VAR (v, b, p, eqtv)) ind = ind ^ "TYPE_VAR("^(printTypeVar v)^", "^(printExplicit b)^", "^(printPoly p)^")\n"^(ppEqTyCon eqtv (ind^tab))
 |  ppTyCon (EXPLICIT_TYPE_VAR (id, tv, l, eqtv)) ind = ind ^ "EXPLICIT_TYPE_VAR(NYI)\n"
 |  ppTyCon (TYPE_CONSTRUCTOR (tn, sq, l, eq)) ind = ind ^ "TYPE_CONSTRUCTOR("^(L.printLab l)^")\n"^(ppTyNameTyCon tn (ind^tab))^(ppRowCon sq (ind^tab))^(ppEqTyCon eq (ind^tab))
 |  ppTyCon (APPLICATION (tf, sq, l)) ind = ind ^ "APPLICATION(NYI)\n"
 |  ppTyCon (TYPE_POLY (sq, i ,p, k, l, eq)) ind = ind ^ "TYPE_POLY(NYI)\n"
 |  ppTyCon (GEN tys) ind = ind ^ "GEN(NYI)\n"
 |  ppTyCon (TYPE_DEPENDANCY ety) ind = ind ^ "TYPE_DEPENDENCY("^(EL.ppExtLab ety (ind^tab) ppTyCon)

and ppTyNameTyCon (TYPENAME_VAR x) ind = ind ^ "TYPENAME_VAR(NYI)\n"
 |  ppTyNameTyCon (NC (tn, k, l)) ind = ind ^ "TYPENAME_CONSTRUCTION("^(printTypename' tn)^", "^(printKCons k)^", "^(L.printLab l)^")\n"
 |  ppTyNameTyCon (TYPENAME_DEPENDANCY x) ind = ind ^ "TYPENAME_DEPENDENCY(NYI)\n"

and ppRowCon (ROW_VAR x) ind = ind ^ "ROW_VAR("^(printRowVar x)^")\n"
 |  ppRowCon (ROW_C (rl, b, l)) ind = ind ^ "ROW_CONSTRUCTION("^(printflex b)^", "^(L.printLab l)^")\n"^(ppList ppFieldTy rl (ind^tab))
 |  ppRowCon (ROW_DEPENDANCY eseq) ind = ind ^ "ROW_DEPENDENCY(NYI)\n"

and ppFieldTy (FIELD_VAR x) ind = ind ^ "FIELV_VAR("^(printFieldVar x)^")\n"
 |  ppFieldTy (FC (lt, tv, l)) ind = ind ^ "FIELD_CONSTRUCTION("^(L.printLab l)^")\n"^(ppLabTy lt (ind^tab))^(ppTyCon tv (ind^tab))
 |  ppFieldTy (FIELD_DEPENDANCY x) ind = ind ^ "FIELD_DEPENDENCY(NYI)\n"
 |  ppFieldTy (FIELD_NO_OVERLOAD) ind = ind ^ "FIELD_NO_OVERLOAD\n"

and ppLabTy (LABEL_VAR x) ind = ind ^ "LABEL_VAR("^(printLabelVar x)^")\n"
 |  ppLabTy (LC (lc, l)) ind = ind ^ "LABEL_CONSTRUCTION("^(lc)^", "^(L.printLab l)^")\n"
 |  ppLabTy (LABEL_DEPENDANCY x) ind = ind ^ "LABEL_DEPENDENCY(NYI)\n"

and ppEqTyCon (EQUALITY_TYPE_VAR x) ind = ind ^ "EQUALITY_TYPE_VAR("^(printEqualityTypeVar x)^")\n"
 |  ppEqTyCon (EQUALITY_TYPE_VAR_LIST x) ind = ind ^"EQUALITY_TYPE_VAR_LIST("^(printEqualityTypeVarList x)^")\n"
 |  ppEqTyCon (EQUALITY_TYPE_STATUS x) ind = ind ^ "EQUALITY_TYPE_STATUS("^(printEqualityTypeStatus x)^")\n"
 |  ppEqTyCon (EQUALITY_TYPE_TYPENAME x) ind = ind ^ "EQUALITY_TYPE_TYPENAME("^(printEqualityTypeVarList x)^")\n"
 |  ppEqTyCon (EQUALITY_TYPE_DEPENDANCY x) ind = ind ^ "EQUALITY_TYPE_DEPENDANCY(NYI)\n"
 |  ppEqTyCon (EQUALITY_TYPE_ON_TYPE x) ind = ind ^ "EQUALITY_TYPE_ON_TYPE\n"^(ppTyCon x (ind^tab))

end
