(* Copyright 2009 Heriot-Watt University
 * Copyright 2010 Heriot-Watt University
 *
 *
 * This file is part of the ULTRA SML Type Error Slicer (SMLTES) -
 * a Type Error Slicer for Standard ML written by the ULTRA Group of
 * Heriot-Watt University, Edinburgh.
 *
 * SMLTES is a free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * SMLTES is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with SMLTES.  If not, see <http://www.gnu.org/licenses/>.
 *
 *  o Authors:     Vincent Rahli
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

(* new exception, raised if a non-option value is passed to getflex function *)
exception unflex

(* type and datatype declarations *)
type tyvar     = int
type seqvar    = int
type tynamevar = int
type labvar    = int
type rowvar    = int
type tyfvar    = int

type tyname     = int
type labcons    = string
type idor       = int
type flex       = L.label option
type extv       = I.idl option (* extv stands for EXplicit Type Variable*)
type assoc      = (int * string) list
type exttyvar   = tyvar ExtLab.extLab
datatype poly   = POLY | MONO
datatype kcons  = DE of I.id | PA | OT | BB
datatype orKind = VAL of I.idl
		| CST of string * I.id * L.label

datatype labty = LV  of labvar
	       | LC  of labcons            * L.label
	       | LD  of labty EL.extLab
datatype tnty  = NV  of tynamevar
	       | NC  of tyname     * kcons * L.label
	       | ND  of tnty EL.extLab
datatype rowty = RV  of rowvar
	       | RC  of labty      * ty    * L.label
	       | RD  of rowty EL.extLab
	       | RO
     and seqty = SV  of seqvar
	       | SC  of rowty list * flex  * L.label
	       | SD  of seqty EL.extLab
     and tyfun = TFV of tyfvar
	       | TFC of seqty      * ty    * L.label
	       | TFD of tyfun EL.extLab
     and ty    = V   of tyvar * extv  * poly
	       | E   of I.id  * tyvar * L.label
	       | C   of tnty  * seqty * L.label
	       | A   of tyfun * seqty * L.label
	       | OR  of seqty * idor  * poly * orKind * L.label
	       | GEN of ty list ref
	       | TD  of ty EL.extLab

datatype names = TYNAME of tyname | DUMTYNAME of tyname | MAYTYNAME | NOTTYNAME




(* List of builtin type names.
 * When analysing the basis file, the first occurrence of such a type name
 * is the occurrence which is affected the internal type name as defined by
 * the getTyNameString function (see below). *)
val tyNames = ["unit"     , "int"  , "word" , "real"  , "char", "string",
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

fun dummytyname       () = 0
fun consarrow         () = 1
fun consrecord        () = 2
fun consint           () = 3
fun consword          () = 4
fun consreal          () = 5
fun consbool          () = 6
fun consstring        () = 7
fun conslist          () = 8
fun consref           () = 9
fun conschar          () = 10
fun consexception     () = 11
fun conssubstring     () = 12
fun consarray         () = 13
fun consvector        () = 14
fun consoption        () = 15
fun consorder         () = 16
fun consfrag          () = 17
fun constypenamestart () = 18

val nexttynamevar = ref 0
val nexttyvar     = ref 0
val nextseqvar    = ref 0
val nextlabvar    = ref 0
val nextrowvar    = ref 0
val nexttyfvar    = ref 0
val nextidor      = ref 0
val nexttyname    = ref (constypenamestart ())

(* sets the above ref values to a value n *)
fun setnexts n =
    let val _ = nexttynamevar := n
	val _ = nexttyvar     := n
	val _ = nextseqvar    := n
	val _ = nextrowvar    := n
	val _ = nextlabvar    := n
	val _ = nexttyfvar    := n
	val _ = nextidor      := n
	val _ = nexttyname    := (constypenamestart ())
    in ()
    end

(* resets all ref values above to 0 *)
fun resetnexts () = setnexts 0

(* accessor methods *)
fun gettyvar     () = !nexttyvar
fun getseqvar    () = !nextseqvar
fun getrowvar    () = !nextrowvar
fun getlabvar    () = !nextlabvar
fun gettyfvar    () = !nexttyfvar
fun getidor      () = !nextidor
fun gettynamevar () = !nexttynamevar

(* funtions to cast defined types to integers *)
fun tyvarToInt     tyvar     = tyvar
fun tyfvarToInt    tyfvar    = tyfvar
fun labvarToInt    labvar    = labvar
fun rowvarToInt    rowvar    = rowvar
fun tynamevarToInt tynamevar = tynamevar
fun tynameToInt    tyname    = tyname
fun seqvarToInt    seqvar    = seqvar
fun idorToInt      idor      = idor

fun tynameFromInt tyname = tyname

(* equality testing for defined types *)
fun eqTyvar     tv1 tv2 = (tv1 = (tv2 : tyvar))
fun eqTyfvar    fv1 fv2 = (fv1 = (fv2 : tyfvar))
fun eqSeqvar    sv1 sv2 = (sv1 = (sv2 : seqvar))
fun eqLabvar    lv1 lv2 = (lv1 = (lv2 : labvar))
fun eqRowvar    rv1 rv2 = (rv1 = (rv2 : rowvar))
fun eqIdor      id1 id2 = (id1 = (id2 : idor))
fun eqTyname    tn1 tn2 = (tn1 = (tn2 : tyname))
fun eqTynamevar nv1 nv2 = (nv1 = (nv2 : tynamevar))

(*fun eqSeqTy (SV sv1) (SV sv2) = eqSeqvar sv1 sv2
  | eqSeqTy (SC (rows1, _, _)) (SC (rows2, _, _)) = eqRowTys rows1 rows2
  | eqSeqTy _ _ = false

and eqRowTys [] [] = true
  | eqRowTys (row1 :: rows1) (row2 :: rows2) = eqRowTy row1 row2 andalso eqRowTys rows1 rows2
  | eqRowTys _ _ = false

and eqRowTy (RV rv1) (RV rv2) = eqRowvar rv1 rv2
  | eqRowTy (RC (lab1, ty1, _)) (RC (lab2, ty2, _)) = eqLabTy lab1 lab2 andalso eqTy ty1 ty2
  | eqRowTy _ _ = false

and eqLabTy (LV lv1) (LV lv2) = eqLabvar lv1 lv2
  | eqLabTy (LC (lc1, _)) (LC (lc2, _)) = lc1 = lc2
  | eqLabTy _ _ = false

and eqTy (V (tv1, _, _)) (V (tv2, _, _)) = eqTyvar tv1 tv2
  | eqTy _ _ = raise EH.DeadBranch ""*)


(* Extract the name of a type name *)

fun getNameTyNameTn (NC (name, _, _)) = SOME name
  | getNameTyNameTn (ND exttn) = getNameTyNameTn (EL.getExtLabT exttn)
  | getNameTyNameTn _ = NONE

fun getNameTyNameTy (C (tn, _, _)) = getNameTyNameTn tn
  | getNameTyNameTy (TD extty) = getNameTyNameTy (EL.getExtLabT extty)
  | getNameTyNameTy _ = NONE

fun getNameTyName (TFC (_, ty, _)) = getNameTyNameTy ty
  | getNameTyName (TFD exttf) = getNameTyName (EL.getExtLabT exttf)
  | getNameTyName _ = NONE


(* Strip dependencies off types *)

fun stripDepsTf (tf as TFD (tf1, labs1, stts1, deps1)) =
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
  | stripDepsTf (tf as TFV _) =
    (tf, L.empty, L.empty, CD.empty)

and stripDepsTy (ty as V _) = (ty, L.empty, L.empty, CD.empty)
  | stripDepsTy (ty as E _) = (ty, L.empty, L.empty, CD.empty)
  | stripDepsTy (ty as C (tn, sq, lab)) =
    let val (tn', labs1, stts1, deps1) = stripDepsTn tn
	val (sq', labs2, stts2, deps2) = stripDepsSq sq
    in (C (tn', sq', lab),
	L.union  labs1 labs2,
	L.union  stts1 stts2,
	CD.union deps1 deps2)
    end
  | stripDepsTy (ty as A (tf, sq, lab)) =
    let val (tf', labs1, stts1, deps1) = stripDepsTf tf
	val (sq', labs2, stts2, deps2) = stripDepsSq sq
    in (A (tf', sq', lab),
	L.union  labs1 labs2,
	L.union  stts1 stts2,
	CD.union deps1 deps2)
    end
  | stripDepsTy (ty as OR (sq, id, p, k, lab)) =
    let val (sq', labs, stts, deps) = stripDepsSq sq
    in (OR (sq', id, p, k, lab), labs, stts, deps)
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
  | stripDepsTy (ty as TD (ty1, labs1, stts1, deps1)) =
    let val (ty2, labs2, stts2, deps2) = stripDepsTy ty1
    in (ty2, L.union labs1 labs2, L.union stts1 stts2, CD.union deps1 deps2)
    end

and stripDepsSq (sq as SV _) = (sq, L.empty, L.empty, CD.empty)
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
  | stripDepsSq (sq as SD (sq1, labs1, stts1, deps1)) =
    let val (sq2, labs2, stts2, deps2) = stripDepsSq sq1
    in (sq2, L.union labs1 labs2, L.union stts1 stts2, CD.union deps1 deps2)
    end

and stripDepsRt (rt as RV _) = (rt, L.empty, L.empty, CD.empty)
  | stripDepsRt (rt as RC (lt, ty, lab)) =
    let val (lt', labs1, stts1, deps1) = stripDepsLt lt
	val (ty', labs2, stts2, deps2) = stripDepsTy ty
    in (RC (lt', ty', lab),
	L.union  labs1 labs2,
	L.union  stts1 stts2,
	CD.union deps1 deps2)
    end
  | stripDepsRt (rt as RD (rt1, labs1, stts1, deps1)) =
    let val (rt2, labs2, stts2, deps2) = stripDepsRt rt1
    in (rt2, L.union labs1 labs2, L.union stts1 stts2, CD.union deps1 deps2)
    end
  | stripDepsRt RO = (RO, L.empty, L.empty, CD.empty)

and stripDepsLt (lt as LV _) = (lt, L.empty, L.empty, CD.empty)
  | stripDepsLt (lt as LC _) = (lt, L.empty, L.empty, CD.empty)
  | stripDepsLt (lt as LD (lt1, labs1, stts1, deps1)) =
    let val (lt2, labs2, stts2, deps2) = stripDepsLt lt1
    in (lt2, L.union labs1 labs2, L.union stts1 stts2, CD.union deps1 deps2)
    end

and stripDepsTn (tn as NV _) = (tn, L.empty, L.empty, CD.empty)
  | stripDepsTn (tn as NC _) = (tn, L.empty, L.empty, CD.empty)
  | stripDepsTn (tn as ND (tn1, labs1, stts1, deps1)) =
    let val (tn2, labs2, stts2, deps2) = stripDepsTn tn1
    in (tn2, L.union labs1 labs2, L.union stts1 stts2, CD.union deps1 deps2)
    end


(* NONE means not a type name, SOME SOME tn means a type name tn, SOME NONE means we can't now.*)

fun isTyName tf =
    let val (tf', labs, stts, deps) = stripDepsTf tf
    in (isTyName' tf', labs, stts, deps)
    end

and isTyName' (TFC (sq1, C (NC (name, _, _), sq2, _), _)) =
    (case eqSeqTy sq1 sq2 of
	 SOME true  => TYNAME name
       | SOME false => DUMTYNAME name
       | NONE       => NOTTYNAME)
  | isTyName' (TFC (sq1, C (_, sq2, _), _)) =
    (case eqSeqTy sq1 sq2 of
	 SOME _ => MAYTYNAME
       | NONE   => NOTTYNAME)
  | isTyName' (TFC (_, V (_, SOME _, _), _)) = NOTTYNAME
  | isTyName' _ = MAYTYNAME

and eqSeqTy (SV sv1) (SV sv2) =
    if eqSeqvar sv1 sv2
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

and eqRowTy (RV rv1) (RV rv2) =
    if eqRowvar rv1 rv2
    then SOME true
    else SOME false
  | eqRowTy (RC (lab1, ty1, _)) (RC (lab2, ty2, _)) =
    (case (eqLabTy lab1 lab2, eqTy ty1 ty2) of
	 (SOME b1, SOME b2) => SOME (b1 andalso b2) (*if we can't know for one then we can't know*)
       | _ => NONE)
  | eqRowTy _ _ = SOME false

and eqLabTy (LV lv1) (LV lv2) =
    if eqLabvar lv1 lv2
    then SOME true
    else SOME false
  | eqLabTy (LC (lc1, _)) (LC (lc2, _)) =
    if lc1 = lc2
    then SOME true
    else NONE
  | eqLabTy _ _ = SOME false

and eqTy (V (tv1, _, _)) (V (tv2, _, _)) =
    if eqTyvar tv1 tv2
    then SOME true
    else SOME false
  | eqTy _ _ = NONE


(* Freshening *)

(* increment a ref value given as a parameter *)
fun freshAVar avar =
    let val x = !avar
    in (avar := !avar + 1; x)
    end

(* increments the ref associated with the function name *)
fun freshtyvar     () = freshAVar nexttyvar
fun freshseqvar    () = freshAVar nextseqvar
fun freshtynamevar () = freshAVar nexttynamevar
fun freshlabvar    () = freshAVar nextlabvar
fun freshrowvar    () = freshAVar nextrowvar
fun freshtyfvar    () = freshAVar nexttyfvar
fun freshtyname    () = freshAVar nexttyname
fun freshidor      () = freshAVar nextidor

fun getTyNameString "unit"      = consrecord    ()
  | getTyNameString "int"       = consint       ()
  | getTyNameString "word"      = consword      ()
  | getTyNameString "real"      = consreal      ()
  | getTyNameString "bool"      = consbool      ()
  | getTyNameString "string"    = consstring    ()
  | getTyNameString "list"      = conslist      ()
  | getTyNameString "ref"       = consref       ()
  | getTyNameString "char"      = conschar      ()
  | getTyNameString "exn"       = consexception ()
  | getTyNameString "substring" = conssubstring ()
  | getTyNameString "array"     = consarray     ()
  | getTyNameString "vector"    = consvector    ()
  | getTyNameString "option"    = consoption    ()
  | getTyNameString "order"     = consorder     ()
  | getTyNameString "frag"      = consfrag      ()
  | getTyNameString _           = freshtyname   ()


(**)

(* type constructor for a label *)
fun consTyNameVar lab = C (NV (freshtynamevar ()),
			   SV (freshseqvar ()),
			   lab)

(* constructs an implicit type variable *)
fun consV   tv  = V (tv, NONE, POLY)

(* constructs a sequence variable *)
fun consSV  sv  = SV sv

(* constructs a row variable *)
fun consRV  rv  = RV rv

(* constructs a function variable *)
fun consTFV tfv = TFV tfv

fun newV   () = consV   (freshtyvar  ())
fun newRV  () = consRV  (freshrowvar ())
fun newSV  () = consSV  (freshseqvar ())
fun newTFV () = consTFV (freshtyfvar ())

(* check if parameter is a type construction *)
fun isTyC (C _)    = true
  | isTyC _        = false

(* check if the parameter is an implicit type variable *)
fun isTyV (V _)    = true
  | isTyV _        = false

(* gets type name modelled by the type constructor *)
fun getTyName (C (tn, _, _)) = SOME tn
  | getTyName  _             = NONE

(* returns labels from TyLab*)
fun getTyLab (V  _)               = NONE
  | getTyLab (E  (_, _, l))       = SOME l
  | getTyLab (C  (_, _, l))       = SOME l
  | getTyLab (A  (_, _, l))       = SOME l
  | getTyLab (OR (_, _, _, _, l)) = SOME l
  | getTyLab (GEN _)              = NONE
  | getTyLab (TD  _)              = NONE


(* Extract the tyvar from a type.  The type as to be a type variable. *)
fun tyToTyvar (V (tv, _, _)) = tv
  | tyToTyvar _ = raise EH.DeadBranch "the type should a variable"
(* Extract the seqvar from a type sequence.  The type sequence as to be a sequence variable.*)
fun seqToSeqvar (SV sv) = sv
  | seqToSeqvar _ = raise EH.DeadBranch "the type sequence should be a variable"


(**)

(* constructor functions *)
fun conslabty n lab = LC (Int.toString n, lab)

fun constuple tvl lab =
    let fun cons []          _ = []
	  | cons (tv :: tvl) n = (RC (conslabty n lab, consV tv, lab)) :: (cons tvl (n + 1))
    in cons tvl 1
    end

fun consTupleTy tyl lab =
    #1 (foldl (fn (x, (xs, n)) => ((RC (conslabty n lab, x, lab)) :: xs, n+1)) ([], 1) tyl)

fun constyarrow' tv1 tv2 lab k = C (NC (consarrow     (), k, lab), SC (constuple [tv1, tv2] lab, noflex (), lab), lab)
fun constyrecord'  tvl f lab k = C (NC (consrecord    (), k, lab), SC (map (fn x => RV x) tvl, f, lab), lab)
fun constybool'          lab k = C (NC (consbool      (), k, lab), SC ([], noflex (), lab), lab)
fun constyint'           lab k = C (NC (consint       (), k, lab), SC ([], noflex (), lab), lab)
fun constyword'          lab k = C (NC (consword      (), k, lab), SC ([], noflex (), lab), lab)
fun constyreal'          lab k = C (NC (consreal      (), k, lab), SC ([], noflex (), lab), lab)
fun constychar'          lab k = C (NC (conschar      (), k, lab), SC ([], noflex (), lab), lab)
fun constystring'        lab k = C (NC (consstring    (), k, lab), SC ([], noflex (), lab), lab)
fun constyexception'     lab k = C (NC (consexception (), k, lab), SC ([], noflex (), lab), lab)
fun constyunit'          lab k = C (NC (consrecord    (), k, lab), SC ([], noflex (), lab), lab)
fun constylist'       tv lab k = C (NC (conslist      (), k, lab), SC (constuple [tv] lab, noflex (), lab), lab)
fun constyref'        tv lab k = C (NC (consref       (), k, lab), SC (constuple [tv] lab, noflex (), lab), lab)
fun constytuple'     tvl lab k = C (NC (consrecord    (), k, lab), SC (constuple tvl lab, noflex (), lab), lab)
fun constysubstring'     lab k = C (NC (conssubstring (), k, lab), SC ([], noflex (), lab), lab)
fun constyarray'      tv lab k = C (NC (consarray     (), k, lab), SC (constuple [tv] lab, noflex (), lab), lab)
fun constyvector'     tv lab k = C (NC (consvector    (), k, lab), SC (constuple [tv] lab, noflex (), lab), lab)
fun constyoption'     tv lab k = C (NC (consoption    (), k, lab), SC (constuple [tv] lab, noflex (), lab), lab)
fun constyorder'         lab k = C (NC (consorder     (), k, lab), SC ([], noflex (), lab), lab)
fun constyfrag'       tv lab k = C (NC (consfrag      (), k, lab), SC (constuple [tv] lab, noflex (), lab), lab)
fun constynewcons'       lab k = C (NC (freshtyname   (), k, lab), SC ([], noflex (), lab), lab) (* a new constant type *)

fun consTyArrowTy ty1 ty2 lab k = C (NC (consarrow  (), k, lab), SC (consTupleTy [ty1, ty2] lab, noflex (), lab), lab)
fun consTyTupleTy     tyl lab k = C (NC (consrecord (), k, lab), SC (consTupleTy tyl lab, noflex (), lab), lab)

fun constyarrow tv1 tv2 lab = constyarrow' tv1 tv2 lab OT
fun constyrecord  tvl f lab = constyrecord'  tvl f lab OT
fun constybool          lab = constybool'          lab OT
fun constyint           lab = constyint'           lab OT
fun constyword          lab = constyword'          lab OT
fun constyreal          lab = constyreal'          lab OT
fun constychar          lab = constychar'          lab OT
fun constystring        lab = constystring'        lab OT
fun constyexception     lab = constyexception'     lab OT
fun constyunit          lab = constyunit'          lab OT
fun constylist       tv lab = constylist'       tv lab OT
fun constyref        tv lab = constyref'        tv lab OT
fun constytuple     tvl lab = constytuple'     tvl lab OT
fun constysubstring     lab = constysubstring'     lab OT
fun constyarray      tv lab = constyarray'      tv lab OT
fun constyvector     tv lab = constyvector'     tv lab OT
fun constyoption     tv lab = constyoption'     tv lab OT
fun constyorder         lab = constyorder'         lab OT
fun constyfrag       tv lab = constyfrag'       tv lab OT
fun constynewcons       lab = constynewcons'       lab OT

fun isBase' tn = tn < constypenamestart () andalso tn > dummytyname ()
fun isBase  tn = tn = consarrow ()         orelse  tn = consrecord ()
(*orelse (tn = consint ())*)

fun isBaseTy (NV _)          = false
  | isBaseTy (NC (tn, _, _)) = isBase tn
  | isBaseTy (ND etn)        = isBaseTy (EL.getExtLabT etn)

fun isArrowTy (NV _)          = false
  | isArrowTy (NC (tn, _, _)) = tn = consarrow ()
  | isArrowTy (ND etn)        = isArrowTy (EL.getExtLabT etn)

fun isExcTy (NV _)          = false
  | isExcTy (NC (tn, _, _)) = tn = consexception ()
  | isExcTy (ND etn)        = isExcTy (EL.getExtLabT etn)

fun isDecTy (NC (_, DE _, _)) = true
  | isDecTy (ND etn)          = isDecTy (EL.getExtLabT etn)
  | isDecTy _                 = false

fun isPatTy (NC (_, PA, _)) = true
  | isPatTy (ND etn)        = isPatTy (EL.getExtLabT etn)
  | isPatTy _               = false


(* Check is the top level constructor of a type has been generated for a builtin identifier.
 * If it is the case then 3 labels are updated. *)
(* We might have to do that in the rtls if the top label is the dummy one. *)
fun labelBuiltinTy' (C (tn, seq, _)) lab =
    (case (labelBuiltinTn tn lab, labelBuiltinSq seq lab) of
	 (SOME tn', SOME seq') => SOME (C (tn', seq', lab))
       | _ => NONE)
  | labelBuiltinTy' (TD (ty, labs, stts, deps)) lab =
    (case labelBuiltinTy' ty lab of
	 SOME ty' => SOME (TD (ty', labs, stts, deps))
       | NONE => NONE)
  | labelBuiltinTy' ty _ = NONE
and labelBuiltinTy ty lab =
    (case labelBuiltinTy' ty lab of
	 SOME ty' => ty'
       | NONE => ty)
and labelBuiltinTn (NC (tn, BB, _)) lab = SOME (NC (tn, OT, lab))
  | labelBuiltinTn (ND (tn, labs, stts, deps)) lab =
    (case labelBuiltinTn tn lab of
	 SOME tn' => SOME (ND (tn', labs, stts, deps))
       | NONE => NONE)
  | labelBuiltinTn (NC _) _ = NONE
  | labelBuiltinTn (NV _) _ = NONE
and labelBuiltinSq (SC (rows, flex, _)) lab = SOME (SC (labelBuiltinRowList rows lab, flex, lab))
  | labelBuiltinSq (SD (seq, labs, stts, deps)) lab =
    (case labelBuiltinSq seq lab of
	 SOME seq' => SOME (SD (seq', labs, stts, deps))
       | NONE => NONE)
  | labelBuiltinSq (SV _) _ = NONE
and labelBuiltinRowList xs lab = map (fn x => labelBuiltinRow x lab) xs
and labelBuiltinRow (RV x)           _   = RV x
  | labelBuiltinRow (RC (lt, ty, _)) lab = RC (lt, labelBuiltinTy ty lab, lab)
  | labelBuiltinRow (RD erow)        lab = RD (EL.mapExtLab erow (fn row => labelBuiltinRow row lab))
  | labelBuiltinRow RO               _   = RO
and labelBuiltinTyf (TFC (seq, ty, l)) lab =
    (case (labelBuiltinSq seq lab, labelBuiltinTy' ty lab) of
	 (SOME seq', SOME ty') => TFC (seq', ty', lab)
       | _ => TFC (seq, ty, l))
  | labelBuiltinTyf (TFD etf) lab = TFD (EL.mapExtLab etf (fn tf => labelBuiltinTyf tf lab))
  | labelBuiltinTyf (TFV x) _ = (TFV x)

fun isVarTyName (NV _) = true
  | isVarTyName _ = false

fun isShallowRow (RV _) = true
  | isShallowRow _ = false

fun isShallowSeq (SV _)            = true
  | isShallowSeq (SC (rows, _, _)) = List.all (fn row => isShallowRow row) rows
  | isShallowSeq (SD eseq)         = isShallowSeq (EL.getExtLabT eseq)

fun decorateExtTyVars set labs stts deps =
    S.map (fn (labs0, stts0, deps0) =>
	      (L.union labs labs0, L.union stts stts0, CD.union deps deps0))
	  set

fun unionExtTyVars (set1, set2) =
    S.unionWith (fn ((labs1, stts1, deps1), (labs2, stts2, deps2)) =>
		    (L.union labs1 labs2, L.union stts1 stts2, CD.union deps1 deps2))
		(set1, set2)

fun gettyvarsrowty (RV _)                       = S.empty
  | gettyvarsrowty (RC (_, ty, _))              = gettyvarsty ty
  | gettyvarsrowty (RD (row, labs, stts, deps)) = decorateExtTyVars (gettyvarsrowty row) labs stts deps
  | gettyvarsrowty RO                           = S.empty
and gettyvarstyseq (SV _)                       = S.empty
  | gettyvarstyseq (SC (rows, _, _))            = foldr (fn (row, tvs) => unionExtTyVars (gettyvarsrowty row, tvs)) S.empty rows
  | gettyvarstyseq (SD (seq, labs, stts, deps)) = decorateExtTyVars (gettyvarstyseq seq) labs stts deps
and gettyvarstytf  (TFV _)                      = S.empty
  | gettyvarstytf  (TFC (sq, ty, _))            = unionExtTyVars (gettyvarstyseq sq, gettyvarsty ty)
  | gettyvarstytf  (TFD etf)                    = gettyvarstytf (EL.getExtLabT etf)
and gettyvarsty    (V  (v, _, _))               = S.insert (S.empty, v, (L.empty, L.empty, CD.empty))
  | gettyvarsty    (E  (_, v, _))               = S.insert (S.empty, v, (L.empty, L.empty, CD.empty)) (*??*)
  | gettyvarsty    (C  (_,  sq, _))             = gettyvarstyseq sq
  | gettyvarsty    (A  (tf, sq, _))             = unionExtTyVars (gettyvarstytf tf, gettyvarstyseq sq)
  | gettyvarsty    (OR (sq, _, _, _, _))        = gettyvarstyseq sq
  | gettyvarsty    (GEN ty)                     = S.empty
  | gettyvarsty    (TD (ty, labs, stts, deps))  = decorateExtTyVars (gettyvarsty ty) labs stts deps
(*and gettyvarstylist xs = foldr (fn (x, y) => (gettyvarsty x) @ y) [] xs
and gettyvarstyseqlist xs = foldr (fn (x, y) => (gettyvarstyseq x) @ y) [] xs*)

fun getTyVarsTy ty =
    S.foldri (fn (tv, (labs, stts, deps), list) =>
		 (tv, labs, stts, deps) :: list)
	     []
	     (gettyvarsty ty)


(* Returns the actual type constructor of a tnty *)
fun tntyToTyCon (NV tnv)        = dummytyname () (* we could also return "tnv" but we don't care about the variable really *)
  | tntyToTyCon (NC (tn, _, _)) = tn
  | tntyToTyCon (ND etn)        = tntyToTyCon (EL.getExtLabT etn)

(* determines if a parameter is of monomorphic or polymorphic type *)
fun isPoly POLY = true
  | isPoly MONO = false

(********* PRINTING SECTION **********)

fun printtyvar     tv  = "t"   ^ Int.toString tv
fun printetyvar    tv  = "e"   ^ Int.toString tv (*printetyvar for print explicit type variable *)
fun printseqvar    sv  = "s"   ^ Int.toString sv
fun printtynamevar tnv = "tnv" ^ Int.toString tnv
fun printtyname    tn  = "n"   ^ Int.toString tn
fun printrowvar    rv  = "r"   ^ Int.toString rv
fun printlabvar    lv  = "f"   ^ Int.toString lv
fun printtyfvar    tfv = "tfv" ^ Int.toString tfv
fun printlabcons   lc  = lc
fun printlabel     l   = "l"   ^ L.printLab l
fun printsmllc     lc  = "\""  ^ lc ^ "\""
fun printsmltn     tn  = Int.toString tn

fun printtyname'   0   = "a new type name"
  | printtyname'   1   = "arrow"
  | printtyname'   2   = "record"
  | printtyname'   3   = "int"
  | printtyname'   4   = "word"
  | printtyname'   5   = "real"
  | printtyname'   6   = "bool"
  | printtyname'   7   = "string"
  | printtyname'   8   = "list"
  | printtyname'   9   = "ref"
  | printtyname'   10  = "char"
  | printtyname'   11  = "exception"
  | printtyname'   12  = "substring"
  | printtyname'   13  = "array"
  | printtyname'   14  = "vector"
  | printtyname'   15  = "option"
  | printtyname'   16  = "order"
  | printtyname'   17  = "frag"
  | printtyname'   _   = "a user type" (* should be a raise DeadBranch *)

fun printTyNameAssoc tyname assoc =
    if tyname >= constypenamestart ()
    then case I.lookupId (I.fromInt tyname) assoc of
	     NONE => printtyname' tyname
	   | SOME str => "a user type named " ^ str
    else printtyname' tyname

fun printlabty (LV lv)      = printlabvar lv
  | printlabty (LC (lc, l)) = "(" ^ printlabcons lc ^ "," ^ printlabel l ^ ")"
  | printlabty (LD elt)     = "LD" ^ EL.printExtLab' elt printlabty

fun printlistgen xs f = "[" ^ #1 (foldr (fn (t, (s, c)) => (f t ^ c ^ s, ",")) ("", "") xs) ^ "]"

fun printrowvarlist xs = printlistgen xs printrowvar
fun printtyvarlist  xs = printlistgen xs printtyvar
fun printseqvarlist xs = printlistgen xs printseqvar

fun printKCons (DE id) = "DE(" ^ I.printId id ^ ")"
  | printKCons PA      = "PA"
  | printKCons OT      = "OT"
  | printKCons BB      = "BB"

fun printtnty (NV tnv)        = printtynamevar tnv
  | printtnty (NC (tn, k, l)) = "(" ^ printtyname tn ^ "," ^ printKCons k ^ "," ^ printlabel l ^ ")"
  | printtnty (ND etn)        = "ND" ^ EL.printExtLab' etn printtnty

fun printOp NONE     _ = "-"
  | printOp (SOME x) f = f x

fun printflex     x = printOp x printlabel
fun printExplicit x = printOp x I.printIdL

fun printidor i = Int.toString i

fun printPoly POLY = "POLY"
  | printPoly MONO = "MONO"

fun printOrKind (VAL idl) = "VAL(" ^ I.printIdL idl ^ ")"
  | printOrKind (CST (st, id, lab)) = "CST(" ^ st ^ "," ^ I.printId id ^ "," ^ L.printLab lab ^ ")"


(* functions to print internal types *)

fun printrowty (RV rv)            = "RV(" ^ printrowvar rv ^ ")"
  | printrowty (RC (lt, tv, l))   = "RC(" ^ printlabty lt ^
				    ":"   ^ printty    tv ^
				    ","   ^ printlabel l  ^ ")"
  | printrowty (RD erow)          = "RD"  ^ EL.printExtLab' erow printrowty
  | printrowty RO                 = "RO"
and printrowtylist xs             = printlistgen xs printrowty
and printseqty (SV sv)            = "SV(" ^ printseqvar     sv ^ ")"
  | printseqty (SC (rl, b, l))    = "SC("  ^ printrowtylist rl ^
				    ","    ^ printflex      b  ^
				    ","    ^ printlabel     l  ^ ")"
  | printseqty (SD eseq)          = "SD"   ^ EL.printExtLab' eseq printseqty
and printtyf (TFV v)              = "TFV(" ^ printtyfvar   v   ^ ")"
  | printtyf (TFC (sq, ty, l))    = "TFC(" ^ printseqty    sq  ^
				    ","    ^ printty       ty  ^
				    ","    ^ printlabel    l   ^ ")"
  | printtyf (TFD etf)            = "TFD"  ^ EL.printExtLab' etf printtyf
and printty (V (v, b, p))         = "V("   ^ printtyvar    v   ^
				    ","    ^ printExplicit b   ^
				    ","    ^ printPoly     p   ^ ")"
  | printty (E (id, tv, l))       = "E("   ^ I.printId     id  ^
				    ","    ^ printtyvar    tv  ^
				    ","    ^ printlabel    l   ^ ")"
  | printty (C (tn, sq, l))       = "C("   ^ printtnty     tn  ^
				    ","    ^ printseqty    sq  ^
				    ","    ^ printlabel    l   ^ ")"
  | printty (A (tf, sq, l))       = "A("   ^ printtyf      tf  ^
				    ","    ^ printseqty    sq  ^
				    ","    ^ printlabel    l   ^ ")"
  | printty (OR (sq, i, p, k, l)) = "OR("  ^ printseqty    sq  ^
				    ","    ^ printidor     i   ^
				    ","    ^ printPoly     p   ^
				    ","    ^ printOrKind   k   ^
				    ","    ^ printlabel    l   ^ ")"
  | printty (GEN tys)             = "GEN(" ^ printTyGen    tys ^ ")"
  | printty (TD ety)              = "TD"   ^ EL.printExtLab' ety printty
and printtylist    xs = printlistgen xs printty
and printseqtylist xs = printlistgen xs printseqty
and printTyGen tys = printtylist (!tys)


(* this printty is user friendly *)


fun printtnty' (NV tnv)            = "NV(" ^ printtynamevar tnv ^ ")"
  | printtnty' (NC (tn, k, l))     = "NC(" ^ printtyname' tn ^
				     ","   ^ printKCons   k  ^
				     ","   ^ printlabel   l  ^ ")"
  | printtnty' (ND etn)            = "ND"  ^ EL.printExtLab' etn printtnty'

fun printrowty' (RV rv)            = "RV(" ^ printrowvar rv ^ ")"
  | printrowty' (RC (lt, tv, l))   = "RC(" ^ printlabty  lt ^
				     ":"   ^ printty'    tv ^
				     ","   ^ printlabel  l  ^ ")"
  | printrowty' (RD erow)          = "RD"  ^ EL.printExtLab' erow printrowty'
  | printrowty' RO                 = "RO"
and printrowtylist' xs = printlistgen xs printrowty'
and printseqty' (SV sv)            = "SV(" ^ printseqvar sv ^ ")"
  | printseqty' (SC (rtl, b, lab)) = "SC(" ^ printrowtylist' rtl ^
				     ","   ^ printflex       b   ^
				     ","   ^ printlabel      lab ^ ")"
  | printseqty' (SD eseq)          = "SD"  ^ EL.printExtLab' eseq printseqty'
and printtyf' (TFV v)              = "TFV("  ^ printtyfvar   v   ^ ")"
  | printtyf' (TFC (sq, ty, l))    = "TFC("  ^ printseqty'   sq  ^
				     ","     ^ printty'      ty  ^
				     ","     ^ printlabel    l   ^ ")"
  | printtyf' (TFD etf)            = "TFD"   ^ EL.printExtLab' etf printtyf'
and printty' (V (v, b, p))         = "V("    ^ printtyvar    v   ^
				     ","     ^ printExplicit b   ^
				     ","     ^ printPoly     p   ^ ")"
  | printty' (E (id, tv, l))       = "E("    ^ I.printId     id  ^
				     ","     ^ printtyvar    tv  ^
				     ","     ^ printlabel    l   ^ ")"
  | printty' (C (tn, sq, l))       = "CONS(" ^ printtnty'    tn  ^
				     ","     ^ printseqty'   sq  ^
				     ","     ^ printlabel    l   ^ ")"
  | printty' (A (tf, sq, l))       = "A("    ^ printtyf'     tf  ^
				     ","     ^ printseqty'   sq  ^
				     ","     ^ printlabel    l   ^ ")"
  | printty' (OR (sq, i, p, k, l)) = "OR("   ^ printseqty'   sq  ^
				     ","     ^ printidor     i   ^
				     ","     ^ printPoly     p   ^
				     ","     ^ printOrKind   k   ^
				     ","     ^ printlabel    l   ^ ")"
  | printty' (GEN tys)             = "GEN("  ^ printTyGen'  tys  ^ ")"
  | printty' (TD ety)              = "TD"    ^ EL.printExtLab' ety printty'
and printtylist' tys = printlistgen tys printty'
and printTyGen'  tys = printtylist' (!tys)

fun printAssoc  xs = printlistgen xs (fn (tv, st) => "(" ^ Int.toString tv ^ "," ^ st ^ ")")
fun printAssoc' xs = printlistgen xs (fn (tv, st) => "(" ^ Int.toString tv ^ "," ^ "\"" ^ st ^ "\"" ^ ")")

end
