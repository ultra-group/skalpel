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
 * along with Skalpel.  If not, see <http://www.gnu.org/licenses/>.
 *
 *  o Authors:     Vincent Rahli, John Pirie
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        21 May 2010
 *  o File name:   Unification.sml
 *  o Description: Contains the unification algorithm.  The file defines
 *      the functor Unif with signature UNIF and takes a parameter of
 *      signature STATE.
 *)

(* we need to use ":" instead of ":>" because of the types of State *)
functor Unif (S : STATE) : UNIF = struct

structure S   = S
structure Set  = BinarySetFn (OrdKey)
structure P   = Poly
structure D   = Debug
structure O   = OrdSet
structure L   = Label
structure I   = Id
structure T   = Ty
structure E   = Env
structure F   = Fresh
structure C   = ConsId
structure X   = Expans
structure EL  = ExtLab
structure FI  = Filter
structure CL  = ClassId
structure CD  = LongId
structure EK  = ErrorKind
structure VT  = VTimer
structure EH  = ErrorHandler
structure OM  = SplayMapFn(OrdKey)
structure ERR = Error
structure D   = Debug

val analysingBasis : bool ref = ref false

(* error shoud be called state or end_state ad S.state should be called context.
 * An error (end_state) is either a success or an error.  Our constraint solver
 * terminates in one of these states.  It terminates in a success state given
 * solvable constaints and returns its current context.  It terminates in an
 * error state given unsolvable constraint and returns an error as well as the
 * current context. *)
datatype error = Success of S.state
               | Error   of ERR.error * S.state

(* These datatypes are used by the occurs check.
 * We check that given a CT, a CS, or a CR term, the variable does not
 * occur nested in the type. *)
datatype occty = T  of T.typeVar  EL.extLab
               | S  of T.rowVar EL.extLab
               | R  of T.fieldVar EL.extLab
datatype cocc  = CT of T.typeVar  * T.ty
               | CS of T.rowVar * T.rowType
               | CR of T.fieldVar * T.fieldType

(* THESE bind forms are used to solve binders.   *)
datatype 'a sbind = BINDOUT                  (* the binding has been discarded                    *)
		  | BINDNOT of E.constraints (* the binding is not a binding                      *)
		  | BINDIN  of 'a E.bind     (* we kept the binding                               *)
		  | BINDPOL of 'a E.bind     (* we kept the binding but weakened the poly field   *)
		  | BINDDUM of 'a E.bind     (* the binding has been transformed into a dummy one *)

datatype user = MIN of ERR.error
	      | ENUM
	      | DBENUM


type tfun = T.typeFunction OM.map

(* This is used to pass errors to the constraint solver from
 * auxiliary functions. *)
exception errorfound of ERR.error


(* printing *)

fun printCocc (CT (tv, ty))  = "CT(" ^ T.printTypeVar  tv ^ "," ^ T.printty    ty ^ ")"
  | printCocc (CS (sv, sq))  = "CS(" ^ T.printRowVar sv ^ "," ^ T.printseqty sq ^ ")"
  | printCocc (CR (rv, rt))  = "CR(" ^ T.printFieldVar rv ^ "," ^ T.printFieldType rt ^ ")"

fun printOccty (T x) = "T" ^ EL.printExtLab' x T.printTypeVar
  | printOccty (S x) = "S" ^ EL.printExtLab' x T.printRowVar
  | printOccty (R x) = "R" ^ EL.printExtLab' x T.printFieldVar

fun printOcctyList [] = ""
  | printOcctyList (x :: xs) = printOccty x ^ "\n" ^ printOcctyList xs

(*fun printError (Success state)      =
    "SUCCESS:\n" ^ S.printState state
  | printError (Error (err, state)) =
    "ERROR:\n" ^
    ERR.printOneXmlErr err "" true ^ "\n" ^
    S.printState state*)

fun printTFun tfun =
    "[" ^ #2 (OM.foldri (fn (id, exttf, (sep, str)) =>
			    (",", EL.printExtLab' exttf T.printtyf ^ sep ^ str))
			("", "]")
			tfun)


(* -------------- *)


fun decomptyfield (T.FIELD_VAR rv)                ll deps ids = ([R (rv, ll, deps, ids)], 0)
  | decomptyfield (T.FC (_, ty, _))        ll deps ids = decomptyty ty ll deps ids
  | decomptyfield (T.FIELD_DEPENDANCY (r, x, y, z))      ll deps ids = decomptyfield r (L.union x ll) (L.union y deps) (CD.union z ids)
  | decomptyfield T.FIELD_NO_OVERLOAD                     ll deps ids = ([], 0)
and decomptysq  (T.ROW_VAR sv)                ll deps ids = ([S (sv, ll, deps, ids)], 0)
  | decomptysq  (T.ROW_C (rtl, _, _))       ll deps ids = (decomptyfieldlist rtl ll deps ids, 1)
  | decomptysq  (T.ROW_DEPENDANCY (s, x, y, z))      ll deps ids = decomptysq s (L.union x ll) (L.union y deps) (CD.union z ids)
and decomptyty  (T.TYPE_VAR (tv, _, _, _))         ll deps ids = ([T (tv, ll, deps, ids)], 0)
  | decomptyty  (T.EXPLICIT_TYPE_VAR (n, tv, l, eqtv))         ll deps ids = ([], 0) (* TODO: because it's a constant type but check that anyway with a circularity test *)
  | decomptyty  (T.TYPE_CONSTRUCTOR (_, sq, _, _))         ll deps ids = decomptysq sq ll deps ids
  | decomptyty  (T.APPLICATION (tyf, sq, _))       ll deps ids = ([], 0) (* NOTE: Can a circularity error go through a type/datatype definition? *)
  | decomptyty  (T.TYPE_POLY  (sq, _, _, _, _, _)) ll deps ids = decomptysq sq ll deps ids
  | decomptyty  (T.GEN ty)               ll deps ids = ([], 0) (*(2010-06-23)Isn'y that risky?*)
  | decomptyty  (T.TYPE_DEPENDANCY (t, x, y, z))      ll deps ids = decomptyty t (L.union x ll) (L.union y deps) (CD.union z ids)
and decomptyfieldlist xs ll deps ids = foldr (fn (x, y) => (#1 (decomptyfield x ll deps ids)) @ y) [] xs
(*and decomptytylist  xs ll deps ids = foldr (fn (x, y) => (#1 (decomptyty  x ll deps ids)) @ y) [] xs*)


(* -------------- *)

fun getpairs l1 l2 =
    foldr (fn (x, pairs) => foldr (fn (y, pairs) => (x, y) :: pairs) pairs l2) [] l1

fun comparetypsenv typsenv1 typsenv2 filters ls deps ids = raise EH.TODO "no description, raised in the 'comparetypsenv' function of Unification.sml"

fun comparevidsenv idenv1 idenv2 filters ls deps ids =
    let val dom1  = E.dom idenv1
	val dom2  = E.dom idenv2
	val dom   = I.inter dom1 dom2
	val dom1' = I.difference dom dom1 (* the env that is in idenv1 but not in idenv2 *)
	val dom2' = I.difference dom dom2 (* the env that is in idenv2 but not in idenv1 *)
	(* we should check the emptyness of dom1' and dom2' only if
	 * the labels associated to idenv1 and idenv2 are OK wrt projlab and filter *)
	val cs = I.foldr
		     (fn (id, cs) =>
			 let val tvl1 = map (fn x => E.getBindT x) (E.plusproj idenv1 id)
			     val tvl2 = map (fn x => E.getBindT x) (E.plusproj idenv2 id)
			     val pairs = getpairs tvl1 tvl2
			 in (map (fn (tv1, tv2) => E.genCstTyAll tv1 tv2 ls deps ids) pairs) @ cs
			 end)
		     []
		     dom
    in cs
    end

(* TODO: we don't pass test273 because of that:
 * We should have a similar test as in genenv *)
fun comparestrsenv strenv1 strenv2 filters ls deps ids =
    let val dom1  = E.dom strenv1
	val dom2  = E.dom strenv2
	val dom   = I.inter dom1 dom2
	val dom1' = I.difference dom dom1 (* the env that is in idenv1 but not in idenv2 *)
	val dom2' = I.difference dom dom2 (* the env that is in idenv2 but not in idenv1 *)
	(* we should check the emptyness of dom1' and dom2' only if
	 * the labels associated to idenv1 and idenv2 are OK wrt projlab and filter *)
	val cs = I.foldr
		     (fn (id, cs) =>
			 let val envl1 = map (fn x => E.getBindT x) (E.plusproj strenv1 id)
			     val envl2 = map (fn x => E.getBindT x) (E.plusproj strenv2 id)
			     val pairs = getpairs envl1 envl2
			 in (map (fn (x1, x2) => E.genCstEvAll x1 x2 ls deps ids) pairs) @ cs
			 end)
		     []
		     dom
    in cs
    end

(* compares the value identifiers from two different envs *)
fun compareenv env1 env2 filters ls deps ids =
    let val cs1 = comparevidsenv (E.getValueIds env1) (E.getValueIds env2) filters ls deps ids
	val cs2 = comparetypsenv (E.getTypeNameEnv env1) (E.getTypeNameEnv env2) filters ls deps ids
	val cs3 = comparestrsenv (E.getStructs env1) (E.getStructs env2) filters ls deps ids
    in cs1 @ cs2 @ cs3
    end

fun decorateCst' (E.ROW_CONSTRAINT x) labs stts deps = E.ROW_CONSTRAINT (EL.updExtLab x labs stts deps)
  | decorateCst' (E.TYPE_CONSTRAINT x) labs stts deps = E.TYPE_CONSTRAINT (EL.updExtLab x labs stts deps)
  | decorateCst' (E.FUNCTION_TYPE_CONSTRAINT x) labs stts deps = E.FUNCTION_TYPE_CONSTRAINT (EL.updExtLab x labs stts deps)
  | decorateCst' c labs stts deps = (print (E.printEnv (E.CONSTRAINT_ENV (E.singleConstraint (L.dummyLab, c))) "");
				     raise EH.DeadBranch "")

fun decorateCst xs labs stts deps =
    map (fn x => decorateCst' x labs stts deps) xs


(* Collapsing of dependencies - have to be moved to Ty.sml. *)

fun collapseTy (T.TYPE_DEPENDANCY (T.TYPE_DEPENDANCY (ty, labs1, stts1, deps1), labs2, stts2, deps2)) labs3 stts3 deps3 =
    collapseTy ty
	       (L.union  (L.union  labs1 labs2) labs3)
	       (L.union  (L.union  stts1 stts2) stts3)
	       (CD.union (CD.union deps1 deps2) deps3)
  | collapseTy (T.TYPE_DEPENDANCY (ty, labs1, stts1, deps1)) labs2 stts2 deps2 =
    T.TYPE_DEPENDANCY (ty,
	  L.union  labs1 labs2,
	  L.union  stts1 stts2,
	  CD.union deps1 deps2)
  | collapseTy ty labs stts deps = T.TYPE_DEPENDANCY (ty, labs, stts, deps)

fun collapseSq (T.ROW_DEPENDANCY (T.ROW_DEPENDANCY (sq, labs1, stts1, deps1), labs2, stts2, deps2)) labs3 stts3 deps3 =
    collapseSq sq
	       (L.union  (L.union  labs1 labs2) labs3)
	       (L.union  (L.union  stts1 stts2) stts3)
	       (CD.union (CD.union deps1 deps2) deps3)
  | collapseSq (T.ROW_DEPENDANCY (sq, labs1, stts1, deps1)) labs2 stts2 deps2 =
    T.ROW_DEPENDANCY (sq,
	  L.union  labs1 labs2,
	  L.union  stts1 stts2,
	  CD.union deps1 deps2)
  | collapseSq sq labs stts deps = T.ROW_DEPENDANCY (sq, labs, stts, deps)

fun collapseRt (T.FIELD_DEPENDANCY (T.FIELD_DEPENDANCY (rt, labs1, stts1, deps1), labs2, stts2, deps2)) labs3 stts3 deps3 =
    collapseRt rt
	       (L.union  (L.union  labs1 labs2) labs3)
	       (L.union  (L.union  stts1 stts2) stts3)
	       (CD.union (CD.union deps1 deps2) deps3)
  | collapseRt (T.FIELD_DEPENDANCY (rt, labs1, stts1, deps1)) labs2 stts2 deps2 =
    T.FIELD_DEPENDANCY (rt,
	  L.union  labs1 labs2,
	  L.union  stts1 stts2,
	  CD.union deps1 deps2)
  | collapseRt rt labs stts deps = T.FIELD_DEPENDANCY (rt, labs, stts, deps)

fun collapseTn (T.TYPENAME_DEPENDANCY (T.TYPENAME_DEPENDANCY (tn, labs1, stts1, deps1), labs2, stts2, deps2)) labs3 stts3 deps3 =
    collapseTn tn
	       (L.union  (L.union  labs1 labs2) labs3)
	       (L.union  (L.union  stts1 stts2) stts3)
	       (CD.union (CD.union deps1 deps2) deps3)
  | collapseTn (T.TYPENAME_DEPENDANCY (tn, labs1, stts1, deps1)) labs2 stts2 deps2 =
    T.TYPENAME_DEPENDANCY (tn,
	  L.union  labs1 labs2,
	  L.union  stts1 stts2,
	  CD.union deps1 deps2)
  | collapseTn tn labs stts deps = T.TYPENAME_DEPENDANCY (tn, labs, stts, deps)

fun collapseTf (T.TYPE_FUNCTION_DEPENDANCY (T.TYPE_FUNCTION_DEPENDANCY (tf, labs1, stts1, deps1), labs2, stts2, deps2)) labs3 stts3 deps3 =
    collapseTf tf
	       (L.union  (L.union  labs1 labs2) labs3)
	       (L.union  (L.union  stts1 stts2) stts3)
	       (CD.union (CD.union deps1 deps2) deps3)
  | collapseTf (T.TYPE_FUNCTION_DEPENDANCY (tf, labs1, stts1, deps1)) labs2 stts2 deps2 =
    T.TYPE_FUNCTION_DEPENDANCY (tf,
	  L.union  labs1 labs2,
	  L.union  stts1 stts2,
	  CD.union deps1 deps2)
  | collapseTf tf labs stts deps = T.TYPE_FUNCTION_DEPENDANCY (tf, labs, stts, deps)

(* This is used for or types *)

fun gatherAllTnTy (T.TYPE_CONSTRUCTOR (tn, _, _, _)) = gatherAllTnTn tn
  | gatherAllTnTy (T.TYPE_POLY (sq, _, _, _, _, _)) = gatherAllTnSq sq
  | gatherAllTnTy (T.TYPE_DEPENDANCY (ty, labs, stts, deps)) =
    let val (list, labs', stts', deps') = gatherAllTnTy ty
    in (list, L.union labs labs', L.union stts stts', CD.union deps deps')
    end
  | gatherAllTnTy _ = ([], L.empty, L.empty, CD.empty)
and gatherAllTnSq (T.ROW_VAR _) = ([], L.empty, L.empty, CD.empty)
  | gatherAllTnSq (T.ROW_C (rtl, _, _)) =
    foldr (fn ((list1, labs1, stts1, deps1), (list2, labs2, stts2, deps2)) =>
	      (list1 @ list2,
	       L.union  labs1 labs2,
	       L.union  stts1 stts2,
	       CD.union deps1 deps2))
	  ([], L.empty, L.empty, CD.empty)
	  (map gatherAllTnRt rtl)
  | gatherAllTnSq (T.ROW_DEPENDANCY (seq, labs, stts, deps)) =
    let val (list, labs', stts', deps') = gatherAllTnSq seq
    in (list, L.union labs labs', L.union stts stts', CD.union deps deps')
    end
and gatherAllTnRt (T.FIELD_VAR _) = ([], L.empty, L.empty, CD.empty)
  | gatherAllTnRt (T.FC (_, ty, _)) = gatherAllTnTy ty
  | gatherAllTnRt (T.FIELD_DEPENDANCY (field, labs, stts, deps)) =
    let val (list, labs', stts', deps') = gatherAllTnRt field
    in (list, L.union labs labs', L.union stts stts', CD.union deps deps')
    end
  | gatherAllTnRt T.FIELD_NO_OVERLOAD = ([], L.empty, L.empty, CD.empty)
and gatherAllTnTn (T.NC (tn, _, l)) =
    ([(L.toInt l, T.typenameToInt tn)], L.empty, L.empty, CD.empty)
  | gatherAllTnTn (T.TYPENAME_DEPENDANCY (tn, labs, stts, deps)) =
    let val (list, labs', stts', deps') = gatherAllTnTn tn
    in (list, L.union labs labs', L.union stts stts', CD.union deps deps')
    end
  | gatherAllTnTn (T.TYPENAME_VAR _) = ([], L.empty, L.empty, CD.empty)

fun isAllTy (T.TYPE_POLY (sq, _, _, _, _, _)) = isAllSq sq
  | isAllTy (T.TYPE_CONSTRUCTOR (tn, _, _, _)) = isAllTn tn
  | isAllTy (T.TYPE_DEPENDANCY ety) = isAllTy (EL.getExtLabT ety)
  | isAllTy _ = false
and isAllSq (T.ROW_VAR _) = false
  | isAllSq (T.ROW_C (rtl, _, _)) = List.all isAllRt rtl
  | isAllSq (T.ROW_DEPENDANCY eseq) = isAllSq (EL.getExtLabT eseq)
and isAllRt (T.FIELD_VAR _) = false
  | isAllRt (T.FC (_, ty, _)) = isAllTy ty
  | isAllRt (T.FIELD_DEPENDANCY efield) = isAllRt (EL.getExtLabT efield)
  | isAllRt T.FIELD_NO_OVERLOAD = true
and isAllTn (T.NC _) = true
  | isAllTn (T.TYPENAME_DEPENDANCY etn) = isAllTn (EL.getExtLabT etn)
  | isAllTn (T.TYPENAME_VAR _) = false

fun mergeFindInOrLists [] list = list
  | mergeFindInOrLists list [] = list
  | mergeFindInOrLists ((T.TYPE_VAR _, _) :: x :: _) _ = raise EH.DeadBranch ""
  | mergeFindInOrLists _ ((T.TYPE_VAR _, _) :: x :: _) = raise EH.DeadBranch ""
  | mergeFindInOrLists list [(T.TYPE_VAR _, _)] = list (* list cannot be empty *)
  | mergeFindInOrLists [(T.TYPE_VAR _, _)] list = list (* list cannot be empty *)
  | mergeFindInOrLists list1 list2 = list1 @ list2

fun findInOrTy tn path (t as (T.TYPE_CONSTRUCTOR (tnc, _, _, _))) =
    (case findInOrTn tnc of
	 SOME tn' =>
	 if T.eqTypename tn tn'
	 then ([(t, path)], true, true)
	 else ([], false, false)
       | NONE => ([], true, false))
  | findInOrTy tn path (T.TYPE_POLY (sq, _, _, _, _, _)) = findInOrSq tn path sq
  | findInOrTy _ path (t as (T.TYPE_VAR _)) = ([(t, path)], true, false)
  | findInOrTy tn path (T.TYPE_DEPENDANCY ety) = findInOrTy tn path (EL.getExtLabT ety)
  | findInOrTy _ _ _ = ([], true, false)

and findInOrSq tn path (T.ROW_C (rtl, _, _)) =
    #1 (List.foldl
	    (fn (rt, ((x, y, z), c)) =>
		(* y is true if we've found something.
		 * z is false if we've found a variable and true if a construction.
		 * y is false if we haven't found anything, then z is true in case of RO. *)
		let val triple =
			case findInOrRt tn (path @ [c]) rt of
			    (* haven't found anything because of var *)
			    ([], true, false)   => (x, true, z)
			  (* impossible *)
			  | ([], true, true)    => raise EH.DeadBranch ""
			  (* haven't found anything *)
			  | ([], false, false)  => (x, y, z)
			  (* impossible *)
			  | ([], false, true)   => raise EH.DeadBranch ""
			  (* found a type construction *)
			  | (list, true, true)  => (mergeFindInOrLists x list, true, true)
			  (* found a variable *)
			  | (list, true, false) => (mergeFindInOrLists x list, true, z)
			  (* impossible *)
			  | (_, false, _)       => raise EH.DeadBranch ""
		in (triple, c + 1)
		end)
	    (([], false, false), 0)
	    rtl)
  | findInOrSq _ _ (T.ROW_VAR _) = ([], true, false)
  | findInOrSq tn path (T.ROW_DEPENDANCY eseq) = findInOrSq tn path (EL.getExtLabT eseq)

and findInOrRt tn path (T.FC (_, ty, _)) = findInOrTy tn path ty
  | findInOrRt _ _ (T.FIELD_VAR _) = ([], true, false)
  | findInOrRt tn path (T.FIELD_DEPENDANCY efield) = findInOrRt tn path (EL.getExtLabT efield)
  | findInOrRt tn path T.FIELD_NO_OVERLOAD = ([], false, false)

and findInOrTn (T.NC (tn, _, _)) = SOME tn
  | findInOrTn (T.TYPENAME_VAR _) = NONE
  | findInOrTn (T.TYPENAME_DEPENDANCY etn) = findInOrTn (EL.getExtLabT etn)

fun gotoInOrTy path (t as (T.TYPE_CONSTRUCTOR (tn, _, _, _))) =
    (if List.null path
     then case gotoInOrTn tn of
	      SOME _ => SOME t
	    | NONE => NONE
     else NONE)
  | gotoInOrTy path (T.TYPE_POLY (sq, _, _, _, _, _)) = gotoInOrSq path sq
  | gotoInOrTy path (t as (T.TYPE_VAR _)) =
    if List.null path then SOME t else NONE
  | gotoInOrTy path (T.TYPE_DEPENDANCY (ty, labs, stts, deps)) =
    (case gotoInOrTy path ty of
	 SOME ty => SOME (collapseTy ty labs stts deps)
       | NONE => NONE)
  | gotoInOrTy _ _ = NONE
and gotoInOrSq (p :: path) (T.ROW_C (rtl, _, _)) =
    (gotoInOrRt path (List.nth (rtl, p))
     handle Subscript => raise EH.DeadBranch "an OR type has an unexpected structure")
  | gotoInOrSq [] (T.ROW_C _) = raise EH.DeadBranch "an OR type has an unexpected structure"
  | gotoInOrSq _ (T.ROW_VAR _) = NONE
  | gotoInOrSq path (T.ROW_DEPENDANCY (seq, labs, stts, deps)) =
    (case gotoInOrSq path seq of
	 SOME ty => SOME (collapseTy ty labs stts deps)
       | NONE => NONE)
and gotoInOrRt path (T.FC (_, ty, _)) = gotoInOrTy path ty
  | gotoInOrRt _ (T.FIELD_VAR _) = NONE
  | gotoInOrRt path (T.FIELD_DEPENDANCY (field, labs, stts, deps)) =
    (case gotoInOrRt path field of
	 SOME ty => SOME (collapseTy ty labs stts deps)
       | NONE => NONE)
  | gotoInOrRt path T.FIELD_NO_OVERLOAD = NONE
and gotoInOrTn (tn as T.NC (name, _, _)) = SOME tn
  | gotoInOrTn (T.TYPENAME_DEPENDANCY (tn, labs, stts, deps)) =
    (case gotoInOrTn tn of
	 SOME tn => SOME (collapseTn tn labs stts deps)
       | NONE => NONE)
  | gotoInOrTn (T.TYPENAME_VAR _) = NONE

(* We build the row variable in a OR because in the case of overloading constants
 * These are not already built when dealing with the binder. *)
fun buildDirectOr (ty as T.TYPE_POLY (T.ROW_VAR sv, idor, poly, kind, lab, eq)) state =
    (case S.getValStateSq state sv of
	 NONE => ty
       | SOME sq => T.TYPE_POLY (sq, idor, poly, kind, lab, eq))
  | buildDirectOr ty state = ty


fun getPathsCol (paths : S.paths) col =
    List.mapPartial (fn [] => NONE
		      | (c :: path) =>
			if c = col
			then SOME path
			else NONE)
		    paths

fun selectPathsSeq [] seq = NONE
  | selectPathsSeq paths (seq as T.ROW_VAR _) = SOME seq
  | selectPathsSeq paths (T.ROW_DEPENDANCY (seq, labs, stts, deps)) =
    (case selectPathsSeq paths seq of
	 NONE => NONE
       | SOME seq' => SOME (T.ROW_DEPENDANCY (seq', labs, stts, deps)))
  | selectPathsSeq paths (sq as T.ROW_C (fields, flex, lab)) =
    let val (fields', some, _) =
	    foldl (fn (field, (fields, some, col)) =>
		      let val paths' = getPathsCol paths col
		      in case selectPathsField paths' field of
			     NONE => (fields @ [T.FIELD_NO_OVERLOAD], some, col + 1)
			   | SOME field' => (fields @ [field'], true, col + 1)
		      end)
		  ([], false, 0)
		  fields
    in if some
       then SOME (T.ROW_C (fields', flex, lab))
       else NONE
    end

and selectPathsField [] field = NONE
  | selectPathsField paths (field as T.FIELD_VAR _) = SOME field
  | selectPathsField paths (T.FIELD_DEPENDANCY (field, labs, stts, deps)) =
    (case selectPathsField paths field of
	 NONE => NONE
       | SOME field' => SOME (T.FIELD_DEPENDANCY (field', labs, stts, deps)))
  | selectPathsField paths (T.FC (fieldname, ty, lab)) =
    (case selectPathsTy paths ty of
	 NONE => NONE
       | SOME ty' => SOME (T.FC (fieldname, ty', lab)))
  | selectPathsField paths T.FIELD_NO_OVERLOAD = SOME T.FIELD_NO_OVERLOAD

and selectPathsTy [] ty = NONE
  | selectPathsTy paths (ty as T.TYPE_CONSTRUCTOR (tn, _, _, _)) =
    if List.exists (fn path => List.null path) paths
    then SOME ty
    else NONE
  | selectPathsTy paths (T.TYPE_POLY (seq, id, poly, kind, lab, eq)) =
    (case selectPathsSeq paths seq of
	 NONE => NONE
       | SOME seq' => SOME (T.TYPE_POLY (seq', id, poly, kind, lab, eq)))
  | selectPathsTy paths (ty as T.TYPE_VAR _) = SOME ty
  | selectPathsTy paths (T.TYPE_DEPENDANCY (ty, labs, stts, deps)) =
    (case selectPathsTy paths ty of
	 NONE => NONE
       | SOME ty' => SOME (T.TYPE_DEPENDANCY (ty', labs, stts, deps)))
  | selectPathsTy _ _ = NONE

fun selectPaths paths seq =
    case selectPathsSeq paths seq of
	NONE => T.ROW_VAR (T.freshRowVar ())
      | SOME seq => seq

fun isFullOrSeq (T.ROW_VAR _) = false
  | isFullOrSeq (T.ROW_DEPENDANCY eseq) = isFullOrSeq (EL.getExtLabT eseq)
  | isFullOrSeq (T.ROW_C (fields, _, _)) =
    List.all (fn field => isFullOrField field) fields

and isFullOrField (T.FIELD_VAR _) = false
  | isFullOrField (T.FIELD_DEPENDANCY efield) = isFullOrField (EL.getExtLabT efield)
  | isFullOrField (T.FC (_, ty, _)) = isFullOrTy ty
  | isFullOrField T.FIELD_NO_OVERLOAD = true

and isFullOrTy (T.TYPE_CONSTRUCTOR (tn, _, _, _)) = isFullOrTn tn
  | isFullOrTy (T.TYPE_POLY (seq, _, _, _, _, _)) = isFullOrSeq seq
  | isFullOrTy (T.TYPE_DEPENDANCY ety) = isFullOrTy (EL.getExtLabT ety)
  | isFullOrTy _ = false

and isFullOrTn (T.NC (name, _, _)) = true
  | isFullOrTn (T.TYPENAME_DEPENDANCY etn) = isFullOrTn (EL.getExtLabT etn)
  | isFullOrTn (T.TYPENAME_VAR _) = false

fun isFullOr seq = isFullOrSeq seq


(* New matching of or/seq structures *)

fun concatOptList (list1, b1) (list2, b2) =
    let fun isin _ [] = false
	  | isin (path : int list) ((_, path') :: list) =
	    path = path' orelse isin path list
	val b = b1 andalso b2
	val list =
	    foldr (fn ((t, path), list) =>
		      if isin path list
		      then list
		      else (t, path) :: list)
		  list1
		  list2
    in (list, b)
    end

fun tryToMatchOrsSq path (T.ROW_VAR _) sq2 = (([], false), ([], true), true)
  | tryToMatchOrsSq path (sq1 as T.ROW_C (fields, _, _)) sq2 =
    let val (typaths1, typaths2, found, _) =
	    foldl (fn (field, (typaths1, typaths2, found, c)) =>
		      case tryToMatchOrsRt (path @ [c]) field sq2 of
			  (_, _, false) => (typaths1, typaths2, found, c+1)
			| (typaths1', typaths2', true) =>
			  (concatOptList typaths1 typaths1',
			   concatOptList typaths2 typaths2',
			   true,
			   c+1))
		  (([], true), ([], true), false, 0)
		  fields
	(*val _ = D.printdebug2 ("[" ^ Bool.toString found ^ "]\n" ^
			       T.printseqty sq1    ^ "\n" ^
			       T.printseqty sq2)*)
    in (typaths1, typaths2, found)
    end
  | tryToMatchOrsSq path (T.ROW_DEPENDANCY eseq) sq2 =
    tryToMatchOrsSq path (EL.getExtLabT eseq) sq2

and tryToMatchOrsTy path (t as (T.TYPE_CONSTRUCTOR (tn, _, _, _))) sq =
    (case tryToMatchOrsTn path tn sq of
	 (list as (x :: _), true)  => (([(t, path)], true), (list, true), true)
       | (list as (x :: _), false) => raise EH.DeadBranch ""
       | ([], true)                => (([(t, path)], true), ([], false), true)
       | ([], false)               => (([], false), ([], false), false))
  | tryToMatchOrsTy path (T.TYPE_POLY (sq, _, _, _, _, _)) sq2 = tryToMatchOrsSq path sq sq2
  | tryToMatchOrsTy path (t as T.TYPE_VAR _) sq2 = (([], false), ([], true), true)
  | tryToMatchOrsTy path (T.TYPE_DEPENDANCY ety) sq2 =
    tryToMatchOrsTy path (EL.getExtLabT ety) sq2
  | tryToMatchOrsTy _ _ _ = (([], false), ([], true), true) (* Something isn't complete *)

and tryToMatchOrsTn path (T.NC (name, _, _)) sq =
    (case findInOrSq name [] sq of
	 ([], true, _)                   => ([], true)                (* Something isn't complete *)
       | ([], false, _)                  => ([], false)               (* No match                 *)
       | (list, false, _)                => raise EH.DeadBranch ""    (* Shouldn't happen         *)
       | ([(T.TYPE_VAR _, _)], true, false)     => ([], true)                (* We found a match         *)
       | ([(T.TYPE_VAR _, _)], true, true)      => raise EH.DeadBranch ""    (* Shouldn't happen         *)
       | (((T.TYPE_VAR _, _) :: _), _, _)       => raise EH.DeadBranch ""    (* Shouldn't happen         *)
       | (list, true, true)              => (list, true)              (* We found a match         *)
       | (list, true, false)             => raise EH.DeadBranch "")   (* Shouldn't happen         *)
  | tryToMatchOrsTn path (T.TYPENAME_DEPENDANCY etn) sq = tryToMatchOrsTn path (EL.getExtLabT etn) sq
  | tryToMatchOrsTn path (T.TYPENAME_VAR _) sq = ([], true)

and tryToMatchOrsRt path (T.FC (_, ty, _)) sq = tryToMatchOrsTy path ty sq
  | tryToMatchOrsRt _ (T.FIELD_VAR _) _ = (([], false), ([], true), true)
  | tryToMatchOrsRt path (T.FIELD_DEPENDANCY efield) sq2 =
    tryToMatchOrsRt path (EL.getExtLabT efield) sq2
  | tryToMatchOrsRt path T.FIELD_NO_OVERLOAD sq2 = (([], true), ([], true), false)

fun tryToMatchOrs sq1 sq2 = tryToMatchOrsSq [] sq1 sq2

(* ------ Type freshning ------ *)

fun freshlabty (T.LABEL_VAR lv) state = T.LABEL_VAR (F.freshLabVar lv state)
  | freshlabty labty     _     = labty

fun freshTypename (T.TYPENAME_VAR var) state = T.TYPENAME_VAR (F.freshTypenameVar var state)
  | freshTypename tnty       _     = tnty

fun freshfieldType (T.FIELD_VAR rv)           _   state _    = T.FIELD_VAR (F.freshFieldVar rv state)
  | freshfieldType (T.FC (lt, ty, l))  tvl state bstr = T.FC (freshlabty lt state, freshty ty tvl state bstr, l)
  | freshfieldType (T.FIELD_DEPENDANCY efield)         tvl state bstr = T.FIELD_DEPENDANCY (EL.mapExtLab efield (fn field => freshfieldType field tvl state bstr))
  | freshfieldType T.FIELD_NO_OVERLOAD                _   _     _    = T.FIELD_NO_OVERLOAD
and freshtypeFunction (T.TYPE_FUNCTION_VAR tfv)         _   state _    = T.TYPE_FUNCTION_VAR (F.freshTypeFunctionVar tfv state)
  | freshtypeFunction (T.TFC (sq, ty, l)) tvl state bstr = T.TFC (freshseqty sq tvl state bstr, freshty ty tvl state bstr, l)
  | freshtypeFunction (T.TYPE_FUNCTION_DEPENDANCY etf)         tvl state bstr = T.TYPE_FUNCTION_DEPENDANCY (EL.mapExtLab etf (fn tf => freshtypeFunction tf tvl state bstr))
and freshseqty (T.ROW_VAR var)          _   state _    = T.ROW_VAR (F.freshRowVar var state)
  | freshseqty (T.ROW_C (trl, b, l))  tvl state bstr = T.ROW_C (map (fn rt => freshfieldType rt tvl state bstr) trl, b, l)
  | freshseqty (T.ROW_DEPENDANCY eseq)         tvl state bstr = T.ROW_DEPENDANCY (EL.mapExtLab eseq (fn seq => freshseqty seq tvl state bstr))
and freshty (T.TYPE_VAR (tv, b, p, _))       tvl state bstr =
    (case (tvl, p) of
	(NONE, T.POLY) => T.TYPE_VAR (F.freshTypeVar tv state, if bstr then NONE else b, p, T.UNKNOWN)
      | (SOME tvl', T.POLY) =>
	if O.isin (T.typeVarToInt tv) tvl'
	then T.TYPE_VAR (tv, if bstr then NONE else b, p, T.UNKNOWN)
	else T.TYPE_VAR (F.freshTypeVar tv state, if bstr then NONE else b, p, T.UNKNOWN)
      | (_, T.MONO) => T.TYPE_VAR (tv, if bstr then NONE else b, (*T.POLY*)(*N*)T.MONO, T.UNKNOWN)) (* NOTE: We reset all the type variables as polymorphic.  Why?  Because of the accessors. *)
  | freshty (T.EXPLICIT_TYPE_VAR   (id, tv,   l, eqtv))  tvl state bstr =
    (*(2010-06-14)bstr is false when we refresh an env when dealing with SIGNATURE_CONSTRAINT*)
    if bstr then T.EXPLICIT_TYPE_VAR (id, tv, l, eqtv) else T.TYPE_VAR (F.freshTypeVar tv state, SOME (id, l), T.POLY, T.UNKNOWN)
  | freshty (T.TYPE_CONSTRUCTOR  (tn, sq,   l, eq))    tvl state bstr = T.TYPE_CONSTRUCTOR   (freshTypename tn     state,      freshseqty sq tvl state bstr, l, eq)
  | freshty (T.APPLICATION  (tf, sq,   l))    tvl state bstr = T.APPLICATION   (freshtypeFunction  tf tvl state bstr, freshseqty sq tvl state bstr, l)
  | freshty (T.TYPE_POLY (sq, i, p, k, l, eq)) tvl state bstr = T.TYPE_POLY  (freshseqty  sq tvl state bstr, if T.isPoly p then F.freshIdOr i state else i, T.MONO, k, l, eq)
  | freshty (T.GEN tys)             tvl state bstr = T.GEN (ref (map (fn ty => freshty ty tvl state bstr) (!tys)))
  | freshty (T.TYPE_DEPENDANCY  ety)             tvl state bstr = T.TYPE_DEPENDANCY (EL.mapExtLab ety (fn ty => freshty ty tvl state bstr))

(* bstr below has to be true so that we don't refresh explicit type variables that
 * are bound higher and so cannot be generalised.  The true is to say that explicit
 * type variables are here considered as constant types. *)
fun freshTy ty tvl P.POLY = freshty ty tvl (F.finitState ()) true
  | freshTy ty _ _ = ty

fun freshTypeFunction typeFunction bstr = freshtypeFunction typeFunction NONE (F.finitState ()) bstr

(* for some crazy reason there are two functions called
 * the same thing with different abbreviations. Busy working
 * on other naming issues at the moment, discover the difference
 * and rename. Adding a prime to the end for now *)
fun freshTypeVar' tyvar P.POLY = T.freshTypeVar ()
  | freshTypeVar' tyvar _      = tyvar

fun freshextgen (ext as ({id, bind, class, lab, poly}, labs, stts, deps)) tvl state bstr f =
    (C.consBind id (f bind tvl state bstr) class lab poly, labs, stts, deps)

fun freshextty x tvl state bstr = freshextgen x tvl state bstr freshty

fun freshvarenv idenv tvl state bstr =
    E.mapenv (fn semty => map (fn x => freshextty x tvl state bstr) semty) idenv

fun freshextseqty x tvl state bstr = freshextgen x tvl state bstr freshseqty

fun freshocsenv clenv tvl state bstr =
    E.mapenv (fn semty => map (fn x => freshextseqty x tvl state bstr) semty) clenv

fun freshexttypfun x tvl state bstr =
    freshextgen x tvl state bstr (fn (typfun, tnKind, varenv) =>
				  fn tvl                      =>
				  fn state                    =>
				  fn bstr                     =>
				     let val (cons, b) = !varenv
					 val _ = varenv := (freshvarenv cons tvl state bstr, b)
				     in (freshtypeFunction typfun tvl state bstr, tnKind, varenv)
				     end)

fun freshtypenv idenv tvl state bstr =
    E.mapenv (fn semty => map (fn x => freshexttypfun x tvl state bstr) semty) idenv

fun freshenv (E.ENV_VAR (ev, lab)) tvl state _ = E.ENV_VAR (F.freshEnvVar ev state, lab)
  | freshenv (env as E.ENV_CONS _) tvl state bstr =
    let val vids = freshvarenv (E.getValueIds env) tvl state bstr
	val typs = freshtypenv (E.getTypeNameEnv env) tvl state false (*bstr*)
	(*(2010-03-03)We want false for typs because the types
	 * have to be the same, and so we want to know when a type
	 * variables comes from an explicit type variable. *)
	val tyvs = E.getExplicitTypeVars env
	val strs = freshstrenv (E.getStructs env) tvl state bstr
	val sigs = freshstrenv (E.getSigs env) tvl state bstr
	val funs = E.getFunctors env
	val ovcs = freshocsenv (E.getOverloadingClasses env) tvl state bstr
	val info = E.getInfo env
    in E.consEnvConstructor vids typs tyvs strs sigs funs ovcs info
    end
  | freshenv (E.ROW_ENV (env1, env2)) tvl state bstr =
    E.ROW_ENV (freshenv env1 tvl state bstr, freshenv env2 tvl state bstr)
  | freshenv (E.ENVDEP (env, labs, stts, deps)) tvl state bstr =
    let val env' = freshenv env tvl state bstr
    in E.ENVDEP (env', labs, stts, deps)
    end
  | freshenv (E.TOP_LEVEL_ENV)   tvl state bstr = E.TOP_LEVEL_ENV
  | freshenv (E.ENVPOL _) tvl state bstr = raise EH.DeadBranch "this should have been built by now"
  | freshenv (E.LOCAL_ENV _) tvl state bstr = raise EH.DeadBranch "this should have been built by now"
  | freshenv (E.ENVWHR _) tvl state bstr = raise EH.DeadBranch "this should have been built by now"
  | freshenv (E.ENVSHA _) tvl state bstr = raise EH.DeadBranch "this should have been built by now"
  | freshenv (E.SIGNATURE_ENV _) tvl state bstr = raise EH.DeadBranch "this should have been built by now"
  | freshenv (E.DATATYPE_CONSTRUCTOR_ENV _) tvl state bstr = raise EH.DeadBranch "this should have been built by now"
  | freshenv (E.ENVOPN _) tvl state bstr = raise EH.DeadBranch "this should have been built by now"
  | freshenv (E.FUNCTOR_ENV _) tvl state bstr = raise EH.DeadBranch "this should have been built by now"
  | freshenv (E.CONSTRAINT_ENV _) tvl state bstr = raise EH.DeadBranch "this should have been built by now"
  | freshenv (E.ENVPTY _) tvl state bstr = raise EH.DeadBranch "this should have been built by now"
  | freshenv (E.ENVFIL _) tvl state bstr = raise EH.DeadBranch "this should have been built by now"
and freshextenv extenv tvl state bstr = freshextgen extenv tvl state bstr freshenv
and freshstrenv strenv tvl state bstr =
    E.mapenv (fn semty => map (fn x => freshextenv x tvl state bstr) semty) strenv

fun freshenv' env tvl bstr = freshenv env tvl (F.finitState ()) bstr




(***********************)
(* type reconstruction *)
(***********************)

fun buildClass (CL.CLVAR clv) state =
    (case S.getValStateCl state clv of
	 NONE => (CL.CLVAR clv, L.empty, L.empty, CD.empty)
       | SOME (cl, labs, stts, deps) => EL.updExtLab (buildClass cl state) labs stts deps)
  | buildClass class state = (class, L.empty, L.empty, CD.empty)

fun buildtnty (T.TYPENAME_VAR tnv) state =
    (case S.getValStateTn state tnv of
	 NONE => T.TYPENAME_VAR tnv
       | SOME tn => buildtnty tn state)
  | buildtnty (T.NC (tn, b, l)) _ = T.NC (tn, b, l)
  | buildtnty (T.TYPENAME_DEPENDANCY (tn, labs, stts, deps)) state =
    T.TYPENAME_DEPENDANCY (buildtnty tn state, labs, stts, deps)

fun buildlabty (T.LABEL_VAR lv) state =
    (case S.getValStateLt state lv of
	 NONE => T.LABEL_VAR lv
       | SOME lt => buildlabty lt state)
  | buildlabty (T.LC (lc, l)) _ = T.LC (lc, l)
  | buildlabty (T.LABEL_DEPENDANCY (lt, labs, stts, deps)) state =
    T.LABEL_DEPENDANCY (buildlabty lt state, labs, stts, deps)


(* - bmon is true if we want to build the monomorphic type variables - Is it still used?
 * - monfun is true if we want to turn all the variables into monomorphic ones.
 *   This is used when a function turns to be monomoprhic.
 *   false is the default value. *)
fun buildty (T.TYPE_VAR (tv, b, p, _)) state dom bmon monfun =
    (case (S.getValStateGe state tv, bmon) of
	 (SOME (_, labs, sts, asmp), false) => T.TYPE_DEPENDANCY (T.TYPE_VAR (tv, b, T.MONO, T.UNKNOWN), labs, sts, asmp)
       (*(2010-08-18)This should not return labs but put it on the type with T.DEP.*)
       | (x, _) => (case S.getValStateTv state tv of
			NONE =>
			let val poly = (*p*)(*N*)if Option.isSome x then p else T.POLY
			    (* If we choose p then the variable keeps the same status and so
			     * in 535 for example, we won't generalise foo's type and we will
			     * get a type error, if we choose the other solution then in 530
			     * for example, we won't find the error because when typing field2
			     * we will generalise its type.  We should do the second one and
			     * in StateEnv when pushing an env we should go down the
			     * structures as well.  This is the proper way to do it. *)
			    val poly = (*N*)if monfun then T.MONO else poly
			    val tv   = T.TYPE_VAR (tv, b, poly, T.UNKNOWN)
			in case x of
			       NONE => tv
			     | SOME (_, labs, stts, deps) => T.TYPE_DEPENDANCY (tv, labs, stts, deps)
			end
		      | SOME ty =>
			let val ty' = buildty ty state dom bmon monfun
			in case x of
			       NONE => ty'
			     | SOME (_, labs, stts, deps) => collapseTy ty' labs stts deps
			end))
  | buildty (T.EXPLICIT_TYPE_VAR (n, tv, lab, eqtv)) state dom bmon monfun =
    (case (S.getValStateGe state tv, I.isin n dom) of
	 (NONE, true) => T.TYPE_VAR (tv, SOME (n, lab), T.POLY, T.UNKNOWN)
       | (NONE, false) => T.EXPLICIT_TYPE_VAR (n, tv, lab, T.UNKNOWN)
       | (SOME (_, labs, stts, deps), _) => T.TYPE_DEPENDANCY (T.EXPLICIT_TYPE_VAR (n, tv, lab, T.UNKNOWN), labs, stts, deps))
  (*(case (S.getValStateGe state tv, bmon) of
	 (SOME (_, labs, stts, deps), false) => (D.printdebug2 ("(1)"); (T.EXPLICIT_TYPE_VAR (n, tv, lab), labs, stts, deps))
       | _ => (D.printdebug2 ("(2)"); (T.TYPE_VAR (tv, SOME lab, T.POLY), L.empty, L.empty, CD.empty)))*)
  | buildty (T.TYPE_CONSTRUCTOR (tn, sq, l, eq)) state dom bmon monfun =
    let val tn' = buildtnty  tn state
	val sq' = buildseqty sq state dom bmon monfun
    in T.TYPE_CONSTRUCTOR (tn', sq', l, eq)
    end
  | buildty (T.APPLICATION (typeFunction, seqty, lab)) state dom bmon monfun =
    let val typeFunction' = buildtypeFunction typeFunction state dom bmon monfun
	val seqty' = buildseqty seqty state dom bmon monfun
    in T.APPLICATION (typeFunction', seqty', lab)
    end
  | buildty (T.TYPE_POLY (sq, i, p, k, l, eq)) state dom bmon monfun =
    let val sq' = buildseqty sq state dom bmon monfun
    in T.TYPE_POLY (sq', i, p, k, l, eq)
    end
  | buildty (T.GEN tys) state dom bmon monfun =
    let val tys = map (fn ty => buildty ty state dom bmon monfun) (!tys)
    in T.GEN (ref tys)
    end
  (*(2010-06-23)We open a GEN when we build it when building an env.
   * It should only be opened when building an env. *)
  | buildty (T.TYPE_DEPENDANCY (ty, labs, stts, deps)) state dom bmon monfun =
    let val ty' = buildty ty state dom bmon monfun
    in collapseTy ty' labs stts deps
    end
and buildseqty (T.ROW_VAR sv) state dom bmon monfun =
    (case S.getValStateSq state sv of
	 NONE => T.ROW_VAR sv
       | SOME sq => buildseqty sq state dom bmon monfun)
  | buildseqty (T.ROW_C (rtl, flex, l)) state dom bmon monfun =
    let val rtl' = map (fn rt => buildfieldType rt state dom bmon monfun) rtl
    in T.ROW_C (rtl', flex, l)
    end
  | buildseqty (T.ROW_DEPENDANCY (sq, labs, stts, deps)) state dom bmon monfun =
    let val sq' = buildseqty sq state dom bmon monfun
    in collapseSq sq' labs stts deps
    end
and buildfieldType (T.FIELD_VAR rv) state dom bmon monfun =
    (case S.getValStateRt state rv of
	 NONE => T.FIELD_VAR rv
       | SOME rt => buildfieldType rt state dom bmon monfun)
  | buildfieldType (T.FC (lt, ty, l)) state dom bmon monfun =
    let val lt' = buildlabty lt state
	val ty' = buildty    ty state dom bmon monfun
    in T.FC (lt', ty', l)
    end
  | buildfieldType (T.FIELD_DEPENDANCY (field, labs, stts, deps)) state dom bmon monfun =
    let val field' = buildfieldType field state dom bmon monfun
    in collapseRt field' labs stts deps
    end
  | buildfieldType (x as T.FIELD_NO_OVERLOAD) _ _ _ _ = x
and buildtypeFunction (T.TYPE_FUNCTION_VAR tfv) state dom bmon monfun =
    (case S.getValStateTf state tfv of
	 NONE => T.TYPE_FUNCTION_VAR tfv
       | SOME tf => buildtypeFunction tf state dom bmon monfun)
  | buildtypeFunction (T.TFC (seqty, ty, lab)) state dom bmon monfun =
    let val seqty' = buildseqty seqty state dom bmon monfun
	val ty'    = buildty    ty    state dom bmon monfun
    in T.TFC (seqty', ty', lab)
    end
  | buildtypeFunction (T.TYPE_FUNCTION_DEPENDANCY (tf, labs, stts, deps)) state dom bmon monfun =
    let val tf' = buildtypeFunction tf state dom bmon monfun
    in collapseTf tf' labs stts deps
    end

fun buildVarEnv varenv state dom bmon =
    E.foldrienv (fn (id, sem, varenv) =>
		E.addenv (id, map (fn bd as (bind, labs, stts, deps) =>
				      ((*D.printdebug2 (C.printBind' bind T.printty);*)
				       if (*E.isMonoBind bd*)(*N*)false
				       then (bind, labs, stts, deps)
				       else let (*N*)val (dom, monfun) = if E.isMonoBind bd then (I.empty, true) else (dom, false)
						val ty = buildty (C.getBindT bind) state dom bmon monfun
					    (*val _ = D.printdebug2 (T.printty (C.getBindT bind) ^ "\n" ^ T.printty ty)*)
					    (*val _ = D.printdebug2 (S.printState state)*)
					    in (C.mapBind bind (fn _ => ty), labs, stts, deps)
					    end))
				  sem)
			 varenv)
		E.emvar
		varenv

fun buildIdEnv idenv state fresh ffresh fbuild bstr =
    E.foldrienv (fn (id, sem, genenv) =>
		    E.addenv (id, map (fn bd as (bind, labs, stts, deps) =>
					  (*(2010-07-02)We want to build the type of the exceptions so
					   * that we can refresh them in the case of an opaque signature.
					   * An exception is the only monorphic specification that we
					   * can have in a signature.
					   * Why do we want to refresh the type of exceptions? *)
					  if E.isMonoBind bd andalso not (CL.classIsEXC (E.getBindC bd))
					  then (bind, labs, stts, deps)
					  else let val bmon = true
						   val b  = fbuild (C.getBindT bind) state fresh I.empty bmon bstr
						   val b' = case fresh of
								NONE => b
							      | SOME fstate => ffresh b (SOME O.empty) fstate bstr
					       in (C.mapBind bind (fn _ => b'), labs, stts, deps)
					       end)
				      sem)
			     genenv)
		E.emptyMap
		idenv

fun buildnobuild x _ _ _ _ _ = x

val buildTyVar = buildnobuild

fun freshTypeVar (tyvar, b) tvl fresh bstr = (F.freshTypeVar tyvar fresh, b)

fun buildTy' ty state fresh dom bmon bstr = buildty ty state dom bmon false

fun buildSeqTy' seqty state fresh dom bmon bstr = buildseqty seqty state dom bmon false

fun buildTypSem (typfun, tnKind, cons) state fresh dom bmon bstr =
    let val typfun' = buildtypeFunction typfun state dom bmon false
    in (typfun', tnKind, cons)
    end

fun freshTypSem (typfun, tnKind, cons) tvl fresh bstr =
    (freshtypeFunction typfun tvl fresh bstr, tnKind, cons)

fun freshEnv env tvl fresh bstr = env

val buildFuns = buildnobuild

fun freshFuns funs tvl fresh bstr = funs

fun buildEnv (E.ENV_VAR (ev, lab)) state fresh bstr =
    (case S.getValStateEv state ev of
	 NONE => E.ENV_VAR (ev, lab)
       | SOME env =>
	 let val env' = buildEnv env state fresh bstr
	 (*(2010-06-22)We can't just push into env' in case env' is, say, an empty structure.
	  * We also need to gather these. *)
	 in env'
	 end)
  | buildEnv (E.ROW_ENV (env1, env2)) state fresh bstr =
    let val env1 = buildEnv env1 state fresh bstr
	val env2 = buildEnv env2 state fresh bstr
    in E.ROW_ENV (env1, env2)
    end
  | buildEnv (env as E.ENV_CONS _) state fresh bstr =
    E.consEnvConstructor (buildIdEnv (E.getValueIds env) state fresh freshty     buildTy'    bstr)
	       (buildIdEnv (E.getTypeNameEnv env) state fresh freshTypSem buildTypSem false)
	       (buildIdEnv (E.getExplicitTypeVars env) state fresh freshTypeVar  buildTyVar  bstr)
	       (buildIdEnv (E.getStructs env) state fresh freshEnv    buildEnv'   bstr)
	       (buildIdEnv (E.getSigs env) state fresh freshEnv    buildEnv'   bstr)
	       (buildIdEnv (E.getFunctors env) state fresh freshFuns   buildFuns   bstr)
	       (buildIdEnv (E.getOverloadingClasses env) state fresh freshseqty  buildSeqTy' bstr)
	       (E.getInfo env)
  | buildEnv (env as E.ENVDEP (env0, labs0, stts0, deps0)) state fresh bstr =
    let val env1 = buildEnv env0 state fresh bstr
    in E.pushExtEnv env1 labs0 stts0 deps0
    end
  | buildEnv env state fresh bstr = (D.printdebug2 (E.printEnv env ""); raise EH.DeadBranch "")
and buildEnv' env state fresh dom bmon bstr = buildEnv env state fresh bstr

fun buildFEnv env state bstr = buildEnv env state (SOME (F.finitState ())) bstr

fun justBuildEnv env state bstr = buildEnv env state NONE bstr

fun getExplicitTyVars vids tyvs state =
    let val tyvs' = E.foldrienv (fn (id, sem, tyvenv) =>
				    E.addenv (id, List.filter (fn bind => not (CL.classIsANY (E.getBindC bind)))
							      sem)
					     tyvenv)
				E.emtv
				tyvs
	(* NOTE: tyvs' is tyvs without the dummy bindings. *)
	fun getLabsTyvs id = case E.plusproj tyvs' id of
				 [bind] => EL.getExtLabL bind
			       | _ => raise EH.DeadBranch ""
	val dom = E.dom tyvs'
	fun searchTy (T.TYPE_VAR _) = NONE
	  | searchTy (T.EXPLICIT_TYPE_VAR (id, _, lab, _)) =
	    if I.isin id dom
	    then SOME (id, lab, getLabsTyvs id, L.empty, CD.empty)
	    else NONE
	  | searchTy (T.TYPE_CONSTRUCTOR (_, seqty, _, _)) = searchSeqTy seqty
	  | searchTy (T.APPLICATION (typeFunction, seqty, lab)) =
	    let val err = searchTypeFunction typeFunction
	    in if Option.isSome err
	       then err
	       else searchSeqTy seqty
	    end
	  | searchTy (T.TYPE_POLY (seqty, _, _, _, _, _)) = searchSeqTy seqty
	  | searchTy (T.GEN ty) = raise EH.TODO "no description, raised in the 'searchTy' function of Unification.sml" (*(2010-06-23)Should be impossible.*)
	  | searchTy (T.TYPE_DEPENDANCY (ty, labs, stts, deps)) =
	    (case searchTy ty of
		 NONE => NONE
	       | SOME (id, lab, labs', stts', deps') =>
		 SOME (id, lab, L.union labs labs', L.union stts stts', CD.union deps deps'))
	and searchSeqTy (T.ROW_VAR _) = NONE
	  | searchSeqTy (T.ROW_C (fields, _, _)) =
	    foldr (fn (field, err) =>
		      if Option.isSome err
		      then err
		      else searchField field)
		  NONE
		  fields
	  | searchSeqTy (T.ROW_DEPENDANCY (seq, labs, stts, deps)) =
	    (case searchSeqTy seq of
		 NONE => NONE
	       | SOME (id, lab, labs', stts', deps') =>
		 SOME (id, lab, L.union labs labs', L.union stts stts', CD.union deps deps'))
	and searchField (T.FIELD_VAR _) = NONE
	  | searchField (T.FC (_, ty, _)) = searchTy ty
	  | searchField (T.FIELD_DEPENDANCY (field, labs, stts, deps)) =
	    (case searchField field of
		 NONE => NONE
	       | SOME (id, lab, labs', stts', deps') =>
		 SOME (id, lab, L.union labs labs', L.union stts stts', CD.union deps deps'))
	  | searchField T.FIELD_NO_OVERLOAD = NONE
	and searchTypeFunction (T.TYPE_FUNCTION_VAR _) = NONE
	  | searchTypeFunction (T.TFC (seqty, ty, _)) =
	    let val err = searchSeqTy seqty
	    in if Option.isSome err
	       then err
	       else searchTy ty
	    end
	  | searchTypeFunction (T.TYPE_FUNCTION_DEPENDANCY (tf, labs, stts, deps)) =
	    (case searchTypeFunction tf of
		 NONE => NONE
	       | SOME (id, lab, labs', stts', deps') =>
		 SOME (id, lab, L.union labs labs', L.union stts stts', CD.union deps deps'))
    in if I.isEmpty dom
       then NONE
       else E.foldrienv (fn (_, sem, err) =>
			    if Option.isSome err
			    then err
			    else foldr (fn (bd as (bind, labs, stts, deps), err) =>
					   if Option.isSome err
					   then err
					   else let val ty =
							if (*E.isMonoBind bd*)(*N*)false
							then buildty (C.getBindT bind) state I.empty true false
							else C.getBindT bind
						    (*val _ = D.printdebug2 (S.printState state)*)
						    (*val _ = D.printdebug2 (T.printty ty)*)
						in case searchTy ty of
						       NONE => NONE
						     | SOME (id, lab, labs', stts', deps') =>
						       SOME (id, lab, L.union labs labs', L.union stts stts', CD.union deps deps')
						end)
				       NONE
				       sem)
			NONE
			vids
    end

fun getGenTyvars (T.TYPE_VAR (_, SOME idl, _, _)) = [idl]
  | getGenTyvars (T.TYPE_VAR _) = []
  | getGenTyvars (T.EXPLICIT_TYPE_VAR _) = []
  | getGenTyvars (T.TYPE_CONSTRUCTOR (_, seq, _, _)) = getGenTyvarsSeq seq
  | getGenTyvars (T.APPLICATION _) = []
  | getGenTyvars (T.TYPE_POLY (seq, _, _, _, _, _)) = getGenTyvarsSeq seq
  | getGenTyvars (T.GEN tys) =
    foldr (fn (ty, tvlabs) => (getGenTyvars ty) @ tvlabs) [] (!tys)
  | getGenTyvars (T.TYPE_DEPENDANCY ety) = getGenTyvars (EL.getExtLabT ety)

and getGenTyvarsSeq (T.ROW_VAR _) = []
  | getGenTyvarsSeq (T.ROW_C (fields, _, _)) =
    foldr (fn (field, tvlabs) => (getGenTyvarsField field) @ tvlabs) [] fields
  | getGenTyvarsSeq (T.ROW_DEPENDANCY eseq) = getGenTyvarsSeq (EL.getExtLabT eseq)

and getGenTyvarsField (T.FIELD_VAR _) = []
  | getGenTyvarsField (T.FC (_, ty, _)) = getGenTyvars ty
  | getGenTyvarsField (T.FIELD_DEPENDANCY efield) = getGenTyvarsField (EL.getExtLabT efield)
  | getGenTyvarsField T.FIELD_NO_OVERLOAD = []


fun extractTyGen (T.GEN _)   true  = [T.newTYPE_VAR ()]
  | extractTyGen (T.GEN tys) false = !tys
  | extractTyGen ty _ = [ty]


fun checkIsNotTooGen bind1 bind2 bfun lab =
    if CL.classIsVAL (E.getBindC bind2) andalso E.isMonoBind bind2
    then let val tys = extractTyGen (E.getBindT bind1) bfun
	     val ls  = foldr (fn (ty, tvlabs) => (getGenTyvars ty) @ tvlabs) [] tys
	 in case ls of
		((id1, lab1) :: _) =>
		let val labs = L.union  (EL.getExtLabL bind1) (EL.getExtLabL bind2)
		    val stts = L.union  (EL.getExtLabE bind1) (EL.getExtLabE bind2)
		    val deps = CD.union (EL.getExtLabD bind1) (EL.getExtLabD bind2)
		    val mono = P.getLabsPoly (E.getBindP bind2)
		    val lab2 = E.getBindL bind2
		    val id2  = E.getBindI bind2
		    val ek   = EK.TooGenSig ((L.toInt lab2, I.toInt id2), (L.toInt lab1, I.toInt id1), L.toList (#1 mono))
		    val err  = ERR.consPreError ERR.dummyId (L.cons lab labs) deps ek stts
		in raise errorfound err
		end
	      | _ => ()
	 end
    else ()

fun idInIdEnv idenv lab id =
    case E.plusproj idenv id of
	[bind] =>
	L.eq lab (E.getBindL bind) andalso
	not (CL.classIsANY (E.getBindC bind))
      | _ => false

fun idInEnv (env as E.ENV_CONS _) lab id =
    let val lab = L.fromInt lab
	val id  = I.fromInt id
    in idInIdEnv (E.getValueIds env) lab id orelse
       idInIdEnv (E.getTypeNameEnv env) lab id orelse
       idInIdEnv (E.getStructs env) lab id orelse
       idInIdEnv (E.getSigs env) lab id orelse (* We don't need this one *)
       idInIdEnv (E.getFunctors env) lab id orelse
       idInIdEnv (E.getOverloadingClasses env) lab id        (* We don't need this one *)
    end
  | idInEnv _ _ _ = false

fun completeEnv env (SOME err) =
    E.completeEnv env
    orelse
    (case ERR.getK err of
	 EK.Unmatched (_, ids, _) =>
	 List.all (fn (lab, id) => idInEnv env lab id) ids
       | _ => false)
  | completeEnv env _ = E.completeEnv env

(* Generalises the structure env2 so that it is equal to
 * the signature env1.
 * For functors, b is always true and bfun will be false. *)
fun matchSigStr env1 env2 l filters labs stts deps bfun err =
    let (*val _ = D.printdebug2 ("signature\n" ^ E.printEnv env1 "")*)
	(*val _ = D.printdebug2 ("structure\n" ^ E.printEnv env2 "")*)
	(* etv is for the signature *)
	(* if there is nothing corresponding in the structure, it's an error *)
	(* btype is true if we are currently composing for types. *)
	fun genError lab id labs stts deps =
	    if completeEnv env2 err (*labs*)
	    then let val (idlabs, labs1) = E.getLabsIdsEnv env2 1
		     val lab'  = E.getLabEnv env2
		     val labs2 = L.cons l (L.union labs1 labs)
		     val ek    = EK.Unmatched ((L.toInt lab, I.toInt id), idlabs, L.toInt lab')
		     (*val _     = D.printdebug2 (E.printEnv env2 "")*)
		 in raise errorfound (ERR.consPreError ERR.dummyId labs2 deps ek stts)
		 end
	    else ()
	fun compone id [] bind =
	    (if CL.classIsANY (E.getBindC bind)
	     then ()
	     else genError (E.getBindL bind)
			   id
			   (L.union  labs (EL.getExtLabL bind))
			   (L.union  stts (EL.getExtLabE bind))
			   (CD.union deps (EL.getExtLabD bind)); [])
	  (* btype iff we're dealing with type names *)
	  | compone _ es bind1 = (* bind1 is signature *)
	    foldr (fn (bind2, cs) =>
		      if CL.classIsANY (E.getBindC bind1) orelse CL.classIsANY (E.getBindC bind2)
		      then cs
		      else let val _    = checkIsNotTooGen bind1 bind2 bfun l
			       val tys1 = extractTyGen (E.getBindT bind1) bfun
			       val ty2  = E.getBindT bind2
			       val labs = L.union  labs (L.cons l (L.union (EL.getExtLabL bind1) (EL.getExtLabL bind2)))
			       val stts = L.union  stts (L.union  (EL.getExtLabE bind1) (EL.getExtLabE bind2))
			       val deps = CD.union deps (CD.union (EL.getExtLabD bind1) (EL.getExtLabD bind2))
			       fun getTy2 () = if bfun then ty2 else freshty ty2 (SOME O.empty) (F.finitState ()) false
			       val cs'  = map (fn ty1 => ((*D.printdebug2 (T.printty ty1 ^ "\n" ^
									 T.printty ty2 ^ "\n" ^
									 L.toString labs);*)
							  E.genCstTyAll ty1 (getTy2 ()) labs stts deps))
					      tys1
			   in cs' @ cs
			   end) [] es
	fun componeTyp id [] bind =
	    (if CL.classIsANY (E.getBindC bind)
	     then ()
	     else genError (E.getBindL bind)
			   id
			   (L.union  labs (EL.getExtLabL bind))
			   (L.union  stts (EL.getExtLabE bind))
			   (CD.union deps (EL.getExtLabD bind)); [])
	  | componeTyp _ es bind1 =
	    List.mapPartial
		(fn bind2 =>
		    if CL.classIsANY (E.getBindC bind1) orelse CL.classIsANY (E.getBindC bind2)
		    then NONE
		    else let val (tyf1, tnKind1, cons1) = E.getBindT bind1
			     val (tyf2, tnKind2, cons2) = E.getBindT bind2
			     (*val _ = D.printdebug2 (T.printtyf tyf1 ^ "\n" ^ T.printtyf tyf2)*)
			     val labs = L.union  labs (L.cons l (L.union (EL.getExtLabL bind1) (EL.getExtLabL bind2)))
			     val stts = L.union  stts (L.union  (EL.getExtLabE bind1) (EL.getExtLabE bind2))
			     val deps = CD.union deps (CD.union (EL.getExtLabD bind1) (EL.getExtLabD bind2))
			 in SOME (E.genCstTfAll tyf1 tyf2 labs stts deps)
			 end)
		es
	fun componestr id [] bind =
	    (if CL.classIsANY (E.getBindC bind)
	     then ()
	     else genError (E.getBindL bind)
			   id
			   (L.union  labs (EL.getExtLabL bind))
			   (L.union  stts (EL.getExtLabE bind))
			   (CD.union deps (EL.getExtLabD bind));
	     ([], []))
	    (* b false means that we're dealing with a where clause as a structure *)
	  | componestr _ es bind1 =
	    foldr (fn (bind2, (cstV, cstT)) =>
		      if CL.classIsANY (E.getBindC bind1) orelse CL.classIsANY (E.getBindC bind2)
		      then (cstV, cstT)
		      else let val env1 = E.getBindT bind1
			       val env2 = E.getBindT bind2
			       val labs = L.union  labs (L.union  (EL.getExtLabL bind1) (EL.getExtLabL bind2))
			       val stts = L.union  stts (L.union  (EL.getExtLabE bind1) (EL.getExtLabE bind2))
			       val deps = CD.union deps (CD.union (EL.getExtLabD bind1) (EL.getExtLabD bind2))
			       val (x, y) = matchSigStr env1 env2 l filters labs stts deps bfun err
				   handle errorfound err => raise errorfound (ERR.labelError err labs stts deps)
			   in ((decorateCst x labs stts deps) @ cstV, (decorateCst y labs stts deps) @ cstT)
			   end)
		  ([], [])
		  es
	fun compG semty id env =
	    map (fn etv => compone id (E.plusproj (E.getValueIds env) id) etv) semty
	fun compT semty tn env = (* env: structure, etv: signature *)
	    map (fn etv =>
		    (()(*checkDatSigStruc (E.plusproj (E.getTypeNameEnv env) tn) etv labs deps asmp l tn*);
		     componeTyp tn (E.plusproj (E.getTypeNameEnv env) tn) etv))
		semty
	  (* We should also check the NONs, because these are errors.
	   * SML/NJ says: Error: type t must be a datatype.
	   * --> checkDatSigTypStruc *)
	  (* We should aslo check that all the conss are declared along with the datatype in the signature!
	   * It seems to be the only case when we have something in the structure that have to be in the signature.
	   * --> checkAllConsInSig *)
	fun compS semty id env =
	    map (fn exv => componestr id (E.plusproj (E.getStructs env) id) exv) semty
	fun compGen idenv fcomp env =
	    E.foldrienv (fn (id, semty, cs) => (fcomp semty id env) @ cs) [] idenv
	fun linkstr env1 env2 =
	    let (* is there something in 'getValueIds env1' ?*)
		val csG = compGen (E.getValueIds env1) compG env2
		val csT = compGen (E.getTypeNameEnv env1) compT env2
		val csS = compGen (E.getStructs env1) compS env2
		val (csSV, csST) = ListPair.unzip csS
		(*val _ = D.printdebug2 (E.printEnv (E.CONSTRAINT_ENV (E.singcsts (L.dummyLab, List.concat csST))) "")*)
	    in (List.concat (csG @ csSV), List.concat (csT @ csST))
	    end
    in case (env1, env2) of (* env1/env2 : signature/structure *)
	   (E.ENV_CONS _, E.ENV_CONS _) => linkstr env1 env2
	 | (E.ENVDEP (env1, labs', stts', deps'), env2) =>
	   let val labs0 = L.union  labs labs'
	       val stts0 = L.union  stts stts'
	       val deps0 = CD.union deps deps'
	   in matchSigStr env1 env2 l filters labs0 stts0 deps0 bfun err
	   end
	 | (env1, E.ENVDEP (env2, labs', stts', deps')) =>
	   let val labs0 = L.union  labs labs'
	       val stts0 = L.union  stts stts'
	       val deps0 = CD.union deps deps'
	   in matchSigStr env1 env2 l filters labs0 stts0 deps0 bfun err
	   end
	 | _ => ([], [])
    end

(* NOTE: if filterLid lid filter = SOME (lid', true) then lid = lid' *)
fun filterLid (lid as I.ID (id, lab)) filters =
    if FI.testtodo filters lab
    then SOME (lid, true)
    else NONE
  | filterLid (I.LID ((id, lab1), lid, lab2)) filters =
    if FI.testtodo filters lab1
    then if FI.testtodo filters lab2
	 then case filterLid lid filters of
		  NONE               => SOME (I.ID (id, lab1), false)
		| SOME (lid', true)  => SOME (I.LID ((id, lab1), lid', lab2), true)
		| SOME (lid', false) => SOME (I.LID ((id, lab1), lid', lab2), false)
	 else SOME (I.ID (id, lab1), false)
    else NONE

fun unionLabs (labs1, stts1, deps1) (labs2, stts2, deps2) =
    (L.union labs1 labs2,
     L.union stts1 stts2,
     CD.union deps1 deps2)




(* Extracts the type functions from a structure.
 * env1 is for the signature and env2 for the structure. *)

fun domTFun tfun =
    OM.foldri (fn (i, _, set) => O.cons i set)
	      O.empty
	      tfun

(* (2010-06-04) Why does the seqty has to be a SC? *)
fun mergeTypeFunction tfn1 tfn2 =
    OM.unionWith (fn (tf1, tf2 as T.TFC ((*T.ROW_C*) _, _, _)) => ((*D.printdebug2 (T.printtyf tf1 ^ "\n" ^ T.printtyf tf2);*) tf2)
		   | (tf1, tf2) => ((*D.printdebug2 (T.printtyf tf1 ^ "\n" ^ T.printtyf tf2);*) tf2))
		 (*(2010-07-06)This is to fix, we don't want to just keep the second one!*)
		 (tfn1, tfn2)

fun decorateTypeFunction tfn labs stts deps =
    OM.map (fn tf => collapseTf tf labs stts deps)
	   tfn

fun newTypeFunction () = T.TFC (T.newROW_VAR (), T.newTYPE_VAR (), L.dummyLab)

fun insertInTypeFunction typeFunction name tf =
    let val tn = T.typenameToInt name
    (*What about when we already have a tn entry in typeFunction because of
     *some sharing?*)
    in OM.insert (typeFunction, tn, tf)
    end

fun getAllTypeFunctionEnv (env as E.ENV_CONS _) =
    let val (tfnDs1, tfnTs1) =
	    foldr (fn ({id, lab, kind, name}, (tfnDs, tfnTs)) =>
		      case kind of
			  E.DATATYPE => (insertInTypeFunction tfnDs name (newTypeFunction ()), tfnTs)
			| E.TYPE => (tfnDs, insertInTypeFunction tfnTs name (newTypeFunction ())))
		  (OM.empty, OM.empty)
		  (E.getITypeNames env)
	val (tfnDs2, tfnTs2) = getAllTypeFunctionStrEnv (E.getStructs env)
    in (mergeTypeFunction tfnDs1 tfnDs2, mergeTypeFunction tfnTs1 tfnTs2)
    end
  | getAllTypeFunctionEnv (E.ROW_ENV (env1, env2)) =
    let val (tfnDs1, tfnTs1) = getAllTypeFunctionEnv env1
	val (tfnDs2, tfnTs2) = getAllTypeFunctionEnv env2
    in (mergeTypeFunction tfnDs1 tfnDs2, mergeTypeFunction tfnTs1 tfnTs2)
    end
  | getAllTypeFunctionEnv (E.ENVDEP (env, labs, stts, deps)) =
    let val (tfnDs, tfnTs) = getAllTypeFunctionEnv env
    in (decorateTypeFunction tfnDs labs stts deps, decorateTypeFunction tfnTs labs stts deps)
    end
  | getAllTypeFunctionEnv (E.ENV_VAR _) = (OM.empty, OM.empty)
  | getAllTypeFunctionEnv (E.TOP_LEVEL_ENV)   = (OM.empty, OM.empty)
  | getAllTypeFunctionEnv (E.ENVOPN _) = raise EH.DeadBranch "This should have been built by now"
  | getAllTypeFunctionEnv (E.FUNCTOR_ENV _) = raise EH.DeadBranch "This should have been built by now"
  | getAllTypeFunctionEnv (E.CONSTRAINT_ENV _) = raise EH.DeadBranch "This should have been built by now"
  | getAllTypeFunctionEnv (E.ENVPOL _) = raise EH.DeadBranch "This should have been built by now"
  | getAllTypeFunctionEnv (E.DATATYPE_CONSTRUCTOR_ENV _) = raise EH.DeadBranch "This should have been built by now"
  | getAllTypeFunctionEnv (E.LOCAL_ENV _) = raise EH.DeadBranch "This should have been built by now"
  | getAllTypeFunctionEnv (E.ENVWHR _) = raise EH.DeadBranch "This should have been built by now"
  | getAllTypeFunctionEnv (E.ENVSHA _) = raise EH.DeadBranch "This should have been built by now"
  | getAllTypeFunctionEnv (E.SIGNATURE_ENV _) = raise EH.DeadBranch "This should have been built by now"
  | getAllTypeFunctionEnv (E.ENVPTY _) = raise EH.DeadBranch "This should have been built by now"
  | getAllTypeFunctionEnv (E.ENVFIL _) = raise EH.DeadBranch "This should have been built by now"

and getAllTypeFunctionStrEnv strenv =
    E.foldrienv
	(fn (id, semty, (tfnDs, tfnTs)) =>
	    foldr (fn (extenv, (tfnDs, tfnTs)) =>
		      let val (tfnDs', tfnTs') = getAllTypeFunctionEnv (E.getBindT extenv)
		      in (mergeTypeFunction tfnDs tfnDs', mergeTypeFunction tfnTs tfnTs')
		      end)
		  (tfnDs, tfnTs)
		  semty)
	(OM.empty, OM.empty)
	strenv

fun getTypeFunctionEnv (env1 as E.ENV_CONS _) (env2 as E.ENV_CONS _) labs stts deps =
    let val (tfnDs1, tfnTs1) =
	    foldr (fn ({id, lab, kind, name}, (tfnDs, tfnTs)) =>
		      case E.plusproj (E.getTypeNameEnv env2) id of
			  [] => if E.getIComplete env2 (* The structure/realisation is complete. *)
				then (tfnDs, tfnTs) (* not in the incomplete structure/realisation so we don't want to generalise *)
				else (case kind of
					  E.DATATYPE => (insertInTypeFunction tfnDs name (newTypeFunction ()), tfnTs)
					| E.TYPE => (tfnDs, insertInTypeFunction tfnTs name (newTypeFunction ())))
			| [etv] =>
			  let val ty = case E.getBindT etv of
					   (tf, _, _) => (* What do we need the T.ROW_C for? *)
					   (case collapseTf tf labs stts deps of
						T.TYPE_FUNCTION_DEPENDANCY (T.TFC ((*T.ROW_C*) _, _, _), _, _, _) =>
						collapseTf tf (EL.getExtLabL etv) (EL.getExtLabE etv) (EL.getExtLabD etv)
					      | _ => newTypeFunction ())
			  (* Here we put the new mapping in tfnT because we've got a match
			   * in the structure/realisation and so the even if name comes from
			   * a datatype specification, we want to rename it with what we got
			   * in the structure/realisation*)
			  in (tfnDs, insertInTypeFunction tfnTs name ty)
			  end
			| _ => raise EH.DeadBranch "There should be only one binding per identifier during constraint solving")
		  (OM.empty, OM.empty)
		  (E.getITypeNames env1)
	val (tfnDs2, tfnTs2) = getTypeFunctionStrEnv (E.getStructs env1) (E.getStructs env2)
	(*val _ = D.printdebug2 (printTFun tfnTs1 ^ "\n" ^ printTFun tfnTs2)*)
    in (mergeTypeFunction tfnDs1 tfnDs2, mergeTypeFunction tfnTs1 tfnTs2)
    end
  | getTypeFunctionEnv (E.ENVDEP (env1, labs', stts', deps')) env2 labs stts deps =
    getTypeFunctionEnv env1
		env2
		(L.union labs labs')
		(L.union stts stts')
		(CD.union deps deps')
  | getTypeFunctionEnv env1 (E.ENVDEP (env2, labs', stts', deps')) labs stts deps =
    getTypeFunctionEnv env1
		env2
		(L.union labs labs')
		(L.union stts stts')
		(CD.union deps deps')
  | getTypeFunctionEnv (env as E.ENV_CONS _) _ _ _ _ = getAllTypeFunctionEnv env
  | getTypeFunctionEnv (E.ROW_ENV (env0, env1)) env2 labs stts deps =
    let val (tfnDs1, tfnTs1) = getTypeFunctionEnv env0 env2 labs stts deps
	val (tfnDs2, tfnTs2) = getTypeFunctionEnv env1 env2 labs stts deps
    in (mergeTypeFunction tfnDs1 tfnDs2, mergeTypeFunction tfnTs1 tfnTs2)
    end
  | getTypeFunctionEnv (E.ENV_VAR _) _ _ _ _ = (OM.empty, OM.empty)
  | getTypeFunctionEnv (E.TOP_LEVEL_ENV)   _ _ _ _ = (OM.empty, OM.empty)
  | getTypeFunctionEnv (E.ENVOPN _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | getTypeFunctionEnv (E.FUNCTOR_ENV _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | getTypeFunctionEnv (E.CONSTRAINT_ENV _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | getTypeFunctionEnv (E.ENVPOL _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | getTypeFunctionEnv (E.DATATYPE_CONSTRUCTOR_ENV _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | getTypeFunctionEnv (E.LOCAL_ENV _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | getTypeFunctionEnv (E.ENVWHR _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | getTypeFunctionEnv (E.ENVSHA _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | getTypeFunctionEnv (E.SIGNATURE_ENV _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | getTypeFunctionEnv (E.ENVPTY _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | getTypeFunctionEnv (E.ENVFIL _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"

and getTypeFunctionExtEnv extenv extenv' =
    let (*val labs = L.union  (EL.getExtLabL extenv) (EL.getExtLabL extenv')
	val stts = L.union  (EL.getExtLabE extenv) (EL.getExtLabE extenv')
	val deps = CD.union (EL.getExtLabD extenv) (EL.getExtLabD extenv')*)
	val labs = EL.getExtLabL extenv'
	val stts = EL.getExtLabE extenv'
	val deps = EL.getExtLabD extenv'
	val (tfnD, tfnT)  = getTypeFunctionEnv (E.getBindT extenv) (E.getBindT extenv') labs stts deps
    in (tfnD, tfnT)
    end

and getTypeFunctionStrEnv strenv strenv' =
    E.foldrienv
	(fn (id, semty, (tfnDs, tfnTs)) =>
	    foldr (fn (extenv, tfns) =>
		      foldr (fn (extenv', (tfnDs, tfnTs)) =>
				let val (tfnDs', tfnTs') = getTypeFunctionExtEnv extenv extenv'
				in (mergeTypeFunction tfnDs tfnDs', mergeTypeFunction tfnTs tfnTs')
				end)
			    (tfnDs, tfnTs)
			    (E.plusproj strenv' id))
		  (tfnDs, tfnTs)
		  semty)
	(OM.empty, OM.empty)
	strenv




fun mergeTypeFunctionSha tfn1 tfn2 =
    OM.unionWith (fn (_, x as SOME _) => x
		   | (x, y) => x)
		 (tfn1, tfn2)

fun decorateTypeFunctionSha tfn labs stts deps =
    OM.map (fn (SOME (labs', stts', deps')) =>
	       SOME (L.union  labs labs',
		     L.union  stts stts',
		     CD.union deps deps')
	     | NONE => NONE)
	   tfn

fun decorateUTypeFunctionSha (SOME utf) labs stts deps = SOME (EL.updExtLab utf labs stts deps)
  | decorateUTypeFunctionSha NONE       _    _    _    = NONE

(* TODO: we should extract the dependencies from both utf1 and utf2 and put them on x using collapseTf. *)
fun mergeUTypeFunctionSha (SOME utf1) (SOME utf2) = SOME (EL.unionExtLab utf1 utf2 (fn (x, y) => x))
  | mergeUTypeFunctionSha (SOME utf)  NONE        = SOME utf
  | mergeUTypeFunctionSha NONE        (SOME utf)  = SOME utf
  | mergeUTypeFunctionSha NONE        NONE        = NONE

fun getTypeFunctionEnvSha (env1 as E.ENV_CONS _) (env2 as E.ENV_CONS _) =
    let val (utfT, tfnT) =
	    foldr (fn ({id, lab, kind, name}, (utf, tfns)) =>
		      case E.plusproj (E.getTypeNameEnv env2) id of
			  [] => if E.getIComplete env2 (* The sharing structure is complete. *)
				then (utf, tfns)  (* not in the incomplete sharing structure so we don't want to generalise *)
				else (utf, OM.insert (tfns, T.typenameToInt name, NONE))
			| [etv] =>
			  if CL.classIsANY (E.getBindC etv)
			  then (utf, OM.insert (tfns, T.typenameToInt name, NONE))
			  else (case E.plusproj (E.getTypeNameEnv env1) id of
				    [] => (utf, OM.insert (tfns, T.typenameToInt name, NONE))
				  | [etv'] => (case E.getBindT etv' of
						   (tf, _, _) =>
						   (case T.getTypename tf of
							SOME _ =>
							let val labs  = L.union  (EL.getExtLabL etv) (EL.getExtLabL etv')
							    val stts  = L.union  (EL.getExtLabE etv) (EL.getExtLabE etv')
							    val deps  = CD.union (EL.getExtLabD etv) (EL.getExtLabD etv')
							    val utf'  = SOME (tf, labs, stts, deps)
							    val v     = SOME (labs, stts, deps)
							    val tfns' = OM.insert (tfns, T.typenameToInt name, v)
							in (mergeUTypeFunctionSha utf utf', tfns')
							end
						      | NONE => (utf, OM.insert (tfns, T.typenameToInt name, NONE))))
				  | _  => raise EH.DeadBranch "There should be only one binding per identifier during constraint solving")
			| _ => raise EH.DeadBranch "There should be only one binding per identifier during constraint solving")
		  (NONE, OM.empty)
		  (E.getITypeNames env1)
	val (utfS, tfnS) = getTypeFunctionShaStrEnv (E.getStructs env1) (E.getStructs env2)
    in (mergeUTypeFunctionSha utfT utfS, mergeTypeFunctionSha tfnT tfnS)
    end
  | getTypeFunctionEnvSha (E.ENVDEP (env1, labs, stts, deps)) env2 =
    let val (utf, tfn) = getTypeFunctionEnvSha env1 env2
    in (decorateUTypeFunctionSha utf labs stts deps,
	decorateTypeFunctionSha  tfn labs stts deps)
    end
  | getTypeFunctionEnvSha env1 (E.ENVDEP (env2, labs, stts, deps)) =
    let val (utf, tfn) = getTypeFunctionEnvSha env1 env2
    in (decorateUTypeFunctionSha utf labs stts deps,
	decorateTypeFunctionSha  tfn labs stts deps)
    end
  | getTypeFunctionEnvSha (env as E.ENV_CONS _) _ =
    let val (tfnD, tfnT) = getAllTypeFunctionEnv env
	val tfn1 = mergeTypeFunction tfnD tfnT
	val tfn2 = OM.map (fn _ => NONE) tfn1
    in (NONE, tfn2)
    end
  | getTypeFunctionEnvSha (E.ROW_ENV (env0, env1)) env2 =
    let val (utf1, tfn1) = getTypeFunctionEnvSha env0 env2
	val (utf2, tfn2) = getTypeFunctionEnvSha env1 env2
    in (mergeUTypeFunctionSha utf1 utf2, mergeTypeFunctionSha tfn1 tfn2)
    end
  | getTypeFunctionEnvSha (E.ENV_VAR _) _ = (NONE, OM.empty)
  | getTypeFunctionEnvSha (E.TOP_LEVEL_ENV)   _ = (NONE, OM.empty)
  | getTypeFunctionEnvSha (E.ENVOPN _) _ = raise EH.DeadBranch "This should have been built by now"
  | getTypeFunctionEnvSha (E.FUNCTOR_ENV _) _ = raise EH.DeadBranch "This should have been built by now"
  | getTypeFunctionEnvSha (E.CONSTRAINT_ENV _) _ = raise EH.DeadBranch "This should have been built by now"
  | getTypeFunctionEnvSha (E.ENVPOL _) _ = raise EH.DeadBranch "This should have been built by now"
  | getTypeFunctionEnvSha (E.DATATYPE_CONSTRUCTOR_ENV _) _ = raise EH.DeadBranch "This should have been built by now"
  | getTypeFunctionEnvSha (E.LOCAL_ENV _) _ = raise EH.DeadBranch "This should have been built by now"
  | getTypeFunctionEnvSha (E.ENVWHR _) _ = raise EH.DeadBranch "This should have been built by now"
  | getTypeFunctionEnvSha (E.ENVSHA _) _ = raise EH.DeadBranch "This should have been built by now"
  | getTypeFunctionEnvSha (E.SIGNATURE_ENV _) _ = raise EH.DeadBranch "This should have been built by now"
  | getTypeFunctionEnvSha (E.ENVPTY _) _ = raise EH.DeadBranch "This should have been built by now"
  | getTypeFunctionEnvSha (E.ENVFIL _) _ = raise EH.DeadBranch "This should have been built by now"

and getTypeFunctionShaExtEnv extenv extenv' =
    let (*val labs = EL.getExtLabL extenv'
	val stts = EL.getExtLabE extenv'
	val deps = EL.getExtLabD extenv'*)
	val labs = L.union  (EL.getExtLabL extenv) (EL.getExtLabL extenv')
	val stts = L.union  (EL.getExtLabE extenv) (EL.getExtLabE extenv')
	val deps = CD.union (EL.getExtLabD extenv) (EL.getExtLabD extenv')
	val (utf, tfns) = getTypeFunctionEnvSha (E.getBindT extenv) (E.getBindT extenv')
    in (decorateUTypeFunctionSha utf labs stts deps, decorateTypeFunctionSha tfns labs stts deps)
    end

and getTypeFunctionShaStrEnv strenv strenv' =
    E.foldrienv
	(fn (id, semty, (utf, tfns)) =>
	    foldr (fn (extenv, (utf, tfns)) =>
		      foldr (fn (extenv', (utf, tfns)) =>
				let val (utf', tfns') = getTypeFunctionShaExtEnv extenv extenv'
				in (mergeUTypeFunctionSha utf utf', mergeTypeFunctionSha tfns tfns')
				end)
			    (utf, tfns)
			    (E.plusproj strenv' id))
		  (utf, tfns)
		  semty)
	(NONE, OM.empty)
	strenv





(****************************************)
(* 2nd generalisation of the type names *)
(****************************************)
fun genTypeFunctionTy (x as T.TYPE_VAR _) _ _ = ([], x)
  | genTypeFunctionTy (x as T.EXPLICIT_TYPE_VAR _) _ _ = ([], x (*T.TYPE_VAR (T.freshTypeVar ())*))
  | genTypeFunctionTy (x as T.TYPE_CONSTRUCTOR (tnc, sq, l, eq)) tfun btyp =
    (case collapseTn tnc L.empty L.empty CD.empty of
	 T.TYPENAME_DEPENDANCY (T.NC (tn, _, _), labs, stts, deps) =>
	 (case OM.find (tfun, T.typenameToInt tn) of
	      NONE =>
	      let val (cs, sq') = genTypeFunctionSeqTy sq tfun btyp
	      in (cs, T.TYPE_CONSTRUCTOR (tnc, sq', l, eq)) end
	    | SOME tyf =>
	      (case collapseTf (freshTypeFunction tyf btyp) labs stts deps of
		   T.TYPE_FUNCTION_DEPENDANCY (T.TFC (sq', ty', l'), labs, stts, deps) =>
		   let val (cs, s) = genTypeFunctionSeqTy sq tfun btyp
		       val v  = T.newTYPE_VAR ()
		       val c1 = E.genCstSqAll s sq' labs stts deps (* the labels and context dependencies are not correct *)
		       val c2 = E.genCstTyAll v ty' labs stts deps
		   (*val _ = D.printdebug2 ("foo")*)
		   in (c1 :: c2 :: cs, v)
		   end
		 | _ => raise EH.DeadBranch "not a valid type function"))
       | _ =>
	 let val (cs, sq') = genTypeFunctionSeqTy sq tfun btyp
	 in (cs, T.TYPE_CONSTRUCTOR (tnc, sq', l, eq))
	 end)
  | genTypeFunctionTy (T.APPLICATION (tf, sq, l)) tfun btyp =
    let val (cs1, tf') = genTypeFunctionFunTy tf tfun btyp
	val (cs2, sq') = genTypeFunctionSeqTy sq tfun btyp
    in (cs1 @ cs2, T.APPLICATION (tf', sq', l))
    end
  | genTypeFunctionTy (T.TYPE_POLY (sq, i, p, k, l, eq)) tfun btyp =
    let val (cs, sq') = genTypeFunctionSeqTy sq tfun btyp
    in (cs, T.TYPE_POLY (sq', i, p, k, l, eq))
    end
  | genTypeFunctionTy (T.GEN tys) tfun btyp =
    let val (cs, tys) = ListPair.unzip (map (fn ty => genTypeFunctionTy ty tfun btyp) (!tys))
    in (List.concat cs, T.GEN (ref tys))
    end
  | genTypeFunctionTy (T.TYPE_DEPENDANCY (ty, labs, stts, deps)) tfun btyp =
    let val (cs, ty') = genTypeFunctionTy ty tfun btyp
    in (cs, collapseTy ty' labs stts deps)
    end
and genTypeFunctionFunTy (x as T.TYPE_FUNCTION_VAR _) _ _ = ([], x)
  | genTypeFunctionFunTy (T.TFC (sq, ty, l)) tfun btyp =
    let val (cs1, sq') = genTypeFunctionSeqTy sq tfun btyp
	val (cs2, ty') = genTypeFunctionTy    ty tfun btyp
    in (cs1 @ cs2, T.TFC (sq', ty', l))
    end
  | genTypeFunctionFunTy (T.TYPE_FUNCTION_DEPENDANCY (tf, labs, stts, deps)) tfun btyp =
    let val (cs, tf') = genTypeFunctionFunTy tf tfun btyp
    in (cs, collapseTf tf' labs stts deps)
    end
and genTypeFunctionSeqTy (x as T.ROW_VAR _) _ _ = ([], x)
  | genTypeFunctionSeqTy (T.ROW_C (rtl, flex, l)) state btyp =
    let val (cs, rtl') = ListPair.unzip (map (fn rt => genTypeFunctionFieldType rt state btyp) rtl)
    in (List.concat cs, T.ROW_C (rtl', flex, l))
    end
  | genTypeFunctionSeqTy (T.ROW_DEPENDANCY (sq, labs, stts, deps)) tfun btyp =
    let val (cs, sq') = genTypeFunctionSeqTy sq tfun btyp
    in (cs, collapseSq sq' labs stts deps)
    end
and genTypeFunctionFieldType (x as T.FIELD_VAR _) _ _ = ([], x)
  | genTypeFunctionFieldType (T.FC (lt, ty, l)) tfun btyp =
    let val (cs, ty') = genTypeFunctionTy ty tfun btyp
    in (cs, T.FC (lt, ty', l))
    end
  | genTypeFunctionFieldType (T.FIELD_DEPENDANCY (field, labs, stts, deps)) tfun btyp =
    let val (cs, field') = genTypeFunctionFieldType field tfun btyp
    in (cs, collapseRt field' labs stts deps)
    end
  | genTypeFunctionFieldType (x as T.FIELD_NO_OVERLOAD) _ _  = ([], x)

fun genTypeFunctionExtGen bind tfun f btyp =
    let val sem = E.getBindT bind
	val (cs, sem') = f sem tfun btyp
	val cs' = decorateCst cs (EL.getExtLabL bind) (EL.getExtLabE bind) (EL.getExtLabD bind)
	val bind' = EL.mapExtLab bind (fn x => C.mapBind x (fn _ => sem'))
    in (cs', bind')
    end

fun genTypeFunctionFunTy' (typeFunction, tnKind, cons) tfun btyp =
    let val (cs, typeFunction') = genTypeFunctionFunTy typeFunction tfun btyp
    in (cs, (typeFunction', tnKind, cons))
    end

fun genTypeFunctionExtTy    x _ tfun _ _ = genTypeFunctionExtGen x tfun genTypeFunctionTy     true
fun genTypeFunctionExtFunTy x _ tfun _ _ = genTypeFunctionExtGen x tfun genTypeFunctionFunTy' false
fun genTypeFunctionExtSeqTy x _ tfun _ _ = genTypeFunctionExtGen x tfun genTypeFunctionSeqTy  true

fun genTypeFunctionGenEnv genenv state tfun dom b genfun =
    E.foldrienv
	(fn (id, semty, (cs, genenv)) =>
	    let val xs = map (fn x => genfun x state tfun dom b) semty
		val (cs', semty') = ListPair.unzip xs
	    in (cs @ (List.concat cs'),	E.addenv (id, semty') genenv)
	    end)
	([], E.emptyMap)
	genenv

(* This is actually used for matchings structure/signature and for sharing *)
fun genTypeFunctionEnv (env as E.ENV_CONS _) state tfun dom b =
    let val tns =
	    if b
	    (*then List.filter (fn {id, kind, name} => not (O.isin (T.typenameToInt name) dom)) (E.getITypeNames env)*)
	    then List.mapPartial (fn x as {id, lab, kind, name} =>
				     case OM.find (tfun, T.typenameToInt name) of
					 SOME tf =>
					 (case T.isTypename tf of
					      (T.TYPENAME name', _, _, _) =>
					      SOME {id = id, lab = lab, kind = kind, name = name'}
					    | _ => NONE)
				       | NONE => SOME x)
				 (E.getITypeNames env)
	    else map (fn {id, lab, kind, name} =>
			 case Option.join (Option.map T.getTypename (OM.find (tfun, T.typenameToInt name))) of
			     SOME name' => {id = id, lab = lab, kind = kind, name = name'}
			   | _ => {id = id, lab = lab, kind = kind, name = name})
		     (E.getITypeNames env)
	val (csValueIds, vids) = genTypeFunctionGenEnv (E.getValueIds env) state tfun dom b genTypeFunctionExtTy
	val (csTyps, typs) = genTypeFunctionGenEnv (E.getTypeNameEnv env) state tfun dom b genTypeFunctionExtFunTy
	val tyvs           = E.getExplicitTypeVars env
	val (csStrs, strs) = genTypeFunctionGenEnv (E.getStructs env) state tfun dom b genTypeFunctionExtEnv
	val (csSigs, sigs) = genTypeFunctionGenEnv (E.getSigs env) state tfun dom b genTypeFunctionExtEnv
	val funs           = E.getFunctors env
	val (csOvcs, ovcs) = genTypeFunctionGenEnv (E.getOverloadingClasses env) state tfun dom b genTypeFunctionExtSeqTy
	val info           = E.consInfo (E.getILab env) (E.getIComplete env) tns (E.getIArgOfFunctor env)
	val cs             = csValueIds @ csTyps @ csOvcs @ csStrs @ csSigs
    in (cs, E.consEnvConstructor vids typs tyvs strs sigs funs ovcs info)
    end
  | genTypeFunctionEnv (x as E.ROW_ENV (env1, env2)) state tfun dom b =
    let val (cs1, env1') = genTypeFunctionEnv env1 state tfun dom b
	val (cs2, env2') = genTypeFunctionEnv env2 state tfun dom b
    in (cs1 @ cs2, E.ROW_ENV (env1', env2'))
    end
  | genTypeFunctionEnv (x as E.ENVDEP (env, labs, stts, deps)) state tfun dom b =
    let val (cs, env') = genTypeFunctionEnv env state tfun dom b
    in (decorateCst cs labs stts deps, E.ENVDEP (env', labs, stts, deps))
    end
  | genTypeFunctionEnv (x as E.ENV_VAR _) _ _ _ _ = ([], x)
  | genTypeFunctionEnv (x as E.TOP_LEVEL_ENV)   _ _ _ _ = ([], x)
  | genTypeFunctionEnv (x as E.ENVOPN _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | genTypeFunctionEnv (x as E.FUNCTOR_ENV _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | genTypeFunctionEnv (x as E.CONSTRAINT_ENV _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | genTypeFunctionEnv (x as E.ENVPOL _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | genTypeFunctionEnv (x as E.DATATYPE_CONSTRUCTOR_ENV _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | genTypeFunctionEnv (x as E.LOCAL_ENV _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | genTypeFunctionEnv (x as E.ENVWHR _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | genTypeFunctionEnv (x as E.ENVSHA _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | genTypeFunctionEnv (x as E.SIGNATURE_ENV _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | genTypeFunctionEnv (x as E.ENVPTY _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | genTypeFunctionEnv (x as E.ENVFIL _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"

and genTypeFunctionExtEnv extenv state tfun dom b =
    genTypeFunctionExtGen extenv tfun (fn env => fn _ => fn _ => genTypeFunctionEnv env state tfun dom b) true

(* b is true if tfun is meant to concretely bind the type functions of env. *)
fun genTypeFunctionEnv' env state tfun b =
    let (*val _   = D.printdebug2 ("B")*)
	val dom = domTFun tfun
	val ret = genTypeFunctionEnv env state tfun dom b
    (*val _   = D.printdebug2 ("E")*)
    in ret
    end




(************************************************************************)
(* Applies a type function to an env                            *)
(* This is only used by ENVWHR for where clauses                        *)
(************************************************************************)
fun applyTypeFunctionTy (x as (T.TYPE_VAR _)) _ _ = ([], x)
  | applyTypeFunctionTy (x as (T.EXPLICIT_TYPE_VAR _)) _ _ = ([], x (*T.TYPE_VAR (T.freshTypeVar ())*))
  | applyTypeFunctionTy (x as (T.TYPE_CONSTRUCTOR (tnc, sq, l, eq))) tfun btyp =
    (case collapseTn tnc L.empty L.empty CD.empty of
	 T.TYPENAME_DEPENDANCY (T.NC (tn, _, _), labs, stts, deps) =>
	 (case OM.find (tfun, T.typenameToInt tn) of
	      NONE =>
	      let val (cs, sq') = applyTypeFunctionSeqTy sq tfun btyp
	      in (cs, T.TYPE_CONSTRUCTOR (tnc, sq', l, eq)) end
	    | SOME tyf =>
	      (case collapseTf (freshTypeFunction tyf btyp) labs stts deps of
		   T.TYPE_FUNCTION_DEPENDANCY (T.TFC (sq', ty', l'), labs, stts, deps) =>
		   let val (cs, s) = applyTypeFunctionSeqTy sq tfun btyp
		       val v  = T.newTYPE_VAR ()
		       val c1 = E.genCstSqAll s sq' labs stts deps (* the labels and context dependencies are not correct *)
		       val c2 = E.genCstTyAll v ty' labs stts deps
		   (*val _ = D.printdebug2 ("foo")*)
		   in (c1 :: c2 :: cs, v)
		   end
		 | _ => raise EH.DeadBranch "not a valid type function"))
       | _ => let val (cs, sq') = applyTypeFunctionSeqTy sq tfun btyp
	      in (cs, T.TYPE_CONSTRUCTOR (tnc, sq', l, eq)) end)
  | applyTypeFunctionTy (T.APPLICATION (tf, sq, l)) tfun btyp =
    let val (cs1, tf') = applyTypeFunctionFunTy tf tfun btyp
	val (cs2, sq') = applyTypeFunctionSeqTy sq tfun btyp
    in (cs1 @ cs2, T.APPLICATION (tf', sq', l))
    end
  | applyTypeFunctionTy (T.TYPE_POLY (sq, i, p, k, l, eq)) tfun btyp =
    let val (cs, sq') = applyTypeFunctionSeqTy sq tfun btyp
    in (cs, T.TYPE_POLY (sq', i, p, k, l, eq))
    end
  | applyTypeFunctionTy (T.GEN tys) tfun btyp =
    let val (cs, tys) = ListPair.unzip (map (fn ty => applyTypeFunctionTy ty tfun btyp) (!tys))
    in (List.concat cs, T.GEN (ref tys))
    end
  | applyTypeFunctionTy (T.TYPE_DEPENDANCY (ty, labs, stts, deps)) tfun btyp =
    let val (cs, ty') = applyTypeFunctionTy ty tfun btyp
    in (cs, collapseTy ty' labs stts deps)
    end
and applyTypeFunctionFunTy (x as (T.TYPE_FUNCTION_VAR _)) _ _ = ([], x)
  | applyTypeFunctionFunTy (T.TFC (sq, ty, l)) tfun btyp =
    let val (cs1, sq') = applyTypeFunctionSeqTy sq tfun btyp
	val (cs2, ty') = applyTypeFunctionTy    ty tfun btyp
    in (cs1 @ cs2, T.TFC (sq', ty', l))
    end
  | applyTypeFunctionFunTy (T.TYPE_FUNCTION_DEPENDANCY (tf, labs, stts, deps)) tfun btyp =
    let val (cs, tf') = applyTypeFunctionFunTy tf tfun btyp
    in (cs, collapseTf tf' labs stts deps)
    end
and applyTypeFunctionSeqTy (x as (T.ROW_VAR _)) _ _ = ([], x)
  | applyTypeFunctionSeqTy (T.ROW_C (rtl, flex, l)) state btyp =
    let val (cs, rtl') = ListPair.unzip (map (fn rt => applyTypeFunctionFieldType rt state btyp) rtl)
    in (List.concat cs, T.ROW_C (rtl', flex, l))
    end
  | applyTypeFunctionSeqTy (T.ROW_DEPENDANCY (sq, labs, stts, deps)) tfun btyp =
    let val (cs, sq') = applyTypeFunctionSeqTy sq tfun btyp
    in (cs, collapseSq sq' labs stts deps)
    end
and applyTypeFunctionFieldType (x as (T.FIELD_VAR _)) _ _ = ([], x)
  | applyTypeFunctionFieldType (T.FC (lt, ty, l)) tfun btyp =
    let val (cs, ty') = applyTypeFunctionTy ty tfun btyp
    in (cs, T.FC (lt, ty', l))
    end
  | applyTypeFunctionFieldType (T.FIELD_DEPENDANCY (field, labs, stts, deps)) tfun btyp =
    let val (cs, field') = applyTypeFunctionFieldType field tfun btyp
    in (cs, collapseRt field' labs stts deps)
    end
  | applyTypeFunctionFieldType (x as T.FIELD_NO_OVERLOAD) _ _ = ([], x)

fun applyTypeFunctionExtGen bind tfun f btyp =
    let val sem = E.getBindT bind
	val (cs, sem') = f sem tfun btyp
	val cs' = decorateCst cs (EL.getExtLabL bind) (EL.getExtLabE bind) (EL.getExtLabD bind)
	val bind' = EL.mapExtLab bind (fn x => C.mapBind x (fn _ => sem'))
    in (cs', bind')
    end

fun applyTypeFunctionFunTy' (typeFunction, tnKind, cons) tfun btyp =
    let val (cs, typeFunction') = applyTypeFunctionFunTy typeFunction tfun btyp
    in (cs, (typeFunction', tnKind, cons))
    end

fun applyTypeFunctionExtTy    x tfun = applyTypeFunctionExtGen x tfun applyTypeFunctionTy     true
fun applyTypeFunctionExtFunTy x tfun = applyTypeFunctionExtGen x tfun applyTypeFunctionFunTy' false
fun applyTypeFunctionExtSeqTy x tfun = applyTypeFunctionExtGen x tfun applyTypeFunctionSeqTy  true

fun applyTypeFunctionGenEnv genenv tfun genfun =
    E.foldrienv
	(fn (id, semty, (cs, genenv)) =>
	    let val xs = map (fn x => genfun x tfun) semty
		val (cs', semty') = ListPair.unzip xs
	    in (cs @ (List.concat cs'),	E.addenv (id, semty') genenv)
	    end)
	([], E.emptyMap)
	genenv

fun applyTypeFunctionEnv (env as E.ENV_CONS _) tfun =
    let val (csValueIds, vids) = applyTypeFunctionGenEnv (E.getValueIds env) tfun applyTypeFunctionExtTy
	val (csTyps, typs) = applyTypeFunctionGenEnv (E.getTypeNameEnv env) tfun applyTypeFunctionExtFunTy
	val tyvs           = E.getExplicitTypeVars env
	val (csStrs, strs) = applyTypeFunctionGenEnv (E.getStructs env) tfun applyTypeFunctionExtEnv
	val (csSigs, sigs) = applyTypeFunctionGenEnv (E.getSigs env) tfun applyTypeFunctionExtEnv
	val (csFuns, funs) = applyTypeFunctionGenEnv (E.getFunctors env) tfun applyTypeFunctionExtFun
	val (csOvcs, ovcs) = applyTypeFunctionGenEnv (E.getOverloadingClasses env) tfun applyTypeFunctionExtSeqTy
	val cs             = csValueIds @ csTyps @ csStrs @ csSigs @ csFuns @ csOvcs
	val tns            =
	    List.mapPartial (fn x as {id, lab, kind, name} =>
				case OM.find (tfun, T.typenameToInt name) of
				    SOME tf =>
				    (case T.isTypename tf of
					 (* We rename a type name if the mapping renames it *)
					 (T.TYPENAME name', _, _, _) =>
					 SOME {id = id, lab = lab, kind = kind, name = name'}
				       (* No, we should do this replacement, but in renameenv, we should
					* not rename the type names that are in the env.*)
				       (* We discart a type name if the mapping maps it to a non type name *)
				       | _ => NONE)
				  (* We keep a type name if the mapping does not map it *)
				  | NONE => SOME x)
			    (E.getITypeNames env)
	val info           = E.consInfo (E.getILab env) (E.getIComplete env) tns (E.getIArgOfFunctor env)
    (*TODO: rename the type names in the info part using tfun.*)
    in (cs, E.consEnvConstructor vids typs tyvs strs sigs funs ovcs info)
    end
  | applyTypeFunctionEnv (x as E.ROW_ENV (env1, env2)) tfun =
    let val (cs1, env1') = applyTypeFunctionEnv env1 tfun
	val (cs2, env2') = applyTypeFunctionEnv env2 tfun
    in (cs1 @ cs2, E.ROW_ENV (env1', env2'))
    end
  | applyTypeFunctionEnv (x as E.ENVDEP (env, labs, stts, deps)) tfun =
    let val (cs, env') = applyTypeFunctionEnv env tfun
    in (decorateCst cs labs stts deps, E.ENVDEP (env', labs, stts, deps))
    end
  | applyTypeFunctionEnv (x as E.ENV_VAR _) _ = ([], x)
  | applyTypeFunctionEnv (x as E.TOP_LEVEL_ENV)   _ = ([], x)
  | applyTypeFunctionEnv (x as E.ENVOPN _) _ = raise EH.DeadBranch "This should have been built by now"
  | applyTypeFunctionEnv (x as E.FUNCTOR_ENV _) _ = raise EH.DeadBranch "This should have been built by now"
  | applyTypeFunctionEnv (x as E.CONSTRAINT_ENV _) _ = raise EH.DeadBranch "This should have been built by now"
  | applyTypeFunctionEnv (x as E.ENVPOL _) _ = raise EH.DeadBranch "This should have been built by now"
  | applyTypeFunctionEnv (x as E.DATATYPE_CONSTRUCTOR_ENV _) _ = raise EH.DeadBranch "This should have been built by now"
  | applyTypeFunctionEnv (x as E.LOCAL_ENV _) _ = raise EH.DeadBranch "This should have been built by now"
  | applyTypeFunctionEnv (x as E.ENVWHR _) _ = raise EH.DeadBranch "This should have been built by now"
  | applyTypeFunctionEnv (x as E.ENVSHA _) _ = raise EH.DeadBranch "This should have been built by now"
  | applyTypeFunctionEnv (x as E.SIGNATURE_ENV _) _ = raise EH.DeadBranch "This should have been built by now"
  | applyTypeFunctionEnv (x as E.ENVPTY _) _ = raise EH.DeadBranch "This should have been built by now"
  | applyTypeFunctionEnv (x as E.ENVFIL _) _ = raise EH.DeadBranch "This should have been built by now"

and applyTypeFunctionExtEnv extenv tfun =
    applyTypeFunctionExtGen extenv tfun (fn env => fn tfun => fn _ => applyTypeFunctionEnv env tfun) true

and applyTypeFunctionExtFun extfun tfun =
    applyTypeFunctionExtGen extfun
		     tfun
		     (fn (env1, env2) =>
		      fn tfun =>
		      fn _ => let val (cs1, env1') = applyTypeFunctionEnv env1 tfun
				  val (cs2, env2') = applyTypeFunctionEnv env2 tfun
			      in (cs1 @ cs2, (env1', env2'))
			      end)
		     true


(* Matching signature/where clause *)

fun matchWhereEnv envsig NONE state = (OM.empty, true)
  | matchWhereEnv (envsig as E.ENV_CONS _) (SOME ({lid = I.ID (idTc, labTc), sem, class, lab}, labs, stts, deps)) state =
    let val tmap = OM.empty
    in case E.plusproj (E.getTypeNameEnv envsig) idTc of
	   [] => (* There is no machting in the signature *)
	   if E.getIComplete envsig andalso not (CL.classIsANY class)
	   (* If the signature is complete then we need to raise a unmatched error. *)
	   then let val (idlabs, labs1) = E.getLabsIdsEnv envsig 1
		    val lab'  = E.getLabEnv envsig
		    val labs2 = L.cons lab' (L.union labs1 labs)
		    val ek    = EK.UnbWhere ((L.toInt labTc, I.toInt idTc), idlabs, L.toInt lab')
		in raise errorfound (ERR.consPreError ERR.dummyId labs2 deps ek stts)
		end
	   (* Otherwise, the type cons might be filtered out, so we can't do anything. *)
	   (* TODO: we need to remove idTc's type name from envsig if it exists.
	    * - The type name shouldn't be used anyway because there is no binding in the envsig. *)
	   else (tmap, false)
	 | [bind] => (* We found a matching in the signature. *)
	   let val (tf, kind, _) = E.getBindT bind
	       val _ = case kind of
			   E.DATATYPE => if CL.classIsANY (E.getBindC bind)
	 			    then ()
				    else (case T.isTypename sem of
					      (T.NOTTYPENAME, labs', stts', deps') =>
					      let val labs = L.union  (L.union  labs labs') (EL.getExtLabL bind)
						  val stts = L.union  (L.union  stts stts') (EL.getExtLabE bind)
						  val deps = CD.union (CD.union deps deps') (EL.getExtLabD bind)
						  val lab' = E.getBindL bind
						  val ek   = EK.IllFormedWhere ((L.toInt lab', I.toInt idTc), (L.toInt labTc, I.toInt idTc))
						  (*val _ = D.printdebug2 (T.printtyf sem)*)
					      in raise errorfound (ERR.consPreError ERR.dummyId labs deps ek stts)
					      end
					    | _ => ())
			 | E.TYPE => ()
	       val (names, labs', stts', deps') = T.isTypename tf
	   in case names of
		  T.TYPENAME name => (* The signature defines a type name for the idTc so it has to be renamed.
				    * (1) If name is in the context then it is an error because of the
				    * 't \not\in T of B' condition for where clauses (rigid type).
				    * (2) If kind is DAT then sem needs to be a type name otherwise it is an
				    * error because of the 'well-formedness' condition for where clauses. *)
		  (* (2) is checked above *)
		  (* TODO: check (1) *)
		  (* TODO: replace name in the list of type names of envsig
		   * - This is done when applying the mapping. *)
		  let (*val _ = if S.isAName name state andalso not (CL.classIsANY class)
			      then let val labs = L.union  labs (EL.getExtLabL bind)
				       val stts = L.union  stts (EL.getExtLabE bind)
				       val deps = CD.union deps (EL.getExtLabD bind)
				       val lab' = E.getBindL bind
				       val ek   = EK.RigidWhere (*((L.toInt lab', I.toInt idTc), (L.toInt labTc, I.toInt idTc))*)
				   in raise errorfound (ERR.consPreError ERR.dummyId labs deps ek stts L.dummyLab)
				   end
			      else ()*)
		      fun loopTf (T.TFC _) = (* The the sem is not a dummy one *)
			  let val labs = L.union  (L.union  labs labs') (EL.getExtLabL bind)
			      val stts = L.union  (L.union  stts stts') (EL.getExtLabE bind)
			      val deps = CD.union (CD.union deps deps') (EL.getExtLabD bind)
			  in (insertInTypeFunction tmap name (collapseTf sem labs stts deps), true)
			  end
			| loopTf (T.TYPE_FUNCTION_DEPENDANCY etf) = loopTf (EL.getExtLabT etf)
			| loopTf _ = (insertInTypeFunction tmap name (newTypeFunction ()), true)
		  in loopTf sem
		  end
		| T.DUMTYPENAME name => (* The signature defines a matching for idTc that could potentially
				       * be a type name, but we don't have enough info to know.*)
		  (* TODO: we need to remove name from the list of type names in envsig.
		   * - This is done when applying the mapping. *)
		  (insertInTypeFunction tmap name (newTypeFunction ()), true)
		| T.MAYTYPENAME => (* We could have a matching in the signature but it does not
				  * a type name so we don't need to do nothing. *)
		  (tmap, true)
		| T.NOTTYPENAME => (* We have a matching in the signature which is definitely not a
				  * type name, so there is an error because of the condition
				  * 'E(longtycon) = (t, VE)' for where clauses (tf is not a t).
				  * (Called non-flexible type in HaMLet.) *)
		  if not (CL.classIsANY class)
		  then let val labs = L.union  (L.union  labs labs') (EL.getExtLabL bind)
			   val stts = L.union  (L.union  stts stts') (EL.getExtLabE bind)
			   val deps = CD.union (CD.union deps deps') (EL.getExtLabD bind)
			   val lab' = E.getBindL bind
			   val ek   = EK.NonFlexWhere ((L.toInt lab', I.toInt idTc), (L.toInt labTc, I.toInt idTc))
		       in raise errorfound (ERR.consPreError ERR.dummyId labs deps ek stts)
		       end
		  else (tmap, true)
	   end
	 | _ => raise EH.DeadBranch "There should be only one binding per identifier during constraint solving"
    end
  | matchWhereEnv (envsig as E.ENV_CONS _) (SOME ({lid = I.LID ((id, lab1), lid, lab2), sem, class, lab}, labs, stts, deps)) state =
    (case E.plusproj (E.getStructs envsig) id of
	 [] => (* There is no machting in the signature *)
	 if E.getIComplete envsig
	 (* If the signature is complete then we need to raise a unmatched error. *)
	 then let val (idlabs, labs1) = E.getLabsIdsEnv envsig 1
		  val lab'  = E.getLabEnv envsig
		  val labs2 = L.cons lab' (L.union labs1 labs)
		  val ek    = EK.UnbWhere ((L.toInt lab1, I.toInt id), idlabs, L.toInt lab')
	      in raise errorfound (ERR.consPreError ERR.dummyId labs2 deps ek stts)
	      end
	 (* Otherwise, the structure might be filtered out, so we can't do anything. *)
	 else (OM.empty, false)
       | [bind] =>
	 let val labs    = L.union  labs (EL.getExtLabL bind)
	     val stts    = L.union  stts (EL.getExtLabE bind)
	     val deps    = CD.union deps (EL.getExtLabD bind)
	     val longtyp = SOME ({lid = lid, sem = sem, class = class, lab = lab}, labs, stts, deps)
	 in matchWhereEnv (E.getBindT bind) longtyp state
	 end
       | _ => raise EH.DeadBranch "There should be only one binding per identifier during constraint solving")
  | matchWhereEnv (envsig as E.ROW_ENV (env1, env2)) longtyp state =
    (let val (tmap, found) = matchWhereEnv env2 longtyp state
     (* If found then it means that we found the matching.
      * Otherwise, either we raised an error because we couldn't find any matching
      * or we don't have enough info to know if env1 has a matching and then we
      * should generates dummies for env1. *)
     in if found
	then (tmap, true)
	else let val (tmapDat, tmapTyp) = getAllTypeFunctionEnv env1
	     in (mergeTypeFunction tmap (mergeTypeFunction tmapDat tmapTyp), false)
	     end
     end
     (* If we found an error it might just be because the matching is in env1. *)
     handle errorfound err => (matchWhereEnv env1 longtyp state handle errorfound _ => raise errorfound err))
  | matchWhereEnv (E.ENVDEP (env, labs, stts, deps)) (SOME (longtyp, labs', stts', deps')) state =
    let val labs0 = L.union  labs labs'
	val stts0 = L.union  stts stts'
	val deps0 = CD.union deps deps'
    in matchWhereEnv env (SOME (longtyp, labs0, stts0, deps0)) state
    end
  | matchWhereEnv (E.ENV_VAR _) _ _ = (OM.empty, false)
  | matchWhereEnv (E.TOP_LEVEL_ENV)   _ _ = (OM.empty, true)
  | matchWhereEnv (E.ENVOPN _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | matchWhereEnv (E.FUNCTOR_ENV _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | matchWhereEnv (E.CONSTRAINT_ENV _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | matchWhereEnv (E.ENVPOL _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | matchWhereEnv (E.DATATYPE_CONSTRUCTOR_ENV _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | matchWhereEnv (E.LOCAL_ENV _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | matchWhereEnv (E.ENVWHR _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | matchWhereEnv (E.ENVSHA _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | matchWhereEnv (E.SIGNATURE_ENV _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | matchWhereEnv (E.ENVPTY _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | matchWhereEnv (E.ENVFIL _) _ _ = raise EH.DeadBranch "This should have been built by now"

(* Env renamening *)

(* TODO *)

fun initRen () = ref OM.empty

fun updateRen state tn =
    (case Option.getOpt (OM.find (!state, T.typenameToInt tn), NONE) of
	 NONE => let val tn' = T.freshTypename ()
		 in (state := (OM.insert (!state, T.typenameToInt tn, SOME tn')); tn') end
       | SOME tn' => tn')

fun getRen state tn =
    (case Option.getOpt (OM.find (!state, T.typenameToInt tn), NONE) of
	 NONE => tn
       | SOME tn' => tn')

fun renametypename (x as T.TYPENAME_VAR _) _ = x
  | renametypename (T.NC (tn, b, l)) state =
    if T.isBase' tn
    then T.NC (tn, b, l)
    else T.NC ((*updateRen*) getRen state tn, b, l)
  | renametypename (T.TYPENAME_DEPENDANCY etn) state = T.TYPENAME_DEPENDANCY (EL.mapExtLab etn (fn tn => renametypename tn state))

fun renamety (x as T.TYPE_VAR _) _ = x
  | renamety (x as T.EXPLICIT_TYPE_VAR _) _ = x
  | renamety (T.TYPE_CONSTRUCTOR (tn, sq, l, eq))       state = T.TYPE_CONSTRUCTOR (renametypename tn state, renameseqty sq state, l, eq)
  | renamety (T.APPLICATION (tf, sq, l))       state = T.APPLICATION (renametypfun tf state, renameseqty sq state, l)
  | renamety (T.TYPE_POLY (sq, i, p, k, l, eq)) state = T.TYPE_POLY (renameseqty sq state, i, p, k, l, eq)
  | renamety (T.GEN ty)              state = raise EH.TODO "no description, raised in the 'renamety' function of Unification.sml"
  | renamety (T.TYPE_DEPENDANCY ety)              state = T.TYPE_DEPENDANCY (EL.mapExtLab ety (fn ty => renamety ty state))
and renametypfun (x as T.TYPE_FUNCTION_VAR _) _ = x
  | renametypfun (T.TFC (sq, ty, lab)) state = T.TFC (renameseqty sq state, renamety ty state, lab)
  | renametypfun (T.TYPE_FUNCTION_DEPENDANCY etf)           state = T.TYPE_FUNCTION_DEPENDANCY (EL.mapExtLab etf (fn tf => renametypfun tf state))
and renameseqty (x as T.ROW_VAR _) _ = x
  | renameseqty (T.ROW_C (rtl, flex, l)) state = T.ROW_C (map (fn rt => renamefieldType rt state) rtl, flex, l)
  | renameseqty (T.ROW_DEPENDANCY eseq)           state = T.ROW_DEPENDANCY (EL.mapExtLab eseq (fn seq => renameseqty seq state))
and renamefieldType (x as T.FIELD_VAR _)      _     = x
  | renamefieldType (T.FC (lt, ty, l)) state = T.FC (lt, renamety ty state, l)
  | renamefieldType (T.FIELD_DEPENDANCY efield)        state = T.FIELD_DEPENDANCY (EL.mapExtLab efield (fn field => renamefieldType field state))
  | renamefieldType (x as T.FIELD_NO_OVERLOAD)        _     = T.FIELD_NO_OVERLOAD
(*and renametylist xs state = map (fn x => renamety x state) xs
and renameseqtylist xs state = map (fn x => renameseqty x state) xs*)

fun renameextgen ({id, bind, class, lab, poly}, labs, stts, deps) state ren f =
    (C.consBind id (f bind state ren) class lab poly, labs, stts, deps)

fun renameextty x state ren = renameextgen x state ren (fn x => fn state => fn ren => renamety x ren)

fun renamevarenv varenv state ren =
    E.mapenv (fn semty => map (fn x => renameextty x state ren) semty) varenv

fun renameextseqty x state ren = renameextgen x state ren (fn x => fn state => fn ren => renameseqty x ren)

fun renameseqenv ovsenv state ren =
    E.mapenv (fn semty => map (fn x => renameextseqty x state ren) semty) ovsenv

fun renameexttypfun x state ren =
    renameextgen x state ren
		 (fn (typfun, tnKind, varenv) =>
		  fn state =>
		  fn ren =>
		     let val (cons, b) = !varenv
			 val _ = varenv := (renamevarenv cons state ren, b)
		     in (renametypfun typfun ren, tnKind, varenv)
		     end)

fun renametypenv typenv state ren =
    E.mapenv (fn semty => map (fn x => renameexttypfun x state ren) semty) typenv

and renameenv (env as E.ENV_CONS _) state ren =
    let (*val state = ref (!state) (*(2010-06-14)Why a new reference?*)*)
	val tns   = map (fn {id, lab, kind, name} =>
			    ((*D.printdebug2 (S.printState state ^ "\n\n" ^ T.printtypename name);*)
			     if S.isAName name state
			     then {id = id, lab = lab, kind = kind, name = name}
			     else {id = id, lab = lab, kind = kind, name = updateRen ren name}))
			(E.getITypeNames env)
	val strs  = renamestrenv (E.getStructs env) state ren
	val sigs  = renamestrenv (E.getSigs env) state ren
	val funs  = renamefunenv (E.getFunctors env) state ren
	(* We do the strs, sigs and funs before as they can update ren *)
	val ovcs  = renameseqenv (E.getOverloadingClasses env) state ren
	val vids  = renamevarenv (E.getValueIds env) state ren
	val typs  = renametypenv (E.getTypeNameEnv env) state ren
	val tyvs  = E.getExplicitTypeVars env
	val info  = E.consInfo (E.getILab env) (E.getIComplete env) tns (E.getIArgOfFunctor env)
    in E.consEnvConstructor vids typs tyvs strs sigs funs ovcs info
    end
  | renameenv (E.ROW_ENV (env1, env2)) state ren =
    E.ROW_ENV (renameenv env1 state ren, renameenv env2 state ren)
  | renameenv (E.ENVDEP (env, labs, stts, deps)) state ren =
    let val env' = renameenv env state ren
    in E.ENVDEP (env', labs, stts, deps)
    end
  | renameenv (x as E.ENV_VAR _) _ _ = x
  | renameenv (x as E.TOP_LEVEL_ENV)   _ _ = x
  | renameenv (E.LOCAL_ENV _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | renameenv (E.ENVWHR _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | renameenv (E.ENVSHA _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | renameenv (E.SIGNATURE_ENV _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | renameenv (E.ENVPOL _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | renameenv (E.DATATYPE_CONSTRUCTOR_ENV _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | renameenv (E.ENVOPN _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | renameenv (E.FUNCTOR_ENV _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | renameenv (E.CONSTRAINT_ENV _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | renameenv (E.ENVPTY _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | renameenv (E.ENVFIL _) _ _ = raise EH.DeadBranch "This should have been built by now"
and renameextstr extstr state ren = renameextgen extstr state ren renameenv
and renameextfun extfun state ren = renameextgen extfun state ren (fn (x, y) => fn state => fn ren => (renameenv x state ren, renameenv y state ren))
and renamestrenv strenv state ren =
    E.mapenv (fn semty => map (fn x => renameextstr x state ren) semty) strenv
and renamefunenv funenv state ren =
    E.mapenv (fn semty => map (fn x => renameextfun x state ren) semty) funenv

fun renameenv' env state = renameenv env state (initRen ())



(* These are similar to the getValStateAr in StateEnv.sml *)

fun buildSeqAr state (T.ROW_VAR sv) labs stts deps=
    (case S.getValStateSq state sv of
	 NONE => T.newROW_VAR ()
       | SOME sq => buildSeqAr state sq labs stts deps)
  | buildSeqAr state (T.ROW_C (xs, flex, lab)) labs stts deps =
    T.ROW_DEPENDANCY (T.ROW_C (map (fn _ => T.newFIELD_VAR ()) xs, flex, lab), labs, stts, deps)
  | buildSeqAr state (T.ROW_DEPENDANCY (sq1, labs1, stts1, deps1)) labs stts deps =
    let val sq2 = buildSeqAr state sq1 labs stts deps
    in T.ROW_DEPENDANCY (sq2, labs1, stts1, deps1)
    end

fun buildTypeFunctionAr state (T.TYPE_FUNCTION_VAR tfv) lab labs stts deps =
    (case S.getValStateTf state tfv of
	 NONE => T.TYPE_FUNCTION_VAR tfv
       | SOME tf => buildTypeFunctionAr state tf lab labs stts deps)
  | buildTypeFunctionAr state (T.TFC (sq, ty, _)) lab labs stts deps =
    (*(2010-06-17)This is to ensure that the type is not preliminary constrainted by something else
     * Ooops, there is still the constraint about the size of sq!*)
    if T.isTyV ty andalso T.isShallowSeq sq
    then let val sq' = buildSeqAr state sq labs stts deps
	 in T.TFC (sq', T.newTYPE_VAR (), lab)
	 end
    else T.newTYPE_FUNCTION_VAR ()
  | buildTypeFunctionAr state (T.TYPE_FUNCTION_DEPENDANCY (tf, labs1, stts1, deps1)) lab labs stts deps =
    let val tf' = buildTypeFunctionAr state tf lab labs stts deps
    in T.TYPE_FUNCTION_DEPENDANCY (tf', labs1, stts1, deps1)
    end



(* Checks if a type variable is already stored in the 'ge' part of the unification env. *)
fun isInState (T.TYPE_VAR (tv, _, _, _)) state =
    S.isInGe state tv
    (*Option.isSome (S.getValStateTv state tv)*)
  | isInState _ _ = false

fun updateStateTyGen state (T.TYPE_VAR (tv, _, _, _)) ty labs stts deps =
    (case S.getValStateTv state tv of
	 NONE => S.updateStateTv state tv (T.GEN (ref [T.TYPE_DEPENDANCY (ty, labs, stts, deps)]))
       | SOME typ =>
	 (case collapseTy typ labs stts deps of
	      T.TYPE_DEPENDANCY (T.GEN tys, labs, stts, deps) =>
	      let val tv' = T.freshTypeVar ()
		  val ty' = T.consTYPE_VAR tv'
		  val _   = S.updateStateTv state tv' (T.TYPE_DEPENDANCY (ty, labs, stts, deps))
	      in tys := ty' :: (!tys)
	      end
	    | _ => ()))
  | updateStateTyGen _ _ _ _ _ _ = ()


(****************************************************)


(*(2010-06-18)What is that?*)
val sigVsStr = ref false
fun sigVsStrON  () = sigVsStr := true
fun sigVsStrOFF () = sigVsStr := false
fun isSigVsStr  () = !sigVsStr

(*(2010-06-18)What is that?*)
val sigVsStrTyp = ref false
fun sigVsStrTypON  () = sigVsStrTyp := true
fun sigVsStrTypOFF () = sigVsStrTyp := false
fun isSigVsStrTyp  () = !sigVsStrTyp



fun isEnum ENUM   = true
  | isEnum DBENUM = true
  | isEnum _      = false


(* bcontinue is true if we don't stop when we discover a type error *)
(* this is used to buid envs *)
fun unif env filters user =
    let exception errorex of ERR.error

	val err     = case user of
			  ENUM    => NONE
			| DBENUM  => NONE
			| MIN err => SOME err

	val builtin = case err of
			  NONE => true
			| SOME err => ERR.getB err

	val filters = if builtin
		      then FI.addLab filters L.builtinLab
		      else filters

	val enum    = isEnum user

	val state = S.initState ()

	val bcontinue = false


	(* ====== CIRCULARITY TEST ====== *)

	(* All these occurs function are the circularity test
	 * (e.g., 'a = 'a -> 'a leads to a circularity error). *)

	fun occursGenZero c var1 var2 sem1 labs stts deps lab feq fget fdecomp foc =
	    if feq var1 var2
	    then false (* false because c have to not be added to the state *)
	    else (case fget state var2 of
                      NONE => true (* because no queue at rho *)
		    | SOME sem2 =>
                      let val (decomp, m) = fdecomp sem2 labs stts deps
                      in foc (c (var1, sem1)) decomp m lab
                      end)

	fun occursGenList c var1 var2 sem1 labs stts deps vars depth lab fget fdecomp foc =
	    (case fget state var2 of
		 NONE => foc (c (var1, sem1)) vars depth lab
               | SOME sem2 =>
		 let val (decomp, m) = fdecomp sem2 labs stts deps
		 in foc (c (var1, sem1)) (decomp @ vars) (depth + m) lab
		 end)

	fun occursGenListEq c var1 var2 sem1 labs stts deps vars depth lab feq fget fdecomp foc foch =
	    if feq var1 var2
	    then if depth = 0
		 then foc (c (var1, sem1)) vars depth lab
		 else foch (ERR.consPreError ERR.dummyId labs deps EK.Circularity stts) (c (var1, sem1)) vars depth lab
	    else occursGenList c var1 var2 sem1 labs stts deps vars depth lab fget fdecomp foc

	fun foccurs c [] n l = true (* true because we have to add c to the state *)
	  | foccurs (c as (CT (var1, sem))) [T (var2, labs, stts, deps)] 0 l = occursGenZero CT var1 var2 sem labs stts deps l T.eqTypeVar  S.getValStateTv decomptyty  foccurs
	  | foccurs (c as (CS (var1, sem))) [S (var2, labs, stts, deps)] 0 l = occursGenZero CS var1 var2 sem labs stts deps l T.eqRowVar S.getValStateSq decomptysq  foccurs
	  | foccurs (c as (CR (var1, sem))) [R (var2, labs, stts, deps)] 0 l = occursGenZero CR var1 var2 sem labs stts deps l T.eqFieldVar S.getValStateRt decomptyfield foccurs
	  | foccurs (c as (CT (var1, sem))) (var :: xs) n l =
	    (case var of
		 T (var2, labs, stts, deps) => occursGenListEq CT var1 var2 sem labs stts deps xs n l T.eqTypeVar S.getValStateTv decomptyty foccurs handleOccurs
	       | S (var2, labs, stts, deps) => occursGenList CT var1 var2 sem labs stts deps xs n l S.getValStateSq decomptysq  foccurs
	       | R (var2, labs, stts, deps) => occursGenList CT var1 var2 sem labs stts deps xs n l S.getValStateRt decomptyfield foccurs)
	  | foccurs (c as (CS (var1, sem))) (var :: xs) n l =
	    (case var of
		 S (var2, labs, stts, deps) => occursGenListEq CS var1 var2 sem labs stts deps xs n l T.eqRowVar S.getValStateSq decomptysq foccurs handleOccurs
	       | T (var2, labs, stts, deps) => occursGenList CS var1 var2 sem labs stts deps xs n l S.getValStateTv decomptyty  foccurs
	       | R (var2, labs, stts, deps) => occursGenList CS var1 var2 sem labs stts deps xs n l S.getValStateRt decomptyfield foccurs)
	  | foccurs (c as (CR (var1, sem))) (var :: xs) n l =
	    (case var of
		 R (var2, labs, stts, deps) => occursGenListEq CR var1 var2 sem labs stts deps xs n l T.eqFieldVar S.getValStateRt decomptyfield foccurs handleOccurs
	       | T (var2, labs, stts, deps) => occursGenList CR var1 var2 sem labs stts deps xs n l S.getValStateTv decomptyty foccurs
	       | S (var2, labs, stts, deps) => occursGenList CR var1 var2 sem labs stts deps xs n l S.getValStateSq decomptysq foccurs)

	and handleOccurs err c xs n l =
	    if bcontinue
	    then false
	    else raise errorex err

	(*
	 * (2011-02-17) In the occurs check, we shouldn't test a variable if it has
	 * already been tested.  This issue is to have a fast tester of whether a variable
	 * has already been tested.
	 * The occurs check does not seem to take that much time.  It would be good to
	 * speed it up anyway because it occasionally takes over 10% of the unification
	 * time.
	 *)

	(*val occurs_time = ref (0 : LargeInt.int)*)

	fun occurs cocc occtyl n l =
	    let (*val timer = VT.startTimer ()*)
		val ret = foccurs cocc occtyl n l
		(*val t = VT.getMilliTime timer*)
		(*val _ = occurs_time := !occurs_time + t*)
	    in ret
	    end


	(*val temp_time = ref (0 : LargeInt.int)*)


	(* ====== VALUE POLYMORPHISM RESTRICTION SOLVER ====== *)

	fun solveMono (x as P.EXPANS (X.Expdep (lid, labels))) =
	    if FI.testtodos filters labels
	    then case S.getValStateIdVa state lid false of
		     (SOME (({id, bind, lab, poly, class}, labs, _, _), _), _, _) =>
		     if CL.classIsCON class orelse
			CL.classIsANY class
		     (*(2010-02-16) then because the binder is a cons,
		      * the expression is not expansive. *)
		     then NONE
		     else if CL.classIsREC class(* orelse
						 CL.classIsVRC class*) (*orelse
									CL.classIsVAL class*)
		     then let val labs' = L.union labels labs
			  in SOME (P.EXPANS (X.Expexp labs'))
			  end
		     else if CL.classIsVRC class
		     then NONE
		     else if CL.classIsVAL class orelse
			     CL.classIsPAT class
		     then let val labs' = L.union labels labs
			  in SOME (P.EXPANS (X.Expdep (lid, labs')))
			  end
		     else raise EH.DeadBranch "wrong kind of identifier"
		   | (NONE, SOME _, true)  => SOME x (* we went down some structure but couldn't find all of lid             *)
		   | (NONE, NONE,   true)  => NONE   (* we went down some structure but even start looking for lid           *)
		   | (NONE, SOME _, false) => SOME x (* we didn't start going down some structure and didn't find all of lid - can this ever happen? *)
		   | (NONE, NONE,   false) => SOME x (* we didn't start going down some structure and didn't find lid at all *)
	    else NONE
	  | solveMono (P.EXPANS (X.Expexp labels)) =
	    if FI.testtodos filters labels
	    then SOME (P.EXPANS (X.Expexp labels))
	    else NONE
	  | solveMono (P.MONBIN labels) =
	    if FI.testtodos filters labels
	    then SOME (P.MONBIN labels)
	    else NONE

	fun solvePoly P.POLY = P.POLY
	  | solvePoly (P.MONO monos) =
	    case List.mapPartial (fn mono => solveMono mono) monos of
		[] => P.POLY
	      | (mono :: _) => P.MONO [mono]


	(* ====== PARTIAL ENV SOLVER ====== *)

	fun preSolveEnv (E.ENV_VAR (ev, lab)) =
	    (case S.getValStateEv state ev of
		 NONE => E.ENV_VAR (ev, lab)
	       | SOME env => preSolveEnv env)
	  | preSolveEnv (E.ENVDEP (env, labs, stts, deps)) =
	    let val env' = preSolveEnv env
	    in E.pushExtEnv env' labs stts deps
	    end
	  | preSolveEnv (E.ROW_ENV (env1, env2)) =
	    let val env1' = preSolveEnv env1
		val env2' = preSolveEnv env2
	    in E.ROW_ENV (env1', env2')
	    end
	  | preSolveEnv env = env


	(* ====== BINDING SOLVER ====== *)

	fun solveextvarkeep bind =
	    (* NOTE: we might want to use 'FI.getStateLabs filters (EL.getExtLabL bind)' here. *)
	    case (*FI.getStateLab filters (E.getBindL bind)*)
		FI.getStateLabs filters (EL.getExtLabL bind) of
		FI.IN   => let val bind1 =
				   case E.getBindC bind of
				       CL.CLVAR clv =>
				       let val (cl, labs, stts, deps) = buildClass (CL.CLVAR clv) state
					   (*(2010-06-10)NOTE: we build up the class for values because
					    * they can be class variables in the case of exceptions.
					    * Is that the only case when a class can be a variable?*)
					   fun updcl x = C.updClass x cl
				       in EL.updExtLab (EL.mapExtLab bind updcl) labs stts deps
				       end
				     | _ => bind
			       (*(*N*)val (ty, labs, stts, deps) = buildty (E.getBindT bind1) state I.empty false*)
			       (*(*N*)val bind1 = EL.mapExtLab bind1 (fn bd => C.mapBind bd (fn _ => ty))*)
			       (*(*N*)val bind1 = EL.updExtLab bind1 labs stts deps*)
			       val poly1 = E.getBindP bind1
			       val poly2 = solvePoly poly1
			       (*val _ = D.printdebug2 (P.toString poly1 ^ "\n" ^ P.toString poly2)*)
			       val (labs, stts, deps) = P.getLabsPoly poly2
			       val bind2 = EL.mapExtLab bind1 (fn x => C.updPoly x poly2)
			       val bind3 = EL.updExtLab bind2 labs stts deps
			   in if P.isMono poly1 andalso P.isPoly poly2
			      then BINDPOL bind3
			      else BINDIN  bind3
			   end
	      | FI.OUT  => BINDOUT
	      | FI.BIND => BINDDUM (E.consBindPoly {id=(E.getBindI bind),
						    typeOfId=(T.newTYPE_VAR ()),
						    classOfId=(CL.consANY ()),
						    labelOfConstraint=(E.getBindL bind)})

	fun solveextvarcontext (bind as (_, labs, stts, deps)) cl =
	    let val id  = E.getBindI bind
		val lab = E.getBindL bind
		val lid = I.idToLid id lab
		fun generateAcc () =
		    let val a = E.genValueIDAccessor (E.consAccId lid (E.getBindT bind) cl lab) labs stts deps
		    in BINDNOT (E.singleConstraint (lab, E.ACCESSOR_CONSTRAINT a))
		    end
	    in case S.getValStateIdVa state lid false of
		   (SOME (({id, bind = _, lab = _, poly, class}, labs', stts', deps'), _), _, _) =>
		   (case class of
			CL.VID CL.VAL => BINDIN (EL.updExtLabD bind (CD.sing id lab))
		      | CL.VID CL.PAT => BINDIN (EL.updExtLabD bind (CD.sing id lab)) (* The one in the context comes from an AS in a pattern *)
		      | CL.VID CL.REC => BINDIN (EL.updExtLab (EL.mapExtLab bind C.toVRC) labs' (L.union labs' stts') deps')
		      | CL.VID CL.VRC => BINDIN (EL.updExtLab (EL.mapExtLab bind C.toVRC) labs' (L.union labs' stts') deps')
		      | CL.VID CL.CO0 => generateAcc ()
		      | CL.VID CL.CO1 => generateAcc ()
		      | CL.VID CL.CON => generateAcc ()
		      | CL.VID CL.DA0 => generateAcc ()
		      | CL.VID CL.DA1 => generateAcc ()
		      | CL.VID CL.DAT => generateAcc ()
		      | CL.VID CL.EX0 => generateAcc ()
		      | CL.VID CL.EX1 => generateAcc ()
		      | CL.VID CL.EXC => generateAcc ()
		      (* NOTE: if in the env there is a dummy binding then we can't
		       * generate a binding because we don't know if the one in the env
		       * comes from a CON or a REC or something else.*)
		      | CL.ANY     => BINDDUM (E.consBindPoly {id=id,
							       typeOfId=(T.newTYPE_VAR ()),
							       classOfId=(CL.consANY ()),
							       labelOfConstraint=lab})
		      | CL.CLVAR _ => BINDDUM (E.consBindPoly {id=id,
							       typeOfId=(T.newTYPE_VAR ()),
							       classOfId=(CL.consANY ()),
							       labelOfConstraint=lab})
		      | class => (print (CL.toString class); raise EH.DeadBranch "identifier in env has an unexpected status"))
		 (* NOTE: if the env in the state hides with env variables
		  * then we can't know if one of these variable should in fact declare
		  * a CON or EXC binder for the same if as the current pseudo-binder. *)
		 | _ => if S.hasEnvVar state
			then BINDDUM (E.consBindPoly {id=id,
						      typeOfId=(T.newTYPE_VAR ()),
						      classOfId=(CL.consANY ()),
						      labelOfConstraint=lab})
			else BINDIN (EL.updExtLabD bind (CD.sing id lab))
	    end

	fun solveextvarrebind bind =
	    case E.getBindC bind of
		CL.VID CL.VAL => solveextvarcontext bind (CL.consPAT ())
	      | CL.VID CL.PAT => solveextvarcontext bind (CL.consVRC ())
	      | CL.VID CL.VRC => BINDIN bind
	      | CL.VID CL.REC => BINDIN bind
	      | CL.VID CL.CO0 => BINDIN bind
	      | CL.VID CL.CO1 => BINDIN bind
	      | CL.VID CL.CON => BINDIN bind
	      | CL.VID CL.DA0 => BINDIN bind
	      | CL.VID CL.DA1 => BINDIN bind
	      | CL.VID CL.DAT => BINDIN bind
	      | CL.VID CL.EX0 => BINDIN bind
	      | CL.VID CL.EX1 => BINDIN bind
	      | CL.VID CL.EXC => BINDIN bind
	      | CL.ANY        => BINDIN bind
	      | CL.CLVAR _    => BINDDUM (E.consBindPoly {id=(E.getBindI bind),
							  typeOfId=(T.newTYPE_VAR ()),
							  classOfId=(CL.consANY ()),
							  labelOfConstraint=(E.getBindL bind)})
	      | class => (print (CL.toString class); raise EH.DeadBranch "identifier in binder has an unexpected status")

	fun solvedumvarbind bind =
	    case S.getValStateIdVa state (I.idToLid (E.getBindI bind) (E.getBindL bind)) false of
		(SOME (bind', _), _, _) =>
		(case E.getBindC bind' of
		     CL.VID CL.VAL => BINDDUM bind
		   | CL.VID CL.PAT => BINDDUM bind
		   | CL.VID CL.REC => BINDDUM bind
		   | CL.VID CL.VRC => BINDDUM bind
		   | CL.VID CL.CO0 => BINDNOT E.emptyConstraint
		   | CL.VID CL.CO1 => BINDNOT E.emptyConstraint
		   | CL.VID CL.CON => BINDNOT E.emptyConstraint
		   | CL.VID CL.DA0 => BINDNOT E.emptyConstraint
		   | CL.VID CL.DA1 => BINDNOT E.emptyConstraint
		   | CL.VID CL.DAT => BINDNOT E.emptyConstraint
		   | CL.VID CL.EX0 => BINDNOT E.emptyConstraint
		   | CL.VID CL.EX1 => BINDNOT E.emptyConstraint
		   | CL.VID CL.EXC => BINDNOT E.emptyConstraint
		   | CL.ANY        => BINDDUM bind
		   | CL.CLVAR _    => BINDDUM bind
		   | class => (print (CL.toString class); raise EH.DeadBranch "identifier in env has an unexpected status"))
	      | _ => BINDDUM bind

	fun solveextvar bind =
	    ((*D.printdebug2 (L.printLab (E.getBindL bind));*)
	     case solveextvarkeep bind of
		 BINDIN  bind' => solveextvarrebind bind'
	       | BINDPOL bind' => solveextvarrebind bind'
	       (*(2010-06-21)In the case of a BINDDUM ANY then we still need to check
		* that the context doesn't hide a real env in case E.getBindC bind
		* is a VAL or a PAT and that the id in the env is a CON or an EXC. *)
	       | BINDDUM bind' => if CL.classIsVAL (E.getBindC bind) orelse
				     CL.classIsPAT (E.getBindC bind)
				  then solvedumvarbind bind'
				  else BINDDUM bind'
	       | sbind => sbind)

	fun solveexttyp (bind as (btyp, labs, stts, deps)) =
	    case FI.getStateLabs filters (EL.getExtLabL bind)
	     (*FI.getStateLab filters (E.getBindL bind)*) of
		FI.IN   => let val (tyf, tnKind, cons) = E.getBindT bind
			       val tyf' = buildtypeFunction tyf state I.empty false false
			   (*val _ = D.printdebug2 (S.printState state ^ "\n" ^ I.printId (E.getBindI bind) ^ "\n" ^ T.printtyf tyf ^ "\n" ^ T.printtyf tyf')*)
			   in BINDIN (C.mapBind btyp (fn _ => (tyf', tnKind, cons)),
				      labs,
				      stts,
				      deps)
			   end
	      | FI.OUT  => BINDOUT
	      | FI.BIND => BINDDUM (E.consBindPoly {id=(E.getBindI bind),
						    typeOfId=(E.getBindT bind),
						    classOfId=(CL.consANY ()),
						    labelOfConstraint=(E.getBindL bind)})

	fun solveextovc (bind as (bovc, labs, stts, deps)) =
	    case FI.getStateLabs filters (EL.getExtLabL bind) of
		FI.IN   => let val ovc  = E.getBindT bind
			       val ovc' = buildseqty ovc state I.empty false false
			   in BINDIN (C.mapBind bovc (fn _ => ovc'),
				      labs,
				      stts,
				      deps)
			   end
	      | FI.OUT  => BINDOUT
	      | FI.BIND => BINDDUM (E.consBindPoly {id=(E.getBindI bind),
						    typeOfId=(E.getBindT bind),
						    classOfId=(CL.consANY ()),
						    labelOfConstraint=(E.getBindL bind)})

	fun solveexttyv bind =
	    case FI.getStateLab filters (E.getBindL bind) of
		FI.IN   => (case E.getBindT bind of
				(tv, true)  => BINDIN bind
			      | (tv, false) =>
				let val id  = E.getBindI bind
				    val lab = E.getBindL bind
				    val lid = I.idToLid id lab
				in case S.getValStateIdTv state lid false of
				       (SOME (({id, bind = (_, true),  lab = _, poly, class}, _, _, _), _), _, _) => BINDNOT E.emptyConstraint
				     | (SOME (({id, bind = (_, false), lab = _, poly, class}, _, _, _), _), _, _) => BINDNOT E.emptyConstraint
				     | _ => BINDIN bind
				end)
	      | FI.OUT  => BINDOUT
	      | FI.BIND => BINDDUM (E.consBindPoly {id=(E.getBindI bind),
						    typeOfId=(E.getBindT bind),
						    classOfId=(CL.consANY ()),
						    labelOfConstraint=(E.getBindL bind)})

	fun solveextenv bind =
	    (case FI.getStateLab filters (E.getBindL bind) of
		 FI.IN   => BINDIN bind
	       (*let val env1 = E.getBindT bind
		     val env2 = preSolveEnv env1
		 in BINDIN (EL.mapExtLab bind (fn benv => C.mapBind benv (fn _ => env2)))
		 end*)
	       | FI.OUT  => BINDOUT
	       | FI.BIND => BINDDUM (E.consBindPoly {id=(E.getBindI bind),
						     typeOfId=(E.getBindT bind),
						     classOfId=(CL.consANY ()),
						     labelOfConstraint=(E.getBindL bind)}))

	fun solveextfun bind =
	    ((*D.printdebug2 (FI.toString filters ^ "\n" ^ I.printId (E.getBindI bind));*)
	     case FI.getStateLab filters (E.getBindL bind) of
		 FI.IN   => BINDIN bind
	       | FI.OUT  => BINDOUT
	       | FI.BIND => BINDDUM (E.consBindPoly {id=(E.getBindI bind),
						     typeOfId=(E.getBindT bind),
						     classOfId=(CL.consANY ()),
						     labelOfConstraint=(E.getBindL bind)}))
	(*(EL.mapExtLab (E.resetExtLab bind) C.resetPoly)*)

	fun genMultiError (SOME bind1) bind2 =
	    let val cl1 = E.getBindC bind1
		val cl2 = E.getBindC bind2
	    (*val _ = D.printdebug2 (CL.toString cl1 ^ " " ^ CL.toString cl2)*)
	    in if (CL.classIsVAL cl1 orelse CL.classIsVRC cl1)
		  andalso
		  (CL.classIsVAL cl2 orelse CL.classIsVRC cl2)
	       then let val labs = L.union  (EL.getExtLabL bind1) (EL.getExtLabL bind2)
			val stts = L.union  (EL.getExtLabE bind1) (EL.getExtLabE bind2)
			val deps = CD.union (EL.getExtLabD bind1) (EL.getExtLabD bind2)
			val ek   = EK.MultiOcc NONE
		    in raise errorfound (ERR.consPreError ERR.dummyId labs deps ek stts)
		    end
	       else ()
	    end
	  | genMultiError _ _ = raise EH.DeadBranch ""

	(* NOTE: the difference between solvegenenv' and solvegenenv is that
	 * solvegenenv' records only one binder per identifier. *)
	fun solvegenenv' genenv fbuild bmon =
	    E.foldrienv
		(fn (id, sem, (genenv, comp, cst)) =>
		    let val (ubind, _, _, comp', cst') =
			    foldr (fn (bind, (ubind, bin, bpol, bcomp, cst)) =>
				      case fbuild bind of
					  BINDOUT       => (ubind, bin, bpol, bcomp, cst)
					| BINDNOT cst'  => (ubind, bin, bpol, bcomp, E.unionConstraintsList [cst, cst'])
					| BINDIN  bind' =>
					  (if bin then genMultiError ubind bind' else ();
					   (SOME bind', true, bpol, true, cst))
					| BINDPOL bind' =>
					  if bin
					  then (ubind, bin, bpol, bcomp, cst)
					  else (SOME bind', bin, true, bcomp, cst)
					| BINDDUM bind' =>
					  if bin orelse bpol
					  then (ubind, bin, bpol, bcomp, cst)
					  else (SOME bind', bin, bpol, bcomp, cst))
				  (NONE, false, false, false, cst)
				  sem
			val comp' = comp' andalso comp
		    in case ubind of
			   NONE => (genenv, comp', cst')
			 | SOME bind => (E.addenv (id, [bind]) genenv, comp', cst')
		    end)
		(E.emptyMap, true, E.emptyConstraint)
		genenv


	fun solvevarenv vars bmon = solvegenenv' vars solveextvar bmon
	fun solvetypenv typs bmon = solvegenenv' typs solveexttyp bmon
	fun solvetyvenv typs bmon = solvegenenv' typs solveexttyv bmon
	(* We build for overloading classes only because of the
	 * overloaded constants. *)
	fun solveovcenv ovcs bmon = solvegenenv' ovcs solveextovc bmon
	fun solvestrenv strs bmon = solvegenenv' strs solveextenv bmon
	fun solvesigenv sigs bmon = solvegenenv' sigs solveextenv bmon
	fun solvefunenv funs bmon = solvegenenv' funs solveextfun bmon


	(* ====== ENV SOLVER ====== *)

	fun solveenv (E.ENV_VAR (ev, lab)) bmon =
	    (case S.getValStateEv state ev of
		 NONE => E.ENV_VAR (ev, lab)
	       | SOME env =>
		 let (*val env' = solveenv env bmon*)
		     val env' = preSolveEnv env
		 in env'
		 end)
	  (* Here we don't need to re-solve env, preSolveEnv is enough *)
	  | solveenv (env as E.ENV_CONS _) bmon =
	    (let val (vids, compValueIds, cst) = solvevarenv (E.getValueIds env) bmon
		 val (typs, compTyps, _)   = solvetypenv (E.getTypeNameEnv env) bmon
		 val (tyvs, compTyvs, _)   = solvetyvenv (E.getExplicitTypeVars env) bmon
		 val (strs, compStrs, _)   = solvestrenv (E.getStructs env) bmon
		 val (sigs, compSigs, _)   = solvesigenv (E.getSigs env) bmon
		 val (funs, compFuns, _)   = solvefunenv (E.getFunctors env) bmon
		 val (ovcs, compOvcs, _)   = solveovcenv (E.getOverloadingClasses env) bmon
		 val cmp   = compValueIds andalso compTyps andalso compTyvs andalso
			     compStrs andalso compSigs andalso compFuns andalso
			     compOvcs andalso E.getIComplete env
		 val nfo   = E.consInfo (E.getILab env) cmp (E.getITypeNames env) (E.getIArgOfFunctor env)
		 val _     = run cst
		 val env'  = E.consEnvConstructor vids typs tyvs strs sigs funs ovcs nfo
	     (*val _     = D.printdebug2 (E.printEnv env' "")*)
	     in env'
	     end
	     handle errorfound err => handleSolveEnv err env)
	  | solveenv (E.ROW_ENV (env1, env2)) bmon =
	    let val env1' = solveenv env1 bmon
		val b     = E.isEmptyEnv env1'
		val tvs   = S.pushEnvToState b env1' state
		val env2' = solveenv env2 bmon
		val _     = S.remEnvFromState b tvs state
	    in E.plusEnv env1' env2'
	    end
	  | solveenv (E.ENVOPN opnenv) bmon = solveopnenv opnenv bmon
	  | solveenv (E.ENVDEP (env, labs, stts, deps)) bmon =
	    (case FI.getStateLabs filters labs of
		 FI.OUT  => E.emptyEnv
	       | FI.BIND => E.newEnvVar L.dummyLab
	       | FI.IN   => E.pushExtEnv (solveenv env bmon) labs stts deps)
	  | solveenv (E.FUNCTOR_ENV cst) bmon = E.FUNCTOR_ENV cst
	  | solveenv (E.CONSTRAINT_ENV cst) bmon = (run cst; E.emptyEnv)
	  | solveenv (E.ENVPOL (tyvenv, env)) bmon =
	    let val env1  = solveenv (E.projExplicitTypeVars tyvenv) bmon
		val b     = E.isEmptyEnv env1
		val tvs   = S.pushEnvToState b env1 state
		val env'  = solveenv env bmon
		val _     = S.remEnvFromState b tvs state
		fun checkTyVars (env as E.ENV_CONS _) labs stts deps =
		    (* NOTE: Should only contain ValueIds's so we only deal with them. *)
		    let val dom  = E.dom (E.getExplicitTypeVars env1)
			val vids = buildVarEnv (E.toPolyValueIds (E.getValueIds env)) state dom bmon
		    (*val _    = D.printdebug2 (S.printState state)*)
		    (*val _    = D.printdebug2 (E.printEnv env "")*)
		    (*val _    = D.printdebug2 (E.printEnv (E.projValueIds vids) "")*)
		    in case getExplicitTyVars vids (E.getExplicitTypeVars env1) state of
			   NONE => E.updateValueIds vids env
			 | SOME (id, lab, labs0, stts0, deps0) =>
			   let val ek    = EK.TypeVarBind NONE
			       val labs1 = L.union  labs labs0
			       val stts1 = L.union  stts stts0
			       val deps1 = CD.union deps deps0
			       val err   = ERR.consPreError ERR.dummyId labs1 deps1 ek stts1
			   (*val _ = D.printdebug2 (E.printEnv (E.projValueIds vids) "")*)
			   in handleSolveEnv err (E.projValueIds vids)
			   end
		    end
		  | checkTyVars (env as E.ENV_VAR _) _ _ _ = env
		  | checkTyVars (E.ENVDEP (env, labs1, stts1, deps1)) labs stts deps =
		    let val labs2 = L.union  labs1 labs
			val stts2 = L.union  stts1 stts
			val deps2 = CD.union deps1 deps
			val env'  = checkTyVars env labs2 stts2 deps2
		    in E.pushExtEnv env' labs1 stts1 deps1
		    end
		  | checkTyVars _ _ _ _ = raise EH.DeadBranch ""
	    in checkTyVars env' L.empty L.empty CD.empty
	    end
	  | solveenv (E.DATATYPE_CONSTRUCTOR_ENV (idlab, env)) bmon =
	    let val env' = solveenv env bmon
		val _    = S.updateDatCons state idlab env'
	    in env'
	    end
	  | solveenv (E.LOCAL_ENV (env1, env2)) bmon =
	    let val env1' = solveenv env1 bmon
		val b     = E.isEmptyEnv env1'
		val tvs   = S.pushEnvToState b env1' state
		val env2' = solveenv env2 bmon
		val _     = S.remEnvFromState b tvs state
	    in env2'
	    end
	  | solveenv (envwhere as E.ENVWHR (env, longtyp)) bmon =
	    (let (*val env0 = solveenv env bmon*)
		 (*val _ = D.printdebug2 (E.printEnv env0 "")*)
		 (*val (labs1, stts1, deps1) = (L.empty, L.empty, CD.empty)*)
		 val env0 = justBuildEnv (solveenv env bmon) state false
		 val solvedLongTyp = solveLongTyp longtyp
		 (*Modify matchWhere so that it takes solvedlongTyp instead of env2
		  * We can then do the matchSigStr directly in the matchWhereEnv.
		  * Why do we do this matchSigStr anyway?  We do that to catch the
		  * unmatch errors but we can also do that in the matchWhereEnv.*)
		 (*val _ = D.printdebug2 ("[where]")*)
		 (*The state is needed to check if the where clause does not try to
		  * rebind a type name from the env. *)
		 (*val _ = D.printdebug2 (E.printEnv env0 "")*)
		 val (tmap, _) = matchWhereEnv env0 solvedLongTyp state
		 val (cs0, env1) = applyTypeFunctionEnv env0 tmap
		 val _ = sigVsStrON ()
		 val _ = fsimplify cs0 L.dummyLab
		 val _ = sigVsStrOFF ()
		 val env1' = justBuildEnv env1 state false
	     (*val _ = D.printdebug2 (E.printEnv env1' "")*)
	     in env1'
	     end
	     handle errorfound err => handleSolveEnv err envwhere)
	  | solveenv (envsha as E.ENVSHA (e1, e2)) bmon =
	    (let val env0 = justBuildEnv e1 state false
		 val env2 = justBuildEnv e2 state false
		 val (utf, tfun) = getTypeFunctionEnvSha env0 env2
		 val tfun = case utf of
				NONE => OM.map (fn _ => newTypeFunction ()) tfun
			      | SOME (tf, labs, stts, deps) =>
				OM.map (fn NONE => newTypeFunction ()
					 | SOME (labs', stts', deps') =>
					   collapseTf tf
						      (L.union labs labs')
						      (L.union  stts stts')
						      (CD.union deps deps'))
				       tfun
		 val (cs0, env1) = genTypeFunctionEnv' env0 state tfun false
		 (* Do we need this switch here? *)
		 val _ = sigVsStrON ()
		 val _ = fsimplify cs0 L.dummyLab
		 val _ = sigVsStrOFF ()
	     (*val _ = if E.isEmptyEnv env1
		       then S.updateStateEv state ev1 (env1, L.cons lab labs, stts, deps)
		       else S.updateStateEv state ev1 (env1, L.singleton lab, L.empty, CD.empty)*)
	     in env1
	     end
	     handle errorfound err => handleSolveEnv err envsha)
	  | solveenv (envsig as E.SIGNATURE_ENV (e1, e2, kind)) bmon =
	    (let val env0 = buildFEnv e1 state false
		 val env2 = buildFEnv e2 state true
		 val _ = D.printdebug2 (E.printEnv env0 "" ^ "\n" ^ E.printEnv env2 "")
		 val (tfnD, tfnT) = getTypeFunctionEnv env0 env2 L.empty L.empty CD.empty
		 val (cs0, env1) = genTypeFunctionEnv' env0 state tfnT true
		 val (cs1, cs2) = matchSigStr env1 env2 L.dummyLab filters L.empty L.empty CD.empty true err
		 val _ = sigVsStrON ()
		 val _ = fsimplify (cs0 @ cs1) L.dummyLab
		 val _ = sigVsStrTypON ()
		 val _ = fsimplify cs2 L.dummyLab
		 val _ = sigVsStrTypOFF ()
		 val _ = sigVsStrOFF ()
	     in case kind of
		    E.OPAQUE => freshenv' (renameenv' env0 state) (SOME O.empty) false
		  | E.TRANSLUCENT => justBuildEnv env1 state true
	     end
	     handle errorfound err => handleSolveEnv err envsig)
	  | solveenv E.TOP_LEVEL_ENV bmon = raise EH.TODO "no description, raised in the 'solveenv' function of Unification.sml"
	  | solveenv (E.ENVPTY st) bmon = raise EH.TODO "no description, raised in the 'solveenv' function of Unification.sml"
	  | solveenv (E.ENVFIL (file, env, strm)) bmon =
	    let val _ =
		    case user of
			ENUM => (print ("[Skalpel: analysing " ^ file ^ "]\n");
				 if (String.isSubstring "basis.sml" file)
				 then analysingBasis := true
				 else analysingBasis := false)
		      | _    => ()
		val env1 = solveenv env bmon
		val b    = E.isEmptyEnv env1
		val tvs  = S.pushEnvToState b env1 state
		val env2 = (*E.emptyEnv*) solveenv (strm ()) bmon
		val _    = S.remEnvFromState b tvs state
	    in E.plusEnv env1 env2
	    end

	and handleSolveEnv err env =
	    if bcontinue
	    then env
	    else raise errorex err


	(* ====== LONGTYP SOLVER ====== *)

	and solveLongTyp ({lid, sem, class, lab}, labs, stts, deps) =
	    let fun genDum () = SOME ({lid = lid, sem = sem, class = CL.consANY (), lab = lab},
				      L.empty,
				      L.empty,
				      CD.empty)
	    in case FI.getStateLab filters lab of
		   FI.OUT  => NONE
		 | FI.BIND => genDum ()
		 | FI.IN   =>
		   (case filterLid lid filters of
			NONE => genDum ()
		      | SOME (_, true) =>
			let val sem0 = buildtypeFunction sem state I.empty false false
			in SOME ({lid = lid, sem = sem0, class = class, lab = lab},
				 L.union labs (I.getLabs lid),
				 stts,
				 deps)
			end
		      | SOME (lid', false) =>  genDum ())
	    end


	(* ====== OPENING SOLVER ====== *)

	and solveopnsem (opnsem as (lid, lab, E.OPENED_STRUCT)) bmon = (* opening of a structure *)
	    (case filterLid lid filters of
		 NONE => E.newEnvVar lab
	       | SOME (_, true) =>
		 (case S.getValStateIdSt state lid true of
		      (SOME ((bind, labs, stts, deps), _), _, _) =>
		      let val labs' = L.cons lab (L.union (I.getLabs lid) labs)
			  val env   = preSolveEnv ((*solveenv*) (C.getBindT bind) (*bmon*))
		      (*val _ = D.printdebug2 (E.printEnv env "")*)
		      in E.pushExtEnv env labs' stts deps
		      end
		    | (_, SOME (x as ((id1, lab1), (env, labs', stts', deps'))), true) =>
		      (handleUnmatched lid (L.singleton lab) L.empty CD.empty x lab; E.newEnvVar lab)
		    | (NONE, _, false) => (* FREE ID *)
		      (handleFreeIdent lid true; E.newEnvVar lab)
		    | _ => E.newEnvVar lab)
	       | SOME (lid', false) => E.newEnvVar lab)
	  | solveopnsem (opnsem as (lid, lab, E.DATATYPE_REPLICATION)) bmon = (* datatype replication *)
	    (case filterLid lid filters of
		 NONE => E.newEnvVar lab
	       | SOME (_, true) =>
		 (case S.getValStateIdTy state lid false of
		      (SOME (({id, bind = (bind, tnKind, cons), lab = _, poly, class = CL.ANY}, labs, stts, deps), _), _, _) =>
		      E.newEnvVar lab
		    | (SOME (({id, bind = (bind, tnKind, cons), lab = _, poly, class}, labs, stts, deps), _), _, _) =>
		      let val labs' = L.cons lab (L.union (I.getLabs lid) labs)
			  val (varenv, cmp) = !cons
		      (*val _ = D.printdebug2 (I.printLid lid ^ " " ^ L.printLab lab)*)
		      in E.pushExtEnv (E.projValueIds varenv) labs' stts deps
		      end
		    | (_, SOME (x as ((id1, lab1), (env, labs', stts', deps'))), true) =>
		      (handleUnmatched lid (L.singleton lab) L.empty CD.empty x lab; E.newEnvVar lab)
		    | (NONE, _, false) => (* FREE ID *)
		      (handleFreeIdent lid true; E.newEnvVar lab)
		    | _ => E.newEnvVar lab)
	       | SOME (lid', false) => E.newEnvVar lab)
	  | solveopnsem (lid, lab, E.INCLUDED_SIG) bmon = (* signature inclusion *)
	    (case filterLid lid filters of
		 NONE => E.newEnvVar lab
	       | SOME (_, true) =>
		 (case S.getValStateIdSi state lid true of
		      (SOME ((bind, labs, stts, deps), _), _, _) =>
		      let val labs' = L.cons lab (L.union (I.getLabs lid) labs)
		      in E.pushExtEnv (solveenv (C.getBindT bind) bmon) labs' stts deps
		      end
		    | (_, SOME (x as ((id1, lab1), (env, labs', stts', deps'))), true) =>
		      (handleUnmatched lid (L.singleton lab) L.empty CD.empty x lab; E.newEnvVar lab)
		    | (NONE, _, false) => (* FREE ID *)
		      (handleFreeIdent lid true; E.newEnvVar lab)
		    | _ => E.newEnvVar lab)
	       | SOME (lid', false) => E.newEnvVar lab)

	and solveopnenv opnenv bmon =
	    E.foldlOEnv (fn (opnsem as (_, lab, _), env) =>
			    case FI.getStateLab filters lab of
				FI.OUT  => env
			      | FI.BIND => let val env' = E.newEnvVar lab
					   in E.plusEnv env env'
					   end
			      | FI.IN   => let val env' = solveopnsem opnsem bmon
					   in E.plusEnv env env'
					   end)
			E.emptyEnv
			opnenv


	(* ====== ACCESSOR SOLVER ====== *)

	and solveacc (acc as E.VALUEID_ACCESSOR ({lid, sem, class, lab}, labs, stts, deps)) l =
	    (D.printDebugFeature D.UNIF D.CONSTRAINT_SOLVING (fn _ => "solving the following VALUEID_ACCESSOR accessor:\n"^(#purple D.colors)^(E.printOneAccessor acc));
	    (case filterLid lid filters of
		 NONE => ()
	       | SOME (_, true) =>
		 (case S.getValStateIdVa state lid false of
		      (SOME (({id, bind, lab, poly, class = CL.ANY}, _, _, _), _), _, _) => ()
		    | (SOME (({id, bind, lab = l, poly, class = cl}, labs', stts', deps'), b), _, _) =>
		      (* if: - b    is true (coming from the parameter of a functor)
		       *     - lid  is a long identifier
		       *     - bind is a type variable
		       *     - poly is polymorphic
		       * then lid comes from a structure and is completely unconstrained
		       * we can then try to give its binding some structure. *)
		      if b             andalso
			 I.isLong lid  andalso
			 T.isTyV  bind andalso
			 P.isPoly poly andalso
			 not (isInState bind state) (* We need to do that because bind might be a
						     * variable because of monomorphism issues.
						     * In case of completely forbidding to constraint
						     * bind we can check if it is GEN and then chain
						     * the GENs. *)
		      then let val (labs0, stts0, deps0) = unionLabs (labs, stts, deps) (labs', stts', deps')
			       val labs1 = L.union labs0 (I.getLabs lid)
			   in updateStateTyGen state bind sem labs1 stts0 deps (* returns a unit *)
			   end
		      else let val (labs0, stts0, deps0) = unionLabs (labs, stts, deps) (labs', stts', deps')
			       val labs1 = L.union labs0 (I.getLabs lid)
			       (*val timer = VT.startTimer ()*)
			       val bind1 = freshTy bind (SOME (S.getDomGe state)) poly
			       (*val _     = temp_time := !temp_time + (VT.getMilliTime timer)*)
			       val bind2 = T.labelBuiltinTy bind1 lab
			       val c1    = E.genCstTyAll sem bind2 labs1 stts0 deps0
			       val _ = D.printDebugFeature D.UNIF D.CONSTRAINT_SOLVING (fn _ => (#cyan D.colors)^"c1 = "^(E.printOneConstraint c1))
			       val c2    = E.genCstClAll class cl  labs1 stts0 deps0
			       val _ = D.printDebugFeature D.UNIF D.CONSTRAINT_SOLVING (fn _ => "c2 = "^(E.printOneConstraint c2))
			   in fsimplify [c1, c2] l
			   end
		    | (_, SOME ((id1, lab1), (env, labs', stts', deps')), true) =>
		      (D.printDebugFeature D.UNIF D.CONSTRAINT_SOLVING (fn _ => "case 2");
		      handleUnmatched lid labs stts deps ((id1, lab1), (env, labs', stts', deps')) l)
		    | (NONE, _, false) => (* FREE ID *)
		      (D.printDebugFeature D.UNIF D.CONSTRAINT_SOLVING (fn _ => "in this case, it is suspected we have a free identifier");
		      handleFreeIdent lid false)
		    | _ => ())
	       | SOME (lid', false) => ()))
	  | solveacc (E.EXPLICIT_TYPEVAR_ACCESSOR ({lid, sem, class, lab}, labs, stts, deps)) l =
	    (case filterLid lid filters of
		 NONE => ()
	       | SOME (_, true) =>
		 (case S.getValStateIdTv state lid false of
		      (SOME (({id, bind, lab, poly, class = CL.ANY}, _, _, _), _), _, _) => ()
		    | (SOME (({id, bind = (bind, b), lab = l, poly, class}, labs', stts', deps'), _), _, _) =>
		      let val (labs0, stts0, deps0) = unionLabs (labs, stts, deps) (labs', stts', deps')
			  val ty1 = T.TYPE_VAR (sem, NONE, T.POLY, T.UNKNOWN)
			  val ty2 = T.consTYPE_VAR (freshTypeVar' bind poly)
			  (*(2010-06-29) The order in the constraint actually matters to get the
			   * 'too general in signature' errors.  This needs to be fixed.
			   * Constraint solving shouldn't depend on the order in constraints. *)
			  (*val _   = D.printdebug2 (T.printty ty1 ^ "\n" ^ T.printty ty2)*)
			  val c   = E.genCstTyAll ty1 ty2 labs0 stts0 deps0
		      in fsimplify [c] l
		      end
		    | _ => ())
	       | SOME (lid', false) => ())

	  | solveacc (E.EQUALITY_TYPE_ACCESSOR ({lid, sem, class, lab}, labs, stts, deps)) l =
	    let
		val _ = D.printDebugFeature D.UNIF D.EQUALITY_TYPES (fn _ => "solving an equality type accessor. Labels = " ^ L.toString labs)
		(* val _ = print ("the state is: "^(S.printState state)^"\n\n\n"); *)
	    in
		case filterLid lid filters of
		    NONE => ()
		  | SOME (_, true) =>
		    (case S.getValStateIdVa state lid false of
			 (SOME (({id, bind, lab, poly, class = CL.ANY}, _, _, _), _), _, _) => ()
		       | (SOME (({id, bind, lab = l, poly, class = cl}, labs', stts', deps'), b), _, _) =>
			 let
			     val _ = print ("sem = "^(T.printEqualityType sem)^"\n")
			     val _ = print ("id = "^(Int.toString (I.toInt id))^"\n")
			     val _ = print ("bind = "^(T.printty bind)^"\n\n\n")

			     val (eqStatuses, eqStatusLabels) = T.stripEqualityStatus bind L.empty
			     (* (2012-06-24) jpirie: is this causing us to loose labels? What LABELS are there for NOT_EQUALITY_TYPE? *)
			     val findStatus = List.find (fn T.EQUALITY_TYPE => true | T.NOT_EQUALITY_TYPE => true | _ => false) eqStatuses

			     val _ = case bind of
					 T.TYPE_DEPENDANCY(T.TYPE_CONSTRUCTOR(_,_,_,T.EQUALITY_TYPE_VAR(x)),_,_,_) =>
					 let
					     (* need some handling here for the pattern match warnings *)
					     val E.EQUALITY_TYPE_CONSTRAINT ((T.EQUALITY_TYPE_VAR eqtv, T.EQUALITY_TYPE_VAR eqtv2), ls, deps, ids) = E.initEqualityTypeConstraint sem (T.EQUALITY_TYPE_VAR(x)) lab
					 in
					     (* jpirie: I unioned some more labels and it worked! Figure out precisely which labels I should be unioning :o) *)
					     fsimplify [ E.EQUALITY_TYPE_CONSTRAINT ((T.EQUALITY_TYPE_VAR eqtv, T.EQUALITY_TYPE_VAR(eqtv2)), L.cons (I.getLabId lid) (L.cons l (L.union (L.union labs labs') ls)), deps, ids) ] l
					 end
				       | T.TYPE_DEPENDANCY(T.TYPE_POLY(_,_,_,_,typePolyLabel,T.EQUALITY_TYPE_VAR(x)),depLabels,_,_) =>
					 let
					     (* need some handling here for the pattern match warnings *)
					     val E.EQUALITY_TYPE_CONSTRAINT ((T.EQUALITY_TYPE_VAR eqtv, T.EQUALITY_TYPE_VAR eqtv2), ls, deps, ids) = E.initEqualityTypeConstraint sem (T.EQUALITY_TYPE_VAR(x)) lab
					 in
					     (* jpirie: I unioned some more labels and it worked! Figure out precisely which labels I should be unioning :o) *)
					     fsimplify [ E.EQUALITY_TYPE_CONSTRAINT ((T.EQUALITY_TYPE_VAR eqtv, T.EQUALITY_TYPE_VAR(eqtv2)), L.union depLabels (L.cons typePolyLabel (L.cons (I.getLabId lid) (L.cons l (L.union (L.union labs labs') ls)))), deps, ids) ] l
					 end
				       | _ => ()

			 in
			     case (findStatus) of
				 SOME(x) =>
				 let
				     (* need some handling here for the pattern match warnings *)
				     val E.EQUALITY_TYPE_CONSTRAINT ((T.EQUALITY_TYPE_VAR eqtv, T.EQUALITY_TYPE_STATUS status), ls, deps, ids) = E.initEqualityTypeConstraint sem (T.EQUALITY_TYPE_STATUS(x)) lab
				 in
				     (* jpirie: I unioned some more labels and it worked! Figure out precisely which labels I should be unioning :o) *)
				     fsimplify [ E.EQUALITY_TYPE_CONSTRAINT ((T.EQUALITY_TYPE_VAR eqtv, T.EQUALITY_TYPE_STATUS status), L.cons (I.getLabId lid) (L.cons l (L.union eqStatusLabels (L.union (L.union labs labs') ls))), deps, ids) ] l
				 end
			       | NONE => ()
			 end
		       | (_, SOME ((id1, lab1), (env, labs', stts', deps')), true) =>
			 ()
		       | (NONE, _, false) => (* FREE ID *)
			 ()
		       | _ => ())
		  | SOME _ => ()
	    end

	  | solveacc (E.TYPE_CONSTRUCTOR_ACCESSOR ({lid, sem, class, lab}, labs, stts, deps)) l =
	    (case filterLid lid filters of
		 NONE => ()
	       | SOME (_, true) =>
		 (case S.getValStateIdTy state lid false of
		      (SOME (({id, bind = (bind, _, _), lab = l, poly, class = CL.ANY}, _, _, _), _), _, _) =>
		      (*(2010-06-16)We used to return () but we need to do something
		       * else to catch the arity errors.*)
		      (*let val (tf, labs', stts', deps') = buildTypeFunctionAr state bind lab L.empty L.empty CD.empty
			    (*val _ = D.printdebug2 (S.printState state)*)
			    (*val _ = D.printdebug2 (T.printtyf bind)*)
			    val (labs0, stts0, deps0) = unionLabs (labs, stts, deps) (labs', stts', deps')
			    val c = E.genCstTfAll sem tf (L.union labs0 (I.getLabs lid)) stts0 deps0
			in fsimplify [c] l
			end*)
		      let val sq   = S.getValStateAr state lid (SOME l)
			  val labs = L.union labs (I.getLabs lid)
			  val tf   = T.TFC (sq, T.newTYPE_VAR (), lab)
			  val c    = E.genCstTfAll sem tf labs stts deps
		      in fsimplify [c] l
		      end
		    | (SOME (({id, bind = (bind, _, _), lab = l, poly, class}, labs', stts', deps'), _), _, _) =>
		      let val (labs0, stts0, deps0) = unionLabs (labs, stts, deps) (labs', stts', deps')
			  val labs1 = L.union labs0 (I.getLabs lid)
			  val bind1 = freshTypeFunction bind true
			  val bind2 = T.labelBuiltinTyf bind1 lab
			  (*val _ = D.printdebug2 (T.printtyf bind  ^ "\n" ^
						   T.printtyf bind1 ^ "\n" ^
						   T.printtyf bind2 ^ "\n" ^
						   L.toString labs1)*)
			  val c     = E.genCstTfAll sem bind2 labs1 stts0 deps0
		      in fsimplify [c] l
		      end
		    | (_, SOME ((id1, lab1), (env, labs', stts', deps')), true) =>
		      ((*D.printdebug2 (E.printEnv env "");*)
		       (*D.printdebug2 (S.printState state);*)
		       handleUnmatched lid labs stts deps ((id1, lab1), (env, labs', stts', deps')) l)
		    | (_, _, b) =>
		      (*(2010-06-16)Similar as for ANY.*)
		      if b orelse S.hasEnvVar state
		      then () (*(2010-06-30)As for the other accessors, if b then we need to constrain
			       * the incomplete structure returned as second component in the triple
			       * to be at least sem. *)
		      else let val sq   = S.getValStateAr state lid NONE
			       val labs = L.union labs (I.getLabs lid)
			       val tf   = T.TFC (sq, T.newTYPE_VAR (), lab)
			       val c    = E.genCstTfAll sem tf labs stts deps
			       val _    = if enum then S.updateStateFr state (I.getLeftId lid) else () (* FREE ID *)
			   in fsimplify [c] l
			   end)
	       | SOME (lid', false) => ())
	  | solveacc (E.OVERLOADING_CLASSES_ACCESSOR ({lid, sem, class, lab}, labs, stts, deps)) l =
	    (case filterLid lid filters of
		 NONE => ()
	       | SOME (_, true) =>
		 (case S.getValStateIdOc state lid false of
		      (SOME (({id, bind, lab, poly, class = CL.ANY}, _, _, _), _), _, _) => ()
		    | (SOME (({id, bind, lab, poly, class}, labs', stts', deps'), _), _, _) =>
		      let val (labs0, stts0, deps0) = unionLabs (labs, stts, deps) (labs', stts', deps')
			  val labs1 = L.union labs0 (I.getLabs lid)
			  val c     = E.genCstSqAll sem bind labs1 stts0 deps0
		      (*val _     = D.printdebug2 (S.printState state)*)
		      (*val _     = D.printdebug2 (T.printseqty sem ^ "\n" ^ T.printseqty bind)*)
		      in fsimplify [c] l
		      end
		    | (_, SOME ((id1, lab1), (env, labs', stts', deps')), true) =>
		      handleUnmatched lid labs stts deps ((id1, lab1), (env, labs', stts', deps')) l
		    | (NONE, _, false) => (* FREE ID *)
		      handleFreeIdent lid false
		    | _ => ())
	       | SOME (lid', false) => ())
	  | solveacc (E.STRUCTURE_ACCESSOR ({lid, sem, class, lab}, labs, stts, deps)) l =
	    (case filterLid lid filters of
		 NONE => ()
	       | SOME (_, true) =>
		 (case S.getValStateIdSt state lid false of
		      (SOME (({id, bind, lab, poly, class = CL.ANY}, _, _, _), _), _, _) => ()
		    | (SOME (({id, bind, lab, poly, class}, labs', stts', deps'), _), _, _) =>
		      let val (labs0, stts0, deps0) = unionLabs (labs, stts, deps) (labs', stts', deps')
			  val labs1 = L.union labs0 (I.getLabs lid)
			  val c = E.genCstEvAll sem bind labs1 stts0 deps0
		      in fsimplify [c] l
		      end
		    | (_, SOME ((id1, lab1), (env, labs', stts', deps')), true) =>
		      handleUnmatched lid labs stts deps ((id1, lab1), (env, labs', stts', deps')) l
		    | (NONE, _, false) => (* FREE ID *)
		      handleFreeIdent lid false
		    | _ => ())
	       | SOME (lid', false) => ())
	  | solveacc (E.SIGNATURE_ACCESSOR ({lid, sem, class, lab}, labs, stts, deps)) l =
	    (case filterLid lid filters of
		 NONE => ()
	       | SOME (_, true) =>
		 (case S.getValStateIdSi state lid false of
		      (SOME (({id, bind, lab, poly, class = CL.ANY}, _, _, _), _), _, _) => ()
		    | (SOME (({id, bind, lab, poly, class}, labs', stts', deps'), _), _, _) =>
		      let val (labs0, stts0, deps0) = unionLabs (labs, stts, deps) (labs', stts', deps')
			  val labs1 = L.union labs0 (I.getLabs lid)
			  val c = E.genCstEvAll sem bind labs1 stts0 deps0
		      in fsimplify [c] l
		      end
		    | (_, SOME ((id1, lab1), (env, labs', stts', deps')), true) =>
		      handleUnmatched lid labs stts deps ((id1, lab1), (env, labs', stts', deps')) l
		    | (NONE, _, false) => (* FREE ID *)
		      handleFreeIdent lid false
		    | _ => ())
	       | SOME (lid', false) => ())
	  | solveacc (E.FUNCTOR_ACCESSOR ({lid, sem = (env1, env2), class, lab}, labs, stts, deps)) l =
	    (case filterLid lid filters of
		 NONE => ()
	       | SOME (_, true) =>
		 (case S.getValStateIdFu state lid false of
		      (SOME (({id, bind, lab, poly, class = CL.ANY}, _, _, _), _), _, _) => ()
		    | (SOME (({id, bind = (env3, env4), lab, poly, class}, labs', stts', deps'), _), _, _) =>
		      let val (labs0, stts0, deps0) = unionLabs (labs, stts, deps) (labs', stts', deps')
			  val labs1 = L.union labs0 (I.getLabs lid)
			  val c1 = E.genCstEvAll env1 env3 labs1 stts0 deps0
			  val c2 = E.genCstEvAll env2 env4 labs1 stts0 deps0
		      (*val _ = D.printdebug2 (S.printState state ^ "\n" ^ E.printEnv env2 "" ^ "\n" ^ E.printEnv env4 "" ^ "\n")*)
		      in fsimplify [c1, c2] l
		      end
		    | (_, SOME ((id1, lab1), (env, labs', stts', deps')), true) =>
		      handleUnmatched lid labs stts deps ((id1, lab1), (env, labs', stts', deps')) l
		    | (NONE, _, false) => (* FREE ID *)
		      handleFreeIdent lid false
		    | _ => ())
	       | SOME (lid', false) => ())

	(* Handles the case when a long identifier has not been found in a structure *)
	and handleUnmatched lid labs stts deps
			    ((id1, lab1), (env, labs', stts', deps'))
			    l =
	    if completeEnv env err
	    then let val (idlabs, labs1) = E.getLabsIdsEnv env 2
		     val labs2 = L.union (I.getLabs lid) (L.union labs labs1)
		     val (labs0, stts0, deps0) = unionLabs (labs2, stts, deps) (labs', stts', deps')
		     val lab2  = E.getLabEnv env
		     val ek    = EK.Unmatched ((L.toInt lab1, I.toInt id1), idlabs, L.toInt lab2)
		 in handleSimplify (ERR.consPreError ERR.dummyId labs0 deps0 ek stts0) [] l
		 end
	    else ()

	(* handle the case when a long identifier is not found at all *)
	and handleFreeIdent lid b =
	    if enum andalso not (S.hasEnvVar state)
	    then if b
		 then S.updateStateFo state (I.getLeftId lid)
		 else S.updateStateFr state (I.getLeftId lid)
	    else ()


	(* ====== EQUALITY CONSTRAINT SOLVER ====== *)

	and fsimplify [] l = ()
	  (**)
	  | fsimplify ((E.FUNCTION_TYPE_CONSTRAINT ((T.TYPE_FUNCTION_DEPENDANCY (tf1, labs1, stts1, deps1), tf2), labs, stts, deps)) :: cs') l = fsimplify ((E.FUNCTION_TYPE_CONSTRAINT ((tf1, tf2), L.union labs1 labs, L.union stts1 stts, CD.union deps1 deps)) :: cs') l
	  | fsimplify ((E.TYPE_CONSTRAINT ((T.TYPE_DEPENDANCY  (ty1, labs1, stts1, deps1), ty2), labs, stts, deps)) :: cs') l = fsimplify ((E.TYPE_CONSTRAINT ((ty1, ty2), L.union labs1 labs, L.union stts1 stts, CD.union deps1 deps)) :: cs') l
	  | fsimplify ((E.TYPENAME_CONSTRAINT ((T.TYPENAME_DEPENDANCY  (tn1, labs1, stts1, deps1), tn2), labs, stts, deps)) :: cs') l = fsimplify ((E.TYPENAME_CONSTRAINT ((tn1, tn2), L.union labs1 labs, L.union stts1 stts, CD.union deps1 deps)) :: cs') l
	  | fsimplify ((E.ROW_CONSTRAINT ((T.ROW_DEPENDANCY  (sq1, labs1, stts1, deps1), sq2), labs, stts, deps)) :: cs') l = fsimplify ((E.ROW_CONSTRAINT ((sq1, sq2), L.union labs1 labs, L.union stts1 stts, CD.union deps1 deps)) :: cs') l
	  | fsimplify ((E.FIELD_CONSTRAINT ((T.FIELD_DEPENDANCY  (rt1, labs1, stts1, deps1), rt2), labs, stts, deps)) :: cs') l = fsimplify ((E.FIELD_CONSTRAINT ((rt1, rt2), L.union labs1 labs, L.union stts1 stts, CD.union deps1 deps)) :: cs') l
	  | fsimplify ((E.LABEL_CONSTRAINT ((T.LABEL_DEPENDANCY  (lt1, labs1, stts1, deps1), lt2), labs, stts, deps)) :: cs') l = fsimplify ((E.LABEL_CONSTRAINT ((lt1, lt2), L.union labs1 labs, L.union stts1 stts, CD.union deps1 deps)) :: cs') l
	  (**)
	  | fsimplify ((E.FUNCTION_TYPE_CONSTRAINT ((tf1, T.TYPE_FUNCTION_DEPENDANCY (tf2, labs1, stts1, deps1)), labs, stts, deps)) :: cs') l = fsimplify ((E.FUNCTION_TYPE_CONSTRAINT ((tf1, tf2), L.union labs1 labs, L.union stts1 stts, CD.union deps1 deps)) :: cs') l
	  | fsimplify ((E.TYPE_CONSTRAINT ((ty1, T.TYPE_DEPENDANCY  (ty2, labs1, stts1, deps1)), labs, stts, deps)) :: cs') l = fsimplify ((E.TYPE_CONSTRAINT ((ty1, ty2), L.union labs1 labs, L.union stts1 stts, CD.union deps1 deps)) :: cs') l
	  | fsimplify ((E.TYPENAME_CONSTRAINT ((tn1, T.TYPENAME_DEPENDANCY  (tn2, labs1, stts1, deps1)), labs, stts, deps)) :: cs') l = fsimplify ((E.TYPENAME_CONSTRAINT ((tn1, tn2), L.union labs1 labs, L.union stts1 stts, CD.union deps1 deps)) :: cs') l
	  | fsimplify ((E.ROW_CONSTRAINT ((sq1, T.ROW_DEPENDANCY  (sq2, labs1, stts1, deps1)), labs, stts, deps)) :: cs') l = fsimplify ((E.ROW_CONSTRAINT ((sq1, sq2), L.union labs1 labs, L.union stts1 stts, CD.union deps1 deps)) :: cs') l
	  | fsimplify ((E.FIELD_CONSTRAINT ((rt1, T.FIELD_DEPENDANCY  (rt2, labs1, stts1, deps1)), labs, stts, deps)) :: cs') l = fsimplify ((E.FIELD_CONSTRAINT ((rt1, rt2), L.union labs1 labs, L.union stts1 stts, CD.union deps1 deps)) :: cs') l
	  | fsimplify ((E.LABEL_CONSTRAINT ((lt1, T.LABEL_DEPENDANCY  (lt2, labs1, stts1, deps1)), labs, stts, deps)) :: cs') l = fsimplify ((E.LABEL_CONSTRAINT ((lt1, lt2), L.union labs1 labs, L.union stts1 stts, CD.union deps1 deps)) :: cs') l
	  (**)
	  | fsimplify ((E.ENV_CONSTRAINT ((env1, E.ENVDEP (env2, ls', deps', ids')), ls, deps, ids)) :: cs') l = simplify ((E.ENV_CONSTRAINT ((env1, env2), L.union ls ls', L.union deps deps', CD.union ids ids')) :: cs') l
	  | fsimplify ((E.ENV_CONSTRAINT ((E.ENVDEP (env1, ls', deps', ids'), env2), ls, deps, ids)) :: cs') l = simplify ((E.ENV_CONSTRAINT ((env1, env2), L.union ls ls', L.union deps deps', CD.union ids ids')) :: cs') l
	  (**)
	  | fsimplify ((E.TYPE_CONSTRAINT ((T.TYPE_CONSTRUCTOR (tn1, sq1, l1, eq1), T.TYPE_CONSTRUCTOR (tn2, sq2, l2, eq2)), ls, deps, ids)) :: cs') l =
	    (
	     (* we check that the equality type status of the two type constructors are the same (so they cannot be both EQUALITY_TYPE and NOT_EQUALITY_TYPE)
	      * however, if one of both of them are unknown then that's acceptable
	      *)
	     (* this was code that used to be here when eq was an EQUALITY_TYPE_STATUS. Now it's more than that
	      * because we can have type variables there. *)
	     (* if (eq1 <> eq2 andalso eq1 <> T.UNKNOWN andalso eq2 <> T.UNKNOWN) *)
	     (* then *)
	     (* 	 let *)
	     (* 	     val _ = D.printDebugFeature D.UNIF D.EQUALITY_TYPES (fn _ => "equality type error detected (fsimplify TYPE_CONSTRAINT of TYPE_CONSTRUCTOR and TYPE_CONSTUCTOR)"); *)
	     (* 	     val ek    = EK.EqTypeRequired (L.toInt l) *)
	     (* 	     val err   = ERR.consPreError ERR.dummyId ls ids ek deps *)
	     (* 	 in handleSimplify err cs' l *)
	     (* 	 end *)
	     (* else (); *)
	     if (T.isBaseTy tn1 andalso not (T.isBaseTy tn2))
		orelse
		(T.isBaseTy tn2 andalso not (T.isBaseTy tn1))
	     (* one is a tnv the other one is a -> or a * *)
	     then (* a typenamevar can never be an arrow or record *)
		 if (T.isArrowTy tn1 andalso T.isDecTy tn1 andalso T.isVarTypename tn2)
		    orelse
		    (T.isArrowTy tn2 andalso T.isDecTy tn2 andalso T.isVarTypename tn1)
		    orelse
		    (T.isArrowTy tn1 andalso T.isDecTy tn1 andalso T.isExcTy tn2)
		    orelse
		    (T.isArrowTy tn2 andalso T.isDecTy tn2 andalso T.isExcTy tn1)
		 then let val ek  = EK.ConsArgNApp (L.toInt l1, L.toInt l2)
			  val err = ERR.consPreError ERR.dummyId ls ids ek deps
		      in handleSimplify err cs' l
		      end
		 else if (T.isArrowTy tn1 andalso T.isPatTy tn1 andalso T.isVarTypename tn2)
			 orelse
			 (T.isArrowTy tn2 andalso T.isPatTy tn2 andalso T.isVarTypename tn1)
			 orelse
			 (T.isArrowTy tn1 andalso T.isPatTy tn1 andalso T.isExcTy tn2)
			 orelse
			 (T.isArrowTy tn2 andalso T.isPatTy tn2 andalso T.isExcTy tn1)
		 then let val ek  = EK.ConsNArgApp (L.toInt l1, L.toInt l2)
			  val err = ERR.consPreError ERR.dummyId ls ids ek deps
		      in handleSimplify err cs' l
		      end
		 else let val name1 = T.tntyToTyCon tn1
			  val name2 = T.tntyToTyCon tn2
			  val ek    = EK.TyConsClash ((L.toInt l1, T.typenameToInt name1), (L.toInt l2, T.typenameToInt name2))
			  val err   = ERR.consPreError ERR.dummyId ls ids ek deps
		      (*
		       * (2011-02-17) We don't actually need to try to remove l from the error because it
		       * has to be in the error.  Is that the only one we're sure about?
		       * Are we not sure that the end points have to be in the error as well?
		       *)
		      (*val _ = if T.isVarTypename tn1 orelse T.isVarTypename tn2
				then D.printdebug2 ("foo")
				else ()*)
		      (*val _ = D.printdebug2 (S.printState state)*)
		      in handleSimplify err cs' l
		      end
	     else
		 let
		     val c1 = E.genCstTnAll tn1 tn2 ls deps ids
		     val c2 = E.genCstSqAll sq1 sq2 ls deps ids
		 in
		     fsimplify (c1 :: c2 :: cs') l
		 end)
	  | fsimplify ((E.TYPENAME_CONSTRAINT ((T.NC (tn1, b1, l1), T.NC (tn2, b2, l2)), ls, deps, ids)) :: cs') l =
	    (if T.eqTypename tn1 tn2
	     then fsimplify cs' l
	     else let val ek  = EK.TyConsClash ((L.toInt l1, T.typenameToInt tn1), (L.toInt l2, T.typenameToInt tn2))
		      val err = ERR.consPreError ERR.dummyId ls ids ek deps
		  in handleSimplify err cs' l
		  end)
	  | fsimplify ((E.ROW_CONSTRAINT ((T.ROW_C (rtl1, b1, l1), T.ROW_C (rtl2, b2, l2)), ls, deps, ids)) :: cs') l =
	    let val n1 = length rtl1
		val n2 = length rtl2
	    (*val _  = D.printdebug2 ("-("  ^ (O.printelt l1)   ^
				      ","   ^ (Int.toString n1) ^
				      ","   ^ (T.printflex b1)  ^
				      ")-(" ^ (O.printelt l2)   ^
				      ","   ^ (Int.toString n2) ^
				      ","   ^ (T.printflex b2)  ^
				      ")-\n")*)
	    in if n1 = n2 orelse (T.isflex b1 andalso n1 < n2) orelse (T.isflex b2 andalso n2 < n1)
               then let val ls1 = [T.getflex b1] handle unflex => []
			val ls2 = [T.getflex b2] handle unflex => []
			val ls' = L.unions [L.ord ls1, L.ord ls2, ls]
			(* we want to use ls' instead of ls below,
			 * but for some strange reason it seems to slow down the code a lot
			 * - that's normal that's because there is no constraint with ls1 or ls2 *)
			(* anyway I should use something else than these srec... *)
			val rtl1' = map (fn field => T.FIELD_DEPENDANCY (field, ls, deps, ids)) rtl1
			val rtl2' = map (fn field => T.FIELD_DEPENDANCY (field, ls, deps, ids)) rtl2
			val sr = ((rtl1', b1, []), (rtl2', b2, []))
			val (cs'', err) = S.updateRecOne state  sr
		    (* don't we get unwanted occurrences of srec by doing that? *)
		    in case err of
			   [] => fsimplify (cs'' @ cs') l
			 | [((ek1, ek2, ek3, ek4), ell, edeps, eids)] =>
			   handleSimplify (ERR.consPreError ERR.dummyId
							    ell
							    eids
							    (EK.LabTyClash (ek1, ek2, ek3, ek4))
							    edeps)
					  cs'
					  l
			 | _ => raise EH.DeadBranch ""
		    end
               else let val ek  = EK.ArityClash ((L.toInt l1, n1), (L.toInt l2, n2))
			val err = ERR.consPreError ERR.dummyId ls ids ek deps
		    in handleSimplify err cs' l
		    end
	    end
	  | fsimplify ((E.TYPE_CONSTRAINT ((ty1 as T.TYPE_VAR (tv1, b1, p1, eq1), ty2 as T.TYPE_VAR (tv2, b2, p2, eq2)), ls, deps, ids)) :: cs') l =
	    let
		fun continue () =
		    case S.getValStateTv state tv1 of
			NONE =>
			let val t = T.TYPE_VAR (tv2, if Option.isSome b2 then b2 else b1, p2, eq2)
			    (*val _ = BI.updateMono state tv1 t ls deps ids*)
			    val _ = if occurs (CT (tv1, t)) [T (tv2, ls, deps, ids)] 0 l
				    then S.updateStateTv state tv1 (T.TYPE_DEPENDANCY (t, ls, deps, ids))
				    else ()
			in fsimplify cs' l
			end
		      | SOME ty =>
			let val bop = if Option.isSome b2 then b2 else b1
			    val t   = T.TYPE_VAR (tv2, bop, p2, eq2)
			    val c   = E.genCstTyAll ty t ls deps ids
			in fsimplify (c :: cs') l
	    		end
	    (*val _ = D.printdebug2 (T.printty ty1 ^ "\n" ^ T.printty ty2)*)
	    in if T.eqTypeVar tv1 tv2
	       then fsimplify cs' l
	       else case (b1, b2) of
			(SOME _, NONE) => fsimplify ((E.TYPE_CONSTRAINT ((T.TYPE_VAR (tv2, b2, p2, eq2), T.TYPE_VAR (tv1, b1, p1, eq1)), ls, deps, ids)) :: cs') l
		      | (SOME (id1, lab1), SOME (id2, lab2)) =>
			if I.eqId id1 id2
			then continue ()
			else let val ek  = EK.TyConsClash ((L.toInt lab1, T.typenameToInt (T.DUMMYTYPENAME)), (L.toInt lab2, T.typenameToInt (T.DUMMYTYPENAME)))
				 val err = ERR.consPreError ERR.dummyId ls ids ek deps
			     in handleSimplify err cs' l
			     end
		      | _ => continue ()
	    end
	  | fsimplify ((E.TYPE_CONSTRAINT ((T.EXPLICIT_TYPE_VAR (n1, tv1, l1, eqtv1), T.EXPLICIT_TYPE_VAR (n2, tv2, l2, eqtv2)), ls, deps, ids)) :: cs') l =
	    (D.printDebugFeature D.UNIF D.CONSTRAINT_SOLVING (fn _ => "solving type constraint of two EXPLICIT_TYPE_VAR values");
	     if I.eqId n1 n2
	     then
		 if eqtv1 <> eqtv2 andalso eqtv1 <> T.UNKNOWN andalso eqtv2 <> T.UNKNOWN
		 then
		     let
			 val _ = D.printDebugFeature D.UNIF D.EQUALITY_TYPES (fn _ => "equality type error detected (eqtv1="
				 ^(T.printEqualityTypeStatus eqtv1)^", eqtv2="^(T.printEqualityTypeStatus eqtv2)^")");
			 (* jpirie: shouldn't we include l2 in the error here too *)
			 val ek    = EK.EqTypeRequired (L.toInt l1)
			 (* jpirie: I've put l2 here so both l1 and l2 are used. Should we use l2 here? Hmm! *)
			 val err   = ERR.consPreError ERR.dummyId ls ids ek deps
		     in handleSimplify err cs' l
		     end
		 else
		     fsimplify cs' l
	     else let val ek  = EK.TyConsClash ((L.toInt l1, T.typenameToInt (T.DUMMYTYPENAME)), (L.toInt l2, T.typenameToInt (T.DUMMYTYPENAME)))
		      val err = ERR.consPreError ERR.dummyId ls ids ek deps
		  in handleSimplify err cs' l
		  end)
	  | fsimplify ((E.TYPE_CONSTRAINT ((T.EXPLICIT_TYPE_VAR (n, tv, l1, _), T.TYPE_CONSTRUCTOR (tn, _, l2, _)), ls, deps, ids)) :: cs') l =
	    let val ek  = EK.TyConsClash ((L.toInt l1, T.typenameToInt (T.DUMMYTYPENAME)), (L.toInt l2, T.typenameToInt (T.tntyToTyCon tn)))
		val err = ERR.consPreError ERR.dummyId ls ids ek deps
	    in handleSimplify err cs' l
	    end
	  | fsimplify ((E.TYPENAME_CONSTRAINT ((T.TYPENAME_VAR tnv1, T.TYPENAME_VAR tnv2), ls, deps, ids)) :: cs') l =
	    if T.eqTypenameVar tnv1 tnv2
	    then fsimplify cs' l
	    else (case S.getValStateTn state tnv1 of
                      NONE =>
                      let val _ = case S.getValStateTn state tnv2 of
				      NONE => S.updateStateTn state tnv1 (T.TYPENAME_DEPENDANCY (T.TYPENAME_VAR tnv2, ls, deps, ids))
				    | _ => ()
                      in fsimplify cs' l
                      end
		    | SOME tn =>
                      let val c = E.genCstTnAll tn (T.TYPENAME_VAR tnv2) ls deps ids
                      in fsimplify (c :: cs') l
                      end)
	  | fsimplify ((E.ROW_CONSTRAINT ((T.ROW_VAR sqv1, sq2 as (T.ROW_VAR sqv2)), ls, deps, ids)) :: cs') l =
	    if T.eqRowVar sqv1 sqv2
	    then fsimplify cs' l
	    else (case S.getValStateSq state sqv1 of
                      NONE =>
                      let val _ = if occurs (CS (sqv1, sq2)) [S (sqv2, ls, deps, ids)] 0 l
				  then S.updateStateSq state sqv1 (T.ROW_DEPENDANCY (T.ROW_VAR sqv2, ls, deps, ids))
				  else ()
                      in fsimplify cs' l
                      end
		    | SOME sq =>
                      let val c = E.genCstSqAll sq (T.ROW_VAR sqv2) ls deps ids
                      in fsimplify (c :: cs') l
                      end)
	  | fsimplify ((E.TYPE_CONSTRAINT ((tyv as T.TYPE_VAR (tv, b, p, eq), ty), ls, deps, ids)) :: cs') l =
	    let
		val _ = if (not (!analysingBasis))
			then D.printDebugFeature D.UNIF D.EQUALITY_TYPES (fn _ => "solving the case of TYPE_VAR, which is this: "^
									  (#purple D.colors)^(T.printty tyv)^(D.textReset)^
									  " and something else, namely this: "
									  ^(#cyan D.colors)^(T.printty ty))
			else ()

		fun checkForEqualityErrors ty =
		    let
			(* strip all of the equality type statuses out of ty and put them in a list *)
			val tyEqStatus = T.stripEqualityStatus ty L.empty

			(* look through the list and see if any of them contradict the value of eq *)
			val findEqClash = NONE (*List.find (fn x => (if x = eq orelse x = T.UNKNOWN then false else true)) tyEqStatus*)
		    in
			case findEqClash of
			    (* there are no contradicting values, no equality type error here *)
			    NONE => ()

			  (* there is a contradicting value, there's an equality type error here *)
			  | SOME x =>
	  		    let
				(* this should really be the label that comes from the Ty.TYPE_POLY tuple right? *)
				val _ = D.printDebugFeature D.UNIF D.EQUALITY_TYPES (fn _ => (#red D.colors)
										     ^"equality type error detected (the something else turned out to be TYPE_POLY). eq is "
										     ^T.printEqualityTypeStatus eq ^ " and contradicting value was found in "^(T.printty ty));
				  (* we should take the label by looking at x really, using l is not right here *)
				val ek    = EK.EqTypeRequired (L.toInt l)
				val err   = ERR.consPreError ERR.dummyId ls ids ek deps
	  		    in handleSimplify err cs' l
	  		    end
		    end

		val _ = if eq = T.UNKNOWN
			(* if the equality type status of eq hasn't been determined yet, then there can't possibly be an equality type error *)
			then ()
			(* otherwise check ty to see if it has any contradicting equality type status values *)
			else (D.printDebugFeature D.UNIF D.EQUALITY_TYPES (fn _ => "looking for equality type errors in fsimplify TYPE_CONSTRAINT of TYPE_VAR (equality constraint "^(T.printEqualityTypeStatus eq)^") and something else...");
			      (* (2012-07-09-12:20) jpirie: I've taken this check out, this is the wrong way to do it
			       * leaving it in for now because I might come back in a bit and decide I want bits of this
			       * delete if you're see this in year >= 2013. *)
			      (* checkForEqualityErrors ty; *)
			      D.printDebugFeature D.UNIF D.EQUALITY_TYPES (fn _ => "done"))

		fun reportGenError () =
		    if Option.isSome b       (* Type variable comes from an explicit type variable        *)
		       andalso isSigVsStr () (* We're dealing with constraints on signature vs. structure *)
		       andalso T.isTyC ty
		    then (* we're dealing with a too general signature's structure *)
			let val l1  = #2 (Option.valOf b)           handle Option => raise EH.DeadBranch ""
			    val l2  = Option.valOf (T.getTyLab  ty) handle Option => raise EH.DeadBranch ""
			    val tn  = Option.valOf (T.getTypenameType ty) handle Option => raise EH.DeadBranch ""
			    val fek = if isSigVsStrTyp () then EK.TyFunClash else EK.NotGenClash
			    val ek  = fek ((L.toInt l1, T.typeVarToInt tv), (L.toInt l2, T.typenameToInt (T.tntyToTyCon tn)))
			    val err = ERR.consPreError ERR.dummyId ls ids ek deps
			(* We should raise another error here:
			 * something like, signature too general. *)
			in handleSimplify err cs' l
			end
		    else ()
		fun updateFlex (T.TYPE_VAR (tv, NONE, p, eq)) flex = T.TYPE_VAR (tv, flex, p, eq)
		  | updateFlex (T.TYPE_DEPENDANCY (ty, labs, stts, deps)) flex =
		    collapseTy (updateFlex ty flex) labs stts deps
		  | updateFlex ty _ = ty
		val ty = buildDirectOr ty state
	    in case S.getValStateTv state tv of
		   NONE =>
		   let (* here we have to update statege (we know that ty is not a variable) *)
		       (*val _ = BI.updateMono state tv ty ls deps ids*)
		       val _ = reportGenError ()
		       val (rho, n) = decomptyty ty ls deps ids
		       val _ = if occurs (CT (tv, ty)) rho n l
			       then S.updateStateTv state tv (collapseTy ty ls deps ids)
			       else ()
		   in fsimplify cs' l
		   end
		 | SOME ty' =>
		   let val c = E.genCstTyAll (updateFlex ty' b) ty ls deps ids
		   in fsimplify (c :: cs') l
		   end
	    end
	  | fsimplify ((E.TYPENAME_CONSTRAINT ((T.TYPENAME_VAR tnv1, tnty2), ls, deps, ids)) :: cs') l =
	    (case S.getValStateTn state tnv1 of
		 NONE =>
		 let val _ = S.updateStateTn state tnv1 (T.TYPENAME_DEPENDANCY (tnty2, ls, deps, ids))
		 in fsimplify cs' l end
	       | SOME tnty =>
		 let val c = E.genCstTnAll tnty tnty2 ls deps ids
		 in fsimplify (c :: cs') l
		 end)
	  | fsimplify ((E.ROW_CONSTRAINT ((T.ROW_VAR sqv1, sq2), ls, deps, ids)) :: cs') l =
	    (case S.getValStateSq state sqv1 of
		 NONE =>
		 let val (rho, n) = decomptysq sq2 ls deps ids
		     val _ = if occurs (CS (sqv1, sq2)) rho n l
			     then S.updateStateSq state sqv1 (collapseSq sq2 ls deps ids)
			     else ()
		 in fsimplify cs' l
		 end
               | SOME sq =>
		 let val c = E.genCstSqAll sq sq2 ls deps ids
		 in fsimplify (c :: cs') l
		 end)
	  | fsimplify ((E.FIELD_CONSTRAINT ((T.FIELD_VAR rv, rt as T.FC _), ls, deps, ids)) :: cs') l =
	    (case S.getValStateRt state rv of
		 NONE =>
		 let val (rho, n) = decomptyfield rt ls deps ids
		     val _ = if occurs (CR (rv, rt)) rho n l
			     then S.updateStateRt state rv (collapseRt rt ls deps ids)
			     else ()
		     val (cs'', err) = S.updateRec state
		 in case err of
			[] => fsimplify (cs'' @ cs') l
		      | [((ek1, ek2, ek3, ek4), ell, edeps, eids)] => (* Can that happen? *)
			handleSimplify (ERR.consPreError ERR.dummyId
							 ell
							 eids
							 (EK.LabTyClash (ek1, ek2, ek3, ek4))
							 edeps)
				       cs'
				       l
		      | _  => raise EH.DeadBranch ""
		 end
               | SOME rt' => raise EH.DeadBranch "")
	  (* we update and store *)
	  (* if it's 2 variables we raise deadbranch, same if it's 2 cons *)
	  | fsimplify ((E.LABEL_CONSTRAINT ((T.LABEL_VAR lv, lt as T.LC _), ls, deps, ids)) :: cs') l =
	    (case S.getValStateLt state lv of
		 NONE =>
		 let val _ = S.updateStateLt state lv (T.LABEL_DEPENDANCY (lt, ls, deps, ids))
		     val (cs'', err) = S.updateRec state
		 in case err of
			[] => fsimplify (cs'' @ cs') l
		      | [((ek1, ek2, ek3, ek4), ell, edeps, eids)] => (* Can that happen? *)
			handleSimplify (ERR.consPreError ERR.dummyId
							 ell
							 eids
							 (EK.LabTyClash (ek1, ek2, ek3, ek4))
							 edeps)
				       cs'
				       l
		      | _  => raise EH.DeadBranch ""
		 end
               | SOME lt' => raise EH.DeadBranch "")
	  | fsimplify ((E.TYPE_CONSTRAINT ((t1 as T.APPLICATION (T.TYPE_FUNCTION_VAR tfv, seqty, lab), t2), ls, deps, ids)) :: cs') l =
	    (case S.getValStateTf state tfv of
                 NONE => fsimplify cs' l
	       | SOME tf =>
                 let val c = E.genCstTyAll (T.APPLICATION (tf, seqty, lab)) t2 ls deps ids
                 in fsimplify (c :: cs') l
                 end)
	  | fsimplify ((E.TYPE_CONSTRAINT ((t1 as T.APPLICATION (T.TFC (seqty1, ty1, lab1), seqty2, lab2), ty2), ls, deps, ids)) :: cs') l =
	    let
		val labs = L.cons lab1 (L.cons lab2 ls)
		val c1 = E.genCstSqAll seqty1 seqty2 labs deps ids
		val c2 = E.genCstTyAll ty1 ty2 labs deps ids
	    in fsimplify (c1 :: c2 :: cs') l
	    end
	  | fsimplify ((E.TYPE_CONSTRAINT ((t1 as T.APPLICATION (T.TYPE_FUNCTION_DEPENDANCY (tf2, labs2, stts2, deps2), seqty2, lab2), ty2), labs, stts, deps)) :: cs') l =
	    fsimplify ((E.TYPE_CONSTRAINT ((T.APPLICATION (tf2, seqty2, lab2), ty2), L.union labs labs2, L.union stts stts2, CD.union deps deps2)) :: cs') l
	  | fsimplify ((E.TYPE_CONSTRAINT ((tc as (T.TYPE_CONSTRUCTOR (tnty, sq, lab1, eq1)), to as T.TYPE_POLY (sq', idor, poly, orKind, lab2, eq2)), ls, deps, ids)) :: cs') l =
	    (* Build the sq' in case it's a variable. *)
	    let fun checkTn tnty labs stts deps seq =
		    case tnty of
			T.TYPENAME_VAR _ => fsimplify cs' l
		      | T.NC (tn, _, l') =>
			let fun getErr () =
				let val tnerr = (L.toInt l', T.typenameToInt tn)
				    val (tnerrs, labs, stts, deps) = gatherAllTnSq seq
				    val kind =
					case orKind of
					    T.VALUE (id, lab) => EK.Overload ((L.toInt lab, I.toInt id), tnerr, tnerrs)
					  | T.CONSTANT (st, id, lab) => EK.OverloadCst ((L.toInt lab, I.toInt id, st), tnerr, tnerrs)
				in (kind, labs, stts, deps)
				end
			(*val (seq', _, _, _) = T.stripDepsSq seq*)
			(* we should check that tl is complete! *)
			in case findInOrSq tn [] seq of
			       ([], false, _) =>
			       let val (kind, labs2, stts2, deps2) = getErr ()
				   val labs' = L.union  labs labs2
				   val stts' = L.union  stts stts2
				   val deps' = CD.union deps deps2
				   val err   = ERR.consPreError ERR.dummyId labs' deps' kind stts'
			       in handleSimplify err cs' l
			       end
			     | ([], true, _) => fsimplify cs' l
			     | (((t, path) :: _), true, _) =>
			       let val _ = S.updateStateOr state idor ([path], labs, stts, deps)
				   val c = E.genCstTyAll tc t labs stts deps
			       in fsimplify (c :: cs') l end
			     | _ => raise EH.DeadBranch ""
			end
		      | T.TYPENAME_DEPENDANCY (tn1, labs1, stts1, deps1) => checkTn tn1
								   (L.union labs labs1)
								   (L.union stts stts1)
								   (CD.union deps deps1)
								   seq

	    in
		(
		 (* check for equality type errors
		  * if both the type constructor and the type poly have their equality type status
		  * set (that is, they are not UNKNOWN), then they should be the same
		  *)
		 (* this was code that used to be here when eq was an EQUALITY_TYPE_STATUS. Now it's more than that
		  * because we can have type variables there. *)
		 (* if (eq1 <> eq2 andalso eq1 <> T.UNKNOWN andalso eq2 <> T.UNKNOWN) *)
		 (* then *)
		 (*     let *)
		 (* 	 val _ = D.printDebugFeature D.UNIF D.EQUALITY_TYPES (fn _ => "equality type error detected (fsimplify with TYPE_CONSTRAINT of a TYPE_CONSTRUCTOR and TYPE_POLY)") *)
		 (* 	 val ek    = EK.EqTypeRequired (L.toInt l) *)
		 (* 	 val err   = ERR.consPreError ERR.dummyId ls ids ek deps *)
		 (*     in handleSimplify err cs' l *)
		 (*     end *)
		 (* else (); *)

		case S.getValStateOr state idor of
		   NONE => checkTn tnty ls deps ids sq'
		 | SOME ([], _, _, _) => raise EH.DeadBranch ""
		 | SOME ([path], ls', deps', ids') =>
		   (case gotoInOrSq path sq' of
			NONE => fsimplify cs' l
		      | SOME t =>
			let val labs0 = L.union ls ls'
			    val stts0 = L.union deps deps'
			    val deps0 = CD.union ids ids'
			    val c = E.genCstTyAll tc t labs0 stts0 deps0
			in fsimplify (c :: cs') l
			end)
		 | SOME (paths, ls', deps', ids') =>
		   let val labs0 = L.union ls ls'
		       val stts0 = L.union deps deps'
		       val deps0 = CD.union ids ids'
		   in checkTn tnty labs0 stts0 deps0 (selectPaths paths sq')
		   end)
	    end
	  | fsimplify ((E.TYPE_CONSTRAINT ((T.EXPLICIT_TYPE_VAR (n, tv, l1, eqtv1), T.TYPE_POLY (sq, _, poly, orKind, l2, eqtv2)), ls, deps, ids)) :: cs') l =
	    let fun getErr () =
		    let val tnerr  = (L.toInt l1, T.typenameToInt (T.DUMMYTYPENAME))
			val (tnerrs, labs, stts, deps) = gatherAllTnSq sq
			val kind =
			    case orKind of
				T.VALUE (id, lab) => EK.Overload ((L.toInt lab, I.toInt id), tnerr, tnerrs)
			      | T.CONSTANT (st, id, lab) => EK.OverloadCst ((L.toInt lab, I.toInt id, st), tnerr, tnerrs)
		    in (kind, labs, stts, deps)
		    end
	    in if isAllSq sq
	       then let val (kind, labs1, stts1, deps1) = getErr ()
			val labs' = L.union  labs1 ls
			val stts' = L.union  stts1 deps
			val deps' = CD.union deps1 ids
		    in handleSimplify (ERR.consPreError ERR.dummyId labs' deps' kind stts') cs' l
		    end
	       else
		   (* if eqtv1 <> eqtv2 andalso eqtv1 <> T.UNKNOWN andalso eqtv2 <> T.UNKNOWN *)
		   (* then *)
		   (*     let *)
		   (* 	   val _ = D.printDebugFeature D.UNIF D.EQUALITY_TYPES (fn _ => "equality type error detected (eqtv1=" *)
		   (* 								^(T.printEqualityTypeStatus eqtv1)^", eqtv2="^(T.printEqualityTypeStatus eqtv2)^")"); *)
		   (* 	   (* jpirie: shouldn't we include l2 in the error here too *) *)
		   (* 	   val ek    = EK.EqTypeRequired (L.toInt l1) *)
		   (* 	   (* jpirie: I've put l2 here so both l1 and l2 are used. Should we use l2 here? Hmm! *) *)
		   (* 	   val err   = ERR.consPreError ERR.dummyId ls ids ek deps *)
		   (*     in handleSimplify err cs' l *)
		   (*     end *)
		   (* else *)
		   fsimplify cs' l
	    end
	  | fsimplify ((E.FIELD_CONSTRAINT _) :: cs') l = raise EH.DeadBranch ""
	  | fsimplify ((E.LABEL_CONSTRAINT _) :: cs') l = raise EH.DeadBranch ""
	  | fsimplify ((E.TYPE_CONSTRAINT ((ty1 as T.TYPE_POLY (sq1, i1, poly1, orKind1, lab1, eq1),
					    ty2 as T.TYPE_POLY (sq2, i2, poly2, orKind2, lab2, eq2)),
					   ls, deps, ids)) :: cs') l =
	    let val _ = D.printDebugFeature D.UNIF D.CONSTRAINT_SOLVING (fn _ => "solving type constraint of two TYPE_POLY constructors (lab1="^(Int.toString (L.toInt lab1))^", lab2="^(Int.toString (L.toInt lab2))^")...")
		val _ = D.printDebugFeature D.UNIF D.EQUALITY_TYPES (fn _ => "solving type constraint of two TYPE_POLY constructors (eq1="^(T.printEqualityType eq1)^", eq2="^(T.printEqualityType eq2)^")...")
		fun match seq1 seq2 ls deps ids =
		    case tryToMatchOrs seq1 seq2 of
			(_, _, false) => (* tryToMatchOrs can be simplified as it is just a checking *)
			(case (orKind1, orKind2) of
			     (T.CONSTANT (st1, id1, lab1), T.CONSTANT (st2, id2, lab2)) =>
			     let val (tnerrs1, labs1, stts1, deps1) = gatherAllTnSq seq1
				 val (tnerrs2, labs2, stts2, deps2) = gatherAllTnSq seq2
				 val labs = L.union (L.union labs1 labs2) ls
				 val stts = L.union (L.union stts1 stts2) deps
				 val deps = CD.union (CD.union deps1 deps2) ids
				 val ek = EK.OverloadClash ((L.toInt lab1, I.toInt id1, st1),
							    tnerrs1,
							    (L.toInt lab2, I.toInt id2, st2),
							    tnerrs2)
			     in handleSimplify (ERR.consPreError ERR.dummyId labs deps ek stts) cs' l
			     end
			   | (T.CONSTANT (st1, id1, lab1), T.VALUE (id2, lab2)) =>
			     let val (tnerrs1, labs1, stts1, deps1) = gatherAllTnSq seq1
				 val (tnerrs2, labs2, stts2, deps2) = gatherAllTnSq seq2
				 val labs = L.union (L.union labs1 labs2) ls
				 val stts = L.union (L.union stts1 stts2) deps
				 val deps = CD.union (CD.union deps1 deps2) ids
				 val ek = EK.OverloadIdCst ((L.toInt lab2, I.toInt id2),
							    tnerrs2,
							    (L.toInt lab1, I.toInt id1, st1),
							    tnerrs1)
			     in handleSimplify (ERR.consPreError ERR.dummyId labs deps ek stts) cs' l
			     end
			   | (T.VALUE (id1, lab1), T.CONSTANT (st2, id2, lab2)) =>
			     let val (tnerrs1, labs1, stts1, deps1) = gatherAllTnSq seq1
				 val (tnerrs2, labs2, stts2, deps2) = gatherAllTnSq seq2
				 val labs = L.union (L.union labs1 labs2) ls
				 val stts = L.union (L.union stts1 stts2) deps
				 val deps = CD.union (CD.union deps1 deps2) ids
				 val ek = EK.OverloadIdCst ((L.toInt lab1, I.toInt id1),
							    tnerrs1,
							    (L.toInt lab2, I.toInt id2, st2),
							    tnerrs2)
			     in handleSimplify (ERR.consPreError ERR.dummyId labs deps ek stts) cs' l
			     end
			   | _ => fsimplify cs' l)
		      | ((typaths1, f1), (typaths2, f2), true) =>
			let fun upd id typaths = S.updateStateOr state id (map (fn (_, y) => y) typaths, ls, deps, ids)
			    val _ = if f2  (* f2 means that we've found a concrete match of seq1 in seq2 *)
				       andalso isFullOr seq2 (* means that no field of seq2 is a var *)
				       andalso (not (T.isPoly poly1))
				       (* why do we insist that poly1 is not poly?
					* It seems to be why we don't get an error
					* for, e.g., '1 + 1.1'.
					* What could go wrong with POLY marker?
					* Is the POLY marker only used when 'over' is turned ON? *)
				       andalso not (List.null typaths1)
				    then upd i1 typaths1
				    else ()
			    (* Why don't we have the same set of conditions for the second row? *)
			    val _ = if f1
				       andalso (not (T.isPoly poly2))
				       andalso not (List.null typaths2)
				       andalso List.length typaths1 = List.length typaths2
				    then upd i2 typaths2
				    else ()
			in fsimplify cs' l
			end
	    in
		((* if (eq1 = eq2 orelse eq1 = T.UNKNOWN orelse eq2 = T.UNKNOWN) *)
		 (* then () *)
		 (* else *)
	  	 (*     (let *)
		 (* 	  val _ = D.printDebugFeature D.UNIF D.EQUALITY_TYPES (fn _ => (#red D.colors)^"equality type error detected!") *)
	  	 (* 	  (* need to figure out how to get the labels to the user *) *)
 	  	 (* 	  val ek    = EK.EqTypeRequired (L.toInt lab1) *)
	  	 (* 	  val err   = ERR.consPreError ERR.dummyId ls ids ek deps *)
	  	 (*      in handleSimplify err cs' l *)
	  	 (*      end); *)
		 case (S.getValStateOr state i1, S.getValStateOr state i2) of
		   (SOME ([path1], ls1, deps1, ids1), SOME ([path2], ls2, deps2, ids2)) =>
		   (case (gotoInOrSq path1 sq1, gotoInOrSq path2 sq2) of
			(SOME t1, SOME t2) =>
			let (* (2010-02-05) This is never happening, is it? *)
			    val c = E.genCstTyAll t1
						  t2
						  (L.unions [ls,   ls1,   ls2])
						  (L.unions [deps, deps1, deps2])
						  (CD.unions [ids,  ids1,  ids2])
			in fsimplify (c :: cs') l
			end
		      | _ => fsimplify cs' l)
		 | (SOME ([path1], labs1, stts1, deps1), NONE) =>
		   (case gotoInOrSq path1 sq1 of
			SOME t1 =>
			let val c = E.genCstTyAll t1 ty2 (L.union ls labs1) (L.union deps stts1) (CD.union ids  deps1)
			in fsimplify (c :: cs') l
			end
		      | _ => fsimplify cs' l)
		 | (NONE, SOME ([path2], labs2, stts2, deps2)) =>
		   (case gotoInOrSq path2 sq2 of
			SOME t2 =>
			let val c = E.genCstTyAll ty1 t2 (L.union ls labs2) (L.union deps stts2) (CD.union ids  deps2)
			in fsimplify (c :: cs') l
			end
		      | _ => fsimplify cs' l)
		 | (_, SOME ([], _, _, _)) => raise EH.DeadBranch ""
		 | (SOME ([], _, _, _), _) => raise EH.DeadBranch ""
		 | (NONE, SOME (paths2, ls2, deps2, ids2)) =>
		   match sq1
			 (selectPaths paths2 sq2)
			 (L.union  ls   ls2)
			 (L.union  deps deps2)
			 (CD.union ids  ids2)
		 | (SOME (paths1, ls1, deps1, ids1), NONE) =>
		   match (selectPaths paths1 sq1)
			 sq2
			 (L.union  ls   ls1)
			 (L.union  deps deps1)
			 (CD.union ids  ids1)
		 | (SOME (paths1, ls1, deps1, ids1), SOME (paths2, ls2, deps2, ids2)) =>
		   match (selectPaths paths1 sq1)
			 (selectPaths paths2 sq2)
			 (L.unions  [ls,   ls1,   ls2])
			 (L.unions  [deps, deps1, deps2])
			 (CD.unions [ids,  ids1,  ids2])
		 | (NONE, NONE) =>
		   (* The 2 rows are not set yet to a path (a path set),
		    * so we don't know yet whether they match on at least a type.
		    * We've got to check that with 'match'. *)
		   match sq1 sq2 ls deps ids)
	    end
	  (* TODO: check that *)
	  | fsimplify ((E.ENV_CONSTRAINT ((E.ENV_VAR (ev1, lab1), env2 as E.ENV_VAR (ev2, lab2)), ls, deps, ids)) :: cs') l =
	    if E.eqEnvVar ev1 ev2
	    then fsimplify cs' l
	    else (case S.getValStateEv state ev1 of
		      NONE =>
		      let val _ = S.updateStateEv state ev1 (E.pushExtEnv (E.ENV_VAR (ev2, lab2)) ls deps ids)
		      in fsimplify cs' l
		      end
		    | SOME env =>
                      let val c = E.genCstEvAll env (E.ENV_VAR (ev2, lab2)) ls deps ids
                      in fsimplify (c :: cs') l
                      end)
	  | fsimplify ((E.ENV_CONSTRAINT ((E.ENV_VAR (ev, lab), env), ls, deps, ids)) :: cs') l =
	    (case S.getValStateEv state ev of
		 NONE =>
		 let val env' = solveenv env false (*Why do we need to solve here?*)
		     val _    = S.updateStateEv state ev (E.pushExtEnv env' ls deps ids)
		 in fsimplify cs' l
		 end
               | SOME env' =>
		 let val cs''  = E.genCstEvAll env' env ls deps ids
		 in fsimplify (cs'' :: cs') l
		 end)
	  | fsimplify ((E.ENV_CONSTRAINT ((env1 as E.ENV_CONS _, env2 as E.ENV_CONS _), ls, deps, ids)) :: cs') l =
	    let val cs = compareenv env1 env2 filters ls deps ids
	    (* do something for idV and idS *)
	    in fsimplify (cs @ cs') l
	    end
	  (*(2010-04-16)TODO:*)
	  | fsimplify ((E.ENV_CONSTRAINT ((E.ROW_ENV x, E.ENV_CONS y), ls, deps, ids)) :: cs') l = raise EH.TODO "unhandled case in the constraint solver, raised in the 'f_simplify' function of Unification.sml"
	  | fsimplify ((E.ENV_CONSTRAINT ((E.ROW_ENV x, E.ROW_ENV y), ls, deps, ids)) :: cs') l = raise EH.TODO "unhandled case in the constraint solver, raised in the 'f_simplify' function of Unification.sml"
	  | fsimplify ((E.ENV_CONSTRAINT ((E.ROW_ENV x, E.ENVOPN y), ls, deps, ids)) :: cs') l = raise EH.TODO "unhandled case in the constraint solver, raised in the 'f_simplify' function of Unification.sml"
	  | fsimplify ((E.ENV_CONSTRAINT ((E.ROW_ENV x, E.CONSTRAINT_ENV y), ls, deps, ids)) :: cs') l = raise EH.TODO "unhandled case in the constraint solver, raised in the 'f_simplify' function of Unification.sml"
	  (**)
	  | fsimplify ((E.ENV_CONSTRAINT ((E.ENVOPN x, E.ENV_CONS y), ls, deps, ids)) :: cs') l = raise EH.TODO "unhandled case in the constraint solver, raised in the 'f_simplify' function of Unification.sml"
	  | fsimplify ((E.ENV_CONSTRAINT ((E.ENVOPN x, E.ROW_ENV y), ls, deps, ids)) :: cs') l = raise EH.TODO "unhandled case in the constraint solver, raised in the 'f_simplify' function of Unification.sml"
	  | fsimplify ((E.ENV_CONSTRAINT ((E.ENVOPN x, E.ENVOPN y), ls, deps, ids)) :: cs') l = raise EH.TODO "unhandled case in the constraint solver, raised in the 'f_simplify' function of Unification.sml"
	  | fsimplify ((E.ENV_CONSTRAINT ((E.ENVOPN x, E.ENV_VAR y), ls, deps, ids)) :: cs') l = raise EH.TODO "unhandled case in the constraint solver, raised in the 'f_simplify' function of Unification.sml"
	  | fsimplify ((E.ENV_CONSTRAINT ((E.ENVOPN x, E.CONSTRAINT_ENV y), ls, deps, ids)) :: cs') l = raise EH.TODO "unhandled case in the constraint solver, raised in the 'f_simplify' function of Unification.sml"
	  (**)
	  | fsimplify ((E.ENV_CONSTRAINT ((E.CONSTRAINT_ENV x, E.ENV_CONS y), ls, deps, ids)) :: cs') l = raise EH.TODO "unhandled case in the constraint solver, raised in the 'f_simplify' function of Unification.sml"
	  | fsimplify ((E.ENV_CONSTRAINT ((E.CONSTRAINT_ENV x, E.ROW_ENV y), ls, deps, ids)) :: cs') l = raise EH.TODO "unhandled case in the constraint solver, raised in the 'f_simplify' function of Unification.sml"
	  | fsimplify ((E.ENV_CONSTRAINT ((E.CONSTRAINT_ENV x, E.ENVOPN y), ls, deps, ids)) :: cs') l = raise EH.TODO "unhandled case in the constraint solver, raised in the 'f_simplify' function of Unification.sml"
	  | fsimplify ((E.ENV_CONSTRAINT ((E.CONSTRAINT_ENV x, E.ENV_VAR y), ls, deps, ids)) :: cs') l = raise EH.TODO "unhandled case in the constraint solver, raised in the 'f_simplify' function of Unification.sml"
	  | fsimplify ((E.ENV_CONSTRAINT ((E.CONSTRAINT_ENV x, E.CONSTRAINT_ENV y), ls, deps, ids)) :: cs') l = raise EH.TODO "unhandled case in the constraint solver, raised in the 'f_simplify' function of Unification.sml"
	  (**)
	  | fsimplify ((E.ENV_CONSTRAINT ((E.ENV_CONS x, E.ROW_ENV y), ls, deps, ids)) :: cs') l = raise EH.TODO "unhandled case in the constraint solver, raised in the 'f_simplify' function of Unification.sml"
	  | fsimplify ((E.ENV_CONSTRAINT ((E.ENV_CONS x, E.ENVOPN y), ls, deps, ids)) :: cs') l = raise EH.TODO "unhandled case in the constraint solver, raised in the 'f_simplify' function of Unification.sml"
	  | fsimplify ((E.ENV_CONSTRAINT ((E.ENV_CONS x, E.CONSTRAINT_ENV y), ls, deps, ids)) :: cs') l = raise EH.TODO "unhandled case in the constraint solver, raised in the 'f_simplify' function of Unification.sml"
	  (**)
	  | fsimplify ((E.FUNCTION_TYPE_CONSTRAINT ((T.TYPE_FUNCTION_VAR tfv, typeFunction), labs, stts, deps)) :: cs') l =
	    (case S.getValStateTf state tfv of
		 NONE =>
		 let val _ = S.updateStateTf state tfv (collapseTf typeFunction labs stts deps)
		 in fsimplify cs' l
		 end
	       | SOME typeFunction' =>
		 let val c     = E.genCstTfAll typeFunction' typeFunction labs stts deps
		 in fsimplify (c :: cs') l
		 end)
	  | fsimplify ((E.FUNCTION_TYPE_CONSTRAINT ((T.TFC (seqty1, ty1, lab1), T.TFC (seqty2, ty2, lab2)), labs, stts, deps)) :: cs') l =
	    let val c1 = E.genCstSqAll seqty1 seqty2 labs stts deps
		val c2 = E.genCstTyAll ty1    ty2    labs stts deps
	    in fsimplify (c1 :: c2 :: cs') l
	    end
	  | fsimplify ((E.FUNCTION_TYPE_CONSTRAINT ((typeFunction1, typeFunction2 as T.TYPE_FUNCTION_VAR tfv), labs, stts, deps)) :: cs') l =
	    fsimplify ((E.FUNCTION_TYPE_CONSTRAINT ((typeFunction2, typeFunction1), labs, stts, deps)) :: cs') l
	  | fsimplify ((E.IDENTIFIER_CLASS_CONSTRAINT ((CL.CLVAR clvar, cl), labs, stts, deps)) :: cs') l =
	    (case S.getValStateCl state clvar of
		 NONE =>
		 let val _ = S.updateStateCl state clvar (cl, labs, stts, deps)
		 in fsimplify cs' l end
	       | SOME (cl', labs', stts', deps') =>
		 let val labs0 = L.union labs labs'
		     val stts0 = L.union stts stts'
		     val deps0 = CD.union deps deps'
		     val c     = E.genCstClAll cl' cl labs0 stts0 deps0
		 in fsimplify (c :: cs') l
		 end)
	  | fsimplify ((E.IDENTIFIER_CLASS_CONSTRAINT ((x, CL.CLVAR cv), ls, deps, ids)) :: cs') l =
	    fsimplify ((E.IDENTIFIER_CLASS_CONSTRAINT ((CL.CLVAR cv, x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.IDENTIFIER_CLASS_CONSTRAINT ((CL.VID vid1, CL.VID vid2), labs, stts, deps)) :: cs') l =
	    let fun genError kind =
		    let val err = ERR.consPreError ERR.dummyId labs deps kind stts
		    in handleSimplify err cs' l
		    end
	    in case (vid1, vid2) of
		   (* REC vs CON (CON/DAT/EXC) *)
		   (CL.REC, CL.CON) => genError (EK.ConIsVar NONE)
		 | (CL.CON, CL.REC) => genError (EK.ConIsVar NONE)
		 | (CL.REC, CL.CO0) => genError (EK.ConIsVar NONE)
		 | (CL.CO0, CL.REC) => genError (EK.ConIsVar NONE)
		 | (CL.REC, CL.CO1) => genError (EK.ConIsVar NONE)
		 | (CL.CO1, CL.REC) => genError (EK.ConIsVar NONE)
		 | (CL.REC, CL.DAT) => genError (EK.ConIsVar NONE)
		 | (CL.DAT, CL.REC) => genError (EK.ConIsVar NONE)
		 | (CL.REC, CL.DA0) => genError (EK.ConIsVar NONE)
		 | (CL.DA0, CL.REC) => genError (EK.ConIsVar NONE)
		 | (CL.REC, CL.DA1) => genError (EK.ConIsVar NONE)
		 | (CL.DA1, CL.REC) => genError (EK.ConIsVar NONE)
		 | (CL.REC, CL.EXC) => genError (EK.ExcIsVar NONE)
		 | (CL.EXC, CL.REC) => genError (EK.ExcIsVar NONE)
		 | (CL.REC, CL.EX0) => genError (EK.ExcIsVar NONE)
		 | (CL.EX0, CL.REC) => genError (EK.ExcIsVar NONE)
		 | (CL.REC, CL.EX1) => genError (EK.ExcIsVar NONE)
		 | (CL.EX1, CL.REC) => genError (EK.ExcIsVar NONE)
		 (* VRC vs CON (CON/DAT/EXC) *)
		 | (CL.VRC, CL.CON) => genError (EK.ConIsVar NONE)
		 | (CL.CON, CL.VRC) => genError (EK.ConIsVar NONE)
		 | (CL.VRC, CL.CO0) => genError (EK.ConIsVar NONE)
		 | (CL.CO0, CL.VRC) => genError (EK.ConIsVar NONE)
		 | (CL.VRC, CL.CO1) => genError (EK.ConIsVar NONE)
		 | (CL.CO1, CL.VRC) => genError (EK.ConIsVar NONE)
		 | (CL.VRC, CL.DAT) => genError (EK.ConIsVar NONE)
		 | (CL.DAT, CL.VRC) => genError (EK.ConIsVar NONE)
		 | (CL.VRC, CL.DA0) => genError (EK.ConIsVar NONE)
		 | (CL.DA0, CL.VRC) => genError (EK.ConIsVar NONE)
		 | (CL.VRC, CL.DA1) => genError (EK.ConIsVar NONE)
		 | (CL.DA1, CL.VRC) => genError (EK.ConIsVar NONE)
		 | (CL.VRC, CL.EXC) => genError (EK.ExcIsVar NONE)
		 | (CL.EXC, CL.VRC) => genError (EK.ExcIsVar NONE)
		 | (CL.VRC, CL.EX0) => genError (EK.ExcIsVar NONE)
		 | (CL.EX0, CL.VRC) => genError (EK.ExcIsVar NONE)
		 | (CL.VRC, CL.EX1) => genError (EK.ExcIsVar NONE)
		 | (CL.EX1, CL.VRC) => genError (EK.ExcIsVar NONE)
		 (* PAT vs CO1 (CO1/DA1/EX1) *)
		 | (CL.PAT, CL.CO1) => genError (EK.ValVarApp NONE)
		 | (CL.CO1, CL.PAT) => genError (EK.ValVarApp NONE)
		 | (CL.PAT, CL.DA1) => genError (EK.ValVarApp NONE)
		 | (CL.DA1, CL.PAT) => genError (EK.ValVarApp NONE)
		 | (CL.PAT, CL.EX1) => genError (EK.ValVarApp NONE)
		 | (CL.EX1, CL.PAT) => genError (EK.ValVarApp NONE)
		 (* DAT vs EXC *)
		 | (CL.DAT, CL.EXC) => genError (EK.ExcIsDat NONE)
		 | (CL.EXC, CL.DAT) => genError (EK.ExcIsDat NONE)
		 | (CL.DAT, CL.EX0) => genError (EK.ExcIsDat NONE)
		 | (CL.EX0, CL.DAT) => genError (EK.ExcIsDat NONE)
		 | (CL.DAT, CL.EX1) => genError (EK.ExcIsDat NONE)
		 | (CL.EX1, CL.DAT) => genError (EK.ExcIsDat NONE)
		 | (CL.DA0, CL.EXC) => genError (EK.ExcIsDat NONE)
		 | (CL.EXC, CL.DA0) => genError (EK.ExcIsDat NONE)
		 | (CL.DA0, CL.EX0) => genError (EK.ExcIsDat NONE)
		 | (CL.EX0, CL.DA0) => genError (EK.ExcIsDat NONE)
		 | (CL.DA0, CL.EX1) => genError (EK.ExcIsDat NONE)
		 | (CL.EX1, CL.DA0) => genError (EK.ExcIsDat NONE)
		 | (CL.DA1, CL.EXC) => genError (EK.ExcIsDat NONE)
		 | (CL.EXC, CL.DA1) => genError (EK.ExcIsDat NONE)
		 | (CL.DA1, CL.EX0) => genError (EK.ExcIsDat NONE)
		 | (CL.EX0, CL.DA1) => genError (EK.ExcIsDat NONE)
		 | (CL.DA1, CL.EX1) => genError (EK.ExcIsDat NONE)
		 | (CL.EX1, CL.DA1) => genError (EK.ExcIsDat NONE)
		 (* CO1 vs CO0 (CO0/DA0/EX0) *)
		 | (CL.CO1, CL.CO0) => genError (EK.ConsArgNApp (0, 0))
		 | (CL.CO0, CL.CO1) => genError (EK.ConsArgNApp (0, 0))
		 | (CL.CO1, CL.DA0) => genError (EK.ConsArgNApp (0, 0))
		 | (CL.DA0, CL.CO1) => genError (EK.ConsArgNApp (0, 0))
		 | (CL.CO1, CL.EX0) => genError (EK.ConsArgNApp (0, 0))
		 | (CL.EX0, CL.CO1) => genError (EK.ConsArgNApp (0, 0))
		 (* DA1 vs CO0 (CO0/DA0/EX0) *)
		 | (CL.DA1, CL.CO0) => genError (EK.ConsArgNApp (0, 0))
		 | (CL.CO0, CL.DA1) => genError (EK.ConsArgNApp (0, 0))
		 | (CL.DA1, CL.DA0) => genError (EK.ConsArgNApp (0, 0))
		 | (CL.DA0, CL.DA1) => genError (EK.ConsArgNApp (0, 0))
		 (* EX1 vs CO0 (CO0/DA0/EX0) *)
		 | (CL.EX1, CL.CO0) => genError (EK.ConsArgNApp (0, 0))
		 | (CL.CO0, CL.EX1) => genError (EK.ConsArgNApp (0, 0))
		 | (CL.EX1, CL.EX0) => genError (EK.ConsArgNApp (0, 0))
		 | (CL.EX0, CL.EX1) => genError (EK.ConsArgNApp (0, 0))
		 | _ => fsimplify cs' l
	    end
	  | fsimplify ((E.IDENTIFIER_CLASS_CONSTRAINT ((CL.TYCON, CL.TYCON), _, _, _)) :: cs') l = fsimplify cs' l
	  | fsimplify ((E.IDENTIFIER_CLASS_CONSTRAINT ((CL.TYVAR, CL.TYVAR), _, _, _)) :: cs') l = fsimplify cs' l
	  | fsimplify ((E.IDENTIFIER_CLASS_CONSTRAINT ((CL.STR,   CL.STR),   _, _, _)) :: cs') l = fsimplify cs' l
	  | fsimplify ((E.IDENTIFIER_CLASS_CONSTRAINT ((CL.SIG,   CL.SIG),   _, _, _)) :: cs') l = fsimplify cs' l
	  | fsimplify ((E.IDENTIFIER_CLASS_CONSTRAINT ((CL.FUNC,  CL.FUNC),  _, _, _)) :: cs') l = fsimplify cs' l
	  | fsimplify ((E.IDENTIFIER_CLASS_CONSTRAINT ((CL.OC,    CL.OC),    _, _, _)) :: cs') l = fsimplify cs' l
	  | fsimplify ((E.IDENTIFIER_CLASS_CONSTRAINT ((CL.ANY, _),   _, _, _)) :: cs') l = fsimplify cs' l
	  | fsimplify ((E.IDENTIFIER_CLASS_CONSTRAINT ((_, CL.ANY),   _, _, _)) :: cs') l = fsimplify cs' l
	  | fsimplify ((E.IDENTIFIER_CLASS_CONSTRAINT ((_, CL.TYCON), _, _, _)) :: cs') l = raise EH.DeadBranch ""
	  | fsimplify ((E.IDENTIFIER_CLASS_CONSTRAINT ((CL.TYCON, _), _, _, _)) :: cs') l = raise EH.DeadBranch ""
	  | fsimplify ((E.IDENTIFIER_CLASS_CONSTRAINT ((_, CL.TYVAR), _, _, _)) :: cs') l = raise EH.DeadBranch ""
	  | fsimplify ((E.IDENTIFIER_CLASS_CONSTRAINT ((CL.TYVAR, _), _, _, _)) :: cs') l = raise EH.DeadBranch ""
	  | fsimplify ((E.IDENTIFIER_CLASS_CONSTRAINT ((_, CL.STR),   _, _, _)) :: cs') l = raise EH.DeadBranch ""
	  | fsimplify ((E.IDENTIFIER_CLASS_CONSTRAINT ((CL.STR, _),   _, _, _)) :: cs') l = raise EH.DeadBranch ""
	  | fsimplify ((E.IDENTIFIER_CLASS_CONSTRAINT ((_, CL.SIG),   _, _, _)) :: cs') l = raise EH.DeadBranch ""
	  | fsimplify ((E.IDENTIFIER_CLASS_CONSTRAINT ((CL.SIG, _),   _, _, _)) :: cs') l = raise EH.DeadBranch ""
	  | fsimplify ((E.IDENTIFIER_CLASS_CONSTRAINT ((_, CL.FUNC),  _, _, _)) :: cs') l = raise EH.DeadBranch ""
	  | fsimplify ((E.IDENTIFIER_CLASS_CONSTRAINT ((CL.FUNC, _),  _, _, _)) :: cs') l = raise EH.DeadBranch ""
	  | fsimplify ((E.IDENTIFIER_CLASS_CONSTRAINT ((_, CL.OC),    _, _, _)) :: cs') l = raise EH.DeadBranch ""
	  | fsimplify ((E.IDENTIFIER_CLASS_CONSTRAINT ((CL.OC, _),    _, _, _)) :: cs') l = raise EH.DeadBranch ""
	  (**)
	  | fsimplify ((E.LET_CONSTRAINT env) :: cs') l =
	    let val _ = solveenv env false
	    in fsimplify cs' l
	    end
	  (**)
	  | fsimplify ((E.SIGNATURE_CONSTRAINT (ev0, ev1, ev2, ev3, lab)) :: cs') l =
	    (* 0: signature, 2: structure, 1: translucent, 3: opaque *)
	    let val btest = FI.testtodo filters lab

	    in if btest
	       then let val env0 = buildFEnv (E.consENV_VAR ev0 lab) state false
			(*(2010-06-15)Why do we need to refresh the signature/structure?
			 * Because we don't have proper type schemes and type functions? *)
			val env2 = buildFEnv (E.consENV_VAR ev2 lab) state true
			(*(2010-06-22)We might need to decorate all the constraint generated in here
			 * with labs, stts and deps.*)
			(*First we build the envs associated to the structure and signature,
			 * then we refresh the env associated to the signature.
			 * The (SOME O.empty) is to specify that we refresh all the internal and
			 * explicit type variables. *)
			(*(2010-03-03)We only refresh env2 to refresh the explicit type variables.
			 * We then could have a faster function that does less work! *)
			(*tfun are the type functions of the structure/realisation specified
			 * by the signature. *)
			val (tfnD, tfnT) = getTypeFunctionEnv env0 env2 L.empty L.empty CD.empty
			val (cs0, env1) = genTypeFunctionEnv' env0 state tfnT true
			(* If mk is E.STR then env0 is a signature and env2 is a structure.
			 * If env2 is incomplete then we don't need to turn the datatypes
			 * into dummy types, just the type functions. *)
			(* NOTE: IF env2 IS A VAR HERE THEN WE CAN TAKE env1 AS THE NON
			 * GENERALISED ENV.  No we can't do that because it will happen
			 * during enumeration (when removing labels) and it might cause type
			 * errors to occur for typable programs.
			 * We might however tag ev2 when dealing with a dummy structure in
			 * the basis. *)
			(*(2010-03-03)The second constraint set if for datatypes and type functions.*)
			val (cs1, cs2) = matchSigStr env1 env2 lab filters L.empty L.empty CD.empty true err
			(*val _ = D.printdebug2 (E.printEnv (E.CONSTRAINT_ENV (E.singcsts (L.dummyLab, cs1))) "")*)
			(*val _ = D.printdebug2 (E.printEnv (E.CONSTRAINT_ENV (E.singcsts (L.dummyLab, cs2))) "")*)
			val _ = sigVsStrON ()
			val _ = fsimplify ((decorateCst cs0 (L.singleton lab) L.empty CD.empty) @ cs1) l
			val _ = sigVsStrTypON ()
			val _ = fsimplify cs2 l
			val _ = sigVsStrTypOFF ()
			val _ = sigVsStrOFF ()
			val _ = case ev1 of
				    NONE => ()
				  | SOME ev =>
				    (*(2010-06-23)We need to rebuild env1 because of the new constraints
				     * generated by matchSigStr.  We don't need to refresh though. *)
				    let val env1' = buildFEnv (*justBuildEnv*) env1 state true
				    in S.updateStateEv state ev (E.pushExtEnv env1' (L.singleton lab) L.empty CD.empty)
				    end
			val _ = case ev3 of
				    NONE => ()
				  | SOME ev =>
				    let val env3 = freshenv' (renameenv' env0 state) (SOME O.empty) false
				    (*val _ = D.printdebug2 (E.printEnv (E.consENV_VAR ev lab) "" ^ "\n" ^ E.printEnv env3 "")*)
				    in S.updateStateEv state ev (E.pushExtEnv env3 (L.singleton lab) L.empty CD.empty)
				    end
		    in fsimplify cs' l
		    end
		    handle errorfound err => handleSimplify err cs' l
	       else fsimplify cs' l
	    end
	  (**)
	  | fsimplify ((E.FUNCTOR_CONSTRAINT (ev1, ev2, ev3, ev4, lab)) :: cs') l =
	    (* functor: ev1 -> ev2, argument : ev3, result ev4 *)
	    if FI.testtodo filters lab
	    then let val env1 = buildFEnv (E.consENV_VAR ev1 lab) state false
		     val env2 = buildFEnv (E.consENV_VAR ev2 lab) state true
		     val env3 = buildFEnv (E.consENV_VAR ev3 lab) state true
		     val (tfnD, tfnT) = getTypeFunctionEnv env1 env3 L.empty L.empty CD.empty
		     val tfun = mergeTypeFunction tfnD tfnT
		     val (cs0, env1') = genTypeFunctionEnv' env1 state tfun true
		     val (cs1, env2') = genTypeFunctionEnv' env2 state tfun true
		     val (cs2, cs3) = matchSigStr env1' env3 lab filters L.empty L.empty CD.empty false err
		     val _ = sigVsStrON ()
		     val _ = fsimplify ((decorateCst (cs0 @ cs1) (L.singleton lab) L.empty CD.empty) @ cs2) l
		     val _ = sigVsStrTypON ()
		     val _ = fsimplify cs3 l
		     val _ = sigVsStrTypOFF ()
		     val _ = sigVsStrOFF ()
		     (*val _ = D.printdebug2 (E.printEnv env2 "" ^ "\n" ^ E.printEnv env2' "")*)
		     val env2'' = buildFEnv env2' state true
		     val _ = S.updateStateEv state ev4 (E.pushExtEnv env2'' (L.singleton lab) L.empty CD.empty)
		 in fsimplify cs' l
		 end
		 handle errorfound err => handleSimplify err cs' l
	    else fsimplify cs' l
	  | fsimplify ((E.SHARING_CONSTRAINT (ev0, ev1, ev2, lab)) :: cs') l =
	    (* I need to transform this constraint in env as I've done for WHR. *)
	    (* 0: signature, 1: returned, 2: sharing *)
	    if FI.testtodo filters lab
	    then let val env0 = buildFEnv (*justBuildEnv*) (E.consENV_VAR ev0 lab) state false
		     val env2 = buildFEnv (*justBuildEnv*) (E.consENV_VAR ev2 lab) state false
		     (* We don't need to refresh these two *)
		     val (utf, tfun) = getTypeFunctionEnvSha env0 env2
		     val tfun = case utf of
				    NONE => OM.map (fn _ => newTypeFunction ()) tfun
				  | SOME (tf, labs, stts, deps) =>
				    OM.map (fn NONE => newTypeFunction ()
					     | SOME (labs', stts', deps') =>
					       collapseTf tf
							  (L.union  labs labs')
							  (L.union  stts stts')
							  (CD.union deps deps'))
					   tfun
		     val (cs0, env1) = genTypeFunctionEnv' env0 state tfun false
		     (*val _ = D.printdebug2 (S.printState state ^ "\n" ^ E.printEnv (E.consENV_VAR ev0 lab) "" ^ "\n" ^ E.printEnv env0 "")*)
		     (* Do we need this switch here? *)
		     val _ = sigVsStrON ()
		     val _ = fsimplify (decorateCst cs0 (L.singleton lab) L.empty CD.empty) l
		     val _ = sigVsStrOFF ()
		     (*val _ = D.printdebug2 (S.printState state ^ "\n" ^ E.printEnv env0 "" ^ "\n" ^ E.printEnv env2 "" ^ "\n" ^ E.printEnv env1 "")*)
		     val _ = S.updateStateEv state ev1 (E.pushExtEnv env1 (L.singleton lab) L.empty CD.empty)
		 in fsimplify cs' l
		 end
		 handle errorfound err => handleSimplify err cs' l
	    else fsimplify cs' l
	  (**)
	  | fsimplify ((E.ACCESSOR_CONSTRAINT acc) :: cs') l = (solveacc acc l; fsimplify cs' l)
	  | fsimplify ((E.TYPE_CONSTRAINT ((T.GEN _, T.TYPE_CONSTRUCTOR   _), _, _, _)) :: cs') l = raise EH.TODO "unhandled case in the constraint solver, raised in the 'f_simplify' function of Unification.sml"
	  | fsimplify ((E.TYPE_CONSTRAINT ((T.GEN _, T.EXPLICIT_TYPE_VAR   _), _, _, _)) :: cs') l = raise EH.TODO "unhandled case in the constraint solver, raised in the 'f_simplify' function of Unification.sml"
	  | fsimplify ((E.TYPE_CONSTRAINT ((T.GEN _, T.APPLICATION   _), _, _, _)) :: cs') l = raise EH.TODO "unhandled case in the constraint solver, raised in the 'f_simplify' function of Unification.sml"
	  | fsimplify ((E.TYPE_CONSTRAINT ((T.GEN _, T.TYPE_POLY  _), _, _, _)) :: cs') l = raise EH.TODO "unhandled case in the constraint solver, raised in the 'f_simplify' function of Unification.sml"
	  | fsimplify ((E.TYPE_CONSTRAINT ((T.GEN _, T.GEN _), _, _, _)) :: cs') l = fsimplify cs' l (*raise EH.TODO*)


	  (*(2010-06-23)We keep unifying but we should really chain the GENs.*)

	  (* here we swapping the order that the constraints come in
	   * So for example if we are solving an accessor (which happens above) and we get a form that looks like this:
	   *
	   * E.TYPE_CONSTRAINT ((T.TYPE_POLY(...), T.TYPE_VAR(...)...))
	   *
	   * we have a case to handle such a constraint, but when pattern matching we check for the T.TYPE_VAR coming
           * before the TYPE_POLY. In order so that we can get to this case, we simply call the function again
	   * and flip the arguments around.
	   *
	   *)
	  | fsimplify ((E.TYPE_CONSTRAINT ((T.TYPE_CONSTRUCTOR   x, T.TYPE_VAR   y), ls, deps, ids)) :: cs') l = fsimplify ((E.TYPE_CONSTRAINT ((T.TYPE_VAR   y, T.TYPE_CONSTRUCTOR   x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.TYPE_CONSTRAINT ((T.EXPLICIT_TYPE_VAR   x, T.TYPE_VAR   y), ls, deps, ids)) :: cs') l = fsimplify ((E.TYPE_CONSTRAINT ((T.TYPE_VAR   y, T.EXPLICIT_TYPE_VAR   x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.TYPE_CONSTRAINT ((T.TYPE_POLY  x, T.TYPE_VAR   y), ls, deps, ids)) :: cs') l = fsimplify ((E.TYPE_CONSTRAINT ((T.TYPE_VAR   y, T.TYPE_POLY  x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.TYPE_CONSTRAINT ((T.GEN x, T.TYPE_VAR   y), ls, deps, ids)) :: cs') l = fsimplify ((E.TYPE_CONSTRAINT ((T.TYPE_VAR   y, T.GEN x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.TYPE_CONSTRAINT ((T.TYPE_POLY  x, T.TYPE_CONSTRUCTOR   y), ls, deps, ids)) :: cs') l = fsimplify ((E.TYPE_CONSTRAINT ((T.TYPE_CONSTRUCTOR   y, T.TYPE_POLY  x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.TYPE_CONSTRAINT ((T.TYPE_POLY  x, T.APPLICATION   y), ls, deps, ids)) :: cs') l = fsimplify ((E.TYPE_CONSTRAINT ((T.APPLICATION   y, T.TYPE_POLY  x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.TYPE_CONSTRAINT ((T.TYPE_CONSTRUCTOR   x, T.APPLICATION   y), ls, deps, ids)) :: cs') l = fsimplify ((E.TYPE_CONSTRAINT ((T.APPLICATION   y, T.TYPE_CONSTRUCTOR   x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.TYPE_CONSTRAINT ((T.EXPLICIT_TYPE_VAR   x, T.APPLICATION   y), ls, deps, ids)) :: cs') l = fsimplify ((E.TYPE_CONSTRAINT ((T.APPLICATION   y, T.EXPLICIT_TYPE_VAR   x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.TYPE_CONSTRAINT ((T.TYPE_POLY  x, T.GEN y), ls, deps, ids)) :: cs') l = fsimplify ((E.TYPE_CONSTRAINT ((T.GEN y, T.TYPE_POLY  x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.TYPE_CONSTRAINT ((T.TYPE_CONSTRUCTOR   x, T.GEN y), ls, deps, ids)) :: cs') l = fsimplify ((E.TYPE_CONSTRAINT ((T.GEN y, T.TYPE_CONSTRUCTOR   x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.TYPE_CONSTRAINT ((T.EXPLICIT_TYPE_VAR   x, T.GEN y), ls, deps, ids)) :: cs') l = fsimplify ((E.TYPE_CONSTRAINT ((T.GEN y, T.EXPLICIT_TYPE_VAR   x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.TYPE_CONSTRAINT ((T.TYPE_POLY  x, T.EXPLICIT_TYPE_VAR   y), ls, deps, ids)) :: cs') l = fsimplify ((E.TYPE_CONSTRAINT ((T.EXPLICIT_TYPE_VAR   y, T.TYPE_POLY  x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.TYPE_CONSTRAINT ((T.TYPE_CONSTRUCTOR   x, T.EXPLICIT_TYPE_VAR   y), ls, deps, ids)) :: cs') l = fsimplify ((E.TYPE_CONSTRAINT ((T.EXPLICIT_TYPE_VAR   y, T.TYPE_CONSTRUCTOR   x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.TYPENAME_CONSTRAINT ((T.NC  x, T.TYPENAME_VAR  y), ls, deps, ids)) :: cs') l = fsimplify ((E.TYPENAME_CONSTRAINT ((T.TYPENAME_VAR  y, T.NC  x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.ROW_CONSTRAINT ((T.ROW_C  x, T.ROW_VAR  y), ls, deps, ids)) :: cs') l = fsimplify ((E.ROW_CONSTRAINT ((T.ROW_VAR  y, T.ROW_C  x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.ENV_CONSTRAINT ((E.ROW_ENV x, E.ENV_VAR y), ls, deps, ids)) :: cs') l = fsimplify ((E.ENV_CONSTRAINT ((E.ENV_VAR y, E.ROW_ENV x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.ENV_CONSTRAINT ((E.ENV_CONS x, E.ENV_VAR y), ls, deps, ids)) :: cs') l = fsimplify ((E.ENV_CONSTRAINT ((E.ENV_VAR y, E.ENV_CONS x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.ENV_CONSTRAINT _) :: cs') l = raise EH.TODO "unhandled case in the constraint solver, raised in the 'f_simplify' function of Unification.sml"


	  | fsimplify ((E.EQUALITY_TYPE_CONSTRAINT ((eq1, T.EQUALITY_TYPE_DEPENDANCY  (eq2, labs1, stts1, deps1)), labs, stts, deps)) :: cs') l = fsimplify ((E.EQUALITY_TYPE_CONSTRAINT ((eq1, eq2), L.union labs1 labs, L.union stts1 stts, CD.union deps1 deps)) :: cs') l

	  | fsimplify ((E.EQUALITY_TYPE_CONSTRAINT ((equalityTypeVar as T.EQUALITY_TYPE_VAR eqtv, equalityTypeStatus as T.EQUALITY_TYPE_STATUS status), ls, deps, ids)):: cs') l =
	    let
		val _ = if (not (!analysingBasis))
			then D.printDebugFeature D.UNIF D.EQUALITY_TYPES (fn _ => "solving an equality type constraint of "^(#red D.colors)
	  									  ^ T.printEqualityType(equalityTypeVar)^D.textReset^" and "^(#green D.colors)
	  									  ^ T.printEqualityType(equalityTypeStatus)
										  ^ ". Labels = " ^ L.toString ls)
			else ()
	    in
		case S.getValStateEq state eqtv of

	  	    (* we haven't seen this equality type variable in the state, so let's put it in *)
		    NONE =>
		    let
			val _ = S.updateStateEq state eqtv (T.EQUALITY_TYPE_DEPENDANCY ((T.EQUALITY_TYPE_STATUS status), (L.cons l ls), deps, ids))
		    in
			fsimplify cs' l
		    end
		  (* we've seen this equality type variable before, we should check the status values against one another *)
		  | SOME (T.EQUALITY_TYPE_DEPENDANCY((T.EQUALITY_TYPE_STATUS statusInMap), resultLabels, resultDeps, resultIds)) =>
		    (D.printDebugFeature D.UNIF D.EQUALITY_TYPES (fn _ => "status already exists in the map! Checking for equaliy type error...");
		     (* if the status assigned to the equality type variable in the map is the same as the status we're constraining
		      * it to, then that's alright *)
	  	     if (statusInMap = status andalso statusInMap <> T.UNKNOWN) then fsimplify cs' l
	  	     else
			 (* otherwise, an equality type variable is constrained to be both an EQUALITY_TYPE and NOT_EQUALITY_TYPE. Generate an error! *)
	  		 let
			     val _ = D.printDebugFeature D.UNIF D.EQUALITY_TYPES (fn _ => "Equality type error detected. Label information: l = "^(L.printLab l)^", ls = "^(L.toString (L.union resultLabels ls)))
 	  		     val ek   = EK.EqTypeRequired (L.toInt l)
			     val err  = ERR.consPreError ERR.dummyId (L.union resultLabels ls) ids ek deps
	  		 in handleSimplify err cs' l
	  		 end
	  	    )
		  | SOME (T.EQUALITY_TYPE_DEPENDANCY (T.EQUALITY_TYPE_VAR eqTypeVarDep, labs, stts, cd)) =>
		    let
			(* here we update the map so that the equality type variable is no longer mapped to a dependancy on an equality type variable,
			 * but now to the eqaulity type status (status dependancies are more important than equality type variable dependencies). *)
			val _ = S.replaceStateEq state eqtv (T.EQUALITY_TYPE_DEPENDANCY (equalityTypeStatus, L.cons l (L.union ls labs), deps, ids))

			(* jpirie: what about stts and deps? Should these be combined somehow? *)
			val c = E.EQUALITY_TYPE_CONSTRAINT ((T.EQUALITY_TYPE_VAR eqTypeVarDep, T.EQUALITY_TYPE_STATUS status), L.cons l (L.union labs ls), deps, ids)
		    in
			fsimplify (c::cs') l
		    end
		  | SOME _ => raise EH.TODO "got something in the equality status map that isn't handled!"
	    end

	  | fsimplify ((E.EQUALITY_TYPE_CONSTRAINT ((equalityTypeVar1 as T.EQUALITY_TYPE_VAR eqtv1, equalityTypeVar2 as T.EQUALITY_TYPE_VAR eqtv2), ls, deps, ids)):: cs') l =
	    let
	  	val _ = if (not (!analysingBasis))
			then D.printDebugFeature D.UNIF D.EQUALITY_TYPES (fn _ => "solving an equality type constraint of "^(#red D.colors)
	  									  ^ T.printEqualityType(equalityTypeVar1)^D.textReset^" and "^(#green D.colors)
	  									  ^ T.printEqualityType(equalityTypeVar2)
										  ^ ". Labels = " ^ L.toString ls)
			else ()
	    in
	  	if T.eqEqualityTypeVar eqtv1 eqtv2
	  	then fsimplify cs' l (* should this actually *ever* happen? *)
	  	else
		    case S.getValStateEq state eqtv2 of
			(* equality type status is not in the map *)
			NONE =>
			let
			    val _ = case S.getValStateEq state eqtv1 of
					(* we haven't seen either of these equality type variables before. Put the first one in the map here,
					 * and then we'll create a dependancy from the second to the first *)
					NONE =>
					(S.updateStateEq state eqtv2 (T.EQUALITY_TYPE_STATUS T.UNKNOWN);
					 S.updateStateEq state eqtv2 (T.EQUALITY_TYPE_DEPENDANCY (equalityTypeVar1, ls, deps, ids)))

				      (* the first equality type variable is in the map, we can create a dependancy safely *)
				      | SOME _ =>
					S.updateStateEq state eqtv2 (T.EQUALITY_TYPE_DEPENDANCY (equalityTypeVar1, ls, deps, ids))
			in fsimplify cs' l
			end

		      (* the eqaulity type variable already exists in the map
		       * we want to go create a constraint to see whether all the constraints are still satisfyable *)
		      | SOME (T.EQUALITY_TYPE_DEPENDANCY(T.EQUALITY_TYPE_STATUS mapStatus, labs, stts, cd)) =>
			let
			    (* jpirie: what about stts and deps? Should these be combined somehow? *)
			    val c = E.EQUALITY_TYPE_CONSTRAINT ((equalityTypeVar1, T.EQUALITY_TYPE_STATUS mapStatus), L.cons l (L.union labs ls), deps, ids)
			in
			    fsimplify (c::cs') l
			end


		      | _ => fsimplify cs' l
	    end

	  | fsimplify ((E.EQUALITY_TYPE_CONSTRAINT _)::cs') l = raise EH.TODO "equality type discovered in the constraint solver, raised in the 'f_simplify' function of Unification.sml"

	and handleSimplify err xs l =
	    if bcontinue
	    then fsimplify xs l
	    else raise errorex err

	and simplify cs l =
	    let val ret = fsimplify cs l
	    in ret
	    end

	(* hack: in run I fold right because then all the context will
         * be treated before the CSTGEN and CSTVAL.  We can then
         * discard a part of the state in CSTVAL.  Otherwise we would
         * have to modify the state to change T.EXPLICIT_TYPE_VAR into T.TYPE_VAR instead of
         * what we're currently doing in freshty - see example 96 *)
	and run cs =
	    (E.foldlicst
		 (fn (l, ocstl, ()) =>
		     if FI.testtodo filters (L.fromInt l)
		     then simplify ocstl (L.fromInt l)
		     else ())
		 ()
		 cs;
	     Success state)

	val timer = VT.startTimer ()
	val _ = sigVsStrOFF ()
	val _ = sigVsStrTypOFF ()
	val ret = run (E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env))
	    handle errorex err =>
		   (D.printDebugFeature D.UNIF D.CONSTRAINT_SOLVING (fn _ => "handling errorex - an error has been detected during unification");
		    if L.isin L.builtinLab (ERR.getL err)
		    then Error (ERR.setB (ERR.stripDummys err) true,  state)
		    else Error (ERR.setB (ERR.stripDummy  err) false, state))
    in
	let
	    val _ = D.printDebugFeature D.UNIF D.STATE (fn _ => S.printState state)
	    val _ = case ret of
			Error _ => D.printDebugFeature D.UNIF D.CONSTRAINT_SOLVING (fn _ => "returning 'Error' status from the unification algorithm")
		      | Success _ => D.printDebugFeature D.UNIF D.CONSTRAINT_SOLVING (fn _ => "returning 'Success' status from the unification algorithm")
	in
	    ret
	end
    end

end
