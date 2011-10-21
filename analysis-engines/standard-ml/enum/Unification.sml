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
 *  o Date:        21 May 2010
 *  o File name:   Unification.sml
 *  o Description: Contains the unification algorithm.  The file defines
 *      the functor Unif with signature UNIF and takes a parameter of
 *      signature STATE.
 *)

(* we need to use ":" instead of ":>" because of the types of State *)
functor Unif (S : STATE) : UNIF = struct

structure S   = S
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
datatype occty = T  of T.tyvar  EL.extLab
               | S  of T.seqvar EL.extLab
               | R  of T.rowvar EL.extLab
datatype cocc  = CT of T.tyvar  * T.ty
               | CS of T.seqvar * T.seqty
               | CR of T.rowvar * T.rowty

(* THESE bind forms are used to solve binders.   *)
datatype 'a sbind = BINDOUT              (* the binding has been discarded                    *)
		  | BINDNOT of E.cst     (* the binding is not a binding                      *)
		  | BINDIN  of 'a E.bind (* we kept the binding                               *)
		  | BINDPOL of 'a E.bind (* we kept the binding but weakened the poly field   *)
		  | BINDDUM of 'a E.bind (* the binding has been transformed into a dummy one *)

datatype user = MIN of ERR.error
	      | ENUM
	      | DBENUM


type tfun = T.tyfun OM.map

(* This is used to pass errors to the constraint solver from
 * auxiliary functions. *)
exception errorfound of ERR.error


(* printing *)

fun printCocc (CT (tv, ty))  = "CT(" ^ T.printtyvar  tv ^ "," ^ T.printty    ty ^ ")"
  | printCocc (CS (sv, sq))  = "CS(" ^ T.printseqvar sv ^ "," ^ T.printseqty sq ^ ")"
  | printCocc (CR (rv, rt))  = "CR(" ^ T.printrowvar rv ^ "," ^ T.printrowty rt ^ ")"

fun printOccty (T x) = "T" ^ EL.printExtLab' x T.printtyvar
  | printOccty (S x) = "S" ^ EL.printExtLab' x T.printseqvar
  | printOccty (R x) = "R" ^ EL.printExtLab' x T.printrowvar

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


fun decomptyrow (T.RV rv)                ll deps ids = ([R (rv, ll, deps, ids)], 0)
  | decomptyrow (T.RC (_, ty, _))        ll deps ids = decomptyty ty ll deps ids
  | decomptyrow (T.RD (r, x, y, z))      ll deps ids = decomptyrow r (L.union x ll) (L.union y deps) (CD.union z ids)
  | decomptyrow T.RO                     ll deps ids = ([], 0)
and decomptysq  (T.SV sv)                ll deps ids = ([S (sv, ll, deps, ids)], 0)
  | decomptysq  (T.SC (rtl, _, _))       ll deps ids = (decomptyrowlist rtl ll deps ids, 1)
  | decomptysq  (T.SD (s, x, y, z))      ll deps ids = decomptysq s (L.union x ll) (L.union y deps) (CD.union z ids)
and decomptyty  (T.V (tv, _, _))         ll deps ids = ([T (tv, ll, deps, ids)], 0)
  | decomptyty  (T.E (n, tv, l))         ll deps ids = ([], 0) (* TODO: because it's a constant type but check that anyway with a circularity test *)
  | decomptyty  (T.C (_, sq, _))         ll deps ids = decomptysq sq ll deps ids
  | decomptyty  (T.A (tyf, sq, _))       ll deps ids = ([], 0) (* NOTE: Can a circularity error go through a type/datatype definition? *)
  | decomptyty  (T.OR  (sq, _, _, _, _)) ll deps ids = decomptysq sq ll deps ids
  | decomptyty  (T.GEN ty)               ll deps ids = ([], 0) (*(2010-06-23)Isn'y that risky?*)
  | decomptyty  (T.TD (t, x, y, z))      ll deps ids = decomptyty t (L.union x ll) (L.union y deps) (CD.union z ids)
and decomptyrowlist xs ll deps ids = foldr (fn (x, y) => (#1 (decomptyrow x ll deps ids)) @ y) [] xs
(*and decomptytylist  xs ll deps ids = foldr (fn (x, y) => (#1 (decomptyty  x ll deps ids)) @ y) [] xs*)


(* -------------- *)

(*fun bindsVal' (csenv1, csenv2, _, _, _, _, _, _) =
    not (List.null csenv1) orelse not (List.null csenv2)*)

(*fun bindsVal env =
    case SO.getSol () of
	SO.SOL8 => not (E.isEmptyValEnv env) (*bindsVal' env*)
      | _ => true

fun pushEnvToState env state =
    case SO.getSol () of
	SO.SOL8 => S.pushEnvToState env state
      | _       => ()

fun remEnvFromState env state =
    case SO.getSol () of
	SO.SOL8 => S.remEnvFromState env state
      | _       => ()

(*fun getBackBinder xs id =
    List.find (fn x => I.eqId (C.getBindI x) id) xs*)*)

fun getpairs l1 l2 =
    foldr (fn (x, pairs) => foldr (fn (y, pairs) => (x, y) :: pairs) pairs l2) [] l1

fun comparetypsenv typsenv1 typsenv2 filters ls deps ids = raise EH.TODO

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

(* compares the value identifiers from two different environments *)
fun compareenv env1 env2 filters ls deps ids =
    let val cs1 = comparevidsenv (E.getVids env1) (E.getVids env2) filters ls deps ids
	val cs2 = comparetypsenv (E.getTyps env1) (E.getTyps env2) filters ls deps ids
	val cs3 = comparestrsenv (E.getStrs env1) (E.getStrs env2) filters ls deps ids
    in cs1 @ cs2 @ cs3
    end

fun decorateCst' (E.CSTSEQ x) labs stts deps = E.CSTSEQ (EL.updExtLab x labs stts deps)
  | decorateCst' (E.CSTTYP x) labs stts deps = E.CSTTYP (EL.updExtLab x labs stts deps)
  | decorateCst' (E.CSTTYF x) labs stts deps = E.CSTTYF (EL.updExtLab x labs stts deps)
  | decorateCst' c labs stts deps = (print (E.printEnv (E.ENVCST (E.singcst (L.dummyLab, c))) "");
				     raise EH.DeadBranch "")

fun decorateCst xs labs stts deps =
    map (fn x => decorateCst' x labs stts deps) xs


(* Collapsing of dependencies - have to be moved to Ty.sml. *)

fun collapseTy (T.TD (T.TD (ty, labs1, stts1, deps1), labs2, stts2, deps2)) labs3 stts3 deps3 =
    collapseTy ty
	       (L.union  (L.union  labs1 labs2) labs3)
	       (L.union  (L.union  stts1 stts2) stts3)
	       (CD.union (CD.union deps1 deps2) deps3)
  | collapseTy (T.TD (ty, labs1, stts1, deps1)) labs2 stts2 deps2 =
    T.TD (ty,
	  L.union  labs1 labs2,
	  L.union  stts1 stts2,
	  CD.union deps1 deps2)
  | collapseTy ty labs stts deps = T.TD (ty, labs, stts, deps)

fun collapseSq (T.SD (T.SD (sq, labs1, stts1, deps1), labs2, stts2, deps2)) labs3 stts3 deps3 =
    collapseSq sq
	       (L.union  (L.union  labs1 labs2) labs3)
	       (L.union  (L.union  stts1 stts2) stts3)
	       (CD.union (CD.union deps1 deps2) deps3)
  | collapseSq (T.SD (sq, labs1, stts1, deps1)) labs2 stts2 deps2 =
    T.SD (sq,
	  L.union  labs1 labs2,
	  L.union  stts1 stts2,
	  CD.union deps1 deps2)
  | collapseSq sq labs stts deps = T.SD (sq, labs, stts, deps)

fun collapseRt (T.RD (T.RD (rt, labs1, stts1, deps1), labs2, stts2, deps2)) labs3 stts3 deps3 =
    collapseRt rt
	       (L.union  (L.union  labs1 labs2) labs3)
	       (L.union  (L.union  stts1 stts2) stts3)
	       (CD.union (CD.union deps1 deps2) deps3)
  | collapseRt (T.RD (rt, labs1, stts1, deps1)) labs2 stts2 deps2 =
    T.RD (rt,
	  L.union  labs1 labs2,
	  L.union  stts1 stts2,
	  CD.union deps1 deps2)
  | collapseRt rt labs stts deps = T.RD (rt, labs, stts, deps)

fun collapseTn (T.ND (T.ND (tn, labs1, stts1, deps1), labs2, stts2, deps2)) labs3 stts3 deps3 =
    collapseTn tn
	       (L.union  (L.union  labs1 labs2) labs3)
	       (L.union  (L.union  stts1 stts2) stts3)
	       (CD.union (CD.union deps1 deps2) deps3)
  | collapseTn (T.ND (tn, labs1, stts1, deps1)) labs2 stts2 deps2 =
    T.ND (tn,
	  L.union  labs1 labs2,
	  L.union  stts1 stts2,
	  CD.union deps1 deps2)
  | collapseTn tn labs stts deps = T.ND (tn, labs, stts, deps)

fun collapseTf (T.TFD (T.TFD (tf, labs1, stts1, deps1), labs2, stts2, deps2)) labs3 stts3 deps3 =
    collapseTf tf
	       (L.union  (L.union  labs1 labs2) labs3)
	       (L.union  (L.union  stts1 stts2) stts3)
	       (CD.union (CD.union deps1 deps2) deps3)
  | collapseTf (T.TFD (tf, labs1, stts1, deps1)) labs2 stts2 deps2 =
    T.TFD (tf,
	  L.union  labs1 labs2,
	  L.union  stts1 stts2,
	  CD.union deps1 deps2)
  | collapseTf tf labs stts deps = T.TFD (tf, labs, stts, deps)



(* This is used for or types *)

fun gatherAllTnTy (T.C (tn, _, _)) = gatherAllTnTn tn
  | gatherAllTnTy (T.OR (sq, _, _, _, _)) = gatherAllTnSq sq
  | gatherAllTnTy (T.TD (ty, labs, stts, deps)) =
    let val (list, labs', stts', deps') = gatherAllTnTy ty
    in (list, L.union labs labs', L.union stts stts', CD.union deps deps')
    end
  | gatherAllTnTy _ = ([], L.empty, L.empty, CD.empty)
and gatherAllTnSq (T.SV _) = ([], L.empty, L.empty, CD.empty)
  | gatherAllTnSq (T.SC (rtl, _, _)) =
    foldr (fn ((list1, labs1, stts1, deps1), (list2, labs2, stts2, deps2)) =>
	      (list1 @ list2,
	       L.union  labs1 labs2,
	       L.union  stts1 stts2,
	       CD.union deps1 deps2))
	  ([], L.empty, L.empty, CD.empty)
	  (map gatherAllTnRt rtl)
  | gatherAllTnSq (T.SD (seq, labs, stts, deps)) =
    let val (list, labs', stts', deps') = gatherAllTnSq seq
    in (list, L.union labs labs', L.union stts stts', CD.union deps deps')
    end
and gatherAllTnRt (T.RV _) = ([], L.empty, L.empty, CD.empty)
  | gatherAllTnRt (T.RC (_, ty, _)) = gatherAllTnTy ty
  | gatherAllTnRt (T.RD (row, labs, stts, deps)) =
    let val (list, labs', stts', deps') = gatherAllTnRt row
    in (list, L.union labs labs', L.union stts stts', CD.union deps deps')
    end
  | gatherAllTnRt T.RO = ([], L.empty, L.empty, CD.empty)
and gatherAllTnTn (T.NC (tn, _, l)) =
    ([(L.toInt l, T.tynameToInt tn)], L.empty, L.empty, CD.empty)
  | gatherAllTnTn (T.ND (tn, labs, stts, deps)) =
    let val (list, labs', stts', deps') = gatherAllTnTn tn
    in (list, L.union labs labs', L.union stts stts', CD.union deps deps')
    end
  | gatherAllTnTn (T.NV _) = ([], L.empty, L.empty, CD.empty)

(*fun gatherTnTy tn (T.C (T.NC (tn', _, l), _, _)) = if tn = tn' then [] else [(l, tn')]
  | gatherTnTy tn (T.Or (sq, _, _)) = gatherTnSq tn sq
  | gatherTnTy _ _ = []
and gatherTnSq _ (T.SV _) = []
  | gatherTnSq tn (T.SC (rtl, _, _)) = List.concat (map (fn x => gatherTnRt tn x) rtl)
and gatherTnRt _ (T.RV _) = []
  | gatherTnRt tn (T.RC (_, ty, _)) = gatherTnTy tn ty*)

fun isAllTy (T.OR (sq, _, _, _, _)) = isAllSq sq
  | isAllTy (T.C (tn, _, _)) = isAllTn tn
  | isAllTy (T.TD ety) = isAllTy (EL.getExtLabT ety)
  | isAllTy _ = false
and isAllSq (T.SV _) = false
  | isAllSq (T.SC (rtl, _, _)) = List.all isAllRt rtl
  | isAllSq (T.SD eseq) = isAllSq (EL.getExtLabT eseq)
and isAllRt (T.RV _) = false
  | isAllRt (T.RC (_, ty, _)) = isAllTy ty
  | isAllRt (T.RD erow) = isAllRt (EL.getExtLabT erow)
  | isAllRt T.RO = true
and isAllTn (T.NC _) = true
  | isAllTn (T.ND etn) = isAllTn (EL.getExtLabT etn)
  | isAllTn (T.NV _) = false

fun mergeFindInOrLists [] list = list
  | mergeFindInOrLists list [] = list
  | mergeFindInOrLists ((T.V _, _) :: x :: _) _ = raise EH.DeadBranch ""
  | mergeFindInOrLists _ ((T.V _, _) :: x :: _) = raise EH.DeadBranch ""
  | mergeFindInOrLists list [(T.V _, _)] = list (* list cannot be empty *)
  | mergeFindInOrLists [(T.V _, _)] list = list (* list cannot be empty *)
  | mergeFindInOrLists list1 list2 = list1 @ list2

fun findInOrTy tn path (t as (T.C (tnc, _, _))) =
    (case findInOrTn tnc of
	 SOME tn' =>
	 if T.eqTyname tn tn'
	 then ([(t, path)], true, true)
	 else ([], false, false)
       | NONE => ([], true, false))
  | findInOrTy tn path (T.OR (sq, _, _, _, _)) = findInOrSq tn path sq
  | findInOrTy _ path (t as (T.V _)) = ([(t, path)], true, false)
  | findInOrTy tn path (T.TD ety) = findInOrTy tn path (EL.getExtLabT ety)
  | findInOrTy _ _ _ = ([], true, false)

and findInOrSq tn path (T.SC (rtl, _, _)) =
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
  | findInOrSq _ _ (T.SV _) = ([], true, false)
  | findInOrSq tn path (T.SD eseq) = findInOrSq tn path (EL.getExtLabT eseq)

and findInOrRt tn path (T.RC (_, ty, _)) = findInOrTy tn path ty
  | findInOrRt _ _ (T.RV _) = ([], true, false)
  | findInOrRt tn path (T.RD erow) = findInOrRt tn path (EL.getExtLabT erow)
  | findInOrRt tn path T.RO = ([], false, false)

and findInOrTn (T.NC (tn, _, _)) = SOME tn
  | findInOrTn (T.NV _) = NONE
  | findInOrTn (T.ND etn) = findInOrTn (EL.getExtLabT etn)

fun gotoInOrTy path (t as (T.C (tn, _, _))) =
    (if List.null path
     then case gotoInOrTn tn of
	      SOME _ => SOME t
	    | NONE => NONE
     else NONE)
  | gotoInOrTy path (T.OR (sq, _, _, _, _)) = gotoInOrSq path sq
  | gotoInOrTy path (t as (T.V _)) =
    if List.null path then SOME t else NONE
  | gotoInOrTy path (T.TD (ty, labs, stts, deps)) =
    (case gotoInOrTy path ty of
	 SOME ty => SOME (collapseTy ty labs stts deps)
       | NONE => NONE)
  | gotoInOrTy _ _ = NONE
and gotoInOrSq (p :: path) (T.SC (rtl, _, _)) =
    (gotoInOrRt path (List.nth (rtl, p))
     handle Subscript => raise EH.DeadBranch "an OR type has an unexpected structure")
  | gotoInOrSq [] (T.SC _) = raise EH.DeadBranch "an OR type has an unexpected structure"
  | gotoInOrSq _ (T.SV _) = NONE
  | gotoInOrSq path (T.SD (seq, labs, stts, deps)) =
    (case gotoInOrSq path seq of
	 SOME ty => SOME (collapseTy ty labs stts deps)
       | NONE => NONE)
and gotoInOrRt path (T.RC (_, ty, _)) = gotoInOrTy path ty
  | gotoInOrRt _ (T.RV _) = NONE
  | gotoInOrRt path (T.RD (row, labs, stts, deps)) =
    (case gotoInOrRt path row of
	 SOME ty => SOME (collapseTy ty labs stts deps)
       | NONE => NONE)
  | gotoInOrRt path T.RO = NONE
and gotoInOrTn (tn as T.NC (name, _, _)) = SOME tn
  | gotoInOrTn (T.ND (tn, labs, stts, deps)) =
    (case gotoInOrTn tn of
	 SOME tn => SOME (collapseTn tn labs stts deps)
       | NONE => NONE)
  | gotoInOrTn (T.NV _) = NONE

(* We build the sequence variable in a OR because in the case of overloading constants
 * These are not already built when dealing with the binder. *)
fun buildDirectOr (ty as T.OR (T.SV sv, idor, poly, kind, lab)) state =
    (case S.getValStateSq state sv of
	 NONE => ty
       | SOME sq => T.OR (sq, idor, poly, kind, lab))
  | buildDirectOr ty state = ty


fun getPathsCol (paths : S.paths) col =
    List.mapPartial (fn [] => NONE
		      | (c :: path) =>
			if c = col
			then SOME path
			else NONE)
		    paths

fun selectPathsSeq [] seq = NONE
  | selectPathsSeq paths (seq as T.SV _) = SOME seq
  | selectPathsSeq paths (T.SD (seq, labs, stts, deps)) =
    (case selectPathsSeq paths seq of
	 NONE => NONE
       | SOME seq' => SOME (T.SD (seq', labs, stts, deps)))
  | selectPathsSeq paths (sq as T.SC (rows, flex, lab)) =
    let val (rows', some, _) =
	    foldl (fn (row, (rows, some, col)) =>
		      let val paths' = getPathsCol paths col
		      in case selectPathsRow paths' row of
			     NONE => (rows @ [T.RO], some, col + 1)
			   | SOME row' => (rows @ [row'], true, col + 1)
		      end)
		  ([], false, 0)
		  rows
    in if some
       then SOME (T.SC (rows', flex, lab))
       else NONE
    end

and selectPathsRow [] row = NONE
  | selectPathsRow paths (row as T.RV _) = SOME row
  | selectPathsRow paths (T.RD (row, labs, stts, deps)) =
    (case selectPathsRow paths row of
	 NONE => NONE
       | SOME row' => SOME (T.RD (row', labs, stts, deps)))
  | selectPathsRow paths (T.RC (fieldname, ty, lab)) =
    (case selectPathsTy paths ty of
	 NONE => NONE
       | SOME ty' => SOME (T.RC (fieldname, ty', lab)))
  | selectPathsRow paths T.RO = SOME T.RO

and selectPathsTy [] ty = NONE
  | selectPathsTy paths (ty as T.C (tn, seq, lab)) =
    if List.exists (fn path => List.null path) paths
    then SOME ty
    else NONE
  | selectPathsTy paths (T.OR (seq, id, poly, kind, lab)) =
    (case selectPathsSeq paths seq of
	 NONE => NONE
       | SOME seq' => SOME (T.OR (seq', id, poly, kind, lab)))
  | selectPathsTy paths (ty as T.V _) = SOME ty
  | selectPathsTy paths (T.TD (ty, labs, stts, deps)) =
    (case selectPathsTy paths ty of
	 NONE => NONE
       | SOME ty' => SOME (T.TD (ty', labs, stts, deps)))
  | selectPathsTy _ _ = NONE

fun selectPaths paths seq =
    case selectPathsSeq paths seq of
	NONE => T.SV (T.freshseqvar ())
      | SOME seq => seq


(*fun sameSeqs (T.SV _) seq2 = (NONE, NONE, true)
  | sameSeqs seq1 (T.SV _) = (NONE, NONE, true)
  | sameSeqs (T.SD eseq1) seq2 = sameSeqs (EL.getExtLabT eseq1) seq2
  | sameSeqs seq1 (T.SD eseq2) = sameSeqs seq1 (EL.getExtLabT eseq2)
  | sameSeqs (T.SC (rows1, _, _)) (T.SC (rows2, _, _)) =
    if sameRowsList rows1 rows2
    then (SOME [], SOME [], true)
    else (NONE, NONE, false)

and sameRows (T.RV _) row2 = (NONE, NONE, true)
  | sameRows row1 (T.RV _) = (NONE, NONE, true)
  | sameRows (T.RD erow1) row2 = sameRows (EL.getExtLabT erow1) row2
  | sameRows row1 (T.RD erow2) = sameRows row1 (EL.getExtLabT erow2)
  | sameRows (T.RC (_, ty1, _)) (T.TC (_, ty2, _)) =

and sameTys (T.V _) ty2 = (NONE, NONE, true)
  | sameTys ty1 (T.V _) = (NONE, NONE, true)
  | sameTys (T.TD ety1) ty2 = sameTys (EL.getExtLabT ety1) ty2
  | sameTys ty1 (T.TD ety2) = sameTys ty1 (EL.getExtLabT ety2)
  | sameTys ()*)


fun isFullOrSeq (T.SV _) = false
  | isFullOrSeq (T.SD eseq) = isFullOrSeq (EL.getExtLabT eseq)
  | isFullOrSeq (T.SC (rows, _, _)) =
    List.all (fn row => isFullOrRow row) rows

and isFullOrRow (T.RV _) = false
  | isFullOrRow (T.RD erow) = isFullOrRow (EL.getExtLabT erow)
  | isFullOrRow (T.RC (_, ty, _)) = isFullOrTy ty
  | isFullOrRow T.RO = true

and isFullOrTy (T.C (tn, _, _)) = isFullOrTn tn
  | isFullOrTy (T.OR (seq, _, _, _, _)) = isFullOrSeq seq
  | isFullOrTy (T.TD ety) = isFullOrTy (EL.getExtLabT ety)
  | isFullOrTy _ = false

and isFullOrTn (T.NC (name, _, _)) = true
  | isFullOrTn (T.ND etn) = isFullOrTn (EL.getExtLabT etn)
  | isFullOrTn (T.NV _) = false

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

fun tryToMatchOrsSq path (T.SV _) sq2 = (([], false), ([], true), true)
  | tryToMatchOrsSq path (sq1 as T.SC (rows, _, _)) sq2 =
    let val (typaths1, typaths2, found, _) =
	    foldl (fn (row, (typaths1, typaths2, found, c)) =>
		      case tryToMatchOrsRt (path @ [c]) row sq2 of
			  (_, _, false) => (typaths1, typaths2, found, c+1)
			| (typaths1', typaths2', true) =>
			  (concatOptList typaths1 typaths1',
			   concatOptList typaths2 typaths2',
			   true,
			   c+1))
		  (([], true), ([], true), false, 0)
		  rows
	(*val _ = D.printdebug2 ("[" ^ Bool.toString found ^ "]\n" ^
			       T.printseqty sq1    ^ "\n" ^
			       T.printseqty sq2)*)
    in (typaths1, typaths2, found)
    end
  | tryToMatchOrsSq path (T.SD eseq) sq2 =
    tryToMatchOrsSq path (EL.getExtLabT eseq) sq2

and tryToMatchOrsTy path (t as (T.C (tn, _, _))) sq =
    (case tryToMatchOrsTn path tn sq of
	 (list as (x :: _), true)  => (([(t, path)], true), (list, true), true)
       | (list as (x :: _), false) => raise EH.DeadBranch ""
       | ([], true)                => (([(t, path)], true), ([], false), true)
       | ([], false)               => (([], false), ([], false), false))
  | tryToMatchOrsTy path (T.OR (sq, _, _, _, _)) sq2 = tryToMatchOrsSq path sq sq2
  | tryToMatchOrsTy path (t as T.V _) sq2 = (([], false), ([], true), true)
  | tryToMatchOrsTy path (T.TD ety) sq2 =
    tryToMatchOrsTy path (EL.getExtLabT ety) sq2
  | tryToMatchOrsTy _ _ _ = (([], false), ([], true), true) (* Something isn't complete *)

and tryToMatchOrsTn path (T.NC (name, _, _)) sq =
    (case findInOrSq name [] sq of
	 ([], true, _)                   => ([], true)                (* Something isn't complete *)
       | ([], false, _)                  => ([], false)               (* No match                 *)
       | (list, false, _)                => raise EH.DeadBranch ""    (* Shouldn't happen         *)
       | ([(T.V _, _)], true, false)     => ([], true)                (* We found a match         *)
       | ([(T.V _, _)], true, true)      => raise EH.DeadBranch ""    (* Shouldn't happen         *)
       | (((T.V _, _) :: _), _, _)       => raise EH.DeadBranch ""    (* Shouldn't happen         *)
       | (list, true, true)              => (list, true)              (* We found a match         *)
       | (list, true, false)             => raise EH.DeadBranch "")   (* Shouldn't happen         *)
  | tryToMatchOrsTn path (T.ND etn) sq = tryToMatchOrsTn path (EL.getExtLabT etn) sq
  | tryToMatchOrsTn path (T.NV _) sq = ([], true)

and tryToMatchOrsRt path (T.RC (_, ty, _)) sq = tryToMatchOrsTy path ty sq
  | tryToMatchOrsRt _ (T.RV _) _ = (([], false), ([], true), true)
  | tryToMatchOrsRt path (T.RD erow) sq2 =
    tryToMatchOrsRt path (EL.getExtLabT erow) sq2
  | tryToMatchOrsRt path T.RO sq2 = (([], true), ([], true), false)

fun tryToMatchOrs sq1 sq2 = tryToMatchOrsSq [] sq1 sq2



(* Old matching of or/seq structures *)

(*fun tryToMatchOrsSq path (T.SV _) sq2 = (NONE, NONE, true)
  | tryToMatchOrsSq path (T.SC (rtl, _, _)) sq2 =
    let val (typath1, typath2, found, _) =
	    foldl (fn (rt, (typath1, typath2, found, c)) =>
		      if found
		      then if Option.isSome typath1
			      andalso Option.isSome typath2
			      andalso Option.valOf (Option.map (T.isTyC o #1) typath1)
			      andalso Option.valOf (Option.map (T.isTyC o #1) typath2)
			   then (typath1, typath2, found, c+1)
			   else case tryToMatchOrsRt (path @ [c]) rt sq2 of
				    (_, _, false) => (typath1, typath2, found, c+1)
				  | (typath1', typath2', true) =>
				    ((*if Option.isSome typath1 then typath1 else*) typath1',
				     (*if Option.isSome typath2 then typath2 else*) typath2',
				     true,
				     c+1)
		      else let val (typath1', typath2', found') = tryToMatchOrsRt (path @ [c]) rt sq2
			   in (typath1', typath2', found', c+1)
			   end)
		  (NONE, NONE, false, 0)
		  rtl
    in (typath1, typath2, found)
    end
  | tryToMatchOrsSq path (T.SD eseq) sq2 =
    tryToMatchOrsSq path (EL.getExtLabT eseq) sq2

and tryToMatchOrsTy path (t as (T.C (tn, _, _))) sq =
    (case tryToMatchOrsTn path tn sq of
	 (SOME x, b) => (SOME (t, path), SOME x, b)
       | (NONE, b) => (NONE, NONE, b))
(*    (case findInOrSq tn [] sq of
	 (SOME (t', path'), true, _, _, _) => (SOME (t, path), SOME (t', path'), true)  (* We found a match         *)
       | (SOME x, false, _, _, _)          => raise EH.DeadBranch ""                    (* Shouldn't happen         *)
       | (NONE, true, _, _, _)             => (NONE, NONE, true)                        (* Something isn't complete *)
       | (NONE, false, _, _, _)            => (NONE, NONE, false))                      (* No match                 *)*)
  | tryToMatchOrsTy path (T.OR (sq, _, _, _, _)) sq2 = tryToMatchOrsSq path sq sq2
  | tryToMatchOrsTy path (t as T.V _) sq2 = (SOME (t, path), NONE, true)
  | tryToMatchOrsTy path (T.TD ety) sq2 =
    tryToMatchOrsTy path (EL.getExtLabT ety) sq2
  | tryToMatchOrsTy _ _ _ = (NONE, NONE, true) (* Something isn't complete *)

and tryToMatchOrsTn path (T.NC (name, _, _)) sq =
    (case findInOrSq name [] sq of
	 (SOME (t', path'), true, _) => (SOME (t', path'), true)  (* We found a match         *)
       | (SOME x, false, _)          => raise EH.DeadBranch ""    (* Shouldn't happen         *)
       | (NONE, true, _)             => (NONE, true)              (* Something isn't complete *)
       | (NONE, false, _)            => (NONE, false))            (* No match                 *)
  | tryToMatchOrsTn path (T.ND etn) sq = tryToMatchOrsTn path (EL.getExtLabT etn) sq
  | tryToMatchOrsTn path (T.NV _) sq = (NONE, true)

and tryToMatchOrsRt path (T.RC (_, ty, _)) sq = tryToMatchOrsTy path ty sq
  | tryToMatchOrsRt _ (T.RV _) _ = (NONE, NONE, true)
  | tryToMatchOrsRt path (T.RD erow) sq2 =
    tryToMatchOrsRt path (EL.getExtLabT erow) sq2

fun tryToMatchOrs sq1 sq2 = tryToMatchOrsSq [] sq1 sq2*)


(* ------ Type freshning ------ *)

fun freshlabty (T.LV lv) state = T.LV (F.freshLabVar lv state)
  | freshlabty labty     _     = labty

fun freshtyname (T.NV var) state = T.NV (F.freshTyNameVar var state)
  | freshtyname tnty       _     = tnty

fun freshrowty (T.RV rv)           _   state _    = T.RV (F.freshRowVar rv state)
  | freshrowty (T.RC (lt, ty, l))  tvl state bstr = T.RC (freshlabty lt state, freshty ty tvl state bstr, l)
  | freshrowty (T.RD erow)         tvl state bstr = T.RD (EL.mapExtLab erow (fn row => freshrowty row tvl state bstr))
  | freshrowty T.RO                _   _     _    = T.RO
and freshtyfun (T.TFV tfv)         _   state _    = T.TFV (F.freshTyfVar tfv state)
  | freshtyfun (T.TFC (sq, ty, l)) tvl state bstr = T.TFC (freshseqty sq tvl state bstr, freshty ty tvl state bstr, l)
  | freshtyfun (T.TFD etf)         tvl state bstr = T.TFD (EL.mapExtLab etf (fn tf => freshtyfun tf tvl state bstr))
and freshseqty (T.SV var)          _   state _    = T.SV (F.freshSeqVar var state)
  | freshseqty (T.SC (trl, b, l))  tvl state bstr = T.SC (map (fn rt => freshrowty rt tvl state bstr) trl, b, l)
  | freshseqty (T.SD eseq)         tvl state bstr = T.SD (EL.mapExtLab eseq (fn seq => freshseqty seq tvl state bstr))
and freshty (T.V (tv, b, p))       tvl state bstr =
    (case (tvl, p) of
	(NONE, T.POLY) => T.V (F.freshTyVar tv state, if bstr then NONE else b, p)
      | (SOME tvl', T.POLY) =>
	if O.isin (T.tyvarToInt tv) tvl'
	then T.V (tv, if bstr then NONE else b, p)
	else T.V (F.freshTyVar tv state, if bstr then NONE else b, p)
      | (_, T.MONO) => T.V (tv, if bstr then NONE else b, (*T.POLY*)(*N*)T.MONO)) (* NOTE: We reset all the type variables as polymorphic.  Why?  Because of the accessors. *)
  | freshty (T.E   (id, tv,   l))  tvl state bstr =
    (*(2010-06-14)bstr is false when we refresh an environment when dealing with CSTSIG*)
    if bstr then T.E (id, tv, l) else T.V (F.freshTyVar tv state, SOME (id, l), T.POLY)
  | freshty (T.C  (tn, sq,   l))    tvl state bstr = T.C   (freshtyname tn     state,      freshseqty sq tvl state bstr, l)
  | freshty (T.A  (tf, sq,   l))    tvl state bstr = T.A   (freshtyfun  tf tvl state bstr, freshseqty sq tvl state bstr, l)
  | freshty (T.OR (sq, i, p, k, l)) tvl state bstr = T.OR  (freshseqty  sq tvl state bstr, if T.isPoly p then F.freshIdOr i state else i, T.MONO, k, l)
  | freshty (T.GEN tys)             tvl state bstr = T.GEN (ref (map (fn ty => freshty ty tvl state bstr) (!tys)))
  | freshty (T.TD  ety)             tvl state bstr = T.TD (EL.mapExtLab ety (fn ty => freshty ty tvl state bstr))

(* bstr below has to be true so that we don't refresh explicit type variables that
 * are bound higher and so cannot be generalised.  The true is to say that explicit
 * type variables are here considered as constant types. *)
fun freshTy ty tvl P.POLY = freshty ty tvl (F.finitState ()) true
  | freshTy ty _ _ = ty

fun freshTyFun tyfun bstr = freshtyfun tyfun NONE (F.finitState ()) bstr

fun freshTyVar tyvar P.POLY = T.freshtyvar ()
  | freshTyVar tyvar _      = tyvar

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
				     in (freshtyfun typfun tvl state bstr, tnKind, varenv)
				     end)

fun freshtypenv idenv tvl state bstr =
    E.mapenv (fn semty => map (fn x => freshexttypfun x tvl state bstr) semty) idenv

fun freshenv (E.ENVVAR (ev, lab)) tvl state _ = E.ENVVAR (F.freshEnvVar ev state, lab)
  | freshenv (env as E.ENVCON _) tvl state bstr =
    let val vids = freshvarenv (E.getVids env) tvl state bstr
	val typs = freshtypenv (E.getTyps env) tvl state false (*bstr*)
	(*(2010-03-03)We want false for typs because the types
	 * have to be the same, and so we want to know when a type
	 * variables comes from an explicit type variable. *)
	val tyvs = E.getTyvs env
	val strs = freshstrenv (E.getStrs env) tvl state bstr
	val sigs = freshstrenv (E.getSigs env) tvl state bstr
	val funs = E.getFuns env
	val ovcs = freshocsenv (E.getOvcs env) tvl state bstr
	val info = E.getInfo env
    in E.consEnvC vids typs tyvs strs sigs funs ovcs info
    end
  | freshenv (E.ENVSEQ (env1, env2)) tvl state bstr =
    E.ENVSEQ (freshenv env1 tvl state bstr, freshenv env2 tvl state bstr)
  | freshenv (E.ENVDEP (env, labs, stts, deps)) tvl state bstr =
    let val env' = freshenv env tvl state bstr
    in E.ENVDEP (env', labs, stts, deps)
    end
  | freshenv (E.ENVTOP)   tvl state bstr = E.ENVTOP
  | freshenv (E.ENVPOL _) tvl state bstr = raise EH.DeadBranch "this should have been built by now"
  | freshenv (E.ENVLOC _) tvl state bstr = raise EH.DeadBranch "this should have been built by now"
  | freshenv (E.ENVWHR _) tvl state bstr = raise EH.DeadBranch "this should have been built by now"
  | freshenv (E.ENVSHA _) tvl state bstr = raise EH.DeadBranch "this should have been built by now"
  | freshenv (E.ENVSIG _) tvl state bstr = raise EH.DeadBranch "this should have been built by now"
  | freshenv (E.ENVDAT _) tvl state bstr = raise EH.DeadBranch "this should have been built by now"
  | freshenv (E.ENVOPN _) tvl state bstr = raise EH.DeadBranch "this should have been built by now"
  | freshenv (E.ENVFUN _) tvl state bstr = raise EH.DeadBranch "this should have been built by now"
  | freshenv (E.ENVCST _) tvl state bstr = raise EH.DeadBranch "this should have been built by now"
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

fun buildtnty (T.NV tnv) state =
    (case S.getValStateTn state tnv of
	 NONE => T.NV tnv
       | SOME tn => buildtnty tn state)
  | buildtnty (T.NC (tn, b, l)) _ = T.NC (tn, b, l)
  | buildtnty (T.ND (tn, labs, stts, deps)) state =
    T.ND (buildtnty tn state, labs, stts, deps)

fun buildlabty (T.LV lv) state =
    (case S.getValStateLt state lv of
	 NONE => T.LV lv
       | SOME lt => buildlabty lt state)
  | buildlabty (T.LC (lc, l)) _ = T.LC (lc, l)
  | buildlabty (T.LD (lt, labs, stts, deps)) state =
    T.LD (buildlabty lt state, labs, stts, deps)


(* - bmon is true if we want to build the monomorphic type variables - Is it still used?
 * - monfun is true if we want to turn all the variables into monomorphic ones.
 *   This is used when a function turns to be monomoprhic.
 *   false is the default value. *)
fun buildty (T.V (tv, b, p)) state dom bmon monfun =
    (case (S.getValStateGe state tv, bmon) of
	 (SOME (_, labs, sts, asmp), false) => T.TD (T.V (tv, b, T.MONO), labs, sts, asmp)
       (*(2010-08-18)This should not return labs but put it on the type with T.DEP.*)
       | (x, _) => (case S.getValStateTv state tv of
			NONE =>
			let val poly = (*p*)(*N*)if Option.isSome x then p else T.POLY
			    (* If we choose p then the variable keeps the same status and so
			     * in 535 for example, we won't generalise foo's type and we will
			     * get a type error, if we choose the other solution then in 530
			     * for example, we won't find the error because when typing field2
			     * we will generalise its type.  We should do the second one and
			     * in StateEnv when pushing an environment we should go down the
			     * structures as well.  This is the proper way to do it. *)
			    val poly = (*N*)if monfun then T.MONO else poly
			    val tv   = T.V (tv, b, poly)
			in case x of
			       NONE => tv
			     | SOME (_, labs, stts, deps) => T.TD (tv, labs, stts, deps)
			end
		      | SOME ty =>
			let val ty' = buildty ty state dom bmon monfun
			in case x of
			       NONE => ty'
			     | SOME (_, labs, stts, deps) => collapseTy ty' labs stts deps
			end))
  | buildty (T.E (n, tv, lab)) state dom bmon monfun =
    (case (S.getValStateGe state tv, I.isin n dom) of
	 (NONE, true) => T.V (tv, SOME (n, lab), T.POLY)
       | (NONE, false) => T.E (n, tv, lab)
       | (SOME (_, labs, stts, deps), _) => T.TD (T.E (n, tv, lab), labs, stts, deps))
  (*(case (S.getValStateGe state tv, bmon) of
	 (SOME (_, labs, stts, deps), false) => (D.printdebug2 ("(1)"); (T.E (n, tv, lab), labs, stts, deps))
       | _ => (D.printdebug2 ("(2)"); (T.V (tv, SOME lab, T.POLY), L.empty, L.empty, CD.empty)))*)
  | buildty (T.C (tn, sq, l)) state dom bmon monfun =
    let val tn' = buildtnty  tn state
	val sq' = buildseqty sq state dom bmon monfun
    in T.C (tn', sq', l)
    end
  | buildty (T.A (tyfun, seqty, lab)) state dom bmon monfun =
    let val tyfun' = buildtyfun tyfun state dom bmon monfun
	val seqty' = buildseqty seqty state dom bmon monfun
    in T.A (tyfun', seqty', lab)
    end
  | buildty (T.OR (sq, i, p, k, l)) state dom bmon monfun =
    let val sq' = buildseqty sq state dom bmon monfun
    in T.OR (sq', i, p, k, l)
    end
  | buildty (T.GEN tys) state dom bmon monfun =
    let val tys = map (fn ty => buildty ty state dom bmon monfun) (!tys)
    in T.GEN (ref tys)
    end
  (*(2010-06-23)We open a GEN when we build it when building an environment.
   * It should only be opened when building an environment. *)
  | buildty (T.TD (ty, labs, stts, deps)) state dom bmon monfun =
    let val ty' = buildty ty state dom bmon monfun
    in collapseTy ty' labs stts deps
    end
and buildseqty (T.SV sv) state dom bmon monfun =
    (case S.getValStateSq state sv of
	 NONE => T.SV sv
       | SOME sq => buildseqty sq state dom bmon monfun)
  | buildseqty (T.SC (rtl, flex, l)) state dom bmon monfun =
    let val rtl' = map (fn rt => buildrowty rt state dom bmon monfun) rtl
    in T.SC (rtl', flex, l)
    end
  | buildseqty (T.SD (sq, labs, stts, deps)) state dom bmon monfun =
    let val sq' = buildseqty sq state dom bmon monfun
    in collapseSq sq' labs stts deps
    end
and buildrowty (T.RV rv) state dom bmon monfun =
    (case S.getValStateRt state rv of
	 NONE => T.RV rv
       | SOME rt => buildrowty rt state dom bmon monfun)
  | buildrowty (T.RC (lt, ty, l)) state dom bmon monfun =
    let val lt' = buildlabty lt state
	val ty' = buildty    ty state dom bmon monfun
    in T.RC (lt', ty', l)
    end
  | buildrowty (T.RD (row, labs, stts, deps)) state dom bmon monfun =
    let val row' = buildrowty row state dom bmon monfun
    in collapseRt row' labs stts deps
    end
  | buildrowty (x as T.RO) _ _ _ _ = x
and buildtyfun (T.TFV tfv) state dom bmon monfun =
    (case S.getValStateTf state tfv of
	 NONE => T.TFV tfv
       | SOME tf => buildtyfun tf state dom bmon monfun)
  | buildtyfun (T.TFC (seqty, ty, lab)) state dom bmon monfun =
    let val seqty' = buildseqty seqty state dom bmon monfun
	val ty'    = buildty    ty    state dom bmon monfun
    in T.TFC (seqty', ty', lab)
    end
  | buildtyfun (T.TFD (tf, labs, stts, deps)) state dom bmon monfun =
    let val tf' = buildtyfun tf state dom bmon monfun
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
		E.emgen
		idenv

fun buildnobuild x _ _ _ _ _ = x

val buildTyVar = buildnobuild

fun freshtyvar (tyvar, b) tvl fresh bstr = (F.freshTyVar tyvar fresh, b)

fun buildTy' ty state fresh dom bmon bstr = buildty ty state dom bmon false

fun buildSeqTy' seqty state fresh dom bmon bstr = buildseqty seqty state dom bmon false

fun buildTypSem (typfun, tnKind, cons) state fresh dom bmon bstr =
    let val typfun' = buildtyfun typfun state dom bmon false
    in (typfun', tnKind, cons)
    end

fun freshTypSem (typfun, tnKind, cons) tvl fresh bstr =
    (freshtyfun typfun tvl fresh bstr, tnKind, cons)

fun freshEnv env tvl fresh bstr = env

val buildFuns = buildnobuild

fun freshFuns funs tvl fresh bstr = funs

fun buildEnv (E.ENVVAR (ev, lab)) state fresh bstr =
    (case S.getValStateEv state ev of
	 NONE => E.ENVVAR (ev, lab)
       | SOME env =>
	 let val env' = buildEnv env state fresh bstr
	 (*(2010-06-22)We can't just push into env' in case env' is, say, an empty structure.
	  * We also need to gather these. *)
	 in env'
	 end)
  | buildEnv (E.ENVSEQ (env1, env2)) state fresh bstr =
    let val env1 = buildEnv env1 state fresh bstr
	val env2 = buildEnv env2 state fresh bstr
    in E.ENVSEQ (env1, env2)
    end
  | buildEnv (env as E.ENVCON _) state fresh bstr =
    E.consEnvC (buildIdEnv (E.getVids env) state fresh freshty     buildTy'    bstr)
	       (buildIdEnv (E.getTyps env) state fresh freshTypSem buildTypSem false)
	       (buildIdEnv (E.getTyvs env) state fresh freshtyvar  buildTyVar  bstr)
	       (buildIdEnv (E.getStrs env) state fresh freshEnv    buildEnv'   bstr)
	       (buildIdEnv (E.getSigs env) state fresh freshEnv    buildEnv'   bstr)
	       (buildIdEnv (E.getFuns env) state fresh freshFuns   buildFuns   bstr)
	       (buildIdEnv (E.getOvcs env) state fresh freshseqty  buildSeqTy' bstr)
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
	fun searchTy (T.V _) = NONE
	  | searchTy (T.E (id, tv, lab)) =
	    if I.isin id dom
	    then SOME (id, lab, getLabsTyvs id, L.empty, CD.empty)
	    else NONE
	  | searchTy (T.C (_, seqty, _)) = searchSeqTy seqty
	  | searchTy (T.A (tyfun, seqty, lab)) =
	    let val err = searchTyFun tyfun
	    in if Option.isSome err
	       then err
	       else searchSeqTy seqty
	    end
	  | searchTy (T.OR (seqty, _, _, _, _)) = searchSeqTy seqty
	  | searchTy (T.GEN ty) = raise EH.TODO (*(2010-06-23)Should be impossible.*)
	  | searchTy (T.TD (ty, labs, stts, deps)) =
	    (case searchTy ty of
		 NONE => NONE
	       | SOME (id, lab, labs', stts', deps') =>
		 SOME (id, lab, L.union labs labs', L.union stts stts', CD.union deps deps'))
	and searchSeqTy (T.SV _) = NONE
	  | searchSeqTy (T.SC (rows, _, _)) =
	    foldr (fn (row, err) =>
		      if Option.isSome err
		      then err
		      else searchRow row)
		  NONE
		  rows
	  | searchSeqTy (T.SD (seq, labs, stts, deps)) =
	    (case searchSeqTy seq of
		 NONE => NONE
	       | SOME (id, lab, labs', stts', deps') =>
		 SOME (id, lab, L.union labs labs', L.union stts stts', CD.union deps deps'))
	and searchRow (T.RV _) = NONE
	  | searchRow (T.RC (_, ty, _)) = searchTy ty
	  | searchRow (T.RD (row, labs, stts, deps)) =
	    (case searchRow row of
		 NONE => NONE
	       | SOME (id, lab, labs', stts', deps') =>
		 SOME (id, lab, L.union labs labs', L.union stts stts', CD.union deps deps'))
	  | searchRow T.RO = NONE
	and searchTyFun (T.TFV _) = NONE
	  | searchTyFun (T.TFC (seqty, ty, _)) =
	    let val err = searchSeqTy seqty
	    in if Option.isSome err
	       then err
	       else searchTy ty
	    end
	  | searchTyFun (T.TFD (tf, labs, stts, deps)) =
	    (case searchTyFun tf of
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




(*fun checkOneDatConsInDatCons _ [] = false
  | checkOneDatConsInDatCons (x as (id, _, _, _, _)) ((id', _, _, _, _) :: xs) =
    I.eqId id id' orelse checkOneDatConsInDatCons x xs

fun checkDatConsInDatCons cons1 cons2 labs deps asmp l ek =
    foldr (fn (x as (id, lab, _, _, _), _) => (*(2010-04-21)Shouldn't we check the other lab here?*)
	      if not (O.isin lab labs) orelse checkOneDatConsInDatCons x cons2
	      then ()
	      else let val fst = (lab, I.toInt id)
		       val snd = map (fn (id, l, _, _, _) => (l, I.toInt id)) cons2
		       val err = ek (fst, snd)
		   in raise misscons (ERR.consPreError ERR.dummyId labs asmp err deps l)
		   end)
	  ()
	  cons1

fun checkDatSigStruc es etv labs deps asmp l id =
    let val lab1  = E.getBindL etv
	val cl1   = E.getBindC etv
	val cons1 = CL.getClassDATconsC cl1
    in if O.isin lab1 labs (* do we need that? *)
	  andalso not (CL.isClassDATem cl1) (* etv (in the signature) is for a datatype and not a type function *)
       then app (fn e =>
		    let val lab2  = E.getBindL e
			val cl2   = E.getBindC e
			val cons2 = CL.getClassDATconsC cl2
		    in if O.isin lab2 labs
		       then if CL.isClassDATem cl2 (* e is for a type function (in the structure) *)
			    then raise dattyp (ERR.consPreError ERR.dummyId
								labs
								asmp
								(EK.DatTypClash (I.toInt id, lab1, lab2))
								deps
								l)
			    else (if O.subseteq (CL.getClassDATlabs cl2) labs
				  then checkDatConsInDatCons cons1 cons2 labs deps asmp l EK.MissConsStr
				  else ();
				  if O.subseteq (CL.getClassDATlabs cl1) labs
				  then checkDatConsInDatCons cons2 cons1 labs deps asmp l EK.MissConsSig
				  else ())
		       else ()
		    end)
		  es
       else ()
    end*)


fun getGenTyvars (T.V (tv, SOME idl, p)) = [idl]
  | getGenTyvars (T.V _) = []
  | getGenTyvars (T.E _) = []
  | getGenTyvars (T.C (_, seq, _)) = getGenTyvarsSeq seq
  | getGenTyvars (T.A _) = []
  | getGenTyvars (T.OR (seq, _, _, _, _)) = getGenTyvarsSeq seq
  | getGenTyvars (T.GEN tys) =
    foldr (fn (ty, tvlabs) => (getGenTyvars ty) @ tvlabs) [] (!tys)
  | getGenTyvars (T.TD ety) = getGenTyvars (EL.getExtLabT ety)

and getGenTyvarsSeq (T.SV _) = []
  | getGenTyvarsSeq (T.SC (rows, _, _)) =
    foldr (fn (row, tvlabs) => (getGenTyvarsRow row) @ tvlabs) [] rows
  | getGenTyvarsSeq (T.SD eseq) = getGenTyvarsSeq (EL.getExtLabT eseq)

and getGenTyvarsRow (T.RV _) = []
  | getGenTyvarsRow (T.RC (_, ty, _)) = getGenTyvars ty
  | getGenTyvarsRow (T.RD erow) = getGenTyvarsRow (EL.getExtLabT erow)
  | getGenTyvarsRow T.RO = []


fun extractTyGen (T.GEN _)   true  = [T.newV ()]
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
		    val err  = ERR.consPreError ERR.dummyId (L.cons lab labs) deps ek stts L.dummyLab
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

fun idInEnv (env as E.ENVCON _) lab id =
    let val lab = L.fromInt lab
	val id  = I.fromInt id
    in idInIdEnv (E.getVids env) lab id orelse
       idInIdEnv (E.getTyps env) lab id orelse
       idInIdEnv (E.getStrs env) lab id orelse
       idInIdEnv (E.getSigs env) lab id orelse (* We don't need this one *)
       idInIdEnv (E.getFuns env) lab id orelse
       idInIdEnv (E.getOvcs env) lab id        (* We don't need this one *)
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
		 in raise errorfound (ERR.consPreError ERR.dummyId labs2 deps ek stts l)
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
	    map (fn etv => compone id (E.plusproj (E.getVids env) id) etv) semty
	fun compT semty tn env = (* env: structure, etv: signature *)
	    map (fn etv =>
		    (()(*checkDatSigStruc (E.plusproj (E.getTyps env) tn) etv labs deps asmp l tn*);
		     componeTyp tn (E.plusproj (E.getTyps env) tn) etv))
		semty
	  (* We should also check the NONs, because these are errors.
	   * SML/NJ says: Error: type t must be a datatype.
	   * --> checkDatSigTypStruc *)
	  (* We should aslo check that all the constructors are declared along with the datatype in the signature!
	   * It seems to be the only case when we have something in the structure that have to be in the signature.
	   * --> checkAllConsInSig *)
	fun compS semty id env =
	    map (fn exv => componestr id (E.plusproj (E.getStrs env) id) exv) semty
	fun compGen idenv fcomp env =
	    E.foldrienv (fn (id, semty, cs) => (fcomp semty id env) @ cs) [] idenv
	fun linkstr env1 env2 =
	    let (* is there something in 'getVids env1' ?*)
		val csG = compGen (E.getVids env1) compG env2
		val csT = compGen (E.getTyps env1) compT env2
		val csS = compGen (E.getStrs env1) compS env2
		val (csSV, csST) = ListPair.unzip csS
		(*val _ = D.printdebug2 (E.printEnv (E.ENVCST (E.singcsts (L.dummyLab, List.concat csST))) "")*)
	    in (List.concat (csG @ csSV), List.concat (csT @ csST))
	    end
    in case (env1, env2) of (* env1/env2 : signature/structure *)
	   (E.ENVCON _, E.ENVCON _) => linkstr env1 env2
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
fun mergeTyFun tfn1 tfn2 =
    OM.unionWith (fn (tf1, tf2 as T.TFC ((*T.SC*) _, _, _)) => ((*D.printdebug2 (T.printtyf tf1 ^ "\n" ^ T.printtyf tf2);*) tf2)
		   | (tf1, tf2) => ((*D.printdebug2 (T.printtyf tf1 ^ "\n" ^ T.printtyf tf2);*) tf2))
		 (*(2010-07-06)This is to fix, we don't want to just keep the second one!*)
		 (tfn1, tfn2)

fun decorateTyFun tfn labs stts deps =
    OM.map (fn tf => collapseTf tf labs stts deps)
	   tfn

fun newTyFun () = T.TFC (T.newSV (), T.newV (), L.dummyLab)

(*fun insertInTyFun tyfun name (exttf as (tf, labs, stts, deps)) =
    let val tn = T.tynameToInt name
    in case OM.find (tyfun, tn) of
	   NONE => OM.insert (tyfun, tn, exttf)
	 | SOME (tf', labs', stts', deps') =>
	   OM.insert (tyfun, tn, (tf, L.union labs labs', L.union stts stts', CD.union deps deps'))
    end*)

fun insertInTyFun tyfun name tf =
    let val tn = T.tynameToInt name
    (*What about when we already have a tn entry in tyfun because of
     *some sharing?*)
    in OM.insert (tyfun, tn, tf)
    end

fun getAllTyFunEnv (env as E.ENVCON _) =
    let val (tfnDs1, tfnTs1) =
	    foldr (fn ({id, lab, kind, name}, (tfnDs, tfnTs)) =>
		      case kind of
			  E.DAT => (insertInTyFun tfnDs name (newTyFun ()), tfnTs)
			| E.TYP => (tfnDs, insertInTyFun tfnTs name (newTyFun ())))
		  (OM.empty, OM.empty)
		  (E.getITns env)
	val (tfnDs2, tfnTs2) = getAllTyFunStrEnv (E.getStrs env)
    in (mergeTyFun tfnDs1 tfnDs2, mergeTyFun tfnTs1 tfnTs2)
    end
  | getAllTyFunEnv (E.ENVSEQ (env1, env2)) =
    let val (tfnDs1, tfnTs1) = getAllTyFunEnv env1
	val (tfnDs2, tfnTs2) = getAllTyFunEnv env2
    in (mergeTyFun tfnDs1 tfnDs2, mergeTyFun tfnTs1 tfnTs2)
    end
  | getAllTyFunEnv (E.ENVDEP (env, labs, stts, deps)) =
    let val (tfnDs, tfnTs) = getAllTyFunEnv env
    in (decorateTyFun tfnDs labs stts deps, decorateTyFun tfnTs labs stts deps)
    end
  | getAllTyFunEnv (E.ENVVAR _) = (OM.empty, OM.empty)
  | getAllTyFunEnv (E.ENVTOP)   = (OM.empty, OM.empty)
  | getAllTyFunEnv (E.ENVOPN _) = raise EH.DeadBranch "This should have been built by now"
  | getAllTyFunEnv (E.ENVFUN _) = raise EH.DeadBranch "This should have been built by now"
  | getAllTyFunEnv (E.ENVCST _) = raise EH.DeadBranch "This should have been built by now"
  | getAllTyFunEnv (E.ENVPOL _) = raise EH.DeadBranch "This should have been built by now"
  | getAllTyFunEnv (E.ENVDAT _) = raise EH.DeadBranch "This should have been built by now"
  | getAllTyFunEnv (E.ENVLOC _) = raise EH.DeadBranch "This should have been built by now"
  | getAllTyFunEnv (E.ENVWHR _) = raise EH.DeadBranch "This should have been built by now"
  | getAllTyFunEnv (E.ENVSHA _) = raise EH.DeadBranch "This should have been built by now"
  | getAllTyFunEnv (E.ENVSIG _) = raise EH.DeadBranch "This should have been built by now"
  | getAllTyFunEnv (E.ENVPTY _) = raise EH.DeadBranch "This should have been built by now"
  | getAllTyFunEnv (E.ENVFIL _) = raise EH.DeadBranch "This should have been built by now"

and getAllTyFunStrEnv strenv =
    E.foldrienv
	(fn (id, semty, (tfnDs, tfnTs)) =>
	    foldr (fn (extenv, (tfnDs, tfnTs)) =>
		      let val (tfnDs', tfnTs') = getAllTyFunEnv (E.getBindT extenv)
		      in (mergeTyFun tfnDs tfnDs', mergeTyFun tfnTs tfnTs')
		      end)
		  (tfnDs, tfnTs)
		  semty)
	(OM.empty, OM.empty)
	strenv

fun getTyFunEnv (env1 as E.ENVCON _) (env2 as E.ENVCON _) labs stts deps =
    let val (tfnDs1, tfnTs1) =
	    foldr (fn ({id, lab, kind, name}, (tfnDs, tfnTs)) =>
		      case E.plusproj (E.getTyps env2) id of
			  [] => if E.getICmp env2 (* The structure/realisation is complete. *)
				then (tfnDs, tfnTs) (* not in the incomplete structure/realisation so we don't want to generalise *)
				else (case kind of
					  E.DAT => (insertInTyFun tfnDs name (newTyFun ()), tfnTs)
					| E.TYP => (tfnDs, insertInTyFun tfnTs name (newTyFun ())))
			| [etv] =>
			  let val ty = case E.getBindT etv of
					   (tf, _, _) => (* What do we need the T.SC for? *)
					   (case collapseTf tf labs stts deps of
						T.TFD (T.TFC ((*T.SC*) _, _, _), _, _, _) =>
						collapseTf tf (EL.getExtLabL etv) (EL.getExtLabE etv) (EL.getExtLabD etv)
					      | _ => newTyFun ())
			  (* Here we put the new mapping in tfnT because we've got a match
			   * in the structure/realisation and so the even if name comes from
			   * a datatype specification, we want to rename it with what we got
			   * in the structure/realisation*)
			  in (tfnDs, insertInTyFun tfnTs name ty)
			  end
			| _ => raise EH.DeadBranch "There should be only one binding per identifier during constraint solving")
		  (OM.empty, OM.empty)
		  (E.getITns env1)
	val (tfnDs2, tfnTs2) = getTyFunStrEnv (E.getStrs env1) (E.getStrs env2)
	(*val _ = D.printdebug2 (printTFun tfnTs1 ^ "\n" ^ printTFun tfnTs2)*)
    in (mergeTyFun tfnDs1 tfnDs2, mergeTyFun tfnTs1 tfnTs2)
    end
  | getTyFunEnv (E.ENVDEP (env1, labs', stts', deps')) env2 labs stts deps =
    getTyFunEnv env1
		env2
		(L.union labs labs')
		(L.union stts stts')
		(CD.union deps deps')
  | getTyFunEnv env1 (E.ENVDEP (env2, labs', stts', deps')) labs stts deps =
    getTyFunEnv env1
		env2
		(L.union labs labs')
		(L.union stts stts')
		(CD.union deps deps')
  | getTyFunEnv (env as E.ENVCON _) _ _ _ _ = getAllTyFunEnv env
  | getTyFunEnv (E.ENVSEQ (env0, env1)) env2 labs stts deps =
    let val (tfnDs1, tfnTs1) = getTyFunEnv env0 env2 labs stts deps
	val (tfnDs2, tfnTs2) = getTyFunEnv env1 env2 labs stts deps
    in (mergeTyFun tfnDs1 tfnDs2, mergeTyFun tfnTs1 tfnTs2)
    end
  | getTyFunEnv (E.ENVVAR _) _ _ _ _ = (OM.empty, OM.empty)
  | getTyFunEnv (E.ENVTOP)   _ _ _ _ = (OM.empty, OM.empty)
  | getTyFunEnv (E.ENVOPN _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | getTyFunEnv (E.ENVFUN _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | getTyFunEnv (E.ENVCST _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | getTyFunEnv (E.ENVPOL _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | getTyFunEnv (E.ENVDAT _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | getTyFunEnv (E.ENVLOC _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | getTyFunEnv (E.ENVWHR _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | getTyFunEnv (E.ENVSHA _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | getTyFunEnv (E.ENVSIG _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | getTyFunEnv (E.ENVPTY _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | getTyFunEnv (E.ENVFIL _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"

and getTyFunExtEnv extenv extenv' =
    let (*val labs = L.union  (EL.getExtLabL extenv) (EL.getExtLabL extenv')
	val stts = L.union  (EL.getExtLabE extenv) (EL.getExtLabE extenv')
	val deps = CD.union (EL.getExtLabD extenv) (EL.getExtLabD extenv')*)
	val labs = EL.getExtLabL extenv'
	val stts = EL.getExtLabE extenv'
	val deps = EL.getExtLabD extenv'
	val (tfnD, tfnT)  = getTyFunEnv (E.getBindT extenv) (E.getBindT extenv') labs stts deps
    in (tfnD, tfnT)
    end

and getTyFunStrEnv strenv strenv' =
    E.foldrienv
	(fn (id, semty, (tfnDs, tfnTs)) =>
	    foldr (fn (extenv, tfns) =>
		      foldr (fn (extenv', (tfnDs, tfnTs)) =>
				let val (tfnDs', tfnTs') = getTyFunExtEnv extenv extenv'
				in (mergeTyFun tfnDs tfnDs', mergeTyFun tfnTs tfnTs')
				end)
			    (tfnDs, tfnTs)
			    (E.plusproj strenv' id))
		  (tfnDs, tfnTs)
		  semty)
	(OM.empty, OM.empty)
	strenv




fun mergeTyFunSha tfn1 tfn2 =
    OM.unionWith (fn (_, x as SOME _) => x
		   | (x, y) => x)
		 (tfn1, tfn2)

fun decorateTyFunSha tfn labs stts deps =
    OM.map (fn (SOME (labs', stts', deps')) =>
	       SOME (L.union  labs labs',
		     L.union  stts stts',
		     CD.union deps deps')
	     | NONE => NONE)
	   tfn

fun decorateUTyFunSha (SOME utf) labs stts deps = SOME (EL.updExtLab utf labs stts deps)
  | decorateUTyFunSha NONE       _    _    _    = NONE

(* TODO: we should extract the dependencies from both utf1 and utf2 and put them on x using collapseTf. *)
fun mergeUTyFunSha (SOME utf1) (SOME utf2) = SOME (EL.unionExtLab utf1 utf2 (fn (x, y) => x))
  | mergeUTyFunSha (SOME utf)  NONE        = SOME utf
  | mergeUTyFunSha NONE        (SOME utf)  = SOME utf
  | mergeUTyFunSha NONE        NONE        = NONE

fun getTyFunEnvSha (env1 as E.ENVCON _) (env2 as E.ENVCON _) =
    let val (utfT, tfnT) =
	    foldr (fn ({id, lab, kind, name}, (utf, tfns)) =>
		      case E.plusproj (E.getTyps env2) id of
			  [] => if E.getICmp env2 (* The sharing structure is complete. *)
				then (utf, tfns)  (* not in the incomplete sharing structure so we don't want to generalise *)
				else (utf, OM.insert (tfns, T.tynameToInt name, NONE))
			| [etv] =>
			  if CL.classIsANY (E.getBindC etv)
			  then (utf, OM.insert (tfns, T.tynameToInt name, NONE))
			  else (case E.plusproj (E.getTyps env1) id of
				    [] => (utf, OM.insert (tfns, T.tynameToInt name, NONE))
				  | [etv'] => (case E.getBindT etv' of
						   (tf, _, _) =>
						   (case T.getNameTyName tf of
							SOME _ =>
							let val labs  = L.union  (EL.getExtLabL etv) (EL.getExtLabL etv')
							    val stts  = L.union  (EL.getExtLabE etv) (EL.getExtLabE etv')
							    val deps  = CD.union (EL.getExtLabD etv) (EL.getExtLabD etv')
							    val utf'  = SOME (tf, labs, stts, deps)
							    val v     = SOME (labs, stts, deps)
							    val tfns' = OM.insert (tfns, T.tynameToInt name, v)
							in (mergeUTyFunSha utf utf', tfns')
							end
						      | NONE => (utf, OM.insert (tfns, T.tynameToInt name, NONE))))
				  | _  => raise EH.DeadBranch "There should be only one binding per identifier during constraint solving")
			| _ => raise EH.DeadBranch "There should be only one binding per identifier during constraint solving")
		  (NONE, OM.empty)
		  (E.getITns env1)
	val (utfS, tfnS) = getTyFunShaStrEnv (E.getStrs env1) (E.getStrs env2)
    in (mergeUTyFunSha utfT utfS, mergeTyFunSha tfnT tfnS)
    end
  | getTyFunEnvSha (E.ENVDEP (env1, labs, stts, deps)) env2 =
    let val (utf, tfn) = getTyFunEnvSha env1 env2
    in (decorateUTyFunSha utf labs stts deps,
	decorateTyFunSha  tfn labs stts deps)
    end
  | getTyFunEnvSha env1 (E.ENVDEP (env2, labs, stts, deps)) =
    let val (utf, tfn) = getTyFunEnvSha env1 env2
    in (decorateUTyFunSha utf labs stts deps,
	decorateTyFunSha  tfn labs stts deps)
    end
  | getTyFunEnvSha (env as E.ENVCON _) _ =
    let val (tfnD, tfnT) = getAllTyFunEnv env
	val tfn1 = mergeTyFun tfnD tfnT
	val tfn2 = OM.map (fn _ => NONE) tfn1
    in (NONE, tfn2)
    end
  | getTyFunEnvSha (E.ENVSEQ (env0, env1)) env2 =
    let val (utf1, tfn1) = getTyFunEnvSha env0 env2
	val (utf2, tfn2) = getTyFunEnvSha env1 env2
    in (mergeUTyFunSha utf1 utf2, mergeTyFunSha tfn1 tfn2)
    end
  | getTyFunEnvSha (E.ENVVAR _) _ = (NONE, OM.empty)
  | getTyFunEnvSha (E.ENVTOP)   _ = (NONE, OM.empty)
  | getTyFunEnvSha (E.ENVOPN _) _ = raise EH.DeadBranch "This should have been built by now"
  | getTyFunEnvSha (E.ENVFUN _) _ = raise EH.DeadBranch "This should have been built by now"
  | getTyFunEnvSha (E.ENVCST _) _ = raise EH.DeadBranch "This should have been built by now"
  | getTyFunEnvSha (E.ENVPOL _) _ = raise EH.DeadBranch "This should have been built by now"
  | getTyFunEnvSha (E.ENVDAT _) _ = raise EH.DeadBranch "This should have been built by now"
  | getTyFunEnvSha (E.ENVLOC _) _ = raise EH.DeadBranch "This should have been built by now"
  | getTyFunEnvSha (E.ENVWHR _) _ = raise EH.DeadBranch "This should have been built by now"
  | getTyFunEnvSha (E.ENVSHA _) _ = raise EH.DeadBranch "This should have been built by now"
  | getTyFunEnvSha (E.ENVSIG _) _ = raise EH.DeadBranch "This should have been built by now"
  | getTyFunEnvSha (E.ENVPTY _) _ = raise EH.DeadBranch "This should have been built by now"
  | getTyFunEnvSha (E.ENVFIL _) _ = raise EH.DeadBranch "This should have been built by now"

and getTyFunShaExtEnv extenv extenv' =
    let (*val labs = EL.getExtLabL extenv'
	val stts = EL.getExtLabE extenv'
	val deps = EL.getExtLabD extenv'*)
	val labs = L.union  (EL.getExtLabL extenv) (EL.getExtLabL extenv')
	val stts = L.union  (EL.getExtLabE extenv) (EL.getExtLabE extenv')
	val deps = CD.union (EL.getExtLabD extenv) (EL.getExtLabD extenv')
	val (utf, tfns) = getTyFunEnvSha (E.getBindT extenv) (E.getBindT extenv')
    in (decorateUTyFunSha utf labs stts deps, decorateTyFunSha tfns labs stts deps)
    end

and getTyFunShaStrEnv strenv strenv' =
    E.foldrienv
	(fn (id, semty, (utf, tfns)) =>
	    foldr (fn (extenv, (utf, tfns)) =>
		      foldr (fn (extenv', (utf, tfns)) =>
				let val (utf', tfns') = getTyFunShaExtEnv extenv extenv'
				in (mergeUTyFunSha utf utf', mergeTyFunSha tfns tfns')
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
fun genTyFunTy (x as T.V _) _ _ = ([], x)
  | genTyFunTy (x as T.E _) _ _ = ([], x (*T.V (T.freshtyvar ())*))
  | genTyFunTy (x as T.C (tnc, sq, l)) tfun btyp =
    (case collapseTn tnc L.empty L.empty CD.empty of
	 T.ND (T.NC (tn, _, _), labs, stts, deps) =>
	 (case OM.find (tfun, T.tynameToInt tn) of
	      NONE =>
	      let val (cs, sq') = genTyFunSeqTy sq tfun btyp
	      in (cs, T.C (tnc, sq', l)) end
	    | SOME tyf =>
	      (case collapseTf (freshTyFun tyf btyp) labs stts deps of
		   T.TFD (T.TFC (sq', ty', l'), labs, stts, deps) =>
		   let val (cs, s) = genTyFunSeqTy sq tfun btyp
		       val v  = T.newV ()
		       val c1 = E.genCstSqAll s sq' labs stts deps (* the labels and context dependencies are not correct *)
		       val c2 = E.genCstTyAll v ty' labs stts deps
		   (*val _ = D.printdebug2 ("foo")*)
		   in (c1 :: c2 :: cs, v)
		   end
		 | _ => raise EH.DeadBranch "not a valid type function"))
       | _ =>
	 let val (cs, sq') = genTyFunSeqTy sq tfun btyp
	 in (cs, T.C (tnc, sq', l))
	 end)
  | genTyFunTy (T.A (tf, sq, l)) tfun btyp =
    let val (cs1, tf') = genTyFunFunTy tf tfun btyp
	val (cs2, sq') = genTyFunSeqTy sq tfun btyp
    in (cs1 @ cs2, T.A (tf', sq', l))
    end
  | genTyFunTy (T.OR (sq, i, p, k, l)) tfun btyp =
    let val (cs, sq') = genTyFunSeqTy sq tfun btyp
    in (cs, T.OR (sq', i, p, k, l))
    end
  | genTyFunTy (T.GEN tys) tfun btyp =
    let val (cs, tys) = ListPair.unzip (map (fn ty => genTyFunTy ty tfun btyp) (!tys))
    in (List.concat cs, T.GEN (ref tys))
    end
  | genTyFunTy (T.TD (ty, labs, stts, deps)) tfun btyp =
    let val (cs, ty') = genTyFunTy ty tfun btyp
    in (cs, collapseTy ty' labs stts deps)
    end
and genTyFunFunTy (x as T.TFV _) _ _ = ([], x)
  | genTyFunFunTy (T.TFC (sq, ty, l)) tfun btyp =
    let val (cs1, sq') = genTyFunSeqTy sq tfun btyp
	val (cs2, ty') = genTyFunTy    ty tfun btyp
    in (cs1 @ cs2, T.TFC (sq', ty', l))
    end
  | genTyFunFunTy (T.TFD (tf, labs, stts, deps)) tfun btyp =
    let val (cs, tf') = genTyFunFunTy tf tfun btyp
    in (cs, collapseTf tf' labs stts deps)
    end
and genTyFunSeqTy (x as T.SV _) _ _ = ([], x)
  | genTyFunSeqTy (T.SC (rtl, flex, l)) state btyp =
    let val (cs, rtl') = ListPair.unzip (map (fn rt => genTyFunRowTy rt state btyp) rtl)
    in (List.concat cs, T.SC (rtl', flex, l))
    end
  | genTyFunSeqTy (T.SD (sq, labs, stts, deps)) tfun btyp =
    let val (cs, sq') = genTyFunSeqTy sq tfun btyp
    in (cs, collapseSq sq' labs stts deps)
    end
and genTyFunRowTy (x as T.RV _) _ _ = ([], x)
  | genTyFunRowTy (T.RC (lt, ty, l)) tfun btyp =
    let val (cs, ty') = genTyFunTy ty tfun btyp
    in (cs, T.RC (lt, ty', l))
    end
  | genTyFunRowTy (T.RD (row, labs, stts, deps)) tfun btyp =
    let val (cs, row') = genTyFunRowTy row tfun btyp
    in (cs, collapseRt row' labs stts deps)
    end
  | genTyFunRowTy (x as T.RO) _ _  = ([], x)

fun genTyFunExtGen bind tfun f btyp =
    let val sem = E.getBindT bind
	val (cs, sem') = f sem tfun btyp
	val cs' = decorateCst cs (EL.getExtLabL bind) (EL.getExtLabE bind) (EL.getExtLabD bind)
	val bind' = EL.mapExtLab bind (fn x => C.mapBind x (fn _ => sem'))
    in (cs', bind')
    end

fun genTyFunFunTy' (tyfun, tnKind, cons) tfun btyp =
    let val (cs, tyfun') = genTyFunFunTy tyfun tfun btyp
    in (cs, (tyfun', tnKind, cons))
    end

fun genTyFunExtTy    x _ tfun _ _ = genTyFunExtGen x tfun genTyFunTy     true
fun genTyFunExtFunTy x _ tfun _ _ = genTyFunExtGen x tfun genTyFunFunTy' false
fun genTyFunExtSeqTy x _ tfun _ _ = genTyFunExtGen x tfun genTyFunSeqTy  true

fun genTyFunGenEnv genenv state tfun dom b genfun =
    E.foldrienv
	(fn (id, semty, (cs, genenv)) =>
	    let val xs = map (fn x => genfun x state tfun dom b) semty
		val (cs', semty') = ListPair.unzip xs
	    in (cs @ (List.concat cs'),	E.addenv (id, semty') genenv)
	    end)
	([], E.emgen)
	genenv

(* This is actually used for matchings structure/signature and for sharing *)
fun genTyFunEnv (env as E.ENVCON _) state tfun dom b =
    let val tns =
	    if b
	    (*then List.filter (fn {id, kind, name} => not (O.isin (T.tynameToInt name) dom)) (E.getITns env)*)
	    then List.mapPartial (fn x as {id, lab, kind, name} =>
				     case OM.find (tfun, T.tynameToInt name) of
					 SOME tf =>
					 (case T.isTyName tf of
					      (T.TYNAME name', _, _, _) =>
					      SOME {id = id, lab = lab, kind = kind, name = name'}
					    | _ => NONE)
				       | NONE => SOME x)
				 (E.getITns env)
	    else map (fn {id, lab, kind, name} =>
			 case Option.join (Option.map T.getNameTyName (OM.find (tfun, T.tynameToInt name))) of
			     SOME name' => {id = id, lab = lab, kind = kind, name = name'}
			   | _ => {id = id, lab = lab, kind = kind, name = name})
		     (E.getITns env)
	val (csVids, vids) = genTyFunGenEnv (E.getVids env) state tfun dom b genTyFunExtTy
	val (csTyps, typs) = genTyFunGenEnv (E.getTyps env) state tfun dom b genTyFunExtFunTy
	val tyvs           = E.getTyvs env
	val (csStrs, strs) = genTyFunGenEnv (E.getStrs env) state tfun dom b genTyFunExtEnv
	val (csSigs, sigs) = genTyFunGenEnv (E.getSigs env) state tfun dom b genTyFunExtEnv
	val funs           = E.getFuns env
	val (csOvcs, ovcs) = genTyFunGenEnv (E.getOvcs env) state tfun dom b genTyFunExtSeqTy
	val info           = E.consInfo (E.getILab env) (E.getICmp env) tns (E.getIFct env)
	val cs             = csVids @ csTyps @ csOvcs @ csStrs @ csSigs
    in (cs, E.consEnvC vids typs tyvs strs sigs funs ovcs info)
    end
  | genTyFunEnv (x as E.ENVSEQ (env1, env2)) state tfun dom b =
    let val (cs1, env1') = genTyFunEnv env1 state tfun dom b
	val (cs2, env2') = genTyFunEnv env2 state tfun dom b
    in (cs1 @ cs2, E.ENVSEQ (env1', env2'))
    end
  | genTyFunEnv (x as E.ENVDEP (env, labs, stts, deps)) state tfun dom b =
    let val (cs, env') = genTyFunEnv env state tfun dom b
    in (decorateCst cs labs stts deps, E.ENVDEP (env', labs, stts, deps))
    end
  | genTyFunEnv (x as E.ENVVAR _) _ _ _ _ = ([], x)
  | genTyFunEnv (x as E.ENVTOP)   _ _ _ _ = ([], x)
  | genTyFunEnv (x as E.ENVOPN _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | genTyFunEnv (x as E.ENVFUN _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | genTyFunEnv (x as E.ENVCST _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | genTyFunEnv (x as E.ENVPOL _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | genTyFunEnv (x as E.ENVDAT _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | genTyFunEnv (x as E.ENVLOC _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | genTyFunEnv (x as E.ENVWHR _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | genTyFunEnv (x as E.ENVSHA _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | genTyFunEnv (x as E.ENVSIG _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | genTyFunEnv (x as E.ENVPTY _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"
  | genTyFunEnv (x as E.ENVFIL _) _ _ _ _ = raise EH.DeadBranch "This should have been built by now"

and genTyFunExtEnv extenv state tfun dom b =
    genTyFunExtGen extenv tfun (fn env => fn _ => fn _ => genTyFunEnv env state tfun dom b) true

(* b is true if tfun is meant to concretely bind the type functions of env. *)
fun genTyFunEnv' env state tfun b =
    let (*val _   = D.printdebug2 ("B")*)
	val dom = domTFun tfun
	val ret = genTyFunEnv env state tfun dom b
    (*val _   = D.printdebug2 ("E")*)
    in ret
    end




(************************************************************************)
(* Applies a type function to an environment                            *)
(* This is only used by ENVWHR for where clauses                        *)
(************************************************************************)
fun applyTyFunTy (x as (T.V _)) _ _ = ([], x)
  | applyTyFunTy (x as (T.E _)) _ _ = ([], x (*T.V (T.freshtyvar ())*))
  | applyTyFunTy (x as (T.C (tnc, sq, l))) tfun btyp =
    (case collapseTn tnc L.empty L.empty CD.empty of
	 T.ND (T.NC (tn, _, _), labs, stts, deps) =>
	 (case OM.find (tfun, T.tynameToInt tn) of
	      NONE =>
	      let val (cs, sq') = applyTyFunSeqTy sq tfun btyp
	      in (cs, T.C (tnc, sq', l)) end
	    | SOME tyf =>
	      (case collapseTf (freshTyFun tyf btyp) labs stts deps of
		   T.TFD (T.TFC (sq', ty', l'), labs, stts, deps) =>
		   let val (cs, s) = applyTyFunSeqTy sq tfun btyp
		       val v  = T.newV ()
		       val c1 = E.genCstSqAll s sq' labs stts deps (* the labels and context dependencies are not correct *)
		       val c2 = E.genCstTyAll v ty' labs stts deps
		   (*val _ = D.printdebug2 ("foo")*)
		   in (c1 :: c2 :: cs, v)
		   end
		 | _ => raise EH.DeadBranch "not a valid type function"))
       | _ => let val (cs, sq') = applyTyFunSeqTy sq tfun btyp
	      in (cs, T.C (tnc, sq', l)) end)
  | applyTyFunTy (T.A (tf, sq, l)) tfun btyp =
    let val (cs1, tf') = applyTyFunFunTy tf tfun btyp
	val (cs2, sq') = applyTyFunSeqTy sq tfun btyp
    in (cs1 @ cs2, T.A (tf', sq', l))
    end
  | applyTyFunTy (T.OR (sq, i, p, k, l)) tfun btyp =
    let val (cs, sq') = applyTyFunSeqTy sq tfun btyp
    in (cs, T.OR (sq', i, p, k, l))
    end
  | applyTyFunTy (T.GEN tys) tfun btyp =
    let val (cs, tys) = ListPair.unzip (map (fn ty => applyTyFunTy ty tfun btyp) (!tys))
    in (List.concat cs, T.GEN (ref tys))
    end
  | applyTyFunTy (T.TD (ty, labs, stts, deps)) tfun btyp =
    let val (cs, ty') = applyTyFunTy ty tfun btyp
    in (cs, collapseTy ty' labs stts deps)
    end
and applyTyFunFunTy (x as (T.TFV _)) _ _ = ([], x)
  | applyTyFunFunTy (T.TFC (sq, ty, l)) tfun btyp =
    let val (cs1, sq') = applyTyFunSeqTy sq tfun btyp
	val (cs2, ty') = applyTyFunTy    ty tfun btyp
    in (cs1 @ cs2, T.TFC (sq', ty', l))
    end
  | applyTyFunFunTy (T.TFD (tf, labs, stts, deps)) tfun btyp =
    let val (cs, tf') = applyTyFunFunTy tf tfun btyp
    in (cs, collapseTf tf' labs stts deps)
    end
and applyTyFunSeqTy (x as (T.SV _)) _ _ = ([], x)
  | applyTyFunSeqTy (T.SC (rtl, flex, l)) state btyp =
    let val (cs, rtl') = ListPair.unzip (map (fn rt => applyTyFunRowTy rt state btyp) rtl)
    in (List.concat cs, T.SC (rtl', flex, l))
    end
  | applyTyFunSeqTy (T.SD (sq, labs, stts, deps)) tfun btyp =
    let val (cs, sq') = applyTyFunSeqTy sq tfun btyp
    in (cs, collapseSq sq' labs stts deps)
    end
and applyTyFunRowTy (x as (T.RV _)) _ _ = ([], x)
  | applyTyFunRowTy (T.RC (lt, ty, l)) tfun btyp =
    let val (cs, ty') = applyTyFunTy ty tfun btyp
    in (cs, T.RC (lt, ty', l))
    end
  | applyTyFunRowTy (T.RD (row, labs, stts, deps)) tfun btyp =
    let val (cs, row') = applyTyFunRowTy row tfun btyp
    in (cs, collapseRt row' labs stts deps)
    end
  | applyTyFunRowTy (x as T.RO) _ _ = ([], x)

fun applyTyFunExtGen bind tfun f btyp =
    let val sem = E.getBindT bind
	val (cs, sem') = f sem tfun btyp
	val cs' = decorateCst cs (EL.getExtLabL bind) (EL.getExtLabE bind) (EL.getExtLabD bind)
	val bind' = EL.mapExtLab bind (fn x => C.mapBind x (fn _ => sem'))
    in (cs', bind')
    end

fun applyTyFunFunTy' (tyfun, tnKind, cons) tfun btyp =
    let val (cs, tyfun') = applyTyFunFunTy tyfun tfun btyp
    in (cs, (tyfun', tnKind, cons))
    end

fun applyTyFunExtTy    x tfun = applyTyFunExtGen x tfun applyTyFunTy     true
fun applyTyFunExtFunTy x tfun = applyTyFunExtGen x tfun applyTyFunFunTy' false
fun applyTyFunExtSeqTy x tfun = applyTyFunExtGen x tfun applyTyFunSeqTy  true

fun applyTyFunGenEnv genenv tfun genfun =
    E.foldrienv
	(fn (id, semty, (cs, genenv)) =>
	    let val xs = map (fn x => genfun x tfun) semty
		val (cs', semty') = ListPair.unzip xs
	    in (cs @ (List.concat cs'),	E.addenv (id, semty') genenv)
	    end)
	([], E.emgen)
	genenv

fun applyTyFunEnv (env as E.ENVCON _) tfun =
    let val (csVids, vids) = applyTyFunGenEnv (E.getVids env) tfun applyTyFunExtTy
	val (csTyps, typs) = applyTyFunGenEnv (E.getTyps env) tfun applyTyFunExtFunTy
	val tyvs           = E.getTyvs env
	val (csStrs, strs) = applyTyFunGenEnv (E.getStrs env) tfun applyTyFunExtEnv
	val (csSigs, sigs) = applyTyFunGenEnv (E.getSigs env) tfun applyTyFunExtEnv
	val (csFuns, funs) = applyTyFunGenEnv (E.getFuns env) tfun applyTyFunExtFun
	val (csOvcs, ovcs) = applyTyFunGenEnv (E.getOvcs env) tfun applyTyFunExtSeqTy
	val cs             = csVids @ csTyps @ csStrs @ csSigs @ csFuns @ csOvcs
	val tns            =
	    List.mapPartial (fn x as {id, lab, kind, name} =>
				case OM.find (tfun, T.tynameToInt name) of
				    SOME tf =>
				    (case T.isTyName tf of
					 (* We rename a type name if the mapping renames it *)
					 (T.TYNAME name', _, _, _) =>
					 SOME {id = id, lab = lab, kind = kind, name = name'}
				       (* No, we should do this replacement, but in renameenv, we should
					* not rename the type names that are in the environment.*)
				       (* We discart a type name if the mapping maps it to a non type name *)
				       | _ => NONE)
				  (* We keep a type name if the mapping does not map it *)
				  | NONE => SOME x)
			    (E.getITns env)
	val info           = E.consInfo (E.getILab env) (E.getICmp env) tns (E.getIFct env)
    (*TODO: rename the type names in the info part using tfun.*)
    in (cs, E.consEnvC vids typs tyvs strs sigs funs ovcs info)
    end
  | applyTyFunEnv (x as E.ENVSEQ (env1, env2)) tfun =
    let val (cs1, env1') = applyTyFunEnv env1 tfun
	val (cs2, env2') = applyTyFunEnv env2 tfun
    in (cs1 @ cs2, E.ENVSEQ (env1', env2'))
    end
  | applyTyFunEnv (x as E.ENVDEP (env, labs, stts, deps)) tfun =
    let val (cs, env') = applyTyFunEnv env tfun
    in (decorateCst cs labs stts deps, E.ENVDEP (env', labs, stts, deps))
    end
  | applyTyFunEnv (x as E.ENVVAR _) _ = ([], x)
  | applyTyFunEnv (x as E.ENVTOP)   _ = ([], x)
  | applyTyFunEnv (x as E.ENVOPN _) _ = raise EH.DeadBranch "This should have been built by now"
  | applyTyFunEnv (x as E.ENVFUN _) _ = raise EH.DeadBranch "This should have been built by now"
  | applyTyFunEnv (x as E.ENVCST _) _ = raise EH.DeadBranch "This should have been built by now"
  | applyTyFunEnv (x as E.ENVPOL _) _ = raise EH.DeadBranch "This should have been built by now"
  | applyTyFunEnv (x as E.ENVDAT _) _ = raise EH.DeadBranch "This should have been built by now"
  | applyTyFunEnv (x as E.ENVLOC _) _ = raise EH.DeadBranch "This should have been built by now"
  | applyTyFunEnv (x as E.ENVWHR _) _ = raise EH.DeadBranch "This should have been built by now"
  | applyTyFunEnv (x as E.ENVSHA _) _ = raise EH.DeadBranch "This should have been built by now"
  | applyTyFunEnv (x as E.ENVSIG _) _ = raise EH.DeadBranch "This should have been built by now"
  | applyTyFunEnv (x as E.ENVPTY _) _ = raise EH.DeadBranch "This should have been built by now"
  | applyTyFunEnv (x as E.ENVFIL _) _ = raise EH.DeadBranch "This should have been built by now"

and applyTyFunExtEnv extenv tfun =
    applyTyFunExtGen extenv tfun (fn env => fn tfun => fn _ => applyTyFunEnv env tfun) true

and applyTyFunExtFun extfun tfun =
    applyTyFunExtGen extfun
		     tfun
		     (fn (env1, env2) =>
		      fn tfun =>
		      fn _ => let val (cs1, env1') = applyTyFunEnv env1 tfun
				  val (cs2, env2') = applyTyFunEnv env2 tfun
			      in (cs1 @ cs2, (env1', env2'))
			      end)
		     true


(* Matching signature/where clause *)

fun matchWhereEnv envsig NONE state = (OM.empty, true)
  | matchWhereEnv (envsig as E.ENVCON _) (SOME ({lid = I.ID (idTc, labTc), sem, class, lab}, labs, stts, deps)) state =
    let val tmap = OM.empty
    in case E.plusproj (E.getTyps envsig) idTc of
	   [] => (* There is no machting in the signature *)
	   if E.getICmp envsig andalso not (CL.classIsANY class)
	   (* If the signature is complete then we need to raise a unmatched error. *)
	   then let val (idlabs, labs1) = E.getLabsIdsEnv envsig 1
		    val lab'  = E.getLabEnv envsig
		    val labs2 = L.cons lab' (L.union labs1 labs)
		    val ek    = EK.UnbWhere ((L.toInt labTc, I.toInt idTc), idlabs, L.toInt lab')
		in raise errorfound (ERR.consPreError ERR.dummyId labs2 deps ek stts L.dummyLab)
		end
	   (* Otherwise, the type constructor might be filtered out, so we can't do anything. *)
	   (* TODO: we need to remove idTc's type name from envsig if it exists.
	    * - The type name shouldn't be used anyway because there is no binding in the envsig. *)
	   else (tmap, false)
	 | [bind] => (* We found a matching in the signature. *)
	   let val (tf, kind, _) = E.getBindT bind
	       val _ = case kind of
			   E.DAT => if CL.classIsANY (E.getBindC bind)
	 			    then ()
				    else (case T.isTyName sem of
					      (T.NOTTYNAME, labs', stts', deps') =>
					      let val labs = L.union  (L.union  labs labs') (EL.getExtLabL bind)
						  val stts = L.union  (L.union  stts stts') (EL.getExtLabE bind)
						  val deps = CD.union (CD.union deps deps') (EL.getExtLabD bind)
						  val lab' = E.getBindL bind
						  val ek   = EK.IllFormedWhere ((L.toInt lab', I.toInt idTc), (L.toInt labTc, I.toInt idTc))
						  (*val _ = D.printdebug2 (T.printtyf sem)*)
					      in raise errorfound (ERR.consPreError ERR.dummyId labs deps ek stts L.dummyLab)
					      end
					    | _ => ())
			 | E.TYP => ()
	       val (names, labs', stts', deps') = T.isTyName tf
	   in case names of
		  T.TYNAME name => (* The signature defines a type name for the idTc so it has to be renamed.
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
			  in (insertInTyFun tmap name (collapseTf sem labs stts deps), true)
			  end
			| loopTf (T.TFD etf) = loopTf (EL.getExtLabT etf)
			| loopTf _ = (insertInTyFun tmap name (newTyFun ()), true)
		  in loopTf sem
		  end
		| T.DUMTYNAME name => (* The signature defines a matching for idTc that could potentially
				       * be a type name, but we don't have enough info to know.*)
		  (* TODO: we need to remove name from the list of type names in envsig.
		   * - This is done when applying the mapping. *)
		  (insertInTyFun tmap name (newTyFun ()), true)
		| T.MAYTYNAME => (* We could have a matching in the signature but it does not
				  * a type name so we don't need to do nothing. *)
		  (tmap, true)
		| T.NOTTYNAME => (* We have a matching in the signature which is definitely not a
				  * type name, so there is an error because of the condition
				  * 'E(longtycon) = (t, VE)' for where clauses (tf is not a t).
				  * (Called non-flexible type in HaMLet.) *)
		  if not (CL.classIsANY class)
		  then let val labs = L.union  (L.union  labs labs') (EL.getExtLabL bind)
			   val stts = L.union  (L.union  stts stts') (EL.getExtLabE bind)
			   val deps = CD.union (CD.union deps deps') (EL.getExtLabD bind)
			   val lab' = E.getBindL bind
			   val ek   = EK.NonFlexWhere ((L.toInt lab', I.toInt idTc), (L.toInt labTc, I.toInt idTc))
		       in raise errorfound (ERR.consPreError ERR.dummyId labs deps ek stts L.dummyLab)
		       end
		  else (tmap, true)
	   end
	 | _ => raise EH.DeadBranch "There should be only one binding per identifier during constraint solving"
    end
  | matchWhereEnv (envsig as E.ENVCON _) (SOME ({lid = I.LID ((id, lab1), lid, lab2), sem, class, lab}, labs, stts, deps)) state =
    (case E.plusproj (E.getStrs envsig) id of
	 [] => (* There is no machting in the signature *)
	 if E.getICmp envsig
	 (* If the signature is complete then we need to raise a unmatched error. *)
	 then let val (idlabs, labs1) = E.getLabsIdsEnv envsig 1
		  val lab'  = E.getLabEnv envsig
		  val labs2 = L.cons lab' (L.union labs1 labs)
		  val ek    = EK.UnbWhere ((L.toInt lab1, I.toInt id), idlabs, L.toInt lab')
	      in raise errorfound (ERR.consPreError ERR.dummyId labs2 deps ek stts L.dummyLab)
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
  | matchWhereEnv (envsig as E.ENVSEQ (env1, env2)) longtyp state =
    (let val (tmap, found) = matchWhereEnv env2 longtyp state
     (* If found then it means that we found the matching.
      * Otherwise, either we raised an error because we couldn't find any matching
      * or we don't have enough info to know if env1 has a matching and then we
      * should generates dummies for env1. *)
     in if found
	then (tmap, true)
	else let val (tmapDat, tmapTyp) = getAllTyFunEnv env1
	     in (mergeTyFun tmap (mergeTyFun tmapDat tmapTyp), false)
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
  | matchWhereEnv (E.ENVVAR _) _ _ = (OM.empty, false)
  | matchWhereEnv (E.ENVTOP)   _ _ = (OM.empty, true)
  | matchWhereEnv (E.ENVOPN _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | matchWhereEnv (E.ENVFUN _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | matchWhereEnv (E.ENVCST _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | matchWhereEnv (E.ENVPOL _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | matchWhereEnv (E.ENVDAT _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | matchWhereEnv (E.ENVLOC _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | matchWhereEnv (E.ENVWHR _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | matchWhereEnv (E.ENVSHA _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | matchWhereEnv (E.ENVSIG _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | matchWhereEnv (E.ENVPTY _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | matchWhereEnv (E.ENVFIL _) _ _ = raise EH.DeadBranch "This should have been built by now"

(*(* I'm doing that the wrong way, we should first check envwhere and in case it is incomplete
 * the we should deal with all the dummy cases. *)
fun matchWhereEnv (envsig as E.ENVCON _) (envwhere as E.ENVCON _) =
    let val tnmap = E.getTyNames (E.getTyps envsig)
	val (tmapDat1, tmapTyp1) =
	    foldr (fn (E.TYNAME ({id, kind, name}, labs, stts, deps), (tmapDat, tmapTyp)) =>
		      (* name is a type name defined by id. *)
		      (* The labs, stts, and deps might be used to report errors. *)
		      (case E.plusproj (E.getTyps envwhere) id of
			   [] => if E.getICmp envwhere
				 (* If the 'where clause' is complete and it has no matching for id
				  * then it means that the type name is not renamed. *)
				 then (tmapDat, tmapTyp)
				 (* Otherwise, the 'where clause' might contain a matching for the id
				  * which is currently fitered out.
				  * If the type name is for a datatype then we want to keep a
				  * datatype-like type for the binding. *)
				 (* WARNING: we need to remove the type name in the info part of the envsig. *)
				 else (case kind of
					   E.DAT => (insertInTyFun tmapDat name (newTyFun ()), tmapTyp)
					 | E.TYP => (tmapDat, insertInTyFun tmapTyp name (newTyFun ())))
			 | [bind] => (* We found a matching in the where clause so the type name will
				      * have to be renamed.
				      * (1) We also need to check that if name is in the context then
				      * we have an error because of the 't \not\in T of B' condition
				      * for where clauses (rigid type).
				      * (2) We also need to check that if kind is a DAT then
				      * the bind is a type name, because of the well-formedness
				      * condition for where clauses. *)
			   (* TODO: check (1) and (2) and raise errors if necessary. *)
			   (* WARNING: we need to update the type name in the info part of the envsig. *)
			   (case E.getBindT bind of
				(tf as T.TFC _, _, _) =>
				let val labs = EL.getExtLabL bind
				    val stts = EL.getExtLabE bind
				    val deps = EL.getExtLabD bind
				in (tmapDat, insertInTyFun tmapTyp name (tf, labs, stts, deps))
				end
			      | _ => (tmapDat, insertInTyFun tmapTyp name (newTyFun ())))
			 | _ => raise EH.DeadBranch "There should be only one binding per identifier during constraint solving")
		    | (E.DUMTYNAME {id, kind, name}, (tmapDat, tmapTyp)) =>
		      (* name could potentially be a type name defined by id. *)
		      (case E.plusproj (E.getTyps envwhere) id of
			   [] => if E.getICmp envwhere
				 (* If the 'where clause' is complete and it has no matching for id
				  * then it means that the type name is not renamed. *)
				 then (tmapDat, tmapTyp)
				 (* Otherwise, the 'where clause' might contain a matching for the id
				  * which is currently fitered out.
				  * If the type name is for a datatype then we want to keep a
				  * datatype-like type for the binding. *)
				 (* WARNING: we need to remove the type name in the info part of the envsig. *)
				 else (case kind of
					   E.DAT => (insertInTyFun tmapDat name (newTyFun ()), tmapTyp)
					 | E.TYP => (tmapDat, insertInTyFun tmapTyp name (newTyFun ())))
			 | [bind] => (* We found a matching in the where clause but we don't know if it has
				      * to rename name because we don't know if name is declared in the
				      * signature (not enough info), but it might, so we are going to replace
				      * the name by a dummy type function.
				      * If the name were not a name declared for id then it would mean that
				      * the E(longtycon) = (t, VE) would be violated, because we wouldn't
				      * have a t. *)
			   (* WARNING: we need to remove the type name in the info part of the envsig. *)
			   (tmapDat, insertInTyFun tmapTyp name (newTyFun ()))
			 | _ => raise EH.DeadBranch "There should be only one binding per identifier during constraint solving")
		    | (E.MAYTYNAME, (tmapDat, tmapTyp)) => (tmapDat, tmapTyp) (* Remove this constructor! Useless! *)
		    (* We don't have enough info to do anything. *)
		    (* WARNING: if a name was associated to id in the info part of the envsig we should remove it (impossible case?). *)
		    | (E.NOTTYNAME (id, labs, stts, deps), (tmapDat, tmapTyp)) =>
		      (* the id definitely doesn't define a type name, so if the id is in the where clause
		       * then it is violating the condition: E(longtycon) = (t, VE) because we don't have
		       * a t here. *)
		      (case E.plusproj (E.getTyps envwhere) id of
			   [] => (tmapDat, tmapTyp)
			 | [bind] => (* We found a matching in the where clause so it means that we have
				      * an error (non-flexible type). *)
			   (*TODO: raise the error *)
			   (tmapDat, tmapTyp)
			 | _ => raise EH.DeadBranch "There should be only one binding per identifier during constraint solving"))
		  (OM.empty, OM.empty)
		  tnmap
	val (tmapDat2, tmapTyp2) = matchWhereStrEnv (E.getStrs envsig) (E.getStrs envwhere)
    in (mergeTyFun tmapDat1 tmapDat2, mergeTyFun tmapTyp1 tmapTyp2)
    end
  | matchWhereEnv (env as E.ENVCON _) _ = getAllTyFunEnv env
  | matchWhereEnv (E.ENVSEQ (env0, env1)) env2 =
    let val (tfnDs1, tfnTs1) = matchWhereEnv env0 env2
	val (tfnDs2, tfnTs2) = matchWhereEnv env1 env2
    in (mergeTyFun tfnDs1 tfnDs2, mergeTyFun tfnTs1 tfnTs2)
    end
  | matchWhereEnv (E.ENVVAR _) _ = (OM.empty, OM.empty)
  | matchWhereEnv (E.ENVTOP)   _ = (OM.empty, OM.empty)
  | matchWhereEnv (E.ENVOPN _) _ = raise EH.DeadBranch "This should have been built by now"
  | matchWhereEnv (E.ENVDEP _) _ = raise EH.DeadBranch "This should have been built by now"
  | matchWhereEnv (E.ENVFUN _) _ = raise EH.DeadBranch "This should have been built by now"
  | matchWhereEnv (E.ENVCST _) _ = raise EH.DeadBranch "This should have been built by now"
  | matchWhereEnv (E.ENVPOL _) _ = raise EH.DeadBranch "This should have been built by now"
  | matchWhereEnv (E.ENVDAT _) _ = raise EH.DeadBranch "This should have been built by now"
  | matchWhereEnv (E.ENVLOC _) _ = raise EH.DeadBranch "This should have been built by now"
  | matchWhereEnv (E.ENVWHR _) _ = raise EH.DeadBranch "This should have been built by now"
  | matchWhereEnv (E.ENVSHA _) _ = raise EH.DeadBranch "This should have been built by now"

and matchWhereExtEnv extenv extenv' =
    let val labs = EL.getExtLabL extenv'
	val stts = EL.getExtLabE extenv'
	val deps = EL.getExtLabD extenv'
	val (tmapDat, tmapTyp)  = matchWhereEnv (E.getBindT extenv) (E.getBindT extenv')
    in (decorateTyFun tmapDat labs stts deps, decorateTyFun tmapTyp labs stts deps)
    end

and matchWhereStrEnv strenv strenv' =
    E.foldrienv
	(fn (id, semty, (tmapDat, tmapTyp)) =>
	    foldr (fn (extenv, tfns) =>
		      foldr (fn (extenv', (tmapDat, tmapTyp)) =>
				let val (tmapDat', tmapTyp') = matchWhereExtEnv extenv extenv'
				in (mergeTyFun tmapDat tmapDat', mergeTyFun tmapTyp tmapTyp')
				end)
			    (tmapDat, tmapTyp)
			    (E.plusproj strenv' id))
		  (tmapDat, tmapTyp)
		  semty)
	(OM.empty, OM.empty)
	strenv*)





(* Environment renamening *)

(* TODO *)

fun initRen () = ref OM.empty

fun updateRen state tn =
    (case Option.getOpt (OM.find (!state, T.tynameToInt tn), NONE) of
	 NONE => let val tn' = T.freshtyname ()
		 in (state := (OM.insert (!state, T.tynameToInt tn, SOME tn')); tn') end
       | SOME tn' => tn')

fun getRen state tn =
    (case Option.getOpt (OM.find (!state, T.tynameToInt tn), NONE) of
	 NONE => tn
       | SOME tn' => tn')

fun renametyname (x as T.NV _) _ = x
  | renametyname (T.NC (tn, b, l)) state =
    if T.isBase' tn
    then T.NC (tn, b, l)
    else T.NC ((*updateRen*) getRen state tn, b, l)
  | renametyname (T.ND etn) state = T.ND (EL.mapExtLab etn (fn tn => renametyname tn state))

fun renamety (x as T.V _) _ = x
  | renamety (x as T.E _) _ = x
  | renamety (T.C (tn, sq, l))       state = T.C (renametyname tn state, renameseqty sq state, l)
  | renamety (T.A (tf, sq, l))       state = T.A (renametypfun tf state, renameseqty sq state, l)
  | renamety (T.OR (sq, i, p, k, l)) state = T.OR (renameseqty sq state, i, p, k, l)
  | renamety (T.GEN ty)              state = raise EH.TODO
  | renamety (T.TD ety)              state = T.TD (EL.mapExtLab ety (fn ty => renamety ty state))
and renametypfun (x as T.TFV _) _ = x
  | renametypfun (T.TFC (sq, ty, lab)) state = T.TFC (renameseqty sq state, renamety ty state, lab)
  | renametypfun (T.TFD etf)           state = T.TFD (EL.mapExtLab etf (fn tf => renametypfun tf state))
and renameseqty (x as T.SV _) _ = x
  | renameseqty (T.SC (rtl, flex, l)) state = T.SC (map (fn rt => renamerowty rt state) rtl, flex, l)
  | renameseqty (T.SD eseq)           state = T.SD (EL.mapExtLab eseq (fn seq => renameseqty seq state))
and renamerowty (x as T.RV _)      _     = x
  | renamerowty (T.RC (lt, ty, l)) state = T.RC (lt, renamety ty state, l)
  | renamerowty (T.RD erow)        state = T.RD (EL.mapExtLab erow (fn row => renamerowty row state))
  | renamerowty (x as T.RO)        _     = T.RO
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

and renameenv (env as E.ENVCON _) state ren =
    let (*val state = ref (!state) (*(2010-06-14)Why a new reference?*)*)
	val tns   = map (fn {id, lab, kind, name} =>
			    ((*D.printdebug2 (S.printState state ^ "\n\n" ^ T.printtyname name);*)
			     if S.isAName name state
			     then {id = id, lab = lab, kind = kind, name = name}
			     else {id = id, lab = lab, kind = kind, name = updateRen ren name}))
			(E.getITns env)
	val strs  = renamestrenv (E.getStrs env) state ren
	val sigs  = renamestrenv (E.getSigs env) state ren
	val funs  = renamefunenv (E.getFuns env) state ren
	(* We do the strs, sigs and funs before as they can update ren *)
	val ovcs  = renameseqenv (E.getOvcs env) state ren
	val vids  = renamevarenv (E.getVids env) state ren
	val typs  = renametypenv (E.getTyps env) state ren
	val tyvs  = E.getTyvs env
	val info  = E.consInfo (E.getILab env) (E.getICmp env) tns (E.getIFct env)
    in E.consEnvC vids typs tyvs strs sigs funs ovcs info
    end
  | renameenv (E.ENVSEQ (env1, env2)) state ren =
    E.ENVSEQ (renameenv env1 state ren, renameenv env2 state ren)
  | renameenv (E.ENVDEP (env, labs, stts, deps)) state ren =
    let val env' = renameenv env state ren
    in E.ENVDEP (env', labs, stts, deps)
    end
  | renameenv (x as E.ENVVAR _) _ _ = x
  | renameenv (x as E.ENVTOP)   _ _ = x
  | renameenv (E.ENVLOC _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | renameenv (E.ENVWHR _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | renameenv (E.ENVSHA _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | renameenv (E.ENVSIG _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | renameenv (E.ENVPOL _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | renameenv (E.ENVDAT _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | renameenv (E.ENVOPN _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | renameenv (E.ENVFUN _) _ _ = raise EH.DeadBranch "This should have been built by now"
  | renameenv (E.ENVCST _) _ _ = raise EH.DeadBranch "This should have been built by now"
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

fun buildSeqAr state (T.SV sv) labs stts deps=
    (case S.getValStateSq state sv of
	 NONE => T.newSV ()
       | SOME sq => buildSeqAr state sq labs stts deps)
  | buildSeqAr state (T.SC (xs, flex, lab)) labs stts deps =
    T.SD (T.SC (map (fn _ => T.newRV ()) xs, flex, lab), labs, stts, deps)
  | buildSeqAr state (T.SD (sq1, labs1, stts1, deps1)) labs stts deps =
    let val sq2 = buildSeqAr state sq1 labs stts deps
    in T.SD (sq2, labs1, stts1, deps1)
    end

fun buildTyFunAr state (T.TFV tfv) lab labs stts deps =
    (case S.getValStateTf state tfv of
	 NONE => T.TFV tfv
       | SOME tf => buildTyFunAr state tf lab labs stts deps)
  | buildTyFunAr state (T.TFC (sq, ty, _)) lab labs stts deps =
    (*(2010-06-17)This is to ensure that the type is not preliminary constrainted by something else
     * Ooops, there is still the constraint about the size of sq!*)
    if T.isTyV ty andalso T.isShallowSeq sq
    then let val sq' = buildSeqAr state sq labs stts deps
	 in T.TFC (sq', T.newV (), lab)
	 end
    else T.newTFV ()
  | buildTyFunAr state (T.TFD (tf, labs1, stts1, deps1)) lab labs stts deps =
    let val tf' = buildTyFunAr state tf lab labs stts deps
    in T.TFD (tf', labs1, stts1, deps1)
    end



(* Checks if a type variable is already stored in the 'ge' part of the unification environment. *)
fun isInState (T.V (tv, _, _)) state =
    S.isInGe state tv
    (*Option.isSome (S.getValStateTv state tv)*)
  | isInState _ _ = false

fun updateStateTyGen state (T.V (tv, _, _)) ty labs stts deps =
    (case S.getValStateTv state tv of
	 NONE => S.updateStateTv state tv (T.GEN (ref [T.TD (ty, labs, stts, deps)]))
       | SOME typ =>
	 (case collapseTy typ labs stts deps of
	      T.TD (T.GEN tys, labs, stts, deps) =>
	      let val tv' = T.freshtyvar ()
		  val ty' = T.consV tv'
		  val _   = S.updateStateTv state tv' (T.TD (ty, labs, stts, deps))
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
(* this is used to buid environments *)
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
		 else foch (ERR.consPreError ERR.dummyId labs deps EK.Circularity stts lab) (c (var1, sem1)) vars depth lab
	    else occursGenList c var1 var2 sem1 labs stts deps vars depth lab fget fdecomp foc

	fun foccurs c [] n l = true (* true because we have to add c to the state *)
	  | foccurs (c as (CT (var1, sem))) [T (var2, labs, stts, deps)] 0 l = occursGenZero CT var1 var2 sem labs stts deps l T.eqTyvar  S.getValStateTv decomptyty  foccurs
	  | foccurs (c as (CS (var1, sem))) [S (var2, labs, stts, deps)] 0 l = occursGenZero CS var1 var2 sem labs stts deps l T.eqSeqvar S.getValStateSq decomptysq  foccurs
	  | foccurs (c as (CR (var1, sem))) [R (var2, labs, stts, deps)] 0 l = occursGenZero CR var1 var2 sem labs stts deps l T.eqRowvar S.getValStateRt decomptyrow foccurs
	  | foccurs (c as (CT (var1, sem))) (var :: xs) n l =
	    (case var of
		 T (var2, labs, stts, deps) => occursGenListEq CT var1 var2 sem labs stts deps xs n l T.eqTyvar S.getValStateTv decomptyty foccurs handleOccurs
	       | S (var2, labs, stts, deps) => occursGenList CT var1 var2 sem labs stts deps xs n l S.getValStateSq decomptysq  foccurs
	       | R (var2, labs, stts, deps) => occursGenList CT var1 var2 sem labs stts deps xs n l S.getValStateRt decomptyrow foccurs)
	  | foccurs (c as (CS (var1, sem))) (var :: xs) n l =
	    (case var of
		 S (var2, labs, stts, deps) => occursGenListEq CS var1 var2 sem labs stts deps xs n l T.eqSeqvar S.getValStateSq decomptysq foccurs handleOccurs
	       | T (var2, labs, stts, deps) => occursGenList CS var1 var2 sem labs stts deps xs n l S.getValStateTv decomptyty  foccurs
	       | R (var2, labs, stts, deps) => occursGenList CS var1 var2 sem labs stts deps xs n l S.getValStateRt decomptyrow foccurs)
	  | foccurs (c as (CR (var1, sem))) (var :: xs) n l =
	    (case var of
		 R (var2, labs, stts, deps) => occursGenListEq CR var1 var2 sem labs stts deps xs n l T.eqRowvar S.getValStateRt decomptyrow foccurs handleOccurs
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
		     (*(2010-02-16) then because the binder is a constructor,
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
		   | (NONE, SOME _, true)  => SOME x   (* we went down some structure but couldn't find all of lid             *)
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


	(* ====== PARTIAL ENVIRONMENT SOLVER ====== *)

	fun preSolveEnv (E.ENVVAR (ev, lab)) =
	    (case S.getValStateEv state ev of
		 NONE => E.ENVVAR (ev, lab)
	       | SOME env => preSolveEnv env)
	  | preSolveEnv (E.ENVDEP (env, labs, stts, deps)) =
	    let val env' = preSolveEnv env
	    in E.pushExtEnv env' labs stts deps
	    end
	  | preSolveEnv (E.ENVSEQ (env1, env2)) =
	    let val env1' = preSolveEnv env1
		val env2' = preSolveEnv env2
	    in E.ENVSEQ (env1', env2')
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
	      | FI.BIND => BINDDUM (E.consBindPoly (E.getBindI bind) (T.newV ()) (CL.consANY ()) (E.getBindL bind))

	fun solveextvarcontext (bind as (_, labs, stts, deps)) cl =
	    let val id  = E.getBindI bind
		val lab = E.getBindL bind
		val lid = I.idToLid id lab
		fun generateAcc () =
		    let val a = E.genAccIvAll (E.consAccId lid (E.getBindT bind) cl lab) labs stts deps
		    in BINDNOT (E.singcst (lab, E.CSTACC a))
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
		      (* NOTE: if in the environment there is a dummy binding then we can't
		       * generate a binding because we don't know if the one in the environment
		       * comes from a CON or a REC or something else.*)
		      | CL.ANY     => BINDDUM (E.consBindPoly id (T.newV ()) (CL.consANY ()) lab)
		      | CL.CLVAR _ => BINDDUM (E.consBindPoly id (T.newV ()) (CL.consANY ()) lab)
		      | class => (print (CL.toString class); raise EH.DeadBranch "identifier in environment has an unexpected status"))
		 (* NOTE: if the environment in the state hides with environment variables
		  * then we can't know if one of these variable should in fact declare
		  * a CON or EXC binder for the same if as the current pseudo-binder. *)
		 | _ => if S.hasEnvVar state
			then BINDDUM (E.consBindPoly id (T.newV ()) (CL.consANY ()) lab)
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
	      | CL.CLVAR _    => BINDDUM (E.consBindPoly (E.getBindI bind) (T.newV ()) (CL.consANY ()) (E.getBindL bind))
	      | class => (print (CL.toString class); raise EH.DeadBranch "identifier in binder has an unexpected status")

	fun solvedumvarbind bind =
	    case S.getValStateIdVa state (I.idToLid (E.getBindI bind) (E.getBindL bind)) false of
		(SOME (bind', _), _, _) =>
		(case E.getBindC bind' of
		     CL.VID CL.VAL => BINDDUM bind
		   | CL.VID CL.PAT => BINDDUM bind
		   | CL.VID CL.REC => BINDDUM bind
		   | CL.VID CL.VRC => BINDDUM bind
		   | CL.VID CL.CO0 => BINDNOT E.emcst
		   | CL.VID CL.CO1 => BINDNOT E.emcst
		   | CL.VID CL.CON => BINDNOT E.emcst
		   | CL.VID CL.DA0 => BINDNOT E.emcst
		   | CL.VID CL.DA1 => BINDNOT E.emcst
		   | CL.VID CL.DAT => BINDNOT E.emcst
		   | CL.VID CL.EX0 => BINDNOT E.emcst
		   | CL.VID CL.EX1 => BINDNOT E.emcst
		   | CL.VID CL.EXC => BINDNOT E.emcst
		   | CL.ANY        => BINDDUM bind
		   | CL.CLVAR _    => BINDDUM bind
		   | class => (print (CL.toString class); raise EH.DeadBranch "identifier in environment has an unexpected status"))
	      | _ => BINDDUM bind

	fun solveextvar bind =
	    ((*D.printdebug2 (L.printLab (E.getBindL bind));*)
	     case solveextvarkeep bind of
		 BINDIN  bind' => solveextvarrebind bind'
	       | BINDPOL bind' => solveextvarrebind bind'
	       (*(2010-06-21)In the case of a BINDDUM ANY then we still need to check
		* that the context doesn't hide a real environment in case E.getBindC bind
		* is a VAL or a PAT and that the id in the environment is a CON or an EXC. *)
	       | BINDDUM bind' => if CL.classIsVAL (E.getBindC bind) orelse
				     CL.classIsPAT (E.getBindC bind)
				  then solvedumvarbind bind'
				  else BINDDUM bind'
	       | sbind => sbind)

	fun solveexttyp (bind as (btyp, labs, stts, deps)) =
	    case FI.getStateLabs filters (EL.getExtLabL bind)
	     (*FI.getStateLab filters (E.getBindL bind)*) of
		FI.IN   => let val (tyf, tnKind, cons) = E.getBindT bind
			       val tyf' = buildtyfun tyf state I.empty false false
			   (*val _ = D.printdebug2 (S.printState state ^ "\n" ^ I.printId (E.getBindI bind) ^ "\n" ^ T.printtyf tyf ^ "\n" ^ T.printtyf tyf')*)
			   in BINDIN (C.mapBind btyp (fn _ => (tyf', tnKind, cons)),
				      labs,
				      stts,
				      deps)
			   end
	      | FI.OUT  => BINDOUT
	      | FI.BIND => BINDDUM (E.consBindPoly (E.getBindI bind) (E.getBindT bind) (CL.consANY ()) (E.getBindL bind))

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
	      | FI.BIND => BINDDUM (E.consBindPoly (E.getBindI bind) (E.getBindT bind) (CL.consANY ()) (E.getBindL bind))

	fun solveexttyv bind =
	    case FI.getStateLab filters (E.getBindL bind) of
		FI.IN   => (case E.getBindT bind of
				(tv, true)  => BINDIN bind
			      | (tv, false) =>
				let val id  = E.getBindI bind
				    val lab = E.getBindL bind
				    val lid = I.idToLid id lab
				in case S.getValStateIdTv state lid false of
				       (SOME (({id, bind = (_, true),  lab = _, poly, class}, _, _, _), _), _, _) => BINDNOT E.emcst
				     | (SOME (({id, bind = (_, false), lab = _, poly, class}, _, _, _), _), _, _) => BINDNOT E.emcst
				     | _ => BINDIN bind
				end)
	      | FI.OUT  => BINDOUT
	      | FI.BIND => BINDDUM (E.consBindPoly (E.getBindI bind) (E.getBindT bind) (CL.consANY ()) (E.getBindL bind))

	fun solveextenv bind =
	    (case FI.getStateLab filters (E.getBindL bind) of
		 FI.IN   => BINDIN bind
	       (*let val env1 = E.getBindT bind
		     val env2 = preSolveEnv env1
		 in BINDIN (EL.mapExtLab bind (fn benv => C.mapBind benv (fn _ => env2)))
		 end*)
	       | FI.OUT  => BINDOUT
	       | FI.BIND => BINDDUM (E.consBindPoly (E.getBindI bind) (E.getBindT bind) (CL.consANY ()) (E.getBindL bind)))

	fun solveextfun bind =
	    ((*D.printdebug2 (FI.toString filters ^ "\n" ^ I.printId (E.getBindI bind));*)
	     case FI.getStateLab filters (E.getBindL bind) of
		 FI.IN   => BINDIN bind
	       | FI.OUT  => BINDOUT
	       | FI.BIND => BINDDUM (E.consBindPoly (E.getBindI bind) (E.getBindT bind) (CL.consANY ()) (E.getBindL bind)))
	(*(EL.mapExtLab (E.resetExtLab bind) C.resetPoly)*)

	(*fun solvegenenv genenv fbuild bmon =
	      E.foldrienv
		  (fn (id, sem, (genenv, comp)) =>
		      let val (sem', b) =
			      foldr (fn (bind, (sem, b)) =>
					case fbuild bind of
					    NONE => (sem, false)
					  | SOME (bind', b') =>
					    (bind' :: sem, b andalso b'))
				    ([], true)
				    sem
		      in if List.null sem'
			 then (genenv, false)
			 else (E.addenv (id, sem') genenv, b andalso comp)
		      end)
		  (E.emgen, true)
		  genenv*)

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
		    in raise errorfound (ERR.consPreError ERR.dummyId labs deps ek stts L.dummyLab)
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
					| BINDNOT cst'  => (ubind, bin, bpol, bcomp, E.uenvcst [cst, cst'])
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
		(E.emgen, true, E.emcst)
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


	(* ====== ENVIRONMENT SOLVER ====== *)

	fun solveenv (E.ENVVAR (ev, lab)) bmon =
	    (case S.getValStateEv state ev of
		 NONE => E.ENVVAR (ev, lab)
	       | SOME env =>
		 let (*val env' = solveenv env bmon*)
		     val env' = preSolveEnv env
		 in env'
		 end)
	  (* Here we don't need to re-solve env, preSolveEnv is enough *)
	  | solveenv (env as E.ENVCON _) bmon =
	    (let val (vids, compVids, cst) = solvevarenv (E.getVids env) bmon
		 val (typs, compTyps, _)   = solvetypenv (E.getTyps env) bmon
		 val (tyvs, compTyvs, _)   = solvetyvenv (E.getTyvs env) bmon
		 val (strs, compStrs, _)   = solvestrenv (E.getStrs env) bmon
		 val (sigs, compSigs, _)   = solvesigenv (E.getSigs env) bmon
		 val (funs, compFuns, _)   = solvefunenv (E.getFuns env) bmon
		 val (ovcs, compOvcs, _)   = solveovcenv (E.getOvcs env) bmon
		 val cmp   = compVids andalso compTyps andalso compTyvs andalso
			     compStrs andalso compSigs andalso compFuns andalso
			     compOvcs andalso E.getICmp env
		 val nfo   = E.consInfo (E.getILab env) cmp (E.getITns env) (E.getIFct env)
		 val _     = run cst
		 val env'  = E.consEnvC vids typs tyvs strs sigs funs ovcs nfo
	     (*val _     = D.printdebug2 (E.printEnv env' "")*)
	     in env'
	     end
	     handle errorfound err => handleSolveEnv err env)
	  | solveenv (E.ENVSEQ (env1, env2)) bmon =
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
		 FI.OUT  => E.emenv
	       | FI.BIND => E.newEnvVar L.dummyLab
	       | FI.IN   => E.pushExtEnv (solveenv env bmon) labs stts deps)
	  | solveenv (E.ENVFUN cst) bmon = E.ENVFUN cst
	  | solveenv (E.ENVCST cst) bmon = (run cst; E.emenv)
	  | solveenv (E.ENVPOL (tyvenv, env)) bmon =
	    let val env1  = solveenv (E.projTyvs tyvenv) bmon
		val b     = E.isEmptyEnv env1
		val tvs   = S.pushEnvToState b env1 state
		val env'  = solveenv env bmon
		val _     = S.remEnvFromState b tvs state
		fun checkTyVars (env as E.ENVCON _) labs stts deps =
		    (* NOTE: Should only contain Vids's so we only deal with them. *)
		    let val dom  = E.dom (E.getTyvs env1)
			val vids = buildVarEnv (E.toPolyVids (E.getVids env)) state dom bmon
		    (*val _    = D.printdebug2 (S.printState state)*)
		    (*val _    = D.printdebug2 (E.printEnv env "")*)
		    (*val _    = D.printdebug2 (E.printEnv (E.projVids vids) "")*)
		    in case getExplicitTyVars vids (E.getTyvs env1) state of
			   NONE => E.updateVids vids env
			 | SOME (id, lab, labs0, stts0, deps0) =>
			   let val ek    = EK.TyVarBind NONE
			       val labs1 = L.union  labs labs0
			       val stts1 = L.union  stts stts0
			       val deps1 = CD.union deps deps0
			       val err   = ERR.consPreError ERR.dummyId labs1 deps1 ek stts1 lab
			   (*val _ = D.printdebug2 (E.printEnv (E.projVids vids) "")*)
			   in handleSolveEnv err (E.projVids vids)
			   end
		    end
		  | checkTyVars (env as E.ENVVAR _) _ _ _ = env
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
	  | solveenv (E.ENVDAT (idlab, env)) bmon =
	    let val env' = solveenv env bmon
		val _    = S.updateDatCons state idlab env'
	    in env'
	    end
	  | solveenv (E.ENVLOC (env1, env2)) bmon =
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
		  * rebind a type name from the environment. *)
		 (*val _ = D.printdebug2 (E.printEnv env0 "")*)
		 val (tmap, _) = matchWhereEnv env0 solvedLongTyp state
		 val (cs0, env1) = applyTyFunEnv env0 tmap
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
		 val (utf, tfun) = getTyFunEnvSha env0 env2
		 val tfun = case utf of
				NONE => OM.map (fn _ => newTyFun ()) tfun
			      | SOME (tf, labs, stts, deps) =>
				OM.map (fn NONE => newTyFun ()
					 | SOME (labs', stts', deps') =>
					   collapseTf tf
						      (L.union labs labs')
						      (L.union  stts stts')
						      (CD.union deps deps'))
				       tfun
		 val (cs0, env1) = genTyFunEnv' env0 state tfun false
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
	  | solveenv (envsig as E.ENVSIG (e1, e2, kind)) bmon =
	    (let val env0 = buildFEnv e1 state false
		 val env2 = buildFEnv e2 state true
		 val _ = D.printdebug2 (E.printEnv env0 "" ^ "\n" ^ E.printEnv env2 "")
		 val (tfnD, tfnT) = getTyFunEnv env0 env2 L.empty L.empty CD.empty
		 val (cs0, env1) = genTyFunEnv' env0 state tfnT true
		 val (cs1, cs2) = matchSigStr env1 env2 L.dummyLab filters L.empty L.empty CD.empty true err
		 val _ = sigVsStrON ()
		 val _ = fsimplify (cs0 @ cs1) L.dummyLab
		 val _ = sigVsStrTypON ()
		 val _ = fsimplify cs2 L.dummyLab
		 val _ = sigVsStrTypOFF ()
		 val _ = sigVsStrOFF ()
	     in case kind of
		    E.OPA => freshenv' (renameenv' env0 state) (SOME O.empty) false
		  | E.TRA => justBuildEnv env1 state true
	     end
	     handle errorfound err => handleSolveEnv err envsig)
	  | solveenv E.ENVTOP bmon = raise EH.TODO
	  | solveenv (E.ENVPTY st) bmon = raise EH.TODO
	  | solveenv (E.ENVFIL (file, env, strm)) bmon =
	    let val _ =
		    case user of
			ENUM => print ("[TES: analysing " ^ file ^ "]\n")
		      | _    => ()
		val env1 = solveenv env bmon
		val b    = E.isEmptyEnv env1
		val tvs  = S.pushEnvToState b env1 state
		val env2 = (*E.emenv*) solveenv (strm ()) bmon
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
			let val sem0 = buildtyfun sem state I.empty false false
			in SOME ({lid = lid, sem = sem0, class = class, lab = lab},
				 L.union labs (I.getLabs lid),
				 stts,
				 deps)
			end
		      | SOME (lid', false) =>  genDum ())
	    end


	(* ====== OPENING SOLVER ====== *)

	and solveopnsem (opnsem as (lid, lab, E.OST)) bmon = (* opening of a structure *)
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
	  | solveopnsem (opnsem as (lid, lab, E.DRE)) bmon = (* datatype replication *)
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
		      in E.pushExtEnv (E.projVids varenv) labs' stts deps
		      end
		    | (_, SOME (x as ((id1, lab1), (env, labs', stts', deps'))), true) =>
		      (handleUnmatched lid (L.singleton lab) L.empty CD.empty x lab; E.newEnvVar lab)
		    | (NONE, _, false) => (* FREE ID *)
		      (handleFreeIdent lid true; E.newEnvVar lab)
		    | _ => E.newEnvVar lab)
	       | SOME (lid', false) => E.newEnvVar lab)
	  | solveopnsem (lid, lab, E.ISI) bmon = (* signature inclusion *)
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
			E.emenv
			opnenv


	(* ====== ACCESSOR SOLVER ====== *)

	and solveacc (E.ACCVAR ({lid, sem, class, lab}, labs, stts, deps)) l =
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
			       val c2    = E.genCstClAll class cl  labs1 stts0 deps0
			   (*val _     = D.printdebug2 (S.printState state)*)
			   (*val _     = D.printdebug2 (I.printLid lid ^ " " ^ L.printLab l ^ "\n" ^ T.printty sem ^ "\n" ^ T.printty bind ^ "\n" ^ T.printty bind2)*)
  		           (*val _     = D.printdebug2 (CL.toString class ^ "\n" ^ CL.toString cl)*)
			   in fsimplify [c1, c2] l
			   end
		    | (_, SOME ((id1, lab1), (env, labs', stts', deps')), true) =>
		      handleUnmatched lid labs stts deps ((id1, lab1), (env, labs', stts', deps')) l
		    | (NONE, _, false) => (* FREE ID *)
		      handleFreeIdent lid false
		    | _ => ())
	       | SOME (lid', false) => ())
	  | solveacc (E.ACCETV ({lid, sem, class, lab}, labs, stts, deps)) l =
	    (case filterLid lid filters of
		 NONE => ()
	       | SOME (_, true) =>
		 (case S.getValStateIdTv state lid false of
		      (SOME (({id, bind, lab, poly, class = CL.ANY}, _, _, _), _), _, _) => ()
		    | (SOME (({id, bind = (bind, b), lab = l, poly, class}, labs', stts', deps'), _), _, _) =>
		      let val (labs0, stts0, deps0) = unionLabs (labs, stts, deps) (labs', stts', deps')
			  val ty1 = T.V (sem, NONE, T.POLY)
			  val ty2 = T.consV (freshTyVar bind poly)
			  (*(2010-06-29) The order in the constraint actually matters to get the
			   * 'too general in signature' errors.  This needs to be fixed.
			   * Constraint solving shouldn't depend on the order in constraints. *)
			  (*val _   = D.printdebug2 (T.printty ty1 ^ "\n" ^ T.printty ty2)*)
			  val c   = E.genCstTyAll ty1 ty2 labs0 stts0 deps0
		      in fsimplify [c] l
		      end
		    | _ => ())
	       | SOME (lid', false) => ())
	  | solveacc (E.ACCTYP ({lid, sem, class, lab}, labs, stts, deps)) l =
	    (case filterLid lid filters of
		 NONE => ()
	       | SOME (_, true) =>
		 (case S.getValStateIdTy state lid false of
		      (SOME (({id, bind = (bind, _, _), lab = l, poly, class = CL.ANY}, _, _, _), _), _, _) =>
		      (*(2010-06-16)We used to return () but we need to do something
		       * else to catch the arity errors.*)
		      (*let val (tf, labs', stts', deps') = buildTyFunAr state bind lab L.empty L.empty CD.empty
			    (*val _ = D.printdebug2 (S.printState state)*)
			    (*val _ = D.printdebug2 (T.printtyf bind)*)
			    val (labs0, stts0, deps0) = unionLabs (labs, stts, deps) (labs', stts', deps')
			    val c = E.genCstTfAll sem tf (L.union labs0 (I.getLabs lid)) stts0 deps0
			in fsimplify [c] l
			end*)
		      let val sq   = S.getValStateAr state lid (SOME l)
			  val labs = L.union labs (I.getLabs lid)
			  val tf   = T.TFC (sq, T.newV (), lab)
			  val c    = E.genCstTfAll sem tf labs stts deps
		      in fsimplify [c] l
		      end
		    | (SOME (({id, bind = (bind, _, _), lab = l, poly, class}, labs', stts', deps'), _), _, _) =>
		      let val (labs0, stts0, deps0) = unionLabs (labs, stts, deps) (labs', stts', deps')
			  val labs1 = L.union labs0 (I.getLabs lid)
			  val bind1 = freshTyFun bind true
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
			       val tf   = T.TFC (sq, T.newV (), lab)
			       val c    = E.genCstTfAll sem tf labs stts deps
			       val _    = if enum then S.updateStateFr state (I.getLeftId lid) else () (* FREE ID *)
			   in fsimplify [c] l
			   end)
	       | SOME (lid', false) => ())
	  | solveacc (E.ACCOVC ({lid, sem, class, lab}, labs, stts, deps)) l =
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
	  | solveacc (E.ACCSTR ({lid, sem, class, lab}, labs, stts, deps)) l =
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
	  | solveacc (E.ACCSIG ({lid, sem, class, lab}, labs, stts, deps)) l =
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
	  | solveacc (E.ACCFUN ({lid, sem = (env1, env2), class, lab}, labs, stts, deps)) l =
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
		 in handleSimplify (ERR.consPreError ERR.dummyId labs0 deps0 ek stts0 l) [] l
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
	  | fsimplify ((E.CSTTYF ((T.TFD (tf1, labs1, stts1, deps1), tf2), labs, stts, deps)) :: cs') l = fsimplify ((E.CSTTYF ((tf1, tf2), L.union labs1 labs, L.union stts1 stts, CD.union deps1 deps)) :: cs') l
	  | fsimplify ((E.CSTTYP ((T.TD  (ty1, labs1, stts1, deps1), ty2), labs, stts, deps)) :: cs') l = fsimplify ((E.CSTTYP ((ty1, ty2), L.union labs1 labs, L.union stts1 stts, CD.union deps1 deps)) :: cs') l
	  | fsimplify ((E.CSTTYN ((T.ND  (tn1, labs1, stts1, deps1), tn2), labs, stts, deps)) :: cs') l = fsimplify ((E.CSTTYN ((tn1, tn2), L.union labs1 labs, L.union stts1 stts, CD.union deps1 deps)) :: cs') l
	  | fsimplify ((E.CSTSEQ ((T.SD  (sq1, labs1, stts1, deps1), sq2), labs, stts, deps)) :: cs') l = fsimplify ((E.CSTSEQ ((sq1, sq2), L.union labs1 labs, L.union stts1 stts, CD.union deps1 deps)) :: cs') l
	  | fsimplify ((E.CSTROW ((T.RD  (rt1, labs1, stts1, deps1), rt2), labs, stts, deps)) :: cs') l = fsimplify ((E.CSTROW ((rt1, rt2), L.union labs1 labs, L.union stts1 stts, CD.union deps1 deps)) :: cs') l
	  | fsimplify ((E.CSTLAB ((T.LD  (lt1, labs1, stts1, deps1), lt2), labs, stts, deps)) :: cs') l = fsimplify ((E.CSTLAB ((lt1, lt2), L.union labs1 labs, L.union stts1 stts, CD.union deps1 deps)) :: cs') l
	  (**)
	  | fsimplify ((E.CSTTYF ((tf1, T.TFD (tf2, labs1, stts1, deps1)), labs, stts, deps)) :: cs') l = fsimplify ((E.CSTTYF ((tf1, tf2), L.union labs1 labs, L.union stts1 stts, CD.union deps1 deps)) :: cs') l
	  | fsimplify ((E.CSTTYP ((ty1, T.TD  (ty2, labs1, stts1, deps1)), labs, stts, deps)) :: cs') l = fsimplify ((E.CSTTYP ((ty1, ty2), L.union labs1 labs, L.union stts1 stts, CD.union deps1 deps)) :: cs') l
	  | fsimplify ((E.CSTTYN ((tn1, T.ND  (tn2, labs1, stts1, deps1)), labs, stts, deps)) :: cs') l = fsimplify ((E.CSTTYN ((tn1, tn2), L.union labs1 labs, L.union stts1 stts, CD.union deps1 deps)) :: cs') l
	  | fsimplify ((E.CSTSEQ ((sq1, T.SD  (sq2, labs1, stts1, deps1)), labs, stts, deps)) :: cs') l = fsimplify ((E.CSTSEQ ((sq1, sq2), L.union labs1 labs, L.union stts1 stts, CD.union deps1 deps)) :: cs') l
	  | fsimplify ((E.CSTROW ((rt1, T.RD  (rt2, labs1, stts1, deps1)), labs, stts, deps)) :: cs') l = fsimplify ((E.CSTROW ((rt1, rt2), L.union labs1 labs, L.union stts1 stts, CD.union deps1 deps)) :: cs') l
	  | fsimplify ((E.CSTLAB ((lt1, T.LD  (lt2, labs1, stts1, deps1)), labs, stts, deps)) :: cs') l = fsimplify ((E.CSTLAB ((lt1, lt2), L.union labs1 labs, L.union stts1 stts, CD.union deps1 deps)) :: cs') l
	  (**)
	  | fsimplify ((E.CSTENV ((env1, E.ENVDEP (env2, ls', deps', ids')), ls, deps, ids)) :: cs') l = simplify ((E.CSTENV ((env1, env2), L.union ls ls', L.union deps deps', CD.union ids ids')) :: cs') l
	  | fsimplify ((E.CSTENV ((E.ENVDEP (env1, ls', deps', ids'), env2), ls, deps, ids)) :: cs') l = simplify ((E.CSTENV ((env1, env2), L.union ls ls', L.union deps deps', CD.union ids ids')) :: cs') l
	  (**)
	  | fsimplify ((E.CSTTYP ((T.C (tn1, sq1, l1), T.C (tn2, sq2, l2)), ls, deps, ids)) :: cs') l =
	    if (T.isBaseTy tn1 andalso not (T.isBaseTy tn2))
	       orelse
	       (T.isBaseTy tn2 andalso not (T.isBaseTy tn1))
	    (* one is a tnv the other one is a -> or a * *)
	    then (* a tynamevar can never be an arrow or record *)
		if (T.isArrowTy tn1 andalso T.isDecTy tn1 andalso T.isVarTyName tn2)
		   orelse
		   (T.isArrowTy tn2 andalso T.isDecTy tn2 andalso T.isVarTyName tn1)
		   orelse
		   (T.isArrowTy tn1 andalso T.isDecTy tn1 andalso T.isExcTy tn2)
		   orelse
		   (T.isArrowTy tn2 andalso T.isDecTy tn2 andalso T.isExcTy tn1)
		then let val ek  = EK.ConsArgNApp (L.toInt l1, L.toInt l2)
			 val err = ERR.consPreError ERR.dummyId ls ids ek deps l
		     in handleSimplify err cs' l
		     end
		else if (T.isArrowTy tn1 andalso T.isPatTy tn1 andalso T.isVarTyName tn2)
			orelse
			(T.isArrowTy tn2 andalso T.isPatTy tn2 andalso T.isVarTyName tn1)
			orelse
			(T.isArrowTy tn1 andalso T.isPatTy tn1 andalso T.isExcTy tn2)
			orelse
			(T.isArrowTy tn2 andalso T.isPatTy tn2 andalso T.isExcTy tn1)
		then let val ek  = EK.ConsNArgApp (L.toInt l1, L.toInt l2)
			 val err = ERR.consPreError ERR.dummyId ls ids ek deps l
		     in handleSimplify err cs' l
		     end
		else let val name1 = T.tntyToTyCon tn1
			 val name2 = T.tntyToTyCon tn2
			 val ek    = EK.TyConsClash ((L.toInt l1, T.tynameToInt name1), (L.toInt l2, T.tynameToInt name2))
			 val err   = ERR.consPreError ERR.dummyId ls ids ek deps l
		     (*
		      * (2011-02-17) We don't actually need to try to remove l from the error because it
		      * has to be in the error.  Is that the only one we're sure about?
		      * Are we not sure that the end points have to be in the error as well?
		      *)
		     (*val _ = if T.isVarTyName tn1 orelse T.isVarTyName tn2
			       then D.printdebug2 ("foo")
			       else ()*)
		     (*val _ = D.printdebug2 (S.printState state)*)
		     in handleSimplify err cs' l
		     end
	    else let val c1 = E.genCstTnAll tn1 tn2 ls deps ids
		     val c2 = E.genCstSqAll sq1 sq2 ls deps ids
		 (*val _ = D.printdebug2 (T.printseqty sq1 ^ "\n" ^ T.printseqty sq2)*)
		 in fsimplify (c1 :: c2 :: cs') l
		 end
	  | fsimplify ((E.CSTTYN ((T.NC (tn1, b1, l1), T.NC (tn2, b2, l2)), ls, deps, ids)) :: cs') l =
	    if T.eqTyname tn1 tn2
	    then fsimplify cs' l
	    else let val ek  = EK.TyConsClash ((L.toInt l1, T.tynameToInt tn1), (L.toInt l2, T.tynameToInt tn2))
		     val err = ERR.consPreError ERR.dummyId ls ids ek deps l
		 in handleSimplify err cs' l
		 end
	  | fsimplify ((E.CSTSEQ ((T.SC (rtl1, b1, l1), T.SC (rtl2, b2, l2)), ls, deps, ids)) :: cs') l =
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
			val rtl1' = map (fn row => T.RD (row, ls, deps, ids)) rtl1
			val rtl2' = map (fn row => T.RD (row, ls, deps, ids)) rtl2
			val sr = ((rtl1', b1, []), (rtl2', b2, []))
			val (cs'', err) = S.updateRecOne state sr
		    (*val _ = print ((E.printlcs cs'') ^ "\n")*)
		    (* don't we get unwanted occurrences of srec by doing that? *)
		    in case err of
			   [] => fsimplify (cs'' @ cs') l
			 | [((ek1, ek2, ek3, ek4), ell, edeps, eids)] =>
			   handleSimplify (ERR.consPreError ERR.dummyId
							    ell
							    eids
							    (EK.LabTyClash (ek1, ek2, ek3, ek4))
							    edeps
							    l)
					  cs'
					  l
			 | _ => raise EH.DeadBranch ""
		    end
               else let val ek  = EK.ArityClash ((L.toInt l1, n1), (L.toInt l2, n2))
			val err = ERR.consPreError ERR.dummyId ls ids ek deps l
		    in handleSimplify err cs' l
		    end
	    end
	  | fsimplify ((E.CSTTYP ((ty1 as T.V (tv1, b1, p1), ty2 as T.V (tv2, b2, p2)), ls, deps, ids)) :: cs') l =
	    let fun continue () =
		    case S.getValStateTv state tv1 of
			NONE =>
			let val t = T.V (tv2, if Option.isSome b2 then b2 else b1, p2)
			    (*val _ = BI.updateMono state tv1 t ls deps ids*)
			    val _ = if occurs (CT (tv1, t)) [T (tv2, ls, deps, ids)] 0 l
				    then S.updateStateTv state tv1 (T.TD (t, ls, deps, ids))
				    else ()
			in fsimplify cs' l
			end
		      | SOME ty =>
			let val bop = if Option.isSome b2 then b2 else b1
			    val t   = T.V (tv2, bop, p2)
			    val c   = E.genCstTyAll ty t ls deps ids
			in fsimplify (c :: cs') l
	    		end
	    (*val _ = D.printdebug2 (T.printty ty1 ^ "\n" ^ T.printty ty2)*)
	    in if T.eqTyvar tv1 tv2
	       then fsimplify cs' l
	       else case (b1, b2) of
			(SOME _, NONE) => fsimplify ((E.CSTTYP ((T.V (tv2, b2, p2), T.V (tv1, b1, p1)), ls, deps, ids)) :: cs') l
		      | (SOME (id1, lab1), SOME (id2, lab2)) =>
			if I.eqId id1 id2
			then continue ()
			else let val ek  = EK.TyConsClash ((L.toInt lab1, T.tynameToInt (T.dummytyname ())), (L.toInt lab2, T.tynameToInt (T.dummytyname ())))
				 val err = ERR.consPreError ERR.dummyId ls ids ek deps l
			     in handleSimplify err cs' l
			     end
		      | _ => continue ()
	    end
	  | fsimplify ((E.CSTTYP ((T.E (n1, tv1, l1), T.E (n2, tv2, l2)), ls, deps, ids)) :: cs') l =
	    if I.eqId n1 n2 (*tv1 = tv2*)
	    then fsimplify cs' l
	    else let val ek  = EK.TyConsClash ((L.toInt l1, T.tynameToInt (T.dummytyname ())), (L.toInt l2, T.tynameToInt (T.dummytyname ())))
		     val err = ERR.consPreError ERR.dummyId ls ids ek deps l
		 in handleSimplify err cs' l
		 end
	  | fsimplify ((E.CSTTYP ((T.E (n, tv, l1), T.C (tn, sq, l2)), ls, deps, ids)) :: cs') l =
	    let val ek  = EK.TyConsClash ((L.toInt l1, T.tynameToInt (T.dummytyname ())), (L.toInt l2, T.tynameToInt (T.tntyToTyCon tn)))
		val err = ERR.consPreError ERR.dummyId ls ids ek deps l
	    in handleSimplify err cs' l
	    end
	  | fsimplify ((E.CSTTYN ((T.NV tnv1, T.NV tnv2), ls, deps, ids)) :: cs') l =
	    if T.eqTynamevar tnv1 tnv2
	    then fsimplify cs' l
	    else (case S.getValStateTn state tnv1 of
                      NONE =>
                      let val _ = case S.getValStateTn state tnv2 of
				      NONE => S.updateStateTn state tnv1 (T.ND (T.NV tnv2, ls, deps, ids))
				    | _ => ()
                      in fsimplify cs' l
                      end
		    | SOME tn =>
                      let val c = E.genCstTnAll tn (T.NV tnv2) ls deps ids
                      in fsimplify (c :: cs') l
                      end)
	  | fsimplify ((E.CSTSEQ ((T.SV sqv1, sq2 as (T.SV sqv2)), ls, deps, ids)) :: cs') l =
	    if T.eqSeqvar sqv1 sqv2
	    then fsimplify cs' l
	    else (case S.getValStateSq state sqv1 of
                      NONE =>
                      let val _ = if occurs (CS (sqv1, sq2)) [S (sqv2, ls, deps, ids)] 0 l
				  then S.updateStateSq state sqv1 (T.SD (T.SV sqv2, ls, deps, ids))
				  else ()
                      in fsimplify cs' l
                      end
		    | SOME sq =>
                      let val c = E.genCstSqAll sq (T.SV sqv2) ls deps ids
                      in fsimplify (c :: cs') l
                      end)
	  (*| fsimplify ((E.CSTIT ((ty, {id, class, lab}), ls, deps, ids)) :: cs') l =
	   let val xs = if CL.classIsVID class
			then S.getValStateIdVa state id false
			else if CL.classIsTYCON class
			then S.getValStateIdTy state id false
			else (print (CL.toString class ^ " " ^ O.printelt lab);
			      raise EH.DeadBranch "wrong kind of identifier")
	       val labsid = I.getLabs id
	       (*val _ = D.printdebug2 (I.printLid id)*)
	       (*val _ = D.printdebug2 (S.printState state)*)
	       fun genUnboundCs () =
		   ((*D.printdebug2 ("[unbound]");*)
		    if FI.testtodos filters labsid
		    then if CL.classIsTYP class
			 (* arity clashes for unbound type constructors *)
			 then S.updateStateTc state id (E.CCL (class, lab, id, false), L.union labsid ls, deps, ids)
			 else if CL.classIsCON class andalso FI.testtodos filters (CL.getClassCON class)
			 then S.updateStateAp state id (E.CCL (class, lab, id, false), L.union labsid ls, deps, ids)
			 else []
		    else [])
	   in case xs of
		  (SOME ({id = i, scope, bind, lab = l', poly, class = cl}, labs', sts', cds'), _) =>
		  (* There might have some labs' which do not satisfy FI.testtodo filters *)
		  if FI.testtodos filters labsid
		  then let val ls = L.union ls labsid
			   val bcomp = CL.compatible cl class
			   val btest = FI.testtodo filters l'
			   val (cs1, labs0, deps0, asmp0) =
			       if bcomp
				  andalso not (CL.classIsAVI cl)
				  andalso btest (*(2010-02-22)We can't just skip this test because of the type constructors*)
			       then let val bind' = T.labelBuiltin bind lab
					val domge = S.getDomGe state
					val (ty', _, asmp1, deps1, ll1) = BI.freshTyPoly poly bind' domge E.DEC true
					(*val _ = D.printdebug2 (S.printState state)*)
					(*val _ = D.printdebug2 (I.printId i ^ "\n" ^ O.toString labs' ^ "\n" ^ T.printty bind' ^ "\n" ^ T.printty ty')*)
					val labs0 = L.unions [ll1,   ls,   labs']
					val deps0 = L.unions [deps1, deps, sts']
					val asmp0 = CD.unions [asmp1, ids,  cds']
					val c     = E.genCstTyAll ty ty' labs0 deps0 asmp0
				    in (c :: cs', labs0, deps0, asmp0)
				    end
			       else if btest andalso not (CL.classIsAVI cl)
			       then (cs', L.union ls labs', L.union deps sts', CD.union ids cds')
			       else (cs', ls, deps, ids)
			   val cs2 = Option.getOpt (Option.map (fn cv => ((*D.printdebug2 (O.toString labs0 ^ " " ^ T.printty bind ^ " " ^ CL.toString cl ^ " " ^ CL.toString class ^ " " ^ O.printelt l');*)
									  [E.genCstClAll (E.VCL cv) (E.CCL (class, lab, id, false)) labs0 deps0 asmp0]))
							       (CL.getClassVar cl), [])
		       in fsimplify (cs2 @ cs1) l
		       end
		  else fsimplify cs' l
		(* Below the meanind of the record is slightly different,
		 * it means that (id,lab) is missing from the env bind (which is a ENVCON).
		 * If env is a ENVVAR then we might want to have it equal to a INJ. *)
		| (_, SOME ({id = i, scope, bind, lab, poly, class = cl}, labs', sts', cds')) =>
		  ((*D.printdebug2 ("[error]");*)
		   if FI.testtodo filters lab
		      andalso
		      FI.testtodos filters labs' (*(2010-03-03)We just want to check the ones in O.inter labsid labs'*)
		   then if E.completeEnv bind
			then let val ek = EK.Unmatched ((lab, I.toInt i),
							E.getLabsIdsEnv bind 2,
							E.getLabEnv bind)
				 val labs0 = L.union ls labs'
				 val deps0 = L.union deps sts'
				 val asmp0 = CD.union ids cds'
			     in handleSimplify (ERR.consPreError ERR.dummyId labs0 asmp0 ek deps0 l) cs' l
			     end
			else (case E.getEnvVar bind of
				  SOME ev =>
				  let fun flid lid =
					  let val env   = E.generateINJ lid ty class
					      val labs0 = L.union ls labs'
					      val deps0 = L.union deps sts'
					      val asmp0 = CD.union ids cds'
					  (*val _ = D.printdebug2 ("[incomplete structure]")*)
					  in [E.genCstEvAll (E.consEnvVar ev) env labs0 deps0 asmp0] end
				      (*val _ = D.printdebug2 ("[incomplete structure]")*)
				      val cs2 = Option.getOpt (Option.map flid (I.getSubLid id i lab), [])
				  in fsimplify (cs2 @ cs') l
				  end
				| _ => fsimplify cs' l)
		   else fsimplify cs' l)
		| _ => fsimplify ((genUnboundCs ()) @ cs') l
	   end
	  | fsimplify ((E.CSTIO ((sq, {id, class, lab}), ls, deps, ids)) :: cs') l =
	    let val xs = if CL.classIsOC class
			 then S.getValStateIdOc state id true
			 else raise EH.DeadBranch "wrong kind of identifier"
		val labsid = I.getLabs id
	    in case xs of
		   (SOME ({id, scope, bind, lab, poly, class}, labs', sts', cds'), _) =>
		   if FI.testtodo filters lab
		      andalso
		      FI.testtodos filters labsid
		   then let val ls = O.cons lab (L.union ls labsid)
			    val labs0 = L.union ls labs'
			    val deps0 = L.union deps sts'
			    val asmp0 = CD.union ids cds'
			    val c     = E.genCstSqAll sq bind labs0 deps0 asmp0
			in fsimplify (c :: cs') l
			end
		   else fsimplify cs' l
		 | (_, SOME ({id, scope, bind, lab, poly, class}, labs', sts', cds')) =>
		   if E.completeEnv bind
		      andalso
		      FI.testtodo filters lab
		      andalso
		      FI.testtodos filters labs' (*(2010-03-03)See similar note for CSTIT*)
		   then let val ek = EK.Unmatched ((lab, I.toInt id),
						   E.getLabsIdsEnv bind 3,
						   E.getLabEnv bind)
			    val labs0 = L.union ls labs'
			    val deps0 = L.union deps sts'
			    val asmp0 = CD.union ids cds'
			in handleSimplify (ERR.consPreError ERR.dummyId labs0 asmp0 ek deps0 l) cs' l
			end
		   else fsimplify cs' l
		 | _ => fsimplify cs' l
	    end
	  | fsimplify ((E.CSTIS ((env, {id, class, lab}), ls, deps, ids)) :: cs') l =
	    let val xs = if CL.classIsSTR class
			 then S.getValStateIdSt state id true
			 else if CL.classIsSIG class
			 then S.getValStateIdSi state id true
			 else raise EH.DeadBranch "wrong kind of identifier"
		val labsid = I.getLabs id
	    in case xs of
		   (SOME ({id, scope, bind, lab, poly, class}, labs', sts', cds'), _) =>
		   if FI.testtodo filters lab
		      andalso
		      FI.testtodos filters labsid
		   then let val ls = O.cons lab (L.union ls labsid)
			    val labs0 = L.union ls labs'
			    val deps0 = L.union deps sts'
			    val asmp0 = CD.union ids cds'
			    val c     = E.genCstEvAll env bind labs0 deps0 asmp0
			in fsimplify (c :: cs') l
			end
		   else fsimplify cs' l
		 | (_, SOME ({id, scope, bind, lab, poly, class}, labs', sts', cds')) =>
		   if E.completeEnv bind
		      andalso
		      FI.testtodo filters lab
		      andalso
		      FI.testtodos filters labs' (*(2010-03-03)See similar note for CSTIT*)
		   then let val ek = EK.Unmatched ((lab, I.toInt id),
						   E.getLabsIdsEnv bind 4,
						   E.getLabEnv bind)
			    val labs0 = L.union ls labs'
			    val deps0 = L.union deps sts'
			    val asmp0 = CD.union ids cds'
			in handleSimplify (ERR.consPreError ERR.dummyId labs0 asmp0 ek deps0 l) cs' l
			end
		   else fsimplify cs' l
		 | _ => fsimplify cs' l
	    end
	  | fsimplify ((E.CSTIF _) :: cs') l = raise EH.TODO*)
	  | fsimplify ((E.CSTTYP ((tyv as T.V (tv, b, p), ty), ls, deps, ids)) :: cs') l =
	    let (*val _ = D.printdebug2 (T.printty tyv ^ "\n" ^ T.printty ty ^ "\n" ^ S.printState state)*)
		fun reportGenError () =
		    if Option.isSome b       (* Type variable comes from an explicit type variable        *)
		       andalso isSigVsStr () (* We're dealing with constraints on signature vs. structure *)
		       andalso T.isTyC ty
		    then (* we're dealing with a too general signature's structure *)
			let val l1  = #2 (Option.valOf b)           handle Option => raise EH.DeadBranch ""
			    val l2  = Option.valOf (T.getTyLab  ty) handle Option => raise EH.DeadBranch ""
			    val tn  = Option.valOf (T.getTyName ty) handle Option => raise EH.DeadBranch ""
			    val fek = if isSigVsStrTyp () then EK.TyFunClash else EK.NotGenClash
			    val ek  = fek ((L.toInt l1, T.tyvarToInt tv), (L.toInt l2, T.tynameToInt (T.tntyToTyCon tn)))
			    val err = ERR.consPreError ERR.dummyId ls ids ek deps l
			(*val _ = D.printdebug2 (T.printty tyv)*)
			(* We should raise another error here:
			 * something like, signature too general. *)
			in handleSimplify err cs' l
			end
		    else ()
		fun updateFlex (T.V (tv, NONE, p)) flex = T.V (tv, flex, p)
		  | updateFlex (T.TD (ty, labs, stts, deps)) flex =
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
	  | fsimplify ((E.CSTTYN ((T.NV tnv1, tnty2), ls, deps, ids)) :: cs') l =
	    (case S.getValStateTn state tnv1 of
		 NONE =>
		 let val _ = S.updateStateTn state tnv1 (T.ND (tnty2, ls, deps, ids))
		 in fsimplify cs' l end
	       | SOME tnty =>
		 let val c = E.genCstTnAll tnty tnty2 ls deps ids
		 in fsimplify (c :: cs') l
		 end)
	  | fsimplify ((E.CSTSEQ ((T.SV sqv1, sq2), ls, deps, ids)) :: cs') l =
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
	  | fsimplify ((E.CSTROW ((T.RV rv, rt as T.RC _), ls, deps, ids)) :: cs') l =
	    (case S.getValStateRt state rv of
		 NONE =>
		 let val (rho, n) = decomptyrow rt ls deps ids
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
							 edeps
							 l)
				       cs'
				       l
		      | _  => raise EH.DeadBranch ""
		 end
               | SOME rt' => raise EH.DeadBranch "")
	  (* we update and store *)
	  (* if it's 2 variables we raise deadbranch, same if it's 2 cons *)
	  | fsimplify ((E.CSTLAB ((T.LV lv, lt as T.LC _), ls, deps, ids)) :: cs') l =
	    (case S.getValStateLt state lv of
		 NONE =>
		 let val _ = S.updateStateLt state lv (T.LD (lt, ls, deps, ids))
		     val (cs'', err) = S.updateRec state
		 in case err of
			[] => fsimplify (cs'' @ cs') l
		      | [((ek1, ek2, ek3, ek4), ell, edeps, eids)] => (* Can that happen? *)
			handleSimplify (ERR.consPreError ERR.dummyId
							 ell
							 eids
							 (EK.LabTyClash (ek1, ek2, ek3, ek4))
							 edeps
							 l)
				       cs'
				       l
		      | _  => raise EH.DeadBranch ""
		 end
               | SOME lt' => raise EH.DeadBranch "")
	  | fsimplify ((E.CSTTYP ((t1 as T.A (T.TFV tfv, seqty, lab), t2), ls, deps, ids)) :: cs') l =
	    (case S.getValStateTf state tfv of
                 NONE => fsimplify cs' l
	       | SOME tf =>
                 let val c = E.genCstTyAll (T.A (tf, seqty, lab)) t2 ls deps ids
                 in fsimplify (c :: cs') l
                 end)
	  | fsimplify ((E.CSTTYP ((t1 as T.A (T.TFC (seqty1, ty1, lab1), seqty2, lab2), ty2), ls, deps, ids)) :: cs') l =
	    let val labs = L.cons lab1 (L.cons lab2 ls)
		(*val _ = D.printdebug2 (T.printty ty1)*)
		val c1 = E.genCstSqAll seqty1 seqty2 labs deps ids
		val c2 = E.genCstTyAll ty1 ty2 labs deps ids
	    in fsimplify (c1 :: c2 :: cs') l
	    end
	  | fsimplify ((E.CSTTYP ((t1 as T.A (T.TFD (tf2, labs2, stts2, deps2), seqty2, lab2), ty2), labs, stts, deps)) :: cs') l =
	    fsimplify ((E.CSTTYP ((T.A (tf2, seqty2, lab2), ty2), L.union labs labs2, L.union stts stts2, CD.union deps deps2)) :: cs') l
	  | fsimplify ((E.CSTTYP ((tc as (T.C (tnty, sq, lab1)), to as T.OR (sq', idor, poly, orKind, lab2)), ls, deps, ids)) :: cs') l =
	    (* Build the sq' in case it's a variable. *)
	    let fun checkTn tnty labs stts deps seq =
		    case tnty of
			T.NV _ => fsimplify cs' l
		      | T.NC (tn, _, l') =>
			let fun getErr () =
				let val tnerr = (L.toInt l', T.tynameToInt tn)
				    val (tnerrs, labs, stts, deps) = gatherAllTnSq seq
				    val kind =
					case orKind of
					    T.VAL (id, lab) => EK.Overload ((L.toInt lab, I.toInt id), tnerr, tnerrs)
					  | T.CST (st, id, lab) => EK.OverloadCst ((L.toInt lab, I.toInt id, st), tnerr, tnerrs)
				in (kind, labs, stts, deps)
				end
			(*val (seq', _, _, _) = T.stripDepsSq seq*)
			(*val _ = D.printdebug2 (T.printtnty tnty ^ "\n" ^ T.printseqty seq ^ "\n" ^ T.printseqty seq')*)
			(*val _ = D.printdebug2 (S.printState state ^ "\n" ^
						 T.printty'   tc    ^ "\n" ^
						 T.printty'   to)*)
			(* we should check that tl is complete! *)
			in case findInOrSq tn [] seq of
			       ([], false, _) =>
			       let val (kind, labs2, stts2, deps2) = getErr ()
				   val labs' = L.union  labs labs2
				   val stts' = L.union  stts stts2
				   val deps' = CD.union deps deps2
				   val err   = ERR.consPreError ERR.dummyId labs' deps' kind stts' l
			       in handleSimplify err cs' l
			       end
			     | ([], true, _) => fsimplify cs' l
			     | (((t, path) :: _), true, _) =>
			       let val _ = S.updateStateOr state idor ([path], labs, stts, deps)
				   (*val _ = D.printdebug2 (O.toString ls ^ " " ^ T.printty t ^ " " ^ O.printelt lab2)*)
				   val c = E.genCstTyAll tc t labs stts deps
			       in fsimplify (c :: cs') l end
			     | _ => raise EH.DeadBranch ""
			end
		      | T.ND (tn1, labs1, stts1, deps1) => checkTn tn1
								   (L.union labs labs1)
								   (L.union stts stts1)
								   (CD.union deps deps1)
								   seq
	    (*val _ = D.printdebug2 (T.printty to)*)
	    in case S.getValStateOr state idor of
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
			(*val _ = D.printdebug2 (O.toString ls ^ "\n" ^ O.toString ls')*)
			in fsimplify (c :: cs') l
			end)
		 | SOME (paths, ls', deps', ids') =>
		   let val labs0 = L.union ls ls'
		       val stts0 = L.union deps deps'
		       val deps0 = CD.union ids ids'
		   in checkTn tnty labs0 stts0 deps0 (selectPaths paths sq')
		   end
	    end
	  | fsimplify ((E.CSTTYP ((T.E (n, tv, l1), T.OR (sq, _, poly, orKind, _)), ls, deps, ids)) :: cs') l =
	    let fun getErr () =
		    let val tnerr  = (L.toInt l1, T.tynameToInt (T.dummytyname ()))
			val (tnerrs, labs, stts, deps) = gatherAllTnSq sq
			val kind =
			    case orKind of
				T.VAL (id, lab) => EK.Overload ((L.toInt lab, I.toInt id), tnerr, tnerrs)
			      | T.CST (st, id, lab) => EK.OverloadCst ((L.toInt lab, I.toInt id, st), tnerr, tnerrs)
		    in (kind, labs, stts, deps)
		    end
	    in if isAllSq sq
	       then let val (kind, labs1, stts1, deps1) = getErr ()
			val labs' = L.union  labs1 ls
			val stts' = L.union  stts1 deps
			val deps' = CD.union deps1 ids
		    in handleSimplify (ERR.consPreError ERR.dummyId labs' deps' kind stts' l) cs' l
		    end
	       else fsimplify cs' l
	    end
	  | fsimplify ((E.CSTROW _) :: cs') l = raise EH.DeadBranch ""
	  | fsimplify ((E.CSTLAB _) :: cs') l = raise EH.DeadBranch ""
	  | fsimplify ((E.CSTTYP ((ty1 as T.OR (sq1, i1, poly1, orKind1, lab1),
				   ty2 as T.OR (sq2, i2, poly2, orKind2, lab2)),
				  ls, deps, ids)) :: cs') l =
	    let fun match seq1 seq2 ls deps ids =
		    case tryToMatchOrs seq1 seq2 of
			(_, _, false) => (* tryToMatchOrs can be simplified as it is just a checking *)
			(case (orKind1, orKind2) of
			     (T.CST (st1, id1, lab1), T.CST (st2, id2, lab2)) =>
			     let val (tnerrs1, labs1, stts1, deps1) = gatherAllTnSq seq1
				 val (tnerrs2, labs2, stts2, deps2) = gatherAllTnSq seq2
				 val labs = L.union (L.union labs1 labs2) ls
				 val stts = L.union (L.union stts1 stts2) deps
				 val deps = CD.union (CD.union deps1 deps2) ids
				 val ek = EK.OverloadClash ((L.toInt lab1, I.toInt id1, st1),
							    tnerrs1,
							    (L.toInt lab2, I.toInt id2, st2),
							    tnerrs2)
			     in handleSimplify (ERR.consPreError ERR.dummyId labs deps ek stts l) cs' l
			     end
			   | (T.CST (st1, id1, lab1), T.VAL (id2, lab2)) =>
			     let val (tnerrs1, labs1, stts1, deps1) = gatherAllTnSq seq1
				 val (tnerrs2, labs2, stts2, deps2) = gatherAllTnSq seq2
				 val labs = L.union (L.union labs1 labs2) ls
				 val stts = L.union (L.union stts1 stts2) deps
				 val deps = CD.union (CD.union deps1 deps2) ids
				 val ek = EK.OverloadIdCst ((L.toInt lab2, I.toInt id2),
							    tnerrs2,
							    (L.toInt lab1, I.toInt id1, st1),
							    tnerrs1)
			     in handleSimplify (ERR.consPreError ERR.dummyId labs deps ek stts l) cs' l
			     end
			   | (T.VAL (id1, lab1), T.CST (st2, id2, lab2)) =>
			     let val (tnerrs1, labs1, stts1, deps1) = gatherAllTnSq seq1
				 val (tnerrs2, labs2, stts2, deps2) = gatherAllTnSq seq2
				 val labs = L.union (L.union labs1 labs2) ls
				 val stts = L.union (L.union stts1 stts2) deps
				 val deps = CD.union (CD.union deps1 deps2) ids
				 val ek = EK.OverloadIdCst ((L.toInt lab1, I.toInt id1),
							    tnerrs1,
							    (L.toInt lab2, I.toInt id2, st2),
							    tnerrs2)
			     in handleSimplify (ERR.consPreError ERR.dummyId labs deps ek stts l) cs' l
			     end
			   | _ => fsimplify cs' l)
		      | ((typaths1, f1), (typaths2, f2), true) =>
			let fun upd id typaths = S.updateStateOr state id (map (fn (_, y) => y) typaths, ls, deps, ids)
			    (*val _ = D.printdebug2 (Bool.toString f1 ^ " " ^
						   Bool.toString f2 ^ " " ^
						   Int.toString (List.length typaths1) ^ " " ^
						   Int.toString (List.length typaths2) ^ "\n" ^
						   T.printty ty1 ^ "\n" ^
						   T.printty ty2 ^ "\n" ^
						   T.printseqty seq1 ^ "\n" ^
						   T.printseqty seq2)*)
			    val _ = if f2  (* f2 means that we've found a concrete match of seq1 in seq2 *)
				       andalso isFullOr seq2 (* means that no row of seq2 is a var *)
				       andalso (not (T.isPoly poly1))
				       (* why do we insist that poly1 is not poly?
					* It seems to be why we don't get an error
					* for, e.g., '1 + 1.1'.
					* What could go wrong with POLY marker?
					* Is the POLY marker only used when 'over' is turned ON? *)
				       andalso not (List.null typaths1)
				    then upd i1 typaths1
				    else ()
			    (* Why don't we have the same set of conditions for the second sequence? *)
			    val _ = if f1
				       andalso (not (T.isPoly poly2))
				       andalso not (List.null typaths2)
				       andalso List.length typaths1 = List.length typaths2
				    then upd i2 typaths2
				    else ()
			(*val _ = D.printdebug2 ("(E)")*)
			in fsimplify cs' l
			end
	    (*val _ = D.printdebug2 ((*S.printState state ^ "\n" ^*) T.printty ty1 ^ "\n" ^ T.printty ty2)*)
	    in case (S.getValStateOr state i1, S.getValStateOr state i2) of
		   (SOME ([path1], ls1, deps1, ids1), SOME ([path2], ls2, deps2, ids2)) =>
		   (case (gotoInOrSq path1 sq1, gotoInOrSq path2 sq2) of
			(SOME t1, SOME t2) =>
			let (* (2010-02-05) This is never happening, is it? *)
			    (*val _ = D.printdebug2 (" OR ")*)
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
		   (* The 2 sequences are not set yet to a path (a path set),
		    * so we don't know yet whether they match on at least a type.
		    * We've got to check that with 'match'. *)
		   match sq1 sq2 ls deps ids
	    end
	  (*| fsimplify ((E.CSTGEN (env, cst1, cst2)) :: cs') l =
	   (let val _ = run cst1
		(*val csbind = cleanMultiCsBind (flattenEnv env) state filters l*)
		val (env1, errop) = BI.cleanMultiCsBind env state filters l
		val (tvl, labs, cds, domge) = BI.recomputeMonoTyVarInAll state O.empty (bindsVal env1)
		val env2 = #1 (B.solveenv env1 state filters false)
		val (cs'', tvlFn, tvlVa, env3) = BI.setBind env2 state filters domge l
		(*val _ = D.printdebug2 (E.printEnv env1 "" ^ "\n" ^ E.printEnv env2 "" ^ "\n" ^ E.printEnv env3 "")*)
		val _ = pushEnvToState env3 state
		val _ = BI.recomputeMonoTyVarOut state tvl labs cds
		val _ = BI.updateMonoTyVarIn state (tvlFn @ tvlVa)
		(* Do we still need all the state at this point? *)
		val _ = (fsimplify cs'' l; run cst2)
		val _ = BI.updateMonoTyVarOut state tvlFn
		val _ = remEnvFromState env3 state
	    in case errop of
		   SOME err => handleSimplify err cs' l
		 | NONE     => fsimplify cs' l
	    (* If we want to be able to remove cs'' then we have to duplicate the inpoints
	     * when generating the CSTGEN constraint.  See test44. *)
	    end handle unmatched err => handleSimplify err cs' l)
	  | fsimplify ((E.CSTVAL (tvsbind1, tvsbind2, tvl, cst)) :: cs') l =
	    let fun trans xs =
 		    List.mapPartial (fn (tv, l') =>
					if FI.testtodo filters l'
					then SOME (tv, O.singleton l')
					else NONE) xs
		val tvsbind = trans (tvsbind1 @ tvsbind2)
		val _ = app (fn (tv, ls) => S.updateStateGe state tv ([], ls, O.empty, CD.empty)) tvsbind
		val _ = run cst
		val _ = app (fn (tv, ls) => S.deleteStateGe state tv ([], ls, O.empty, CD.empty)) tvsbind
		val (cs'', errop) = BI.compval tvsbind tvl state filters l
	    in case errop of
		   NONE => fsimplify (cs'' @ cs') l
		 | SOME err => handleSimplify err cs' l
	    end*)
	  (* TODO: check that *)
	  | fsimplify ((E.CSTENV ((E.ENVVAR (ev1, lab1), env2 as E.ENVVAR (ev2, lab2)), ls, deps, ids)) :: cs') l =
	    if E.eqEnvvar ev1 ev2
	    then fsimplify cs' l
	    else (case S.getValStateEv state ev1 of
		      NONE =>
		      let val _ = S.updateStateEv state ev1 (E.pushExtEnv (E.ENVVAR (ev2, lab2)) ls deps ids)
		      in fsimplify cs' l
		      end
		    | SOME env =>
                      let (*val _ = D.printdebug2 (O.toString labs1)*)
			  val c = E.genCstEvAll env (E.ENVVAR (ev2, lab2)) ls deps ids
                      in fsimplify (c :: cs') l
                      end)
	  (* TODO: do something for next rules *)
	  (* we don't want these to happen *)
	  (*| fsimplify ((E.CSTENV ((E.INJSTR stc,  E.INJVID lid),  ls, deps, ids)) :: cs') l = ((*raise EH.DeadBranch "[INJSTR/INJVID]";*) fsimplify cs' l)
	  | fsimplify ((E.CSTENV ((E.INJSTR stc,  E.INJTYP lid),  ls, deps, ids)) :: cs') l = ((*raise EH.DeadBranch "[INJSTR/INJTYP]";*) fsimplify cs' l)
	  | fsimplify ((E.CSTENV ((E.INJVID lid1, E.INJTYP lid2), ls, deps, ids)) :: cs') l = ((*raise EH.DeadBranch "[INJVID/INJTYP]";*) fsimplify cs' l)
	  | fsimplify ((E.CSTENV ((E.INJVID lid1, E.INJVID lid2), ls, deps, ids)) :: cs') l = ((*raise EH.DeadBranch "[INJVID/INJVID]";*) fsimplify cs' l)
	  (* it is not EH.DeadBranch "" anymore, because it can happen when labels are thrown out *)
	  (* this arises since Open *)
	  (* It should be the same for the other ones *)
	  (* we could then check if there is a false and otherwise raise a EH.DeadBranch "" *)
	  (*(D.printdebug2 (S.printState state); raise EH.DeadBranch "")*)
	  | fsimplify ((E.CSTENV ((E.INJTYP lid1, E.INJTYP lid2), ls, deps, ids)) :: cs') l =(* ((*raise EH.DeadBranch "[INJTYP/INJTYP]";*) fsimplify cs' l)*)
	    let val id1 = E.getBindI lid1
		val id2 = E.getBindI lid2
	    in if I.eqId id1 id2 andalso SO.isAtLeast SO.SOL8
	       then let val cl1  = E.getBindC lid1
			val cl2  = E.getBindC lid2
			val lab1 = E.getBindL lid1
			val lab2 = E.getBindL lid2
			val ecl1 = E.CCL (cl1, lab1, I.idToLid id1 lab1, false)
			val ecl2 = E.CCL (cl2, lab2, I.idToLid id2 lab2, false)
			val c    = E.genCstClAll ecl1 ecl2 ls deps ids
		    in fsimplify (c :: cs') l
		    end
	       else fsimplify cs' l
	    end
	  | fsimplify ((E.CSTENV ((E.INJSTR stc1, E.INJSTR stc2), ls, deps, ids)) :: cs') l = ((*raise EH.DeadBranch "[INJSTR/INJSTR]";*) fsimplify cs' l)
	  | fsimplify ((E.CSTENV ((E.INJSTR eenv, env as E.ENVCON _), ls, deps, ids)) :: cs') l =
	    let val id  = E.getBindI eenv
		val e   = E.getBindT eenv
		val lab = E.getBindL eenv
	    in if I.isin id (E.dom (E.getStrs env)) orelse not (E.completeEnv env (*ls*))
	       then fsimplify ((map (fn exv => E.genCstEvAll e (E.getBindT exv) ls deps ids)
				    (E.plusproj (E.getStrs env) id)) @ cs') l
	       else handleSimplify (ERR.consPreError ERR.dummyId
						     ls
						     ids
						     (EK.Unmatched ((lab, I.toInt id),
								    E.getLabsIdsEnv env 5,
								    E.getLabEnv env))
						     deps
						     l)
				   cs'
				   l
	    end
	  | fsimplify ((E.CSTENV ((E.INJVID ({id, scope, bind, class, lab, poly}, _, _, _),
				   env as E.ENVCON _), ls, deps, ids)) :: cs') l =
	    if not (I.isin id (E.dom (E.getVids env))) andalso
	       not (I.isin id (E.dom (E.getVars env))) andalso (* What's in X?  The ones with 'v' status *)
	       not (I.isin id (E.dom (E.getCons env))) andalso
	       E.completeEnv env (*ls*)
	    then handleSimplify (ERR.consPreError ERR.dummyId
						  ls
						  ids
						  (EK.Unmatched ((lab, I.toInt id),
								 E.getLabsIdsEnv env 7,
								 E.getLabEnv env))
						  deps
						  l)
				cs'
				l
	    else let val lids = E.getLabsIdsEnv env 6
		     val labs = FI.filtertodos filters (O.ord (#1 (ListPair.unzip lids)))
		     (*val _ = D.printdebug2 (O.toString ls ^ "\n" ^ E.printEnv env "")*)
		     val ls   = L.union labs ls
		     fun linkAll () =
			 let val semty = E.plusproj (E.uenv [E.getVids env, E.getVars env, E.getCons env]) id
			     val (tvl, labs, asmp, domge) = BI.recomputeMonoTyVarInAll state O.empty true
			     fun linkOne etv =
				 let val ty1 = E.getBindT etv
				     (* we have to rebuild ty1 because the value in the structure has
				      * a monomorphic type and it has been instantiated previously.
				      * (See testcase 372.) *)
				     val (ty0, labs, st, cds) = B.buildty' ty1 state false
				     val ty2 = B.freshty' ty0 (SOME domge)
				 in E.genCstTyAll bind
						  ty2
						  (L.union ls labs)
						  (L.union deps st)
						  (CD.union ids cds)
				 end
			     val cs = map (fn etv => linkOne etv) semty
			     val _  = BI.recomputeMonoTyVarOut state tvl labs asmp
			 in cs
			 end
		 in if CL.classIsREC class
		    then raise EH.DeadBranch "" (* because it is not used for now *)
		    else if CL.classIsVAL class orelse CL.classIsPAT class
		    then fsimplify ((linkAll ()) @ cs') l
		    else if CL.classIsCON class
		    then (* It cannot be a variable in G, neither an exception in C (* Yes it can be an exception! C means Constructor, it does not mean datatype constructor *)*)
			(* It's a bit more complicated than that because if the structure is defined without
			 * signature then the variable can be anything and so we would have a context dependency
			 * ?? Is that the difference between X and G up there??
			 * If it's not, it should be. *)
			if I.isin id (E.dom (E.getVars env))
			then handleSimplify (ERR.consPreError ERR.dummyId ls ids (EK.ConIsVar NONE) deps l) cs' l
			else (*if O.isin id (E.dom (E.getEXCenv (E.getCons env'))) andalso comp (* if it's an exception it should be an error *)
			       then handleSimplify (ls, deps, ids, l, EK.DatIsExc NONE) cs' l
			       else*) fsimplify ((linkAll ()) @ cs') l
		    else if CL.classIsEXC class
		    then let val labs = P.getLabsPoly poly
			 in if I.isin id (E.dom (E.getVars env))
			       andalso FI.testtodos filters labs (*(2010-03-04)We actually need that for SOL6*)
			    then handleSimplify (ERR.consPreError ERR.dummyId (L.union labs ls) ids (EK.ExcIsVar NONE) deps l) cs' l
			    else if I.isin id (E.dom (E.getCONenv (E.getCons env))) (* if it's a datatype constructor it should be an error *)
				    andalso FI.testtodos filters labs (*(2010-03-04)We actually need that for SOL6*)
			    then handleSimplify (ERR.consPreError ERR.dummyId (L.union labs ls) ids (EK.ExcIsDat NONE) deps l) cs' l
			    else fsimplify ((linkAll ()) @ cs') l
			 end
		    else raise EH.DeadBranch "wrong identifier class"
		 end
	  | fsimplify ((E.CSTENV ((E.INJTYP ({id, scope, bind, class, lab, poly}, _, _, _),
				   env as E.ENVCON _), ls, deps, ids)) :: cs') l =
	    if I.isin id (E.dom (E.getTyps env)) orelse
	       not (E.completeEnv env (*ls*))
	    then let val semty = E.plusproj (E.getTyps env) id
		     fun link etv =
			 let val ty1 = E.getBindT etv
			     val ty2 = B.freshty' ty1 (SOME (S.getDomGe state))
			 in E.genCstTyAll bind ty2 ls deps ids
			 end
		     val cs''  = map link semty
		 in fsimplify (cs'' @ cs') l
		 end
	    else handleSimplify (ERR.consPreError ERR.dummyId
						  ls
						  ids
						  (EK.Unmatched ((lab, I.toInt id),
								 E.getLabsIdsEnv env 8,
								 E.getLabEnv env))
						  deps
						  l)
				cs'
				l*)
	  | fsimplify ((E.CSTENV ((E.ENVVAR (ev, lab), env), ls, deps, ids)) :: cs') l =
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
	  | fsimplify ((E.CSTENV ((env1 as E.ENVCON _, env2 as E.ENVCON _), ls, deps, ids)) :: cs') l =
	    let val cs = compareenv env1 env2 filters ls deps ids
	    (* do something for idV and idS *)
	    in fsimplify (cs @ cs') l
	    end
	  (*| fsimplify ((E.CSTEGN ((ev0, ev1, ev2, ev3, lab, b), cst)) :: cs') l =
	   let (* 0: signature, 2: structure, 1: translucent, 3: opaque *)
	       val _ = run cst
	       (*val _ = D.printdebug2 ("--\n" ^ E.printEnv env1 "" ^ "\n" ^ E.printEnv env2 "" ^ "\n")*)
	       (*val _ = D.printdebug2 ("--\n" ^ S.printState state ^ "\n")*)
	       (*val _ = D.printdebug2 ("--" ^ O.toString ll1 ^ " -- " ^ O.toString ll2 ^ "\n")*)
	       (*val _ = D.printdebug2 (O.printelt lab)*)
	       val btest = FI.testtodo filters lab
	       val (csV, csT, errop) =
		   if btest
		   (*orelse
		    (not b andalso not btest)*)
		   (* if not b and not (FI.testtodo filters lab) then we still need to
		    * do the rest with tns at NONE.
		    * That's the btest below. *)
		   then let val ((env0, labs0, deps1, asmp1),
				 (env2, labs2, deps2, asmp2)) =
				(B.solveenv (E.ENVVAR ev0) state filters true,
				 B.solveenv (E.ENVVAR ev2) state filters true)
			    (* First we build the environments associated to the structure and signature,
			     * then we refresh the environment associated to the signature.
			     * The (SOME O.empty) is to specify that we refresh all the internal and
			     * explicit type variables. *)
			    (*val _    = D.printdebug2 (E.printEnv env2 "")*)
			    val env0 = B.freshenv' env0 (SOME O.empty) false
			    (*(2010-03-03)We only refresh env2 to refresh the explicit type variables.
			     * We then could have a faster function that does less work! *)
			    val env2 = B.freshenv' env2 (SOME O.empty) true
			    (*val _    = D.printdebug2 (E.printEnv env0 "")*)
			    (* tfun are the type functions of the structure/realisation specified
			     * by the signature. *)
			    val tfun  = B.getTyFunEnv env0 env2
			    val (cs0, env1) = B.genTyFunEnv' env0 tfun
			    val labs1 = O.cons lab labs0
			    val ls'   = L.union labs1 labs2 (* lab is in labs1 *)
			    val deps' = L.union deps1 deps2
			    val ids'  = CD.union asmp1 asmp2
			    val cs1   = case ev1 of
					    NONE => []
					  | SOME ev =>
					    S.updateStateEv state ev (env1, labs1, deps1, asmp1)
			    val cs3   = case ev3 of
					    NONE => []
					  | SOME ev =>
					    let val env3 = B.freshenv' (B.renameenv' env0) (SOME O.empty) false
					    in S.updateStateEv state ev (env3, labs1, deps1, asmp1)
					    end
			    (*val _ = D.printdebug2 (S.printState state  ^ "\n---" ^
						     Int.toString ev1    ^   "---" ^
						     Int.toString ev2    ^   "---" ^
						     E.printEnv env1 ""  ^ "\n")*)
			    (* NOTE: IF env2 IS A VARIABLE HERE THEN WE CAN TAKE env1 AS THE NON
			     * GENERALISED ENVIRONMENT.  No we can't do that because it will happen
			     * during enumeration (when removing labels) and it might cause type
			     * errors to occur for typable programs.
			     * We might however tag ev2 when dealing with a dummy structure in
			     * the basis. *)
			    (*val _ = D.printdebug2 (E.printEnv env1  "" ^ "\n" ^
						     E.printEnv env2  "" ^ "\n" ^
						     E.printEnv env2' "")*)
			    (*(2010-03-03)The second constraint set if for datatypes and type functions.*)
			    val (cs2, cs2') = genenv env1 env2 ls' deps' ids' lab filters b
			in ((decorateCst cs0 ls' deps' ids') @ cs1 @ cs2 @ cs3, cs2', NONE)
			end
			handle unmatched err => ([], [], SOME err)
			     | unbwhere  err => ([], [], SOME err)
			     | misscons  err => ([], [], SOME err)
			     | dattyp    err => ([], [], SOME err)
		   else ([], [], NONE)
	   in case errop of
		  NONE =>
		  let val _ = sigVsStrON ()
		      val _ = fsimplify csV l
		      val _ = sigVsStrTypON ()
		      val _ = fsimplify csT l
		      val _ = sigVsStrTypOFF ()
		      val _ = sigVsStrOFF ()
		  in fsimplify cs' l
		  end
		| SOME err => handleSimplify err cs' l
	   end*)
	  (*| fsimplify ((E.CSTCLS ((E.VCL _, E.VCL _), ls, deps, ids)) :: cs') l = raise EH.DeadBranch ""
	  | fsimplify ((E.CSTCLS ((E.VCL cv, class), ls, deps, ids)) :: cs') l =
	    (case S.getValStateCl state cv of
		 NONE =>
		 let val cs'' = S.updateStateCl state cv (class, ls, deps, ids)
		 (*val _ = D.printdebug2 ("[update]")*)
		 in fsimplify (cs'' @ cs') l
		 end
               | SOME (class', ls', deps', ids') =>
		 let val labs1 = L.union ls ls'
		     val deps1 = L.union deps deps'
		     val asmp1 = CD.union ids ids'
		     val c     = E.genCstClAll class' class labs1 deps1 asmp1
		 (*val _ = D.printdebug2 (E.printClass class' ^ " " ^ E.printClass class)*)
		 in fsimplify (c :: cs') l
		 end)
	  | fsimplify ((E.CSTCL ((E.CCL (cl1, lab1, id1, true), E.CCL (cl2, lab2, id2, false)), ls, deps, ids)) :: cs') l =
	    if (CL.classIsCON cl2 orelse CL.classIsEXC cl2)
	       andalso
	       (CL.classIsVAL cl1 orelse CL.classIsREC cl1)
	    then let val lsrec = if CL.classIsREC cl1
				 then CL.getClassRECst cl1
				 else O.empty
		 in if CL.classIsCON cl2
		    then if CL.classIsREC cl1 andalso I.isLong id2
			 then handleSimplify (ERR.consPreError ERR.dummyId (O.cons lab1 (L.union lsrec ls)) ids (EK.ConIsVar NONE) deps l) cs' l
			 else let val ls1 = CL.getClassCON cl2
			      (*val _ = D.printdebug2 ("foo")*)
			      in if FI.testtodos filters ls1
				 then handleSimplify (ERR.consPreError ERR.dummyId (O.cons lab1 (L.union lsrec (L.union ls1 ls))) CD.empty (EK.ValVarApp NONE) deps l) cs' l
				 (*(2010-02-18)lsrec should be empty above*)
				 else fsimplify cs' l
			      end
		    else let val ls1 = CL.getClassEXC cl2
			 in if FI.testtodos filters ls1
			    then handleSimplify (ERR.consPreError ERR.dummyId (O.cons lab1 (L.union lsrec (L.union ls1 ls))) ids (EK.ExcIsVar NONE) deps l) cs' l
			    else fsimplify cs' l
			 end
		 end
	    else if CL.classIsEXC cl2 andalso CL.classIsCON cl1
	    then let val ls0 = CL.getClassEXC cl2
		 in if FI.testtodos filters ls0
		    then handleSimplify (ERR.consPreError ERR.dummyId (L.union ls0 ls) ids (EK.ExcIsDat NONE) deps l) cs' l
		    else fsimplify cs' l
		 end
	    else if CL.classIsTYP cl2 andalso CL.classIsTYCON cl1
	    then let val c = E.genCstSqAll (T.SV (CL.getTYP cl2))
					   (T.SV (CL.getTYCONseq cl1))
					   ls deps ids
		 in fsimplify (c :: cs') l end
	    else fsimplify cs' l
	  | fsimplify ((E.CSTCL ((E.CCL (cl1, lab1, id1, false), E.CCL (cl2, lab2, id2, false)), ls, deps, ids)) :: cs') l =
	    if CL.classIsTYP cl1 andalso CL.classIsTYP cl2
	    then let val c = E.genCstSqAll (T.SV (CL.getTYP cl1))
					   (T.SV (CL.getTYP cl2))
					   ls deps ids
		 in fsimplify (c :: cs') l end
	    else if CL.classIsCON cl1
		    andalso CL.isClassVar cl2 (*(2010-03-09)means that the second class is not for a binder but comes from a pattern*)
		    andalso (CL.classIsVAL cl2 orelse CL.classIsREC cl2)
	    then let val ls' = CL.getClassCON cl1 (*(2010-02-22)in Build.sml we should filter the CON and EXC classes as well so that we wouldn't have to test that!*)
		 (*val _ = D.printdebug2 ("foo " ^ I.printLid id2 ^ " " ^ CL.toString cl2 ^ " " ^ CL.toString cl1 ^ " " ^ O.printelt lab2 ^ " " ^ O.printelt lab1)*)
		 in if FI.testtodos filters ls'
		    then handleSimplify (ERR.consPreError ERR.dummyId (O.cons lab2 (L.union ls' ls)) CD.empty (EK.ValVarApp NONE) deps l) cs' l
		    else fsimplify cs' l
		 end
	    else if CL.classIsCON cl2
		    andalso CL.isClassVar cl1
		    andalso (CL.classIsVAL cl1 orelse CL.classIsREC cl1)
	    then let val ls' = CL.getClassCON cl2
		 (*val _ = D.printdebug2 ("bar")*)
		 in if FI.testtodos filters ls'
		    then handleSimplify (ERR.consPreError ERR.dummyId (O.cons lab1 (L.union ls' ls)) CD.empty (EK.ValVarApp NONE) deps l) cs' l
		    else fsimplify cs' l
		 end
	    else fsimplify cs' l
	  | fsimplify ((E.CSTCL _) :: cs') l = raise EH.DeadBranch ""*)
	  (*(2010-04-16)TODO:*)
	  | fsimplify ((E.CSTENV ((E.ENVSEQ x, E.ENVCON y), ls, deps, ids)) :: cs') l = raise EH.TODO
	  | fsimplify ((E.CSTENV ((E.ENVSEQ x, E.ENVSEQ y), ls, deps, ids)) :: cs') l = raise EH.TODO
	  | fsimplify ((E.CSTENV ((E.ENVSEQ x, E.ENVOPN y), ls, deps, ids)) :: cs') l = raise EH.TODO
	  | fsimplify ((E.CSTENV ((E.ENVSEQ x, E.ENVCST y), ls, deps, ids)) :: cs') l = raise EH.TODO
	  (**)
	  | fsimplify ((E.CSTENV ((E.ENVOPN x, E.ENVCON y), ls, deps, ids)) :: cs') l = raise EH.TODO
	  | fsimplify ((E.CSTENV ((E.ENVOPN x, E.ENVSEQ y), ls, deps, ids)) :: cs') l = raise EH.TODO
	  | fsimplify ((E.CSTENV ((E.ENVOPN x, E.ENVOPN y), ls, deps, ids)) :: cs') l = raise EH.TODO
	  | fsimplify ((E.CSTENV ((E.ENVOPN x, E.ENVVAR y), ls, deps, ids)) :: cs') l = raise EH.TODO
	  | fsimplify ((E.CSTENV ((E.ENVOPN x, E.ENVCST y), ls, deps, ids)) :: cs') l = raise EH.TODO
	  (**)
	  | fsimplify ((E.CSTENV ((E.ENVCST x, E.ENVCON y), ls, deps, ids)) :: cs') l = raise EH.TODO
	  | fsimplify ((E.CSTENV ((E.ENVCST x, E.ENVSEQ y), ls, deps, ids)) :: cs') l = raise EH.TODO
	  | fsimplify ((E.CSTENV ((E.ENVCST x, E.ENVOPN y), ls, deps, ids)) :: cs') l = raise EH.TODO
	  | fsimplify ((E.CSTENV ((E.ENVCST x, E.ENVVAR y), ls, deps, ids)) :: cs') l = raise EH.TODO
	  | fsimplify ((E.CSTENV ((E.ENVCST x, E.ENVCST y), ls, deps, ids)) :: cs') l = raise EH.TODO
	  (**)
	  | fsimplify ((E.CSTENV ((E.ENVCON x, E.ENVSEQ y), ls, deps, ids)) :: cs') l = raise EH.TODO
	  | fsimplify ((E.CSTENV ((E.ENVCON x, E.ENVOPN y), ls, deps, ids)) :: cs') l = raise EH.TODO
	  | fsimplify ((E.CSTENV ((E.ENVCON x, E.ENVCST y), ls, deps, ids)) :: cs') l = raise EH.TODO
	  (**)
	  | fsimplify ((E.CSTTYF ((T.TFV tfv, tyfun), labs, stts, deps)) :: cs') l =
	    (case S.getValStateTf state tfv of
		 NONE =>
		 let val _ = S.updateStateTf state tfv (collapseTf tyfun labs stts deps)
		 in fsimplify cs' l
		 end
	       | SOME tyfun' =>
		 let val c     = E.genCstTfAll tyfun' tyfun labs stts deps
		 in fsimplify (c :: cs') l
		 end)
	  | fsimplify ((E.CSTTYF ((T.TFC (seqty1, ty1, lab1), T.TFC (seqty2, ty2, lab2)), labs, stts, deps)) :: cs') l =
	    let val c1 = E.genCstSqAll seqty1 seqty2 labs stts deps
		val c2 = E.genCstTyAll ty1    ty2    labs stts deps
	    (*val _ = D.printdebug2 (S.printState state)*)
	    (*val _ = D.printdebug2 (L.toString labs)*)
	    in fsimplify (c1 :: c2 :: cs') l
	    end
	  | fsimplify ((E.CSTTYF ((tyfun1, tyfun2 as T.TFV tfv), labs, stts, deps)) :: cs') l =
	    fsimplify ((E.CSTTYF ((tyfun2, tyfun1), labs, stts, deps)) :: cs') l
	  | fsimplify ((E.CSTCLS ((CL.CLVAR clvar, cl), labs, stts, deps)) :: cs') l =
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
	  | fsimplify ((E.CSTCLS ((x, CL.CLVAR cv), ls, deps, ids)) :: cs') l =
	    fsimplify ((E.CSTCLS ((CL.CLVAR cv, x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.CSTCLS ((CL.VID vid1, CL.VID vid2), labs, stts, deps)) :: cs') l =
	    let fun genError kind =
		    let val err = ERR.consPreError ERR.dummyId labs deps kind stts l
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
	  | fsimplify ((E.CSTCLS ((CL.TYCON, CL.TYCON), _, _, _)) :: cs') l = fsimplify cs' l
	  | fsimplify ((E.CSTCLS ((CL.TYVAR, CL.TYVAR), _, _, _)) :: cs') l = fsimplify cs' l
	  | fsimplify ((E.CSTCLS ((CL.STR,   CL.STR),   _, _, _)) :: cs') l = fsimplify cs' l
	  | fsimplify ((E.CSTCLS ((CL.SIG,   CL.SIG),   _, _, _)) :: cs') l = fsimplify cs' l
	  | fsimplify ((E.CSTCLS ((CL.FUNC,  CL.FUNC),  _, _, _)) :: cs') l = fsimplify cs' l
	  | fsimplify ((E.CSTCLS ((CL.OC,    CL.OC),    _, _, _)) :: cs') l = fsimplify cs' l
	  | fsimplify ((E.CSTCLS ((CL.ANY, _),   _, _, _)) :: cs') l = fsimplify cs' l
	  | fsimplify ((E.CSTCLS ((_, CL.ANY),   _, _, _)) :: cs') l = fsimplify cs' l
	  | fsimplify ((E.CSTCLS ((_, CL.TYCON), _, _, _)) :: cs') l = raise EH.DeadBranch ""
	  | fsimplify ((E.CSTCLS ((CL.TYCON, _), _, _, _)) :: cs') l = raise EH.DeadBranch ""
	  | fsimplify ((E.CSTCLS ((_, CL.TYVAR), _, _, _)) :: cs') l = raise EH.DeadBranch ""
	  | fsimplify ((E.CSTCLS ((CL.TYVAR, _), _, _, _)) :: cs') l = raise EH.DeadBranch ""
	  | fsimplify ((E.CSTCLS ((_, CL.STR),   _, _, _)) :: cs') l = raise EH.DeadBranch ""
	  | fsimplify ((E.CSTCLS ((CL.STR, _),   _, _, _)) :: cs') l = raise EH.DeadBranch ""
	  | fsimplify ((E.CSTCLS ((_, CL.SIG),   _, _, _)) :: cs') l = raise EH.DeadBranch ""
	  | fsimplify ((E.CSTCLS ((CL.SIG, _),   _, _, _)) :: cs') l = raise EH.DeadBranch ""
	  | fsimplify ((E.CSTCLS ((_, CL.FUNC),  _, _, _)) :: cs') l = raise EH.DeadBranch ""
	  | fsimplify ((E.CSTCLS ((CL.FUNC, _),  _, _, _)) :: cs') l = raise EH.DeadBranch ""
	  | fsimplify ((E.CSTCLS ((_, CL.OC),    _, _, _)) :: cs') l = raise EH.DeadBranch ""
	  | fsimplify ((E.CSTCLS ((CL.OC, _),    _, _, _)) :: cs') l = raise EH.DeadBranch ""
	  (**)
	  | fsimplify ((E.CSTLET env) :: cs') l =
	    let val _ = solveenv env false
	    in fsimplify cs' l
	    end
	  (**)
	  | fsimplify ((E.CSTSIG (ev0, ev1, ev2, ev3, lab)) :: cs') l =
	    (* 0: signature, 2: structure, 1: translucent, 3: opaque *)
	    let val btest = FI.testtodo filters lab
	    (*val _ = D.printdebug2 (S.printState state)*)
	    (*val _ = D.printdebug2 (E.printEnv (E.consEnvVar ev0 lab) "")*)
	    in if btest
	       then let val env0 = buildFEnv (E.consEnvVar ev0 lab) state false
			(*(2010-06-15)Why do we need to refresh the signature/structure?
			 * Because we don't have proper type schemes and type functions? *)
			val env2 = buildFEnv (E.consEnvVar ev2 lab) state true
			(*val _ = D.printdebug2 (S.printState state ^ "\n" ^ E.printEnv (E.consEnvVar ev0 lab) "" ^ "\n" ^ E.printEnv (E.consEnvVar ev2 lab) "")*)
			(*val _ = D.printdebug2 (E.printEnv env0 "" ^ "\n" ^ E.printEnv env2 "")*)
			(*(2010-06-22)We might need to decorate all the constraint generated in here
			 * with labs, stts and deps.*)
			(*First we build the environments associated to the structure and signature,
			 * then we refresh the environment associated to the signature.
			 * The (SOME O.empty) is to specify that we refresh all the internal and
			 * explicit type variables. *)
			(*(2010-03-03)We only refresh env2 to refresh the explicit type variables.
			 * We then could have a faster function that does less work! *)
			(*tfun are the type functions of the structure/realisation specified
			 * by the signature. *)
			val (tfnD, tfnT) = getTyFunEnv env0 env2 L.empty L.empty CD.empty
			val (cs0, env1) = genTyFunEnv' env0 state tfnT true
			(* If mk is E.STR then env0 is a signature and env2 is a structure.
			 * If env2 is incomplete then we don't need to turn the datatypes
			 * into dummy types, just the type functions. *)
			(* NOTE: IF env2 IS A VARIABLE HERE THEN WE CAN TAKE env1 AS THE NON
			 * GENERALISED ENVIRONMENT.  No we can't do that because it will happen
			 * during enumeration (when removing labels) and it might cause type
			 * errors to occur for typable programs.
			 * We might however tag ev2 when dealing with a dummy structure in
			 * the basis. *)
			(*(2010-03-03)The second constraint set if for datatypes and type functions.*)
			(*val _ = D.printdebug2 (E.printEnv env0 "" ^ "\n" ^ E.printEnv env2 "")*)
			val (cs1, cs2) = matchSigStr env1 env2 lab filters L.empty L.empty CD.empty true err
			(*val _ = D.printdebug2 (E.printEnv (E.ENVCST (E.singcsts (L.dummyLab, cs1))) "")*)
			(*val _ = D.printdebug2 (E.printEnv (E.ENVCST (E.singcsts (L.dummyLab, cs2))) "")*)
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
				    (*val _ = D.printdebug2 (E.printEnv (E.consEnvVar ev lab) "" ^ "\n" ^ E.printEnv env3 "")*)
				    in S.updateStateEv state ev (E.pushExtEnv env3 (L.singleton lab) L.empty CD.empty)
				    end
		    in fsimplify cs' l
		    end
		    handle errorfound err => handleSimplify err cs' l
	       else fsimplify cs' l
	    end
	  (**)
	  | fsimplify ((E.CSTFUN (ev1, ev2, ev3, ev4, lab)) :: cs') l =
	    (* functor: ev1 -> ev2, argument : ev3, result ev4 *)
	    if FI.testtodo filters lab
	    then let val env1 = buildFEnv (E.consEnvVar ev1 lab) state false
		     val env2 = buildFEnv (E.consEnvVar ev2 lab) state true
		     val env3 = buildFEnv (E.consEnvVar ev3 lab) state true
		     val (tfnD, tfnT) = getTyFunEnv env1 env3 L.empty L.empty CD.empty
		     val tfun = mergeTyFun tfnD tfnT
		     val (cs0, env1') = genTyFunEnv' env1 state tfun true
		     val (cs1, env2') = genTyFunEnv' env2 state tfun true
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
	  | fsimplify ((E.CSTSHA (ev0, ev1, ev2, lab)) :: cs') l =
	    (* I need to transform this constraint in environment as I've done for WHR. *)
	    (* 0: signature, 1: returned, 2: sharing *)
	    if FI.testtodo filters lab
	    then let val env0 = buildFEnv (*justBuildEnv*) (E.consEnvVar ev0 lab) state false
		     val env2 = buildFEnv (*justBuildEnv*) (E.consEnvVar ev2 lab) state false
		     (* We don't need to refresh these two *)
		     val (utf, tfun) = getTyFunEnvSha env0 env2
		     val tfun = case utf of
				    NONE => OM.map (fn _ => newTyFun ()) tfun
				  | SOME (tf, labs, stts, deps) =>
				    OM.map (fn NONE => newTyFun ()
					     | SOME (labs', stts', deps') =>
					       collapseTf tf
							  (L.union  labs labs')
							  (L.union  stts stts')
							  (CD.union deps deps'))
					   tfun
		     val (cs0, env1) = genTyFunEnv' env0 state tfun false
		     (*val _ = D.printdebug2 (S.printState state ^ "\n" ^ E.printEnv (E.consEnvVar ev0 lab) "" ^ "\n" ^ E.printEnv env0 "")*)
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
	  | fsimplify ((E.CSTACC acc) :: cs') l = (solveacc acc l; fsimplify cs' l)
	  | fsimplify ((E.CSTTYP ((T.GEN _, T.C   _), _, _, _)) :: cs') l = raise EH.TODO
	  | fsimplify ((E.CSTTYP ((T.GEN _, T.E   _), _, _, _)) :: cs') l = raise EH.TODO
	  | fsimplify ((E.CSTTYP ((T.GEN _, T.A   _), _, _, _)) :: cs') l = raise EH.TODO
	  | fsimplify ((E.CSTTYP ((T.GEN _, T.OR  _), _, _, _)) :: cs') l = raise EH.TODO
	  | fsimplify ((E.CSTTYP ((T.GEN _, T.GEN _), _, _, _)) :: cs') l = fsimplify cs' l (*raise EH.TODO*)
	  (*(2010-06-23)We keep unifying but we should really chain the GENs.*)
	  (* otherwise we swap the types of the evaluated constraint *)
	  | fsimplify ((E.CSTTYP ((T.C   x, T.V   y), ls, deps, ids)) :: cs') l = fsimplify ((E.CSTTYP ((T.V   y, T.C   x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.CSTTYP ((T.E   x, T.V   y), ls, deps, ids)) :: cs') l = fsimplify ((E.CSTTYP ((T.V   y, T.E   x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.CSTTYP ((T.OR  x, T.V   y), ls, deps, ids)) :: cs') l = fsimplify ((E.CSTTYP ((T.V   y, T.OR  x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.CSTTYP ((T.GEN x, T.V   y), ls, deps, ids)) :: cs') l = fsimplify ((E.CSTTYP ((T.V   y, T.GEN x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.CSTTYP ((T.OR  x, T.C   y), ls, deps, ids)) :: cs') l = fsimplify ((E.CSTTYP ((T.C   y, T.OR  x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.CSTTYP ((T.OR  x, T.A   y), ls, deps, ids)) :: cs') l = fsimplify ((E.CSTTYP ((T.A   y, T.OR  x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.CSTTYP ((T.C   x, T.A   y), ls, deps, ids)) :: cs') l = fsimplify ((E.CSTTYP ((T.A   y, T.C   x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.CSTTYP ((T.E   x, T.A   y), ls, deps, ids)) :: cs') l = fsimplify ((E.CSTTYP ((T.A   y, T.E   x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.CSTTYP ((T.OR  x, T.GEN y), ls, deps, ids)) :: cs') l = fsimplify ((E.CSTTYP ((T.GEN y, T.OR  x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.CSTTYP ((T.C   x, T.GEN y), ls, deps, ids)) :: cs') l = fsimplify ((E.CSTTYP ((T.GEN y, T.C   x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.CSTTYP ((T.E   x, T.GEN y), ls, deps, ids)) :: cs') l = fsimplify ((E.CSTTYP ((T.GEN y, T.E   x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.CSTTYP ((T.OR  x, T.E   y), ls, deps, ids)) :: cs') l = fsimplify ((E.CSTTYP ((T.E   y, T.OR  x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.CSTTYP ((T.C   x, T.E   y), ls, deps, ids)) :: cs') l = fsimplify ((E.CSTTYP ((T.E   y, T.C   x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.CSTTYN ((T.NC  x, T.NV  y), ls, deps, ids)) :: cs') l = fsimplify ((E.CSTTYN ((T.NV  y, T.NC  x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.CSTSEQ ((T.SC  x, T.SV  y), ls, deps, ids)) :: cs') l = fsimplify ((E.CSTSEQ ((T.SV  y, T.SC  x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.CSTENV ((E.ENVSEQ x, E.ENVVAR y), ls, deps, ids)) :: cs') l = fsimplify ((E.CSTENV ((E.ENVVAR y, E.ENVSEQ x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.CSTENV ((E.ENVCON x, E.ENVVAR y), ls, deps, ids)) :: cs') l = fsimplify ((E.CSTENV ((E.ENVVAR y, E.ENVCON x), ls, deps, ids)) :: cs') l
	  | fsimplify ((E.CSTENV _) :: cs') l = raise EH.TODO

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
         * have to modify the state to change T.E into T.V instead of
         * what we're currently doing in freshty - see example 96 *)
	and run cs =
	    (E.foldlicst
		 (fn (l, ocstl, ()) =>
		     if FI.testtodo filters (L.fromInt l)
		     then simplify ocstl (L.fromInt l)
		     else ())
		 ()
		 cs;
	     (*D.printdebug2 (S.printState state);*)
	     (*D.printdebug2 ("-+-+-+-+-+-U-+-+-+-+-+-\n");*)
	     Success state)
	(*
	 case cs of
	     [] => Success state
	   | ((l, cst) :: xs) =>
	     let
		 (*val _ = print ("LAB: " ^ (O.printelt l) ^ "\n")*)
		 val (state', srec') = simplify state srec (E.extcs l cst) l
	     in run state' srec' xs
	     end
	     handle errorex err => Error err*)

	(*val _ = occurs_time := 0*)
	(*val _ = temp_time := 0*)
	val timer = VT.startTimer ()
	val _ = sigVsStrOFF ()
	val _ = sigVsStrTypOFF ()
	val ret = run (E.singcst (L.dummyLab, E.CSTLET env))
	    handle errorex err =>
		   ((*D.printdebug2 (L.toString (ERR.getL err));*)
		    if L.isin L.builtinLab (ERR.getL err)
		    then Error (ERR.setB (ERR.stripDummys err) true,  state)
		    else Error (ERR.setB (ERR.stripDummy  err) false, state))
	val _ = D.printdebug1 ("[unification] "
			       ^ VT.milliToString "" timer
			      (*^ "  ::inst:" ^ LargeInt.toString (!temp_time) ^ ":"*)
			      (*^ "  ::occurs:" ^ LargeInt.toString (!occurs_time) ^ ":"*)
			      (*^ " " ^ FI.toString filters*))

    in ret
    end
(*val unifstate' = fn cs        =>
		 fn projlab   =>
		 fn filter    =>
		 fn state     =>
		 fn state'    =>
		 fn bcontinue =>
		    MLton.Profile.withData
			(MLton.Profile.Data.malloc (),
		      fn () => unifstate' cs filters state state' bcontinue)*)

(*fun unifstate cs filters state =
    unifstate' cs filters state NONE false

fun unif cs filters =
    unifstate' cs filters (S.initState ()) NONE false*)

end
