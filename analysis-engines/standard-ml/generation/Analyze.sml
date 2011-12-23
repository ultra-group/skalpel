(* Copyright 2009 Heriot-Watt University
 * Copyright 2010 Heriot-Watt University
 * Copyright 2011 Heriot-Watt University
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
 *  o Authors:     Vincent Rahli
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        20 May 2010
 *  o File name:   Analyze.sml
 *  o Description: This file contains our constraint generator.
 *      It defines the structure Analyze that has the signature
 *      ANALYZE itself is defined in Analyze.sig
 *)

structure Analyze :> ANALYZE = struct

structure T  = Ty
structure A  = AstSML
structure V  = ValuePol
structure E  = Env
structure I  = Id
structure L  = Label
structure D  = Debug
structure C  = ConsId
structure EL = ExtLab
structure NA = Name
structure OP = Op
structure CL = ClassId
structure CD = LongId
structure EH = ErrorHandler
structure D  = Debug

(* TODO: true if we want to have recursive types.  This is for the future *)
val rectype = false

(* Name of the open structure in the basis.  We should get rid of that! *)
(*val basisopen = "Basis"*)

val notFullySt  = "not been fully implemented yet"
val poorlySt    = "not been implemented yet"
val implementSt = "not been implemented yet"
val cannotSt    = "cannot be rebound"

(* Singular *)
fun sorryCsS    l x = E.CSSPARS (L.singleton l, "sorry, '" ^ x ^ "' has "  ^ implementSt)
(* Plurial *)
fun sorryCsP    l x = E.CSSPARS (L.singleton l, "sorry, '" ^ x ^ "' have " ^ implementSt)
fun poorlyCs    l x = E.CSSWARN (L.singleton l, "sorry, '" ^ x ^ "' has "  ^ poorlySt)
fun notFullyCs  l x = E.CSSWARN (L.singleton l, "sorry, '" ^ x ^ "' has "  ^ notFullySt)
(* cannot be rebound *)
fun reboundCs   l x = E.CSSPARS (L.singleton l, x ^ " " ^ cannotSt)
(* syntactic error *)
fun syntacticCs l x = E.CSSPARS (L.singleton l, x ^ " is syntacticly incorrect")
(* fixity *)
fun fixityCs    l   = E.CSSPARS (L.singleton l, "fixity precedence must be between 0 and 9")
(* char *)
fun charBadCs   l   = E.CSSPARS (L.singleton l, "not a character")
(* char *)
fun charSizeCs  l   = E.CSSPARS (L.singleton l, "character constant must be of length 1")
(* cannot access file *)
fun accessCs    l   = E.CSSPARS (L.singleton l, "cannot access file")

fun clearRebound css =
    List.mapPartial (fn x as (E.CSSPARS (ll, s)) =>
			if String.isSuffix cannotSt s
			then NONE
			else SOME x
		      | x => SOME x)
		    css

(*fun clearNotImplemented [] = []
  | clearNotImplemented ((x as (E.CSSPARS (ll, s))) :: xs) =
    if String.isSuffix implementSt s
    then clearNotImplemented xs
    else x :: clearNotImplemented xs
  | clearNotImplemented (x :: xs) = x :: clearNotImplemented xs*)

(*fun clearNotImplementedCs (css, cst) = (clearNotImplemented css, cst)*)


(*

(*******************************)
(* #!/usr/bin/env smlnj-script *)
(*******************************)

(*(* not used anymore *)
fun checkSmlnjScript2 progoneexp =
    case String.compare
	     (Slicing.printSlice (A.Progs [(A.Prog [progoneexp],
					      "",
					      false,
					      A.getProgOneNext progoneexp)])
				 false,
	      "#!/ usr / bin / env smlnj - script;")
     of EQUAL => (D.printdebug2 ("foo"); true)
      | _ => false*)*)


(******************************)
(*           BUILDIN          *)
(******************************)


fun buildin (env, css) ascid true =
    let val (typs, cst1) = NA.getTyName ascid
	val (vids, cst2) = OP.getOpType ascid
	val cst  = E.uenvcst [cst1, cst2]
	val env' = E.ENVSEQ (E.ENVCST cst, E.ENVPOL (E.emtv, E.updateTyps typs (E.projVids vids)))
    in if E.isEmptyIdEnv vids andalso E.isEmptyIdEnv typs
       then (env, css)
       else (E.ENVSEQ (env', env), css)
    end
  | buildin (env, css) ascid false = (env, css)


(****************************************)
(*           FREE IDENTIFIERS           *)
(****************************************)


(*(*fun treatFreeIds (basis1, basis2, cs) =
    let
	(*val _ = D.printdebug2 (E.printcs cs [])*)
	(*val _ = D.printdebug2 ("before")*)
	val unif = U.unifstate' (E.getcsSem cs) NONE NONE (S.initState ()) NONE true
	(*val _ = D.printdebug2 ("after")*)
	val state = case unif of
			U.Success state => state
		      | U.Error _ => raise EH.DeadBranch ""
	val E.BASIS (fct1, idS1, env1) = basis1
	fun isInOpenedStr id idO fgets =
	    if newopen
	    then
		foldr (fn ((_, ev), b) =>
			  let
			      (* TODO: take care, buildenv should also go through the opnenvs *)
			      val (env, _, _, _) = C.buildenv (E.ENVVAR ev) state false
			  (*val _ = D.printdebug2 (Int.toString id)
			   val _ = D.printdebug2 (E.printEnv env "")*)
			  in case env of
				 (E.ENVCON _) =>
				 (case  E.plusproj (E.uenv (map (fn f => f env) fgets)) id of
				      [] => b
				    | _ => true)
			       | _ => b
			  end) false idO
	    else false
	fun treatIdEnv idenv fgets =
	    E.foldrienv
		(fn (k, (semty, idO), css) =>
		    if isInOpenedStr k idO fgets
		    then css
		    else
			foldr
			    (fn (etv, css) =>
				E.conscssSyn (E.CSD (L.singleton (C.getBindL etv))) css)
			    css
			    semty)
		E.emcss
		idenv
	fun treatStrEnv (E.ENVSEQ strenv) =
	    E.foldrienv
		(fn (k, (semty, idO), css) =>
		    if isInOpenedStr k idO [E.getSS]
		    then css
		    else
			foldr
			    (fn (exv, css) =>
				E.conscssSyn (E.CSD (L.singleton (E.getextenvL exv))) css)
			    css
			    semty)
		E.emcss
		strenv
	fun treatSigEnv sigenv =
	    E.foldrienv
		(fn (k, semty, css) =>
		    (foldr
			 (fn (exv, css) =>
			     E.conscssSyn (E.CSD (L.singleton (E.getextenvL exv))) css)
			 css
			 semty))
		E.emcss
		sigenv
	fun treatEnv (E.ENVCON ((idG, idX, idC, idT, _, idS, _, _), _, _)) =
	    let (* TODO: do something for the free classes as well *)
		val cssG = treatIdEnv  idG [E.getVids, E.getC]
		val cssC = treatIdEnv  idC [E.getC]
		val cssT = treatIdEnv  idT [E.getT]
		val cssS = treatStrEnv idS
	    in E.uenvcsSyns [cssG, cssC, cssT, cssS]
	    end
	  | treatEnv _ = E.emcss
	val css1 = treatEnv env1
	val css2 = treatSigEnv idS1
	val css  = E.uenvcsSyn css1 css2
    in (basis1, basis2, E.concatcsSyn css cs)
    end*)

fun treatFreeIds (env1, env2, cs) =
    let fun treatGenEnv idenv fgets =
	    E.foldrienv
		(fn (k, semty, css) =>
		    foldr
			(fn (etv, css) =>
			    E.conscssSyn (E.CSD (L.singleton (E.getBindL etv))) css)
			css
			semty)
		E.emcss
		idenv
	fun treatEnv (env as E.ENVCON _) =
	    let (* TODO: do something for the free classes as well *)
		val cssVids = treatGenEnv (E.getVids env) [E.getVids, E.getCons]
		val cssCons = treatGenEnv (E.getCons env) [E.getCons]
		val cssTyps = treatGenEnv (E.getTyps env) [E.getTyps]
		val cssStrs = treatGenEnv (E.getStrs env) [E.getStrs]
		val cssSigs = treatGenEnv (E.getSigs env) [E.getSigs]
	    in E.uenvcsSyns [cssVids, cssCons, cssTyps, cssStrs, cssSigs]
	    end
	  | treatEnv _ = E.emcss
	val css = treatEnv env1
    in (env1, env2, E.concatcsSyn css cs)
    end



(****************************************)
(*           SUB FUNCTIONS              *)
(****************************************)


(* if id is in the list it deletes the corresponding entry *)
(* and returns the associated types                        *)
fun lkup id [] = raise lkupex
  | lkup (id : int) ((x as (y, z)) :: xs) =
    if id = y
    then (z, xs)
    else let val (etl, xss) = lkup id xs in (etl, x :: xss) end


(*(****************************************************)
(* freshning section                                *)
fun initState () = F.finitState ()

fun freshtyvar  x state = F.freshvar x F.TV state
fun freshrowvar x state = F.freshvar x F.RT state
fun freshlabvar x state = F.freshvar x F.LT state
fun freshseqvar x state = F.freshvar x F.SQ state
fun freshtnvar  x state = F.freshvar x F.TN state
fun freshorvar  x state = F.freshvar x F.OR state
fun freshevvar  x state = F.freshvar x F.EV state

fun freshlabty (T.LV lv) state = T.LV (freshlabvar lv state)
  | freshlabty labty     _     = labty

fun freshtyvarlist  xs state = map (fn x => freshtyvar  x state) xs
fun freshenvvarlist xs state = map (fn x => freshevvar  x state) xs
fun freshseqvarlist xs state = map (fn x => freshseqvar x state) xs

fun freshtyname (T.NV var) state = T.NV (freshtnvar var state)
  | freshtyname tytn       _     = tytn

fun freshrowty (T.RV rv)          state = T.RV (freshrowvar rv state)
  | freshrowty (T.RC (lt, ty, l)) state = T.RC (freshlabty lt state, freshty ty state, l)
and freshtyseq (T.SV var)         state = T.SV (freshseqvar var state)
  | freshtyseq (T.SC (trl, b, l)) state = T.SC (map (fn rt => freshrowty rt state) trl, b, l)
and freshty (T.V (tv, b, p))      state = T.V (freshtyvar tv state, b, p)
  | freshty (T.E x)               _     = T.E x (* TODO: check that *)
  | freshty (T.C   (tn, sq, l))   state = T.C   (freshtyname tn state, freshtyseq sq state, l)
  | freshty (T.App (sq, ty, l))   state = T.App (freshtyseq  sq state, freshty    ty state, l)
  | freshty (T.Abs (sq, ty, l))   state = T.Abs (freshtyseq  sq state, freshty    ty state, l)
  | freshty (T.Or  (sq, i, j, l)) state = T.Or  (freshtyseq  sq state, freshorvar i  state, j, l)
(*and freshtylist xs state = map (fn x => freshty x state) xs*)

fun freshchk class state = CL.classTYCONmapSeq class (fn x => freshseqvar x state)

fun freshBind {id, scope, bind, class, lab, poly} ffresh state =
    C.consBind id scope (ffresh bind state) (freshchk class state) lab poly

fun freshEnvBind x ffresh state = E.mapExtLab x (fn x => freshBind x ffresh state)
fun freshextty   x state = freshEnvBind x freshty  state

fun freshgenenv xs ffresh state = E.mapenv (fn x => map (fn y => freshEnvBind y ffresh state) x) xs
fun freshvarenv xs state = freshgenenv xs freshty    state
fun freshovcenv xs state = freshgenenv xs freshtyseq state

fun freshtyvenv xs state = E.mapenv (fn x => map (fn y => freshtyvar y state) x) xs

fun freshextenv x  state = freshEnvBind x  freshenv state
and freshstrenv xs state = freshgenenv  xs freshenv state
and freshenv (E.ENVCON {vids, vars, cons, typs, tyvs, strs, sigs, funs, ovcs, info}) state =
    E.consEnvC (freshvarenv vids state)
	       (freshvarenv vars state)
	       (freshvarenv cons state)
	       (freshvarenv typs state)
	       (freshtyvenv tyvs state)
	       (freshstrenv strs state)
	       (freshstrenv sigs state)
	       funs
	       (freshovcenv ovcs state)
	       info
  | freshenv (E.ENVSEQ (env1, env2)) state = E.ENVSEQ (freshenv env1 state, freshenv env2 state)
  | freshenv (E.ENVOPN opnenv)       state = E.ENVOPN opnenv
  | freshenv (E.ENVCST cst)          state = E.ENVCST (freshcst    cst state)
  | freshenv (E.ENVVAR ev)           state = E.ENVVAR (freshevvar  ev  state)
  | freshenv (E.INJSTR eev)          state = E.INJSTR (freshextenv eev state)
  | freshenv (E.INJTYP ety)          state = E.INJTYP (freshextty  ety state)
  | freshenv (E.INJVID ety)          state = E.INJVID (freshextty  ety state)
and freshocst (E.CSTTY  ((x, y), l, s, d)) state = E.CSTTY ((freshty     x state, freshty     y state), l, s, d)
  | freshocst (E.CSTTN  ((x, y), l, s, d)) state = E.CSTTN ((freshtyname x state, freshtyname y state), l, s, d)
  | freshocst (E.CSTSQ  ((x, y), l, s, d)) state = E.CSTSQ ((freshtyseq  x state, freshtyseq  y state), l, s, d)
  | freshocst (E.CSTRT  ((x, y), l, s, d)) state = E.CSTRT ((freshrowty  x state, freshrowty  y state), l, s, d)
  | freshocst (E.CSTLT  ((x, y), l, s, d)) state = E.CSTLT ((freshlabty  x state, freshlabty  y state), l, s, d)
  | freshocst (E.CSTEV  ((x, y), l, s, d)) state = E.CSTEV ((freshenv    x state, freshenv    y state), l, s, d)
  | freshocst (E.CSTCL  ((x, y), l, s, d)) state = E.CSTCL ((x,                   y),                   l, s, d)
  | freshocst (E.CSTIT  ((x, y), l, s, d)) state = E.CSTIT ((freshty     x state, y),                   l, s, d)
  | freshocst (E.CSTIO  ((x, y), l, s, d)) state = E.CSTIO ((freshtyseq  x state, y),                   l, s, d)
  | freshocst (E.CSTIS  ((x, y), l, s, d)) state = E.CSTIS ((freshenv    x state, y),                   l, s, d)
  | freshocst (E.CSTIF  ((x, y), l, s, d)) state = raise EH.DeadBranch ""
  | freshocst (E.CSTEGN _)                 _     = raise EH.DeadBranch "" (* the freshning shouldn't be used with structures *)
  | freshocst (E.CSTGEN _)                 _     = raise EH.DeadBranch ""
  | freshocst (E.CSTVAL _)                 _     = raise EH.DeadBranch ""
(* we don't even need to add the syntactic ones because in complet, the original are added too *)
and freshcst cst state = E.mapicst (fn (_, ocstl) => map (fn x => freshocst x state) ocstl) cst

fun freshTvCst tv cs =
    let val state = initState ()
	val tv'   = freshtyvar tv state
	val cst   = E.projcsSem (freshcst (E.getcsSem cs) state)
    in (tv', cst)
    end

(* the freshener are for typevars, tyconvars, sequencevars, rowvars, labvars *)
fun fresh (x, cst, env) freshfun =
    let val state = initState ()
	val ty'   = freshfun x state
	val _     = D.printdebug1 "(F1)"
	val cst'  = freshcst cst state
	val _     = D.printdebug1 "(F2)"
	val env'  = freshenv env state
	val _     = D.printdebug1 "(F3)"
    in (ty', cst', env')
    end
(****************************************************)*)



(***********************************************************************)
(* harpoonout is used in complet and harpoonin in compone              *)
(* harpoonout: remove constraints or env when a value is a constructor *)
(* harpoonin:  to add assumptions                                      *)
(*fun exttyout [] _ = []
  | exttyout ((x as {id, bind, class, lab, poly}) :: xs) ids =
    if CD.isEmpty (CD.inter asmp (CD.toLong ids))
    then x :: (exttyout xs ids)
    else exttyout xs ids*)

(*fun exttyin xs ids =
    map (fn {id, bind, class, lab, poly} =>
	    C.consextty id
			bind
			class
			(CD.union asmp ids)
			lab
			poly)
	xs*)

(*(* the harpoon down of the paper *)
fun harpoonout xs ids =
    E.mappartenv
	(fn semty =>
	    case exttyout semty ids of
		[] => NONE
	      | x  => SOME x)
	xs*)

(*fun harpoonout xs ids = xs*)

(*
fun harpoonoutstrone (x as (E.STC (_, asmp, _))) ids = if L.disjoint asmp ids then SOME x else NONE
  | harpoonoutstrone (x as (E.LID (_, asmp, _))) ids = if L.disjoint asmp ids then SOME x else NONE
  | harpoonoutstrone (x as (E.STV (_, asmp, _))) ids = if L.disjoint asmp ids then SOME x else NONE
*)

(*fun harpoonoutstr xs ids = xs*)
(*
    E.mappartenv
	(fn semty =>
	    case List.mapPartial (fn x => harpoonoutstrone x ids) semty of
		[]     => NONE
	      | semty' => SOME semty')
	xs
*)

(*fun harpoonin xs ids =
    E.mapenv (fn semty => exttyin semty ids) xs*)

(*
fun harpooninstrone (E.STC (stc, asmp, lab)) ids = E.STC (stc, L.concat asmp ids, lab)
  | harpooninstrone (E.LID (lid, asmp, lab)) ids = E.LID (lid, L.concat asmp ids, lab)
  | harpooninstrone (E.STV (ev,  asmp, lab)) ids = E.STV (ev,  L.concat asmp ids, lab)
*)

(*fun harpooninstr xs ids = xs (*E.mapenv (fn semty => map (fn x => harpooninstrone x ids) semty) xs*)*)

(*fun harpooninenv (E.ENVCON ((idG, idX, idC, idT, idV, idS, idO, idD), x)) idset =
    E.ENVCON ((harpoonin idG idset,
	     harpoonin idX idset,
	     harpoonin idC idset,
	     harpoonin idT idset,
	     idV,
	     harpooninstr idS idset,
	     idO,
	     idD),
	    x)
  | harpooninenv (E.ENVVAR _) _ = raise EH.DeadBranch ""
  | harpooninenv (E.INJSTR _) _ = raise EH.DeadBranch ""
  | harpooninenv (E.INJVID _) _ = raise EH.DeadBranch ""
  | harpooninenv (E.INJTYP _) _ = raise EH.DeadBranch ""*)

fun harpooninenv x _ = x

fun harpoonoutclass1 csb ids =
    let val class = E.getBindC csb
    in case CL.getClassREC class of
	   NONE => (* not a REC *)
	   if CL.classIsVAL class andalso I.isin (E.getBindI csb) ids
	   then NONE
	   else SOME csb
	 | SOME (_, SOME labs) =>
	   (* if there is no constraint on the status and
	    * it turns out that it is a constructor. *)
	   if L.isEmpty labs andalso I.isin (E.getBindI csb) ids
	   then NONE
	   else SOME csb
	 | _ => SOME csb (* because the status is known *)
    end

fun harpoonoutclass2 csb ids = SOME csb

fun harpoonoutcsenvpoly xs ids f =
    List.mapPartial (fn ({id, scope, bind, poly, lab, class}, _, _, _) =>
			f (E.consBind id scope bind class lab (P.restrictPoly poly ids)) ids)
		    xs


fun harpoonoutidenv idenv ids f =
    E.foldrienv (fn (id, sem, idenv) => E.addenv (id, harpoonoutcsenvpoly sem ids f) idenv)
		E.emvar
		idenv

(*(2010-04-16)harpoonout is only used by harpoonoutcs which is not used by SOL8.
 * So we don't need to do anything for ENVSEQ and ENVOPN which are only used in SOL8.*)
fun harpoonoutenv (env as E.ENVCON _) idl =
    E.consEnvC (harpoonoutidenv (E.getVids env) idl harpoonoutclass1)
	       (harpoonoutidenv (E.getVars env) idl harpoonoutclass2)
	       (harpoonoutidenv (E.getCons env) idl harpoonoutclass2) (*(2010-04-06)This one seems useless!*)
	       (E.getTyps env)
	       (E.getTyvs env)
	       (E.getStrs env)
	       (E.getSigs env)
	       (E.getFuns env)
	       (E.getOvcs env)
	       (E.getInfo env)
  | harpoonoutenv _ _ = raise EH.DeadBranch ""

(*fun harpoonoutcsbind (csenv1, csenv2, csenv3, strcsenv, sigcsenv, clenv, opnenv, bk) ids =
    (harpoonoutcsenvpoly csenv1 ids harpoonoutclass1,
     harpoonoutcsenvpoly csenv2 ids harpoonoutclass2,
     csenv3,
     strcsenv,
     sigcsenv,
     clenv,
     opnenv,
     bk)*)

(*fun harpoonoutCsEnv' xs ids =
    E.foldrienv
	(fn (k, x, idenv) =>
	    if L.isin k ids
	    then idenv
	    else E.addenv (k, x) idenv)
	E.emid
	xs*)

fun harpoonoutCsEnv (E.ENVCON {vids, vars, cons, typs, tyvs, strs, sigs, funs, ovcs, info}) labs =
    E.consEnvC (E.outenv vids labs) vars cons typs tyvs strs sigs funs ovcs info
  | harpoonoutCsEnv x _ = x

fun whichUknStatusCsbs (_, csenv, _, _, _, _) ids = I.difference (E.csenvdom csenv) ids

fun harpoonoutocst (x as (E.CSTTY  (_, _, _, idl))) ids = if CD.disjShort ids idl then SOME x else NONE
  | harpoonoutocst (x as (E.CSTTN  (_, _, _, idl))) ids = if CD.disjShort ids idl then SOME x else NONE
  | harpoonoutocst (x as (E.CSTSQ  (_, _, _, idl))) ids = if CD.disjShort ids idl then SOME x else NONE
  | harpoonoutocst (x as (E.CSTRT  (_, _, _, idl))) ids = if CD.disjShort ids idl then SOME x else NONE
  | harpoonoutocst (x as (E.CSTLT  (_, _, _, idl))) ids = if CD.disjShort ids idl then SOME x else NONE
  | harpoonoutocst (x as (E.CSTEV  ((e1, e2), l, s, d))) ids = SOME (E.CSTEV ((harpoonoutCsEnv e1 ids, harpoonoutCsEnv e2 ids), l, s, d))
  | harpoonoutocst (x as (E.CSTCL  (_, _, _, idl))) ids = if CD.disjShort ids idl then SOME x else NONE
  | harpoonoutocst (x as (E.CSTIT  (_, _, _, idl))) ids = if CD.disjShort ids idl then SOME x else NONE
  | harpoonoutocst (x as (E.CSTIO  (_, _, _, idl))) ids = if CD.disjShort ids idl then SOME x else NONE
  | harpoonoutocst (x as (E.CSTIS  (_, _, _, idl))) ids = if CD.disjShort ids idl then SOME x else NONE
  | harpoonoutocst (x as (E.CSTIF  (_, _, _, idl))) ids = if CD.disjShort ids idl then SOME x else NONE
  | harpoonoutocst (x as (E.CSTEGN (y, cst)))    ids = SOME (E.CSTEGN (y, harpoonoutcst cst ids)) (* this constraint shouldn't be removed *)
  | harpoonoutocst (x as (E.CSTGEN (env, cst1, cst2))) ids =
    (fn ids' => SOME (E.CSTGEN (harpoonoutenv env  ids', (* ids/ids', either or *)
				harpoonoutcst cst1 ids',
				harpoonoutcst cst2 ids'))) ids (*(whichUknStatusCsbs csbind ids)*) (* this should not be needed with restrictreccs *)
  | harpoonoutocst (x as (E.CSTVAL (tvsbind1, tvsbind2, tvl, cst))) ids =
    SOME (E.CSTVAL (tvsbind1, tvsbind2, tvl, harpoonoutcst cst ids))
  (*| harpoonoutocst (x as (E.CSTBIND (env, cst1, cst2))) ids = SOME x*) (* We don't do anything here because this function is not needed with BIND constraints. *)
and harpoonoutcst cst ids = E.mapicst
				(fn (_, ocstl) =>
				    List.mapPartial (fn x => harpoonoutocst x ids) ocstl)
				cst

fun harpoonoutcss [] _ = []
  (*| harpoonoutcss ((x as (E.CSG (_, NONE))) :: xs) ids = x :: (harpoonoutcss xs ids)
  | harpoonoutcss ((x as (E.CSG (_, SOME ([], z)))) :: xs) ids =
    if L.isin z ids then (harpoonoutcss xs ids) else x :: (harpoonoutcss xs ids)
  | harpoonoutcss ((x as (E.CSG (_, SOME (p, z)))) :: xs) ids = x :: (harpoonoutcss xs ids)*)
  (**)
  | harpoonoutcss ((x as (E.CSM (_, SOME (E.ID (z, _, _))))) :: xs) ids =
    if I.isin z ids then (harpoonoutcss xs ids) else x :: (harpoonoutcss xs ids)
  | harpoonoutcss ((x as (E.CSM _)) :: xs) ids = x :: (harpoonoutcss xs ids)
  | harpoonoutcss ((x as (E.CSC (_, SOME (E.ID (z, _, _))))) :: xs) ids =
    if I.isin z ids then (harpoonoutcss xs ids) else x :: (harpoonoutcss xs ids)
  | harpoonoutcss ((x as (E.CSC _)) :: xs) ids = x :: (harpoonoutcss xs ids)
  | harpoonoutcss ((x as (E.CSH (_, SOME (E.ID (z, _, _))))) :: xs) ids =
    if I.isin z ids then (harpoonoutcss xs ids) else x :: (harpoonoutcss xs ids)
  | harpoonoutcss ((x as (E.CSH _)) :: xs) ids = x :: (harpoonoutcss xs ids)
  (**)
  | harpoonoutcss ((x as (E.CSJ _)) :: xs) ids = x :: (harpoonoutcss xs ids)
  | harpoonoutcss ((x as (E.CSI _)) :: xs) ids = x :: (harpoonoutcss xs ids)
  | harpoonoutcss ((x as (E.CSA _)) :: xs) ids = x :: (harpoonoutcss xs ids)
  | harpoonoutcss ((x as (E.CSF _)) :: xs) ids = x :: (harpoonoutcss xs ids)
  | harpoonoutcss ((x as (E.CSN _)) :: xs) ids = x :: (harpoonoutcss xs ids)
  | harpoonoutcss ((x as (E.CSV _)) :: xs) ids = x :: (harpoonoutcss xs ids)
  | harpoonoutcss ((x as (E.CSP _)) :: xs) ids = x :: (harpoonoutcss xs ids)
  | harpoonoutcss ((x as (E.CSR _)) :: xs) ids = x :: (harpoonoutcss xs ids)
  | harpoonoutcss ((x as (E.CSE _)) :: xs) ids = x :: (harpoonoutcss xs ids)
  | harpoonoutcss ((x as (E.CSD _)) :: xs) ids = x :: (harpoonoutcss xs ids)
  (**)
  | harpoonoutcss ((x as (E.CSW _)) :: xs) ids = x :: (harpoonoutcss xs ids)
  (**)
  (*| harpoonoutcss ((x as (E.CSB (_, NONE, _))) :: xs) ids = x :: (harpoonoutcss xs ids)
  | harpoonoutcss ((x as (E.CSB (_, SOME (I.ID (z, _)), _))) :: xs) ids =
    if I.isin z ids then (harpoonoutcss xs ids) else x :: (harpoonoutcss xs ids)
  | harpoonoutcss ((x as (E.CSB (_, SOME _, _))) :: xs) ids = x :: (harpoonoutcss xs ids)*)

(* harpoonoutcst is not used by SOL8. *)
fun harpoonoutcs (css, cst) ids = (harpoonoutcss css ids, harpoonoutcst cst ids)

(* TODO: note that this functions shouldn't be needed once we switched from fresh to CSTGEN *)
fun harpooninocst (E.CSTTY  (x, l, s, idl)) ids = E.CSTTY (x, l, s, CD.union ids idl)
  | harpooninocst (E.CSTTN  (x, l, s, idl)) ids = E.CSTTN (x, l, s, CD.union ids idl)
  | harpooninocst (E.CSTSQ  (x, l, s, idl)) ids = E.CSTSQ (x, l, s, CD.union ids idl)
  | harpooninocst (E.CSTRT  (x, l, s, idl)) ids = E.CSTRT (x, l, s, CD.union ids idl)
  | harpooninocst (E.CSTLT  (x, l, s, idl)) ids = E.CSTLT (x, l, s, CD.union ids idl)
  | harpooninocst (E.CSTEV  (x, l, s, idl)) ids = E.CSTEV (x, l, s, CD.union ids idl)
  | harpooninocst (E.CSTCL  (x, l, s, idl)) ids = E.CSTCL (x, l, s, CD.union ids idl)
  | harpooninocst (E.CSTIT  (x, l, s, idl)) ids = E.CSTIT (x, l, s, CD.union ids idl)
  | harpooninocst (E.CSTIO  (x, l, s, idl)) ids = E.CSTIO (x, l, s, CD.union ids idl)
  | harpooninocst (E.CSTIS  (x, l, s, idl)) ids = E.CSTIS (x, l, s, CD.union ids idl)
  | harpooninocst (E.CSTIF  (x, l, s, idl)) ids = E.CSTIF (x, l, s, CD.union ids idl)
  | harpooninocst (E.CSTEGN (x, cst))    _   = E.CSTEGN (x, cst)
  | harpooninocst (E.CSTGEN (x, cst1, cst2)) ids =
    E.CSTGEN (x, harpoonincst cst1 ids, harpoonincst cst2 ids)
  | harpooninocst (E.CSTVAL (x, y, z, cst)) ids = E.CSTVAL (x, y, z, harpoonincst cst ids)
  (*| harpooninocst (E.CSTBIND (x, cst1, cst2)) ids =
    E.CSTBIND (x, harpoonincst cst1 ids, harpoonincst cst2 ids)*)
and harpoonincst cst ids = E.mapicst (fn (_, ocstl) => map (fn x => harpooninocst x ids) ocstl) cst
(**********************************************************)


fun uenv envl =
    let val timer = VT.startTimer ()
	(*val _ = D.printdebug1 "(U1)"*)
	val env = E.uenv envl
	(*val _ = D.printdebug1 "(U2)"*)
	val _ = D.printdebug1 (VT.milliToString "U:" timer)
    in env
    end

fun getenvEnv envl = map (fn (env, _, _) => env) envl
fun getenvTy  envl = map (fn (_, ty, _)  => ty)  envl
fun getenvCs  envl = map (fn (_, _, cs)  => cs)  envl

fun getenvTyTwo envl = ListPair.unzip (getenvTy envl)
fun getenvTyThree envl =
    foldr (fn ((x, y, z), (xs, ys, zs)) => (x :: xs, y :: ys, z :: zs))
	  ([], [], [])
	  (getenvTy envl)
fun getenvCsThree envl =
    foldr (fn ((x, y, z), (xs, ys, zs)) => (x :: xs, y :: ys, z :: zs))
	  ([], [], [])
	  (getenvCs envl)

fun getenvTyFstTwo envl = map (fn (_, (x, _), _)  => x)  envl
fun getenvTySndTwo envl = map (fn (_, (_, x), _)  => x)  envl

(*fun getenvG envl = map (fn x => E.getVids x) (getenvEnv envl)
fun getenvX envl = map (fn x => E.getVars x) (getenvEnv envl)
fun getenvC envl = map (fn x => E.getCons x) (getenvEnv envl)
fun getenvT envl = map (fn x => E.getTyps x) (getenvEnv envl)
fun getenvV envl = map (fn x => E.getTyvs x) (getenvEnv envl)*)

fun renametyvar tv asc = (#1 (lkup tv asc)) handle lkupex => tv

(*fun renametyvarlist tvl asc = map (fn x => renametyvar x asc) tvl*)

fun renamerowty (T.RV rv)          _   = T.RV rv
  | renamerowty (T.RC (lv, ty, l)) asc = T.RC (lv, renamety ty asc, l)
and renamesq (T.SV sv)             _   = T.SV sv
  | renamesq (T.SC (rtl, b, l))    asc = T.SC (map (fn x => renamerowty x asc) rtl, b, l)
and renamety (T.V (tv, b, p))      asc = T.V (renametyvar tv asc, b, p)
  | renamety (T.E (n, tv, l))      asc = T.V (renametyvar n asc, SOME l, T.POLY) (*(2010-02-05) Why SOME l?*)
  | renamety (T.C (tn, sq, l))     asc = T.C (tn, renamesq sq asc, l)
  | renamety (T.App (sq, t, l))    asc = T.App (renamesq sq asc, renamety t asc, l)
  | renamety (T.Abs (sq, t, l))    asc = T.Abs (renamesq sq asc, renamety t asc, l)
  | renamety (T.Or  (sq, i, j, l)) asc = T.Or  (renamesq sq asc, i, j, l)
(*and renametylist xs asc = map (fn x => renamety x asc) xs*)

fun renamecsenv xs ren = map (fn (id, tv1, tv2, l) => (id, renametyvar tv1 ren, renametyvar tv2 ren, l)) xs

fun renameocst (E.CSTTY  ((x, y), l, s, ids)) asc = E.CSTTY ((renamety    x asc, renamety    y asc), l, s, ids)
  | renameocst (E.CSTTN  ((x, y), l, s, ids)) asc = E.CSTTN ((x,                 y),                 l, s, ids)
  | renameocst (E.CSTSQ  ((x, y), l, s, ids)) asc = E.CSTSQ ((renamesq    x asc, renamesq    y asc), l, s, ids)
  | renameocst (E.CSTRT  ((x, y), l, s, ids)) asc = E.CSTRT ((renamerowty x asc, renamerowty y asc), l, s, ids)
  | renameocst (E.CSTLT  ((x, y), l, s, ids)) asc = E.CSTLT ((x,                 y),                 l, s, ids)
  | renameocst (E.CSTEV  ((x, y), l, s, ids)) asc = E.CSTEV ((x,                 y),                 l, s, ids) (* all these cases shouldn't happen - YES it can for CSTEV at least, see f_typbind *)
  | renameocst (E.CSTCL  ((x, y), l, s, ids)) asc = E.CSTCL ((x,                 y),                 l, s, ids)
  | renameocst (E.CSTIT  ((x, y), l, s, ids)) asc = E.CSTIT ((renamety    x asc, y),                 l, s, ids)
  | renameocst (E.CSTIO  ((x, y), l, s, ids)) asc = E.CSTIO ((renamesq    x asc, y),                 l, s, ids)
  | renameocst (E.CSTIS  ((x, y), l, s, ids)) asc = E.CSTIS ((x,                 y),                 l, s, ids)
  | renameocst (E.CSTIF  ((x, y), l, s, ids)) asc = E.CSTIF ((x,                 y),                 l, s, ids)
  | renameocst (E.CSTEGN (x, cst))        asc = raise EH.DeadBranch ""
  | renameocst (E.CSTGEN (x, cst1, cst2)) asc = raise EH.DeadBranch ""
    (*E.CSTGEN (renamecsenv x asc,
	      renamecsenv y asc,
	      renamecsenv z asc,
	      renamecst cst1 asc,
	      renamecst cst2 asc)*)
  | renameocst (E.CSTVAL (x, y, z, cst)) asc = raise EH.DeadBranch ""
  (*| renameocst (E.CSTBIND (x, cst1, cst2)) asc = raise EH.DeadBranch ""*)
and renamecst cst ren = E.mapicst (fn (_, ocstl) => map (fn x => renameocst x ren) ocstl) cst

fun renamecs cs ren = E.updatecsSem (renamecst (E.getcsSem cs) ren) cs

fun genTyVarSeq ntv = map (fn _ => T.freshtyvar ()) ntv


(* getLabExtSt takes a list of extended type variables and returns a list of pairs.
 * A pair contains the label of the extended type variable and either
 * E.C if the variable if for a datatype/exception constructor or E.I otherwise.
 * This extra information on the status is used in consCSM.
 * This is so that we don't report errors for multi-constructors in patterns. *)
fun getlabExt xs = map (fn x => E.getBindL x) xs

fun getlabExtSt xs =
    map (fn x => if CL.classIsCON (E.getBindC x)
		 then (E.getBindL x, E.C)
		 else (E.getBindL x, E.I))
	xs

fun getlabExtChkRecList xs =
    List.mapPartial (fn x =>
			let val cl = E.getBindC x
			in if CL.classIsREC cl
			   then SOME (CL.getClassREClabs cl)
			   else NONE
			end)
		    xs
(*
fun getlabExtChkCon xs =
    foldr (fn (x, y) => case C.getBindC x of (CL.CON ll) => L.concat ll y | _ => y) L.empty xs
fun getlabExtChkDat xs =
    foldr (fn (x, y) => case C.getBindC x of (CL.DAT ll) => L.concat ll y | _ => y) L.empty xs
fun getlabExtChkNon xs =
    foldr (fn (x, y) => case C.getBindC x of (CL.NON ll) => L.concat ll y | _ => y) L.empty xs
(* TODO: check that *)
fun getlabExtChkVal xs =
    foldr (fn (x, y) =>
	      case C.getBindC x of
		  (CL.VAL (u, v, ll)) => (u andalso (#1 y), L.concat v (#2 y), L.concat ll (#3 y))
		| _                  => y)
	  (true, L.empty, L.empty)
	  xs
*)

(**************************************************************************)
(* restrictrec is for when a value is confirmed to be a variable with rec *)

(* Work on this one *)
(* can we remove it from f_fvalbindcore? *)
(* can we do the E.outenv (see f_fvalbind) in there? *)
(*(2010-04-16)restrictrecenv is only applied on ENVCON.*)
fun restrictrecenv (env as E.ENVCON _) idenv =
    E.consEnvC (E.restrValue (E.getVids env) idenv)
	       (E.restrValue (E.getVars env) idenv) (*(2010-04-06)Why do we do that for vars, cons and typs?*)
	       (E.restrValue (E.getCons env) idenv)
	       (E.restrValue (E.getTyps env) idenv)
	       (E.getTyvs env)
	       (E.getStrs env)
	       (E.getSigs env)
	       (E.getFuns env)
	       (E.getOvcs env)
	       (E.getInfo env)
  | restrictrecenv _ _ = raise EH.DeadBranch ""

fun restrictrecenv' (env1 as E.ENVCON _) (env2 as E.ENVCON _) = restrictrecenv env1 (E.getVars env2)
  | restrictrecenv' env _ = env (*(2010-04-19)Do we need something more sophisticated for ENVSEQ and ENVOPN?*)

(*fun restrictrecCsEnv (E.ENVCON {vids, vars, cons, typs, tyvs, strs, sigs, funs, opns, ovcs, info}) labs =
    E.consEnvC (E.outenv vids labs)
	       (E.uenv [vars, E.toREC (E.inenv vids labs)])
	       cons typs tyvs strs sigs funs opns ovcs info
  | restrictrecCsEnv (x as E.ENVVAR _) _ = x
  | restrictrecCsEnv (x as E.INJSTR _) _ = x
  | restrictrecCsEnv (x as E.INJVID _) _ = x
  | restrictrecCsEnv (x as E.INJTYP _) _ = x*)

(*fun restrictrecbinds csenv1 idenv = map (fn x => E.restrValueBind x idenv) csenv1*)

(*fun restrictRecIdEnv idenv idenv' =
    E.foldrienv (fn (id, sem, idenv) => E.addenv (id, restrictrecbinds sem idenv') idenv)
		E.emvar
		idenv*)

(*(2010-04-16)We can't do much (at least not for the ENVCONs with a ENVOPN on their left)
 * in case of a ENVSEQ because of opens.*)
fun restrictRecEnv (env as E.ENVCON _) idenv =
    E.consEnvC (E.restrValue (E.getVids env) idenv)
	       (E.getVars env)
	       (E.getCons env)
	       (E.getTyps env)
	       (E.getTyvs env)
	       (E.getStrs env)
	       (E.getSigs env)
	       (E.getFuns env)
	       (E.getOvcs env)
	       (E.getInfo env)
  | restrictRecEnv x _ = x

(*fun restrictRecCsBind csbind idenv = restrictRecEnv csbind idenv*)

(*fun restrictRecCsBind (x1, x2, x3, x4, x5, x6, x7, b) idenv =
    (restrictrecbinds x1 idenv, x2, x3, x4, x5, x6, x7, b)*)

(*fun restrictrecexttvlist xs idenv = map (fn x => E.restrValueBind x idenv) xs*)

fun restrictRecCs csc idenv =
    let val _ = D.printdebug1 "(RS1)"
	val idset = E.dom idenv
	fun lkup id = case E.findenv id idenv of
			  NONE => raise lkupex
			| SOME etvset => C.getlabExtChkRec (E.stripExtLab etvset) (* we can raise EH.DeadBranch "" if not REC *)
	(*fun moverec csenv1 csenv2 =
	    (fn (x, y) => (x, y @ csenv2))
		(List.partition
		     (fn csb => if L.isin (E.csbId csb) idset then false else true)
		     csenv1)*)
	fun conocst (E.CSTTY  (x, l, s, res)) = E.CSTTY (x, l, s, CD.diffShort idset res)
	  | conocst (E.CSTTN  (x, l, s, res)) = E.CSTTN (x, l, s, CD.diffShort idset res)
	  | conocst (E.CSTSQ  (x, l, s, res)) = E.CSTSQ (x, l, s, CD.diffShort idset res)
	  | conocst (E.CSTRT  (x, l, s, res)) = E.CSTRT (x, l, s, CD.diffShort idset res)
	  | conocst (E.CSTLT  (x, l, s, res)) = E.CSTLT (x, l, s, CD.diffShort idset res)
	  | conocst (E.CSTEV  (x, l, s, res)) = E.CSTEV (x, l, s, CD.diffShort idset res)
	  | conocst (E.CSTCL  (x, l, s, res)) = E.CSTCL (x, l, s, CD.diffShort idset res)
	  | conocst (E.CSTIT  (x, l, s, res)) = E.CSTIT (x, l, s, CD.diffShort idset res)
	  | conocst (E.CSTIO  (x, l, s, res)) = E.CSTIO (x, l, s, CD.diffShort idset res)
	  | conocst (E.CSTIS  (x, l, s, res)) = E.CSTIS (x, l, s, CD.diffShort idset res)
	  | conocst (E.CSTIF  (x, l, s, res)) = E.CSTIF (x, l, s, CD.diffShort idset res)
	  | conocst (E.CSTEGN (x, cst))    = E.CSTEGN (x, concst cst)
	  | conocst (E.CSTGEN (csbind, cst1, cst2)) =
	    E.CSTGEN (restrictRecEnv csbind idenv, concst cst1, concst cst2)
	  (*moverec x y*)
	  (* if a value appears to be a variable for sure then we move the binding to the second csenv *)
	  | conocst (E.CSTVAL (x, y, z, cst)) =
	    E.CSTVAL (x, y, E.restrValueBindList z idenv, concst cst)
	  (*| conocst (E.CSTBIND (env, cst1, cst2)) = E.CSTBIND (env, cst1, cst2)*)
	(* We don't do anything for BIND constraints because this function shouldn't be needed with these constraints. *)
	and concst cst = E.mapicst (fn (_, ocstl) => map (fn x => conocst x) ocstl) cst
	fun conocss (E.CSM (y, SOME (E.ID (z, l, s)))) =
	    (L.foldr (fn (l, fs) => (E.CSM (L.cons l y, SOME (E.LA l))) :: fs) [] (lkup z)
	     handle lkupex => [E.CSM (y, SOME (E.ID (z, l, s)))])
	  | conocss (E.CSM x) = [E.CSM x]
	  | conocss (E.CSC (y, SOME (E.ID (z, l, s)))) =
	    (L.foldr (fn (l, fs) => (E.CSC (L.cons l y, SOME (E.LA l))) :: fs) [] (lkup z)
	     handle lkupex => [E.CSC (y, SOME (E.ID (z, l, s)))])
	  | conocss (E.CSC x) = [E.CSC x]
	  | conocss (E.CSH (y, SOME (E.ID (z, l, s)))) =
	    (L.foldr (fn (l, fs) => (E.CSH (L.cons l y, SOME (E.LA l))) :: fs) [] (lkup z)
	     handle lkupex => [E.CSH (y, SOME (E.ID (z, l, s)))])
	  | conocss (E.CSH x) = [E.CSH x]
	  (**)
	  (*| conocss (E.CSG (y, NONE)) = [E.CSG (y, NONE)]
	  | conocss (E.CSG (y, SOME ([], z))) = (* move that in complet *)
	    (L.foldr (fn (l, fs) => (E.CSG (L.cons l y, NONE)) :: fs) [] (lkup z)
	     handle lkupex => [E.CSG (y, SOME ([], z))])
	  | conocss (E.CSG (y, SOME (p, z))) = [E.CSG (y, SOME (p, z))]*)
	  (**)
	  | conocss (x as (E.CSJ _)) = [x]
	  | conocss (x as (E.CSI _)) = [x]
	  | conocss (x as (E.CSA _)) = [x]
	  | conocss (x as (E.CSF _)) = [x]
	  | conocss (x as (E.CSN _)) = [x]
	  | conocss (x as (E.CSV _)) = [x]
	  | conocss (x as (E.CSP _)) = [x]
	  | conocss (x as (E.CSR _)) = [x]
	  | conocss (x as (E.CSE _)) = [x]
	  | conocss (x as (E.CSD _)) = [x]
	  (**)
	  | conocss (x as (E.CSW _)) = [x]
	  (**)
	  (*| conocss (E.CSB (y, NONE, tvop)) = [E.CSB (y, NONE, tvop)]
	  | conocss (E.CSB (y, SOME (I.ID (z, l)), tvop)) = (* move that in complet *)
	    (L.foldr (fn (l, fs) => (E.CSB (L.cons l y, NONE, tvop)) :: fs) [] (lkup z)
	     handle lkupex => [E.CSB (y, SOME (I.ID (z, l)), tvop)])
	  | conocss (E.CSB (y, SOME lid, tvop)) = [E.CSB (y, SOME lid, tvop)]*)
	fun concss css = foldr (fn (x, y) => (conocss x) @ y) [] css
	val cs = (concss (E.getcsSyn csc), concst (E.getcsSem csc))
	val _ = D.printdebug1 "(RS2)"
    in cs
    end
(**************************************************************************)


fun getPairs []        = []
  | getPairs [x, y]    = [L.ord [x, y]]
  | getPairs (x :: xs) = (foldr (fn (u, v) => (L.ord [x, u]) :: v) [] xs) @ (getPairs xs)

(* St stands for status. *)
fun treatPairSt (x1, E.C) (x2, E.C) = (L.ord [x1, x2], E.C)
  | treatPairSt (x1, _  ) (x2, _  ) = (L.ord [x1, x2], E.I)

(* This is like getPairs but when we have an extra info on status
 * such as when dealing with the output of getlabExtSt. *)
fun getPairsSt []        = []
  | getPairsSt [x, y]    = [treatPairSt x y]
  | getPairsSt (x :: xs) = (foldr (fn (u, v) => (treatPairSt x u) :: v)
				  []
				  xs) @ (getPairsSt xs)

fun consCSM1 idenvl b =
    let fun fb x st = if b then SOME (E.ID (x, L.dummylab, st)) else NONE
	fun ff (x, y, z) =
	    (map (fn (u, st) => E.CSM (u, fb x st))
		 (getPairsSt (getlabExtSt y))) @ z
    (*val gg = fn (x, y) => E.CSM (y, fb x)*)
    in E.foldlienv ff [] (E.uenv idenvl)
    end*)

fun sameNameFun vids =
    E.foldrienv (fn (id1, sem1, css) =>
		    foldr (fn (bind1, css) =>
			      E.foldrienv (fn (id2, sem2, css) =>
					      if I.eqId id1 id2
					      then css
					      else foldr (fn (bind2, css) =>
							     let val lab1 = E.getBindL bind1
								 val lab2 = E.getBindL bind2
								 val labs = L.ord [lab1, lab2]
							     in E.conscss (E.CSSFNAM labs) css
							     end)
							 css
							 sem2)
					  css
					  vids)
			  css
			  sem1)
		[]
		vids

fun getLabsBinds binds = map (fn bind => E.getBindL bind) binds

fun getPairs [] = []
  | getPairs (lab :: labs) =
    foldr (fn (lab', pairs) => (L.ord [lab, lab']) :: pairs)
	  (getPairs labs)
	  labs

fun consCSM vids =
    E.foldrienv (fn (id, sem, css) =>
		    let val labs  = getLabsBinds sem
			val pairs = getPairs labs
			val css'  = map (fn labs => E.CSSMULT labs) pairs
		    in E.uenvcss [css, css']
		    end)
		[]
		vids

fun consCSMfval vidss =
    #1 (foldr (fn (vids1, (css, vidss)) =>
		  (foldr (fn (vids2, css) =>
			     E.foldrienv (fn (id, sem1, css) =>
					     foldr (fn (bind2, css) =>
						   foldr (fn (bind1, css) =>
							     let val lab1 = E.getBindL bind1
								 val lab2 = E.getBindL bind2
								 val labs = L.ord [lab1, lab2]
							     in E.conscss (E.CSSMULT labs) css
							     end)
							 css
							 sem1)
						   css
						   (E.plusproj vids2 id))
					 css
					 vids1)
			 css
			 vidss,
		   vids1 :: vidss))
	      ([], [])
	      vidss)

(*fun consCSM idenvs b =
    case (b, SO.getSol ()) of
	(true, SO.SOL8) => [] (*(2010-02-23)Because this is dealt with during unification.*)
      | _ => consCSM1 idenvs b

fun consCSMstr strenv =
    let fun getLabStrSem [] = []
	  | getLabStrSem (extenv :: xs) = (E.getBindL extenv) :: (getLabStrSem xs)
	fun ff (y, z) = (map (fn u => E.CSM (u, NONE)) (getPairs (getLabStrSem y))) @ z
    (*val gg = fn (x, y) => E.CSM (y, fb x)*)
    in E.foldlenv ff [] (E.uenv strenv)
    end

fun consCSMsig sigenv =
    let fun getLabStrSem [] = []
	  | getLabStrSem (extenv :: xs) = (E.getBindL extenv) :: (getLabStrSem xs)
	fun ff (y, z) = (map (fn u => E.CSM (u, NONE)) (getPairs (getLabStrSem y))) @ z
    (*val gg = fn (x, y) => E.CSM (y, fb x)*)
    in E.foldlenv ff [] (E.uenv sigenv)
    end*)

fun consCSMlab [] = []
  | consCSMlab ((s : string, l) :: xs) =
    let val css1 =
	    List.mapPartial
		(fn (s', l') => if s = s'
				then SOME (E.CSSMULT (L.ord [l, l']))
				else NONE)
		xs
	val css2 = consCSMlab xs
    in E.uenvcss [css1, css2]
    end

(*(*
fun consCSMstr [] = []
  | consCSMstr ((_, semty) :: xs) =
    let
	fun extractpath (p, n, _) =
	    let
		val (vs, ls) = foldr
				   (fn ((v, l1, l2), (vs, ls)) => (v :: vs, L.cons l1 (L.cons l2 ls)))
				   ([], L.empty)
				   p
	    in (ls, vs, n)
	    end
	fun searchlid [] _ _ = []
	  | searchlid ((E.LID (lid, asmp, lab)) :: ys) (ls, vs, n) l =
	    let
		val (ls', vs', n') = extractpath lid
	    in if (vs, n) = (vs', n')
	       then (E.CSM (L.cons lab (L.concat ls ls'), SOME (vs, n))) :: (searchlid ys (ls, vs, n) l)
	       else searchlid ys (ls, vs, n) l
	    end
	  | searchlid (_ :: ys) (ls, vs, n) l = searchlid ys (ls, vs, n) l
	fun searchstc [] _ _ = []
	  | searchstc ((E.STC (str, asmp, lab)) :: ys) (ls, vs, n) l =
	    let
		val (ls', vs', n') = extractpath str
	    in if (vs, n) = (vs', n')
	       then (E.CSM (L.cons lab (L.concat ls ls'), SOME (vs, n))) :: (searchstc ys (ls, vs, n) l)
	       else searchstc ys (ls, vs, n) l
	    end
	  | searchstc (_ :: ys) (ls, vs, n) l = searchstc ys (ls, vs, n) l
	fun scan [] = []
	  | scan ((E.STC (stc, asmp, lab)) :: ys) = (searchstc ys (extractpath stc) lab) @ (scan ys)
	  | scan ((E.LID (lid, asmp, lab)) :: ys) = (searchlid ys (extractpath lid) lab) @ (scan ys)
	  | scan ((E.STV extenv)           :: ys) = scan ys
    in (scan semty) @ (consCSMstr xs)
    end
*)

fun consCSMfval idenv =
    E.foldrenv
	(fn (semty, cs) =>
	    (map (fn ll => E.CSM (ll, NONE)) (L.getpairss (getlabExtChkRecList semty))) @ cs)
	[]
	idenv

fun consCSMren tvl =
    let fun f [] = ([], [], E.emcst, [])
	  | f ((n, lab1, lab2) :: xs) =
	    let val (ren, tvl, cs, occ) = f xs
		val (ll, occ') = lkup n occ handle lkupex => ([], occ)
	    in (ren, (#2 (#1 (lkup n ren))) :: tvl, cs, (n, lab1 :: ll) :: occ')
	       handle lkupex =>
		      let
			  val tv1 = T.freshtyvar ()
			  val tv2 = T.freshtyvar ()
			  val tv3 = T.freshtyvar ()
			  val c1  = E.genCstTyEm (T.consV tv1) (T.consV tv3) lab1
			  val c2  = E.genCstTyEm (T.consV tv3) (T.consV tv2) lab2
		      in ((n, (tv2, tv1)) :: ren,
			  tv1 :: tvl,
			  E.conscstSem (lab1, c1) (E.conscstSem (lab2, c2) cs),
			  (n, lab1 :: ll) :: occ')
		      end
	    end
	val (ren', tvl, cst, occ) = f tvl
	val g = fn ((n, []), y)  => raise EH.DeadBranch ""
		 | ((n, [l]), y) => y
		 | ((n, ll), y)  =>
		   E.uenvcsSyn
		       (foldl (fn (u, v) => E.conscssSyn (E.CSM (u, NONE)) v)
			      E.emcss
			      (getPairs ll)) y
	val css = foldr g E.emcss occ
	val ren = foldr (fn ((n, (tv1, tv2)), y) => (n, tv1) :: y) [] ren'
    in (ren, tvl, (css, cst))
    end

fun extractpathpcon (A.PconBool (_, _, _, l, _)) = SOME (l, NONE)
  | extractpathpcon (A.PconNil  (_, _, _, l, _)) = SOME (l, NONE)
  | extractpathpcon (A.PconRef  (_, _, _, l, _)) = SOME (l, NONE)
  | extractpathpcon  A.PconDots                  = NONE

fun extractpathid (A.Ident (_, v, _, l, _)) = SOME (l, v)
  | extractpathid (A.IdentPcon pc)          = NONE
  | extractpathid A.IdentDots               = NONE

fun extractpathlid (A.LongIdQual (sid, lid, _, l, _)) =
    (case (A.getlabidStrId sid, extractpathlid lid) of
	 (SOME (n, l'), SOME (ll, nl, v)) => SOME (L.cons l' (L.cons l ll), n :: nl, v)
       | _                                => NONE)
  | extractpathlid (A.LongIdId id) =
    (case extractpathid id of
	 SOME (l, v) => SOME (L.singleton l, [], v)
       | _           => NONE)
  | extractpathlid (A.LongIdDots _) = NONE

(* rule41 of the Definition of SML *)
(* consCSC stands for construction of CSC constraints
 * see Env.sig for a CSC *)
fun consCSC' lab lid =
    (case extractpathlid lid of
	 SOME (ll, [], v) => [E.CSC (L.cons lab ll, SOME (E.ID (v, lab, E.I)))]
       | SOME _ => [] (* lid is a long id and these kind of errors for long ids are handled by the unifier *)
       | _      => [])

fun consCSC lab lid =
    case SO.getSol () of
	SO.SOL8 => []
      | _ => consCSC' lab lid

(*(* rule35 of the Definition of SML *)
fun consCSG lid =
    (case extractpathlid lid of
	 SOME (ll, [], v) => [] (* not a long one! *)
       | SOME (ll, nl, v) => [E.CSG (ll, SOME (nl, v))]
       | _                => [])*)

fun checkArityTyCon (env1 as E.ENVCON _) (env2 as E.ENVCON _) =
    let val tns  = E.dom (E.getTyps env1)
	val typs = E.getTyps env2
    in I.foldr (fn (tn, cs) =>
		   let val sv  = T.freshseqvar ()
		       val etv = E.plusproj typs tn
		       fun conscs class l =
			   (l, map (fn x => E.genCstSqEm (T.SV sv) (T.SV x) l)
				   (CL.classTYP [class]))
		   in foldl (fn (({id, scope, bind, class, lab, poly}, _, _, _), y) =>
				E.conscstSems (conscs class lab) y)
			    cs
			    etv
		   end)
	       E.emcst
	       tns
    end
  | checkArityTyCon _ _ = E.emcst


(*fun checkArityTyCon tns env =
    L.foldr
    (fn (tn, cs) =>
	let
	    val tv  = T.freshtyvar ()
	    val etv = E.plusproj env tn
	    fun conscs ty l = (l, map (fn x => E.genCstTyEm (T.consV tv) ty l) [ty])
	in foldl (fn ({ty, chk, asmp, lab}, y) => E.conscstSems (conscs ty lab) y) cs etv
	end) E.emcst tns*)

(*fun checkAppNotApp idenv cs =
    let
	fun findCsc _ [] = []
	  | findCsc v ((E.CSC (ll, SOME v')) :: xs) =
	    if v = v'
	    then ll :: (findCsc v xs)
	    else findCsc v xs
	  | findCsc v (x :: xs) = findCsc v xs
	fun f v {ty, chk = CL.PAT _, asmp, lab} =
	    let
		val ll = findCsc v cs
	    in map (fn x => E.CSA (L.cons lab x, NONE)) ll
	    end
	  | f _ _ = raise EH.DeadBranch "" (* we use the function only with T.getPat *)
	fun ff ((x, semty), y) = (foldr (fn (u, v) => (f x u) @ v) [] semty) @ y
    in foldr ff [] idenv
    end*)

(* the previous checkAppNotApp function (above) was using the constraints CSC to do the job
 * - now, we're using the labels stored in CON *)
fun checkAppNotApp' (env as E.ENVCON _) fenv =
    let val idenv1 = E.getPATenv (E.getVars env)
	val idenv2 = fenv env
	fun findApp ({id, scope, bind, class, lab, poly}, _, _, _) =
	    if CL.classIsCON class (* exception are not applied, they can come from stuff like: exception x = y (because of y then) *)
	    then let val ll1 = CL.getClassCON class
		     val ll2 = getlabExt (E.plusproj idenv1 id)
		 in map (fn x => E.CSA (L.cons lab (L.cons x ll1))) ll2
		 end
	    else []
	fun ff (semty, y) = (foldr (fn (u, v) => (findApp u) @ v) [] semty) @ y
    in E.foldrenv ff [] idenv2
    end
  | checkAppNotApp' _ _ = []

fun checkAppNotApp env = checkAppNotApp' env (fn env => E.uenv [E.getCons env, E.getVars env])

(* idenv1 are the binders, idenv2 is the environment *)
fun checkExceptionIsVar idenv1 idenv2 =
    let fun checkExc ({id, scope, bind, class, lab, poly}, _, _, _) =
	    if CL.classIsVAL class
	    then let val labs = C.getlabExtChkExc (E.stripExtLab (E.plusproj (E.getEXCenv idenv2) id))
		 in map (fn x => E.CSH (L.cons lab x, SOME (E.ID (id, lab, E.I)))) labs
		 end
	    else if CL.classIsCON class
	    then let val labs = C.getlabExtChkExc (E.stripExtLab (E.plusproj (E.getEXCenv idenv2) id))
		 in map (fn x => E.CSJ (L.cons lab x)) labs
		 end
	    else if CL.classIsEXC class
	    then []
	    else raise EH.DeadBranch "wrong identifier class"
	fun ff (semty, y) = (foldr (fn (u, v) => (checkExc u) @ v) [] semty) @ y
    in E.foldrenv ff E.emcss idenv1
    end

fun checkExceptionIsVarV (env1 as E.ENVCON _) (env2 as E.ENVCON _) =
    checkExceptionIsVar (E.getVids env1) (E.getCons env2)
  | checkExceptionIsVarV _ _ = E.emcss

fun checkExceptionIsVarC (env1 as E.ENVCON _) (env2 as E.ENVCON _) =
    checkExceptionIsVar (E.getCons env1) (E.uenv [E.getCons env2, E.getVars env2])
  | checkExceptionIsVarC _ _ = E.emcss*)


fun checkTyVarInc tyvarsbind labs tyvars =
    let val (labs, ids) =
	    foldl (fn (A.TypeVar (_, id, _, lab, _), (labs, ids)) =>
		      (L.cons lab labs, I.add id ids)
		    | (A.TypeVarDots, x) => x)
		  (labs, I.empty)
		  tyvarsbind
	fun f [] = E.emcss
	  | f ((A.TypeVar (_, id, _, lab, _)) :: xs) =
	    if I.isin id ids
	    then f xs
	    else E.conscss (E.CSSINCL (L.cons lab labs)) (f xs)
	  | f (A.TypeVarDots :: xs) = f xs
    in f tyvars
    end

(*(*(* Why do we need that for? - see test78 *)
(* This is not used anymore *)
fun preTreatAppCons etvset etv cs =
    let
	fun ff (e, y) =
	    let
		val (ty, cs', _) = fresh (C.getBindT etv, cs, E.emenv) freshty
		val lab = C.getBindL e
		val c   = E.genCstTyEm ty (T.consTyNameVar lab) lab
	    in E.conscstSem (lab, c) (E.uenvcsSem cs' y)
	    end
    in foldr ff E.emcst etvset
    end*)

(* condititions rule43 *)
fun preTreatPatAs (env1 as E.ENVCON _) (env2 as E.ENVCON _) =
    let val idenv = E.getPATbenv (E.getVars env2)
    in E.foldrenv
	   (fn (etv, z) =>
	       foldr (fn (({id, scope, bind, class, lab = l, poly}, _, _, _), y) => (* HACK: before we were using ll and not we use l *)
			 if CL.classIsCON class orelse CL.classIsEXC class
			 then (map (fn ({id, scope, bind, class, lab, poly}, _, _, _) =>
				       if CL.classIsPAT class
				       then E.CSP (L.cons lab (L.cons l (CL.getClassPATval class)))
				       else raise EH.DeadBranch "wrong identifier class")
				   (E.plusproj idenv id))
			      @ y
			 else raise EH.DeadBranch "wrong identifier class") z etv)
	   E.emcss
	   (E.getCons env1)
    end
  | preTreatPatAs _ _ = E.emcss



(* xs is environment for the constructor
 * idenv is the environment for the ids in patterns *)
fun preTreatNonAppPat (env1 as E.ENVCON _) (env2 as E.ENVCON _) =
    let val idenv = E.getVars env2 (*(2010-04-20)These are the saved environments in case they are constructors.*)
    in E.foldrenv
	   (fn (semty, z) =>
	       foldr (fn (({id, scope, bind, class, lab, poly}, _, _, _), cst) =>
			 if CL.classIsCON class orelse CL.classIsEXC class
			 then foldr (fn (etv, cst) => (* we should labs is in etv, class is: PAT (_, labs) *)
					let val tv = T.freshtyvar ()
					    val l  = E.getBindL etv
					    val c1 = E.genCstTyEm (E.getBindT etv) (T.consV tv) lab
					    val c2 = E.genCstTyEm (T.consV tv) (T.consTyNameVar l) l
					in E.conscstSem (lab, c1) (E.conscstSem (l, c2) cst)
					end)
				    cst
				    (E.plusproj (E.getPATenv idenv) id)
			 else raise EH.DeadBranch "wrong identifier class")
		     z
		     semty)
	   E.emcst
	   (E.getCons env1)
    end
  | preTreatNonAppPat _ _ = E.emcst

(*(* this is to mark an environment as depending on the structures
 * opened after in the code. *)
fun restrictOpen idO env =
    let
	fun restrictOpenIdEnv idenv =
	    E.foldrienv
		(fn (id, (semty, opnenv), idenv) =>
		    E.addenv (id, (semty, idO @ opnenv)) idenv)
		E.emid
		idenv
	fun restrictOpenStrEnv (E.ENVSEQ strenv) =
	    E.foldrienv
		(fn (id, (semty, opnenv), E.ENVSEQ strenv) =>
		    E.ENVSEQ (E.addenv (id, (semty, idO @ opnenv)) strenv))
		E.emstr
		strenv
	val idG = restrictOpenIdEnv (E.getVids env)
	(* we have to do something for E.getVars env
	 * so that it can only be bound to datatype constuctors *)
	val idX = restrictOpenIdEnv (E.getVars env)
	val idC = restrictOpenIdEnv (E.getCons env)
	val idT = restrictOpenIdEnv (E.getTyps env)
	val idS = restrictOpenStrEnv (E.getStrs env)
	val idV = E.getTyvs env
	val idO = E.getOpns env
	val idD = E.getOvcs env
    in E.ENVCON ((idG, idX, idC, idT, idV, idS, idO, idD),
	       E.getILab env, E.getITns env)
    end*)

(*fun bindOpen env =
    let
	fun bindOpenIdEnv idenv f =
	    E.foldrenv
		(fn ((semty, idO), cst) => (* here the opnenv should be empty*)
		    foldr
			(fn ((l, ev), cst) =>
			    foldr
				(fn (ety, cst) =>
				    E.conscstSem
					(C.getBindL ety,
					 E.genCstEvEm (f ety) (E.ENVVAR ev) (C.getBindL ety)) cst)
				cst
				semty)
			cst
			idO)
		E.emcst
		idenv
	fun bindOpenStrEnv (E.ENVSEQ strenv) =
	    E.foldrienv
		(fn (id, (semty, idO), cst) =>
		    foldr
			(fn ((l, ev), cst) =>
			    foldr
				(fn ((E.EENV (E.ENVVAR ev', lab)), cst) =>
				    E.conscstSem
					(lab,
					 E.genCstEvEm (E.INJSTR (E.STC (id, lab, E.ENVVAR ev', false))) (E.ENVVAR ev) lab) cst
				  | _ => raise EH.DeadBranch "")
				cst
				semty)
			cst
			idO)
		E.emcst
		strenv
	val csG = bindOpenIdEnv  (E.getVids env) E.INJVID
	(* we have to do something for E.getVars env
	 * so that it can only be bound to datatype constuctors *)
	val csC = bindOpenIdEnv  (E.getCons env) E.INJVID
	val csT = bindOpenIdEnv  (E.getTyps env) E.INJTYP
	val csS = bindOpenStrEnv (E.getStrs env)
    in E.uenvcsSems [csG, csC, csT, csS]
    end*)

fun genOpenVar evs lab =
    let val xs = map
		     (fn ev =>
			 let
			     val ev' = E.freshenvvar ()
			     val c   = E.genCstEvEm (E.ENVVAR ev) (E.ENVVAR ev') lab
			 in (ev', c)
			 end)
		     evs
	val (evs', cs) = ListPair.unzip xs
    in (evs', E.singcstSems (lab, cs))
    end

(* the second idV is a renaming of the range of the first idV *)
fun comptyvarenv [] idV = []
  | comptyvarenv ((tv, tvl) :: xs) idV =
    let fun compose [] = []
	  | compose (y :: ys) = (E.plusproj idV y) @ (compose ys)
    in (tv, tvl @ (compose tvl)) :: (comptyvarenv xs idV)
    end

fun tyvartoty   tvl = map (fn x => T.consV x) tvl
fun tytotyvar   tvl = map (fn (T.V (x, _, _)) => x | _ => raise EH.DeadBranch "") tvl
fun seqtoseqvar sql = map (fn (T.SV x)        => x | _ => raise EH.DeadBranch "") sql
fun envtoenvvar evl = map (fn (E.ENVVAR x)      => x | _ => raise EH.DeadBranch "") evl

fun compmono1 idenv1 idenv2 b = (* if b is false we know the status of idenv1 is not "c" *)
    let fun getLabs (ety, _, _, _) = (* with special care of REC*)
	    let val (l, labs) = C.getREClabs ety
	    in if L.isEmpty labs then L.singleton l else labs end
	fun g id tv lab ll =
	    let val sid = CD.sing id lab
		val semty = E.plusproj idenv2 id
		fun gg x = if b then sid else CD.empty
		fun ff l x = E.genCstTy tv (E.getBindT x) l (gg x)
	    in L.foldr (fn (l, cst) => E.uenvcsSem (E.singcstSems (l, map (ff l) semty)) cst) E.emcst ll
	    end
	fun f (id, semty, xs) =
	    let fun fsemty [etv]     = g id (E.getBindT etv) (E.getBindL etv) (getLabs etv)
		  | fsemty (y :: ys) = E.uenvcsSem (fsemty [y]) (fsemty ys)
		  | fsemty []        = raise EH.DeadBranch ""
	    in E.uenvcsSem (fsemty semty) xs
	    end
    in E.foldrienv f E.emcst idenv1
    end

(*fun getRecForce b = if b then SOME L.empty else NONE*)

(* 1 for pattern and 2 for expression.
 * b will be false for example for recursive functions inside their definition (monomorphic type)
 * and true for fn bindings.
 * labs is the label set responsible for the monomorphism of the binding.
 * bk is the kind of the binding (DEC, LET, FN). *)
fun compmono2 idenv1 idenv2 b cst1 cst2 labs bk =
    let val csenv =
	    E.foldrenv
		(fn (semty, csenv) =>
		    (List.mapPartial
			 (fn (x as {id, scope, bind, class, lab, poly}, _, _, _) =>
			     let val etvs = map E.getBindT (E.plusproj idenv2 id)
				 val poly = P.MONO (C.genMonREC x labs)
			     in case etvs of
				    [] => NONE
				  | _  => SOME (E.consBind id (tytotyvar etvs) bind class lab poly)
			     end)
			 semty)
		    @ csenv)
		[]
		idenv1
	val (csenv1, csenv2) = if b then (csenv, []) else ([], csenv)
	val vids = E.bindToEnv csenv1
	val vars = E.bindToEnv csenv2
	val env  = E.updateIBkd bk (E.updateVids vids (E.projVars vars))
    in if List.null csenv
       then E.uenvcsSem cst1 cst2
       (* else E.singcstSem (L.dummylab, E.CSTGEN (csbind, cst1, cst2))*)
       else E.singcstSem (L.dummylab, E.CSTGEN (env, E.emcst, E.uenvcsSem cst1 cst2))
    end

(* 1 for pattern and 2 for expression *)
fun compmono3 idenv _ b cst1 cst2 labs bk =
    let val csenv =
	    E.foldrenv
		(fn (semty, csenv) =>
		    (map (fn (x as {id, scope, bind as T.V (tv, _, _), class, lab, poly}, _, _, _) => (* st can be derived from class *)
			     let val poly  = P.MONO (C.genMonREC x labs)
				 val class = if CL.classIsREC class orelse CL.classIsVAL class
					     then class
					     else raise EH.DeadBranch "not a valid class for a monomorphic binding"
			     in E.consBindSc id bind class lab poly
			     end
			   | _ => raise EH.DeadBranch "")
			 semty)
		    @ csenv)
		[]
		idenv
	val env = E.updateIBkd bk (E.projVids (E.bindToEnv csenv))
    in if List.null csenv
       then E.uenvcsSem cst1 cst2
       else E.singcstSem (L.dummylab, E.CSTGEN (env, cst1, cst2))
    (* We need to know inside env that the binding is monomorphic.
     * We can also used the csbs but then we need some status information. *)
    end

(* the first one is the pattern and the other one is the expression *)
fun compmono idenv1 idenv2 b cst1 cst2 labs bk =
    case SO.getSol () of
	SO.SOL1 => E.uenvcsSems [compmono1 idenv1 idenv2 b, cst1, cst2]
      | SO.SOL2 => E.uenvcsSems [compmono1 idenv1 idenv2 b, cst1, cst2]
      | SO.SOL3 => E.uenvcsSems [compmono1 idenv1 idenv2 b, cst1, cst2]
      | SO.SOL4 => E.uenvcsSems [compmono1 idenv1 idenv2 b, cst1, cst2]
      | SO.SOL5 => E.uenvcsSems [compmono1 idenv1 idenv2 b, cst1, cst2]
      | SO.SOL6 => compmono2 idenv1 idenv2 b cst1 cst2 labs bk
      | SO.SOL7 => compmono2 idenv1 idenv2 b cst1 cst2 labs bk
      | SO.SOL8 => compmono3 idenv1 idenv2 b cst1 cst2 labs bk

(*TODO: to write*)
(*fun componestr [] _ _ _ = (E.emcst, E.emenv)
  (*| componestr ((E.STC (stc, asmp, l)) :: es) env cs (extenv as (ev, _, lab)) =
    let
	val (cs2, env2) = componestr es env cs extenv
	val (ev, cs1, env1) = fresh (ev, cs, env)
	val ev' = E.freshenvvar ()
	val c1  = E.CSTEV (E.INS stc, E.ENVVAR ev')
	val c2  = E.CSTEV (E.ENVVAR ev', E.ENVVAR ev)
    in (E.conscstSem (l, c1) (E.conscstSem (lab, c2) (E.uenvcsSem  cs1 cs2)), E.uenvEnv [env1, env2])
    end
  | componestr ((E.LID (lid, asmp, l)) :: es) env cs (extenv as (ev, _, lab)) =
    let
	val (cs2, env2) = componestr es env cs extenv
	val (ev, cs1, env1) = fresh (ev, cs, env)
	val ev' = E.freshenvvar ()
	val c1  = E.CSTEV (E.INL lid, E.ENVVAR ev')
	val c2  = E.CSTEV (E.ENVVAR ev', E.ENVVAR ev)
    in (E.conscstSem (l, c1) (E.conscstSem (lab, c2) (E.uenvcsSem cs1 cs2)), E.uenvEnv [env1, env2])
    end*)
  | componestr (extenv :: es) env cs extenv' =
    let
	val (cs2, env2) = componestr es env cs extenv
	val (env, cs1, env1) = fresh (E.getextenvV extenv', cs, env) freshenv
	val ev' = E.freshenvvar ()
	val l1  = E.getextenvL extenv
	val l2  = E.getextenvL extenv'
	val c1  = E.genCstEvEm (E.getextenvV extenv) (E.ENVVAR ev') l1
	val c2  = E.genCstEvEm (E.ENVVAR ev') env l2
    in (E.conscstSem (l1, c1) (E.conscstSem (l2, c2) (E.uenvcsSem cs1 cs2)), E.uenvEnv [env1, env2])
    end*)

(*fun getlabsExpans (X.Expexp ll)                      = (ll, NONE)
  | getlabsExpans (X.Expdep (([], _), a, l, ll, rf)) = (L.cons l ll, SOME (a, rf))
  | getlabsExpans (X.Expdep (s, a, l, ll, rf))       = raise EH.TODO*)

(* This is not used in the latest version of the slicer *)
fun treatExpans expans tv' ty lab =
    case expans of
	(X.Expdep (I.ID (id, l), _, ll, _)) =>
	(fn (cst, ty) => E.conscstSem (lab, E.genCstTyEm (T.consV tv') ty lab) cst)
	    (L.foldr
		 (fn (l', (cst, ty)) =>
		     let val tv'' = T.freshtyvar ()
			 val cds = if l = l' then CD.sing id l else CD.empty
		     in (E.conscstSem (l', E.genCstTy (T.consV tv'') ty l' cds) cst, T.consV tv'')
		     end) (* we can also put "a" only on the cs labelled by "l" *)
		 (E.emcst, ty)
		 (L.cons l ll))
      | (X.Expdep _) => E.emcst
      | (X.Expexp ll) =>
	(fn (cst, ty) => E.conscstSem (lab, E.genCstTyEm (T.consV tv') ty lab) cst)
	    (L.foldr
		 (fn (l, (cst, ty)) =>
		     let val tv'' = T.freshtyvar ()
		     in (E.conscstSem (l, E.genCstTyEm (T.consV tv'') ty l) cst, T.consV tv'')
		     end)
		 (E.emcst, ty)
		 ll)

(* This is not used in the latest version of the slicer *)
fun treatNonexp nonexp stv ty lab =
    case (nonexp, stv) of
	(X.Expans expans, SOME tv') =>
	foldr
	    (fn (oneexp, cst) => E.uenvcsSem (treatExpans oneexp tv' ty lab) cst)
	    E.emcst
	    expans
      | (X.Nonexp, NONE) => E.emcst
      | _ => raise EH.DeadBranch ""

(* This is not used in the latest version of the slicer *)
fun compone [] _ _ _ _ _ _ = (E.emcst, E.emenv)
  | compone (e :: es) env cs asmp etv nonexp stv =
    let val (ty, cs1, env1) = fresh (E.getBindT etv, cs, env) freshty
	val rl  = asmp
	val lab = E.getBindL etv
	val c   = E.genCstTy ty (E.getBindT e) lab rl
	val cs' = treatNonexp nonexp stv ty (E.getBindL e)
	val (cs2, env2) = compone es env cs asmp etv nonexp stv
	(* cs' is the only set of constraints with path - coming from the expansive restriction *)
    in (E.conscstSem (lab, c) (E.uenvcsSem cs' (E.uenvcsSem (harpoonincst cs1 rl) cs2)),
	E.uenvEnv [harpooninenv env1 rl, env2])
    (*uenv ((map (fn (x, y) => (x, [y])) asc) @ idV2))*)
    (* HACK: replaced this line by idV2 *)
    end

(* this is just a simple version of compone where:
   - the polymorphism is forced and
   - no assumption is propagated*)
fun componesp etvset env cs etv = compone etvset env cs CD.empty etv X.Nonexp NONE

fun getOptNonExp nonexp =
    case nonexp of (X.Expans l) => ((*D.printdebug2 ((A.printnonexp nonexp) ^ "\n");*)
				    SOME (T.freshtyvar ()))
		 | X.Nonexp     => NONE

fun componeg etvset env cs asmp etv nonexp = compone etvset env cs asmp etv nonexp (getOptNonExp nonexp)

fun compG [etv as ({id, scope, bind, class, lab, poly}, _, _, _)] v env1 env2 cs2 = (* we can only only have VAL *)
    if CL.classIsVAL class
    then componeg (E.plusproj (E.getVALenv (E.getVids env1)) v)
		  env2
		  cs2
		  (CD.sing v lab)
		  etv
		  (P.fromPolyToNonexp poly)
    else raise EH.DeadBranch "wrong identifier class"
  | compG (z :: y :: ys) v env1 env2 cs2 =
    let val (cs, env) = compG [z] v env1 env2 cs2
	val (cs', env') = compG (y :: ys) v env1 env2 cs2
    in (E.uenvcsSem cs cs', E.uenvEnv [env, env'])
    end
  | compG [] _ _ _ _ = raise EH.DeadBranch ""

fun compR [etv as ({id, scope, bind, class, lab, poly}, _, _, _)] v env1 env2 cs2 = (* we can only only have REC *)
    if CL.classIsREC class
    then componesp (E.plusproj (E.getVALenv (E.getVids env1)) v) env2 cs2 etv
    else raise EH.DeadBranch "wrong identifier class"
  | compR (z :: y :: ys) v  env1 env2 cs2 =
    let val (cs, env) = compR [z] v env1 env2 cs2
	val (cs', env') = compR (y :: ys) v env1 env2 cs2
    in (E.uenvcsSem cs cs', E.uenvEnv [env, env'])
    end
  | compR [] _ _ _ _ = raise EH.DeadBranch ""

fun compC [etv as ({id, scope, bind, class, lab, poly}, _, _, _)] v env1 env2 cs2 = (* we can only have CON *)
    if CL.classIsCON class
    then componesp (E.plusproj (E.uenv [E.getVids env1, E.getVars env1, E.getCons env1]) v)
		   env2
		   cs2
		   etv
    else raise EH.DeadBranch "wrong identifier class"
  | compC (y :: z :: ys) v env1 env2 cs2 =
    let val (cs, env) = compC [y] v env1 env2 cs2
	val (cs', env') = compC (z :: ys) v env1 env2 cs2
    in (E.uenvcsSem cs cs', E.uenvEnv [env, env'])
    end
  | compC [] _ _ _ _ = raise EH.DeadBranch ""

fun compT [etv as ({id, scope, bind, class, lab, poly}, _, _, _)] tn env1 env2 cs2 = (* we have DAT or NON *)
    if CL.classIsDAT class
    then componesp (E.plusproj (E.getTyps env1) tn) env2 cs2 etv
    else raise EH.DeadBranch "wrong identifier class"
  | compT (y :: z :: ys) tn env1 env2 cs2 =
    let val (cs, env) = compT [y] tn env1 env2 cs2
	val (cs', env') = compT (z :: ys) tn env1 env2 cs2
    in (E.uenvcsSem cs cs', E.uenvEnv [env, env'])
    end
  | compT [] _ _ _ _ = raise EH.DeadBranch ""

fun compS [(*E.STV*) extenv] id env1 env2 cs2 = raise EH.TODO
  (*componestr (E.plusproj (E.getSS env1) id) env2 cs2 extenv*)
  | compS (y :: z :: ys) id env1 env2 cs2 =
    let val (cs, env) = compS [y] id env1 env2 cs2
	val (cs', env') = compS (z :: ys) id env1 env2 cs2
    in (E.uenvcsSem cs cs', E.uenvEnv [env, env'])
    end
  | compS _ _ _ _ _ = raise EH.DeadBranch ""

fun compS' (x, _) y z = compS x y z

fun compGen idenv fcomp env1 env2 cs2 =
    E.foldrienv (fn (id, x, (cs, env)) =>
		    let val (cs', env') = fcomp x id env1 env2 cs2
		    in (E.uenvcsSem cs cs', E.uenvEnv [env, env'])
		    end) (E.emcst, E.emenv) idenv


(*
(* TODO: CL.VAL does not gather info about nonexp!!! *)
fun uniqidenv idenv =
    E.foldrienv
	(fn (_, [], _) => raise EH.DeadBranch ""
	  (*| (id, [etv], (cst, idenv)) => (cst, E.addenv (id, [etv]) idenv)*)
	  | (id, semty, (cst, idenv)) =>
	    let
		val tv = T.freshtyvar ()
		val (cst', newlabs) =
		    foldr
			(fn ({ty, class, asmp, lab, poly}, (y, z)) =>
			    (E.conscstSem (lab, E.CSTTY (T.consV tv, T.consV ty, L.empty)) y,
			    L.concat asmp z))
			(E.emcst, L.empty)
			semty
		val class = (case C.getBindC (List.hd semty) of
			       (CL.VAL _) => CL.VAL (getlabExtChkVal semty)
			     | (CL.CON _) => CL.CON (getlabExtChkCon semty)
			     | (CL.REC _) => CL.REC (getlabExtChkRec semty)
			     | (CL.NON _) => CL.NON (getlabExtChkNon semty)
			     | (CL.DAT _) => CL.DAT (getlabExtChkDat semty)
			     | x => x)
		    handle Empty => raise EH.DeadBranch ""
	    in (E.uenvcsSem cst' cst, E.addenv (id, [C.consextty tv class newlabs L.dummylab]) idenv)
	    end)
	(E.emcst, E.emid)
	idenv
*)

(*fun uniqidenv' idenv = (E.emcst, idenv)*)

(*
fun uniqpattern (idG, idX, idC, idT, idV, idS) cs =
    let
	val (cstG, idG') = uniqidenv idG
	val (cstX, idX') = uniqidenv idX
	val (cstC, idC') = uniqidenv idC
	val (cstT, idT') = uniqidenv idT
    in (E.concatcsSem (E.uenvcsSems [cstG, cstX, cstC, cstT]) cs, (idG', idX', idC', idT', idV, idS))
    (* idV should be empty *)
    (* and for now we don't care about idS*)
    end
*)

(*(* to extend the csenvs with the corresponding label in the pattern *)
fun csenvLab csenv =
    foldr
	(fn ((id, tv1, tv2, poly, lab), (csenv, cst)) =>
	    let
		val tv   = T.freshtyvar ()
		val c    = E.genCstTyEm (T.consV tv) (T.consV tv2) lab
		val cst' = E.conscstSem (lab, c) cst
	    in ((id, tv1, tv, poly, lab) :: csenv, cst')
	    end)
	([], E.emcst)
	csenv*)

fun compcstgen1 env1 cs1 env2 cs2 env3 _ =
    let val (csG, envG) = compGen (E.getVids env3) compG env1 env2 (E.getcsSem cs2)
	(* if the constructor comes from a OF, we can't move a variable from idG or idX to id!
           - or something like that - see test7.sml*)
	val (csX, envX) = compGen (E.getVars env3) compR env1 env2 (E.getcsSem cs2)
	val (csC, envC) = compGen (E.getCONenv (E.getCons env3)) compC env1 env2 (E.getcsSem cs2)
	val (csT, envT) = compGen (E.getTyps env3) compT env1 env2 (E.getcsSem cs2)
	val (csS, envS) = compGen (E.getStrs env3) compS env1 env2 (E.getcsSem cs2)
	val cs  = restrictRecCs (harpoonoutcs cs1 (E.dom (E.getCons env3))) (E.getVars env3)
	val csE = compmono1 (E.getEXCenv (E.getCons env3)) (E.uenv [E.getVids env1, E.getVars env1, E.getCons env1]) false
	val cst = E.uenvcsSems [csG, csX, csC, csT, csS, csE, E.getcsSem cs2]
	val css = E.getcsSyn cs2
    in (E.concatcsSyn css (E.concatcsSem cst cs),
	E.uenvEnv [envG, envX, envC, envT, envS])
    end

fun compcstgen2 env1 cs1 _ cs2 env3 lab bk = (* 1: expression, 2/3: pattern*)
    let (* TODO: we should also link to (E.getCons env1) and generate status errors for those *)
	(* but then E.getCons env1 has to be put into the X part *)
	fun getTo genenv id = map E.getBindT (E.plusproj genenv id)
	val csenv1 = E.foldrenv
			 (fn (semty, csenv) =>
			     (List.mapPartial
				  (fn ({id, scope, bind, class, lab, poly}, _, _, _) =>
				      if CL.classIsVAL class
				      then let val tves = getTo (E.uenv [E.getVids env1, E.getCons env1]) id
					   in case tves of
						  [] => NONE
						| _  => SOME (E.consBind id (tytotyvar tves) bind class lab poly)
					   end
				      else raise EH.DeadBranch "")
				  semty)
			     @ csenv)
			 []
			 (E.getVids env3)
	val csenv2R = E.foldrenv
			  (fn (semty, csenv) =>
			      (List.mapPartial
				   (fn ({id, scope, bind, class, lab, poly}, _, _, _) =>
				       if CL.classIsREC class
				       then let val tves = getTo (E.getVALenv (E.getVids env1)) id
					    in case tves of
						   [] => NONE
						 | _  => SOME (E.consBind id (tytotyvar tves) bind class lab poly)
					    end
				       else raise EH.DeadBranch "")
				   semty)
			      @ csenv)
			  []
			  (E.getVars env3)
	val csenv2C = E.foldrenv
			  (fn (semty, csenv) =>
			      (List.mapPartial
				   (fn ({id, scope, bind, class, lab, poly}, _, _, _) =>
				       if CL.classIsCON class
				       then let val idenvs = E.uenv [E.getVids env1, E.getVars env1, E.getCons env1]
						val tves = getTo idenvs id
					    in case tves of
						   [] => NONE
					    | _  => SOME (E.consBind id (tytotyvar tves) bind class lab poly)
					    end
				       else raise EH.DeadBranch "")
				   semty)
			      @ csenv)
			  []
			  (E.getCONenv (E.getCons env3))
	val csenv2E = E.foldrenv
			  (fn (semty, csenv) =>
			      (List.mapPartial
				   (fn ({id, scope, bind, class, lab, poly}, _, _, _) =>
				       if CL.classIsEXC class
				       then let val idenvs = E.uenv [E.getVids env1, E.getVars env1, E.getCons env1]
						val tves = getTo idenvs id
					    in case tves of
						   [] => NONE
						 | _  => SOME (E.consBind id (tytotyvar tves) bind class lab poly)
					    end
				       else raise EH.DeadBranch "")
				   semty)
			      @ csenv)
			  []
			  (E.getEXCenv (E.getCons env3))
	val tcsenv = E.foldrenv
			 (fn (semty, csenv) =>
			     (List.mapPartial
				  (fn ({id, scope, bind, class, lab, poly}, _, _, _) =>
				      if CL.classIsDAT class
				      then let val tves = getTo (E.getTyps env1) id
					   in case tves of
						  [] => NONE
						| _  => SOME (E.consBind id (tytotyvar tves) bind class lab poly)
					   end
				      else raise EH.DeadBranch "")
				  semty)
			     @ csenv)
			 []
			 (E.getTyps env3)
	val strcsenv = E.foldrienv
			   (fn (id, semty, strcsenv) =>
			       (List.mapPartial
				    (fn ({id, scope, bind, class, lab, poly}, _, _, _) =>
					let val exvs = getTo (E.getStrs env1) id
					in case exvs of
					       [] => NONE
					     | _  => SOME (E.consBind id (envtoenvvar exvs) bind class lab poly)
					end)
				    semty)
			       @ strcsenv)
			   []
			   (E.getStrs env3)
	val sigcsenv = E.foldrienv
			   (fn (id, sigsem, sigcsenv) =>
			       (List.mapPartial
				    (fn ({id, scope, bind, class, lab, poly}, _, _, _) =>
					let val exvs = getTo (E.getSigs env1) id
					in case exvs of
					       [] => NONE
					     | _  => SOME (E.consBind id (envtoenvvar exvs) bind class lab poly)
					end)
				    sigsem)
			       @ sigcsenv)
			   []
			   (E.getSigs env3)
	val clenv = E.foldrenv
			(fn (semty, clenv) =>
			    (List.mapPartial
				 (fn ({id, scope, bind, class, lab, poly}, _, _, _) =>
				     let val xs = getTo (E.getOvcs env1) id
				     in case xs of
					    [] => NONE
					  | _  => SOME (E.consBind id (seqtoseqvar xs) bind class lab poly)
				     end)
				 semty)
			    @ clenv)
			[]
			(E.getOvcs env3)
	val cs  = restrictRecCs (harpoonoutcs cs1 (E.dom (E.getCons env3))) (E.getVars env3)
	(* TODO: we need to do that for the environments in constraints as well!
	 * And do the harpoonout and restrictrec on these envs as well.
	 * harpoonoutenv shouldn't do anything though (the harpoonoutcs does something in the csbs). *)
	val css     = E.uenvcsSyn (E.getcsSyn cs2) (E.getcsSyn cs)
	val env     = E.consEnvC (E.bindToEnv csenv1)
				 (E.bindToEnv csenv2R)
				 (E.bindToEnv (csenv2C @ csenv2E))
				 (E.bindToEnv tcsenv)
				 E.emtv
				 (E.bindToEnv strcsenv)
				 (E.bindToEnv sigcsenv)
				 E.emfun
				 (E.bindToEnv clenv)
				 E.emnfo
	val env'    = E.updateIBkd bk env
	val cst     = E.singcstSem (lab, E.CSTGEN (env', E.getcsSem cs2, E.getcsSem cs))
    in (E.conscsss css cst, E.emenv)
    end

fun compcstgen3 env1 cs1 _ cs2 env3 lab bk = (* 1: expression, 2/3: pattern*)
    let val css = E.uenvcsSyn (E.getcsSyn cs2) (E.getcsSyn cs1)
	val cst = if E.isEmptyEnv env3
		  then E.uenvcsSem (E.getcsSem cs2) (E.getcsSem cs1)
		  else E.singcstSem (lab, E.CSTGEN (E.updateIBkd bk env3, E.getcsSem cs2, E.getcsSem cs1))
    in (E.conscsss css cst, E.emenv)
    end

(*fun compcstgen3 env1 cs1 _ cs2 env3 lab bk = (* 1: expression, 2/3: pattern*)
    let val csenv1 = E.foldrenv
			 (fn (semty, csenv) =>
			     (map (fn x => if CL.classIsVAL (C.getBindC x)
					   then x
					   else raise EH.DeadBranch "")
				  semty)
			     @ csenv)
			 []
			 (E.getVids env3)
	val csenv2R = E.foldrenv
			  (fn (semty, csenv) =>
			      (map (fn x => if CL.classIsREC (C.getBindC x)
					    then x
					    else raise EH.DeadBranch "")
				   semty)
			      @ csenv)
			  []
			  (E.getVars env3)
	val csenv2C = E.foldrenv
			  (fn (semty, csenv) =>
			      (map (fn x => if CL.classIsCON (C.getBindC x)
					    then x
					    else raise EH.DeadBranch "")
				   semty)
			      @ csenv)
			  []
			  (E.getCONenv (E.getCons env3))
	val csenv2E = E.foldrenv
			  (fn (semty, csenv) =>
			      (map (fn x => if CL.classIsEXC (C.getBindC x)
					    then x
					    else raise EH.DeadBranch "")
				   semty)
			      @ csenv)
			  []
			  (E.getEXCenv (E.getCons env3))
	val tcsenv = E.foldrenv
			 (fn (semty, csenv) =>
			     (map (fn x => if CL.classIsDAT (C.getBindC x)
					   then x
					   else raise EH.DeadBranch "")
				  semty)
			     @ csenv)
			 []
			 (E.getTyps env3)
	val strcsenv = E.foldrenv
			   (fn (semty, strcsenv) =>
			       (map (fn x => if CL.classIsSTR (E.getextenvC x)
					     then x
					     else raise EH.DeadBranch "")
				    semty)
			       @ strcsenv)
			   []
			   (E.getStrs env3)
	val sigcsenv = E.foldrenv
			   (fn (sigsem, sigcsenv) =>
			       (map (fn x => if CL.classIsSIG (E.getextenvC x)
					     then x
					     else raise EH.DeadBranch "")
				    sigsem)
			       @ sigcsenv)
			   []
			   (E.getSigs env3)
	val clenv = E.foldrenv
			(fn (semty, clenv) =>
			    (map (fn x => if CL.classIsOC (C.getBindC x)
					  then x
					  else raise EH.DeadBranch "class should be overloading class")
				 semty)
			    @ clenv)
			[]
			(E.getOvcs env3)
	val opnbind = E.getOpns env3
	val csbind = (csenv1 @ csenv2R @ csenv2C @ csenv2E, [], tcsenv, strcsenv, sigcsenv, clenv, opnbind, bk)
	val css = E.uenvcsSyn (E.getcsSyn cs2) (E.getcsSyn cs1)
	val cst = if E.isEmptyCsBind csbind
		  then E.uenvcsSem (E.getcsSem cs2) (E.getcsSem cs1)
		  else E.singcstSem (lab, E.CSTGEN (csbind, E.getcsSem cs2, E.getcsSem cs1))
    in (E.conscsss css cst, E.emenv)
    end*)

fun compcstgen env1 cs1 env2 cs2 env3 lab bk =
    case SO.getSol () of
	SO.SOL1 => compcstgen1 env1 cs1 env2 cs2 env3 lab
      | SO.SOL2 => compcstgen2 env1 cs1 env2 cs2 env3 lab bk
      | SO.SOL3 => compcstgen2 env1 cs1 env2 cs2 env3 lab bk
      | SO.SOL4 => compcstgen2 env1 cs1 env2 cs2 env3 lab bk
      | SO.SOL5 => compcstgen2 env1 cs1 env2 cs2 env3 lab bk
      | SO.SOL6 => compcstgen2 env1 cs1 env2 cs2 env3 lab bk
      | SO.SOL7 => compcstgen2 env1 cs1 env2 cs2 env3 lab bk
      | SO.SOL8 => compcstgen3 env1 cs1 env2 cs2 env3 lab bk

fun removeBoundIds (env1 as E.ENVCON _) (env2 as E.ENVCON _) =
    E.consEnvC (E.outenv (E.getVids env2) (E.doms [E.getVids env1, E.getVars env1, E.getCons env1]))
	       (uenv [E.outenv (E.getVars env2) (E.doms [E.getCons env1, E.getVars env1]),
		      E.inenv (E.getVids env2) (E.dom (E.getVids env1)),
		      E.inenv (E.getCons env2) (E.dom (E.getVids env1))])
	       (E.outenv (E.getCons env2) (E.doms [E.getCons env1, E.getVars env1, E.getVids env1]))
	       (E.outenv (E.getTyps env2) (E.dom (E.getTyps env1)))
	       (E.getTyvs env2)
	       (E.outenv (E.getStrs env2) (E.dom (E.getStrs env1)))
	       (E.outenv (E.getSigs env2) (E.dom (E.getSigs env1)))
	       (E.getFuns env2)
	       (E.outenv (E.getOvcs env2) (E.dom (E.getOvcs env1)))
	       (E.getInfo env2)
  | removeBoundIds env1 env2 = env2

(*(2010-04-20)With SOL8 we should return E.emenv.
 * We should also disable E.toPAT so that it returns empty envs.*)

(*(2010-04-16) env3 is an outter environment with env1 in its scope and
 * env2 is the inner environment of env3.*)
fun completbas env1 cs1 env2 cs2 env3 lab bk =
    let val (csG, envG) = compcstgen env1 cs1 env2 cs2 env3 lab bk (* HACK: change compcstgen into compcstgen' for the new version! *)
	val csA = checkAppNotApp            env1
	val csK = checkArityTyCon      env3 env1
	val csP = preTreatPatAs        env3 env1
	val csN = preTreatNonAppPat    env3 env1
	val csE = checkExceptionIsVarV env3 env1
	val csF = checkExceptionIsVarC env3 env1
	(* TODO: use csE then.
	 * The problem is that for now we can't reject a context dependency
	 * that says, an identifier has to be an exception
	 * If a datatype constructor rebinds the exception then we have the same problem
	 * we can put a status in ocsb
	 * No! we, ve got to do the same testting for the CON in E.getCons env3.
	 * (The E.getCons env1 is then augmented with the E.getVars env1, and we keep getting the EXC in there) *)
	val env' = removeBoundIds env3 env1
	(*val (env2', cs2t') = (env2, E.getcsSem cs2)*) (* -- see test83 *) (*if E.emEnvListAnd [envX, envC, envT, envS] then (env2, E.getcsSem cs2) else (E.emenv, E.emcst)*)
	(*******************************)
	(*TODO: IMPORTANT: if one of envX or envC or envT is not empty then we don't need to add env2 because the other doesn't have any assumption *)
	(*******************************)
	(*val idV = uenv (comptyvarenv (uenv ((E.getTyvs env1) @ (E.getTyvs env2))) (uenv (idV1 @ idV2 @ idV3 @ idV4)))*)
	(*HACK: replaced the line above by the line below - TODO: check test72 for example*)
	(*val idV = uenv ((E.getTyvs env1) @ (E.getTyvs env2))*)
	(*val _ = print ((E.printStrEnv (E.getStrs env2)) ^ "2-+-+- ")*)
	(*val _ = print ((E.printStrEnv (E.getStrs envG)) ^ "G-+-+- ")*)
	(*val idS = uenv ((E.getStrs env1) @ (E.getStrs env2))*)
	val cst = E.uenvcsSems [csK, csN]
	val css = E.uenvcsSyns [csA, csP, csE, csF]
	val cs  = E.nestcsSem cst (E.concatcsSyn css csG) (*HACK: replaced concatcsSem by nestcsSem*)
	(* restrictRecCs takes time *)
	(* IMPORTANT: never remove cs2 above if the syntactic restrictions are not refreshed *)
	(* we don't need to refresh cs2 anymore *)
	val env = E.uenvEnv [env2, envG, restrictrecenv' env' env3]
    in (cs, env)
    end

fun complet env1 cs1 env2 cs2 env3 lab bk = completbas env1 cs1 env2 cs2 env3 lab bk

(*
fun consTyFunFeat tvl tv l =
    let
	fun f [] = raise EH.DeadBranch ""
	  | f [x] = (x, [])
	  | f (x :: xs) =
	    let
		val (tv, cs) = f xs
		val tv' = T.freshtyvar ()
		(*val c = E.CSTTY (T.consV tv', T.C (T.NC (T.consarrow (), l), T.SC ([x, tv], l)), (l, []))*)
		val c = E.CSTTY (T.consV tv', T.constyarrow x tv l, (l, L.empty))
	    in (tv', c :: cs)
	    end
    in f (tvl @ [tv])
    end
*)

(* we close the explicit type variables - see section 4.6 *)
fun closeTypedTerm1 tvs idV cs = raise EH.DeadBranch ""
(*    let
	fun sepcsb [] _ = ([], [])
	  | sepcsb ((x as (E.CSB (_, _, (tv, _)))) :: xs) tv' =
	    (if tv = tv' then fn (u, v) => (x :: u, v) else fn (u, v) => (u, x :: v)) (sepcsb xs tv')
	  | sepcsb (x :: xs) tv' = (fn (u, v) => (u, x :: v)) (sepcsb xs tv')
	(* we don't close if the type variable is annotated by "expansive" *)
	(* IDEA: - maybe at the end we can bind the variables that are involved into CSBs to constant types
                 - or we can generate such constraints here and annotate them by "tv" *)
	(* TODO: this is not finished, many constraint are missing and the slices look bad *)
	fun createren [] css = ([], [], E.emcst, css)
	  | createren ((A.TypeVar (_, tv, _, l, _), nonexp, lop) :: xs) css =
	    let
		(*val _ = D.printdebug2 ("--" ^ (T.printtyvar tv) ^ "\n")*)
		val (cssin, cssout) = sepcsb css tv
		val (asc, ren, cst, css') = createren xs cssout
		fun addlop l ll = case lop of NONE => (L.cons l ll) | SOME l' => L.cons l (L.cons l' ll)
		fun getcss epl =
		    List.concat
			(map
			     (fn X.Expexp ll =>
				 E.conscssSyn
				     (E.CSB (addlop l ll, NONE, (tv, l)))
				     (map (fn (E.CSB (_, _, (_, l))) =>
					      E.CSB (addlop l ll, NONE, (tv, l))
					    | _ => raise EH.DeadBranch "") cssin)
			       | X.Expdep (lid as (I.ID (id, labid)), _, ll, rf) =>
				 E.conscssSyn
				     (E.CSB (addlop l (L.cons labid ll), SOME lid, (tv, l)))
				     (map (fn (E.CSB (_, _, (_, l))) =>
					      E.CSB (addlop l (L.cons labid ll), SOME lid, (tv, l))
					    | _ => raise EH.DeadBranch "") cssin)
			       | X.Expdep _ => E.emcss)
			     epl)
		val css = case nonexp of X.Nonexp => css'
				       | X.Expans epl => E.uenvcsSyn (getcss epl) css'
	    in
		let
		    val (ll, asc') = lkup tv asc
		in ((tv, l :: ll) :: asc', ren, cst, css)
		end
		    handle lkupex =>
			   let
			       val c = E.genCstTyEm (T.consV tv) (T.constynewcons l) l
			   in ((tv, [l]) :: asc, (tv, T.freshtyvar ()) :: ren, E.conscstSem (l, c) cst, css)
			   end
	    end
	  | createren ((A.TypeVarDots, _, _) :: xs) css = createren xs css
	fun unclose [] = E.emcst
	  | unclose ((tv, ll) :: xs) =
	    E.uenvcsSem
		(foldr (fn (x, y) =>
			   E.uenvcsSem
			       (foldr
				    (fn (u, v) =>
					E.conscstSem (u, E.genCstTyEm (T.consV x) (T.consV tv) u) v)
				    E.emcst
				    ll) y)
		       E.emcst
		       (E.plusproj idV (I.fromIntToId tv)))
		(unclose xs)
	val (asc, ren, cs1, css) = createren tvs (E.getcsSyn cs)
	val idV' = foldr (fn ((x, y), z) => E.addenv (I.fromIntToId x, [y]) z) E.emvar ren
	val csu = E.uenvcsSem (E.getcsSem cs) (unclose asc)
	val cs2 = case ren of [] => ((*D.printdebug2 ("-+1+-\n");*)
				     E.emcst)
			    | _  => ((*D.printdebug2 ("-+2+-\n");*)
				     freshcst (E.uenvcsSem csu cs1) (initState ())) (* HACK: we refresh if necessary *)
    (* IMPORTANT: we do the freshning because inside the expression, before being bound,
       the type variables are considered as constant types *)
    in (E.uenv [idV, idV'], E.conscsss css (E.uenvcsSem (renamecst csu ren) cs2))
    (* before: E.plusenv idV idV' *)
    (* cs2 doesn't need to be replicated from now on *)
    (* (renamecs (cs @ (unclose asc)) ren) *)
    (* before: (renamecs cs (ren @ (close ren))) - we're not using close anymore *)
    (* IMPORTANT: ren contains the renaming for generalization *)
    end*)


(* generalisation *)


(* I think we don't need the array anymore because we don't use the variable in T.E anymore *)
(* we should use an association list instead!! *)
fun genState _ = Array.array (T.gettyvar (), NONE)
fun genUpdate x onestate =
    (case Array.sub (onestate, x) of
	 NONE   => let val y = T.freshtyvar ()
		   in (Array.update (onestate, x, SOME y); y) end
       | SOME y => y)

fun unclosety (T.V x) _ _ = T.V x
  | unclosety (T.E (n, tv, l)) tvl state =
    if L.isin tv tvl
    then T.E (n, genUpdate tv state, l)
    else T.E (n, tv, l)
  | unclosety (T.C (tn, sq, l)) tvl state =
    T.C (tn, uncloseseqty sq tvl state, l)
  | unclosety (T.App (sq, ty, l)) tvl state =
    let val sq' = uncloseseqty sq tvl state
	val ty' = unclosety    ty tvl  state
    in T.App (sq', ty', l) end
  | unclosety (T.Abs (sq, ty, l)) tvl state =
    let val sq' = uncloseseqty sq tvl state
	val ty' = unclosety    ty tvl state
    in T.Abs (sq', ty', l) end
  | unclosety (T.Or (sq, i, j, l)) tvl state =
    T.Or (uncloseseqty sq tvl state, i, j, l)
and uncloseseqty (T.SV sv) _ _ = T.SV sv
  | uncloseseqty (T.SC (rtl, flex, l)) tvl state =
    T.SC (map (fn rt => uncloserowty rt tvl state) rtl, flex, l)
and uncloserowty (T.RV rv) _ _ = T.RV rv
  | uncloserowty (T.RC (lt, ty, l)) tvl state =
    T.RC (lt, unclosety ty tvl state, l)
(*and unclosetylist xs tvl state =
    map (fn x => unclosety x tvl state) xs*)

fun uncloseTvsBind tvsbind tvl =
    List.mapPartial (fn (x as (tv, _)) => if L.isin tv tvl then NONE else SOME x) tvsbind

fun uncloseocst (E.CSTTY  ((x, y), l, s, z)) tvl state =
    E.CSTTY ((unclosety x tvl state, unclosety y tvl state), l, s, z)
  | uncloseocst (E.CSTTN  ((x, y), l, s, z)) _ _ =
    E.CSTTN ((x, y), l, s, z)
  | uncloseocst (E.CSTSQ  ((x, y), l, s, z)) tvl state =
    E.CSTSQ ((uncloseseqty x tvl state, uncloseseqty y tvl state), l, s, z)
  | uncloseocst (E.CSTRT  ((x, y), l, s, z)) tvl state =
    E.CSTRT ((uncloserowty x tvl state, uncloserowty y tvl state), l, s, z)
  | uncloseocst (E.CSTLT  ((x, y), l, s, z)) _ _ = E.CSTLT ((x, y), l, s, z)
  | uncloseocst (E.CSTEV  ((x, y), l, s, z)) _ _ = E.CSTEV ((x, y), l, s, z) (* TODO: for now we don't do anything for these *)
  | uncloseocst (E.CSTCL  ((x, y), l, s, z)) _ _ = E.CSTCL ((x, y), l, s, z)
  | uncloseocst (E.CSTIT  ((x, y), l, s, z)) _ _ = E.CSTIT ((x, y), l, s, z)
  | uncloseocst (E.CSTIO  ((x, y), l, s, z)) _ _ = E.CSTIO ((x, y), l, s, z)
  | uncloseocst (E.CSTIS  ((x, y), l, s, z)) _ _ = E.CSTIS ((x, y), l, s, z)
  | uncloseocst (E.CSTIF  ((x, y), l, s, z)) _ _ = E.CSTIF ((x, y), l, s, z)
  | uncloseocst (E.CSTEGN (x, cst))  _ _ = E.CSTEGN (x, cst)
  | uncloseocst (E.CSTGEN (x, cst1, cst2)) tvl state =
    E.CSTGEN (x, unclosecst cst1 tvl state, unclosecst cst2 tvl state)
  | uncloseocst (E.CSTVAL (x, y, z, cst)) tvl state =
    (fn tvl' => E.CSTVAL (uncloseTvsBind x tvl, y, z, unclosecst cst tvl' state))
	(L.difference (L.ord (#1 (ListPair.unzip y))) tvl)
  (*| uncloseocst (E.CSTBIND (x, cst1, cst2)) tvl state =
    E.CSTBIND (x, unclosecst cst1 tvl state, unclosecst cst2 tvl state)*)
and unclosecst cst tvl state =
    E.mapicst
	(fn (_, ocstl) => map (fn x => uncloseocst x tvl state) ocstl)
	cst

fun unclosecst' cst tvl = unclosecst cst tvl (genState ())

(*
fun uncloseocst (E.CSTTY  (x, y, idl)) _ = E.CSTTY  (x, y, idl)
  | uncloseocst (E.CSTTN  (x, y, idl)) _ = E.CSTTN  (x, y, idl)
  | uncloseocst (E.CSTSQ  (x, y, idl)) _ = E.CSTSQ  (x, y, idl)
  | uncloseocst (E.CSTRT  (x, y, idl)) _ = E.CSTRT  (x, y, idl)
  | uncloseocst (E.CSTLT  (x, y, idl)) _ = E.CSTLT  (x, y, idl)
  | uncloseocst (E.CSTEV (x, y, idl)) _ = E.CSTEV (x, y, idl)
  | uncloseocst (E.CSTGEN (x, y, z, cst1, cst2)) tvl =
    E.CSTGEN (x, y, z, unclosecst cst1 tvl, unclosecst cst2 tvl)
  | uncloseocst (E.CSTVAL (x, y, cst)) tvl =
    E.CSTVAL (L.toList (L.difference (L.ord tvl) (L.ord x)), y, unclosecst cst tvl)
and unclosecst cst tvl = E.mapicst (fn (_, ocstl) => map (fn x => uncloseocst x tvl) ocstl) cst
*)

(* tvs1 constraints the implicitly bound type variables *)
(* tvs2 constraints the explicitly bound type variables *)
fun closeTypedTerm2 tvs1 tvs2 idenv cs =
    let fun fromSeqToBind xs ys =
	    List.mapPartial
		(fn (A.TypeVar (_, tv, _, l, _), nonexp, lop) =>
		    if L.isin tv ys
		    then NONE
		    else SOME (tv, l)
		  | _ => NONE)
		xs
	val tvsbind2  = fromSeqToBind tvs2 L.empty
	val tvsbind2' = L.ord (#1 (ListPair.unzip tvsbind2))
	val tvsbind1  = fromSeqToBind tvs1 tvsbind2'
	val tvsbind1' = L.ord (#1 (ListPair.unzip tvsbind1))
	val tvsbind   = L.concat tvsbind1' tvsbind2'
	val tvl = E.foldrenv (fn (semty, tvl) => (E.gettys semty) @ tvl) [] idenv
	val css = E.getcsSyn cs
	val cst = if List.null tvsbind1 andalso List.null tvsbind2 andalso List.null tvl
		  then E.getcsSem cs
		  else E.singcstSem (L.dummylab,
				     E.CSTVAL (tvsbind1, tvsbind2, tvl,
					       unclosecst' (E.getcsSem cs) tvsbind))
    in E.conscsss css cst
    end

(*************)

fun closeTypedTerm tvs1 tvs2 idV cs idenv =
    case SO.getSol ()
     of	SO.SOL1 => closeTypedTerm1 tvs1 idV cs
      | SO.SOL2 => (idV, closeTypedTerm2 tvs1 tvs2 idenv cs)
      | SO.SOL3 => (idV, closeTypedTerm2 tvs1 tvs2 idenv cs)
      | SO.SOL4 => (idV, closeTypedTerm2 tvs1 tvs2 idenv cs)
      | SO.SOL5 => (idV, closeTypedTerm2 tvs1 tvs2 idenv cs)
      | SO.SOL6 => (idV, closeTypedTerm2 tvs1 tvs2 idenv cs)
      | SO.SOL7 => (idV, closeTypedTerm2 tvs1 tvs2 idenv cs)
      | SO.SOL8 => (idV, closeTypedTerm2 tvs1 tvs2 idenv cs)
(* For SOL8 we leave that for now but it will have to change. *)

fun compfvalbindcore idG lab =
    let fun syntrestrfval (semty, (ll, cs)) =
	    foldr (fn (({id, scope, bind, class, lab, poly}, _, _, _), (ll, cs)) =>
		      if CL.classIsREC class
		      then (ll, L.foldr (fn (x, cs) =>
					    L.foldr (fn (y, cs) => (E.CSF (L.ord [x, y])) :: cs)
						    cs
						    (CL.getClassREClabs class))
					cs
					ll)
		      else raise EH.DeadBranch "wrong identifier class")
		  (ll, cs)
		  semty
	fun eqcore (v, semty, (cs1, cs2, env)) =
	    let val tv = T.freshtyvar ()
		val (cs1', ll) =
		    foldr (fn (({id, scope, bind, class, lab = l, poly}, _, _, _), (cs, ll)) =>
			      if CL.classIsREC class
			      then (E.conscstSem (lab, E.genCstTyEm (T.consV tv) bind lab) cs,
				    L.concat (CL.getClassREClabs class) ll) (*HACK: before it was lab*)
			      else raise EH.DeadBranch "wrong identifier class")
			  (cs1, L.empty)
			  semty
		val cs2' = #2 (E.foldrenv syntrestrfval (ll, cs2) env)
		val newRec = CL.consREClabs ll
	    in (cs1', cs2', E.addenv (v, [E.consBindPoly v (T.consV tv) newRec lab]) env)
	    end
    in E.foldrienv eqcore (E.emcst, E.emcss, E.emvar) idG
    end*)

fun createDiffNbArgFun llops =
    let fun getpartsize list size =
	    List.mapPartial
		(fn x => if List.length x = size then SOME x else NONE)
		(foldr (fn (u, v) => (map (fn w => u :: w) v) @ v) [[]] list)
	fun selectenough1 ll1 ll2 =
	    let val l1 = List.length ll1
		val l2 = List.length ll2
		fun withll1 x = L.union x (L.ord ll1)
		fun withll2 x = L.union x (L.ord ll2)
	    in (if l1 > l2
		then if l1 = l2 + 1
		     then [withll2 (L.ord (List.take (ll1, l2)))]
		     else map (fn x => withll2 (L.ord x)) (getpartsize (List.take (ll1, l1-1)) l2)
		(* it is not just take, but take any apart from the last one *)
		else if l1 < l2
		then if l1 + 1 = l2
		     then [withll1 (L.ord (List.take (ll2, l1)))]
		     else map (fn x => withll1 (L.ord x)) (getpartsize (List.take (ll2, l2-1)) l1)
		else raise EH.DeadBranch "")
	       handle Subscript => raise EH.DeadBranch ""
	    end
	fun selectenough2 ll1 ll2 =
	    (if List.length ll1 > List.length ll2
	     then [L.union (L.ord (List.take (ll1, List.length ll2))) (L.ord ll2)]
	     else if List.length ll1 < List.length ll2
	     then [L.union (L.ord (List.take (ll2, List.length ll1))) (L.ord ll1)]
	     else raise EH.DeadBranch "")
	    handle Subscript => raise EH.DeadBranch ""
	fun conserrors ll1 ll2 css =
	    foldr (fn (x, y) => E.conscss (E.CSSFARG x) y)
		  css
		  (selectenough1 ll1 ll2)
    in #1 (foldr (fn (ll, (css, lls)) =>
		     (foldr (fn (ll', css) => if List.length ll = List.length ll'
					      then css
					      else conserrors ll ll' css)
			    css
			    lls,
		      ll :: lls))
		 (E.emcss, [])
		 llops)
    end

fun createDiffNbArgFuns fvalbindones =
    foldr (fn (fvalbindone, css) =>
	      E.uenvcss [createDiffNbArgFun (A.getLabsFValLab fvalbindone), css])
	  E.emcss
	  fvalbindones

(*(*(* this is not used anymore *)
fun consisadatacons [] _ = []
  | consisadatacons (tv :: tvl) lab =
    let
	val tnv = T.freshtynamevar ()
	val sv  = T.freshseqvar ()
	val c   = E.CSTTY (T.consV tv, T.C (T.NV tnv, T.SV sv, lab), L.empty)
    in c :: (consisadatacons tvl lab)
    end*)


fun compdecbas env1 env3 cs1 env2 env4 cs2 lab =
    let val (cs, env) = complet env2 cs2
				env1 cs1 env3 (* DO WE NEED env3'? - can't we do that below? *)
				lab
				E.DEC
    (* idV should be empty anyway *)
    in (env, E.plusEnv env3 env4, cs)
    end

fun compdec env1 env3 cs1 env2 env4 cs2 lab = compdecbas env1 env3 cs1 env2 env4 cs2 lab

(*
(* WORK ON THIS ONE *)
fun comptop basis1 basis3 cs1 basis2 basis4 cs2 =
    let
	val E.BASIS (sig1, env1) = basis1
	val E.BASIS (sig2, env2) = basis2
	val E.BASIS (sig3, env3) = basis3
	val E.BASIS (sig4, env4) = basis4
	val (cs, env) = complet env2 cs2 env1 cs1 env3
	val idG = E.plusenv
		      (E.outenv (E.getVids env3) (E.doms [E.getVars env4, E.getCons env4]))
		      (E.outenv (E.getVids env4) (E.doms [E.getVars env3, E.getCons env3]))
	val idX = E.plusenv
		      (E.outenv (E.getVars env3) (E.dom (E.getCons env4)))
		      (uenv [E.getVars env4, E.inenv (E.toREC (E.getVids env4)) (E.dom (E.getVars env3))])
	val idC = E.plusenv
		      (E.outenv (E.getCons env3) (E.dom (E.getVars env4)))
		      (E.getCons env4)
	val idT = E.plusenv (E.getTyps env3) (E.getTyps env4)
	val idS = E.ENVSEQ (E.plusenv (E.getSS env3) (E.getSS env4))
    in (E.BASIS (E.uenv [sig1, sig2], env),
	E.BASIS (E.emsig, E.ENVCON (idG, idX, idC, idT, E.emvar, idS)),
	cs)
    end
*)

fun compspec env1 env3 cs1 env2 env4 cs2 lab =
    let val (cs, env) = complet env2 cs2 env1 cs1 env3 lab E.DEC
	(* In a specification we shouldn't have any identifier for which we don't know the status *)
	val _ = if E.emEnvListAnd [E.projVids (E.getVids env3), E.projVids (E.getVids env4)]
		then ()
		else raise EH.DeadBranch ""
    in (env, E.uenvEnv [env3, env4], cs)
    end

(* This is not at all what we should use!! *)
fun freeTyVarTopLevel [] = []
  | freeTyVarTopLevel ((A.TypeVar (_, _, _, l, _), _, SOME l') :: xs) =
    (E.CSV (L.ord [l, l'])) :: (freeTyVarTopLevel xs)
  | freeTyVarTopLevel (_ :: xs) = freeTyVarTopLevel xs

(*fun tryAndBuild1 env cs basis pack =
    (case env of
	 E.ENVVAR ev =>
	 let
	     val (ascid, bbuild) = pack
	     (*val _ = D.printdebug2 (E.printcs cs ascid)*)
	     val cs = #3 (buildin (basis, E.embas, cs) ascid bbuild)
	 in case U.unif (E.getcsSem cs) NONE NONE of
		U.Success state =>
		(case C.buildenv env state of
		     (x as E.ENVCON _, labs, _, _, _) =>
		     let
			 (*val _ = D.printdebug2 (L.toString labs)*)
			 val (env', cs') = E.flattenEnv x
		     in (env', E.concatcsSem cs' cs)
		     end
		   | _ => (E.emenv, cs))
	      | U.Error _ => (E.emenv, cs)
	 end
       | _ => (env, cs))*)

(*fun tryAndBuild2 env cs basis pack =
    (case env of
	 E.ENVVAR ev =>
	 let
	     val cst1 = E.getcsSem cs
	     val css  = E.getcsSyn cs
	     val cst2 =
		 E.foldricst
		     (fn (k, ocsts, cst) =>
			 if k = L.dummylab
			 then case ocsts of
				  [E.CSTGEN (_, cst, _)] =>
				  cst
				| _ => cst
			 else cst)
		     E.emcst
		     cst1
	     val (evop, cst3) = (* these are the constraints we need to keep *)
		 E.foldricst
		     (fn (k, ocsts, (evop, cst)) =>
			 if k = L.dummylab
			 then case ocsts of
				  [E.CSTEGN ((ev, _, _, _, _), cst)] =>
				  (SOME ev, cst)
				| _ => (evop, cst)
			 else (evop, cst))
		     (NONE, E.emcst)
		     cst2
	     val (env', cs') =
		 case evop of
		     NONE => (E.emenv, cs)
		   | SOME ev =>
		     E.foldricst
			 (fn (k, ocsts, (env, cs)) =>
			     case ocsts of
				 [E.CSTEV (E.ENVVAR ev', env' as (E.ENVCON _))] =>
				 if ev = ev'
				 then (env', E.conscsss css cst3) (* Maybe in this case we can throw away css *)
				 else (env, cs)
			       | _ => (env, cs))
			 (E.emenv, cs)
			 cst3
	 in (env', cs')
	 end
       | _ => (env, cs))*)

fun tryAndBuild3 (env1, env2, cs) _ =
    let val cst1 = E.getcsSem cs
	val css  = E.getcsSyn cs
	val cst2 = (* this is because of the open *)
	    E.foldricst
		(fn (k, ocsts, cst) =>
		    if k = L.dummylab
		    then case ocsts
			  of [E.CSTGEN (_, cst, _)] => cst
			   | _ => cst
		    else cst)
		E.emcst
		cst1
	val (evop, cst3) = (* these are the constraints we need to keep *)
	    E.foldricst
		(fn (k, ocsts, (evop, cst)) =>
		    if k = L.dummylab
		    then case ocsts
			  of [E.CSTEGN ((ev, _, _, _, _, _), cst)] =>
			     (SOME ev, cst)
			   | _ => (evop, cst)
		    else (evop, cst))
		(NONE, E.emcst)
		cst2
	val (env', cs') =
	    case evop
	     of NONE => (E.emenv, cs)
	      | SOME ev =>
		E.foldricst
		    (fn (k, ocsts, (env, cs)) =>
			case ocsts
			 of [E.CSTEV ((E.ENVVAR ev', env' as E.ENVCON _), _, _, _)] =>
			    if ev = ev'
			    then (env', E.conscsss css cst3) (* Maybe in this case we can throw away css *)
			    else (env, cs)
			  | _ => (env, cs))
		    (E.emenv, cs)
		    cst3
    in (env2, env', cs')
    end

(*(* removes the structure named Basis from the environment *)
fun removeBasisEnv env (ascid, _) =
    let
	(*val _    = D.printdebug2 (L.ascidToStr ascid)*)
	val idS  = E.getSS env
	val idS' = case L.lookupSt basisopen ascid of
			SOME id => E.remenv id idS
		      | _ => idS
    in E.updateStrs (E.ENVSEQ idS') env
    end*)

(* extracts the label associated to the Basis structure *)
fun getLabelBasis env pack =
    let val (ascid, _) = pack
    in case I.lookupSt basisopen ascid
	of SOME id =>
	   (case E.plusproj (E.getStrs env) id
	     of [extenv] => SOME (E.getBindL extenv)
	      | _ => NONE)
	 | _ => NONE
    end

(* lab is the label associated to the Basis *)
fun getEnvVarBasis cst lab =
    case E.getConsLab cst lab
     of [E.CSTEV ((E.ENVVAR ev, _), _, _, _)] => SOME ev
      | _ => NONE

fun tryAndBuild4 csenv pack =
    let val (env1, env2, cs) = csenv
	val cst = E.getcsSem cs
	val css = E.getcsSyn cs
    in case getLabelBasis env2 pack of
	   SOME lab =>
	   (case getEnvVarBasis cst lab of
		SOME ev => (* ev is the variable associated to the basis.
			    * We have to search for that in a CSTEGN. *)
		(case E.getSigOfStr (E.getcsSem cs) ev of
		     (cst', SOME (ev', cst'')) =>
		     (* ev' is the variable associated to the signature of the Basis.
		      * We have to search for this.
		      * cst' is the constaints without the ones for the Basis signature.
		      * cst'' is the constraints for the Basis signature.
		      * Because we extract them then they shouldn't depend on anything else,
		      * it is why Basis and its opening should be at the beginning. *)
		     (case E.getEnvEqVar cst'' ev' of
			  SOME env =>
			  compdecbas E.emenv env (E.projcsSem cst'') (* this is for the opened basis *)
				     env1 env2 (E.conscsss css cst') (* this is for the rest *)
				     L.dummylab
			| NONE => csenv) (* no env equal to the envvar of the signature *)
		   | (_, NONE) => csenv) (* no signature for the envvar of the structure *)
	      | NONE => csenv) (* no constraint associated to the environmnent *)
	 | NONE => csenv (* no basis in the env *)
    end

(*fun treatAtPatOr envs =
    let
	fun mergeExtTys {ty = ty1, class = CL.VAL (nonexp1, labs1), lab = lab1}
			{ty = ty2, class = CL.VAL (nonexp2, labs2), lab = lab2}
			tv =
	    T.consextty (T.consV tv)
			(CL.VAL (A.composeNonexp nonexp1 nonexp2, L.concat labs1 labs2))
			(L.concat cd1 cd2)
			((* which label *))
	fun mergeSemtys [] _ = raise EH.DeadBranch ""
	  | mergeSemtys [x] _ = x
	  | mergeSemtys (x :: y :: xs) tv = mergeSemtys ((mergeExtTys x y tv) :: xs)
	fun mergeTwoIdGs idG1 idG2 =
	    E.foldrienv
		(fn (id, semty, idG) =>
		    let
			val tv = T.freshtyvar ()
			val semty' = E.plusproj idG2 id
			val semty'' = ()
		    in ()
		    end)
		E.emid
		idG1
	fun mergeIdGs [] = E.emid
	  | mergeIdGs [x] = x
	  | mergeIdGs (x :: y :: xs) = mergeIdGs ((mergeTwoIdGs x y) :: xs)
	fun gatherIdGs [] = []
	  | gatherIdGs (x :: xs) = (E.getVids x) :: (gatherIdGs xs)
	val env = E.uenvEnv envs
	val idGs = mergeIdGs (getIdGs envs)
    in env
    end*)

fun updateExpans _ (x as (X.Expexp _)) = x
  | updateExpans _ (x as (X.Expdep (I.ID _, _, _, _))) = x
  | updateExpans env (X.Expdep (lid as (I.LID ((i, l), _, _)), _, ls, st)) =
    let val semty = E.plusproj (E.getStrs env) i
	val etvop = List.find (fn x => (E.getBindL x) = l) semty
	val evop  = case etvop of NONE => NONE (* we have NONE here in case the structure is a free id *) (*raise EH.DeadBranch "unknown structure of long identifier"*)
				| SOME ({id, scope, bind = E.ENVVAR ev, class, lab, poly}, _, _, _) => SOME ev
				| _ => raise EH.DeadBranch "at this stage the signature of a structure should be a variable"
    in X.Expdep (lid, evop, ls, st)
    end

fun updateNonexp _ X.Nonexp = X.Nonexp
  | updateNonexp env (X.Expans xs) =
    X.Expans (map (fn x => updateExpans env x) xs)

fun extractFromLong idS idG =
    let val domS = I.toList (E.dom idS)
	val domG = I.toList (E.dom idG)
	fun onlyOne [x] = x
	  | onlyOne _   = raise EH.DeadBranch ""
    in case (domS, domG) of
	   ([id], []) => (SOME (onlyOne (E.plusproj idS id)), NONE)
	 | ([], [id]) => (NONE, SOME (onlyOne (E.plusproj idG id)))
	 | _ => (NONE, NONE)
    end

fun applyFunOp NONE x = x
  | applyFunOp (SOME f) x = E.mapExtLab x f

fun applyClassFunOp NONE cl _ = cl
  | applyClassFunOp (SOME f) cl lab =
    let	val consid  = C.consBindPoly (I.fromInt 0) (T.consV 0) (*dummy tyvar*) cl lab
    in C.getBindC (f consid) end

fun genCstLongIdTyNameVar (I.ID (_, lab)) tv =
    let val ty1 = T.consV tv
	val ty2 = T.consTyNameVar lab
    in E.singcstSem (lab, E.genCstTyEm ty1 ty2 lab)
    end
  | genCstLongIdTyNameVar (I.LID ((_, lab1), lid, lab2)) tv =
    let val tv1 = T.freshtyvar ()
	val tv2 = T.freshtyvar ()
	val c1  = E.genCstTyEm (T.consV tv)  (T.consV tv1) lab1
	val c2  = E.genCstTyEm (T.consV tv1) (T.consV tv2) lab2
	val cst = genCstLongIdTyNameVar lid tv2
    in E.conscstSem (lab1, c1) (E.conscstSem (lab2, c2) cst)
    end*)

fun unzipThree xs = foldr (fn ((x, y, z), (xs, ys, zs)) => (x :: xs, y :: ys, z :: zs))
			  ([], [], [])
			  xs

fun unzipFour xs = foldr (fn ((x, y, z, w), (xs, ys, zs, ws)) => (x :: xs, y :: ys, z :: zs, w :: ws))
			 ([], [], [], [])
			 xs

fun unzipFive xs = foldr (fn ((x, y, z, w, v), (xs, ys, zs, ws, vs)) => (x :: xs, y :: ys, z :: zs, w :: ws, v :: vs))
			 ([], [], [], [], [])
			 xs

fun unzipSix xs = foldr (fn ((x, y, z, w, v, u), (xs, ys, zs, ws, vs, us)) => (x :: xs, y :: ys, z :: zs, w :: ws, v :: vs, u :: us))
			([], [], [], [], [], [])
			xs

(* Here are all the composition function:
 * - compmono: to compose a monomorphic environment with another one
 * - completbas: to compose an environment with another one without
 *   calculating the new outter environment (it is at the basis level)
 * - complet: same as completbas but at the environment level
 * - compdecbas: similar to completbas but calculate the new outter
 *   environment (at the basis level)
 * - compdec: same as comdecbas but at the environment level
 * - compspec: similar to compdecbas but to specification of the same
 *   id have same priority. *)


(********************************************)
(*           CONSTRAINT GENERATION          *)
(********************************************)


fun generateConstraints' prog pack nenv =
    let

	val benv = case nenv of 2 => true | _ => false

	val isBasis = ref false

	fun setBasis x = isBasis := x
	fun getBasis _ = !isBasis

	val tyNamesTop : string list ref = ref T.tyNames

	fun getTyNameString s = if Tools.isin s (!tyNamesTop)
				then (tyNamesTop := #2 (Tools.remove s (!tyNamesTop));
				      T.getTyNameString s)
				else T.freshtyname ()

	(*fun f_part (A.PartExp e) =
	    let val (env, _, cs) = f_exp e
	    in (env, NONE, cs)
	    end
	  | f_part (A.PartDec d) =
	    let val (env, _, cs) = f_dec d
	    in (env, NONE, cs)
	    end
	  | f_part (A.PartType t) =
	    let val (env, _, cs) = f_type t
	    in (env, NONE, cs)
	    end
	  | f_part (A.PartSeq s) =
	    let val (env, _, cs, _) = f_typeseq s
	    in (env, NONE, cs)
	    end
	  | f_part (A.PartPat p) =
	    let val (env, _, cs) = f_pat p
	    in (env, NONE, cs)
	    end
	  | f_part (A.PartIdTy id) =
	    let val (env, _, cs) = f_identty id
	    in (env, NONE, cs)
	    end
	  | f_part (A.PartTyCon tc) =
	    let val ((idT, idS), _, cs) = f_longtycon tc
	    in (E.updateTyps idT (E.projStrs idS), NONE, cs)
	    end
	  | f_part (A.PartSpec sp) =
	    let val (env, _, cs) = f_specone sp
	    in (env, NONE, cs)
	    end
	  | f_part (A.PartSige si) =
	    let val (env, _, cs) = f_sigexp si (* MARK *)
	    in (env, NONE, cs)
	    end
	  | f_part (A.PartStre se) =
	    let val (env, _, cs) = f_strexp se
	    in (env, NONE, cs)
	    end
	  (*| f_part (A.PartFund fd) =
	    let val (env, _, cs) = f_fundec fd
	    in (env, NONE, cs)
	    end*)
	  | f_part (A.PartSigd sd) =
	    let val (env, _, cs) = f_sigdec sd
	    in (env, NONE, cs)
	    end
	  | f_part (A.PartStrd sd) =
	    let val (env, _, cs) = f_strdecone sd
	    in (env, NONE, cs)
	    end
	  | f_part (A.PartLgid id) =
	    let val ((idG, idS), _, cs) = f_longid id (true, NONE)
	    in (E.uenvEnv [E.projVids idG, E.projStrs idS], NONE, cs)
	    end
	  | f_part (A.PartLgsid id) =
	    let val (idS, _, cs) = f_longstrid id
	    in (E.projStrs idS, NONE, cs)
	    end
	  | f_part (A.PartSigid id) =
	    let val (idS, _, cs) = f_sigid id true
	    in (E.emenv, NONE, cs)
	    end
	  | f_part (A.PartTes st) =
	    let val (env, _, cs) = f_smltes st
	    in (env, NONE, cs)
	    end
	  | f_part (A.PartClass cl) =
	    let val (idD, _, cs) = f_class cl true
	    in (E.projOvcs idD, NONE, cs)
	    end

	   and f_partlist pl =
	       let val envl = map f_part pl
		   val env = E.uenvEnv (getenvEnv envl)
		   val cs = E.uenvcss (getenvCs envl)
	       in (env, NONE, cs)
	       end*)

	fun f_partlist _ = raise EH.TODO


	   and f_scon (A.SconInt (s, v, _, lab, _)) =
	       if benv (* We bind the integer to the "Int" overloading class *)
	       then let val sv = T.freshseqvar ()
			val tv = T.freshtyvar  ()
			val a  = E.genAccIoEm (E.consAccId (I.ID (v, lab)) (T.SV sv) (CL.consOC ()) lab) lab
			val c  = E.genCstTyEm (T.consV tv) (T.OR (T.SV sv, T.freshidor (), T.POLY, T.CST (s, v, lab), lab)) lab false
		    in (tv, E.singcsts (lab, [E.CSTACC a, c]), E.emcss)
		    end
	       else let val tv = T.freshtyvar ()
			val c  = E.genCstTyEm (T.consV tv) (T.constyint lab) lab false
		    in (tv, E.singcst (lab, c), E.emcss)
		    end
	     | f_scon (A.SconWord (s, v, _, lab, _)) =
               if benv (* We bind the word to the "Word" overloading class *)
	       then let val sv = T.freshseqvar ()
			val tv = T.freshtyvar  ()
			val a  = E.genAccIoEm (E.consAccId (I.ID (v, lab)) (T.SV sv) (CL.consOC ()) lab) lab
			val c  = E.genCstTyEm (T.consV tv) (T.OR (T.SV sv, T.freshidor (), T.POLY, T.CST (s, v, lab), lab)) lab false
		    in (tv, E.singcsts (lab, [E.CSTACC a, c]), E.emcss)
		    end
	       else let val tv = T.freshtyvar ()
			val c  = E.genCstTyEm (T.consV tv) (T.constyword lab) lab false
		    in (tv, E.singcst (lab, c), E.emcss)
		    end
	     | f_scon (A.SconReal (s, v, _, lab, _)) =
               if benv (* We bind the real to the "Real" overloading class *)
	       then let val sv = T.freshseqvar ()
			val tv = T.freshtyvar  ()
			val a  = E.genAccIoEm (E.consAccId (I.ID (v, lab)) (T.SV sv) (CL.consOC ()) lab) lab
			val c  = E.genCstTyEm (T.consV tv) (T.OR (T.SV sv, T.freshidor (), T.POLY, T.CST (s, v, lab), lab)) lab false
		    in (tv, E.singcsts (lab, [E.CSTACC a, c]), E.emcss)
		    end
	       else let val tv = T.freshtyvar ()
			val c  = E.genCstTyEm (T.consV tv) (T.constyreal lab) lab false
		    in (tv, E.singcst (lab, c), E.emcss)
		    end
	     | f_scon (A.SconString (s, v, _, lab, _)) =
               if benv (* We bind the string to the "String" overloading class *)
	       then let val sv = T.freshseqvar ()
			val tv = T.freshtyvar  ()
			val a  = E.genAccIoEm (E.consAccId (I.ID (v, lab)) (T.SV sv) (CL.consOC ()) lab) lab
			val c  = E.genCstTyEm (T.consV tv) (T.OR (T.SV sv, T.freshidor (), T.POLY, T.CST (s, v, lab), lab)) lab false
		    in (tv, E.singcsts (lab, [E.CSTACC a, c]), E.emcss)
		    end
	       else let val tv = T.freshtyvar ()
			val c  = E.genCstTyEm (T.consV tv) (T.constystring lab) lab false
		    in (tv, E.singcst (lab, c), E.emcss)
		    end
	     | f_scon (A.SconChar (s, v, _, lab, _)) =
               if benv (* We bind the char to the "Char" overloading class *)
	       then let val sv = T.freshseqvar ()
			val tv = T.freshtyvar  ()
			val a  = E.genAccIoEm (E.consAccId (I.ID (v, lab)) (T.SV sv) (CL.consOC ()) lab) lab
			val c  = E.genCstTyEm (T.consV tv) (T.OR (T.SV sv, T.freshidor (), T.POLY, T.CST (s, v, lab), lab)) lab false
		    in (tv, E.singcsts (lab, [E.CSTACC a, c]), E.emcss)
		    end
	       else let val tv = T.freshtyvar ()
			val c  = E.genCstTyEm (T.consV tv) (T.constychar lab) lab false
			val bs = (Char.fromString
				      (String.translate (fn #"\\" => "\\\\" | x => Char.toString x)
							(String.substring (s, 2, (String.size s) - 3))))
			    handle Subscript => NONE
			val css = case bs of NONE => E.singcss (charBadCs lab)
					   | SOME s'_ =>
					     if size s = 4
						orelse
						(size s = 5 andalso String.isPrefix "#\"\\" s)
					     then E.emcss
					     else E.singcss (charSizeCs lab)
		    in (tv, E.singcst (lab, c), css)
		    end
	     | f_scon A.SconDots = (T.freshtyvar (), E.emcst, E.emcss)

	   (*(* f_pcon, f_ident and f_longid have an extra Boolean parameter.
	    * This parameter is true if the id is inside an expression and false otherwise. *)
	   and f_pcon (A.PconBool (s, v, r, lab, n)) (b, d) =
	       if getBasis () orelse (benv andalso b)
	       then f_ident (A.Ident (s, v, r, lab, n)) (b, d)
	       else let val tv = T.freshtyvar ()
			val c  = E.genCstTyEm (T.consV tv) (T.constybool lab) lab
			val cs = if d then E.emcs else E.singcsSyn (reboundCs lab s)
		    in (E.emvar, tv, E.conscsSem (lab, c) cs) end
	     | f_pcon (A.PconRef (s, v, r, lab, n)) (b, d) =
	       if getBasis () orelse (benv andalso b)
	       then f_ident (A.Ident (s, v, r, lab, n)) (b, d)
	       else let val tv  = T.freshtyvar ()
			val tv1 = T.freshtyvar ()
			val tv2 = T.freshtyvar ()
			val c1  = E.genCstTyEm (T.consV tv2) (T.constyref tv1 lab) lab
			val c2  = E.genCstTyEm (T.consV tv) (T.constyarrow tv1 tv2 lab) lab
			val cs  = if d then E.emcs else E.singcsSyn (reboundCs lab s)
		    in (E.emvar, tv, E.conscsSems (lab, [c1, c2]) cs) end
	     | f_pcon (A.PconNil (s, v, r, lab, n)) (b, d) =
	       if getBasis () orelse (benv andalso b)
	       then f_ident (A.Ident (s, v, r, lab, n)) (b, d)
	       else let val tv  = T.freshtyvar ()
			val tv' = T.freshtyvar ()
			val c   = E.genCstTyEm (T.consV tv) (T.constylist tv' lab) lab
			val cs  = if d then E.emcs else E.singcsSyn (reboundCs lab s)
		    in (E.emvar, tv, E.conscsSem (lab, c) cs) end
	     | f_pcon A.PconDots _ = (E.emvar, T.freshtyvar (), E.emcs)*)

	   (* RETURNS: (Ty.tyvar, Env.varenv, E.emcst, E.emcss) *)
	   and f_labid (A.LabId (ident, _, lab, _)) =
	       let val (tv, vids, cst, css) = f_identpat ident
		   val tv' = T.freshtyvar ()
		   val c   = E.genCstTyEm (T.consV tv') (T.consV tv) lab false
	       in (tv', vids, E.conscst (lab, c) cst, css)
	       end
	     | f_labid (A.LabIdDots pl) =
	       let val env = f_partlist pl
	       in (T.freshtyvar (), E.getVids env, E.emcst, E.emcss)
	       end

	   (*(* b is true in an exp and false in a pat *)
	   and f_ident (A.Ident (s, v, _, lab, _)) (b, _) = (* lst stands for label for status *)
	       if String.isPrefix "_" s
	       then (E.emvar, T.freshtyvar (), E.emcs)
	       else let val tv1 = T.freshtyvar ()
			val tv2 = T.freshtyvar ()
			val cl  = CL.consVAL ()
			val idG = case (SO.getSol (), b) of
				      (SO.SOL8, true) => E.emvar
				    | _ => E.singenv (v, [E.consBindPoly v (T.consV tv2) cl lab])
			val c   = case (SO.getSol (), b) of
				      (SO.SOL8, true) =>
				      let val id = E.genIdCst (I.ID (v, lab)) cl lab
				      in E.genCstItEm (T.consV tv1) id lab end
				    | _ => E.genCstTyEm (T.consV tv1) (T.consV tv2) lab
		    in (idG, tv1, E.singcsSem (lab, c))
		    end
	     | f_ident (A.IdentPcon pc) b =
	       let val (idG, tv, cs) = f_pcon pc b
	       in (idG, tv, cs)
	       end
	     | f_ident A.IdentDots _ = (E.emvar, T.freshtyvar (), E.emcs)*)

	   (* RETURNS: (Env.envvar, Env.strenv, E.emcst, E.emcss) *)
	   and f_strid (A.StrId (s, v, _, lab, _)) =
	       if String.isPrefix "_" s
	       then (E.freshenvvar (), E.emstr, E.emcst, E.emcss)
	       else let val ev1  = E.freshenvvar ()
			val ev2  = E.freshenvvar ()
			val strs = E.singenv (v, [E.consBindPoly v (E.consEnvVar ev2 lab) (CL.consSTR ()) lab])
			val c    = E.genCstEvEm (E.ENVVAR (ev1, lab)) (E.ENVVAR (ev2, lab)) lab
		    in (ev1, strs, E.singcst (lab, c), E.emcss)
		    end
	     | f_strid A.StrIdDots =
	       (E.freshenvvar (), E.emstr, E.emcst, E.emcss)

	   (* RETURNS: (Env.envvar, Env.envvar, Env.cst) *)
	   and f_funid (A.FunId (s, v, _, lab, _)) =
	       if String.isPrefix "_" s
	       then (E.freshenvvar (), E.freshenvvar (), E.emcst)
	       else let val lid  = I.idToLid v lab
			val ev1  = E.freshenvvar ()
			val ev2  = E.freshenvvar ()
			val env1 = E.consEnvVar ev1 lab
			val env2 = E.consEnvVar ev2 lab
			val a    = E.genAccIfEm (E.consAccId lid (env1, env2) (CL.consFUNC ()) lab) lab
		    in (ev1, ev2, E.singcst (lab, E.CSTACC a))
		    end
	     | f_funid A.FunIdDots = (E.freshenvvar (), E.freshenvvar (), E.emcst)

	   (* RETURNS: (Env.envvar, Env.envvar, Env.funenv, E.emcst) *)
	   and f_funidbind (A.FunId (s, v, _, lab, _)) =
	       if String.isPrefix "_" s
	       then (E.freshenvvar (), E.freshenvvar (), E.emfun, E.emcst)
	       else let val ev1  = E.freshenvvar ()
			val ev2  = E.freshenvvar ()
			val ev1' = E.freshenvvar ()
			val ev2' = E.freshenvvar ()
			val env1 = E.consEnvVar ev1' lab
			val env2 = E.consEnvVar ev2' lab
			val funs = E.singenv (v, [E.consBindPoly v (env1, env2) (CL.consFUNC ()) lab])
			val c1   = E.genCstEvEm (E.ENVVAR (ev1, lab)) (E.ENVVAR (ev1', lab)) lab
			val c2   = E.genCstEvEm (E.ENVVAR (ev2, lab)) (E.ENVVAR (ev2', lab)) lab
		    in (ev1, ev2, funs, E.singcsts (lab, [c1, c2]))
		    end
	     | f_funidbind A.FunIdDots =
	       (E.freshenvvar (), E.freshenvvar (), E.emfun, E.emcst)

	   (*and f_funid (A.FunId (s, v, _, lab, _)) b =
	       if String.isPrefix "_" s
	       then (E.emfun, (E.freshenvvar (), E.freshenvvar ()), E.emcs)
	       else let val ev1  = E.freshenvvar ()
			val ev2  = E.freshenvvar ()
			val ev1' = E.freshenvvar ()
			val ev2' = E.freshenvvar ()
			val idF  = case (SO.getSol (), b) of
				       (SO.SOL8, true) => E.emfun
				     | _ => E.singenv (v, [E.consBindPoly v (E.consEnvVar ev2, E.consEnvVar ev2') (CL.consFUNC ()) lab])
			val cs   = case (SO.getSol (), b) of
				       (SO.SOL8, true) =>
				       let val id = E.genIdCst (I.ID (v, lab)) (CL.consFUNC ()) lab
				       in [E.genCstIfEm (E.ENVVAR ev1, E.ENVVAR ev1') id lab]
				       end
				     | _ =>
				       let val c1 = E.genCstEvEm (E.ENVVAR ev1)  (E.ENVVAR ev2)  lab
					   val c2 = E.genCstEvEm (E.ENVVAR ev1') (E.ENVVAR ev2') lab
				       in [c1, c2]
				       end
		    in (idF, (ev1, ev1'), E.singcsSems (lab, cs))
		    end
	     | f_funid A.FunIdDots _ = (E.emfun, (E.freshenvvar (), E.freshenvvar ()), E.emcs)*)

	   (*(* Never called with SOL8, only used by longtycon1. *)
	   and f_tycon (A.TyCon (s, v, _, lab, _)) =
	       if String.isPrefix "_" s
	       then (E.emvar, (T.freshtyvar (), T.freshseqvar ()), E.emcs)
	       else let val tv1 = T.freshtyvar ()
			val tv2 = T.freshtyvar ()
			val sv  = T.freshseqvar ()
			val idT = E.singenv (v, [E.consBindPoly v (T.consV tv1) (CL.consTYPsv sv) lab])
			(*val c1  = E.CSTTY (T.consV tv1, T.App (T.SV sv1, T.consV tv2, lab), L.empty)*)
			val c   = (*case SO.getSol () of
					SO.SOL8 =>
					let val id = E.genIdCst (I.ID (v, lab)) (CL.TYCON (CL.TYP sv)) lab
					in E.genCstItEm tv1 id lab end
				      | _ =>*) E.genCstTyEm (T.consV tv1) (T.consV tv2) lab
		    in (idT, (tv2, sv), E.singcsSem (lab, c)) (* before it was tv3!!!!  Why???? *)
		    end
	     | f_tycon A.TyConDots =
	       (E.emtyp, (T.freshtyvar (), T.freshseqvar ()), E.emcs)*)

	   (* RETURNS: (Env.envvar, Env.cst) *)
	   and f_sigid (A.SigId (s, v, _, lab, _)) =
	       if String.isPrefix "_" s
	       then (E.freshenvvar (), E.emcst)
	       else let val lid = I.idToLid v lab
			val ev  = E.freshenvvar ()
			val a   = E.genAccIiEm (E.consAccId lid (E.consEnvVar ev lab) (CL.consSIG ()) lab) lab
		    in (ev, E.singcst (lab, E.CSTACC a))
		    end
	     | f_sigid A.SigIdDots = (E.freshenvvar (), E.emcst)

	   (* RETURNS: (Env.envvar, Env.sigenv, Env.cst) *)
	   and f_sigidbind (A.SigId (s, v, _, lab, _)) =
	       if String.isPrefix "_" s
	       then (E.freshenvvar (), E.emsig, E.emcst)
	       else let val ev1  = E.freshenvvar ()
			val ev2  = E.freshenvvar ()
			val sigs = E.singenv (v, [E.consBindPoly v (E.consEnvVar ev2 lab) (CL.consSIG ()) lab])
			val c    = E.genCstEvEm (E.consEnvVar ev1 lab) (E.consEnvVar ev2 lab) lab
		    in (ev1, sigs, E.singcst (lab, c))
		    end
	     | f_sigidbind A.SigIdDots = (E.freshenvvar (), E.emsig, E.emcst)

	   (*and f_longid2 longid fstat =
	       case A.longidToLid longid of
		   NONE => ((E.emvar, E.emstr),
			    (T.freshtyvar (), NONE),
			    E.emcs)
		 | SOME lid =>
		   let val lab = I.getTopLab lid
		       val tv  = T.freshtyvar ()
		       val cl  = applyClassFunOp fstat (CL.consVAL ()) lab
		       val id  = E.genIdCst lid cl lab
		       val c   = E.genCstItEm (T.consV tv) id lab
		       val cst = if I.isLong lid
				 then genCstLongIdTyNameVar lid tv
				 else E.emcst
		   in ((E.emvar, E.emstr),
		       (tv, SOME cst),
		       E.singcsSem (lab, c))
		   end*)

	   (*(* If state is NONE then the id comes from an expression. *)
	   (* If false then the longid cannot be a binder. *)
	   and f_longid longid (b, fstat) =
	       case (SO.getSol (), b) of
		   (SO.SOL8, false) => f_longid2 longid fstat
		 | _                => f_longid1 longid fstat*)

	   (* RETURNS: (Ty.tyvar, Env.cst) *)
	   and f_longidexp longid =
	       case A.longidToLid longid of
		   NONE => (T.freshtyvar (), L.dummyLab, CL.newCl (), E.emcst)
		 | SOME lid =>
		   let val lab = I.getLabId lid
		       (* NOTE: We want all the labels from lid *)
		       val tv  = T.freshtyvar ()
		       val cl  = CL.newCl ()
		       val a   = E.genAccIvEm (E.consAccId lid (T.consV tv) cl lab) lab
		   in (tv, lab, cl, E.singcst (lab, E.CSTACC a))
		   end

	   (* RETURNS: (Ty.tyvar, Env.varenv, E.emcst, E.emcss) *)
	   and f_pconpat (A.PconBool (s, v, reg, lab, nxt)) =
	       if getBasis ()
	       then f_identpat (A.Ident (s, v, reg, lab, nxt))
	       else let val tv = T.freshtyvar ()
			val c  = E.genCstTyEm (T.consV tv) (T.constybool lab) lab false
		    in (tv, E.emvar, E.singcst (lab, c), E.singcss (reboundCs lab s))
		    end
	     | f_pconpat (A.PconRef (s, v, reg, lab, nxt)) =
	       if getBasis ()
	       then f_identpat (A.Ident (s, v, reg, lab, nxt))
	       else let val tv  = T.freshtyvar ()
			val tv' = T.freshtyvar ()
			val c   = E.genCstTyEm (T.consV tv) (T.consTyArrowTy (T.consV tv') (T.constyref tv' lab) lab T.OT) lab false
		    in (tv, E.emvar, E.singcst (lab, c), E.singcss (reboundCs lab s))
		    end
	     | f_pconpat (A.PconNil (s, v, reg, lab, nxt)) =
	       if getBasis ()
	       then f_identpat (A.Ident (s, v, reg, lab, nxt))
	       else let val tv  = T.freshtyvar ()
			val tv' = T.freshtyvar ()
			val c   = E.genCstTyEm (T.consV tv) (T.constylist tv' lab) lab false
		    in (tv, E.emvar, E.singcst (lab, c), E.singcss (reboundCs lab s)) end
	     | f_pconpat A.PconDots = (T.freshtyvar (), E.emvar, E.emcst, E.emcss)

	   (* RETURNS: (Ty.tyvar, Env.varenv, E.emcst, Env.emcss) *)
	   and f_identpat (A.Ident (s, id, _, lab, _)) =
	       if String.isPrefix "_" s
	       then (T.freshtyvar (), E.emvar, E.emcst, E.emcss)
	       else let val tv1  = T.freshtyvar ()
			val tv2  = T.freshtyvar ()
			val vids = E.singenv (id, [E.consBindPoly id (T.consV tv2) (CL.consVAL ()) lab])
			val c    = E.genCstTyEm (T.consV tv1) (T.consV tv2) lab false
		    in (tv1, vids, E.singcst (lab, c), E.emcss)
		    end
	     | f_identpat (A.IdentPcon pc) = f_pconpat pc
	     | f_identpat A.IdentDots = (T.freshtyvar (), E.emvar, E.emcst, E.emcss)

	   (* RETURNS: (Ty.tyvar, Env.env, Env.emcss) *)
	   and f_longidpat (A.LongIdId ident) = f_identpat ident
	     | f_longidpat longid =
	       let val (tv, lab, cl, cst) = f_longidexp longid
		   val c = E.genCstClEm cl (CL.consCO0 ()) lab
	       in (tv, E.emvar, E.conscst (lab, c) cst, E.emcss)
	       end

	   (* RETURNS: (Env.envvar, Env.cst) *)
	   and f_longstrid longstrid =
	       case A.longstridToLid longstrid of
		   NONE => (E.freshenvvar (), E.emcst)
		 | SOME lid =>
		   let val lab = I.getLabId lid
		       (* NOTE: We want all the labels from lid *)
		       val ev  = E.freshenvvar ()
		       val a   = E.genAccIsEm (E.consAccId lid (E.consEnvVar ev lab) (CL.consSTR ()) lab) lab
		   in (ev, E.singcst (lab, E.CSTACC a))
		   end

	   (*(* a longstrid is always in an expression *)
	   and f_longstrid1 (A.LongStrIdQual (sid, lsid, _, lab, _)) =
	       let val (idS, (ev1, _), cs1) = f_strid sid
		   val (idS', ev3, cs2) = f_longstrid1 lsid
		   val cs3 = E.uenvcs cs1 cs2
		   val cs = case extractFromLong idS' E.emvar of
				(SOME eenv, NONE) =>
				let val l = E.getBindL eenv
				    val c = E.genCstEvEm (E.INJSTR eenv) (E.ENVVAR ev1) l
				in E.conscsSem (l, c) cs3 end
			      | _ => cs3
	       in (idS, ev3, cs)
	       end
	     | f_longstrid1 (A.LongStrIdId sid) =
	       let val (idS, (ev, _), cs) = f_strid sid
	       in (idS, ev, cs)
	       end
	     | f_longstrid1 (A.LongStrIdDots pl) =
	       let val (env, _, cs) = f_partlist pl
		   val ev = E.freshenvvar ()
	       in (E.emstr, ev, cs)
	       end*)

	   (*and f_longstrid2 longstrid =
	       case A.longstridToLid longstrid of
		   NONE => (E.emstr,
			    E.freshenvvar (),
			    E.emcs)
		 | SOME lid =>
		   let
		       val lab = I.getTopLab lid
		       val ev  = E.freshenvvar ()
		       val id  = E.genIdCst lid (CL.consSTR ()) lab
		       val c   = E.genCstIsEm (E.ENVVAR ev) id lab
		   in (E.emstr,
		       ev,
		       E.singcsSem (lab, c))
		   end*)

	   (*and f_longstrid longstrid =
	       case SO.getSol () of
		   SO.SOL8 => f_longstrid2 longstrid
		 | _       => f_longstrid1 longstrid*)

	   (*and f_longtycon1 (A.LongTyConQual (sid, ltv, _, lab, _)) =
	       let val (idS, (ev1, _), cs1) = f_strid sid
		   val ((idG', idS'), (tv3, sv), cs2) = f_longtycon1 ltv
		   (*val sv1' = T.freshseqvar ()
		   val sv2' = T.freshseqvar ()
		   val c1   = E.CSTTY (T.consV sv1', T.consV sv1, L.empty)
		   val c2   = E.CSTTY (T.consV sv2', T.consV sv2, L.empty)*)
		   val cs3 = E.uenvcs cs1 cs2
		   val cs = case extractFromLong idS' idG' of
				(NONE, SOME ety) =>
				let val l = E.getBindL ety
				    val c = E.genCstEvEm (E.INJTYP ety) (E.ENVVAR ev1) l
				in E.conscsSem (l, c) cs3 end
			      | (SOME eenv, NONE) =>
				let val l = E.getBindL eenv
				    val c = E.genCstEvEm (E.INJSTR eenv) (E.ENVVAR ev1) l
				in E.conscsSem (l, c) cs3 end
			      | _ => cs3
	       in ((E.emtyp, idS), (tv3, sv), cs)
	       end
	     | f_longtycon1 (A.LongTyConId tc) =
	       let val (idT, (tv, sv), cs) = f_tycon tc
	       in ((idT, E.emstr), (tv, sv), cs)
	       end
	     | f_longtycon1 (A.LongTyConDots pl) =
	       let val (env, _, cs) = f_partlist pl
		   val tv = T.freshtyvar ()
		   val sv = T.freshseqvar ()
	       in ((E.emtyp, E.emstr), (tv, sv), cs)
	       end*)

	   (* RETURNS: (Ty.tyfvar, Env.cst) *)
	   and f_longtycon longtycon =
	       case A.longtyconToLid longtycon of
		   NONE => (T.freshtyfvar (), E.emcst)
		 | SOME lid =>
		   let val lab = I.getLabId lid
		       val tfv = T.freshtyfvar ()
		       val a   = E.genAccItEm (E.consAccId lid (T.consTFV tfv) (CL.consTYCON ()) lab) lab
		   in (tfv, E.singcst (lab, E.CSTACC a))
		   end

	   (*and f_longtycon longtycon =
	       case SO.getSol () of
		   SO.SOL8 => f_longtycon2 longtycon
		 | _       => f_longtycon1 longtycon*)

	   (* RETURNS: (Ty.tyvar, Env.cst, Env.css) *)
	   and f_labexp (A.LabExp (exp, _, _, lab, _)) =
	       let
		   val _   = D.printDebug 2 D.AZE ("generating constraints for A.LabExp")
		   val (tv, cst, css) = f_exp exp
		   val tv' = T.freshtyvar ()
		   val c   = E.genCstTyEm (T.consV tv') (T.consV tv) lab false
	       in (tv', E.conscst (lab, c) cst, css)
	       end
	     | f_labexp (A.LabExpDots pl) =
	       let val tv  = T.freshtyvar ()
		   val env = f_partlist pl
	       in (tv, E.singcst (L.dummyLab, E.CSTLET env), E.emcss)
	       end

	   (* RETURNS: (Ty.labvar, (string, Label.label) option, Env.cst) *)
	   and f_tylab (A.TyLab (s, _, lab, _)) =
	       let val lv = T.freshlabvar ()
		   val c  = E.genCstLtEm (T.LV lv) (T.LC (s, lab)) lab
	       in (lv, SOME (s, lab), E.singcst (lab, c))
	       end
	     | f_tylab A.TyLabDots =
	       (T.freshlabvar (), NONE, E.emcst)

	   (* RETURNS: (((string, Label.label) option, Ty.rowvar), Env.emcst, Env.emcss) *)
	   and f_exprow (A.ExpRow (tylab, labexp, _, _, lab, _)) =
	       let val (lv, lop, cst1)  = f_tylab tylab
		   val (tv, cst2, css2) = f_labexp labexp
		   val rv = T.freshrowvar ()
		   val c  = E.genCstRtEm (T.RV rv) (T.RC (T.LV lv, T.consV tv, lab)) lab
	       in ((lop, rv), E.conscst (lab, c) (E.uenvcst [cst1, cst2]), css2)
	       end
	     | f_exprow (A.ExpRowDots pl) =
	       let val rv  = T.freshrowvar ()
		   val env = f_partlist pl
	       in ((NONE, rv), E.singcst (L.dummyLab, E.CSTLET env), E.emcss)
	       end

	   (* RETURNS: (Ty.tyvar, Env.cst, Env.css) *)
	   and f_seqexp (A.SeqExp (labexps, labexp, _, _, lab, _)) =
	       let val (tv2, cst2, css2) = f_labexp labexp
		   val (_, csts, csss) = unzipThree (map f_labexp labexps)
		   val cst = E.uenvcst csts
		   val css = E.uenvcss csss
		   val tv  = T.freshtyvar ()
		   val c   = E.genCstTyEm (T.consV tv) (T.consV tv2) lab false
	       in (tv, E.conscst (lab, c) (E.uenvcst [cst, cst2]), E.uenvcss [css, css2])
	       end
	     | f_seqexp (A.SeqExpSl (parts, labexp, _, lab, _)) =
	       let val env1 = f_partlist parts
		   val (tv2, cst2, css2) = f_labexp labexp
		   val tv = T.freshtyvar ()
		   val c  = E.genCstTyEm (T.consV tv) (T.consV tv2) lab false
	       in (tv, E.conscsts (lab, [E.CSTLET env1, c]) cst2, css2)
	       end
	     | f_seqexp (A.SeqExpDots pl) =
	       let val tv  = T.freshtyvar ()
		   val env = f_partlist pl
	       in (tv, E.singcst (L.dummyLab, E.CSTLET env), E.emcss)
	       end

	   (* RETURNS: (Ty.tyvar, Env.cst, Env.css) *)
	   and f_atexp (A.AtExpId id) =
	       let
		   val _   = D.printDebug 2 D.AZE ("generating constraints for A.AtExpId")
		   val (tv, _, _, cst) = f_longidexp id
	       in (tv, cst, E.emcss)
	       end
	     | f_atexp (A.AtExpScon sc) = f_scon sc
	     | f_atexp (A.AtExpTuple (expl, _, lab, _)) =
               let val tv = T.freshtyvar ()
		   val (tvl, csts, csss) = unzipThree (map f_labexp expl)
		   val cst = E.uenvcst csts
		   val css = E.uenvcss csss
		   val c   = E.genCstTyEm (T.consV tv) (T.constytuple tvl lab) lab false
               in (tv, E.conscst (lab, c) cst, css)
               end
	     | f_atexp (A.AtExpRecord (exprows, _, _, lab, _)) =
               let val tv = T.freshtyvar ()
		   val (xs, csts, csss) = unzipThree (map f_exprow exprows)
		   val (lops, rts) = ListPair.unzip xs
		   val cst  = E.uenvcst csts
		   val css  = E.uenvcss csss
		   val css' = consCSMlab (List.mapPartial (fn x => x) lops)
		   val c    = E.genCstTyEm (T.consV tv) (T.constyrecord rts (T.noflex ()) lab) lab false
               in (tv, E.conscst (lab, c) cst, E.uenvcss [css, css'])
               end
	     | f_atexp (A.AtExpSlRec (exprows, _, lab, _)) =
               let val tv  = T.freshtyvar ()
		   val (xs, csts, csss) = unzipThree (map f_exprow exprows)
		   val (lops, rts) = ListPair.unzip xs
		   val cst  = E.uenvcst csts
		   val css  = E.uenvcss csss
		   val css' = consCSMlab (List.mapPartial (fn x => x) lops)
		   (* We condiser that as a widlcard because it is incomplete: *)
		   val c    = E.genCstTyEm (T.consV tv) (T.constyrecord rts (T.consflex lab) lab) lab false
               in (tv, E.conscst (lab, c) cst, E.uenvcss [css, css'])
               end
	     | f_atexp (A.AtExpLet (decs, labexp, _, lab, _)) =
	       let val tv   = T.freshtyvar ()
		   val (env1, css1) = f_decs decs
		   val (tv2, cst2, css2) = f_labexp labexp
		   val env = E.envsToSeq [env1, E.ENVCST cst2]
		   val c   = E.genCstTyEm (T.consV tv) (T.consV tv2) lab false
	       in (tv, E.conscst (L.dummyLab, E.CSTLET env) (E.singcst (lab, c)), E.uenvcss [css1, css2])
	       end
	     | f_atexp (A.AtExpDLet (decs, seqexp, _, lab, _)) =
	       let val tv   = T.freshtyvar ()
		   val (env1, css1) = f_decs decs
		   val (tv2, cst2, css2) = f_seqexp seqexp
		   val env = E.envsToSeq [env1, E.ENVCST cst2]
		   val c   = E.genCstTyEm (T.consV tv) (T.consV tv2) lab false
	       in (tv, E.conscst (L.dummyLab, E.CSTLET env) (E.singcst (lab, c)), E.uenvcss [css1, css2])
	       end
	     | f_atexp (A.AtExpParen (labexp, _, _, lab, _)) =
	       let val (tv, cst, css) = f_labexp labexp
		   val tv' = T.freshtyvar ()
		   val c   = E.genCstTyEm (T.consV tv') (T.consV tv) lab false
	       in (tv', E.conscst (lab, c) cst, css)
	       end
	     | f_atexp (A.AtExpList (labexps, _, lab, _)) =
	       let val tv   = T.freshtyvar ()
		   val tv'  = T.freshtyvar ()
		   val (tvs, csts, csss) = unzipThree (map f_labexp labexps)
		   val cst  = E.uenvcst csts
		   val css  = E.uenvcss csss
		   val cs'  = map (fn x => E.genCstTyEm (T.consV tv') (T.consV x) lab false) tvs
		   val c    = E.genCstTyEm (T.consV tv) (T.constylist tv' lab) lab false
	       in (tv, E.conscsts (lab, c :: cs') cst, css)
	       end
	     | f_atexp (A.AtExpProj (tylab, _, _, lab, _)) =
	       let val (lv, _, cst) = f_tylab tylab
		   val tv  = T.freshtyvar ()
		   val tv' = T.freshtyvar ()
		   val rv  = T.freshrowvar ()
		   val ty  = (T.constyrecord [rv] (T.consflex lab) lab)
		   val c1  = E.genCstTyEm (T.consV tv) (T.consTyArrowTy ty (T.consV tv') lab T.OT) lab false
		   val c2  = E.genCstRtEm (T.RV rv) (T.RC (T.LV lv, T.consV tv', lab)) lab
	       in (tv, E.conscsts (lab, [c1, c2]) cst, E.emcss)
	       end
	     | f_atexp (A.AtExpSeq (seqseq, _, lab, _)) =
	       let val (tv, cst, css) = f_seqexp seqseq
		   val tv' = T.freshtyvar ()
		   val c   = E.genCstTyEm (T.consV tv') (T.consV tv) lab false
	       in (tv', E.conscst (lab, c) cst, css)
	       end
	     | f_atexp (A.AtExpQuote (quotes, _, lab, _)) =
	       let val (tvs, csts, csss) = unzipThree (map f_quote quotes)
		   val tv  = T.freshtyvar ()
		   val tv' = T.freshtyvar ()
		   val cst = E.uenvcst csts
		   val css = E.uenvcss csss
		   val cs' = map (fn x => E.genCstTyEm (T.consV tv') (T.consV x) lab false) tvs
		   val c   = E.genCstTyEm (T.consV tv) (T.constylist tv' lab) lab false
	       in (tv, E.conscsts (lab, c :: cs') cst, css)
	       end
	     | f_atexp (A.AtExpDots pl) =
	       let val tv  = T.freshtyvar ()
		   val env = f_partlist pl
	       in (tv, E.singcst (L.dummyLab, E.CSTLET env), E.emcss)
	       end

	   (* When evaluation: `^(1)`, SML/NJ returns:
	    *   val it = [QUOTE "",ANTIQUOTE 1,QUOTE ""] : int SMLofNJ.frag list
	    *
	    * For this example, for the antiquote:
	    *   tv  = int
	    *   tv' = type of the antiquote *)

	   (* RETURNS: (Ty.tyvar, Env.cst, Env.css) *)
	   and f_quote (A.Quote (_, _, lab, _)) =
	       let val tv1 = T.freshtyvar ()
		   val tv2 = T.freshtyvar ()
		   val c   = E.genCstTyEm (T.consV tv1) (T.constyfrag tv2 lab) lab false
	       in (tv1, E.singcst (lab, c), E.emcss)
	       end
	     | f_quote (A.Antiquote(exp, _, lab, _)) =
	       let val (tv, cst, css) = f_exp exp
		   val tv' = T.freshtyvar ()
		   val c   = E.genCstTyEm (T.consV tv') (T.constyfrag tv lab) lab false
	       in (tv', E.conscst (lab, c) cst, css)
	       end
	     | f_quote (A.QuoteDots pl) =
	       let val tv  = T.freshtyvar ()
		   val env = f_partlist pl
	       in (tv, E.singcst (L.dummyLab, E.CSTLET env), E.emcss)
	       end

	   (* RETURNS: (Ty.tyvar, Env.cst, Env.css) *)
	   and f_exp (A.ExpAtExp atexp) =
	       (D.printDebug 2 D.AZE "generating constraints for A.ExpAtExp";
		f_atexp atexp)
	     | f_exp (A.ExpFn (match, _, lab, _)) =
	       let
		   val _ = D.printDebug 2 D.AZE "generating constraints for A.ExpAtExp"
		   val (tvs, cst, css) = f_match match
		   val tv = T.freshtyvar ()
		   val cs = map (fn x => E.genCstTyEm (T.consV tv) (T.consV x) lab false) tvs
	       in (tv, E.conscsts (lab, cs) cst, css)
	       end
	     | f_exp (A.ExpApp (exp, atexp, _, _, _, lab, _)) =
	       let
		   val _ = D.printDebug 2 D.AZE "generating constraints for A.ExpApp"
		   val (tv1, cst1, css1) = f_exp exp
		   val (tv2, cst2, css2) = f_atexp atexp
		   val tv  = T.freshtyvar ()
		   val c   = E.genCstTyEm (T.consV tv1) (T.constyarrow tv2 tv lab) lab false
	       in (tv, E.conscst (lab, c) (E.uenvcst [cst1, cst2]), E.uenvcss [css1, css2])
	       end
	     | f_exp (A.ExpCase (labexp, match, _, _, lab, _)) =
	       let
		   val _ = D.printDebug 2 D.AZE "generating constraints for A.ExpCase"
		   val tv  = T.freshtyvar ()
		   val tv' = T.freshtyvar ()
		   val (tv1, cst1, css1) = f_labexp labexp
		   val (tvs, cst2, css2) = f_match match
		   val cs  = map (fn x => E.genCstTyEm (T.consV tv') (T.consV x) lab false) tvs
		   val c   = E.genCstTyEm (T.consV tv') (T.constyarrow tv1 tv lab) lab false
	       in (tv, E.conscsts (lab, c :: cs) (E.uenvcst [cst1, cst2]), E.uenvcss [css1, css2])
	       end
	     | f_exp (A.ExpConsList (id, labexp1, labexp2, reg, lab, nxt)) =
	       if benv
	       then f_exp (A.ExpOp ("::", id, labexp1, labexp2, reg, lab, nxt))
	       else let
			val _ = D.printDebug 2 D.AZE "generating constraints for A.ExpConsList"
			val (tv1, cst1, css1) = f_labexp labexp1
			val (tv2, cst2, css2) = f_labexp labexp2
			val tv = T.freshtyvar ()
			val c1 = E.genCstTyEm (T.consV tv)  (T.constylist tv1 lab) lab false
			val c2 = E.genCstTyEm (T.consV tv2) (T.constylist tv1 lab) lab false
		    in (tv, E.conscsts (lab, [c1, c2]) (E.uenvcst [cst1, cst2]), E.uenvcss [css1, css2])
		    end
	     | f_exp (A.ExpOp (st, id, labexp1, labexp2, _, lab, _)) =
	       let
		   val _ = D.printDebug 2 D.AZE ("generating constraints for A.ExpOp (st = \"" ^ st ^ "\")")
		   val (tv1, cst1, css1) = f_labexp labexp1
		   val (tv2, cst2, css2) = f_labexp labexp2
		   val _ = D.printDebug 3 D.AZE ("ExpOp type variables are tv1 = "^(Int.toString(T.tyvarToInt(tv1)))^", tv2 = "^(Int.toString(T.tyvarToInt(tv2))))
		   val ty  = T.newV ()
		   val ti  = T.constytuple [tv1, tv2] lab
		   val tvo = T.freshtyvar ()
		   val c   = E.genCstTyEm ty (T.consTyArrowTy ti (T.consV tvo) lab T.OT) lab true
		   val a   = E.genAccIvEm (E.consAccId (I.ID (id, lab)) ty (CL.consVAL ()) lab) lab
	       in (tvo, E.conscsts (lab, [c, E.CSTACC a]) (E.uenvcst [cst1, cst2]), E.uenvcss [css1, css2])
	       end
	     | f_exp (A.ExpOr (labexp1, labexp2, _, lab, _)) =
	       let
		   val _ = D.printDebug 2 D.AZE "generating constraints for A.ExpOr"
		   val (tv1, cst1, css1) = f_labexp labexp1
		   val (tv2, cst2, css2) = f_labexp labexp2
		   val tvo = T.freshtyvar ()
		   val c1  = E.genCstTyEm (T.consV tvo) (T.constybool lab) lab false
		   val c2  = E.genCstTyEm (T.consV tv1) (T.constybool lab) lab false
		   val c3  = E.genCstTyEm (T.consV tv2) (T.constybool lab) lab false
	       in (tvo, E.conscsts (lab, [c1, c2, c3]) (E.uenvcst [cst1, cst2]), E.uenvcss [css1, css2])
	       end
	     | f_exp (A.ExpAnd (labexp1, labexp2, _, lab, _)) =
	       let
		   val _ = D.printDebug 2 D.AZE "generating constraints for A.ExpAnd"
		   val (tv1, cst1, css1) = f_labexp labexp1
		   val (tv2, cst2, css2) = f_labexp labexp2
		   val tvo = T.freshtyvar ()
		   val c1  = E.genCstTyEm (T.consV tvo) (T.constybool lab) lab false
		   val c2  = E.genCstTyEm (T.consV tv1) (T.constybool lab) lab false
		   val c3  = E.genCstTyEm (T.consV tv2) (T.constybool lab) lab false
	       in (tvo, E.conscsts (lab, [c1, c2, c3]) (E.uenvcst [cst1, cst2]), E.uenvcss [css1, css2])
	       end
	     | f_exp (A.ExpTyped (labexp, labtyp, _, lab, _)) =
	       let
		   val _ = D.printDebug 2 D.AZE "generating constraints for A.ExpTyped"
		   val tv = T.freshtyvar ()
		   val (tv1, cst1, css1) = f_labexp labexp
		   val (tv2, cst2, css2) = f_labtype labtyp
		   val c1 = E.genCstTyEm (T.consV tv) (T.consV tv1) lab false
		   val c2 = E.genCstTyEm (T.consV tv) (T.consV tv2) lab false
	       in (tv, E.conscsts (lab, [c1, c2]) (E.uenvcst [cst1, cst2]), E.uenvcss [css1, css2])
	       end
	     | f_exp (A.ExpIte (labexp1, labexp2, labexp3, _, lab, _)) =
	       let
		   val _   = D.printDebug 2 D.AZE ("generating constraints for A.ExpIte")
		   (* get the type variables, constraint on types and E.ocss *)
		   val (tv1, cst1, css1) = f_labexp labexp1
		   val (tv2, cst2, css2) = f_labexp labexp2
		   val (tv3, cst3, css3) = f_labexp labexp3

		   (* constrain the condition of the if statement to be of type bool*)
		   val c1  = E.genCstTyEm (T.consV tv1) (T.constybool lab) lab false

		   (* generate a fresh type variable and constrain the true/false branches
		    * of the if statemnt to be of the same type *)
		   val tv = T.freshtyvar ()
		   val c2  = E.genCstTyEm (T.consV tv) (T.consV tv2) lab false
		   val c3  = E.genCstTyEm (T.consV tv) (T.consV tv3) lab false

		   val cst = E.conscsts (lab, [c1, c2, c3]) (E.uenvcst [cst1, cst2, cst3])
		   val css = E.uenvcss [css1, css2, css3]
	       in (tv, cst, css)
	       end
	     | f_exp (A.ExpWhile (labexp1, labexp2, _, _, lab, _)) =
	       let
		   val _ = D.printDebug 2 D.AZE "generating constraints for A.ExpWhile"
		   val tv = T.freshtyvar ()
		   val (tv1, cst1, css1) = f_labexp labexp1
		   val (tv2, cst2, css2) = f_labexp labexp2
		   val c1 = E.genCstTyEm (T.consV tv1) (T.constybool lab) lab false
		   val c2 = E.genCstTyEm (T.consV tv) (T.constytuple [] lab) lab false
	       in (tv, E.conscsts (lab, [c1, c2]) (E.uenvcst [cst1, cst2]), E.uenvcss [css1, css2])
	       end
	     | f_exp (A.ExpRaise (labexp, _, lab, _)) =
	       let
		   val _ = D.printDebug 2 D.AZE "generating constraints for A.ExpRaise"
		   val (tv, cst, css) = f_labexp labexp
		   val tv' = T.freshtyvar ()
		   val c   = E.genCstTyEm (T.consV tv) (T.constyexception lab) lab false
	       in (tv', E.conscst (lab, c) cst, css)
	       end
	     | f_exp (A.ExpHandle (labexp, match, _, lab, _)) =
	       let
		   val _ = D.printDebug 2 D.AZE "generating constraints for A.ExpHandle"
		   val (tv1, cst1, css1) = f_labexp labexp
		   val (tvs, cst2, css2) = f_match match
		   val ty  = T.newV ()
		   val tv  = T.freshtyvar ()
		   val cs  = map (fn x => E.genCstTyEm ty (T.consV x) lab false) tvs
		   val c1  = E.genCstTyEm ty (T.consTyArrowTy (T.constyexception lab) (T.consV tv1) lab T.OT) lab false
		   val c2  = E.genCstTyEm (T.consV tv) (T.consV tv1) lab false
	       in (tv, E.conscsts (lab, c1 :: c2 :: cs) (E.uenvcst [cst1, cst2]), E.uenvcss [css1, css2])
	       end
	     | f_exp (A.ExpDots pl) =
	       let
		   val _ = D.printDebug 2 D.AZE "generating constraints for A.ExpDots"
		   val tv  = T.freshtyvar ()
		   val env = f_partlist pl
	       in (tv, E.singcst (L.dummyLab, E.CSTLET env), E.emcss)
	       end

	   (* RETURNS: (Ty.tyvar list, Env.cst, Env.css) *)
	   and f_match (A.Match (mrules, _, _)) =
	       let val (tvs, csts, csss) = unzipThree (map f_mrule mrules)
		   val cst = E.uenvcst csts
		   val css = E.uenvcss csss
	       in (tvs, cst, css)
	       end
	     | f_match (A.MatchDots pl) =
	       let val env = f_partlist pl
	       in ([], E.singcst (L.dummyLab, E.CSTLET env), E.emcss)
	       end

	   (* RETURNS: (Ty.tyvar, Env.cst, Env.css) *)
	   and f_mrule (A.Mrule (labpat, labexp, _, lab, _)) =
	       let val tv  = T.freshtyvar ()
		   val (tv1, vids, cst1, css1) = f_labpat labpat
		   val (tv2, cst2, css2) = f_labexp labexp
		   val env = E.ENVSEQ (E.ENVSEQ (E.ENVCST cst1, E.projVids (E.toMonoVids vids (L.singleton lab))), E.ENVCST cst2)
		   val c   = E.genCstTyEm (T.consV tv) (T.constyarrow tv1 tv2 lab) lab false
		   val cst = E.conscst (L.dummyLab, E.CSTLET env) (E.singcst (lab, c))
		   val css = E.uenvcss [clearRebound css1, css2]
	       in (tv, cst, css)
	       end
	     | f_mrule (A.MruleDots pl) =
	       let val tv  = T.freshtyvar ()
		   val env = f_partlist pl
	       in (tv, E.singcst (L.dummyLab, E.CSTLET env), E.emcss)
	       end

	   (* RETURNS: (int option, Ty.tyvar, Env.cst) *)
	   and f_typevar (A.TypeVar (st, id, _, lab, _)) =
               let val tv = T.freshtyvar ()
		   val a  = E.genAccIeEm (E.consAccId (I.ID (id, lab)) tv (CL.consTYVAR ()) lab) lab
               in (SOME id, tv, E.singcst (lab, E.CSTACC a))
               end
	     | f_typevar A.TypeVarDots =
	       let val tv = T.freshtyvar ()
	       in (NONE, tv, E.emcst)
	       end

	   (*(* RETURNS: (int option, Ty.tyvar, Env.cst) *)
	   and f_labtyvar (A.LabTyVar (tyvar, _, lab, _)) =
	       let val (idop, tv, cst) = f_typevar tyvar
		   val tv' = T.freshtyvar ()
		   val c   = E.genCstTyEm (T.consV tv') (T.consV tv) lab
	       in (idop, tv', E.conscst (lab, c) cst)
	       end
	     | f_labtyvar (A.LabTyVarDots pl) =
	       let val tv = T.freshtyvar ()
	       in (NONE, tv, E.emcst)
	       end*)

	   (* RETURNS: (Ty.tyvar, Env.cst, Env.css) *)
	   and f_labtype (A.LabType (typ, _, lab, _)) =
	       let val (tv, cst, css) = f_type typ
		   val tv' = T.freshtyvar ()
		   val c   = E.genCstTyEm (T.consV tv') (T.consV tv) lab false
	       in (tv', E.conscst (lab, c) cst, css)
	       end
	     | f_labtype (A.LabTypeDots pl) =
	       let val tv  = T.freshtyvar ()
		   val env = f_partlist pl
	       in (tv, E.singcst (L.dummyLab, E.CSTLET env), E.emcss)
	       end

	   (* RETURNS: (((string, Label.label) option, Ty.rowvar), Env.emcst, Env.emcss) *)
	   and f_tyrow (A.TyRow (tylab, labtyp, _, lab, _)) =
	       let val (lv, lop, cst1)  = f_tylab tylab
		   val (tv, cst2, css2) = f_labtype labtyp
		   val rv = T.freshrowvar ()
		   val c  = E.genCstRtEm (T.RV rv) (T.RC (T.LV lv, T.consV tv, lab)) lab
	       in ((lop, rv), E.conscst (lab, c) (E.uenvcst [cst1, cst2]), css2)
	       end
	     | f_tyrow (A.TyRowDots pl) =
	       let val rv  = T.freshrowvar ()
		   val env = f_partlist pl
	       in ((NONE, rv), E.singcst (L.dummyLab, E.CSTLET env), E.emcss)
	       end

	   (* RETURNS: (Ty.tyvar, Env.cst, Env.css) *)
	   and f_type (A.TypeOneVar tyvar) =
	       let val (_, tv, cst) = f_typevar tyvar
	       in (tv, cst, E.emcss)
	       end
	     | f_type (A.TypeArrow (labtyp1, labtyp2, _, lab, _)) =
               let val (tv1, cst1, css1) = f_labtype labtyp1
		   val (tv2, cst2, css2) = f_labtype labtyp2
		   val tv = T.freshtyvar ()
		   val c  = E.genCstTyEm (T.consV tv) (T.constyarrow tv1 tv2 lab) lab false
               in (tv, E.conscst (lab, c) (E.uenvcst [cst1, cst2]), E.uenvcss [css1, css2])
               end
	     | f_type (A.TypeTuple (labtyps, _, lab, _)) =
               let val (tvs, csts, csss) = unzipThree (map f_labtype labtyps)
		   val tv  = T.freshtyvar ()
		   val cst = E.uenvcst csts
		   val css = E.uenvcss csss
		   val c   = E.genCstTyEm (T.consV tv) (T.constytuple tvs lab) lab false
               in (tv, E.conscst (lab, c) cst, css)
               end
	     | f_type (A.TypeRecord (tyrows, _, _, lab, _)) =
               let val (xs, csts, csss) = unzipThree (map f_tyrow tyrows)
		   val (lops, rts) = ListPair.unzip xs
		   val tv   = T.freshtyvar ()
		   val cst  = E.uenvcst csts
		   val css  = E.uenvcss csss
		   val css' = consCSMlab (List.mapPartial (fn x => x) lops)
		   val c    = E.genCstTyEm (T.consV tv) (T.constyrecord rts (T.noflex ()) lab) lab false
               in (tv, E.conscst (lab, c) cst, E.uenvcss [css, css'])
               end
	     | f_type (A.TypeSlRec (tyrows, _, lab, _)) =
               let val (xs, csts, csss) = unzipThree (map f_tyrow tyrows)
		   val (lops, rts) = ListPair.unzip xs
		   val tv   = T.freshtyvar ()
		   val cst  = E.uenvcst csts
		   val css  = E.uenvcss csss
		   val css' = consCSMlab (List.mapPartial (fn x => x) lops)
		   val c    = E.genCstTyEm (T.consV tv) (T.constyrecord rts (T.consflex lab) lab) lab false
               in (tv, E.conscst (lab, c) cst, E.uenvcss [css, css'])
               end
	     | f_type (A.TypeTyCon (typseq, longtycon, _, lab, _)) =
               let val (sv, cst1, css1) = f_typeseq typseq
		   val (tfv, cst2) = f_longtycon longtycon
		   val tv  = T.freshtyvar ()
		   val tv' = T.freshtyvar ()
		   val sv' = T.freshseqvar ()
		   val c1  = E.genCstTfEm (T.TFV tfv) (T.TFC (T.SV sv', T.consV tv', lab)) lab
		   val c2  = E.genCstTyEm (T.consV tv) (T.consV tv') lab false
		   val c3  = E.genCstSqEm (T.SV sv) (T.SV sv') lab
               in (tv, E.conscsts (lab, [c1, c2, c3]) (E.uenvcst [cst1, cst2]), css1)
               end
	     | f_type (A.TypeParen (labtyp, _, _, lab, _)) =
               let val (tv, cst, css) = f_labtype labtyp
		   val tv' = T.freshtyvar ()
		   val c   = E.genCstTyEm (T.consV tv') (T.consV tv) lab false
               in (tv', E.conscst (lab, c) cst, css)
               end
	     | f_type (A.TypeDots pl) =
	       let val tv  = T.freshtyvar ()
		   val env = f_partlist pl
	       in (tv, E.singcst (L.dummyLab, E.CSTLET env), E.emcss)
	       end

	   (* RETURNS: (T.seqvar, E.emcst, E.emcss) *)
	   and f_typeseq (A.TypeSeqOne (typ, _, lab, _)) =
               let val (tv, cst, css) = f_type typ
		   val sv = T.freshseqvar ()
		   val c  = E.genCstSqEm (T.SV sv) (T.SC (T.constuple [tv] lab, T.noflex (), lab)) lab
	       in (sv, E.conscst (lab, c) cst, css)
               end
	     | f_typeseq (A.TypeSeqEm (_, lab, _)) =
	       let val sv = T.freshseqvar ()
		   val c  = E.genCstSqEm (T.SV sv) (T.SC ([], T.noflex (), lab)) lab
	       in (sv, E.singcst (lab, c), E.emcss)
	       end
	     | f_typeseq (A.TypeSeqSeq (labtyps, _, lab, _)) =
               let val (svs, csts, csss) = unzipThree (map f_labtype labtyps)
		   val sv  = T.freshseqvar ()
		   val cst = E.uenvcst csts
		   val css = E.uenvcss csss
		   val c   = E.genCstSqEm (T.SV sv) (T.SC (T.constuple svs lab, T.noflex (), lab)) lab
               in (sv, E.conscst (lab, c) cst, css)
               end
	     | f_typeseq (A.TypeSeqDots pl) =
	       let val sv  = T.freshseqvar ()
		   val env = f_partlist pl
	       in (sv, E.singcst (L.dummyLab, E.CSTLET env), E.emcss)
	       end

	   (*and f_typelist (A.TypeSeqOne (ty, reg, lab, nxt)) =
               let val (env, tv, cs) = f_type ty
	       in (env, ([tv], SOME lab), cs)
               end
	     | f_typelist (A.TypeSeqEm (reg, lab, nxt)) =
	       (E.emenv, ([], SOME lab), E.emcs)
	     | f_typelist (A.TypeSeqSeq (tyl, posl, lab, nxt)) =
               let val envl = map f_labtype tyl
		   val tvl  = getenvTy envl
               in (E.uenvEnv (getenvEnv envl), (tvl, SOME lab), E.uenvcss (getenvCs envl))
               end
	     | f_typelist (A.TypeSeqDots pl) =
	       let val (env, _, cs) = f_partlist pl
	       in (env, ([], NONE), cs)
	       end*)

	   (* RETURNS: (Ty.tyvar, Env.varenv, E.emcst, Env.css) *)
	   and f_labpat (A.LabPat (pat, _, _, lab, _)) =
	       let val (tv, vids, cst, css) = f_pat pat
		   val tv' = T.freshtyvar ()
		   val c   = E.genCstTyEm (T.consV tv') (T.consV tv) lab false
	       in (tv', vids, E.conscst (lab, c) cst, css)
	       end
	     | f_labpat (A.LabPatDots pl) =
	       let val env = f_partlist pl
		   val tv  = T.freshtyvar ()
	       in (tv, E.getVids env, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Ty.tyvar, Env.varenv, E.emcst, E.emcss) *)
	   and f_identty (A.IdentTyId ident) = f_identpat ident
	     | f_identty (A.IdentTyTy (labid, labtyp, _, lab, _)) =
	       let val (tv1, vids, cst1, css1) = f_labid labid
		   val (tv2, cst2, css2) = f_labtype labtyp
		   val tv = T.freshtyvar ()
		   val c1 = E.genCstTyEm (T.consV tv) (T.consV tv1) lab false
		   val c2 = E.genCstTyEm (T.consV tv) (T.consV tv2) lab false
	       in (tv, vids, E.conscsts (lab, [c1, c2]) (E.uenvcst [cst1, cst2]), E.uenvcss [css1, css2])
	       end
	     | f_identty (A.IdentTyDots pl) =
	       let val env = f_partlist pl
		   val tv  = T.freshtyvar ()
	       in (tv, E.getVids env, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Ty.tyvar, Env.varenv, E.emcst, E.emcss) *)
	   and f_labidty (A.LabIdTy (identty, _, lab, _)) =
	       let val (tv, vids, cst, css) = f_identty identty
		   val tv' = T.freshtyvar ()
		   val c   = E.genCstTyEm (T.consV tv') (T.consV tv) lab false
	       in (tv', vids, E.conscst (lab, c) cst, css)
	       end
	     | f_labidty (A.LabIdTyDots pl) =
	       let val env = f_partlist pl
		   val tv  = T.freshtyvar ()
	       in (tv, E.getVids env, E.emcst, E.emcss)
	       end

	   (* RETURNS: (((string, Label.label) option, Ty.rowvar option, Label.label option), Env.varenv, Env.emcst, Env.emcss) *)
	   and f_patrow (A.PatRow (tylab, labpat, _, _, lab, _)) =
	       let val (lv, lop, cst1) = f_tylab tylab
		   val (tv, vids, cst2, css2) = f_labpat labpat
		   val rv = T.freshrowvar ()
		   val c  = E.genCstRtEm (T.RV rv) (T.RC (T.LV lv, T.consV tv, lab)) lab
	       in ((lop, SOME rv, NONE), vids, E.conscst (lab, c) (E.uenvcst [cst1, cst2]), css2)
	       end
	     | f_patrow (A.PatRowId (identty, _)) =
	       let val (tv, vids, cst, css) = f_identty identty
		   val rv   = T.freshrowvar ()
		   val llop = A.getlabstIdentTy identty
		   val cst' = case llop of
				 NONE => E.emcst
			       | SOME (l1, l2, s) =>
				 let val lv = T.freshlabvar ()
				     val c1 = E.genCstLtEm (T.LV lv) (T.LC (s, l1)) l1
				     val c2 = E.genCstRtEm (T.RV rv) (T.RC (T.LV lv, T.consV tv, l2)) l2
				 in E.conscst (l1, c1) (E.singcst (l2, c2))
				 end
		   val lop  = case llop of NONE => NONE | SOME (_, l, s) => SOME (s, l)
	       in ((lop, SOME rv, NONE), vids, E.uenvcst [cst, cst'], css)
	       end
	     | f_patrow (A.PatRowAs (labidty, labpat, _, lab, _)) =
	       let val (tv1, vids1, cst1, css1) = f_labidty labidty
		   val (tv2, vids2, cst2, css2) = f_labpat labpat
		   val tv   = T.freshtyvar ()
		   val rv   = T.freshrowvar ()
		   val c1   = E.genCstTyEm (T.consV tv) (T.consV tv1) lab false
		   val c2   = E.genCstTyEm (T.consV tv) (T.consV tv2) lab false
		   val labs = L.cons lab (A.getlabelsLabIdTy labidty)
		   val llop = A.getlabstLabIdTy labidty
		   val cst  = case llop of
				 NONE => E.emcst
			       | SOME (l1, l2, s) =>
				 let val lv = T.freshlabvar ()
				     val c1 = E.genCstLtEm (T.LV lv) (T.LC (s, l1)) l1
				     val c2 = E.genCstRtEm (T.RV rv) (T.RC (T.LV lv, T.consV tv, l2)) l2
				 in E.conscst (l1, c1) (E.singcst (l2, c2))
				 end
		   val lop' = case llop of NONE => NONE | SOME (_, l, s) => SOME (s, l)
		   val cst  = E.conscsts (lab, [c1, c2]) (E.uenvcst [cst1, cst2, cst])
		   val css  = E.uenvcss [css1, css2]
	       in ((lop', SOME rv, NONE), E.uenv [E.toRECVids vids1 labs, vids2], cst, css)
	       end
	     | f_patrow (A.PatRowWild (r, l, n)) = ((NONE, NONE, SOME l), E.emvar, E.emcst, E.emcss)
	     | f_patrow (A.PatRowDots pl) =
	       let val env = f_partlist pl
	       in ((NONE, NONE, NONE), E.getVids env, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Ty.tyvar, Env.varenv, Env.cst, Env.css) *)
	   and f_atpat (A.AtPatWild _) = (T.freshtyvar (), E.emvar, E.emcst, E.emcss)
	     | f_atpat (A.AtPatId longid) = f_longidpat longid
	     | f_atpat (A.AtPatScon scon) =
               let val (tv, cst, css) = f_scon scon
		   val css' = case A.isPatScon scon of
				  NONE     => E.emcss
				| SOME lab => E.singcss (E.CSSREAL (L.singleton lab))
               in (tv, E.emvar, cst, E.uenvcss [css, css'])
               end
	     | f_atpat (A.AtPatTuple (labpats, _, lab, _)) =
               let val tv   = T.freshtyvar ()
		   val (tvs, vidss, csts, csss) = unzipFour (map f_labpat labpats)
		   val vids = E.uenv    vidss
		   val cst  = E.uenvcst csts
		   val css  = E.uenvcss csss
		   val c    = E.genCstTyEm (T.consV tv) (T.constytuple tvs lab) lab false
               in (tv, vids, E.conscst (lab, c) cst, css)
               end
	     | f_atpat (A.AtPatRecord (patrows, _, _, lab, _)) =
               let val tv   = T.freshtyvar ()
		   val (xs, vidss, csts, csss) = unzipFour (map f_patrow patrows)
		   val (lops, rvs, flexs) = unzipThree xs
		   val vids = E.uenv vidss
		   val cst  = E.uenvcst csts
		   val css  = E.uenvcss csss
		   val css' = consCSMlab (List.mapPartial (fn x => x) lops)
		   val rvs' = List.mapPartial (fn x => x) rvs
		   val flex = case List.mapPartial (fn x => x) flexs of
				  []  => T.noflex ()
				| [x] => T.consflex x
				| _   => raise EH.DeadBranch "Only one flex per record"
		   val c    = E.genCstTyEm (T.consV tv) (T.constyrecord rvs' flex lab) lab false
               in (tv, vids, E.conscst (lab, c) cst, E.uenvcss [css, css'])
               end
	     | f_atpat (A.AtPatParen (labpat, _, _, lab, _)) =
               let val (tv, vids, cst, css) = f_labpat labpat
		   val tv' = T.freshtyvar ()
		   val c   = E.genCstTyEm (T.consV tv') (T.consV tv) lab false
               in (tv', vids, E.conscst (lab, c) cst, css)
               end
	     | f_atpat (A.AtPatList (labpats, _, lab, _)) =
               let val tv   = T.freshtyvar ()
		   val tv'  = T.freshtyvar ()
		   val (tvs, vidss, csts, csss) = unzipFour (map f_labpat labpats)
		   val vids = E.uenv vidss
		   val cst  = E.uenvcst csts
		   val css  = E.uenvcss csss
		   val cs   = map (fn x => E.genCstTyEm (T.consV tv') (T.consV x) lab false) tvs
		   val c    = E.genCstTyEm (T.consV tv) (T.constylist tv' lab) lab false
	       in (tv, vids, E.conscsts (lab, c :: cs) cst, css)
               end
	     | f_atpat (A.AtPatOr (labpats, _, lab, _)) =
               let val (tvs, vidss, csts, csss) = unzipFour (map f_labpat labpats)
		   val vids = E.uenv vidss (* do something else here: treatAtPatOr envs *)
		   val cst  = E.uenvcst csts
		   val css  = E.uenvcss csss
		   val tv   = T.freshtyvar ()
		   val cs   = map (fn x => E.genCstTyEm (T.consV tv) (T.consV x) lab false) tvs
	       in (tv, vids, E.conscsts (lab, cs) cst, css)
               end
	     | f_atpat (A.AtPatDots pl) =
	       let val env = f_partlist pl
		   val tv  = T.freshtyvar ()
	       in (tv, E.getVids env, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Ty.tyvar, Env.varenv, Env.cst, Env.css) *)
	   and f_pat (A.PatAtPat atpat) = f_atpat atpat
	     | f_pat (A.PatApp (longid, atpat, _, _, lab, _)) =
               let val (tv1, vids1, cst1, css1) = f_atpat atpat
		   val (tv2, lab', cl, cst2) = f_longidexp longid
		   val tv  = T.freshtyvar ()
		   val c1  = E.genCstTyEm (T.consV tv2) (T.constyarrow' tv1 tv lab T.PA) lab false
		   val c2  = E.genCstClEm cl (CL.consCO1 ()) lab
		   val cst = if A.isLongIdent longid
			     then E.singcst (lab', E.genCstClEm cl (CL.consCON ()) lab')
			     else E.emcst
               in (tv, vids1, E.conscsts (lab, [c1, c2]) (E.uenvcst [cst1, cst2, cst]), css1)
               end
	     | f_pat (A.PatConsList (v, labpat1, labpat2, reg, lab, next)) =
	       if benv
	       then f_pat (A.PatOp ("::", v, labpat1, labpat2, reg, lab, next))
	       else let val (tv1, vids1, cst1, css1) = f_labpat labpat1
			val (tv2, vids2, cst2, css2) = f_labpat labpat2
			val tv  = T.freshtyvar ()
			val c1  = E.genCstTyEm (T.consV tv) (T.constylist tv1 lab) lab false
			val c2  = E.genCstTyEm (T.consV tv) (T.consV tv2) lab false
			val cst = E.conscsts (lab, [c1, c2]) (E.uenvcst [cst1, cst2])
			val css = E.uenvcss [css1, css2]
		    in (tv, E.uenv [vids1, vids2], cst, css)
		    end
	     | f_pat (A.PatOp (st, v, labpat1, labpat2, _, lab, _)) =
	       let val (tv1, vids1, cst1, css1) = f_labpat labpat1
		   val (tv2, vids2, cst2, css2) = f_labpat labpat2
		   val ty  = T.newV ()
		   val ti  = T.constytuple [tv1, tv2] lab
		   val tvo = T.freshtyvar ()
		   val c   = E.genCstTyEm ty (T.consTyArrowTy ti (T.consV tvo) lab T.OT) lab false
		   val a   = E.genAccIvEm (E.consAccId (I.ID (v, lab)) ty (CL.consDA1 ()) lab) lab
		   val cst = E.conscsts (lab, [c, E.CSTACC a]) (E.uenvcst [cst1, cst2])
		   val css = E.uenvcss [css1, css2]
	       in (tvo, E.uenv [vids1, vids2], cst, css)
	       end
	     | f_pat (A.PatTyped (labpat, labtyp, _, lab, _)) =
	       let val tv = T.freshtyvar ()
		   val (tv1, vids, cst1, css1) = f_labpat labpat
		   val (tv2, cst2, css2) = f_labtype labtyp
		   val c1  = E.genCstTyEm (T.consV tv) (T.consV tv1) lab false
		   val c2  = E.genCstTyEm (T.consV tv) (T.consV tv2) lab false
		   val cst = E.conscsts (lab, [c1, c2]) (E.uenvcst [cst1, cst2])
		   val css = E.uenvcss [css1, css2]
	       in (tv, vids, cst, css)
	       end
	     | f_pat (A.PatAs (labidty, labpat, _, lab, _)) = (* For this one and PatRowAs we need to constrain the envs to a value variables. *)
	       let val (tv1, vids1, cst1, css1) = f_labidty labidty
		   val (tv2, vids2, cst2, css2) = f_labpat labpat
		   (*val clv  = CL.newCl ()*)
		   val tv   = T.freshtyvar ()
		   val c1   = E.genCstTyEm (T.consV tv) (T.consV tv1) lab false
		   val c2   = E.genCstTyEm (T.consV tv) (T.consV tv2) lab false
		   (*val c3   = E.genCstClEm clv (CL.consPAT ()) lab*)
		   val labs = L.cons lab (A.getlabelsLabIdTy labidty)
		   val cst  = E.conscsts (lab, [c1, c2(*, c3*)]) (E.uenvcst [cst1, cst2])
		   val css  = E.uenvcss [css1, css2]
		   (*val vids = E.toCLSVids vids1 clv labs*)
		   val vids = E.toPATVids vids1 labs
	       in (tv, E.uenv [vids, vids2], cst, css)
	       end
	     | f_pat (A.PatDots pl) =
	       let val tv  = T.freshtyvar ()
		   val env = f_partlist pl
	       in (tv, E.getVids env, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Ty.tyvar, Env.varenv, Env.cst, Env.css) *)
	   and f_conbind (A.ConBind (ident, _)) =
               let val (tv, cons, cst, css) = f_identpat ident
               in (tv, E.toDA0Vids cons L.empty, cst, css)
               end
	     | f_conbind (A.ConBindOf (labid, labtyp, _, lab, _)) =
               let val (tv1, cons, cst1, css1) = f_labid labid
		   val (tv2, cst2, css2) = f_labtype labtyp
		   val (lab1, lab2) = Option.getOpt (A.getLabelsIdLabId labid, (lab, lab))
		   val clv1 = CL.newCl ()
		   val clv2 = CL.newCl ()
		   val tv   = T.freshtyvar ()
		   val c1   = E.genCstTyEm (T.consV tv1) (T.constyarrow' tv2 tv lab (T.DE I.dummyId)) lab false
		   val c2   = E.genCstClEm clv2 (CL.consDA1 ()) lab
		   val c3   = E.genCstClEm clv1 clv2 lab1
		   val c4   = E.genCstClEm clv1 (CL.consDAT ()) lab2
		   val cst  = E.uenvcst [cst1, cst2]
		   val css  = E.uenvcss [css1, css2]
		   val cst' = E.conscsts (lab, [c1, c2]) (E.conscst (lab1, c3) (E.conscst (lab2, c4) cst))
		   (*(2010-06-24)I don't believe labid's label is necessary here, but
		    * this is what the old implementation had recorded in the database. *)
		   (*val labs = L.cons lab (A.getLabelLabId labid)*)
		   (*val cons = E.toDA1Vids cons labs*)
	       (* We actually don't need labs to constrain cons to be a DAT. *)
	       in (tv, E.toCLSVids cons clv1 L.empty, cst', css)
               end
	     | f_conbind (A.ConBindNoOf (ident, _)) =
               let val (tv, cons, cst, css) = f_identpat ident
		   val tv' = T.freshtyvar ()
	       in (tv', E.toDATVids cons L.empty, cst, css)
               end
	     | f_conbind (A.ConBindDots pl) =
	       let val env = f_partlist pl
		   val tv  = T.freshtyvar ()
	       in (tv, E.getVids env, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Ty.tyvar list, Env.varenv, Env.cst, Env.css) *)
	   and f_conbindseq (A.ConBindSeq conbinds) =
               let val (tvs, conss, csts, csss) = unzipFour (map f_conbind conbinds)
		   val cons = E.uenv conss
		   val cst  = E.uenvcst csts
		   val css  = E.uenvcss csss
               in (tvs, cons, cst, css)
               end
	     | f_conbindseq (A.ConBindSeqDots pl) =
	       let val env = f_partlist pl
	       in ([], E.getVids env, E.emcst, E.emcss)
	       end

	   (*and f_tyvarsq (A.TypeVar (_, n, _, lab, _)) = [(n, lab, lab)]
	     | f_tyvarsq A.TypeVarDots = []

	   and f_labtyvarsq (A.LabTyVar (tv, _, lab, _)) =
	       (case f_tyvarsq tv of
		    []          => []
		  | [(n, l, _)] => [(n, l, lab)]
		  | _           => raise EH.DeadBranch "")
	     | f_labtyvarsq (A.LabTyVarDots tvl) = [] (* this is not the right thing to do *)*)

	   (* RETURNS: (Ty.tyvar, Ty.tvenv, Env.cst) *)
	   and f_typevarbind (A.TypeVar (_, n, _, lab, _)) =
	       let val tv1  = T.freshtyvar ()
		   val tv2  = T.freshtyvar ()
		   val tyvs = E.singenv (n, [E.consBindMono n (tv1, true) (CL.consTYVAR ()) lab])
		   val c    = E.genCstTyEm (T.consV tv1) (T.V (tv2, SOME (n, lab), T.POLY)) lab false
	       in (tv2, tyvs, E.singcst (lab, c))
	       end
	     | f_typevarbind A.TypeVarDots = (T.freshtyvar (), E.emtv, E.emcst)

	   (* RETURNS: (Ty.tyvar, Ty.tvenv, Env.cst) *)
	   and f_labtypevarbind (A.LabTyVar (tyvar, _, lab, _)) =
	       let val (tv, tyvs, cst) = f_typevarbind tyvar
		   val tv' = T.freshtyvar ()
		   val c   = E.genCstTyEm (T.consV tv) (T.consV tv') lab false
	       in (tv', tyvs, E.conscst (lab, c) cst)
	       end
	     | f_labtypevarbind (A.LabTyVarDots tyvars) =
	       (T.freshtyvar (), E.emtv, E.emcst)

	   (* RETURNS: (Ty.seqvar, Ty.tvenv, Env.cst) *)
	   and f_typevarseq (A.TyVarSeqOne (tyvar, _, lab, _)) =
               let val (tv, tyvs, cst) = f_typevarbind tyvar
		   val sv = T.freshseqvar ()
		   val sq = T.SC (T.constuple [tv] lab, T.noflex (), lab)
		   val c  = E.genCstSqEm (T.SV sv) sq lab
               in (sv, tyvs, E.conscst (lab, c) cst)
               end
	     | f_typevarseq (A.TyVarSeqEm (_, lab, _)) =
               let val sv = T.freshseqvar ()
		   val sq = T.SC ([], T.noflex (), lab)
		   val c  = E.genCstSqEm (T.SV sv) sq lab
               in (sv, E.emtv, E.singcst (lab, c))
               end
	     | f_typevarseq (A.TyVarSeqSeq (typevars, _, lab, _)) =
               let val (tvs, tyvss, csts) = unzipThree (map f_labtypevarbind typevars)
		   val tyvs = E.uenv tyvss
		   val cst  = E.uenvcst csts
		   val sv   = T.freshseqvar ()
		   val sq   = T.SC (T.constuple tvs lab, T.noflex (), lab)
		   val c    = E.genCstSqEm (T.SV sv) sq lab
               in (sv, tyvs, E.conscst (lab, c) cst)
               end
	     | f_typevarseq (A.TyVarSeqDots tyvars) =
	       let val (_, tyvss, csts) = unzipThree (map f_typevarbind tyvars)
		   val tyvs = E.uenv tyvss
		   val cst  = E.uenvcst csts
		   val sv   = T.freshseqvar ()
               in (sv, tyvs, cst)
	       end

	   (*(* we add "db" (datbind) because for now it's only used in f_datname *)
	   and f_tycondb (A.TyCon (s, v, _, l, _)) =
	       let val tv  = T.freshtyvar ()
		   val tv' = T.freshtyvar ()
		   val tn  = if benv andalso getBasis () then getTyNameString s else T.freshtyname ()
		   val sv  = T.freshseqvar ()
		   val c   = E.genCstTyEm (T.consV tv') (T.C (T.NC (tn, T.DE, l), T.SV sv, l)) l (* the type name has to be here because of the label coming along with the type name *)
		   val idT = E.singenv (v, [E.consBindPoly v (T.consV tv) (CL.consDAT ()) l])
	       in (idT, (tv, tv', sv, SOME (v, tn)), E.singcsSem (l, c))
	       end
	     | f_tycondb A.TyConDots =
	       let val tv  = T.freshtyvar ()
		   val tv' = T.freshtyvar ()
		   val sv  = T.freshseqvar ()
	       in (E.emtyp, (tv, tv', sv, NONE), E.emcs)
	       end*)

	   (*and f_datnamedb (A.DatName (tvs, tn, _, _)) =
	       let val (asc, sv, cs1) = f_typevarseq tvs
		   val (idT, (tv, tv', sv', tnop), cs2) = f_tycondb tn
	       in ((asc, idT), (tv, tv', sv', sv, tnop), E.uenvcs cs1 cs2)
	       end
	     | f_datnamedb A.DatNameDots =
	       let val tv  = T.freshtyvar ()
		   val tv' = T.freshtyvar ()
		   val sv  = T.freshseqvar ()
		   val sv' = T.freshseqvar ()
	       in (([], E.emtyp), (tv, tv', sv, sv', NONE), E.emcs)
	       end*)

	   (* RETURNS: (Ty.typenv, Ty.varenv * Id.idl option, Env.cst, Env.cst, Env.css) *)
	   and f_datbind (A.DatBind (datname, conbindseq, _, lab, _)) =
               let val (tvs, cons, cst1, css1) = f_conbindseq conbindseq
		   val (s, v, tv, sv1, sv2, typs, tyvs, cst2, css2) = f_datname datname
		   val tn   = if benv andalso getBasis () then getTyNameString s else T.freshtyname ()
		   val id   = Option.getOpt (Option.map (fn (i, _) => i) v, I.dummyId)
		   val lab' = Option.getOpt (Option.map (fn (_, l) => l) v, lab)
		   (* (2010-06-10)We do that so that for datatypes, the end points will not be
		    * the equal signs but the datatype names, in the case of constructor clashes.*)
		   val c1   = E.genCstSqEm (T.SV sv1) (T.SV sv2) lab
		   val c2   = E.genCstTyEm (T.consV tv) (T.C (T.NC (tn, T.DE id, lab'), T.SV sv1, lab')) lab' false
		   val c2'  = E.genCstTyEm (T.consV tv) (T.consTyNameVar lab) lab false
		   (*(2010-06-21)c2' is similar to c2 but weaker.  It just constrains tv to be of the
		    * form T.C.*)
		   val c3   = E.CSTLET (E.ENVSEQ (E.projTyvs tyvs, E.ENVCST cst1))
		   val cs   = map (fn x => E.genCstTyEm (T.consV x) (T.consV tv) lab false) tvs
		   val cst  = E.conscsts (lab, [c1, c2']) (E.conscst (lab', c2) cst2)
		   val cst' = E.conscsts (lab, cs) (E.singcst (L.dummyLab, c3))
		   val css3 = checkTyVarInc (A.gettyvarDatName datname) (A.getlabDatName datname) (A.gettyvarConbindseq conbindseq)
		   val css  = E.uenvcss [css1, css2, css3]
	       in (E.toTYCONTyps typs E.emvar false (L.singleton lab), (cons, v), cst, cst', css)
               end
	     | f_datbind (A.DatBindDots pl) =
	       let val env = f_partlist pl
	       in (E.getTyps env, (E.getVids env, NONE), E.emcst, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Ty.typenv, (Ty.varenv * Id.idl option) list, Env.cst, Env.css) *)
	   and f_datbindseq (A.DatBindSeq (datbinds, _, _)) =
               let val (typss, conss, csts1, csts2, csss) = unzipFive (map f_datbind datbinds)
		   val typs = E.uenv typss
		   val cst1 = E.uenvcst csts1
		   val cst2 = E.uenvcst csts2
		   val css  = E.uenvcss csss
		   val css1 = consCSM typs
		   val css2 = consCSM (E.uenv (map (fn (cons, _) => cons) conss))
               in (typs, conss, cst1, cst2, E.uenvcss [css, css1, css2])
               end
	     | f_datbindseq (A.DatBindSeqDots pl) =
	       let val env = f_partlist pl
	       in (E.getTyps env, [(E.getVids env, NONE)], E.emcst, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Env.varenv, Env.cst, Env.cst, Env.css) *)
	   and f_valbindcore (A.ValBindCore (labpat, labexp, _, lab, _)) =
               let val (tv1, vids, cst1, css1) = f_labpat labpat
		   val (tv2, cst2, css2) = f_labexp labexp
		   val vids' = E.closeVids vids (V.nonexpLabExp labexp)
		   val c     = E.genCstTyEm (T.consV tv1) (T.consV tv2) lab false
		   val cst   = E.conscst (lab, c) (cst2)
		   val css   = E.uenvcss [clearRebound css1, css2]
	       in (vids', cst1, cst, css)
               end
	     | f_valbindcore (A.ValBindCoreDots pl) =
	       let val env = f_partlist pl
	       in (E.getVids env, E.emcst, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Env.varenv, Env.cst, Env.cst, Env.css) *)
	   and f_valbindseq (A.ValBindSeq (valbindcores, _, _)) =
	       let val (vidss, csts1, csts2, csss) = unzipFour (map f_valbindcore valbindcores)
		   val vids = E.uenv vidss
		   val cst1 = E.uenvcst csts1
		   val cst2 = E.uenvcst csts2
		   val css  = E.uenvcss csss
	       in (vids, cst1, cst2, css)
	       end
	     | f_valbindseq (A.ValBindSeqDots pl) =
	       let val env = f_partlist pl
	       in (E.getVids env, E.emcst, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_valbind (A.ValBindRec (valbindseq, _, lab, _)) =
	       let val (vids, cst1, cst2, css) = f_valbindseq valbindseq
		   val labs  = L.singleton lab
		   val vids' = E.toMonoVids (E.toRECVids vids labs) labs
		   val css1  = consCSM vids
		   val css2  = map (fn x => E.CSSFREC (L.cons lab x)) (A.isExpFnValBindSeq valbindseq)
		   val env   = E.ENVSEQ (E.ENVCST cst1, E.ENVSEQ (E.projVids vids', E.ENVCST cst2))
	       in (env, E.uenvcss [css, css1, css2])
	       end
	     | f_valbind (A.ValBind valbindseq) =
	       let val (vids, cst1, cst2, css) = f_valbindseq valbindseq
		   val env = E.ENVSEQ (E.ENVCST (E.uenvcst [cst1, cst2]), E.projVids vids)
	       in (env, css)
	       end
	     | f_valbind (A.ValBindDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emcss)
	       end

	   (* RETURNS: (Ty.tyvar, Env.varenv, Env.cst, Env.css) *)
	   and f_labatpat (A.LabAtPat (atpat, _, lab, _)) =
	       let val (tv, vids, cst, css) = f_atpat atpat
		   val tv' = T.freshtyvar ()
		   val c   = E.genCstTyEm (T.consV tv) (T.consV tv') lab false
	       in (tv', vids, E.conscst (lab, c) cst, css)
	       end
	     | f_labatpat (A.LabAtPatDots pl) =
	       let val tv  = T.freshtyvar ()
		   val env = f_partlist pl
	       in (tv, E.getVids env, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Ty.tyvar, Env.varenv, Env.varenv, Env.cst, Env.css) *)
	   and f_fmatch (A.FMatchId (ident, _, _)) =
	       let val (tv, vids, cst, css) = f_identpat ident
	       in (tv, vids, E.emvar, cst, css)
	       end
	     | f_fmatch (A.FMatchApp (fmatch, labatpat, _, _, lab, _)) =
	       let val (tv1, vids, vids1, cst1, css1) = f_fmatch fmatch
		   val (tv2, vids2, cst2, css2) = f_labatpat labatpat
		   val tv  = T.freshtyvar ()
		   val c   = E.genCstTyEm (T.consV tv1) (T.constyarrow tv2 tv lab) lab false
		   val cst = E.conscst (lab, c) (E.uenvcst [cst1, cst2])
		   val css = E.uenvcss [css1, clearRebound css2]
	       in (tv, vids, E.uenv [vids1, vids2], cst, css)
	       end
	     | f_fmatch (A.FMatchSlApp (fmatch, labatpat, _)) =
	       let val (tv1, vids, vids1, cst1, css1) = f_fmatch fmatch
		   val (tv2, vids2, cst2, css2) = f_labatpat labatpat
		   val tv  = T.freshtyvar ()
		   val cst = E.uenvcst [cst1, cst2]
		   val css = E.uenvcss [css1, clearRebound css2]
	       in (tv, vids, E.uenv [vids1, vids2], cst, css)
	       end
	     | f_fmatch (A.FMatchNoApp (fmatch, _)) =
	       let val (_, vids, vids1, cst1, css1) = f_fmatch fmatch
		   val tv = T.freshtyvar ()
	       in (tv, vids, vids1, cst1, css1)
	       end
	     | f_fmatch A.FMatchDots =
	       let val tv = T.freshtyvar ()
	       in (tv, E.emvar, E.emvar, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Ty.tyvar, Env.varenv, Env.varenv, Env.cst, Env.css) *)
	   and f_labfmatch (A.LabFMatch (fmatch, _, lab, _)) =
	       let val (tv, vids, vids', cst, css) = f_fmatch fmatch
		   val tv' = T.freshtyvar ()
		   val c   = E.genCstTyEm (T.consV tv) (T.consV tv') lab false
	       in (tv', vids, vids', E.conscst (lab, c) cst, css)
	       end
	     | f_labfmatch (A.LabFMatchSl (fmatch, _)) =
	       let val (tv, vids, vids', cst, css) = f_fmatch fmatch
		   val tv' = T.freshtyvar ()
	       in (tv', vids, vids', cst, css)
	       end
	     | f_labfmatch A.LabFMatchDots =
	       let val tv = T.freshtyvar ()
	       in (tv, E.emvar, E.emvar, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Ty.tyvar, Env.varenv, Env.varenv, Env.cst, Env.css) *)
	   and f_fmatchty (A.FMatchT fmatch) = f_labfmatch fmatch
	     | f_fmatchty (A.FMatchTTy (fmatch, labtyp, _, lab, _)) =
	       let val (tv1, vids1, vids1', cst1, css1) = f_labfmatch fmatch
		   val (tv2, cst2, css2) = f_labtype labtyp
		   val tv  = T.freshtyvar ()
		   val c1  = E.genCstTyEm (T.consV tv) (T.consV tv1) lab false
		   val c2  = E.genCstTyEm (T.consV tv) (T.consV tv2) lab false
		   val cst = E.conscsts (lab, [c1, c2]) (E.uenvcst [cst1, cst2])
		   val css = E.uenvcss [css1, css2]
	       in (tv, vids1, vids1', cst, css)
	       end
	     | f_fmatchty A.FMatchTDots =
	       let val tv = T.freshtyvar ()
	       in (tv, E.emvar, E.emvar, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Env.varenv, Env.cst, Env.css) *)
	   and f_fvalbindcore (A.FValBindCore (fmatch, labexp, _, lab, _)) =
	       let val (tv1, vids, vids', cst1, css1) = f_fmatchty fmatch
		   val (tv2, cst2, css2) = f_labexp labexp
		   val labs = L.singleton lab
		   val c1   = E.genCstTyEm (T.consV tv1) (T.consV tv2) lab false
		   val env  = E.ENVSEQ (E.ENVCST cst1, E.projVids (E.toMonoVids vids' L.empty))
		   val c2   = E.CSTLET (E.ENVSEQ (env, E.ENVCST cst2))
		   val cst  = E.conscst (lab, c1) (E.singcst (L.dummyLab, c2))
		   val css  = E.uenvcss [css1, css2]
		   val vids = E.toRECVids (E.toMonoVids vids L.empty) L.empty
	       (*(2010-06-11)NOTE: We've got a lot of L.empty here!?*)
	       in (vids, cst, css)
	       end
	     | f_fvalbindcore (A.FVBCoreDots pl) =
	       let val env = f_partlist pl
	       in (E.getVids env, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Env.varenv, Env.cst, Env.css) *)
	   and f_fvalbindone (A.FValBindOne (fvalbindcores, _, lab, _)) =
	       let val (vidss, csts, csss) = unzipThree (map f_fvalbindcore fvalbindcores)
		   val vids = E.uenv vidss
		   val cst  = E.uenvcst csts
		   val css  = E.uenvcss csss
		   val cst' = E.allEqualVids vids
		   val css' = sameNameFun vids
	       in (vids, E.uenvcst [cst, cst'], E.uenvcss [css, css'])
	       end
	     | f_fvalbindone (A.FVBOneDots pl) =
	       let val env = f_partlist pl
	       in (E.getVids env, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_fvalbind (A.FValBind (fvalbindones, _, _)) =
	       let val (vidss, csts, csss) = unzipThree (map f_fvalbindone fvalbindones)
		   val vids = E.uenv vidss
		   val cst  = E.uenvcst csts
		   val css  = E.uenvcss csss
		   val env  = E.ENVSEQ (E.projVids vids, E.ENVCST cst)
		   val css1 = createDiffNbArgFuns fvalbindones
		   val css2 = consCSMfval vidss
	       in (env, E.uenvcss [css, css1, css2])
	       end
	     | f_fvalbind (A.FValBindDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emcss)
	       end

	   (* RETURNS: (string, Id.id option, Ty.tyvar, Ty.seqvar, Env.typenv, Env.cst) *)
	   and f_tyconbind (A.TyCon (s, v, _, lab, _)) =
	       let val tv   = T.freshtyvar  ()
		   val sv   = T.freshseqvar ()
		   val tfv  = T.freshtyfvar ()
		   val typs = E.singenv (v, [E.consBindPoly v (T.TFV tfv, E.TYP, ref (E.emvar, false)) (CL.consTYCON ()) lab])
		   (*(2010-06-10)NOTE: the false above is because we still don't know v's constructors.*)
		   val c    = E.genCstTfEm (T.TFV tfv) (T.TFC (T.SV sv, T.consV tv, lab)) lab
	       in (s, SOME (v, lab), tv, sv, typs, E.singcst (lab, c))
	       end
	     | f_tyconbind A.TyConDots =
	       let val tv = T.freshtyvar  ()
		   val sv = T.freshseqvar ()
	       in ("", NONE, tv, sv, E.emtyp, E.emcst)
	       end

	   (* RETURNS: (string, Id.id option, Ty.tyvar, Ty.seqvar, Ty.seqvar, Env.typenv, Env.tvenv, Env.cst, Env.css) *)
	   and f_datname (A.DatName (typvarseq, tycon, _, _)) =
	       let val (sv1, tyvs, cst1) = f_typevarseq typvarseq
		   val (s, v, tv2, sv2, typs, cst2) = f_tyconbind tycon
		   val css = consCSM tyvs
	       (* NOTE: sv1 and sv2 have to be equal *)
	       in (s, v, tv2, sv2, sv1, typs, tyvs, E.uenvcst [cst1, cst2], css)
	       end
	     | f_datname A.DatNameDots =
	       let val tv  = T.freshtyvar ()
		   val sv1 = T.freshseqvar ()
		   val sv2 = T.freshseqvar ()
	       in ("", NONE, tv, sv1, sv2, E.emtyp, E.emtv, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Env.typenv, Env.cst, Env.css) *)
	   and f_typbind (A.TypBind (datname, labtyp, _, lab, _)) =
	       let
		   val _   = D.printDebug 2 D.AZE "generating constraints for A.TypBind"
		   val (_, _, tv1, sv1, sv1', typs, tyvs, cst1, css1) = f_datname datname
		   val (tv2, cst2, css2) = f_labtype labtyp
		   val c1  = E.genCstTyEm (T.consV tv1) (T.consV tv2) lab false
		   val c2  = E.genCstSqEm (T.SV sv1) (T.SV sv1') lab
		   val c3  = E.CSTLET (E.ENVSEQ (E.projTyvs tyvs, E.ENVCST cst2))
		   val cst = E.conscsts (lab, [c1, c2]) (E.conscst (L.dummyLab, c3) cst1)
		   val css = E.uenvcss [css1, css2]
               in (typs, cst, css)
               end
	     | f_typbind (A.TypBindDots pl) =
	       let val env = f_partlist pl
	       in (E.getTyps env, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Env.typenv, Env.cst, Env.css) *)
	   and f_typbindseq (A.TypBindSeq (typbinds, _, _)) =
               let val (typss, csts, csss) = unzipThree (map f_typbind typbinds)
		   val typs = E.uenv typss
		   val cst  = E.uenvcst csts
		   val css  = E.uenvcss csss
		   val css1 = consCSM typs
               in (typs, cst, E.uenvcss [css, css1])
	       end
	     | f_typbindseq (A.TypBindSeqDots pl) =
	       let val env = f_partlist pl
	       in (E.getTyps env, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Env.varenv, Env.cst, Env.css) *)
	   and f_exbind (A.ExBind (ident, lab, _)) =
	       let val (tv, cons, cst, css) = f_identpat ident
		   val c = E.genCstTyEm (T.consV tv) (T.constyexception' lab (T.DE I.dummyId)) lab false
               in (E.toEX0Vids cons (L.singleton lab), E.conscst (lab, c) cst, css)
	       end
	     | f_exbind (A.ExBindOf (labid, labtyp, _, lab, _)) =
               let val (tv1, cst1, css1) = f_labtype labtyp
		   val (tv2, cons, cst2, css2) = f_labid labid
		   val (lab1, lab2) = Option.getOpt (A.getLabelsIdLabId labid, (lab, lab))
		   val clv1 = CL.newCl ()
		   val clv2 = CL.newCl ()
		   val ty   = T.constyexception' lab (T.DE I.dummyId)
		   val c1   = E.genCstTyEm (T.consV tv2) (T.consTyArrowTy (T.consV tv1) ty lab (T.DE I.dummyId)) lab false
		   val c2   = E.genCstClEm clv2 (CL.consEX1 ()) lab
		   val c3   = E.genCstClEm clv1 clv2 lab1
		   val c4   = E.genCstClEm clv1 (CL.consEXC ()) lab2
		   val cst  = E.uenvcst [cst1, cst2]
		   val css  = E.uenvcss [css1, css2]
		   val cst' = E.conscsts (lab, [c1, c2]) (E.conscst (lab1, c3) (E.conscst (lab2, c4) cst))
		   (*(2010-06-24)Same remark for labs as in f_conbind.*)
		   (*val labs = L.cons lab (A.getLabelLabId labid)*)
		   (*val cons = E.toEX1Vids cons labs*)
	       (* We actually don't need labs to constrain cons to be MONO and EXC.
		* The whole binding shouldn't be constrained by labs but only the status.
		* And the we should pass 71 and 73 :D *)
	       in (E.toCLSVids (E.toMonoVids cons L.empty) clv1 L.empty, cst', css)
               end
	     | f_exbind (A.ExBindEq (labid, longid, _, lab, _)) =
               let val (tv1, cons, cst1, css1) = f_labid labid
		   val (tv2, _, cl, cst2) = f_longidexp longid
		   val clv  = CL.newCl ()
		   val c1   = E.genCstTyEm (T.consV tv2) (T.consV tv1) lab false
		   val c2   = E.genCstClEm cl clv lab
		   val c3   = E.genCstClEm cl (CL.consEXC ()) lab
		   (* NOTE: the class could be a EX0 or a EX1 - a EXC *)
		   val cst  = E.conscsts (lab, [c1, c2]) (E.uenvcst [cst1, cst2])
		   (* NOTE: we build c like that so that the accessor in cst2 is resolved before c3
		    * and we bind the class cl (and clv) to the correct EX0 or EX1 before checking
		    * that it is a EXC. *)
		   val c    = E.CSTLET (E.ENVSEQ (E.ENVCST cst, E.ENVCST (E.singcst (lab, c3))))
		   val labs = L.singleton lab
	       in (E.toCLSVids (E.toMonoVids cons labs) clv labs, E.singcst (L.dummyLab, c), css1)
               end
	     | f_exbind (A.ExBindNo (ident, _)) =
	       let val (tv, cons, cst, css) = f_identpat ident
	       in (E.toEX0Vids cons L.empty, cst, css)
	       end
	     | f_exbind (A.ExBindDots pl) =
	       let val env = f_partlist pl
	       in (E.getVids env, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Env.varenv, Env.cst, Env.css) *)
	   and f_exbindseq (A.ExBindSeq (exbinds, _, _)) =
               let val (conss, csts, csss) = unzipThree (map f_exbind exbinds)
		   val cons = E.uenv conss
		   val cst  = E.uenvcst csts
		   val css  = E.uenvcss csss
		   val css1 = consCSM cons
               in (cons, cst, E.uenvcss [css, css1])
               end
	     | f_exbindseq (A.ExBindSeqDots pl) =
	       let val env = f_partlist pl
	       in (E.getVids env, E.emcst, E.emcss)
	       end

	   (*and f_longstrseq (A.LongStrSeq (ids, _)) =
	       let
		   (* val (idS, (ev, (ev', lop)), cs) = f_longstrid id
		    * ev  is the envvar of the right-most structure
		    * ev' is the envvar of the left-most structure
		    * with Open we want to ev for the free variables
		    * *************** MARK ************* *)
		   val envl = map f_longstrid ids
		   val idS  = E.ENVSEQ (E.uenv (map (fn (E.ENVSEQ x) => x) (getenvEnv envl)))
		   val evs  = getenvTy envl
		   val cs   = E.uenvcss (getenvCs envl)
	       in (idS, evs, cs)
	       end
	     | f_longstrseq (A.LongStrSeqDots pl) =
	       let
		   val (env, _, cs) = f_partlist pl
	       in (E.getStrs env, [], cs)
	       end*)

	   (* RETURNS: (Id.lid list) *)
	   and f_longstrseq (A.LongStrSeq (ids, _)) =
	       List.mapPartial (fn x => A.longstridToLid x) ids
	     | f_longstrseq (A.LongStrSeqDots pl) = []

	   (* RETURNS: (Ty.seqvar, Env.ocenv, Env.cst) *)
	   and f_classbind (A.Class (s, v, _, lab, _)) =
	       let val sv1  = T.freshseqvar ()
		   val sv2  = T.freshseqvar ()
		   val c    = E.genCstSqEm (T.SV sv1) (T.SV sv2) lab
		   val ovcs = E.singenv (v, [E.consBindPoly v (T.SV sv2) (CL.consOC ()) lab])
	       in (sv1, ovcs, E.singcst (lab, c))
	       end
	     | f_classbind A.ClassDots =
	       let val sv = T.freshseqvar ()
	       in (sv, E.emoc, E.emcst)
	       end

	   (* RETURNS: (Ty.seqvar, Env.ocenv, Env.cst) *)
	   and f_labclassbind (A.LabClass (class, _, lab, _)) =
	       let val (sv, ovcs, cst) = f_classbind class
		   val sv' = T.freshseqvar ()
		   val c   = E.genCstSqEm (T.SV sv') (T.SV sv) lab
	       in (sv', ovcs, E.conscst (lab, c) cst)
	       end
	     | f_labclassbind (A.LabClassDots pl) =
	       let val env = f_partlist pl
	       in (T.freshseqvar (), E.emoc, E.singcst (L.dummyLab, E.CSTLET env))
	       end

	   (* RETURNS: (Ty.seqvar, Env.cst) *)
	   and f_class (A.Class (s, v, _, lab, _)) =
	       let val sv = T.freshseqvar ()
		   val a  = E.genAccIoEm (E.consAccId (I.ID (v, lab)) (T.SV sv) (CL.consOC ()) lab) lab
	       in (sv, E.singcst (lab, E.CSTACC a))
	       end
	     | f_class A.ClassDots = (T.freshseqvar (), E.emcst)

	   (* RETURNS: (Ty.seqvar, Env.cst) *)
	   and f_labclass (A.LabClass (class, _, lab, _)) =
	       let val (sv, cst) = f_class class
		   val sv' = T.freshseqvar ()
		   val c   = E.genCstSqEm (T.SV sv') (T.SV sv) lab
	       in (sv', E.conscst (lab, c) cst)
	       end
	     | f_labclass (A.LabClassDots pl) =
	       let val env = f_partlist pl
	       in (T.freshseqvar (), E.singcst (L.dummyLab, E.CSTLET env))
	       end

	   (* RETURNS: (Ty.tyvar, Env.cst, Env.css) *)
	   and f_tyclass (A.TyClassCl (labclass, _, lab, _)) =
	       let val (sv, cst) = f_labclass labclass
		   val tv  = T.freshtyvar ()
		   val lid = Option.getOpt (A.getlabidLabclass labclass, (I.dummyId, L.dummyLab))
		   val c   = E.genCstTyEm (T.consV tv) (T.OR (T.SV sv, T.freshidor (), T.POLY, T.VAL lid, lab)) lab false
	       in (tv, E.conscst (lab, c) cst, E.emcss)
	       end
	     | f_tyclass (A.TyClassTy (typ, lab, _)) =
	       let val (tv, cst, css) = f_type typ
		   val tv' = T.freshtyvar ()
		   val c   = E.genCstTyEm (T.consV tv') (T.consV tv) lab false
	       in (tv', E.conscst (lab, c) cst, css)
	       end
	     | f_tyclass (A.TyClassDots pl) =
	       let val env = f_partlist pl
		   val tv  = T.freshtyvar ()
	       in (tv, E.singcst (L.dummyLab, E.CSTLET env), E.emcss)
	       end

	   (* RETURNS: (Ty.seqvar, Env.cst, Env.css) *)
	   and f_labtyclass (A.LabTyClass (tyclass, _, lab, _)) =
	       let val (tv, cst, css) = f_tyclass tyclass
		   val tv' = T.freshtyvar ()
		   val c   = E.genCstTyEm (T.consV tv') (T.consV tv) lab false
	       in (tv', E.conscst (lab, c) cst, css)
	       end
	     | f_labtyclass (A.LabTyClassDots pl) =
	       let val env = f_partlist pl
	       in (T.freshtyvar (), E.singcst (L.dummyLab, E.CSTLET env), E.emcss)
	       end

	   (* RETURNS: (Ty.seqvar, Env.cst, Env.css) *)
	   and f_tyclassseq (A.TyClassSeqOne (tyclass, _, lab, _)) =
               let val (tv, cst, css) = f_tyclass tyclass
		   val sv = T.freshseqvar ()
		   val c  = E.genCstSqEm (T.SV sv) (T.SC (T.constuple [tv] lab, T.noflex (), lab)) lab
	       in (sv, E.conscst (lab, c) cst, E.emcss)
               end
	     | f_tyclassseq (A.TyClassSeqEm (_, lab, _)) =
	       let val sv = T.freshseqvar ()
		   val c  = E.genCstSqEm (T.SV sv) (T.SC ([], T.noflex (), lab)) lab
	       in (sv, E.singcst (lab, c), E.emcss)
	       end
	     | f_tyclassseq (A.TyClassSeqSeq (labtyclasss, _, lab, _)) =
               let val (tvs, csts, csss) = unzipThree (map f_labtyclass labtyclasss)
		   val sv  = T.freshseqvar ()
		   val cst = E.uenvcst csts
		   val css = E.uenvcss csss
		   val c   = E.genCstSqEm (T.SV sv) (T.SC (T.constuple tvs lab, T.noflex (), lab)) lab
               in (sv, E.conscst (lab, c) cst, css)
               end
	     | f_tyclassseq (A.TyClassSeqDots pl) =
	       let val env = f_partlist pl
		   val sv  = T.freshseqvar ()
	       in (sv, E.singcst (L.dummyLab, E.CSTLET env), E.emcss)
	       end

	   (* RETURNS: (Env.tvenv, Env.cst) *)
	   and f_typevarval (A.TypeVar (_, n, _, lab, _)) =
	       let val tv   = T.freshtyvar ()
		   val tyvs = E.singenv (n, [E.consBindMono n (tv, false) (CL.consTYVAR ()) lab])
		   val c    = E.genCstTyEm (T.consV tv) (T.E (n, T.freshtyvar (), lab)) lab false
	       in (tyvs, E.singcst (lab, c))
	       end
	     | f_typevarval A.TypeVarDots = (E.emtv, E.emcst)

	   (* RETURNS: (Env.tvenv, Env.cst) *)
	   and f_typevarvallist [] = (E.emtv, E.emcst)
	     | f_typevarvallist [typevar] = f_typevarval typevar
	     | f_typevarvallist (typevar :: typevars) =
	       let val (tyvs1, cst1) = f_typevarval typevar
		   val (tyvs2, cst2) = f_typevarvallist typevars
	       in (E.uenv [tyvs1, tyvs2], E.uenvcst [cst1, cst2])
	       end

	   (* RETURNS: (Env.tvenv, Env.cst) *)
	   and f_labtypevarval (A.LabTyVar (tyvar, _, lab, _)) =
	       let val (tyvs, cst) = f_typevarval tyvar
	       (* NOTE: the label is not used. *)
	       in (tyvs, cst)
	       end
	     | f_labtypevarval (A.LabTyVarDots tyvars) = (E.emtv, E.emcst)

	   (* RETURNS: (Env.tvenv, Env.cst) *)
	   and f_tyvarseqval (A.TyVarSeqOne (tyvar, _, lab, _)) = f_typevarval tyvar
	     | f_tyvarseqval (A.TyVarSeqEm (_, lab, _)) = (E.emtv, E.emcst)
	     | f_tyvarseqval (A.TyVarSeqSeq (typevars, _, lab, _)) =
               let val (tyvss, csts) = ListPair.unzip (map f_labtypevarval typevars)
		   val tyvs = E.uenv tyvss
		   val cst  = E.uenvcst csts
               in (tyvs, cst)
               end
	     | f_tyvarseqval (A.TyVarSeqDots tyvars) =
	       let val (tyvss, csts) = ListPair.unzip (map f_typevarval tyvars)
		   val tyvs = E.uenv tyvss
		   val cst  = E.uenvcst csts
               in (tyvs, cst)
	       end

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_dec (A.DecVal (tyvarseq, valbind, _, _)) =
	       let val (env, css) = f_valbind valbind
		   val (tyvs1, cst1) = f_tyvarseqval tyvarseq
		   val (tyvs2, cst2) = f_typevarvallist (A.gettyvarValBind valbind)
		   (*val (idV, cs') = closeTypedTerm (A.gettyvarValBind vb)
						   (A.gettyvarTyVarSeq tvsq)
						   (E.getTyvs env)
						   cs
						   (E.uenv [idG, idR])*)
		   val tyvs = E.plusenv tyvs2 tyvs1
		   val cst  = E.uenvcst [cst1, cst2]
		   val env' = E.ENVSEQ (E.ENVCST cst, E.ENVPOL (tyvs, env))
	       in (env', css)
	       end
	     | f_dec (A.DecFVal (tyvarseq, fvalbind, _, _)) =
	       let val (env, css) = f_fvalbind fvalbind
		   val (tyvs1, cst1) = f_tyvarseqval tyvarseq
		   val (tyvs2, cst2) = f_typevarvallist (A.gettyvarFValBind fvalbind)
		   (*val (idV, cs') = closeTypedTerm (A.gettyvarFValBind fvb)
						   (A.gettyvarTyVarSeq tvsq)
						   (E.getTyvs env)
						   cs
						   idR*)
		   val tyvs = E.plusenv tyvs2 tyvs1
		   val cst  = E.uenvcst [cst1, cst2]
		   val env' = E.ENVSEQ (E.ENVCST cst, E.ENVPOL (tyvs, env))
	       in (env', css)
	       end
	     | f_dec (A.DecDatType (datbinds, _, _)) =
	       let val (typs, conss, cst1, cst2, css) = f_datbindseq datbinds
		   val envs = map (fn (cons, SOME idl) => E.ENVDAT (idl, E.ENVPOL (E.emtv, E.projVids cons))
				    | (cons, NONE) => E.ENVPOL (E.emtv, E.projVids cons)) conss
		   val env1 = E.ENVSEQ (E.ENVCST cst2, E.envsToSeq envs)
		   val env2 = E.ENVSEQ (E.ENVSEQ (E.ENVCST cst1, E.projTyps typs), env1)
	       in (env2, css)
	       end
	     | f_dec (A.DecDatWith (datbind, typbind, _, lab, _)) =
	       let val (typs1, conss, cst1, cst1', css1) = f_datbindseq datbind
		   val (typs2, cst2, css2) = f_typbindseq typbind
		   val css  = E.uenvcss [css1, css2]
		   val envs = map (fn (cons, SOME idl) => E.ENVDAT (idl, E.ENVPOL (E.emtv, E.projVids cons))
				    | (cons, NONE) => E.ENVPOL (E.emtv, E.projVids cons)) conss
		   val env1 = E.ENVSEQ (E.ENVCST cst1', E.envsToSeq envs)
		   val env2 = E.ENVSEQ (E.ENVCST cst1, E.projTyps typs1)
		   val env3 = E.ENVSEQ (E.ENVCST cst2, E.projTyps typs2)
		   val env4 = E.ENVSEQ (env2, E.ENVSEQ (env3, env1))
	       in (env4, css)
	       end
	     | f_dec (A.DecDatRep (tycon, longtycon, _, lab, _)) =
	       let val (s, v, tv, sv, typs, cst1) = f_tyconbind tycon
		   val (tfv, cst2) = f_longtycon longtycon
		   val c   = E.genCstTfEm (T.TFV tfv) (T.TFC (T.SV sv, T.consV tv, lab)) lab
		   val cst = E.conscst (lab, c) (E.uenvcst [cst1, cst2])
		   val opn = case A.longtyconToLid longtycon of
				 NONE => E.emopn
			       | SOME lid => E.singOEnv (lid, lab, E.DRE)
		   val env1 = case v of SOME idl => E.ENVDAT (idl, E.ENVOPN opn) | NONE => E.ENVOPN opn
		   val env2 = E.ENVSEQ (E.ENVCST cst, E.ENVSEQ (E.projTyps typs, env1))
	       in (env2, E.emcss)
	       end
	     | f_dec (A.DecType (typbindseq, _, _)) =
	       let val (typs, cst, css) = f_typbindseq typbindseq
	       in (E.ENVSEQ (E.ENVCST cst, E.projTyps typs), css)
	       end
	     | f_dec (A.DecEx (exbindseq, _, _)) =
	       let val (cons, cst, css) = f_exbindseq exbindseq
	       in (E.ENVSEQ (E.ENVCST cst, E.projVids cons), css)
	       end
	     | f_dec (A.DecOpen (longstrseq, _, lab, _)) =
	       let val opns = foldl (fn (lid, opn) => E.addOEnv (lid, I.getLabId lid (*lab*), E.OST) opn)
				    (* NOTE: We can't use lab for all of then because then we can't
				     * distinguish between the ones that are IN and OUT during unification. *)
				    E.emopn
				    (f_longstrseq longstrseq)
	       in (E.projOpns opns, E.emcss)
	       end
	     | f_dec (A.DecLocal (decs1, decs2, _, lab, _)) =
	       let val (env1, css1) = f_decs decs1
		   val (env2, css2) = f_decs decs2
		   val ev   = E.freshenvvar ()
		   val c    = E.genCstEvEm (E.consEnvVar ev L.dummyLab) env1 L.dummyLab
		   val env3 = E.ENVCST (E.singcst (L.dummyLab, c))
		   val env4 = E.ENVDEP (EL.initExtLab (E.consEnvVar ev lab) lab)
		   val env5 = E.ENVSEQ (env3, E.ENVLOC (env4, env2))
	       in (env5 (*E.ENVLOC (env1, env2)*), E.uenvcss [css1, css2])
	       end
	     | f_dec (A.DecAbsType (datbinds, decs, _, lab, _)) =
	       let val (typs1, conss, cst1, cst1', css1) = f_datbindseq datbinds
		   val (env2, css2) = f_decs decs
		   val envs = map (fn (cons, SOME idl) => E.ENVDAT (idl, E.ENVPOL (E.emtv, E.projVids cons))
				    | (cons, NONE) => E.ENVPOL (E.emtv, E.projVids cons)) conss
		   val env3 = E.ENVSEQ (E.ENVCST cst1', E.envsToSeq envs)
		   val env4 = E.ENVLOC (env3, env2)
		   val env5 = E.ENVSEQ (E.ENVSEQ (E.ENVCST cst1, E.projTyps typs1), env4)
		   val css0 = if getBasis ()
			      then E.emcss
			      else E.singcss (notFullyCs lab "abstype")
		   val css  = E.uenvcss [css1, css2, css0]
	       in (env5, css)
	       end
	     | f_dec (A.DecAbsWith (datbinds, typbinds, decs, _, lab, _)) =
	       let val (typs1, conss, cst1, cst1', css1) = f_datbindseq datbinds
		   val (typs2, cst2, css2) = f_typbindseq typbinds
		   val (env3, css3) = f_decs decs
		   val envs = map (fn (cons, SOME idl) => E.ENVDAT (idl, E.ENVPOL (E.emtv, E.projVids cons))
				    | (cons, NONE) => E.ENVPOL (E.emtv, E.projVids cons)) conss
		   val env4 = E.ENVSEQ (E.ENVCST cst1', E.envsToSeq envs)
		   val env5 = E.ENVSEQ (E.ENVCST cst1,  E.projTyps typs1)
		   val env6 = E.ENVSEQ (E.ENVCST cst2,  E.projTyps typs2)
		   val env7 = E.ENVSEQ (env5, E.ENVSEQ (env6, E.ENVLOC (env4, env3)))
		   val css0 = if getBasis ()
			      then E.emcss
			      else E.singcss (notFullyCs lab "abstype")
		   val css  = E.uenvcss [css1, css2, css3, css0]
	       in (env7, css)
	       end
	     | f_dec (A.DecInfix (i, identseq, _, lab, _)) =
	       let val css1 = if getBasis ()
			      then E.emcss
			      else E.singcss (notFullyCs lab "infix")
		   val css2 = if 0 <= i andalso i <= 9
			      then E.emcss
			      else E.singcss (fixityCs lab)
	       in (E.emenv, E.uenvcss [css1, css2])
	       end
	     | f_dec (A.DecInfixr (i, identseq, _, lab, _)) =
	       let val css1 = if getBasis ()
			      then E.emcss
			      else E.singcss (notFullyCs lab "infixr")
		   val css2 = if 0 <= i andalso i <= 9
			      then E.emcss
			      else E.singcss (fixityCs lab)
	       in (E.emenv, E.uenvcss [css1, css2])
	       end
	     | f_dec (A.DecNonfix (identseq, _, lab, _)) =
	       let val css = if getBasis ()
			     then E.emcss
			     else E.singcss (notFullyCs lab "nonfix")
	       in (E.emenv, css)
	       end
	     | f_dec (A.DecOverload (labid, labtyp, labtyvar, tyclassseq, _, lab, _)) =
	       let val (tv1, vids, cst1, css1) = f_labid labid
		   val (tv2, cst2, css2) = f_labtype labtyp
		   val (tv3, tyvs, cst3) = f_labtypevarbind labtyvar
		   val (sv4, cst4, css4) = f_tyclassseq tyclassseq
		   val lid  = Option.getOpt (A.getlabidLabId labid, (I.dummyId, L.dummyLab))
		   val c1   = E.genCstTyEm (T.consV tv3) (T.OR (T.SV sv4, T.freshidor (), T.POLY, T.VAL lid, lab)) lab false
		   val c2   = E.genCstTyEm (T.consV tv1) (T.consV tv2) lab false
		   val cst  = E.conscsts (lab, [c1, c2]) (E.uenvcst [cst1, cst3, cst4])
		   val css  = E.uenvcss [css1, css2, css4]
		   val env1 = E.ENVSEQ (E.ENVCST cst2, E.projVids (E.toRECVids vids (L.singleton lab)))
		   val env2 = E.ENVSEQ (E.ENVCST cst, E.ENVPOL (tyvs, env1))
	       in (env2, css)
	       end
	     | f_dec (A.DecClass (labclass, tyclassseq, _, lab, _)) =
	       let val (sv1, ovcs, cst1) = f_labclassbind labclass
		   val (sv2, cst2, css2) = f_tyclassseq tyclassseq
		   val c   = E.genCstSqEm (T.SV sv1) (T.SV sv2) lab
		   val cst = E.conscst (lab, c) (E.uenvcst [cst1, cst2])
		   val env = E.ENVSEQ (E.ENVCST cst, E.projOvcs ovcs)
	       in (env, css2)
	       end
	     | f_dec (A.DecDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emcss)
	       end

	   and f_declist []    = (E.emenv, E.emcss)
	     | f_declist [dec] = f_dec dec
	     | f_declist (dec :: decs) =
	       let val (env1, css1) = f_dec dec
		   val (env2, css2) = f_declist decs
	       in (E.ENVSEQ (env1, env2), E.uenvcss [css1, css2])
	       end

	   and f_decs (A.Decs (decs, _)) = f_declist decs
	     | f_decs (A.DecsDots pl)  =
	       let val env = f_partlist pl
	       in (env, E.emcss)
	       end

	   (*and f_decslet (A.Decs (ds, _)) = map (fn x => f_dec x) ds
	     | f_decslet (A.DecsDots pl)  =
	       let val (env, _, cs) = f_partlist pl
	       in [(env, E.emenv, cs)]
	       end*)

	   (* RETURNS: (Env.varenv, E.emcst, E.emcss) *)
	   and f_valdescone (A.ValDescOne (labid, labtyp, _, lab, _)) =
	       let val (tv1, vids, cst1, css1) = f_labid labid
		   val (tv2, cst2, css2) = f_labtype labtyp
		   val c   = E.genCstTyEm (T.consV tv1) (T.consV tv2) lab false
		   val cst = E.conscst (lab, c) (E.uenvcst [cst1, cst2])
		   val css = E.uenvcss [css1, css2]
	       in (E.toRECVids vids (L.singleton lab), cst, css)
	       end
	     | f_valdescone (A.ValDescOneDots pl) =
	       let val env = f_partlist pl
	       in (E.getVids env, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Env.varenv, Env.emcst, Env.emcss) *)
	   and f_valdesc (A.ValDesc (valdescs, _, _)) =
	       let val (vidss, csts, csss) = unzipThree (map f_valdescone valdescs)
		   val vids = E.uenv vidss
		   val cst  = E.uenvcst csts
		   val css  = E.uenvcss csss
	       in (vids, cst, css)
	       end
	     | f_valdesc (A.ValDescDots pl) =
	       let val env = f_partlist pl
	       in (E.getVids env, E.emcst, E.emcss)
	       end

	   (* RETURNS: ((Id.id, Ty.tyname) option, Env.typenv, Env.emcst, Env.emcss) *)
	   and f_typdescone (A.TypDescOne (datname, lab, _)) =
	       let val (s, v, tv, sv1, sv2, typs, tyvs, cst, css) = f_datname datname
		   (* NOTE: tyvs is not used *)
		   val id   = Option.getOpt (Option.map (fn (id, _) => id) v, I.dummyId)
		   val lab' = Option.getOpt (Option.map (fn (_, lab) => lab) v, lab)
		   val tn   = if benv andalso getBasis () then getTyNameString s else T.freshtyname ()
		   val c1   = E.genCstSqEm (T.SV sv1) (T.SV sv2) lab
		   val c2   = E.genCstTyEm (T.consV tv) (T.C (T.NC (tn, T.DE id, lab'), T.SV sv1, lab')) lab' false
		   val tnop = Option.map (fn (id, lab) => {id = id, lab = lab, kind = E.TYP, name = tn}) v
	       in (tnop, typs, E.conscst (lab, c1) (E.conscst (lab', c2) cst), css)
	       end
	     | f_typdescone (A.TypDescOneDots pl) =
	       let val env = f_partlist pl
	       in (NONE, E.getTyps env, E.emcst, E.emcss)
	       end

	   (* RETURNS: ((Id.id, Ty.tyname) option, Env.typenv, E.emcst, E.emcss) *)
	   and f_typdesc (A.TypDesc (typdescs, _, _)) =
	       let val (tnss, typss, csts, csss) = unzipFour (map f_typdescone typdescs)
		   val typs = E.uenv typss
		   val cst  = E.uenvcst csts
		   val css  = E.uenvcss csss
		   val tns  = List.mapPartial (fn x => x) tnss
	       in (tns, typs, cst, css)
	       end
	     | f_typdesc (A.TypDescDots pl) =
	       let val env = f_partlist pl
	       in ([], E.getTyps env, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Env.env, Env.emcst, E.emcss) *)
	   and f_tdrdescone (A.TdrDescOne (datname, labtyp, _, lab, _)) =
	       let val (s, v, tv1, sv1, sv2, typs, tyvs, cst1, css1) = f_datname datname
		   val (tv2, cst2, css2) = f_labtype labtyp
		   val c1  = E.genCstTyEm (T.consV tv1) (T.consV tv2) lab false
		   val c2  = E.genCstSqEm (T.SV sv1) (T.SV sv2) lab
		   val c3  = E.CSTLET (E.ENVSEQ (E.projTyvs tyvs, E.ENVCST cst2))
		   val env = E.ENVDEP (EL.initExtLab (E.projTyps typs) lab)
		   val cst = E.conscsts (lab, [c1, c2]) (E.conscst (L.dummyLab, c3) cst1)
		   val css = E.uenvcss [css1, css2]
	       in (env, cst, css)
	       end
	     | f_tdrdescone (A.TdrDescOneDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Env.env, E.emcss) *)
	   and f_tdrdesclist []                    = (E.emenv, E.emcss)
	     | f_tdrdesclist [tdrdesc]             =
	       let val (env, cst, css) = f_tdrdescone tdrdesc
	       in (E.ENVSEQ (E.ENVCST cst, env), css)
	       end
	     | f_tdrdesclist (tdrdesc :: tdrdescs) =
	       let val (env1, cst, css1) = f_tdrdescone tdrdesc
		   val (env2, css2) = f_tdrdesclist tdrdescs
		   val css = E.uenvcss [css1, css2]
	       in (E.ENVSEQ (E.ENVSEQ (E.ENVCST cst, env1), env2), css)
	       end

	   (* RETURNS: (Env.env, E.emcss) *)
	   and f_tdrdesc (A.TdrDesc (tdrdescs, _, _)) = f_tdrdesclist tdrdescs
	       (*let val (typss, csts, csss) = unzipThree (map f_tdrdescone tdrdescs)
		   val typs = E.uenv typss
		   val cst  = E.uenvcst csts
		   val css  = E.uenvcss csss
	       in (typs, cst, css)
	       end*)
	     | f_tdrdesc (A.TdrDescDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emcss)
	       end

	   (* RETURNS: (Env.varenv, Env.emcst, E.emcss) *)
	   and f_excdescone (A.ExcDescOne (ident, lab, _)) =
	       let val (tv, cons, cst, css) = f_identpat ident
		   val c = E.genCstTyEm (T.consV tv) (T.constyexception' lab (T.DE I.dummyId)) lab false
	       in (E.toEX0Vids cons (L.singleton lab), E.conscst (lab, c) cst, css)
	       end
	     | f_excdescone (A.ExcDescOf (labid, labtyp, _, lab, _)) =
	       let val (tv1, cons, cst1, css1) = f_labid labid
		   val (tv2, cst2, css2) = f_labtype labtyp
		   val ty  = T.constyexception' lab (T.DE I.dummyId)
		   val c   = E.genCstTyEm (T.consV tv1) (T.consTyArrowTy (T.consV tv2) ty lab (T.DE I.dummyId)) lab false
		   val cst = E.conscst (lab, c) (E.uenvcst [cst1, cst2])
		   val css = E.uenvcss [css1, css2]
	       in (E.toEX1Vids cons (L.singleton lab), cst, css)
	       end
	     | f_excdescone (A.ExcDescOneDots pl) =
	       let val env = f_partlist pl
	       in (E.getVids env, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Env.varenv, Env.emcst, E.emcss) *)
	   and f_excdesc (A.ExcDesc (exdescs, _, _)) =
	       let val (vidss, csts, csss) = unzipThree (map f_excdescone exdescs)
		   val vids = E.uenv vidss
		   val cst  = E.uenvcst csts
		   val css  = E.uenvcss csss
	       in (vids, cst, css)
	       end
	     | f_excdesc (A.ExcDescDots pl) =
	       let val env = f_partlist pl
	       in (E.getVids env, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Ty.tyvar, Env.varenv, Env.emcst, E.emcss) *)
	   and f_condescone (A.ConDescOneId (ident, _)) =
               let val (tv, cons, cst, css) = f_identpat ident
               in (tv, E.toDA0Vids cons L.empty, cst, css)
               end
	     | f_condescone (A.ConDescOneOf (labid, labtyp, _, lab, _)) =
               let val (tv1, cons, cst1, css1) = f_labid labid
		   val (tv2, cst2, css2) = f_labtype labtyp
		   val (lab1, lab2) = Option.getOpt (A.getLabelsIdLabId labid, (lab, lab))
		   val clv1 = CL.newCl ()
		   val clv2 = CL.newCl ()
		   val tv   = T.freshtyvar ()
		   val c1   = E.genCstTyEm (T.consV tv1) (T.constyarrow' tv2 tv lab (T.DE I.dummyId)) lab false
		   val c2   = E.genCstClEm clv2 (CL.consDA1 ()) lab
		   val c3   = E.genCstClEm clv1 clv2 lab1
		   val c4   = E.genCstClEm clv1 (CL.consDAT ()) lab2
		   val cst  = E.uenvcst [cst1, cst2]
		   val css  = E.uenvcss [css1, css2]
		   val cst' = E.conscsts (lab, [c1, c2]) (E.conscst (lab1, c3) (E.conscst (lab2, c4) cst))
	       (*val cons = E.toDA1Vids cons (L.singleton lab)*)
	       in (tv, E.toCLSVids cons clv1 L.empty, cst', css)
               end
	     | f_condescone (A.ConDescOneNoOf (ident, _)) =
               let val (tv, cons, cst, css) = f_identpat ident
		   val tv' = T.freshtyvar ()
	       in (tv, E.toDATVids cons L.empty, cst, css)
               end
	     | f_condescone (A.ConDescOneDots pl) =
	       let val env = f_partlist pl
		   val tv  = T.freshtyvar ()
	       in (tv, E.getVids env, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Ty.tyvar list, Env.varenv, Env.emcst, E.emcss) *)
	   and f_condesc (A.ConDesc (condescs, _, _)) =
               let val (tvs, conss, csts, csss) = unzipFour (map f_condescone condescs)
		   val cons = E.uenv conss
		   val cst  = E.uenvcst csts
		   val css  = E.uenvcss csss
               in (tvs, cons, cst, css)
               end
	     | f_condesc (A.ConDescDots pl) =
	       let val env = f_partlist pl
	       in ([], E.getVids env, E.emcst, E.emcss)
	       end

	   (*and f_tycondd (A.TyCon (_, v, _, l, _)) =
	       let val tv1 = T.freshtyvar ()
		   val tv2 = T.freshtyvar ()
		   val idT = E.singenv (v, [E.consBindPoly v (T.consV tv1) (CL.consDAT ()) l])
		   val c   = E.genCstTyEm (T.consV tv1) (T.consV tv2) l
	       in (idT, tv2, E.singcsSem(l, c))
	       end
	     | f_tycondd A.TyConDots =
	       let val tv = T.freshtyvar ()
	       in (E.emtyp, tv, E.emcs)
	       end

	   and f_datnamedd (A.DatName (tvs, tn, _, _)) =
	       let val (asc, sv, cs1) = f_typevarseq tvs
		   val (idT, tv, cs2) = f_tycondd tn
	       in ((asc, idT), (tv, sv), E.uenvcs cs1 cs2)
	       end
	     | f_datnamedd A.DatNameDots =
	       let val tv = T.freshtyvar ()
		   val sv = T.freshseqvar ()
	       in (([], E.emtyp), (tv, sv), E.emcs)
	       end*)

	   (*(* this is not used anymore *)
	   and f_datdescone' (A.DatDescOne (dn, cd, _, l, _)) =
               let
		   val (env, (idC, tyl, lidops), cs1) = f_condesc cd
		   val ((asc, idT), (tv2, sv2), cs2) = f_datnamedd dn
		   val idT = E.toDATcons idT lidops
		   val tnv = T.freshtynamevar ()
		   val c1  = E.CSTTY (T.consV tv2, T.Abs (T.SV sv2, T.C (T.NV tnv, T.SV sv2, l), l), L.empty)
		   val cs3 = map (fn x => E.CSTTY (T.consV x, T.consV tv2, L.empty)) tyl
		   val cs4 = checkTyVarInc (A.gettyvarDatName dn) (A.getlabDatName dn) (A.gettyvarConDesc cd)
		   val cs5 = consisadatacons tyl l
		   val cs  = E.conscsSems (l, [c1]) cs2
               in (env, (idT, E.toCONlab idC l, E.getcsSem cs),
		   E.conscsSems (l,  cs3 @ cs5) (E.concatcsSyn cs4 (E.uenvcs (renamecs cs1 asc) cs)))
               end
	     | f_datdescone' (A.DatDescOneDots pl) =
	       let
		   val (env, _, cs) = f_partlist pl
	       in (env, (E.emid, E.emid, E.emcst), cs)
	       end*)

	   (* RETURNS: ((Id.id, Ty.tyname) option, Env.typenv, E.varenv * Id.idl option, E.emcst, E.emcst, E.emcss) *)
	   and f_datdescone (A.DatDescOne (datname, consdesc, _, lab, _)) =
               let val (s, v, tv, sv1, sv2, typs, tyvs, cst1, css1) = f_datname datname
		   val (tvs, cons, cst2, css2) = f_condesc consdesc
		   val tn   = if benv andalso getBasis () then getTyNameString s else T.freshtyname ()
		   val id   = Option.getOpt (Option.map (fn (i, _) => i) v, I.dummyId)
		   val lab' = Option.getOpt (Option.map (fn (_, l) => l) v, lab)
		   val c1   = E.genCstSqEm (T.SV sv1) (T.SV sv2) lab
		   val c2   = E.genCstTyEm (T.consV tv) (T.C (T.NC (tn, T.DE id, lab'), T.SV sv1, lab')) lab' false
		   val c3   = E.CSTLET (E.ENVSEQ (E.projTyvs tyvs, E.ENVCST cst2))
		   val cs   = map (fn x => E.genCstTyEm (T.consV x) (T.consV tv) lab false) tvs
		   val cst  = E.conscst (lab, c1) (E.conscst (lab', c2) cst1)
		   val cst' = E.conscsts (lab, cs) (E.singcst (L.dummyLab, c3))
		   val tnop = Option.map (fn (id, lab) => {id = id, lab = lab, kind = E.DAT, name = tn}) v
		   val typs = E.toTYCONTyps typs E.emvar false (L.singleton lab)
		   (*val cs4 = checkTyVarInc (A.gettyvarDatName dn) (A.getlabDatName dn) (A.gettyvarConDesc cd)*)
               in (tnop, typs, (cons, v), cst, cst', E.uenvcss [css1, css2])
               end
	     | f_datdescone (A.DatDescOneDots pl) =
	       let val env = f_partlist pl
	       in (NONE, E.getTyps env, (E.getVids env, NONE), E.emcst, E.emcst, E.emcss)
	       end

	   (* RETURNS: ((Id.id, Ty.tyname) option, Env.typenv, (E.varenv * Id.idl option) list, E.emcst, E.emcst, E.emcss) *)
	   and f_datdesc (A.DatDesc (datdescs, _, _)) =
               let val (tnss, typss, conss, csts, csts', csss) = unzipSix (map f_datdescone datdescs)
		   val typs = E.uenv typss
		   val cst  = E.uenvcst csts
		   val cst' = E.uenvcst csts'
		   val css  = E.uenvcss csss
		   val tns  = List.mapPartial (fn x => x) tnss
               in (tns, typs, conss, cst, cst', css)
               end
	     | f_datdesc (A.DatDescDots pl) =
	       let val env = f_partlist pl
	       in ([], E.getTyps env, [(E.getVids env, NONE)], E.emcst, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Env.strenv, E.emcst, E.emcss) *)
	   and f_strdescone (A.StrDescOne (strid, labsigexp, _, lab, _)) =
	       let val (ev1, strs, cst1, css1) = f_strid strid
		   val (ev2, cst2, css2) = f_labsigexp labsigexp
		   val c   = E.genCstEvEm (E.consEnvVar ev1 lab) (E.consEnvVar ev2 lab) lab
		   val cst = E.conscst (lab, c) (E.uenvcst [cst1, cst2])
		   val css = E.uenvcss [css1, css2]
	       in (strs, cst, css)
	       end
	     | f_strdescone (A.StrDescOneDots pl) =
	       let val env = f_partlist pl
	       in (E.getStrs env, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Env.strenv, E.emcst, E.emcss) *)
	   and f_strdesc (A.StrDesc (strdescs, _, _)) =
	       let val (strss, csts, csss) = unzipThree (map f_strdescone strdescs)
		   val strs = E.uenv strss
		   val cst  = E.uenvcst csts
		   val css  = E.uenvcss csss
	       in (strs, cst, css)
	       end
	     | f_strdesc (A.StrDescDots pl) =
	       let val env = f_partlist pl
	       in (E.getStrs env, E.emcst, E.emcss)
	       end

	   (* RETURNS: (E.env, E.emcss) *)
	   and f_speconesmltes (A.SpecVal (valdesc, _, lab, _)) =
	       let val (vids, cst1, css) = f_valdesc valdesc
		   val (tyvs, cst2) = f_typevarvallist (A.gettyvarValDesc valdesc)
		   val env1 = E.ENVPOL (tyvs, E.ENVSEQ (E.ENVCST cst1, E.projVids vids))
		   val env2 = E.ENVSEQ (E.ENVCST cst2, env1)
		   val ev   = E.freshenvvar ()
		   val c    = E.genCstEvEm (E.consEnvVar ev lab) env2 lab
		   val env3 = E.ENVSEQ (E.ENVCST (E.singcst (lab, c)), E.ENVDEP (EL.initExtLab (E.consEnvVar ev lab) lab))
	       in (env3, css)
	       end
	     | f_speconesmltes spec = f_specone spec

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_speconesmlteslist []        = (E.emenv, E.emcss)
	     | f_speconesmlteslist [x]       = f_speconesmltes x
	     | f_speconesmlteslist (x :: xs) =
	       let val (env1, css1) = f_speconesmltes x
		   val (env2, css2) = f_speconesmlteslist xs
	       in (E.ENVSEQ (env1, env2), E.uenvcss [css1, css2])
	       end

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_specsmltes (A.Spec (specs, _)) =
	       let val (env, css) = f_speconesmlteslist specs
	       (*(2010-04-19)These gets might pose problem with ENVSEQ and ENVOPN.
		* They pose problem.  We need to build before and then check at unification
		* time that we don't have duplicates for signatures only.*)
	       (*val csc1 = if E.isEnvC env2 then consCSM [E.getVids env2, E.getCons env2] false else []*)
	       (*val csc2 = if E.isEnvC env2 then consCSM [E.getTyps env2] false else []*)
	       in (env, css)
	       end
	     | f_specsmltes (A.SpecDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emcss)
	       end

	   (* RETURNS: (Ty.tvenv, Env.cst) *)
	   and f_typevarspec (A.TypeVar (_, n, _, lab, _)) =
	       let val tv1  = T.freshtyvar ()
		   val tv2  = T.freshtyvar ()
		   val tyvs = E.singenv (n, [E.consBindMono n (tv1, true) (CL.consTYVAR ()) lab])
		   val c    = E.genCstTyEm (T.consV tv1) (T.V (tv2, SOME (n, lab), T.POLY)) lab false
	       in (tyvs, E.singcst (lab, c))
	       end
	     | f_typevarspec A.TypeVarDots = (E.emtv, E.emcst)

	   (* RETURNS: (Env.tvenv, Env.cst) *)
	   and f_typevarspeclist [] = (E.emtv, E.emcst)
	     | f_typevarspeclist [typevar] = f_typevarspec typevar
	     | f_typevarspeclist (typevar :: typevars) =
	       let val (tyvs1, cst1) = f_typevarspec typevar
		   val (tyvs2, cst2) = f_typevarspeclist typevars
	       in (E.uenv [tyvs1, tyvs2], E.uenvcst [cst1, cst2])
	       end

	   (* RETURNS: (E.env, E.emcss) *)
	   and f_specone (A.SpecVal (valdesc, _, lab, _)) =
	       let val (vids, cst1, css) = f_valdesc valdesc
		   (*(2010-06-23)Before we were using f_typevarvallist which generates binders
		    * of explicit type variables, now we use binders of implicit type variables
		    * tagged with the labels of the implicit type variables they're coming from. *)
		   val (tyvs, cst2) = f_typevarspeclist (A.gettyvarValDesc valdesc)
		   val env1 = E.ENVSEQ (E.ENVCST cst2, E.projTyvs tyvs)
		   val env2 = E.ENVSEQ (E.ENVCST cst1, E.ENVDEP (EL.initExtLab (E.projVids vids) lab))
		   (*val env3 = E.ENVLOC (env1, env2)*)
		   val env3 = E.ENVPOL (E.emtv, E.ENVLOC (E.ENVDEP (EL.initExtLab env1 lab), env2))
		   val ev1  = E.freshenvvar ()
		   val ev2  = E.freshenvvar ()
		   val c1   = E.genCstEvEm (E.consEnvVar ev1 L.dummyLab) env3 L.dummyLab
		   val c2   = E.genCstEvEm (E.consEnvVar ev2 lab) (E.consEnvVar ev1 lab) lab
		   val cst  = E.conscst (L.dummyLab, c1) (E.singcst (lab, c2))
		   val env4 = E.ENVSEQ (E.ENVCST cst, E.ENVDEP (EL.initExtLab (E.consEnvVar ev2 lab) lab))
	       (*(2010-06-17)Don't we want to use ENVPOL on vids?
		* The problem is that when we check a signature against a structure
		* then we want the explicit type variables to be explicit to check
		* that the structure is at least as general as the signature. *)
	       (* NOTE: we have to bind the explicit type variables. *)
	       in (env4, css)
	       end
	     | f_specone (A.SpecTyp (typdesc, _, lab, _)) =
	       let val (tns, typs, cst, css) = f_typdesc typdesc
		   val env  = E.ENVSEQ (E.ENVCST cst, E.updateITns tns (E.projTyps typs))
		   val ev   = E.freshenvvar ()
		   val c    = E.genCstEvEm (E.consEnvVar ev lab) env lab
		   val env' = E.ENVSEQ (E.ENVCST (E.singcst (lab, c)), E.ENVDEP (EL.initExtLab (E.consEnvVar ev lab) lab))
	       in (env', css)
	       end
	     | f_specone (A.SpecEqT (typdesc, _, lab, _)) =
	       let val (tns, typs, cst, css) = f_typdesc typdesc
		   val env  = E.ENVSEQ (E.ENVCST cst, E.updateITns tns (E.projTyps typs))
		   val css  = if getBasis ()
			      then css
			      else E.conscss (sorryCsS lab "eqtype") css
		   val ev   = E.freshenvvar ()
		   val c    = E.genCstEvEm (E.consEnvVar ev lab) env lab
		   val env' = E.ENVSEQ (E.ENVCST (E.singcst (lab, c)), E.ENVDEP (EL.initExtLab (E.consEnvVar ev lab) lab))
	       in (env', css)
	       end
	     | f_specone (A.SpecTdr (tdrdesc, _, lab, _)) =
	       let val (env, css) = f_tdrdesc tdrdesc
		   val ev1  = E.freshenvvar ()
		   val ev2  = E.freshenvvar ()
		   val c1   = E.genCstEvEm (E.consEnvVar ev1 L.dummyLab) env L.dummyLab
		   val c2   = E.genCstEvEm (E.consEnvVar ev2 lab) (E.consEnvVar ev1 lab) lab
		   val env1 = E.ENVCST (E.conscst (L.dummyLab, c1) (E.singcst (lab, c2)))
		   val env2 = E.ENVSEQ (env1, E.ENVDEP (EL.initExtLab (E.consEnvVar ev2 lab) lab))
	       (* We can't have an ENVDEP here because we can have an error in
		* the type part that does not need the spec to exist.
		* It is the same for all the specs because of the type parts. *)
	       in (env2, css)
	       end
	     | f_specone (A.SpecExc (exdesc, _, lab, _)) =
	       let val (cons, cst, css) = f_excdesc exdesc
		   val env  = E.ENVSEQ (E.ENVCST cst, E.ENVDEP (EL.initExtLab (E.projVids cons) lab))
		   val ev1  = E.freshenvvar ()
		   val ev2  = E.freshenvvar ()
		   val c1   = E.genCstEvEm (E.consEnvVar ev1 L.dummyLab) env L.dummyLab
		   val c2   = E.genCstEvEm (E.consEnvVar ev2 lab) (E.consEnvVar ev1 lab) lab
		   val cst' = E.conscst (L.dummyLab, c1) (E.singcst (lab, c2))
		   val env' = E.ENVSEQ (E.ENVCST cst', E.ENVDEP (EL.initExtLab (E.consEnvVar ev2 lab) lab))
	       in (env', css)
	       end
	     | f_specone (A.SpecDat (datdesc, _, lab, _)) =
	       let val (tns, typs, conss, cst1, cst2, css) = f_datdesc datdesc
		   val envs = map (fn (cons, SOME idl) => E.ENVDAT (idl, E.ENVPOL (E.emtv, E.projVids cons))
				    | (cons, NONE) => E.ENVPOL (E.emtv, E.projVids cons)) conss
		   val env1 = E.ENVSEQ (E.ENVCST cst2, E.envsToSeq envs)
		   val env2 = E.updateITns tns (E.projTyps typs)
		   val env3 = E.ENVSEQ (E.ENVSEQ (E.ENVCST cst1, E.ENVDEP (EL.initExtLab env2 lab)), env1)
		   val ev1  = E.freshenvvar ()
		   val ev2  = E.freshenvvar ()
		   val c1   = E.genCstEvEm (E.consEnvVar ev1 L.dummyLab) env3 L.dummyLab
		   val c2   = E.genCstEvEm (E.consEnvVar ev2 lab) (E.consEnvVar ev1 lab) lab
		   val cst  = E.conscst (L.dummyLab, c1) (E.singcst (lab, c2))
		   val env4 = E.ENVSEQ (E.ENVCST cst, E.ENVDEP (EL.initExtLab (E.consEnvVar ev2 lab) lab))
	       in (env4, css)
	       end
	     | f_specone (A.SpecStr (strdesc, _, lab, _)) =
	       let val (strs, cst, css) = f_strdesc strdesc
		   val env  = E.ENVSEQ (E.ENVCST cst, E.ENVDEP (EL.initExtLab (E.projStrs strs) lab))
		   val ev1  = E.freshenvvar ()
		   val ev2  = E.freshenvvar ()
		   val c1   = E.genCstEvEm (E.consEnvVar ev1 L.dummyLab) env L.dummyLab
		   val c2   = E.genCstEvEm (E.consEnvVar ev2 lab) (E.consEnvVar ev1 lab) lab
		   val env1 = E.ENVCST (E.conscst (L.dummyLab, c1) (E.singcst (lab, c2)))
		   val env2 = E.ENVSEQ (env1, E.ENVDEP (EL.initExtLab (E.consEnvVar ev2 lab) lab))
		   (*val _ = D.printdebug2 (L.printLab lab)*)
		   (*val _ = D.printdebug2 (E.printEnv env "")*)
	       in (env2, css)
	       end
	     | f_specone (A.SpecInc (labsigexp, _, lab, _)) =
	       let val (ev, cst, css) = f_labsigexp labsigexp
		   val ev'  = E.freshenvvar ()
		   val c    = E.genCstEvEm (E.consEnvVar ev' lab) (E.consEnvVar ev lab) lab
		   val env1 = E.ENVCST (E.conscst (lab, c) cst)
		   val env2 = E.ENVSEQ (env1, E.ENVDEP (EL.initExtLab (E.consEnvVar ev' lab) lab))
		   val css' = if getBasis ()
			      then E.emcss
			      else E.singcss (sorryCsS lab "include")
	       in (env2, css')
	       end
	     | f_specone (A.SpecIsi (_, _, lab, _)) =
	       let val css = if getBasis ()
			     then E.emcss
			     else E.singcss (sorryCsS lab "include")
	       in (E.emenv, css)
	       end
	     | f_specone (A.SpecRep (tycon, longtycon, _, lab, _)) =
	       let val (s, v, tv, sv, typs, cst1) = f_tyconbind tycon
		   val (tfv, cst2) = f_longtycon longtycon
		   val c   = E.genCstTfEm (T.TFV tfv) (T.TFC (T.SV sv, T.consV tv, lab)) lab
		   val cst = E.conscst (lab, c) (E.uenvcst [cst1, cst2])
		   val opn = case A.longtyconToLid longtycon of
				 NONE => E.emopn
			       | SOME lid => E.singOEnv (lid, lab, E.DRE)
		   val env1 = case v of SOME idl => E.ENVDAT (idl, E.ENVOPN opn) | NONE => E.ENVOPN opn
		   val env2 = E.ENVSEQ (E.ENVCST cst, E.ENVSEQ (E.projTyps typs, env1))
		   val ev   = E.freshenvvar ()
		   val c    = E.genCstEvEm (E.consEnvVar ev lab) env2 lab
		   val env3 = E.ENVSEQ (E.ENVCST (E.singcst (lab, c)), E.ENVDEP (EL.initExtLab (E.consEnvVar ev lab) lab))
	       in (env3, E.emcss)
	       end
	     | f_specone (A.SpecSha (spec, longtyconeq, _, lab, _)) =
	       let val (env1, css) = f_spec spec
		   val env2 = f_longtyconeq longtyconeq
		   (*(2010-07-02)This is not the correct thing to do for sharing.*)
		   val ev   = E.freshenvvar ()
		   val ev1  = E.freshenvvar ()
		   val ev2  = E.freshenvvar ()
		   val c1   = E.genCstEvEm (E.consEnvVar ev1 lab) env1 lab
		   val c2   = E.genCstEvEm (E.consEnvVar ev2 lab) env2 lab
		   val c3   = E.CSTSHA (ev1, ev, ev2, lab)
		   (*val c3   = E.CSTSIG (ev1, SOME ev, ev2, NONE, lab, E.WHR)*)
		   val env3 = E.ENVSEQ (E.ENVCST (E.singcsts (lab, [c1, c2])), E.ENVCST (E.singcst (lab, c3)))
		   val env4 = E.ENVSEQ (env3, E.consEnvVar ev lab)
		   val css' = if getBasis ()
			      then E.emcss
			      else E.singcss (poorlyCs lab "sharing")
	       in (env4, E.uenvcss [css, css'])
	       end
	     | f_specone (A.SpecSsi (spec, longstrideq, _, lab, _)) =
	       let val (env, css) = f_spec spec
		   val css' = if getBasis ()
			      then E.emcss
			      else E.singcss (poorlyCs lab "sharing")
		   val env' = E.newEnvVar lab
	       in (E.ENVSEQ (env, env'), E.uenvcss [css, css'])
	       end
	     | f_specone (A.SpecOneDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emcss)
	       end

	   (* RETURNS: Env.env *)
	   and f_longtyconeq (A.LongTyConEq (longtycons, _, lab, _)) =
	       let val (tfvs, envs, csts) = unzipThree (map f_longtyconbind longtycons)
		   val env = E.uenvEnv envs
		   val cst = E.uenvcst csts
		   val tfc = T.TFC (T.newSV (), T.newV (), lab)
		   val cs  = map (fn tfv => E.genCstTfEm (T.TFV tfv) tfc lab) tfvs
	       in E.ENVSEQ (E.ENVCST (E.conscsts (lab, cs) cst), env)
	       end
	     | f_longtyconeq (A.LongTyConEqDots pl) =
	       let val env = f_partlist pl
	       in env
	       end

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_speconelist []        = (E.emenv, E.emcss)
	     | f_speconelist [x]       = f_specone x
	     | f_speconelist (x :: xs) =
	       let val (env1, css1) = f_specone x
		   val (env2, css2) = f_speconelist xs
	       in (E.ENVSEQ (env1, env2), E.uenvcss [css1, css2])
	       end

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_spec (A.Spec (specs, _)) =
	       let val (env, css) = f_speconelist specs
	       (*(2010-04-19)These gets might pose problem with ENVSEQ and ENVOPN.
		* They pose problem.  We need to build before and then check at unification
		* time that we don't have duplicates for signatures only.*)
	       (*val csc1 = if E.isEnvC env2 then consCSM [E.getVids env2, E.getCons env2] false else []*)
	       (*val csc2 = if E.isEnvC env2 then consCSM [E.getTyps env2] false else []*)
	       in (env, css)
	       end
	     | f_spec (A.SpecDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emcss)
	       end

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_strbind (A.StrBind (strbinds, _, _)) =
	       let val (strs, cst, css) = f_strbindonelist strbinds
	       in (E.ENVSEQ (E.ENVCST cst, E.projStrs strs), css)
	       end
	     | f_strbind (A.StrBindDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emcss)
	       end

	   (* RETURNS: (Env.strenv, Env.cst, Env.css) *)
	   and f_strbindonelist [] = (E.emstr, E.emcst, E.emcss)
	     | f_strbindonelist [x] = f_strbindone x
	     | f_strbindonelist (x :: xs) =
	       let val (strs1, cst1, css1) = f_strbindone x
		   val (strs2, cst2, css2) = f_strbindonelist xs
		   val strs = E.uenv [strs1, strs2]
		   val cst  = E.uenvcst [cst1, cst2]
		   val css  = E.uenvcss [css1, css2]
	       in (strs, cst, css)
	       end

	   (* RETURNS: (Env.strenv, Env.cst, Env.css) *)
	   and f_strbindone (A.StrBindOneOp (strid, labsigexp, labstrexp, _, lab, _)) =
	       let val (ev1, cst1, css1) = f_labstrexp labstrexp
		   val (ev2, cst2, css2) = f_labsigexp labsigexp
		   val (ev3, strs, cst3, css3) = f_strid strid
		   val c    = E.CSTSIG (ev2, NONE, ev1, SOME ev3, lab)
		   val cst  = E.singcst (lab, c)
		   val css  = E.uenvcss [css1, css2, css3]
		   val env  = E.ENVSEQ (E.ENVCST (E.uenvcst [cst1, cst2]), E.ENVCST cst)
		   val cst' = E.singcst (L.dummyLab, E.CSTLET env)
	       in (strs, E.uenvcst [cst3, cst'], css)
	       end
	     | f_strbindone (A.StrBindOneTr (strid, labsigexp, labstrexp, _, lab, _)) =
	       let val (ev1, cst1, css1) = f_labstrexp labstrexp
		   val (ev2, cst2, css2) = f_labsigexp labsigexp
		   val (ev3, strs, cst3, css3) = f_strid strid
		   val c    = E.CSTSIG (ev2, SOME ev3, ev1, NONE, lab)
		   val cst  = E.singcst (lab, c)
		   val css  = E.uenvcss [css1, css2, css3]
		   val env  = E.ENVSEQ (E.ENVCST (E.uenvcst [cst1, cst2]), E.ENVCST cst)
		   val cst' = E.singcst (L.dummyLab, E.CSTLET env)
	       in (strs, E.uenvcst [cst3, cst'], css)
	       end
	     | f_strbindone (A.StrBindOne (strid, labstrexp, _, lab, _)) =
	       let val (ev1, strs, cst1, css1) = f_strid strid
		   val (ev2, cst2, css2) = f_labstrexp labstrexp
		   val c   = E.genCstEvEm (E.consEnvVar ev1 lab) (E.consEnvVar ev2 lab) lab
		   val cst = E.conscst (lab, c) (E.uenvcst [cst1, cst2])
		   val css = E.uenvcss [css1, css2]
	       in (strs, cst, css)
	       end
	     | f_strbindone (A.StrBindOneDots pl) =
	       let val env = f_partlist pl
	       in (E.getStrs env, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_strdec (A.StrDec (strdecones, lab, _)) =
	       f_strdeconelist strdecones
	     | f_strdec (A.StrDecDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emcss)
	       end

	   (*and f_strdeclet (A.StrDec (xs, l, _)) = map (fn x => f_strdecone x) xs
	     | f_strdeclet (A.StrDecDots pl) =
	       let val (env, _, cs) = f_partlist pl
	       in [(env, E.emenv, cs)]
	       end*)

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_strdeconelist [] = (E.emenv, E.emcss)
	     | f_strdeconelist [strdec] = f_strdecone strdec
	     | f_strdeconelist (strdec :: strdecs) =
	       let val (env1, css1) = f_strdecone strdec
		   val (env2, css2) = f_strdeconelist strdecs
	       in (E.ENVSEQ (env1, env2), E.uenvcss [css1, css2])
	       end

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_strdecone (A.StrDecOneDec decs) = f_decs decs
	     | f_strdecone (A.StrDecOneStr (strbind, _, _)) = f_strbind strbind
	     | f_strdecone (A.StrDecOneLoc (strdec1, strdec2, _, lab, _)) =
	       let val (env1, css1) = f_strdec strdec1
		   val (env2, css2) = f_strdec strdec2
		   val ev1  = E.freshenvvar ()
		   val ev2  = E.freshenvvar ()
		   val c1   = E.genCstEvEm (E.consEnvVar ev1 L.dummyLab) env1 L.dummyLab
		   val c2   = E.genCstEvEm (E.consEnvVar ev2 lab) (E.consEnvVar ev1 lab) lab
		   val env3 = E.ENVCST (E.conscst (L.dummyLab, c1) (E.singcst (lab, c2)))
		   val env4 = E.ENVDEP (EL.initExtLab (E.consEnvVar ev2 lab) lab)
		   val env5 = E.ENVSEQ (env3, E.ENVLOC (env4, env2))
	       in (env5 (*E.ENVLOC (env1, env2)*), E.uenvcss [css1, css2])
	       end
	     | f_strdecone (A.StrDecOneFun (funbind, _, lab, _)) = f_funbind funbind
	     | f_strdecone (A.StrDecOneDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emcss)
	       end

	   (* RETURNS: (Env.envvar, Env.cst, Env.css) *)
	   and f_labstrexp (A.LabStrExp (strexp, _, _, lab, _)) =
	       let val (ev, cst, css) = f_strexp strexp
		   val ev' = E.freshenvvar ()
		   val c   = E.genCstEvEm (E.consEnvVar ev' lab) (E.consEnvVar ev lab) lab
	       in (ev', E.conscst (lab, c) cst, css)
	       end
	     | f_labstrexp (A.LabStrExpDots pl) =
	       let val ev  = E.freshenvvar ()
		   val env = f_partlist pl
	       in (ev, E.singcst (L.dummyLab, E.CSTLET env), E.emcss)
	       end

	   (* RETURNS: (Env.envvar, Env.cst, Env.css) *)
	   and f_strexp (A.StrExpBasic (strdec, _, lab, _)) =
	       let val (env, css) = f_strdec strdec
		   val env = E.updateILab lab env
		   val ev1 = E.freshenvvar ()
		   val ev2 = E.freshenvvar ()
		   val c1  = E.genCstEvEm (E.consEnvVar ev1 lab) (E.consEnvVar ev2 lab) lab
		   val c2  = E.genCstEvEm (E.consEnvVar ev2 L.dummyLab) env L.dummyLab
	       in (ev1, E.conscst (lab, c1) (E.singcst (L.dummyLab, c2)), css)
	       end
	     | f_strexp (A.StrExpId (longstrid, lab, _)) =
	       let val (ev, cst) = f_longstrid longstrid
		   val ev' = E.freshenvvar ()
		   val c   = E.genCstEvEm (E.consEnvVar ev' lab) (E.consEnvVar ev lab) lab
	       in (ev', E.conscst (lab, c) cst, E.emcss)
	       end
	     | f_strexp (A.StrExpOp (labstrexp, labsigexp, _, lab, _)) =
	       let val (ev1, cst1, css1) = f_labstrexp labstrexp
		   val (ev2, cst2, css2) = f_labsigexp labsigexp
		   val ev  = E.freshenvvar ()
		   val cst = E.uenvcst [cst1, cst2]
		   val css = E.uenvcss [css1, css2]
		   val c   = E.CSTSIG (ev2, NONE, ev1, SOME ev, lab)
		   val env = E.ENVSEQ (E.ENVCST cst, E.ENVCST (E.singcst (lab, c)))
	       in (ev, E.singcst (L.dummyLab, E.CSTLET env), css)
	       end
	     | f_strexp (A.StrExpTr (labstrexp, labsigexp, _, lab, _)) =
	       let val (ev1, cst1, css1) = f_labstrexp labstrexp
		   val (ev2, cst2, css2) = f_labsigexp labsigexp
		   val ev  = E.freshenvvar ()
		   val cst = E.uenvcst [cst1, cst2]
		   val css = E.uenvcss [css1, css2]
		   val c   = E.CSTSIG (ev2, SOME ev, ev1, NONE, lab)
		   val env = E.ENVSEQ (E.ENVCST cst, E.ENVCST (E.singcst (lab, c)))
	       in (ev, E.singcst (L.dummyLab, E.CSTLET env), css)
	       end
	     | f_strexp (A.StrExpFExp (funid, labstrexp, _, lab, _)) =
	       let val (ev0, ev1, cst1) = f_funid funid
		   val (ev2, cst2, css) = f_labstrexp labstrexp
		   val ev  = E.freshenvvar ()
		   val c   = E.CSTFUN (ev0, ev1, ev2, ev, lab)
		   val env = E.ENVSEQ (E.ENVCST (E.uenvcst [cst1, cst2]), E.ENVCST (E.singcst (lab, c)))
	       in (ev, E.singcst (L.dummyLab, E.CSTLET env), css)
	       end
	     | f_strexp (A.StrExpFDec (funid, strdec, _, lab, _)) =
	       let val (ev1, ev2, cst) = f_funid funid
		   val (env, css) = f_strdec strdec
		   val ev   = E.freshenvvar ()
		   val ev'  = E.freshenvvar ()
		   val c1   = E.genCstEvEm (E.consEnvVar ev' lab) env lab
		   val c2   = E.CSTFUN (ev1, ev2, ev', ev, lab)
		   val env1 = E.ENVSEQ (E.ENVCST (E.singcst (lab, c1)), E.ENVCST (E.singcst (lab, c2)))
		   val env2 = E.ENVSEQ (E.ENVCST cst, env1)
	       in (ev, E.singcst (L.dummyLab, E.CSTLET env2), css)
	       end
	     | f_strexp (A.StrExpLocal (strdec, labstrexp, _, lab, _)) =
	       let val (env1, css1) = f_strdec strdec
		   val (ev, cst2, css2) = f_labstrexp labstrexp
		   val ev' = E.freshenvvar ()
		   val c   = E.genCstEvEm (E.consEnvVar ev' lab) (E.consEnvVar ev lab) lab
		   val env = E.ENVSEQ (env1, E.ENVCST cst2)
		   val cst = E.conscst (L.dummyLab, E.CSTLET env) (E.singcst (lab, c))
		   val css = E.uenvcss [css1, css2]
	       in (ev', cst, css)
	       end
	     | f_strexp (A.StrExpDots pl) =
	       let val env = f_partlist pl
		   val ev  = E.freshenvvar ()
	       in (ev, E.singcst (L.dummyLab, E.CSTLET env), E.emcss)
	       end

	   (*and f_longtycontb (A.LongTyConQual (sid, ltv, _, lab, _)) =
	       let val (idS, (ev1, _), cs1) = f_strid sid
		   val ((idG', idS'), tv3, cs2) = f_longtycontb ltv
		   val cs3 = E.uenvcs cs1 cs2
		   val cs = case extractFromLong idS' idG' of
				(NONE, SOME ety) =>
				let val l = E.getBindL ety
				    val c = E.genCstEvEm (E.INJTYP ety) (E.ENVVAR ev1) l
				in E.conscsSem (l, c) cs3 end
			      | (SOME eenv, NONE) =>
				let val l = E.getBindL eenv
				    val c = E.genCstEvEm (E.INJSTR eenv) (E.ENVVAR ev1) l
				in E.conscsSem (l, c) cs3 end
			      | _ => cs3
	       in ((E.emtyp, idS), tv3, cs)
	       end
	     | f_longtycontb (A.LongTyConId tc) =
	       let val (idT, tv, cs) = f_tycontb tc
	       in ((idT, E.emstr), tv, cs)
	       end
	     | f_longtycontb (A.LongTyConDots pl) =
	       let val (env, _, cs) = f_partlist pl
		   val tv  = T.freshtyvar ()
	       in ((E.emtyp, E.emstr), tv, cs)
	       end*)

	   (* RETURNS: (Ty.tyfvar, Env.env, Env.cst) *)
	   and f_longtyconbind longtycon =
	       case A.longtyconToLid longtycon of
		   NONE => (T.freshtyfvar (), E.emenv, E.emcst)
		 | SOME lid =>
		   let val lab = I.getLabId lid
		       val tfv = T.freshtyfvar ()
		       val (cst, env) = E.genLongEnv lid (T.consTFV tfv)
		   in (tfv, env, cst)
		   end

	   (*(* RETURNS: (Ty.tyfvar, Ty.seqvar, Env.env, Env.cst) *)
	   and f_ldatname (A.LDatName (typevarseq, longtycon, _, _)) =
	       let val (sv, tyvs, cst1) = f_typevarseq typevarseq
		   val (tfv, env, cst2) = f_longtyconbind longtycon
	       in (tfv, sv, tyvs, env, E.uenvcst [cst1, cst2])
	       end
	     | f_ldatname A.LDatNameDots =
	       let val tfv = T.freshtyfvar ()
		   val sv  = T.freshseqvar ()
	       in (tfv, sv, E.emtv, E.emenv, E.emcst)
	       end

	   (* RETURNS: (Env.env, Env.cst, Env.css) *)
	   and f_ltreadescone (A.LTReaDOne (ldatname, labtyp, _, lab, _)) =
	       let val (tfv, sv, tyvs, env, cst1) = f_ldatname ldatname
		   val (tv, cst2, css) = f_labtype labtyp
		   val c1 = E.genCstTfEm (T.TFV tfv) (T.TFC (T.SV sv, T.consV tv, lab)) lab
		   val c2 = E.CSTLET (E.ENVSEQ (E.ENVCST cst1, E.ENVSEQ (E.projTyvs tyvs, E.ENVCST cst2)))
		   (*val cs3 = checkTyVarInc (A.gettyvarLDatName dn) (A.getlabLDatName dn) (A.gettyvarLabType ty)*)
               in (env, E.conscst (lab, c1) (E.singcst (L.dummyLab, c2)), css)
               end
	     | f_ltreadescone (A.LTReaDOneDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Env.env, Env.cst, Env.css) *)
	   and f_ltreadesc (A.LTReaDesc (ltreadescs, _, lab, _)) =
               let val (envs, csts, csss) = unzipThree (map f_ltreadescone ltreadescs)
		   val env = E.uenvEnv envs
		   val cst = E.uenvcst csts
		   val css = E.uenvcss csss
		   val ev  = E.freshenvvar ()
		   val c   = E.genCstEvEm (E.consEnvVar ev lab) (E.ENVDEP (env, lab)) lab
               in (E.consEnvVar ev lab, E.conscst (L.dummyLab, c) cst, css)
	       end
	     | f_ltreadesc (A.LTReaDescDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emcst, E.emcss)
	       end*)

	   (* RETURNS: (Id.lid option) *)
	   and f_longtyconwhere longtycon =
	       case A.longtyconToLid longtycon of
		   NONE => NONE
		 | SOME lid => SOME lid

	   (* RETURNS: (Id.lid option, Ty.seqvar, Env.env, Env.cst) *)
	   and f_ldatname (A.LDatName (typevarseq, longtycon, _, _)) =
	       let val (sv, tyvs, cst1) = f_typevarseq typevarseq
		   val lidop = f_longtyconwhere longtycon
	       in (lidop, sv, tyvs, cst1)
	       end
	     | f_ldatname A.LDatNameDots =
	       let val sv  = T.freshseqvar ()
	       in (NONE, sv, E.emtv, E.emcst)
	       end

	   (* RETURNS: (Id.lid option, Ty.tyfvar, Env.cst, Env.css) *)
	   and f_ltreadescone (A.LTReaDOne (ldatname, labtyp, _, lab, _)) =
	       let val (lidop, sv, tyvs, cst1) = f_ldatname ldatname
		   val (tv, cst2, css) = f_labtype labtyp
		   val tfv = T.freshtyfvar ()
		   val c1  = E.genCstTfEm (T.TFV tfv) (T.TFC (T.SV sv, T.consV tv, lab)) lab
		   val c2  = E.CSTLET (E.ENVSEQ (E.ENVCST cst1, E.ENVSEQ (E.projTyvs tyvs, E.ENVCST cst2)))
		   (*val cs3 = checkTyVarInc (A.gettyvarLDatName dn) (A.getlabLDatName dn) (A.gettyvarLabType ty)*)
               in (lidop, tfv, E.conscst (lab, c1) (E.singcst (L.dummyLab, c2)), css)
               end
	     | f_ltreadescone (A.LTReaDOneDots pl) =
	       let val env = f_partlist pl
		   val tfv = T.freshtyfvar ()
	       in (NONE, tfv, E.singcst (L.dummyLab, E.CSTLET env), E.emcss)
	       end

	   (* RETURNS: (Env.longtyp list, Env.cst, Env.css) *)
	   and f_ltreadesc (A.LTReaDesc (ltreadescs, _, lab, _)) =
	       foldr (fn (ltreadesc, (reas, cst, css)) =>
			 case f_ltreadescone ltreadesc of
			     (SOME lid, tfv, cst0, css0) =>
			     let val longid = {lid = lid, sem = T.consTFV tfv, class = CL.consTYCON (), lab = lab}
				 val cst1 = E.uenvcst [cst, cst0]
				 val css1 = E.uenvcss [css, css0]
			     in ((longid, L.singleton lab, L.empty, CD.empty) :: reas, cst1, css1)
			     end
			   | (NONE, _, cst0, css0) => (reas, E.uenvcst [cst, cst0], E.uenvcss [css, css0]))
		     ([], E.emcst, E.emcss)
		     ltreadescs
	     | f_ltreadesc (A.LTReaDescDots pl) =
	       let val env = f_partlist pl
	       in ([], E.singcst (L.dummyLab, E.CSTLET env), E.emcss)
	       end

	   (* RETURNS: (Env.envvar, Env.cst, Env.css) *)
	   and f_labsigexp (A.LabSigExp (sigexp, _, _, lab, _)) =
	       let val (ev, cst, css) = f_sigexp sigexp
		   val ev' = E.freshenvvar ()
		   val c   = E.genCstEvEm (E.consEnvVar ev' lab) (E.consEnvVar ev lab) lab
	       in (ev', E.conscst (lab, c) cst, css)
	       end
	     | f_labsigexp (A.LabSigExpDots pl) =
	       let val ev  = E.freshenvvar ()
		   val env = f_partlist pl
	       in (ev, E.singcst (L.dummyLab, E.CSTLET env), E.emcss)
	       end

	   (* RETURNS: (Env.envvar, Env.cst, Env.css) *)
	   and f_sigexp (A.SigExpBasic (spec, _, lab, _)) =
	       let val (env, css) = f_spec spec
		   val env = E.updateILab lab env
		   val ev1 = E.freshenvvar ()
		   val ev2 = E.freshenvvar ()
		   val c1  = E.genCstEvEm (E.consEnvVar ev1 L.dummyLab) env L.dummyLab
		   (*(2010-07-03)c1 is really slowing down the code, so I had to put
		    * all these ENVDEP in the specs.
		    * Can we do that in the decs as well, as we have the same kind of
		    * constraint for 'struct' structure expressions.
		    * No we can't do that for structures because we don't always need the
		    * label of a declaration when the error is nested.
		    * I had to add this constraint because errors involving sharing can
		    * occur inside a signature and we don't need the label of the signature
		    * expression in the minimal slices.
		    * Well, we can also have unmatched errors involving only the
		    * type parts of the specification, so we need to be able to go
		    * through the specifications' constraints.  *)
		   val c2  = E.genCstEvEm (E.consEnvVar ev2 lab) (E.consEnvVar ev1 lab) lab
	       in (ev2, E.conscst (L.dummyLab, c1) (E.singcst (lab, c2)), css)
	       end
	     | f_sigexp (A.SigExpId (sigid, lab, _)) =
	       let val (ev, cst) = f_sigid sigid
		   val ev' = E.freshenvvar ()
		   val c   = E.genCstEvEm (E.consEnvVar ev' lab) (E.consEnvVar ev lab) lab
	       in (ev', E.conscst (lab, c) cst, E.emcss)
	       end
	     (*| f_sigexp (A.SigExpRea (labsigexp, rea, _, lab, _)) =
	       let val (ev1, cst1, css1) = f_labsigexp labsigexp
		   (*(2010-07-07)We shouldn't do that for the where clauses as they are in fact
		    * sequences of where clauses and here we treat them as a bloc without any
		    * precedence, which is going to pose problem at constraint solving. *)
		   val (env2, cst2, css2) = f_ltreadesc rea
		   val ev  = E.freshenvvar ()
		   val ev' = E.freshenvvar ()
		   val c1  = E.genCstEvEm (E.consEnvVar ev lab) (E.ENVSEQ (E.ENVCST cst2, env2)) lab
		   val c2  = E.CSTSIG (ev1, SOME ev', ev, NONE, lab, E.WHR)
		   val env = E.ENVSEQ (E.ENVCST (E.conscst (lab, c1) cst1), E.ENVCST (E.singcst (lab, c2)))
	       in (ev', E.singcst (L.dummyLab, E.CSTLET env), E.uenvcss [css1, css2])
	       end*)
	     | f_sigexp (A.SigExpRea (labsigexp, rea, _, lab, _)) =
	       let val (ev1, cst1, css1) = f_labsigexp labsigexp
		   val (reas, cst2, css2) = f_ltreadesc rea
		   val ev   = E.freshenvvar ()
		   val ev'  = E.freshenvvar ()
		   val cst  = E.uenvcst [cst1, cst2]
		   val css  = E.uenvcss [css1, css2]
		   val c1   = E.genCstEvEm (E.consEnvVar ev' lab) (E.consEnvVar ev1 lab) lab
		   val env1 = foldl (fn (rea, env) => E.ENVWHR (env, rea)) (E.consEnvVar ev' lab) reas
		   val env2 = E.ENVSEQ (E.ENVCST (E.conscst (lab, c1) cst), env1)
		   val c2   = E.genCstEvEm (E.consEnvVar ev lab) env2 lab
	       in (ev, E.singcst (lab, c2), css)
	       end
	     | f_sigexp (A.SigExpDots pl) =
	       let val env = f_partlist pl
		   val ev  = E.freshenvvar ()
	       in (ev, E.singcst (L.dummyLab, E.CSTLET env), E.emcss)
	       end

	   (* RETURNS: (Env.sigenv, Env.cst, Env.css) *)
	   and f_sigbindone (A.SigBindOne (sigid, labsigexp, _, lab, _)) =
	       let val (ev1, sigs, cst1) = f_sigidbind sigid
		   val (ev2, cst2, css2) = f_labsigexp labsigexp
		   val c = E.genCstEvEm (E.consEnvVar ev1 lab) (E.consEnvVar ev2 lab) lab
	       in (sigs, E.conscst (lab, c) (E.uenvcst [cst1, cst2]), css2)
	       end
	     | f_sigbindone (A.SigBindOneDots pl) =
	       let val env = f_partlist pl
	       in (E.getSigs env, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Env.sigenv, Env.cst, Env.css) *)
	   and f_sigbind (A.SigBind (sigbinds, _, _)) =
	       let val (sigss, csts, csss) = unzipThree (map f_sigbindone sigbinds)
		   val sigs = E.uenv sigss
		   val cst  = E.uenvcst csts
		   val css  = E.uenvcss csss
	       in (sigs, cst, css)
	       end
	     | f_sigbind (A.SigBindDots pl) =
	       let val env = f_partlist pl
	       in (E.getSigs env, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Env.funenv, Env.cst, Env.css) *)
	   and f_funbindone (A.FunBindO (funid, strid, labsigexp, labstrexp, _, lab, _)) =
	       let val (ev0, ev1, funs, cst1) = f_funidbind funid
		   val (ev2, strs, cst2, css2) = f_strid strid
		   val (ev3, cst3, css3) = f_labsigexp labsigexp
		   val (ev4, cst4, css4) = f_labstrexp labstrexp
		   val c1   = E.genCstEvEm (E.consEnvVar ev2 lab) (E.consEnvVar ev3 lab) lab (* strid has signature labsigexp            *)
		   val c2   = E.genCstEvEm (E.consEnvVar ev0 lab) (E.consEnvVar ev2 lab) lab (* the functor takes the strid as parameter *)
		   val c3   = E.genCstEvEm (E.consEnvVar ev1 lab) (E.consEnvVar ev4 lab) lab (* the functor returns the labstrexp        *)
		   val env0 = E.updateIFct true (E.projStrs strs) (* We mark the environment as being the parameter of a functor *)
		   val env1 = E.ENVCST (E.conscst (lab, c1) (E.uenvcst [cst2, cst3]))
		   val env2 = E.ENVSEQ (E.ENVSEQ (env1, env0), E.ENVCST cst4)
		   val cst  = E.conscst (L.dummyLab, E.CSTLET env2) (E.conscsts (lab, [c2, c3]) cst1)
		   val css  = E.uenvcss [css2, css3, css4]
	       in (funs, cst, css)
	       end
	     | f_funbindone (A.FunBindOO (funid, strid, labsigexp1, labsigexp2, labstrexp, _, lab, _)) =
	       let val (ev0, ev1, funs, cst1) = f_funidbind funid
		   val (ev2, strs, cst2, css2) = f_strid strid
		   val (ev3, cst3, css3) = f_labsigexp labsigexp1
		   val (ev4, cst4, css4) = f_labsigexp labsigexp2
		   val (ev5, cst5, css5) = f_labstrexp labstrexp
		   val c1   = E.genCstEvEm (E.consEnvVar ev2 lab) (E.consEnvVar ev3 lab) lab (* strid has signature labsigexp            *)
		   val c2   = E.genCstEvEm (E.consEnvVar ev0 lab) (E.consEnvVar ev2 lab) lab (* the functor takes the strid as parameter *)
		   val c3   = E.CSTSIG (ev4, NONE, ev5, SOME ev1, lab)                       (* the functor returns the labstrexp        *)
		   val env0 = E.updateIFct true (E.projStrs strs) (* We mark the environment as being the parameter of a functor *)
		   val env1 = E.ENVCST (E.conscst (lab, c1) (E.uenvcst [cst2, cst3]))
		   val env2 = E.ENVCST (E.uenvcst [cst4, cst5])
		   val env3 = E.ENVCST (E.conscsts (lab, [c2, c3]) cst1)
		   val env4 = E.ENVSEQ (E.ENVSEQ (env1, env0), E.ENVSEQ (env2, env3))
		   val cst  = E.singcst (L.dummyLab, E.CSTLET env4)
		   val css  = E.uenvcss [css2, css3, css4, css5]
	       in (funs, cst, css)
	       end
	     | f_funbindone (A.FunBindOT (funid, strid, labsigexp1, labsigexp2, labstrexp, _, lab, _)) =
	       let val (ev0, ev1, funs, cst1) = f_funidbind funid
		   val (ev2, strs, cst2, css2) = f_strid strid
		   val (ev3, cst3, css3) = f_labsigexp labsigexp1
		   val (ev4, cst4, css4) = f_labsigexp labsigexp2
		   val (ev5, cst5, css5) = f_labstrexp labstrexp
		   val c1   = E.genCstEvEm (E.consEnvVar ev2 lab) (E.consEnvVar ev3 lab) lab (* strid has signature labsigexp            *)
		   val c2   = E.genCstEvEm (E.consEnvVar ev0 lab) (E.consEnvVar ev2 lab) lab (* the functor takes the strid as parameter *)
		   val c3   = E.CSTSIG (ev4, SOME ev1, ev5, NONE, lab)                       (* the functor returns the labstrexp        *)
		   val env0 = E.updateIFct true (E.projStrs strs) (* We mark the environment as being the parameter of a functor *)
		   val env1 = E.ENVCST (E.conscst (lab, c1) (E.uenvcst [cst2, cst3]))
		   val env2 = E.ENVCST (E.uenvcst [cst4, cst5])
		   val env3 = E.ENVCST (E.conscsts (lab, [c2, c3]) cst1)
		   val env4 = E.ENVSEQ (E.ENVSEQ (env1, env0), E.ENVSEQ (env2, env3))
		   val cst  = E.singcst (L.dummyLab, E.CSTLET env4)
		   val css  = E.uenvcss [css2, css3, css4, css5]
	       in (funs, cst, css)
	       end
	     | f_funbindone (A.FunBindOS (funid, spec, labstrexp, _, lab, _)) =
	       let val (ev0, ev1, funs, cst1) = f_funidbind funid
		   val (env, css2) = f_spec spec
		   val (ev3, cst3, css3) = f_labstrexp labstrexp
		   val ev2  = E.freshenvvar ()
		   val env' = E.updateIFct true env (* We mark the environment as being the parameter of a functor *)
		   val c1   = E.genCstEvEm (E.consEnvVar ev2 lab) env' lab                   (* bind the specs to ev2                   *)
		   val c2   = E.genCstEvEm (E.consEnvVar ev0 lab) (E.consEnvVar ev2 lab) lab (* the functor takes the spec as parameter *)
		   val c3   = E.genCstEvEm (E.consEnvVar ev1 lab) (E.consEnvVar ev3 lab) lab (* the functor returns the labstrexp       *)
		   val env1 = E.ENVSEQ (E.ENVCST (E.singcst (lab, c1)), E.consEnvVar ev2 lab)
		   val env2 = E.ENVSEQ (env1, E.ENVCST cst3)
		   val cst  = E.conscst (L.dummyLab, E.CSTLET env2) (E.conscsts (lab, [c2, c3]) cst1)
		   val css  = E.uenvcss [css2, css3]
	       in (funs, cst, css)
	       end
	     | f_funbindone (A.FunBindOSO (funid, spec, labsigexp, labstrexp, _, lab, _)) =
	       let val (ev0, ev1, funs, cst1) = f_funidbind funid
		   val (env, css2) = f_spec spec
		   val (ev3, cst3, css3) = f_labsigexp labsigexp
		   val (ev4, cst4, css4) = f_labstrexp labstrexp
		   val ev2  = E.freshenvvar ()
		   val env' = E.updateIFct true env (* We mark the environment as being the parameter of a functor *)
		   val c1   = E.genCstEvEm (E.consEnvVar ev2 lab) env' lab                   (* bind the specs to ev2                   *)
		   val c2   = E.genCstEvEm (E.consEnvVar ev0 lab) (E.consEnvVar ev2 lab) lab (* the functor takes the spec as parameter *)
		   val c3   = E.CSTSIG (ev3, NONE, ev4, SOME ev1, lab)                       (* the functor returns the labstrexp       *)
		   val env1 = E.ENVSEQ (E.ENVCST (E.singcst (lab, c1)), E.consEnvVar ev2 lab)
		   val env2 = E.ENVCST (E.uenvcst [cst3, cst4])
		   val env3 = E.ENVCST (E.conscsts (lab, [c2, c3]) cst1)
		   val env4 = E.ENVSEQ (env2, env3)
		   val cst  = E.singcst (L.dummyLab, E.CSTLET (E.ENVSEQ (env1, env4)))
		   val css  = E.uenvcss [css2, css3, css4]
	       in (funs, cst, css)
	       end
	     | f_funbindone (A.FunBindOST (funid, spec, labsigexp, labstrexp, _, lab, _)) =
	       let val (ev0, ev1, funs, cst1) = f_funidbind funid
		   val (env, css2) = f_spec spec
		   val (ev3, cst3, css3) = f_labsigexp labsigexp
		   val (ev4, cst4, css4) = f_labstrexp labstrexp
		   val ev2  = E.freshenvvar ()
		   val env' = E.updateIFct true env (* We mark the environment as being the parameter of a functor *)
		   val c1   = E.genCstEvEm (E.consEnvVar ev2 lab) env' lab                   (* bind the specs to ev2                   *)
		   val c2   = E.genCstEvEm (E.consEnvVar ev0 lab) (E.consEnvVar ev2 lab) lab (* the functor takes the spec as parameter *)
		   val c3   = E.CSTSIG (ev3, SOME ev1, ev4, NONE, lab)                       (* the functor returns the labstrexp       *)
		   val env1 = E.ENVSEQ (E.ENVCST (E.singcst (lab, c1)), E.consEnvVar ev2 lab)
		   val env2 = E.ENVCST (E.uenvcst [cst3, cst4])
		   val env3 = E.ENVCST (E.conscsts (lab, [c2, c3]) cst1)
		   val env4 = E.ENVSEQ (env2, env3)
		   val cst  = E.singcst (L.dummyLab, E.CSTLET (E.ENVSEQ (env1, env4)))
		   val css  = E.uenvcss [css2, css3, css4]
		   (*val _    = D.printdebug2 ("[functor]")*)
	       in (funs, cst, css)
	       end
	     | f_funbindone (A.FunBindODots parts) =
	       let val env = f_partlist parts
	       in (E.getFuns env, E.emcst, E.emcss)
	       end

	   (* RETURNS: (Env.funenv, Env.cst, Env.css) *)
	   and f_funbind (A.FunBind (funbinds, _, _)) =
	       let val (funss, csts, csss) = unzipThree (map f_funbindone funbinds)
		   val funs = E.uenv funss
		   val cst  = E.uenvcst csts
		   val css  = E.uenvcss csss
	       in (E.ENVSEQ (E.ENVCST cst, E.projFuns funs), css)
	       end
	     | f_funbind (A.FunBindDots parts) =
	       let val env = f_partlist parts
	       in (env, E.emcss)
	       end

	   (* RETURNS: (Env.env, Env.cst, Env.css) *)
	   and f_sigdec (A.SigDec (sigbind, _, _)) =
	       let val (sigs, cst, css) = f_sigbind sigbind
		   val env = E.ENVSEQ (E.ENVCST cst, E.projSigs sigs)
	       in (env, css)
	       end
	     | f_sigdec (A.SigDecDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emcss)
	       end

	   (*and f_fundec (A.FunDec (fb, r, l, n)) =
	       let
		   val c = sorryCsP l "functors"
	       in (E.embas, E.emfun, E.singcsSyn c)
	       end
	     | f_fundec (A.FunDecDots pl) =
	       let
		   val (env, _, cs) = f_partlist pl
	       in (E.projBasE env, E.emfun, cs)
	       end*)

	   (* RETURNS: (Env.css) *)
	   and f_afile (A.AFile (file, _, lab, _)) =
	       let val css = if OS.FileSys.access (file, [OS.FileSys.A_READ])
				andalso
				not (OS.FileSys.isDir file)
				andalso
				not (OS.FileSys.isLink file)
			     then E.emcss
			     else E.singcss (accessCs lab)
	       in css
	       end
	     | f_afile A.AFileDots = E.emcss

	   (* RETURNS: (E.env, Env.css) *)
	   and f_smltes (A.SmlTesDec  (atopdec, _, _)) = f_atopdec atopdec
	     | f_smltes (A.SmlTesSpec (spec, _, _))    = f_specsmltes spec
	     (*(2010-06-17)The SMLTES spec is different because we need to directly close value specs
	      * and not wait to match them against structures. *)
	     | f_smltes (A.SmlTesUse  (af, _, _)) = (E.emenv, f_afile af)
	     | f_smltes (A.SmlTesSBas (af, _, _)) = (E.emenv, f_afile af)
	     | f_smltes (A.SmlTesCBas _)  = (E.emenv, E.emcss)
	     | f_smltes (A.SmlTesQuote _) = (E.emenv, E.emcss)
	     | f_smltes (A.SmlTesType (id, _, _)) = (E.ENVPTY id, E.emcss)
	     | f_smltes (A.SmlTesDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emcss)
	       end

	   (* RETURNS: (E.env, Env.css) *)
	   and f_atopdec (A.ATopDecStr strdec) = f_strdec strdec
	     | f_atopdec (A.ATopDecSig sigdec) = f_sigdec sigdec
	     | f_atopdec (A.ATopDecDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emcss)
	       end

	   (* RETURNS: (E.env, Env.css) *)
	   and f_topdecone (A.TopDecOneTes (x, _)) = f_smltes x
	     | f_topdecone (A.TopDecOneDec (x, _)) = f_atopdec x
	     | f_topdecone (A.TopDecOneDots pl)    =
	       let val env = f_partlist pl
	       in (env, E.emcss)
	       end

	   (* RETURNS: (E.env, Env.css) *)
	   and f_topdeconelist [] = (E.emenv, E.emcss)
	     | f_topdeconelist [x] = f_topdecone x
	     | f_topdeconelist (x :: xs) =
	       let val (env1, css1) = f_topdecone x
		   val (env2, css2) = f_topdeconelist xs
	       in (E.ENVSEQ (env1, env2), E.uenvcss [css1, css2])
	       end

	   (* RETURNS: (E.env, Env.css) *)
	   and f_topdec (A.TopDec xs) = f_topdeconelist xs
	     | f_topdec (A.TopDecDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emcss)
	       end

	   (* RETURNS: (E.env, Env.css) *)
	   and f_progone (A.ProgOneDec td) = f_topdec td
	     | f_progone (A.ProgOneExp (exp, v, _, lab, _)) =
	       let val (tv, cst1, css) = f_exp exp
		   val (tyvs, cst2) = f_typevarvallist (A.gettyvarExp exp)
		   val tv'  = T.freshtyvar ()
		   val c    = E.genCstTyEm (T.consV tv') (T.consV tv) lab false
		   val cst  = E.conscst (lab, c) cst1
		   val vids = E.singenv (v, [E.consBindPoly v (T.consV tv') (CL.consVAL ()) lab])
		   val env1 = E.ENVSEQ (E.ENVCST cst, E.projVids (E.closeVids vids (V.nonexpExp exp)))
		   val env2 = E.ENVSEQ (E.ENVCST cst2, E.ENVPOL (tyvs, env1))
	       (*val (idV, cs1) = closeTypedTerm (A.gettyvarExp exp) [] (E.getTyvs env2) cs0 idG'*)
	       in (env2, css)
	       end
	     | f_progone (A.ProgOneParse (s, _, lab, _)) =
	       (* Here we generate an environmnet variable because the file is not parsable
		* and it might generate binding errors. *)
	       (E.newEnvVar lab, E.singcss (E.CSSPARS (L.singleton lab, s)))
	     | f_progone (A.ProgOneFile (af, _)) = (E.emenv, f_afile af)
	     | f_progone (A.ProgOneDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emcss)
	       end

	   (* RETURNS: (E.env, Env.css) *)
	   and f_progonelist []  =  (E.emenv, E.emcss)
	     | f_progonelist [x] = f_progone x
	     | f_progonelist (x :: xs) =
	       let val (env1, css1) = f_progone x
		   val (env2, css2) = f_progonelist xs
	       in (E.ENVSEQ (env1, env2), E.uenvcss [css1, css2])
	       end

	   (* RETURNS: (E.env, Env.css) *)
	   and f_prog (A.Prog tdl) = f_progonelist tdl
	     | f_prog (A.ProgDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emcss)
	       end

	   (* RETURNS: (E.env) *)
	   and f_progfile file env1 env2 =
	       E.ENVFIL (file, env1, fn () => env2)

	   (* RETURNS: (E.env, Env.css) *)
	   and f_proglist [] = (E.emenv, E.emcss)
	     | f_proglist [(x, file, false, _)] =
	       let val (env, css) = f_prog x
	       in (f_progfile file env E.emenv, css)
	       end
	     | f_proglist [(x, file, true, _)] =
	       if benv
	       then let val _ = setBasis true
			val (env, css) = f_prog x
			val _ = setBasis false
		    in (f_progfile file env E.emenv, css)
		    end
	       else (E.emenv, E.emcss)
	     | f_proglist ((x, file, false, _) :: xs) =
	       let val (env1, css1) = f_prog x
		   val (env2, css2) = f_proglist xs
		   val env = f_progfile file env1 env2
	       in (env, E.uenvcss [css1, css2])
	       end
	     | f_proglist ((x, file, true, _) :: xs) =
	       if benv
	       then let val _ = setBasis true
			val (env1, css1) = f_prog x
			val _ = setBasis false
			val (env2, css2) = f_proglist xs
			val env = f_progfile file env1 env2
		    in (env, E.uenvcss [css1, css2])
		    end
	       else f_proglist xs

	   and f_progs (A.Progs xs) = f_proglist xs

	fun bindPcon env =
	    let val (ascid, _) = pack
		val inascid =
		    List.mapPartial
			(fn x => case I.lookupSt x ascid of
				     SOME y => SOME (y, x)
				   | _      => NONE)
			["true", "false", "nil", "ref"]
		val class0 = CL.consDA0 ()
		val class1 = CL.consDA1 ()
		fun bindOne (id, "true") =
		    let val ty1  = T.newV ()
			val ty2  = T.constybool' L.dummyLab T.BB
			val c    = E.genCstTyEm ty1 ty2 L.dummyLab false
			val bind = E.consBindPoly id ty1 class0 L.dummyLab
		    in (bind, c)
		    end
		  | bindOne (id, "false") =
		    let val ty1  = T.newV ()
			val ty2  = T.constybool' L.dummyLab T.BB
			val c    = E.genCstTyEm ty1 ty2 L.dummyLab false
			val bind = E.consBindPoly id ty1 class0 L.dummyLab
		    in (bind, c)
		    end
		  | bindOne (id, "nil") =
		    let val ty1  = T.newV ()
			val ty2  = T.constylist' (T.freshtyvar ()) L.dummyLab T.BB
			val c    = E.genCstTyEm ty1 ty2 L.dummyLab false
			val bind = E.consBindPoly id ty1 class0 L.dummyLab
		    in (bind, c)
		    end
		  | bindOne (id, "ref") =
		    let val tv   = T.freshtyvar ()
			val ty1  = T.newV ()
			val ty2  = T.consTyArrowTy (T.consV tv) (T.constyref' tv L.dummyLab T.BB) L.dummyLab T.BB
			val c    = E.genCstTyEm ty1 ty2 L.dummyLab false
			val bind = E.consBindPoly id ty1 class1 L.dummyLab
		    in (bind, c)
		    end
		  | bindOne _ = raise EH.DeadBranch "this constructor can be rebound"
		val (binds, cs) = ListPair.unzip (map bindOne inascid)
		val env' = if List.null binds
			   then env
			   else let val env1 = E.projVids (E.bindToEnv binds)
				    val cst  = E.singcsts (L.dummyLab, cs)
				in E.ENVSEQ (E.ENVSEQ (E.ENVCST cst, E.ENVPOL (E.emtv, env1)), env)
				end
	    in env'
	    end

	   val (env, css) = f_progs prog
	   val env' = bindPcon env

    in (env', css)
    end

fun generateConstraints prog nenv = generateConstraints' prog (I.emAssoc, false) nenv


(**************************************)
(*           'FULL' ANALYZE           *)
(**************************************)


fun fullConsGen progs ascid nenv =
    let val benv    = case nenv of 1 => true | 2 => true | _ => false
	val pack1   = (ascid, true)
	val pack2   = ([], false)
	val envcss1 = generateConstraints' progs pack1 nenv
	val envcss2 = buildin envcss1 ascid benv
    in envcss2
    end


end
