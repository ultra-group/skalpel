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

(******************************)
(*           BUILDIN          *)
(******************************)


fun buildin (env, css) ascid true =
    let val (typs, cst1) = NA.getTyName ascid
	val (vids, cst2) = OP.getOpType ascid
	val cst  = E.unionConstraintsList [cst1, cst2]
	val env' = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst, E.ENVPOL (E.emtv, E.updateTypenames typs (E.projVids vids)))
    in if E.isEmptyIdEnv vids andalso E.isEmptyIdEnv typs
       then (env, css)
       else (E.SEQUENCE_ENV (env', env), css)
    end
  | buildin (env, css) ascid false = (env, css)

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
							     in ((E.CSSFNAM labs)::css)
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
							     in ((E.CSSMULT labs)::css)
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

fun checkTyVarInc tyvarsbind labs tyvars =
    let val (labs, ids) =
	    foldl (fn (A.TypeVar (_, id, _, lab, _), (labs, ids)) =>
		      (L.cons lab labs, I.add id ids)
		    | (A.TypeVarDots, x) => x)
		  (labs, I.empty)
		  tyvarsbind
	fun f [] = E.emptyContextSensitiveSyntaxError
	  | f ((A.TypeVar (_, id, _, lab, _)) :: xs) =
	    if I.isin id ids
	    then f xs
	    else ((E.CSSINCL (L.cons lab labs))::(f xs))
	  | f (A.TypeVarDots :: xs) = f xs
    in f tyvars
    end

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
	    foldr (fn (x, y) => ((E.CSSFARG x)::y))
		  css
		  (selectenough1 ll1 ll2)
    in #1 (foldr (fn (ll, (css, lls)) =>
		     (foldr (fn (ll', css) => if List.length ll = List.length ll'
					      then css
					      else conserrors ll ll' css)
			    css
			    lls,
		      ll :: lls))
		 (E.emptyContextSensitiveSyntaxError, [])
		 llops)
    end

fun createDiffNbArgFuns fvalbindones =
    foldr (fn (fvalbindone, css) =>
	      E.uenvcss [createDiffNbArgFun (A.getLabsFValLab fvalbindone), css])
	  E.emptyContextSensitiveSyntaxError
	  fvalbindones

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

	fun f_partlist _ = raise EH.TODO


	   and f_scon (A.SconInt (s, v, _, lab, _)) =
	       if benv (* We bind the integer to the "Int" overloading class *)
	       then let val _   = D.printDebug 3 D.AZE ("generating constraints for A.SconInt. benv = "^Bool.toString(benv)^", lab = "^(Int.toString(L.toInt(lab))^")"))
			val sv = T.freshseqvar ()
			val tv = T.freshtyvar  ()
			val a  = E.genAccIoEm (E.consAccId (I.ID (v, lab)) (T.SV sv) (CL.consOC ()) lab) lab
			val c  = E.initTypeConstraint (T.consV tv) (T.OR (T.SV sv, T.freshidor (), T.POLY, T.CST (s, v, lab), lab)) lab
		    in (tv, E.singcsts (lab, [E.ACCESSOR_CONSTRAINT a, c]), E.emptyContextSensitiveSyntaxError)
		    end
	       else let val _   = D.printDebug 3 D.AZE ("generating constraints for A.SconInt. benv = "^Bool.toString(benv)^", lab = "^(Int.toString(L.toInt(lab))^")"))
			val tv = T.freshtyvar ()
			val c  = E.initTypeConstraint (T.consV tv) (T.constyint lab) lab
		    in (tv, E.singleConstraint (lab, c), E.emptyContextSensitiveSyntaxError)
		    end
	     | f_scon (A.SconWord (s, v, _, lab, _)) =
	       if benv (* We bind the word to the "Word" overloading class *)
	       then let val _   = D.printDebug 3 D.AZE ("generating constraints for A.SconWord. benv = "^Bool.toString(benv)^", lab = "^(Int.toString(L.toInt(lab))^")"))
			val sv = T.freshseqvar ()
			val tv = T.freshtyvar  ()
			val a  = E.genAccIoEm (E.consAccId (I.ID (v, lab)) (T.SV sv) (CL.consOC ()) lab) lab
			val c  = E.initTypeConstraint (T.consV tv) (T.OR (T.SV sv, T.freshidor (), T.POLY, T.CST (s, v, lab), lab)) lab
		    in (tv, E.singcsts (lab, [E.ACCESSOR_CONSTRAINT a, c]), E.emptyContextSensitiveSyntaxError)
		    end
	       else let val _   = D.printDebug 3 D.AZE ("generating constraints for A.SconInt. benv = "^Bool.toString(benv)^", lab = "^(Int.toString(L.toInt(lab))^")"))
			val tv = T.freshtyvar ()
			val c  = E.initTypeConstraint (T.consV tv) (T.constyword lab) lab
		    in (tv, E.singleConstraint (lab, c), E.emptyContextSensitiveSyntaxError)
		    end
	     | f_scon (A.SconReal (s, v, _, lab, _)) =
	       if benv (* We bind the real to the "Real" overloading class *)
	       then let val _   = D.printDebug 3 D.AZE ("generating constraints for A.SconReal (benv = "^Bool.toString(benv)^", s = "^s^", v = "^I.printId(v)^", lab = "^(Int.toString(L.toInt(lab))^")"))
			val sv = T.freshseqvar ()
			val tv = T.freshtyvar  ()
			val a  = E.genAccIoEm (E.consAccId (I.ID (v, lab)) (T.SV sv) (CL.consOC ()) lab) lab
			val c  = E.initTypeConstraint (T.consV tv) (T.OR (T.SV sv, T.freshidor (), T.POLY, T.CST (s, v, lab), lab)) lab
		    in (tv, E.singcsts (lab, [E.ACCESSOR_CONSTRAINT a, c]), E.emptyContextSensitiveSyntaxError)
		    end
	       else let val _   = D.printDebug 3 D.AZE ("generating constraints for A.SconReal (benv = "^Bool.toString(benv)^", s = "^s^", v = "^I.printId(v)^", lab = "^(Int.toString(L.toInt(lab))^")"))
			val tv = T.freshtyvar ()
			val c  = E.initTypeConstraint (T.consV tv) (T.constyreal lab) lab
			val _   = D.printDebug 3 D.AZE ("A.SconReal constraints generated (tv = "^Int.toString(T.tyvarToInt tv)^")")
		    in (tv, E.singleConstraint (lab, c), E.emptyContextSensitiveSyntaxError)
		    end
	     | f_scon (A.SconString (s, v, _, lab, _)) =
	       if benv (* We bind the string to the "String" overloading class *)
	       then let val _   = D.printDebug 3 D.AZE ("generating constraints for A.SconString. benv = "^Bool.toString(benv)^", lab = "^(Int.toString(L.toInt(lab))^")"))
			val sv = T.freshseqvar ()
			val tv = T.freshtyvar  ()
			val a  = E.genAccIoEm (E.consAccId (I.ID (v, lab)) (T.SV sv) (CL.consOC ()) lab) lab
			val c  = E.initTypeConstraint (T.consV tv) (T.OR (T.SV sv, T.freshidor (), T.POLY, T.CST (s, v, lab), lab)) lab
		    in (tv, E.singcsts (lab, [E.ACCESSOR_CONSTRAINT a, c]), E.emptyContextSensitiveSyntaxError)
		    end
	       else let val _   = D.printDebug 3 D.AZE ("generating constraints for A.SconString. benv = "^Bool.toString(benv)^", lab = "^(Int.toString(L.toInt(lab))^")"))
			val tv = T.freshtyvar ()
			val c  = E.initTypeConstraint (T.consV tv) (T.constystring lab) lab
		    in (tv, E.singleConstraint (lab, c), E.emptyContextSensitiveSyntaxError)
		    end
	     | f_scon (A.SconChar (s, v, _, lab, _)) =
	       if benv (* We bind the char to the "Char" overloading class *)
	       then let val _   = D.printDebug 3 D.AZE ("generating constraints for A.SconChar. benv = "^Bool.toString(benv)^", lab = "^(Int.toString(L.toInt(lab))^")"))
			val sv = T.freshseqvar ()
			val tv = T.freshtyvar  ()
			val a  = E.genAccIoEm (E.consAccId (I.ID (v, lab)) (T.SV sv) (CL.consOC ()) lab) lab
			val c  = E.initTypeConstraint (T.consV tv) (T.OR (T.SV sv, T.freshidor (), T.POLY, T.CST (s, v, lab), lab)) lab
		    in (tv, E.singcsts (lab, [E.ACCESSOR_CONSTRAINT a, c]), E.emptyContextSensitiveSyntaxError)
		    end
	       else let val _   = D.printDebug 3 D.AZE ("generating constraints for A.SconChar. benv = "^Bool.toString(benv)^", lab = "^(Int.toString(L.toInt(lab))^")"))
			val tv = T.freshtyvar ()
			val c  = E.initTypeConstraint (T.consV tv) (T.constychar lab) lab
			val bs = (Char.fromString
				      (String.translate (fn #"\\" => "\\\\" | x => Char.toString x)
							(String.substring (s, 2, (String.size s) - 3))))
			    handle Subscript => NONE
			val contextSensitiveSyntaxError = case bs of NONE => E.singcss (charBadCs lab)
					   | SOME s'_ =>
					     if size s = 4
						orelse
						(size s = 5 andalso String.isPrefix "#\"\\" s)
					     then E.emptyContextSensitiveSyntaxError
					     else E.singcss (charSizeCs lab)
		    in (tv, E.singleConstraint (lab, c), contextSensitiveSyntaxError)
		    end
	     | f_scon A.SconDots = (D.printDebug 3 D.AZE ("generating constraints for A.SconDots"); (T.freshtyvar (), E.emptyConstraint, E.emptyContextSensitiveSyntaxError))

	   (* RETURNS: (Ty.tyvar, Env.varenv, E.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
	   and f_labid (A.LabId (ident, _, lab, _)) =
	       let val (tv, vids, cst, contextSensitiveSyntaxError) = f_identpat ident
		   val tv' = T.freshtyvar ()
		   val c   = E.initTypeConstraint (T.consV tv') (T.consV tv) lab
	       in (tv', vids, E.consConstraint (lab, c) cst, contextSensitiveSyntaxError)
	       end
	     | f_labid (A.LabIdDots pl) =
	       let val env = f_partlist pl
	       in (T.freshtyvar (), E.getVids env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.envvar, Env.strenv, E.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
	   and f_strid (A.StrId (s, v, _, lab, _)) =
	       if String.isPrefix "_" s
	       then (E.freshEnvVar (), E.emstr, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       else let val ev1  = E.freshEnvVar ()
			val ev2  = E.freshEnvVar ()
			val strs = E.singenv (v, [E.consBindPoly v (E.consENVVAR ev2 lab) (CL.consSTR ()) lab])
			val c    = E.initEnvConstraint (E.ENVVAR (ev1, lab)) (E.ENVVAR (ev2, lab)) lab
		    in (ev1, strs, E.singleConstraint (lab, c), E.emptyContextSensitiveSyntaxError)
		    end
	     | f_strid A.StrIdDots =
	       (E.freshEnvVar (), E.emstr, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)

	   (* RETURNS: (Env.envvar, Env.envvar, Env.cst) *)
	   and f_funid (A.FunId (s, v, _, lab, _)) =
	       if String.isPrefix "_" s
	       then (E.freshEnvVar (), E.freshEnvVar (), E.emptyConstraint)
	       else let val lid  = I.idToLid v lab
			val ev1  = E.freshEnvVar ()
			val ev2  = E.freshEnvVar ()
			val env1 = E.consENVVAR ev1 lab
			val env2 = E.consENVVAR ev2 lab
			val a    = E.genAccIfEm (E.consAccId lid (env1, env2) (CL.consFUNC ()) lab) lab
		    in (ev1, ev2, E.singleConstraint (lab, E.ACCESSOR_CONSTRAINT a))
		    end
	     | f_funid A.FunIdDots = (E.freshEnvVar (), E.freshEnvVar (), E.emptyConstraint)

	   (* RETURNS: (Env.envvar, Env.envvar, Env.funenv, E.emptyConstraint) *)
	   and f_funidbind (A.FunId (s, v, _, lab, _)) =
	       if String.isPrefix "_" s
	       then (E.freshEnvVar (), E.freshEnvVar (), E.emfun, E.emptyConstraint)
	       else let val ev1  = E.freshEnvVar ()
			val ev2  = E.freshEnvVar ()
			val ev1' = E.freshEnvVar ()
			val ev2' = E.freshEnvVar ()
			val env1 = E.consENVVAR ev1' lab
			val env2 = E.consENVVAR ev2' lab
			val funs = E.singenv (v, [E.consBindPoly v (env1, env2) (CL.consFUNC ()) lab])
			val c1   = E.initEnvConstraint (E.ENVVAR (ev1, lab)) (E.ENVVAR (ev1', lab)) lab
			val c2   = E.initEnvConstraint (E.ENVVAR (ev2, lab)) (E.ENVVAR (ev2', lab)) lab
		    in (ev1, ev2, funs, E.singcsts (lab, [c1, c2]))
		    end
	     | f_funidbind A.FunIdDots =
	       (E.freshEnvVar (), E.freshEnvVar (), E.emfun, E.emptyConstraint)

	   (* RETURNS: (Env.envvar, Env.cst) *)
	   and f_sigid (A.SigId (s, v, _, lab, _)) =
	       if String.isPrefix "_" s
	       then (E.freshEnvVar (), E.emptyConstraint)
	       else let val lid = I.idToLid v lab
			val ev  = E.freshEnvVar ()
			val a   = E.genAccIiEm (E.consAccId lid (E.consENVVAR ev lab) (CL.consSIG ()) lab) lab
		    in (ev, E.singleConstraint (lab, E.ACCESSOR_CONSTRAINT a))
		    end
	     | f_sigid A.SigIdDots = (E.freshEnvVar (), E.emptyConstraint)

	   (* RETURNS: (Env.envvar, Env.sigenv, Env.cst) *)
	   and f_sigidbind (A.SigId (s, v, _, lab, _)) =
	       if String.isPrefix "_" s
	       then (E.freshEnvVar (), E.emsig, E.emptyConstraint)
	       else let val ev1  = E.freshEnvVar ()
			val ev2  = E.freshEnvVar ()
			val sigs = E.singenv (v, [E.consBindPoly v (E.consENVVAR ev2 lab) (CL.consSIG ()) lab])
			val c    = E.initEnvConstraint (E.consENVVAR ev1 lab) (E.consENVVAR ev2 lab) lab
		    in (ev1, sigs, E.singleConstraint (lab, c))
		    end
	     | f_sigidbind A.SigIdDots = (E.freshEnvVar (), E.emsig, E.emptyConstraint)

	   (* RETURNS: (Ty.tyvar, Env.cst) *)
	   and f_longidexp longid =
	       case A.longidToLid longid of
		   NONE => (D.printDebug 3 D.AZE "in f_longidexp - result of `A.longidToLid longid` was NONE";
			    (T.freshtyvar (), L.dummyLab, CL.newCl (), E.emptyConstraint))
		 | SOME lid =>
		   let
		       val _   = D.printDebug 3 D.AZE "in f_longidexp - result of `A.longidToLid longid` was SOME lid";
		       val lab = I.getLabId lid
		       (* NOTE: We want all the labels from lid *)
		       val tv  = T.freshtyvar ()
		       val cl  = CL.newCl ()
		       val a   = E.genAccIvEm (E.consAccId lid (T.consV tv) cl lab) lab
		       val _   = D.printDebug 3 D.AZE ("               - lab = "^Int.toString(L.toInt lab)^", tv = "^Int.toString(T.tyvarToInt tv));
		   in (tv, lab, cl, E.singleConstraint (lab, E.ACCESSOR_CONSTRAINT a))
		   end

	   (* RETURNS: (Ty.tyvar, Env.varenv, E.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
	   and f_pconpat (A.PconBool (s, v, reg, lab, nxt)) =
	       if getBasis ()
	       then f_identpat (A.Ident (s, v, reg, lab, nxt))
	       else let val tv = T.freshtyvar ()
			val c  = E.initTypeConstraint (T.consV tv) (T.constybool lab) lab
		    in (tv, E.emvar, E.singleConstraint (lab, c), E.singcss (reboundCs lab s))
		    end
	     | f_pconpat (A.PconRef (s, v, reg, lab, nxt)) =
	       if getBasis ()
	       then f_identpat (A.Ident (s, v, reg, lab, nxt))
	       else let val tv  = T.freshtyvar ()
			val tv' = T.freshtyvar ()
			val c   = E.initTypeConstraint (T.consV tv) (T.consTyArrowTy (T.consV tv') (T.constyref tv' lab) lab T.OT) lab
		    in (tv, E.emvar, E.singleConstraint (lab, c), E.singcss (reboundCs lab s))
		    end
	     | f_pconpat (A.PconNil (s, v, reg, lab, nxt)) =
	       if getBasis ()
	       then f_identpat (A.Ident (s, v, reg, lab, nxt))
	       else let val tv  = T.freshtyvar ()
			val tv' = T.freshtyvar ()
			val c   = E.initTypeConstraint (T.consV tv) (T.constylist tv' lab) lab
		    in (tv, E.emvar, E.singleConstraint (lab, c), E.singcss (reboundCs lab s)) end
	     | f_pconpat A.PconDots = (T.freshtyvar (), E.emvar, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)

	   (* RETURNS: (Ty.tyvar, Env.varenv, E.emptyConstraint, Env.emptyContextSensitiveSyntaxError) *)
	   and f_identpat (A.Ident (s, id, _, lab, _)) =
	       if String.isPrefix "_" s
	       then (T.freshtyvar (), E.emvar, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       else let val tv1  = T.freshtyvar ()
			val tv2  = T.freshtyvar ()
			val vids = E.singenv (id, [E.consBindPoly id (T.consV tv2) (CL.consVAL ()) lab])
			val c    = E.initTypeConstraint (T.consV tv1) (T.consV tv2) lab
		    in (tv1, vids, E.singleConstraint (lab, c), E.emptyContextSensitiveSyntaxError)
		    end
	     | f_identpat (A.IdentPcon pc) = f_pconpat pc
	     | f_identpat A.IdentDots = (T.freshtyvar (), E.emvar, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)

	   (* RETURNS: (Ty.tyvar, Env.env, Env.emptyContextSensitiveSyntaxError) *)
	   and f_longidpat (A.LongIdId ident) = f_identpat ident
	     | f_longidpat longid =
	       let val (tv, lab, cl, cst) = f_longidexp longid
		   val c = E.initClassConstraint cl (CL.consCO0 ()) lab
	       in (tv, E.emvar, E.consConstraint (lab, c) cst, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.envvar, Env.cst) *)
	   and f_longstrid longstrid =
	       case A.longstridToLid longstrid of
		   NONE => (E.freshEnvVar (), E.emptyConstraint)
		 | SOME lid =>
		   let val lab = I.getLabId lid
		       (* NOTE: We want all the labels from lid *)
		       val ev  = E.freshEnvVar ()
		       val a   = E.genAccIsEm (E.consAccId lid (E.consENVVAR ev lab) (CL.consSTR ()) lab) lab
		   in (ev, E.singleConstraint (lab, E.ACCESSOR_CONSTRAINT a))
		   end

	   (* RETURNS: (Ty.tyfvar, Env.cst) *)
	   and f_longtycon longtycon =
	       case A.longtyconToLid longtycon of
		   NONE => (T.freshtyfvar (), E.emptyConstraint)
		 | SOME lid =>
		   let val lab = I.getLabId lid
		       val tfv = T.freshtyfvar ()
		       val a   = E.genAccItEm (E.consAccId lid (T.consTFV tfv) (CL.consTYCON ()) lab) lab
		   in (tfv, E.singleConstraint (lab, E.ACCESSOR_CONSTRAINT a))
		   end

	   (* RETURNS: (Ty.tyvar, Env.cst, Env.css) *)
	   and f_labexp (A.LabExp (exp, _, _, lab, _)) =
	       let
		   val _   = D.printDebug 2 D.AZE ("generating constraints for A.LabExp (lab = "^(Int.toString(L.toInt(lab))^")"))
		   val (tv, cst, contextSensitiveSyntaxError) = f_exp exp
		   val tv' = T.freshtyvar ()
		   val c   = E.initTypeConstraint (T.consV tv') (T.consV tv) lab
		   val _   = D.printDebug 2 D.AZE ("A.LabExp constraints generated (tv = "^Int.toString(T.tyvarToInt tv)^", "
						   ^"tv' = "^Int.toString(T.tyvarToInt tv')
						   ^", lab = "^(Int.toString(L.toInt(lab))^")"))
	       in (tv', E.consConstraint (lab, c) cst, contextSensitiveSyntaxError)
	       end
	     | f_labexp (A.LabExpDots pl) =
	       let val tv  = T.freshtyvar ()
		   val env = f_partlist pl
	       in (tv, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.labvar, (string, Label.label) option, Env.cst) *)
	   and f_tylab (A.TyLab (s, _, lab, _)) =
	       let val lv = T.freshlabvar ()
		   val c  = E.initLabelConstraint (T.LV lv) (T.LC (s, lab)) lab
	       in (lv, SOME (s, lab), E.singleConstraint (lab, c))
	       end
	     | f_tylab A.TyLabDots =
	       (T.freshlabvar (), NONE, E.emptyConstraint)

	   (* RETURNS: (((string, Label.label) option, Ty.rowvar), Env.emptyConstraint, Env.emptyContextSensitiveSyntaxError) *)
	   and f_exprow (A.ExpRow (tylab, labexp, _, _, lab, _)) =
	       let val (lv, lop, cst1)  = f_tylab tylab
		   val (tv, cst2, css2) = f_labexp labexp
		   val rv = T.freshrowvar ()
		   val c  = E.initRowConstraint (T.RV rv) (T.RC (T.LV lv, T.consV tv, lab)) lab
	       in ((lop, rv), E.consConstraint (lab, c) (E.unionConstraintsList [cst1, cst2]), css2)
	       end
	     | f_exprow (A.ExpRowDots pl) =
	       let val rv  = T.freshrowvar ()
		   val env = f_partlist pl
	       in ((NONE, rv), E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.tyvar, Env.cst, Env.css) *)
	   and f_seqexp (A.SeqExp (labexps, labexp, _, _, lab, _)) =
	       let val (tv2, cst2, css2) = f_labexp labexp
		   val (_, csts, csss) = unzipThree (map f_labexp labexps)
		   val cst = E.unionConstraintsList csts
		   val contextSensitiveSyntaxError = E.uenvcss csss
		   val tv  = T.freshtyvar ()
		   val c   = E.initTypeConstraint (T.consV tv) (T.consV tv2) lab
	       in (tv, E.consConstraint (lab, c) (E.unionConstraintsList [cst, cst2]), E.uenvcss [contextSensitiveSyntaxError, css2])
	       end
	     | f_seqexp (A.SeqExpSl (parts, labexp, _, lab, _)) =
	       let val env1 = f_partlist parts
		   val (tv2, cst2, css2) = f_labexp labexp
		   val tv = T.freshtyvar ()
		   val c  = E.initTypeConstraint (T.consV tv) (T.consV tv2) lab
	       in (tv, E.conscsts (lab, [E.LET_CONSTRAINT env1, c]) cst2, css2)
	       end
	     | f_seqexp (A.SeqExpDots pl) =
	       let val tv  = T.freshtyvar ()
		   val env = f_partlist pl
	       in (tv, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.tyvar, Env.cst, Env.css) *)
	   and f_atexp (A.AtExpId id) =
	       let
		   val _   = D.printDebug 3 D.AZE ("generating constraints for A.AtExpId (calling f_longidexp with parameter id)")
		   val (tv, _, _, cst) = f_longidexp id
		   val _   = D.printDebug 3 D.AZE ("A.AtExpId constraints generated (tv = "^Int.toString(T.tyvarToInt tv)^")")
	       in (tv, cst, E.emptyContextSensitiveSyntaxError)
	       end
	     | f_atexp (A.AtExpScon sc) = (D.printDebug 3 D.AZE ("generating constraints for A.AtExpScon"); f_scon sc)
	     | f_atexp (A.AtExpTuple (expl, _, lab, _)) =
	       let val _   = D.printDebug 3 D.AZE ("generating constraints for A.AtExpTuple (lab = "^(Int.toString(L.toInt(lab))^")"))
		   val tv = T.freshtyvar ()
		   val (tvl, csts, csss) = unzipThree (map f_labexp expl)
		   val cst = E.unionConstraintsList csts
		   val contextSensitiveSyntaxError = E.uenvcss csss
		   val c   = E.initTypeConstraint (T.consV tv) (T.constytuple tvl lab) lab
	       in (tv, E.consConstraint (lab, c) cst, contextSensitiveSyntaxError)
	       end
	     | f_atexp (A.AtExpRecord (exprows, _, _, lab, _)) =
	       let val _   = D.printDebug 3 D.AZE ("generating constraints for A.AtExpRecord (lab = "^(Int.toString(L.toInt(lab))^")"))
		   val tv = T.freshtyvar ()
		   val (xs, csts, csss) = unzipThree (map f_exprow exprows)
		   val (lops, rts) = ListPair.unzip xs
		   val cst  = E.unionConstraintsList csts
		   val contextSensitiveSyntaxError  = E.uenvcss csss
		   val css' = consCSMlab (List.mapPartial (fn x => x) lops)
		   val c    = E.initTypeConstraint (T.consV tv) (T.constyrecord rts (T.noflex ()) lab) lab
	       in (tv, E.consConstraint (lab, c) cst, E.uenvcss [contextSensitiveSyntaxError, css'])
	       end
	     | f_atexp (A.AtExpSlRec (exprows, _, lab, _)) =
	       let val _   = D.printDebug 3 D.AZE ("generating constraints for A.AtExpSlRec (lab = "^(Int.toString(L.toInt(lab))^")"))
		   val tv  = T.freshtyvar ()
		   val (xs, csts, csss) = unzipThree (map f_exprow exprows)
		   val (lops, rts) = ListPair.unzip xs
		   val cst  = E.unionConstraintsList csts
		   val contextSensitiveSyntaxError  = E.uenvcss csss
		   val contextSensitiveSyntaxError' = consCSMlab (List.mapPartial (fn x => x) lops)
		   (* We condiser that as a widlcard because it is incomplete: *)
		   val c    = E.initTypeConstraint (T.consV tv) (T.constyrecord rts (T.consflex lab) lab) lab
	       in (tv, E.consConstraint (lab, c) cst, E.uenvcss [contextSensitiveSyntaxError, contextSensitiveSyntaxError'])
	       end
	     | f_atexp (A.AtExpLet (decs, labexp, _, lab, _)) =
	       let val _   = D.printDebug 3 D.AZE ("generating constraints for A.AtExpLet (lab = "^(Int.toString(L.toInt(lab))^")"))
		   val tv   = T.freshtyvar ()
		   val (env1, css1) = f_decs decs
		   val (tv2, cst2, css2) = f_labexp labexp
		   val env = E.envsToSeq [env1, E.CONSTRAINT_ENV cst2]
		   val c   = E.initTypeConstraint (T.consV tv) (T.consV tv2) lab
	       in (tv, E.consConstraint (L.dummyLab, E.LET_CONSTRAINT env) (E.singleConstraint (lab, c)), E.uenvcss [css1, css2])
	       end
	     | f_atexp (A.AtExpDLet (decs, seqexp, _, lab, _)) =
	       let val _   = D.printDebug 3 D.AZE ("generating constraints for A.AtExpDLet (lab = "^(Int.toString(L.toInt(lab))^")"))
		   val tv   = T.freshtyvar ()
		   val (env1, contextSensitiveSyntaxError1) = f_decs decs
		   val (tv2, cst2, contextSensitiveSyntaxError2) = f_seqexp seqexp
		   val env = E.envsToSeq [env1, E.CONSTRAINT_ENV cst2]
		   val c   = E.initTypeConstraint (T.consV tv) (T.consV tv2) lab
	       in (tv, E.consConstraint (L.dummyLab, E.LET_CONSTRAINT env) (E.singleConstraint (lab, c)), E.uenvcss [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2])
	       end
	     | f_atexp (A.AtExpParen (labexp, _, _, lab, _)) =
	       let val _   = D.printDebug 3 D.AZE ("generating constraints for A.AtExpParen (lab = "^(Int.toString(L.toInt(lab))^")"))
		   val (tv, cst, contextSensitiveSyntaxError) = f_labexp labexp
		   val tv' = T.freshtyvar ()
		   val c   = E.initTypeConstraint (T.consV tv') (T.consV tv) lab
	       in (tv', E.consConstraint (lab, c) cst, contextSensitiveSyntaxError)
	       end
	     | f_atexp (A.AtExpList (labexps, _, lab, _)) =
	       let val _   = D.printDebug 3 D.AZE ("generating constraints for A.AtExpList (lab = "^(Int.toString(L.toInt(lab))^")"))
		   val tv   = T.freshtyvar ()
		   val tv'  = T.freshtyvar ()
		   val (tvs, csts, csss) = unzipThree (map f_labexp labexps)
		   val cst  = E.unionConstraintsList csts
		   val contextSensitiveSyntaxError  = E.uenvcss csss
		   val cs'  = map (fn x => E.initTypeConstraint (T.consV tv') (T.consV x) lab) tvs
		   val c    = E.initTypeConstraint (T.consV tv) (T.constylist tv' lab) lab
	       in (tv, E.conscsts (lab, c :: cs') cst, contextSensitiveSyntaxError)
	       end
	     | f_atexp (A.AtExpProj (tylab, _, _, lab, _)) =
	       let val _   = D.printDebug 3 D.AZE ("generating constraints for A.AtExpProj (lab = "^(Int.toString(L.toInt(lab))^")"))
		   val (lv, _, cst) = f_tylab tylab
		   val tv  = T.freshtyvar ()
		   val tv' = T.freshtyvar ()
		   val rv  = T.freshrowvar ()
		   val ty  = (T.constyrecord [rv] (T.consflex lab) lab)
		   val c1  = E.initTypeConstraint (T.consV tv) (T.consTyArrowTy ty (T.consV tv') lab T.OT) lab
		   val c2  = E.initRowConstraint (T.RV rv) (T.RC (T.LV lv, T.consV tv', lab)) lab
	       in (tv, E.conscsts (lab, [c1, c2]) cst, E.emptyContextSensitiveSyntaxError)
	       end
	     | f_atexp (A.AtExpSeq (seqseq, _, lab, _)) =
	       let val _   = D.printDebug 3 D.AZE ("generating constraints for A.AtExpSeq (lab = "^(Int.toString(L.toInt(lab))^")"))
		   val (tv, cst, contextSensitiveSyntaxError) = f_seqexp seqseq
		   val tv' = T.freshtyvar ()
		   val c   = E.initTypeConstraint (T.consV tv') (T.consV tv) lab
	       in (tv', E.consConstraint (lab, c) cst, contextSensitiveSyntaxError)
	       end
	     | f_atexp (A.AtExpQuote (quotes, _, lab, _)) =
	       let val _   = D.printDebug 3 D.AZE ("generating constraints for A.AtExpQuote (lab = "^(Int.toString(L.toInt(lab))^")"))
		   val (tvs, csts, csss) = unzipThree (map f_quote quotes)
		   val tv  = T.freshtyvar ()
		   val tv' = T.freshtyvar ()
		   val cst = E.unionConstraintsList csts
		   val contextSensitiveSyntaxError = E.uenvcss csss
		   val cs' = map (fn x => E.initTypeConstraint (T.consV tv') (T.consV x) lab) tvs
		   val c   = E.initTypeConstraint (T.consV tv) (T.constylist tv' lab) lab
	       in (tv, E.conscsts (lab, c :: cs') cst, contextSensitiveSyntaxError)
	       end
	     | f_atexp (A.AtExpDots pl) =
	       let val _   = D.printDebug 3 D.AZE ("generating constraints for A.AtExpDots")
		   val tv  = T.freshtyvar ()
		   val env = f_partlist pl
	       in (tv, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
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
		   val c   = E.initTypeConstraint (T.consV tv1) (T.constyfrag tv2 lab) lab
	       in (tv1, E.singleConstraint (lab, c), E.emptyContextSensitiveSyntaxError)
	       end
	     | f_quote (A.Antiquote(exp, _, lab, _)) =
	       let val (tv, cst, contextSensitiveSyntaxError) = f_exp exp
		   val tv' = T.freshtyvar ()
		   val c   = E.initTypeConstraint (T.consV tv') (T.constyfrag tv lab) lab
	       in (tv', E.consConstraint (lab, c) cst, contextSensitiveSyntaxError)
	       end
	     | f_quote (A.QuoteDots pl) =
	       let val tv  = T.freshtyvar ()
		   val env = f_partlist pl
	       in (tv, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.tyvar, Env.cst, Env.css) *)
	   and f_exp (A.ExpAtExp atexp) =
	       (D.printDebug 2 D.AZE "generating constraints for A.ExpAtExp (just returns result of atexp applied to f_atexp)";
		f_atexp atexp)
	     | f_exp (A.ExpFn (match, _, lab, _)) =
	       let
		   val _ = D.printDebug 2 D.AZE ("generating constraints for A.ExpFn (lab = "^(Int.toString(L.toInt(lab))^")"))
		   val (tvs, cst, contextSensitiveSyntaxError) = f_match match
		   val tv = T.freshtyvar ()
		   val cs = map (fn x => E.initTypeConstraint (T.consV tv) (T.consV x) lab) tvs
	       in (tv, E.conscsts (lab, cs) cst, contextSensitiveSyntaxError)
	       end
	     | f_exp (A.ExpApp (exp, atexp, _, _, _, lab, _)) =
	       let
		   val _ = D.printDebug 2 D.AZE ("generating constraints for A.ExpApp (lab = "^(Int.toString(L.toInt(lab))^")"))
		   val (tv1, cst1, contextSensitiveSyntaxError1) = f_exp exp
		   val (tv2, cst2, contextSensitiveSyntaxError2) = f_atexp atexp
		   val tv  = T.freshtyvar ()
		   val c   = E.initTypeConstraint (T.consV tv1) (T.constyarrow tv2 tv lab) lab
	       in (tv, E.consConstraint (lab, c) (E.unionConstraintsList [cst1, cst2]), E.uenvcss [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2])
	       end
	     | f_exp (A.ExpCase (labexp, match, _, _, lab, _)) =
	       let
		   val _ = D.printDebug 2 D.AZE "generating constraints for A.ExpCase"
		   val tv  = T.freshtyvar ()
		   val tv' = T.freshtyvar ()
		   val (tv1, cst1, contextSensitiveSyntaxError1) = f_labexp labexp
		   val (tvs, cst2, contextSensitiveSyntaxError2) = f_match match
		   val cs  = map (fn x => E.initTypeConstraint (T.consV tv') (T.consV x) lab) tvs
		   val c   = E.initTypeConstraint (T.consV tv') (T.constyarrow tv1 tv lab) lab
	       in (tv, E.conscsts (lab, c :: cs) (E.unionConstraintsList [cst1, cst2]), E.uenvcss [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2])
	       end
	     | f_exp (A.ExpConsList (id, labexp1, labexp2, reg, lab, nxt)) =
	       if benv
	       then f_exp (A.ExpOp ("::", id, labexp1, labexp2, reg, lab, nxt))
	       else let
			val _ = D.printDebug 2 D.AZE "generating constraints for A.ExpConsList"
			val (tv1, cst1, contextSensitiveSyntaxError1) = f_labexp labexp1
			val (tv2, cst2, contextSensitiveSyntaxError2) = f_labexp labexp2
			val tv = T.freshtyvar ()
			val c1 = E.initTypeConstraint (T.consV tv)  (T.constylist tv1 lab) lab
			val c2 = E.initTypeConstraint (T.consV tv2) (T.constylist tv1 lab) lab
		    in (tv, E.conscsts (lab, [c1, c2]) (E.unionConstraintsList [cst1, cst2]), E.uenvcss [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2])
		    end
	     | f_exp (A.ExpOp (st, id, labexp1, labexp2, _, lab, _)) =
	       let
		   val _ = D.printDebug 2 D.AZE ("generating constraints for A.ExpOp (st = \"" ^ st ^ "\", lab="^Int.toString(L.toInt lab)^")")
		   val _ = D.printDebug 3 D.AZE  ("**********  ExpOp left hand side...  **********")
		   val (tv1, cst1, contextSensitiveSyntaxError1) = f_labexp labexp1
		   val _ = D.printDebug 3 D.AZE  ("**********  ExpOp right hand side... **********")
		   val (tv2, cst2, contextSensitiveSyntaxError2) = f_labexp labexp2
		   val _ = D.printDebug 3 D.AZE ("ExpOp type variables are tv1 = "^(Int.toString(T.tyvarToInt(tv1)))^", tv2 = "^(Int.toString(T.tyvarToInt(tv2))))
		   val ty  = T.newV ()
		   val ti  = T.constytuple [tv1, tv2] lab
		   val tvo = T.freshtyvar ()
		   val c   = E.initTypeConstraint ty (T.consTyArrowTy ti (T.consV tvo) lab T.OT) lab
		   val a   = E.genAccIvEm (E.consAccId (I.ID (id, lab)) ty (CL.consVAL ()) lab) lab
		   val _ = D.printDebug 3 D.AZE ("ExpOp constraints generated\n")
	       in (tvo, E.conscsts (lab, [c, E.ACCESSOR_CONSTRAINT a]) (E.unionConstraintsList [cst1, cst2]), E.uenvcss [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2])
	       end
	     | f_exp (A.ExpOr (labexp1, labexp2, _, lab, _)) =
	       let
		   val _ = D.printDebug 2 D.AZE "generating constraints for A.ExpOr"
		   val (tv1, cst1, contextSensitiveSyntaxError1) = f_labexp labexp1
		   val (tv2, cst2, contextSensitiveSyntaxError2) = f_labexp labexp2
		   val tvo = T.freshtyvar ()
		   val c1  = E.initTypeConstraint (T.consV tvo) (T.constybool lab) lab
		   val c2  = E.initTypeConstraint (T.consV tv1) (T.constybool lab) lab
		   val c3  = E.initTypeConstraint (T.consV tv2) (T.constybool lab) lab
	       in (tvo, E.conscsts (lab, [c1, c2, c3]) (E.unionConstraintsList [cst1, cst2]), E.uenvcss [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2])
	       end
	     | f_exp (A.ExpAnd (labexp1, labexp2, _, lab, _)) =
	       let
		   val _ = D.printDebug 2 D.AZE "generating constraints for A.ExpAnd"
		   val (tv1, cst1, contextSensitiveSyntaxError1) = f_labexp labexp1
		   val (tv2, cst2, contextSensitiveSyntaxError2) = f_labexp labexp2
		   val tvo = T.freshtyvar ()
		   val c1  = E.initTypeConstraint (T.consV tvo) (T.constybool lab) lab
		   val c2  = E.initTypeConstraint (T.consV tv1) (T.constybool lab) lab
		   val c3  = E.initTypeConstraint (T.consV tv2) (T.constybool lab) lab
	       in (tvo, E.conscsts (lab, [c1, c2, c3]) (E.unionConstraintsList [cst1, cst2]), E.uenvcss [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2])
	       end
	     | f_exp (A.ExpTyped (labexp, labtyp, _, lab, _)) =
	       let
		   val _ = D.printDebug 2 D.AZE "generating constraints for A.ExpTyped"
		   val tv = T.freshtyvar ()
		   val (tv1, cst1, contextSensitiveSyntaxError1) = f_labexp labexp
		   val (tv2, cst2, contextSensitiveSyntaxError2) = f_labtype labtyp
		   val c1 = E.initTypeConstraint (T.consV tv) (T.consV tv1) lab
		   val c2 = E.initTypeConstraint (T.consV tv) (T.consV tv2) lab
	       in (tv, E.conscsts (lab, [c1, c2]) (E.unionConstraintsList [cst1, cst2]), E.uenvcss [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2])
	       end
	     | f_exp (A.ExpIte (labexp1, labexp2, labexp3, _, lab, _)) =
	       let
		   val _   = D.printDebug 2 D.AZE ("generating constraints for A.ExpIte")
		   (* get the type variables, constraint on types and E.ocss *)
		   val (tv1, cst1, contextSensitiveSyntaxError1) = f_labexp labexp1
		   val (tv2, cst2, contextSensitiveSyntaxError2) = f_labexp labexp2
		   val (tv3, cst3, contextSensitiveSyntaxError3) = f_labexp labexp3

		   (* constrain the condition of the if statement to be of type bool*)
		   val c1  = E.initTypeConstraint (T.consV tv1) (T.constybool lab) lab

		   (* generate a fresh type variable and constrain the true/false branches
		    * of the if statemnt to be of the same type *)
		   val tv = T.freshtyvar ()
		   val c2  = E.initTypeConstraint (T.consV tv) (T.consV tv2) lab
		   val c3  = E.initTypeConstraint (T.consV tv) (T.consV tv3) lab

		   val cst = E.conscsts (lab, [c1, c2, c3]) (E.unionConstraintsList [cst1, cst2, cst3])
		   val contextSensitiveSyntaxError = E.uenvcss [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2, contextSensitiveSyntaxError3]
		   val _   = D.printDebug 3 D.AZE ("A.ExpIte constraints generated\n")
	       in (tv, cst, contextSensitiveSyntaxError)
	       end
	     | f_exp (A.ExpWhile (labexp1, labexp2, _, _, lab, _)) =
	       let
		   val _ = D.printDebug 2 D.AZE "generating constraints for A.ExpWhile"
		   val tv = T.freshtyvar ()
		   val (tv1, cst1, contextSensitiveSyntaxError1) = f_labexp labexp1
		   val (tv2, cst2, contextSensitiveSyntaxError2) = f_labexp labexp2
		   val c1 = E.initTypeConstraint (T.consV tv1) (T.constybool lab) lab
		   val c2 = E.initTypeConstraint (T.consV tv) (T.constytuple [] lab) lab
	       in (tv, E.conscsts (lab, [c1, c2]) (E.unionConstraintsList [cst1, cst2]), E.uenvcss [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2])
	       end
	     | f_exp (A.ExpRaise (labexp, _, lab, _)) =
	       let
		   val _ = D.printDebug 2 D.AZE "generating constraints for A.ExpRaise"
		   val (tv, cst, contextSensitiveSyntaxError) = f_labexp labexp
		   val tv' = T.freshtyvar ()
		   val c   = E.initTypeConstraint (T.consV tv) (T.constyexception lab) lab
	       in (tv', E.consConstraint (lab, c) cst, contextSensitiveSyntaxError)
	       end
	     | f_exp (A.ExpHandle (labexp, match, _, lab, _)) =
	       let
		   val _ = D.printDebug 2 D.AZE "generating constraints for A.ExpHandle"
		   val (tv1, cst1, contextSensitiveSyntaxError1) = f_labexp labexp
		   val (tvs, cst2, contextSensitiveSyntaxError2) = f_match match
		   val ty  = T.newV ()
		   val tv  = T.freshtyvar ()
		   val cs  = map (fn x => E.initTypeConstraint ty (T.consV x) lab) tvs
		   val c1  = E.initTypeConstraint ty (T.consTyArrowTy (T.constyexception lab) (T.consV tv1) lab T.OT) lab
		   val c2  = E.initTypeConstraint (T.consV tv) (T.consV tv1) lab
	       in (tv, E.conscsts (lab, c1 :: c2 :: cs) (E.unionConstraintsList [cst1, cst2]), E.uenvcss [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2])
	       end
	     | f_exp (A.ExpDots pl) =
	       let
		   val _ = D.printDebug 2 D.AZE "generating constraints for A.ExpDots"
		   val tv  = T.freshtyvar ()
		   val env = f_partlist pl
	       in (tv, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.tyvar list, Env.cst, Env.css) *)
	   and f_match (A.Match (mrules, _, _)) =
	       let val (tvs, csts, csss) = unzipThree (map f_mrule mrules)
		   val cst = E.unionConstraintsList csts
		   val contextSensitiveSyntaxError = E.uenvcss csss
	       in (tvs, cst, contextSensitiveSyntaxError)
	       end
	     | f_match (A.MatchDots pl) =
	       let val env = f_partlist pl
	       in ([], E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.tyvar, Env.cst, Env.css) *)
	   and f_mrule (A.Mrule (labpat, labexp, _, lab, _)) =
	       let val tv  = T.freshtyvar ()
		   val (tv1, vids, cst1, contextSensitiveSyntaxError1) = f_labpat labpat
		   val (tv2, cst2, contextSensitiveSyntaxError2) = f_labexp labexp
		   val env = E.SEQUENCE_ENV (E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst1, E.projVids (E.toMonoVids vids (L.singleton lab))), E.CONSTRAINT_ENV cst2)
		   val c   = E.initTypeConstraint (T.consV tv) (T.constyarrow tv1 tv2 lab) lab
		   val cst = E.consConstraint (L.dummyLab, E.LET_CONSTRAINT env) (E.singleConstraint (lab, c))
		   val contextSensitiveSyntaxError = E.uenvcss [clearRebound contextSensitiveSyntaxError1, contextSensitiveSyntaxError2]
	       in (tv, cst, contextSensitiveSyntaxError)
	       end
	     | f_mrule (A.MruleDots pl) =
	       let val tv  = T.freshtyvar ()
		   val env = f_partlist pl
	       in (tv, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* for type variables (such as 'a in a signature) *)
	   (* RETURNS: (int option, Ty.tyvar, Env.cst) *)
	   and f_typevar (A.TypeVar (st, id, _, lab, _)) =
	       let val _   = D.printDebug 2 D.AZE ("in f_typevar - generating constraints for A.TypeVar (lab = "^Int.toString(L.toInt(lab))^")")
		   val tv = T.freshtyvar ()
		   val a  = E.genAccIeEm (E.consAccId (I.ID (id, lab)) tv (CL.consTYVAR ()) lab) lab
	       in (SOME id, tv, E.singleConstraint (lab, E.ACCESSOR_CONSTRAINT a))
	       end
	     | f_typevar A.TypeVarDots =
	       let val tv = T.freshtyvar ()
	       in (NONE, tv, E.emptyConstraint)
	       end

	   (* RETURNS: (Ty.tyvar, Env.cst, Env.css) *)
	   and f_labtype (A.LabType (typ, _, lab, _)) =
	       let val (tv, cst, contextSensitiveSyntaxError) = f_type typ
		   val tv' = T.freshtyvar ()
		   val c   = E.initTypeConstraint (T.consV tv') (T.consV tv) lab
	       in (tv', E.consConstraint (lab, c) cst, contextSensitiveSyntaxError)
	       end
	     | f_labtype (A.LabTypeDots pl) =
	       let val tv  = T.freshtyvar ()
		   val env = f_partlist pl
	       in (tv, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (((string, Label.label) option, Ty.rowvar), Env.emptyConstraint, Env.emptyContextSensitiveSyntaxError) *)
	   and f_tyrow (A.TyRow (tylab, labtyp, _, lab, _)) =
	       let val (lv, lop, cst1)  = f_tylab tylab
		   val (tv, cst2, contextSensitiveSyntaxError2) = f_labtype labtyp
		   val rv = T.freshrowvar ()
		   val c  = E.initRowConstraint (T.RV rv) (T.RC (T.LV lv, T.consV tv, lab)) lab
	       in ((lop, rv), E.consConstraint (lab, c) (E.unionConstraintsList [cst1, cst2]), contextSensitiveSyntaxError2)
	       end
	     | f_tyrow (A.TyRowDots pl) =
	       let val rv  = T.freshrowvar ()
		   val env = f_partlist pl
	       in ((NONE, rv), E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.tyvar, Env.cst, Env.css) *)
	   and f_type (A.TypeOneVar tyvar) =
	       let val (_, tv, cst) = f_typevar tyvar
	       in (tv, cst, E.emptyContextSensitiveSyntaxError)
	       end
	     | f_type (A.TypeArrow (labtyp1, labtyp2, _, lab, _)) =
	       let val (tv1, cst1, contextSensitiveSyntaxError1) = f_labtype labtyp1
		   val (tv2, cst2, contextSensitiveSyntaxError2) = f_labtype labtyp2
		   val tv = T.freshtyvar ()
		   val c  = E.initTypeConstraint (T.consV tv) (T.constyarrow tv1 tv2 lab) lab
	       in (tv, E.consConstraint (lab, c) (E.unionConstraintsList [cst1, cst2]), E.uenvcss [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2])
	       end
	     | f_type (A.TypeTuple (labtyps, _, lab, _)) =
	       let val (tvs, csts, csss) = unzipThree (map f_labtype labtyps)
		   val tv  = T.freshtyvar ()
		   val cst = E.unionConstraintsList csts
		   val contextSensitiveSyntaxError = E.uenvcss csss
		   val c   = E.initTypeConstraint (T.consV tv) (T.constytuple tvs lab) lab
	       in (tv, E.consConstraint (lab, c) cst, contextSensitiveSyntaxError)
	       end
	     | f_type (A.TypeRecord (tyrows, _, _, lab, _)) =
	       let val (xs, csts, csss) = unzipThree (map f_tyrow tyrows)
		   val (lops, rts) = ListPair.unzip xs
		   val tv   = T.freshtyvar ()
		   val cst  = E.unionConstraintsList csts
		   val contextSensitiveSyntaxError  = E.uenvcss csss
		   val contextSensitiveSyntaxError' = consCSMlab (List.mapPartial (fn x => x) lops)
		   val c    = E.initTypeConstraint (T.consV tv) (T.constyrecord rts (T.noflex ()) lab) lab
	       in (tv, E.consConstraint (lab, c) cst, E.uenvcss [contextSensitiveSyntaxError, contextSensitiveSyntaxError'])
	       end
	     | f_type (A.TypeSlRec (tyrows, _, lab, _)) =
	       let val (xs, csts, csss) = unzipThree (map f_tyrow tyrows)
		   val (lops, rts) = ListPair.unzip xs
		   val tv   = T.freshtyvar ()
		   val cst  = E.unionConstraintsList csts
		   val contextSensitiveSyntaxError  = E.uenvcss csss
		   val contextSensitiveSyntaxError' = consCSMlab (List.mapPartial (fn x => x) lops)
		   val c    = E.initTypeConstraint (T.consV tv) (T.constyrecord rts (T.consflex lab) lab) lab
	       in (tv, E.consConstraint (lab, c) cst, E.uenvcss [contextSensitiveSyntaxError, contextSensitiveSyntaxError'])
	       end
	     | f_type (A.TypeTyCon (typseq, longtycon, _, lab, _)) =
	       let val (sv, cst1, contextSensitiveSyntaxError1) = f_typeseq typseq
		   val (tfv, cst2) = f_longtycon longtycon
		   val tv  = T.freshtyvar ()
		   val tv' = T.freshtyvar ()
		   val sv' = T.freshseqvar ()
		   val c1  = E.initFunctionTypeConstraint (T.TFV tfv) (T.TFC (T.SV sv', T.consV tv', lab)) lab
		   val c2  = E.initTypeConstraint (T.consV tv) (T.consV tv') lab
		   val c3  = E.initSequenceConstraint (T.SV sv) (T.SV sv') lab
	       in (tv, E.conscsts (lab, [c1, c2, c3]) (E.unionConstraintsList [cst1, cst2]), contextSensitiveSyntaxError1)
	       end
	     | f_type (A.TypeParen (labtyp, _, _, lab, _)) =
	       let val (tv, cst, contextSensitiveSyntaxError) = f_labtype labtyp
		   val tv' = T.freshtyvar ()
		   val c   = E.initTypeConstraint (T.consV tv') (T.consV tv) lab
	       in (tv', E.consConstraint (lab, c) cst, contextSensitiveSyntaxError)
	       end
	     | f_type (A.TypeDots pl) =
	       let val tv  = T.freshtyvar ()
		   val env = f_partlist pl
	       in (tv, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (T.seqvar, E.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
	   and f_typeseq (A.TypeSeqOne (typ, _, lab, _)) =
	       let val (tv, cst, contextSensitiveSyntaxError) = f_type typ
		   val sv = T.freshseqvar ()
		   val c  = E.initSequenceConstraint (T.SV sv) (T.SC (T.constuple [tv] lab, T.noflex (), lab)) lab
	       in (sv, E.consConstraint (lab, c) cst, contextSensitiveSyntaxError)
	       end
	     | f_typeseq (A.TypeSeqEm (_, lab, _)) =
	       let val sv = T.freshseqvar ()
		   val c  = E.initSequenceConstraint (T.SV sv) (T.SC ([], T.noflex (), lab)) lab
	       in (sv, E.singleConstraint (lab, c), E.emptyContextSensitiveSyntaxError)
	       end
	     | f_typeseq (A.TypeSeqSeq (labtyps, _, lab, _)) =
	       let val (svs, csts, csss) = unzipThree (map f_labtype labtyps)
		   val sv  = T.freshseqvar ()
		   val cst = E.unionConstraintsList csts
		   val contextSensitiveSyntaxError = E.uenvcss csss
		   val c   = E.initSequenceConstraint (T.SV sv) (T.SC (T.constuple svs lab, T.noflex (), lab)) lab
	       in (sv, E.consConstraint (lab, c) cst, contextSensitiveSyntaxError)
	       end
	     | f_typeseq (A.TypeSeqDots pl) =
	       let val sv  = T.freshseqvar ()
		   val env = f_partlist pl
	       in (sv, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.tyvar, Env.varenv, E.emptyConstraint, Env.css) *)
	   and f_labpat (A.LabPat (pat, _, _, lab, _)) =
	       let val (tv, vids, cst, contextSensitiveSyntaxError) = f_pat pat
		   val tv' = T.freshtyvar ()
		   val c   = E.initTypeConstraint (T.consV tv') (T.consV tv) lab
	       in (tv', vids, E.consConstraint (lab, c) cst, contextSensitiveSyntaxError)
	       end
	     | f_labpat (A.LabPatDots pl) =
	       let val env = f_partlist pl
		   val tv  = T.freshtyvar ()
	       in (tv, E.getVids env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.tyvar, Env.varenv, E.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
	   and f_identty (A.IdentTyId ident) = f_identpat ident
	     | f_identty (A.IdentTyTy (labid, labtyp, _, lab, _)) =
	       let val (tv1, vids, cst1, contextSensitiveSyntaxError1) = f_labid labid
		   val (tv2, cst2, contextSensitiveSyntaxError2) = f_labtype labtyp
		   val tv = T.freshtyvar ()
		   val c1 = E.initTypeConstraint (T.consV tv) (T.consV tv1) lab
		   val c2 = E.initTypeConstraint (T.consV tv) (T.consV tv2) lab
	       in (tv, vids, E.conscsts (lab, [c1, c2]) (E.unionConstraintsList [cst1, cst2]), E.uenvcss [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2])
	       end
	     | f_identty (A.IdentTyDots pl) =
	       let val env = f_partlist pl
		   val tv  = T.freshtyvar ()
	       in (tv, E.getVids env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.tyvar, Env.varenv, E.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
	   and f_labidty (A.LabIdTy (identty, _, lab, _)) =
	       let val (tv, vids, cst, contextSensitiveSyntaxError) = f_identty identty
		   val tv' = T.freshtyvar ()
		   val c   = E.initTypeConstraint (T.consV tv') (T.consV tv) lab
	       in (tv', vids, E.consConstraint (lab, c) cst, contextSensitiveSyntaxError)
	       end
	     | f_labidty (A.LabIdTyDots pl) =
	       let val env = f_partlist pl
		   val tv  = T.freshtyvar ()
	       in (tv, E.getVids env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (((string, Label.label) option, Ty.rowvar option, Label.label option), Env.varenv, Env.emptyConstraint, Env.emptyContextSensitiveSyntaxError) *)
	   and f_patrow (A.PatRow (tylab, labpat, _, _, lab, _)) =
	       let val (lv, lop, cst1) = f_tylab tylab
		   val (tv, vids, cst2, contextSensitiveSyntaxError2) = f_labpat labpat
		   val rv = T.freshrowvar ()
		   val c  = E.initRowConstraint (T.RV rv) (T.RC (T.LV lv, T.consV tv, lab)) lab
	       in ((lop, SOME rv, NONE), vids, E.consConstraint (lab, c) (E.unionConstraintsList [cst1, cst2]), contextSensitiveSyntaxError2)
	       end
	     | f_patrow (A.PatRowId (identty, _)) =
	       let val (tv, vids, cst, contextSensitiveSyntaxError) = f_identty identty
		   val rv   = T.freshrowvar ()
		   val llop = A.getlabstIdentTy identty
		   val cst' = case llop of
				 NONE => E.emptyConstraint
			       | SOME (l1, l2, s) =>
				 let val lv = T.freshlabvar ()
				     val c1 = E.initLabelConstraint (T.LV lv) (T.LC (s, l1)) l1
				     val c2 = E.initRowConstraint (T.RV rv) (T.RC (T.LV lv, T.consV tv, l2)) l2
				 in E.consConstraint (l1, c1) (E.singleConstraint (l2, c2))
				 end
		   val lop  = case llop of NONE => NONE | SOME (_, l, s) => SOME (s, l)
	       in ((lop, SOME rv, NONE), vids, E.unionConstraintsList [cst, cst'], contextSensitiveSyntaxError)
	       end
	     | f_patrow (A.PatRowAs (labidty, labpat, _, lab, _)) =
	       let val (tv1, vids1, cst1, contextSensitiveSyntaxError1) = f_labidty labidty
		   val (tv2, vids2, cst2, contextSensitiveSyntaxError2) = f_labpat labpat
		   val tv   = T.freshtyvar ()
		   val rv   = T.freshrowvar ()
		   val c1   = E.initTypeConstraint (T.consV tv) (T.consV tv1) lab
		   val c2   = E.initTypeConstraint (T.consV tv) (T.consV tv2) lab
		   val labs = L.cons lab (A.getlabelsLabIdTy labidty)
		   val llop = A.getlabstLabIdTy labidty
		   val cst  = case llop of
				 NONE => E.emptyConstraint
			       | SOME (l1, l2, s) =>
				 let val lv = T.freshlabvar ()
				     val c1 = E.initLabelConstraint (T.LV lv) (T.LC (s, l1)) l1
				     val c2 = E.initRowConstraint (T.RV rv) (T.RC (T.LV lv, T.consV tv, l2)) l2
				 in E.consConstraint (l1, c1) (E.singleConstraint (l2, c2))
				 end
		   val lop' = case llop of NONE => NONE | SOME (_, l, s) => SOME (s, l)
		   val cst  = E.conscsts (lab, [c1, c2]) (E.unionConstraintsList [cst1, cst2, cst])
		   val contextSensitiveSyntaxError  = E.uenvcss [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2]
	       in ((lop', SOME rv, NONE), E.unionEnvironmentList [E.toRECVids vids1 labs, vids2], cst, contextSensitiveSyntaxError)
	       end
	     | f_patrow (A.PatRowWild (r, l, n)) = ((NONE, NONE, SOME l), E.emvar, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	     | f_patrow (A.PatRowDots pl) =
	       let val env = f_partlist pl
	       in ((NONE, NONE, NONE), E.getVids env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.tyvar, Env.varenv, Env.cst, Env.css) *)
	   and f_atpat (A.AtPatWild _) = (T.freshtyvar (), E.emvar, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	     | f_atpat (A.AtPatId longid) = f_longidpat longid
	     | f_atpat (A.AtPatScon scon) =
	       let val (tv, cst, contextSensitiveSyntaxError) = f_scon scon
		   val contextSensitiveSyntaxError' = case A.isPatScon scon of
				  NONE     => E.emptyContextSensitiveSyntaxError
				| SOME lab => E.singcss (E.CSSREAL (L.singleton lab))
	       in (tv, E.emvar, cst, E.uenvcss [contextSensitiveSyntaxError, contextSensitiveSyntaxError'])
	       end
	     | f_atpat (A.AtPatTuple (labpats, _, lab, _)) =
	       let val tv   = T.freshtyvar ()
		   val (tvs, vidss, csts, csss) = unzipFour (map f_labpat labpats)
		   val vids = E.unionEnvironmentList    vidss
		   val cst  = E.unionConstraintsList csts
		   val contextSensitiveSyntaxError  = E.uenvcss csss
		   val c    = E.initTypeConstraint (T.consV tv) (T.constytuple tvs lab) lab
	       in (tv, vids, E.consConstraint (lab, c) cst, contextSensitiveSyntaxError)
	       end
	     | f_atpat (A.AtPatRecord (patrows, _, _, lab, _)) =
	       let val tv   = T.freshtyvar ()
		   val (xs, vidss, csts, csss) = unzipFour (map f_patrow patrows)
		   val (lops, rvs, flexs) = unzipThree xs
		   val vids = E.unionEnvironmentList vidss
		   val cst  = E.unionConstraintsList csts
		   val contextSensitiveSyntaxError  = E.uenvcss csss
		   val contextSensitiveSyntaxError' = consCSMlab (List.mapPartial (fn x => x) lops)
		   val rvs' = List.mapPartial (fn x => x) rvs
		   val flex = case List.mapPartial (fn x => x) flexs of
				  []  => T.noflex ()
				| [x] => T.consflex x
				| _   => raise EH.DeadBranch "Only one flex per record"
		   val c    = E.initTypeConstraint (T.consV tv) (T.constyrecord rvs' flex lab) lab
	       in (tv, vids, E.consConstraint (lab, c) cst, E.uenvcss [contextSensitiveSyntaxError, contextSensitiveSyntaxError'])
	       end
	     | f_atpat (A.AtPatParen (labpat, _, _, lab, _)) =
	       let val (tv, vids, cst, contextSensitiveSyntaxError) = f_labpat labpat
		   val tv' = T.freshtyvar ()
		   val c   = E.initTypeConstraint (T.consV tv') (T.consV tv) lab
	       in (tv', vids, E.consConstraint (lab, c) cst, contextSensitiveSyntaxError)
	       end
	     | f_atpat (A.AtPatList (labpats, _, lab, _)) =
	       let val tv   = T.freshtyvar ()
		   val tv'  = T.freshtyvar ()
		   val (tvs, vidss, csts, csss) = unzipFour (map f_labpat labpats)
		   val vids = E.unionEnvironmentList vidss
		   val cst  = E.unionConstraintsList csts
		   val contextSensitiveSyntaxError  = E.uenvcss csss
		   val cs   = map (fn x => E.initTypeConstraint (T.consV tv') (T.consV x) lab) tvs
		   val c    = E.initTypeConstraint (T.consV tv) (T.constylist tv' lab) lab
	       in (tv, vids, E.conscsts (lab, c :: cs) cst, contextSensitiveSyntaxError)
	       end
	     | f_atpat (A.AtPatOr (labpats, _, lab, _)) =
	       let val (tvs, vidss, csts, csss) = unzipFour (map f_labpat labpats)
		   val vids = E.unionEnvironmentList vidss (* do something else here: treatAtPatOr envs *)
		   val cst  = E.unionConstraintsList csts
		   val contextSensitiveSyntaxError  = E.uenvcss csss
		   val tv   = T.freshtyvar ()
		   val cs   = map (fn x => E.initTypeConstraint (T.consV tv) (T.consV x) lab) tvs
	       in (tv, vids, E.conscsts (lab, cs) cst, contextSensitiveSyntaxError)
	       end
	     | f_atpat (A.AtPatDots pl) =
	       let val env = f_partlist pl
		   val tv  = T.freshtyvar ()
	       in (tv, E.getVids env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.tyvar, Env.varenv, Env.cst, Env.css) *)
	   and f_pat (A.PatAtPat atpat) = f_atpat atpat
	     | f_pat (A.PatApp (longid, atpat, _, _, lab, _)) =
	       let val (tv1, vids1, cst1, contextSensitiveSyntaxError1) = f_atpat atpat
		   val (tv2, lab', cl, cst2) = f_longidexp longid
		   val tv  = T.freshtyvar ()
		   val c1  = E.initTypeConstraint (T.consV tv2) (T.constyarrow' tv1 tv lab T.PA) lab
		   val c2  = E.initClassConstraint cl (CL.consCO1 ()) lab
		   val cst = if A.isLongIdent longid
			     then E.singleConstraint (lab', E.initClassConstraint cl (CL.consCON ()) lab')
			     else E.emptyConstraint
	       in (tv, vids1, E.conscsts (lab, [c1, c2]) (E.unionConstraintsList [cst1, cst2, cst]), contextSensitiveSyntaxError1)
	       end
	     | f_pat (A.PatConsList (v, labpat1, labpat2, reg, lab, next)) =
	       if benv
	       then f_pat (A.PatOp ("::", v, labpat1, labpat2, reg, lab, next))
	       else let val (tv1, vids1, cst1, contextSensitiveSyntaxError1) = f_labpat labpat1
			val (tv2, vids2, cst2, contextSensitiveSyntaxError2) = f_labpat labpat2
			val tv  = T.freshtyvar ()
			val c1  = E.initTypeConstraint (T.consV tv) (T.constylist tv1 lab) lab
			val c2  = E.initTypeConstraint (T.consV tv) (T.consV tv2) lab
			val cst = E.conscsts (lab, [c1, c2]) (E.unionConstraintsList [cst1, cst2])
			val contextSensitiveSyntaxError = E.uenvcss [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2]
		    in (tv, E.unionEnvironmentList [vids1, vids2], cst, contextSensitiveSyntaxError)
		    end
	     | f_pat (A.PatOp (st, v, labpat1, labpat2, _, lab, _)) =
	       let val (tv1, vids1, cst1, contextSensitiveSyntaxError1) = f_labpat labpat1
		   val (tv2, vids2, cst2, contextSensitiveSyntaxError2) = f_labpat labpat2
		   val ty  = T.newV ()
		   val ti  = T.constytuple [tv1, tv2] lab
		   val tvo = T.freshtyvar ()
		   val c   = E.initTypeConstraint ty (T.consTyArrowTy ti (T.consV tvo) lab T.OT) lab
		   val a   = E.genAccIvEm (E.consAccId (I.ID (v, lab)) ty (CL.consDA1 ()) lab) lab
		   val cst = E.conscsts (lab, [c, E.ACCESSOR_CONSTRAINT a]) (E.unionConstraintsList [cst1, cst2])
		   val contextSensitiveSyntaxError = E.uenvcss [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2]
	       in (tvo, E.unionEnvironmentList [vids1, vids2], cst, contextSensitiveSyntaxError)
	       end
	     | f_pat (A.PatTyped (labpat, labtyp, _, lab, _)) =
	       let val tv = T.freshtyvar ()
		   val (tv1, vids, cst1, contextSensitiveSyntaxError1) = f_labpat labpat
		   val (tv2, cst2, contextSensitiveSyntaxError2) = f_labtype labtyp
		   val c1  = E.initTypeConstraint (T.consV tv) (T.consV tv1) lab
		   val c2  = E.initTypeConstraint (T.consV tv) (T.consV tv2) lab
		   val cst = E.conscsts (lab, [c1, c2]) (E.unionConstraintsList [cst1, cst2])
		   val contextSensitiveSyntaxError = E.uenvcss [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2]
	       in (tv, vids, cst, contextSensitiveSyntaxError)
	       end
	     | f_pat (A.PatAs (labidty, labpat, _, lab, _)) = (* For this one and PatRowAs we need to constrain the envs to a value variables. *)
	       let val (tv1, vids1, cst1, contextSensitiveSyntaxError1) = f_labidty labidty
		   val (tv2, vids2, cst2, contextSensitiveSyntaxError2) = f_labpat labpat
		   (*val clv  = CL.newCl ()*)
		   val tv   = T.freshtyvar ()
		   val c1   = E.initTypeConstraint (T.consV tv) (T.consV tv1) lab
		   val c2   = E.initTypeConstraint (T.consV tv) (T.consV tv2) lab
		   (*val c3   = E.initClassConstraint clv (CL.consPAT ()) lab*)
		   val labs = L.cons lab (A.getlabelsLabIdTy labidty)
		   val cst  = E.conscsts (lab, [c1, c2(*, c3*)]) (E.unionConstraintsList [cst1, cst2])
		   val contextSensitiveSyntaxError  = E.uenvcss [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2]
		   (*val vids = E.toCLSVids vids1 clv labs*)
		   val vids = E.toPATVids vids1 labs
	       in (tv, E.unionEnvironmentList [vids, vids2], cst, contextSensitiveSyntaxError)
	       end
	     | f_pat (A.PatDots pl) =
	       let val tv  = T.freshtyvar ()
		   val env = f_partlist pl
	       in (tv, E.getVids env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
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
		   val c1   = E.initTypeConstraint (T.consV tv1) (T.constyarrow' tv2 tv lab (T.DE I.dummyId)) lab
		   val c2   = E.initClassConstraint clv2 (CL.consDA1 ()) lab
		   val c3   = E.initClassConstraint clv1 clv2 lab1
		   val c4   = E.initClassConstraint clv1 (CL.consDAT ()) lab2
		   val cst  = E.unionConstraintsList [cst1, cst2]
		   val css  = E.uenvcss [css1, css2]
		   val cst' = E.conscsts (lab, [c1, c2]) (E.consConstraint (lab1, c3) (E.consConstraint (lab2, c4) cst))
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
	       in (tv, E.getVids env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.tyvar list, Env.varenv, Env.cst, Env.css) *)
	   and f_conbindseq (A.ConBindSeq conbinds) =
	       let val (tvs, conss, csts, csss) = unzipFour (map f_conbind conbinds)
		   val cons = E.unionEnvironmentList conss
		   val cst  = E.unionConstraintsList csts
		   val css  = E.uenvcss csss
	       in (tvs, cons, cst, css)
	       end
	     | f_conbindseq (A.ConBindSeqDots pl) =
	       let val env = f_partlist pl
	       in ([], E.getVids env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.tyvar, Ty.tvenv, Env.cst) *)
	   and f_typevarbind (A.TypeVar (_, n, _, lab, _)) =
	       let val _   = D.printDebug 2 D.AZE ("in f_typevarbind - generating constraints for A.TypeVar (lab = "^Int.toString(L.toInt(lab))^")")
		   val tv1  = T.freshtyvar ()
		   val tv2  = T.freshtyvar ()
		   val tyvs = E.singenv (n, [E.consBindMono n (tv1, true) (CL.consTYVAR ()) lab])
		   val c    = E.initTypeConstraint (T.consV tv1) (T.V (tv2, SOME (n, lab), T.POLY)) lab
	       in (tv2, tyvs, E.singleConstraint (lab, c))
	       end
	     | f_typevarbind A.TypeVarDots = (T.freshtyvar (), E.emtv, E.emptyConstraint)

	   (* RETURNS: (Ty.tyvar, Ty.tvenv, Env.cst) *)
	   and f_labtypevarbind (A.LabTyVar (tyvar, _, lab, _)) =
	       let val (tv, tyvs, cst) = f_typevarbind tyvar
		   val tv' = T.freshtyvar ()
		   val c   = E.initTypeConstraint (T.consV tv) (T.consV tv') lab
	       in (tv', tyvs, E.consConstraint (lab, c) cst)
	       end
	     | f_labtypevarbind (A.LabTyVarDots tyvars) =
	       (T.freshtyvar (), E.emtv, E.emptyConstraint)

	   (* RETURNS: (Ty.seqvar, Ty.tvenv, Env.cst) *)
	   and f_typevarseq (A.TyVarSeqOne (tyvar, _, lab, _)) =
	       let val (tv, tyvs, cst) = f_typevarbind tyvar
		   val sv = T.freshseqvar ()
		   val sq = T.SC (T.constuple [tv] lab, T.noflex (), lab)
		   val c  = E.initSequenceConstraint (T.SV sv) sq lab
	       in (sv, tyvs, E.consConstraint (lab, c) cst)
	       end
	     | f_typevarseq (A.TyVarSeqEm (_, lab, _)) =
	       let val sv = T.freshseqvar ()
		   val sq = T.SC ([], T.noflex (), lab)
		   val c  = E.initSequenceConstraint (T.SV sv) sq lab
	       in (sv, E.emtv, E.singleConstraint (lab, c))
	       end
	     | f_typevarseq (A.TyVarSeqSeq (typevars, _, lab, _)) =
	       let val (tvs, tyvss, csts) = unzipThree (map f_labtypevarbind typevars)
		   val tyvs = E.unionEnvironmentList tyvss
		   val cst  = E.unionConstraintsList csts
		   val sv   = T.freshseqvar ()
		   val sq   = T.SC (T.constuple tvs lab, T.noflex (), lab)
		   val c    = E.initSequenceConstraint (T.SV sv) sq lab
	       in (sv, tyvs, E.consConstraint (lab, c) cst)
	       end
	     | f_typevarseq (A.TyVarSeqDots tyvars) =
	       let val (_, tyvss, csts) = unzipThree (map f_typevarbind tyvars)
		   val tyvs = E.unionEnvironmentList tyvss
		   val cst  = E.unionConstraintsList csts
		   val sv   = T.freshseqvar ()
	       in (sv, tyvs, cst)
	       end

	   (* RETURNS: (Ty.typenv, Ty.varenv * Id.idl option, Env.cst, Env.cst, Env.css) *)
	   and f_datbind (A.DatBind (datname, conbindseq, _, lab, _)) =
	       let val (tvs, cons, cst1, css1) = f_conbindseq conbindseq
		   val (s, v, tv, sv1, sv2, typs, tyvs, cst2, css2) = f_datname datname
		   val tn   = if benv andalso getBasis () then getTyNameString s else T.freshtyname ()
		   val id   = Option.getOpt (Option.map (fn (i, _) => i) v, I.dummyId)
		   val lab' = Option.getOpt (Option.map (fn (_, l) => l) v, lab)
		   (* (2010-06-10)We do that so that for datatypes, the end points will not be
		    * the equal signs but the datatype names, in the case of constructor clashes.*)
		   val c1   = E.initSequenceConstraint (T.SV sv1) (T.SV sv2) lab
		   val c2   = E.initTypeConstraint (T.consV tv) (T.C (T.NC (tn, T.DE id, lab'), T.SV sv1, lab')) lab'
		   val c2'  = E.initTypeConstraint (T.consV tv) (T.consTyNameVar lab) lab
		   (*(2010-06-21)c2' is similar to c2 but weaker.  It just constrains tv to be of the
		    * form T.C.*)
		   val c3   = E.LET_CONSTRAINT (E.SEQUENCE_ENV (E.projTyvs tyvs, E.CONSTRAINT_ENV cst1))
		   val cs   = map (fn x => E.initTypeConstraint (T.consV x) (T.consV tv) lab) tvs
		   val cst  = E.conscsts (lab, [c1, c2']) (E.consConstraint (lab', c2) cst2)
		   val cst' = E.conscsts (lab, cs) (E.singleConstraint (L.dummyLab, c3))
		   val css3 = checkTyVarInc (A.gettyvarDatName datname) (A.getlabDatName datname) (A.gettyvarConbindseq conbindseq)
		   val css  = E.uenvcss [css1, css2, css3]
	       in (E.toTYCONTyps typs E.emvar false (L.singleton lab), (cons, v), cst, cst', css)
	       end
	     | f_datbind (A.DatBindDots pl) =
	       let val env = f_partlist pl
	       in (E.getTyps env, (E.getVids env, NONE), E.emptyConstraint, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.typenv, (Ty.varenv * Id.idl option) list, Env.cst, Env.css) *)
	   and f_datbindseq (A.DatBindSeq (datbinds, _, _)) =
	       let val (typss, conss, csts1, csts2, csss) = unzipFive (map f_datbind datbinds)
		   val typs = E.unionEnvironmentList typss
		   val cst1 = E.unionConstraintsList csts1
		   val cst2 = E.unionConstraintsList csts2
		   val css  = E.uenvcss csss
		   val css1 = consCSM typs
		   val css2 = consCSM (E.unionEnvironmentList (map (fn (cons, _) => cons) conss))
	       in (typs, conss, cst1, cst2, E.uenvcss [css, css1, css2])
	       end
	     | f_datbindseq (A.DatBindSeqDots pl) =
	       let val env = f_partlist pl
	       in (E.getTyps env, [(E.getVids env, NONE)], E.emptyConstraint, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.varenv, Env.cst, Env.cst, Env.css) *)
	   and f_valbindcore (A.ValBindCore (labpat, labexp, _, lab, _)) =
	       let val _   = D.printDebug 2 D.AZE ("generating constraints for A.ValBindCore (lab = "^Int.toString(L.toInt(lab))^")")
		   val (tv1, vids, cst1, css1) = f_labpat labpat
		   val (tv2, cst2, css2) = f_labexp labexp
		   val vids' = E.closeVids vids (V.nonexpLabExp labexp)
		   val c     = E.initTypeConstraint (T.consV tv1) (T.consV tv2) lab
		   val cst   = E.consConstraint (lab, c) (cst2)
		   val css   = E.uenvcss [clearRebound css1, css2]
		   val _     = D.printDebug 3 D.AZE ("A.ValBindCore constraints generated (tv1="^Int.toString(T.tyvarToInt tv1)^", tv2="^Int.toString(T.tyvarToInt tv2)^")\n")
	       in (vids', cst1, cst, css)
	       end
	     | f_valbindcore (A.ValBindCoreDots pl) =
	       let val env = f_partlist pl
	       in (E.getVids env, E.emptyConstraint, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.varenv, Env.cst, Env.cst, Env.css) *)
	   and f_valbindseq (A.ValBindSeq (valbindcores, _, _)) =
	       let val (vidss, csts1, csts2, csss) = unzipFour (map f_valbindcore valbindcores)
		   val vids = E.unionEnvironmentList vidss
		   val cst1 = E.unionConstraintsList csts1
		   val cst2 = E.unionConstraintsList csts2
		   val css  = E.uenvcss csss
	       in (vids, cst1, cst2, css)
	       end
	     | f_valbindseq (A.ValBindSeqDots pl) =
	       let val env = f_partlist pl
	       in (E.getVids env, E.emptyConstraint, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_valbind (A.ValBindRec (valbindseq, _, lab, _)) =
	       let val (vids, cst1, cst2, css) = f_valbindseq valbindseq
		   val labs  = L.singleton lab
		   val vids' = E.toMonoVids (E.toRECVids vids labs) labs
		   val css1  = consCSM vids
		   val css2  = map (fn x => E.CSSFREC (L.cons lab x)) (A.isExpFnValBindSeq valbindseq)
		   val env   = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst1, E.SEQUENCE_ENV (E.projVids vids', E.CONSTRAINT_ENV cst2))
	       in (env, E.uenvcss [css, css1, css2])
	       end
	     | f_valbind (A.ValBind valbindseq) =
	       let val (vids, cst1, cst2, css) = f_valbindseq valbindseq
		   val env = E.SEQUENCE_ENV (E.CONSTRAINT_ENV (E.unionConstraintsList [cst1, cst2]), E.projVids vids)
	       in (env, css)
	       end
	     | f_valbind (A.ValBindDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.tyvar, Env.varenv, Env.cst, Env.css) *)
	   and f_labatpat (A.LabAtPat (atpat, _, lab, _)) =
	       let val (tv, vids, cst, css) = f_atpat atpat
		   val tv' = T.freshtyvar ()
		   val c   = E.initTypeConstraint (T.consV tv) (T.consV tv') lab
	       in (tv', vids, E.consConstraint (lab, c) cst, css)
	       end
	     | f_labatpat (A.LabAtPatDots pl) =
	       let val tv  = T.freshtyvar ()
		   val env = f_partlist pl
	       in (tv, E.getVids env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
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
		   val c   = E.initTypeConstraint (T.consV tv1) (T.constyarrow tv2 tv lab) lab
		   val cst = E.consConstraint (lab, c) (E.unionConstraintsList [cst1, cst2])
		   val css = E.uenvcss [css1, clearRebound css2]
	       in (tv, vids, E.unionEnvironmentList [vids1, vids2], cst, css)
	       end
	     | f_fmatch (A.FMatchSlApp (fmatch, labatpat, _)) =
	       let val (tv1, vids, vids1, cst1, css1) = f_fmatch fmatch
		   val (tv2, vids2, cst2, css2) = f_labatpat labatpat
		   val tv  = T.freshtyvar ()
		   val cst = E.unionConstraintsList [cst1, cst2]
		   val css = E.uenvcss [css1, clearRebound css2]
	       in (tv, vids, E.unionEnvironmentList [vids1, vids2], cst, css)
	       end
	     | f_fmatch (A.FMatchNoApp (fmatch, _)) =
	       let val (_, vids, vids1, cst1, css1) = f_fmatch fmatch
		   val tv = T.freshtyvar ()
	       in (tv, vids, vids1, cst1, css1)
	       end
	     | f_fmatch A.FMatchDots =
	       let val tv = T.freshtyvar ()
	       in (tv, E.emvar, E.emvar, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.tyvar, Env.varenv, Env.varenv, Env.cst, Env.css) *)
	   and f_labfmatch (A.LabFMatch (fmatch, _, lab, _)) =
	       let val (tv, vids, vids', cst, css) = f_fmatch fmatch
		   val tv' = T.freshtyvar ()
		   val c   = E.initTypeConstraint (T.consV tv) (T.consV tv') lab
	       in (tv', vids, vids', E.consConstraint (lab, c) cst, css)
	       end
	     | f_labfmatch (A.LabFMatchSl (fmatch, _)) =
	       let val (tv, vids, vids', cst, css) = f_fmatch fmatch
		   val tv' = T.freshtyvar ()
	       in (tv', vids, vids', cst, css)
	       end
	     | f_labfmatch A.LabFMatchDots =
	       let val tv = T.freshtyvar ()
	       in (tv, E.emvar, E.emvar, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.tyvar, Env.varenv, Env.varenv, Env.cst, Env.css) *)
	   and f_fmatchty (A.FMatchT fmatch) = f_labfmatch fmatch
	     | f_fmatchty (A.FMatchTTy (fmatch, labtyp, _, lab, _)) =
	       let val (tv1, vids1, vids1', cst1, css1) = f_labfmatch fmatch
		   val (tv2, cst2, css2) = f_labtype labtyp
		   val tv  = T.freshtyvar ()
		   val c1  = E.initTypeConstraint (T.consV tv) (T.consV tv1) lab
		   val c2  = E.initTypeConstraint (T.consV tv) (T.consV tv2) lab
		   val cst = E.conscsts (lab, [c1, c2]) (E.unionConstraintsList [cst1, cst2])
		   val css = E.uenvcss [css1, css2]
	       in (tv, vids1, vids1', cst, css)
	       end
	     | f_fmatchty A.FMatchTDots =
	       let val tv = T.freshtyvar ()
	       in (tv, E.emvar, E.emvar, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.varenv, Env.cst, Env.css) *)
	   and f_fvalbindcore (A.FValBindCore (fmatch, labexp, _, lab, _)) =
	       let val (tv1, vids, vids', cst1, css1) = f_fmatchty fmatch
		   val (tv2, cst2, css2) = f_labexp labexp
		   val labs = L.singleton lab
		   val c1   = E.initTypeConstraint (T.consV tv1) (T.consV tv2) lab
		   val env  = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst1, E.projVids (E.toMonoVids vids' L.empty))
		   val c2   = E.LET_CONSTRAINT (E.SEQUENCE_ENV (env, E.CONSTRAINT_ENV cst2))
		   val cst  = E.consConstraint (lab, c1) (E.singleConstraint (L.dummyLab, c2))
		   val css  = E.uenvcss [css1, css2]
		   val vids = E.toRECVids (E.toMonoVids vids L.empty) L.empty
	       (*(2010-06-11)NOTE: We've got a lot of L.empty here!?*)
	       in (vids, cst, css)
	       end
	     | f_fvalbindcore (A.FVBCoreDots pl) =
	       let val env = f_partlist pl
	       in (E.getVids env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.varenv, Env.cst, Env.css) *)
	   and f_fvalbindone (A.FValBindOne (fvalbindcores, _, lab, _)) =
	       let val (vidss, csts, csss) = unzipThree (map f_fvalbindcore fvalbindcores)
		   val vids = E.unionEnvironmentList vidss
		   val cst  = E.unionConstraintsList csts
		   val css  = E.uenvcss csss
		   val cst' = E.allEqualVids vids
		   val css' = sameNameFun vids
	       in (vids, E.unionConstraintsList [cst, cst'], E.uenvcss [css, css'])
	       end
	     | f_fvalbindone (A.FVBOneDots pl) =
	       let val env = f_partlist pl
	       in (E.getVids env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_fvalbind (A.FValBind (fvalbindones, _, _)) =
	       let val (vidss, csts, csss) = unzipThree (map f_fvalbindone fvalbindones)
		   val vids = E.unionEnvironmentList vidss
		   val cst  = E.unionConstraintsList csts
		   val css  = E.uenvcss csss
		   val env  = E.SEQUENCE_ENV (E.projVids vids, E.CONSTRAINT_ENV cst)
		   val css1 = createDiffNbArgFuns fvalbindones
		   val css2 = consCSMfval vidss
	       in (env, E.uenvcss [css, css1, css2])
	       end
	     | f_fvalbind (A.FValBindDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (string, Id.id option, Ty.tyvar, Ty.seqvar, Env.typenv, Env.cst) *)
	   and f_tyconbind (A.TyCon (str, v, _, lab, _)) =
	       let
		   (* generate new type variable, sequence variable and fvariable (??) *)
		   val tv   = T.freshtyvar  ()
		   val sv   = T.freshseqvar ()
		   val tfv  = T.freshtyfvar ()

		   (* creates a new map, with v as the key, and the rhs as the value *)
		   val typs = E.singenv (v, [E.consBindPoly v (T.TFV tfv, E.TYP, ref (E.emvar, false)) (CL.consTYCON ()) lab])
		   (*(2010-06-10)NOTE: the false abaove is because we still don't know v's constructors.*)
		   val c    = E.initFunctionTypeConstraint (T.TFV tfv) (T.TFC (T.SV sv, T.consV tv, lab)) lab
	       in (str, SOME (v, lab), tv, sv, typs, E.singleConstraint (lab, c))
	       end
	     | f_tyconbind A.TyConDots =
	       let val tv = T.freshtyvar  ()
		   val sv = T.freshseqvar ()
	       in ("", NONE, tv, sv, E.emtyp, E.emptyConstraint)
	       end

	   (* RETURNS: (string, Id.id option, Ty.tyvar, Ty.seqvar, Ty.seqvar, Env.typenv, Env.tvenv, Env.cst, Env.css) *)
	   and f_datname (A.DatName (typvarseq, tycon, _, _)) =
	       let val (sv1, tyvs, cst1) = f_typevarseq typvarseq
		   val (s, v, tv2, sv2, typs, cst2) = f_tyconbind tycon
		   val css = consCSM tyvs
	       (* NOTE: sv1 and sv2 have to be equal *)
	       in (s, v, tv2, sv2, sv1, typs, tyvs, E.unionConstraintsList [cst1, cst2], css)
	       end
	     | f_datname A.DatNameDots =
	       let val tv  = T.freshtyvar ()
		   val sv1 = T.freshseqvar ()
		   val sv2 = T.freshseqvar ()
	       in ("", NONE, tv, sv1, sv2, E.emtyp, E.emtv, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.typenv, Env.cst, Env.css) *)
	   and f_typbind (A.TypBind (datname, labtyp, _, lab, _)) =
	       let
		   val _   = D.printDebug 2 D.AZE "generating constraints for A.TypBind"
		   val (_, _, tv1, sv1, sv1', typs, tyvs, cst1, css1) = f_datname datname
		   val (tv2, cst2, css2) = f_labtype labtyp
		   val c1  = E.initTypeConstraint (T.consV tv1) (T.consV tv2) lab
		   val c2  = E.initSequenceConstraint (T.SV sv1) (T.SV sv1') lab
		   val c3  = E.LET_CONSTRAINT (E.SEQUENCE_ENV (E.projTyvs tyvs, E.CONSTRAINT_ENV cst2))
		   val cst = E.conscsts (lab, [c1, c2]) (E.consConstraint (L.dummyLab, c3) cst1)
		   val css = E.uenvcss [css1, css2]
	       in (typs, cst, css)
	       end
	     | f_typbind (A.TypBindDots pl) =
	       let val env = f_partlist pl
	       in (E.getTyps env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.typenv, Env.cst, Env.css) *)
	   and f_typbindseq (A.TypBindSeq (typbinds, _, _)) =
	       let val (typss, csts, csss) = unzipThree (map f_typbind typbinds)
		   val typs = E.unionEnvironmentList typss
		   val cst  = E.unionConstraintsList csts
		   val css  = E.uenvcss csss
		   val css1 = consCSM typs
	       in (typs, cst, E.uenvcss [css, css1])
	       end
	     | f_typbindseq (A.TypBindSeqDots pl) =
	       let val env = f_partlist pl
	       in (E.getTyps env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.varenv, Env.cst, Env.css) *)
	   and f_exbind (A.ExBind (ident, lab, _)) =
	       let val (tv, cons, cst, css) = f_identpat ident
		   val c = E.initTypeConstraint (T.consV tv) (T.constyexception' lab (T.DE I.dummyId)) lab
	       in (E.toEX0Vids cons (L.singleton lab), E.consConstraint(lab, c) cst, css)
	       end
	     | f_exbind (A.ExBindOf (labid, labtyp, _, lab, _)) =
	       let val (tv1, cst1, css1) = f_labtype labtyp
		   val (tv2, cons, cst2, css2) = f_labid labid
		   val (lab1, lab2) = Option.getOpt (A.getLabelsIdLabId labid, (lab, lab))
		   val clv1 = CL.newCl ()
		   val clv2 = CL.newCl ()
		   val ty   = T.constyexception' lab (T.DE I.dummyId)
		   val c1   = E.initTypeConstraint (T.consV tv2) (T.consTyArrowTy (T.consV tv1) ty lab (T.DE I.dummyId)) lab
		   val c2   = E.initClassConstraint clv2 (CL.consEX1 ()) lab
		   val c3   = E.initClassConstraint clv1 clv2 lab1
		   val c4   = E.initClassConstraint clv1 (CL.consEXC ()) lab2
		   val cst  = E.unionConstraintsList [cst1, cst2]
		   val css  = E.uenvcss [css1, css2]
		   val cst' = E.conscsts (lab, [c1, c2]) (E.consConstraint(lab1, c3) (E.consConstraint(lab2, c4) cst))
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
		   val c1   = E.initTypeConstraint (T.consV tv2) (T.consV tv1) lab
		   val c2   = E.initClassConstraint cl clv lab
		   val c3   = E.initClassConstraint cl (CL.consEXC ()) lab
		   (* NOTE: the class could be a EX0 or a EX1 - a EXC *)
		   val cst  = E.conscsts (lab, [c1, c2]) (E.unionConstraintsList [cst1, cst2])
		   (* NOTE: we build c like that so that the accessor in cst2 is resolved before c3
		    * and we bind the class cl (and clv) to the correct EX0 or EX1 before checking
		    * that it is a EXC. *)
		   val c    = E.LET_CONSTRAINT (E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst, E.CONSTRAINT_ENV (E.singleConstraint (lab, c3))))
		   val labs = L.singleton lab
	       in (E.toCLSVids (E.toMonoVids cons labs) clv labs, E.singleConstraint (L.dummyLab, c), css1)
	       end
	     | f_exbind (A.ExBindNo (ident, _)) =
	       let val (tv, cons, cst, css) = f_identpat ident
	       in (E.toEX0Vids cons L.empty, cst, css)
	       end
	     | f_exbind (A.ExBindDots pl) =
	       let val env = f_partlist pl
	       in (E.getVids env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.varenv, Env.cst, Env.css) *)
	   and f_exbindseq (A.ExBindSeq (exbinds, _, _)) =
	       let val (conss, csts, csss) = unzipThree (map f_exbind exbinds)
		   val cons = E.unionEnvironmentList conss
		   val cst  = E.unionConstraintsList csts
		   val css  = E.uenvcss csss
		   val css1 = consCSM cons
	       in (cons, cst, E.uenvcss [css, css1])
	       end
	     | f_exbindseq (A.ExBindSeqDots pl) =
	       let val env = f_partlist pl
	       in (E.getVids env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Id.lid list) *)
	   and f_longstrseq (A.LongStrSeq (ids, _)) =
	       List.mapPartial (fn x => A.longstridToLid x) ids
	     | f_longstrseq (A.LongStrSeqDots pl) = []

	   (* RETURNS: (Ty.seqvar, Env.ocenv, Env.cst) *)
	   and f_classbind (A.Class (s, v, _, lab, _)) =
	       let val sv1  = T.freshseqvar ()
		   val sv2  = T.freshseqvar ()
		   val c    = E.initSequenceConstraint (T.SV sv1) (T.SV sv2) lab
		   val ovcs = E.consSingleEnv (v, [E.consBindPoly v (T.SV sv2) (CL.consOC ()) lab])
	       in (sv1, ovcs, E.singleConstraint (lab, c))
	       end
	     | f_classbind A.ClassDots =
	       let val sv = T.freshseqvar ()
	       in (sv, E.emoc, E.emptyConstraint)
	       end

	   (* RETURNS: (Ty.seqvar, Env.ocenv, Env.cst) *)
	   and f_labclassbind (A.LabClass (class, _, lab, _)) =
	       let val (sv, ovcs, cst) = f_classbind class
		   val sv' = T.freshseqvar ()
		   val c   = E.initSequenceConstraint (T.SV sv') (T.SV sv) lab
	       in (sv', ovcs, E.consConstraint(lab, c) cst)
	       end
	     | f_labclassbind (A.LabClassDots pl) =
	       let val env = f_partlist pl
	       in (T.freshseqvar (), E.emoc, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env))
	       end

	   (* RETURNS: (Ty.seqvar, Env.cst) *)
	   and f_class (A.Class (s, v, _, lab, _)) =
	       let val sv = T.freshseqvar ()
		   val a  = E.genAccIoEm (E.consAccId (I.ID (v, lab)) (T.SV sv) (CL.consOC ()) lab) lab
	       in (sv, E.singleConstraint (lab, E.ACCESSOR_CONSTRAINT a))
	       end
	     | f_class A.ClassDots = (T.freshseqvar (), E.emptyConstraint)

	   (* RETURNS: (Ty.seqvar, Env.cst) *)
	   and f_labclass (A.LabClass (class, _, lab, _)) =
	       let val (sv, cst) = f_class class
		   val sv' = T.freshseqvar ()
		   val c   = E.initSequenceConstraint (T.SV sv') (T.SV sv) lab
	       in (sv', E.consConstraint(lab, c) cst)
	       end
	     | f_labclass (A.LabClassDots pl) =
	       let val env = f_partlist pl
	       in (T.freshseqvar (), E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env))
	       end

	   (* RETURNS: (Ty.tyvar, Env.cst, Env.css) *)
	   and f_tyclass (A.TyClassCl (labclass, _, lab, _)) =
	       let val (sv, cst) = f_labclass labclass
		   val tv  = T.freshtyvar ()
		   val lid = Option.getOpt (A.getlabidLabclass labclass, (I.dummyId, L.dummyLab))
		   val c   = E.initTypeConstraint (T.consV tv) (T.OR (T.SV sv, T.freshidor (), T.POLY, T.VAL lid, lab)) lab
	       in (tv, E.consConstraint(lab, c) cst, E.emptyContextSensitiveSyntaxError)
	       end
	     | f_tyclass (A.TyClassTy (typ, lab, _)) =
	       let val (tv, cst, css) = f_type typ
		   val tv' = T.freshtyvar ()
		   val c   = E.initTypeConstraint (T.consV tv') (T.consV tv) lab
	       in (tv', E.consConstraint(lab, c) cst, css)
	       end
	     | f_tyclass (A.TyClassDots pl) =
	       let val env = f_partlist pl
		   val tv  = T.freshtyvar ()
	       in (tv, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.seqvar, Env.cst, Env.css) *)
	   and f_labtyclass (A.LabTyClass (tyclass, _, lab, _)) =
	       let val (tv, cst, css) = f_tyclass tyclass
		   val tv' = T.freshtyvar ()
		   val c   = E.initTypeConstraint (T.consV tv') (T.consV tv) lab
	       in (tv', E.consConstraint(lab, c) cst, css)
	       end
	     | f_labtyclass (A.LabTyClassDots pl) =
	       let val env = f_partlist pl
	       in (T.freshtyvar (), E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.seqvar, Env.cst, Env.css) *)
	   and f_tyclassseq (A.TyClassSeqOne (tyclass, _, lab, _)) =
	       let val (tv, cst, css) = f_tyclass tyclass
		   val sv = T.freshseqvar ()
		   val c  = E.initSequenceConstraint (T.SV sv) (T.SC (T.constuple [tv] lab, T.noflex (), lab)) lab
	       in (sv, E.consConstraint(lab, c) cst, E.emptyContextSensitiveSyntaxError)
	       end
	     | f_tyclassseq (A.TyClassSeqEm (_, lab, _)) =
	       let val sv = T.freshseqvar ()
		   val c  = E.initSequenceConstraint (T.SV sv) (T.SC ([], T.noflex (), lab)) lab
	       in (sv, E.singleConstraint (lab, c), E.emptyContextSensitiveSyntaxError)
	       end
	     | f_tyclassseq (A.TyClassSeqSeq (labtyclasss, _, lab, _)) =
	       let val (tvs, csts, csss) = unzipThree (map f_labtyclass labtyclasss)
		   val sv  = T.freshseqvar ()
		   val cst = E.unionConstraintsList csts
		   val css = E.uenvcss csss
		   val c   = E.initSequenceConstraint (T.SV sv) (T.SC (T.constuple tvs lab, T.noflex (), lab)) lab
	       in (sv, E.consConstraint(lab, c) cst, css)
	       end
	     | f_tyclassseq (A.TyClassSeqDots pl) =
	       let val env = f_partlist pl
		   val sv  = T.freshseqvar ()
	       in (sv, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.tvenv, Env.cst) *)
	   and f_typevarval (A.TypeVar (_, n, _, lab, _)) =
	       let val _   = D.printDebug 2 D.AZE ("in f_typevarval - generating constraints for A.TypeVar (lab = "^Int.toString(L.toInt(lab))^")")
		   val tv   = T.freshtyvar ()
		   val tyvs = E.consSingleEnv (n, [E.consBindMono n (tv, false) (CL.consTYVAR ()) lab])
		   val c    = E.initTypeConstraint (T.consV tv) (T.E (n, T.freshtyvar (), lab)) lab
	       in (tyvs, E.singleConstraint (lab, c))
	       end
	     | f_typevarval A.TypeVarDots = (E.emtv, E.emptyConstraint)

	   (* RETURNS: (Env.tvenv, Env.cst) *)
	   and f_typevarvallist [] = (E.emtv, E.emptyConstraint)
	     | f_typevarvallist [typevar] = f_typevarval typevar
	     | f_typevarvallist (typevar :: typevars) =
	       let val (tyvs1, cst1) = f_typevarval typevar
		   val (tyvs2, cst2) = f_typevarvallist typevars
	       in (E.unionEnvironmentList [tyvs1, tyvs2], E.unionConstraintsList [cst1, cst2])
	       end

	   (* RETURNS: (Env.tvenv, Env.cst) *)
	   and f_labtypevarval (A.LabTyVar (tyvar, _, lab, _)) =
	       let val (tyvs, cst) = f_typevarval tyvar
	       (* NOTE: the label is not used. *)
	       in (tyvs, cst)
	       end
	     | f_labtypevarval (A.LabTyVarDots tyvars) = (E.emtv, E.emptyConstraint)

	   (* RETURNS: (Env.tvenv, Env.cst) *)
	   and f_tyvarseqval (A.TyVarSeqOne (tyvar, _, lab, _)) = f_typevarval tyvar
	     | f_tyvarseqval (A.TyVarSeqEm (_, lab, _)) = (E.emtv, E.emptyConstraint)
	     | f_tyvarseqval (A.TyVarSeqSeq (typevars, _, lab, _)) =
	       let val (tyvss, csts) = ListPair.unzip (map f_labtypevarval typevars)
		   val tyvs = E.unionEnvironmentList tyvss
		   val cst  = E.unionConstraintsList csts
	       in (tyvs, cst)
	       end
	     | f_tyvarseqval (A.TyVarSeqDots tyvars) =
	       let val (tyvss, csts) = ListPair.unzip (map f_typevarval tyvars)
		   val tyvs = E.unionEnvironmentList tyvss
		   val cst  = E.unionConstraintsList csts
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
						   (E.unionEnvironmentList [idG, idR])*)
		   val tyvs = E.plusenv tyvs2 tyvs1
		   val cst  = E.unionConstraintsList [cst1, cst2]
		   val env' = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst, E.ENVPOL (tyvs, env))
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
		   val cst  = E.unionConstraintsList [cst1, cst2]
		   val env' = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst, E.ENVPOL (tyvs, env))
	       in (env', css)
	       end
	     | f_dec (A.DecDatType (datbinds, _, _)) =
	       let val (typs, conss, cst1, cst2, css) = f_datbindseq datbinds
		   val envs = map (fn (cons, SOME idl) => E.DATATYPE_CONSTRUCTOR_ENV (idl, E.ENVPOL (E.emtv, E.projVids cons))
				    | (cons, NONE) => E.ENVPOL (E.emtv, E.projVids cons)) conss
		   val env1 = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst2, E.envsToSeq envs)
		   val env2 = E.SEQUENCE_ENV (E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst1, E.consEnvironmentTypenames typs), env1)
	       in (env2, css)
	       end
	     | f_dec (A.DecDatWith (datbind, typbind, _, lab, _)) =
	       let val (typs1, conss, cst1, cst1', css1) = f_datbindseq datbind
		   val (typs2, cst2, css2) = f_typbindseq typbind
		   val css  = E.uenvcss [css1, css2]
		   val envs = map (fn (cons, SOME idl) => E.DATATYPE_CONSTRUCTOR_ENV (idl, E.ENVPOL (E.emtv, E.projVids cons))
				    | (cons, NONE) => E.ENVPOL (E.emtv, E.projVids cons)) conss
		   val env1 = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst1', E.envsToSeq envs)
		   val env2 = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst1, E.consEnvironmentTypenames typs1)
		   val env3 = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst2, E.consEnvironmentTypenames typs2)
		   val env4 = E.SEQUENCE_ENV (env2, E.SEQUENCE_ENV (env3, env1))
	       in (env4, css)
	       end
	     | f_dec (A.DecDatRep (tycon, longtycon, _, lab, _)) =
	       let val (s, v, tv, sv, typs, cst1) = f_tyconbind tycon
		   val (tfv, cst2) = f_longtycon longtycon
		   val c   = E.initFunctionTypeConstraint (T.TFV tfv) (T.TFC (T.SV sv, T.consV tv, lab)) lab
		   val cst = E.consConstraint(lab, c) (E.unionConstraintsList [cst1, cst2])
		   val opn = case A.longtyconToLid longtycon of
				 NONE => E.emopn
			       | SOME lid => E.singOEnv (lid, lab, E.DRE)
		   val env1 = case v of SOME idl => E.DATATYPE_CONSTRUCTOR_ENV (idl, E.ENVOPN opn) | NONE => E.ENVOPN opn
		   val env2 = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst, E.SEQUENCE_ENV (E.consEnvironmentTypenames typs, env1))
	       in (env2, E.emptyContextSensitiveSyntaxError)
	       end
	     | f_dec (A.DecType (typbindseq, _, _)) =
	       let val (typs, cst, css) = f_typbindseq typbindseq
	       in (E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst, E.consEnvironmentTypenames typs), css)
	       end
	     | f_dec (A.DecEx (exbindseq, _, _)) =
	       let val (cons, cst, css) = f_exbindseq exbindseq
	       in (E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst, E.projVids cons), css)
	       end
	     | f_dec (A.DecOpen (longstrseq, _, lab, _)) =
	       let val opns = foldl (fn (lid, opn) => E.addOEnv (lid, I.getLabId lid (*lab*), E.OST) opn)
				    (* NOTE: We can't use lab for all of then because then we can't
				     * distinguish between the ones that are IN and OUT during unification. *)
				    E.emopn
				    (f_longstrseq longstrseq)
	       in (E.projOpns opns, E.emptyContextSensitiveSyntaxError)
	       end
	     | f_dec (A.DecLocal (decs1, decs2, _, lab, _)) =
	       let val (env1, css1) = f_decs decs1
		   val (env2, css2) = f_decs decs2
		   val ev   = E.freshEnvVar ()
		   val c    = E.initEnvConstraint (E.consENVVAR ev L.dummyLab) env1 L.dummyLab
		   val env3 = E.CONSTRAINT_ENV (E.singleConstraint (L.dummyLab, c))
		   val env4 = E.ENVDEP (EL.initExtLab (E.consENVVAR ev lab) lab)
		   val env5 = E.SEQUENCE_ENV (env3, E.LOCAL_ENV (env4, env2))
	       in (env5 (*E.LOCAL_ENV (env1, env2)*), E.uenvcss [css1, css2])
	       end
	     | f_dec (A.DecAbsType (datbinds, decs, _, lab, _)) =
	       let val (typs1, conss, cst1, cst1', css1) = f_datbindseq datbinds
		   val (env2, css2) = f_decs decs
		   val envs = map (fn (cons, SOME idl) => E.DATATYPE_CONSTRUCTOR_ENV (idl, E.ENVPOL (E.emtv, E.projVids cons))
				    | (cons, NONE) => E.ENVPOL (E.emtv, E.projVids cons)) conss
		   val env3 = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst1', E.envsToSeq envs)
		   val env4 = E.LOCAL_ENV (env3, env2)
		   val env5 = E.SEQUENCE_ENV (E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst1, E.consEnvironmentTypenames typs1), env4)
		   val css0 = if getBasis ()
			      then E.emptyContextSensitiveSyntaxError
			      else E.singcss (notFullyCs lab "abstype")
		   val css  = E.uenvcss [css1, css2, css0]
	       in (env5, css)
	       end
	     | f_dec (A.DecAbsWith (datbinds, typbinds, decs, _, lab, _)) =
	       let val (typs1, conss, cst1, cst1', css1) = f_datbindseq datbinds
		   val (typs2, cst2, css2) = f_typbindseq typbinds
		   val (env3, css3) = f_decs decs
		   val envs = map (fn (cons, SOME idl) => E.DATATYPE_CONSTRUCTOR_ENV (idl, E.ENVPOL (E.emtv, E.projVids cons))
				    | (cons, NONE) => E.ENVPOL (E.emtv, E.projVids cons)) conss
		   val env4 = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst1', E.envsToSeq envs)
		   val env5 = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst1,  E.consEnvironmentTypenames typs1)
		   val env6 = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst2,  E.consEnvironmentTypenames typs2)
		   val env7 = E.SEQUENCE_ENV (env5, E.SEQUENCE_ENV (env6, E.LOCAL_ENV (env4, env3)))
		   val css0 = if getBasis ()
			      then E.emptyContextSensitiveSyntaxError
			      else E.singcss (notFullyCs lab "abstype")
		   val css  = E.uenvcss [css1, css2, css3, css0]
	       in (env7, css)
	       end
	     | f_dec (A.DecInfix (i, identseq, _, lab, _)) =
	       let val css1 = if getBasis ()
			      then E.emptyContextSensitiveSyntaxError
			      else E.singcss (notFullyCs lab "infix")
		   val css2 = if 0 <= i andalso i <= 9
			      then E.emptyContextSensitiveSyntaxError
			      else E.singcss (fixityCs lab)
	       in (E.emptyEnvironment, E.uenvcss [css1, css2])
	       end
	     | f_dec (A.DecInfixr (i, identseq, _, lab, _)) =
	       let val css1 = if getBasis ()
			      then E.emptyContextSensitiveSyntaxError
			      else E.singcss (notFullyCs lab "infixr")
		   val css2 = if 0 <= i andalso i <= 9
			      then E.emptyContextSensitiveSyntaxError
			      else E.singcss (fixityCs lab)
	       in (E.emptyEnvironment, E.uenvcss [css1, css2])
	       end
	     | f_dec (A.DecNonfix (identseq, _, lab, _)) =
	       let val css = if getBasis ()
			     then E.emptyContextSensitiveSyntaxError
			     else E.singcss (notFullyCs lab "nonfix")
	       in (E.emptyEnvironment, css)
	       end
	     | f_dec (A.DecOverload (labid, labtyp, labtyvar, tyclassseq, _, lab, _)) =
	       let val (tv1, vids, cst1, css1) = f_labid labid
		   val (tv2, cst2, css2) = f_labtype labtyp
		   val (tv3, tyvs, cst3) = f_labtypevarbind labtyvar
		   val (sv4, cst4, css4) = f_tyclassseq tyclassseq
		   val lid  = Option.getOpt (A.getlabidLabId labid, (I.dummyId, L.dummyLab))
		   val c1   = E.initTypeConstraint (T.consV tv3) (T.OR (T.SV sv4, T.freshidor (), T.POLY, T.VAL lid, lab)) lab
		   val c2   = E.initTypeConstraint (T.consV tv1) (T.consV tv2) lab
		   val cst  = E.conscsts (lab, [c1, c2]) (E.unionConstraintsList [cst1, cst3, cst4])
		   val css  = E.uenvcss [css1, css2, css4]
		   val env1 = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst2, E.projVids (E.toRECVids vids (L.singleton lab)))
		   val env2 = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst, E.ENVPOL (tyvs, env1))
	       in (env2, css)
	       end
	     | f_dec (A.DecClass (labclass, tyclassseq, _, lab, _)) =
	       let val (sv1, ovcs, cst1) = f_labclassbind labclass
		   val (sv2, cst2, css2) = f_tyclassseq tyclassseq
		   val c   = E.initSequenceConstraint (T.SV sv1) (T.SV sv2) lab
		   val cst = E.consConstraint(lab, c) (E.unionConstraintsList [cst1, cst2])
		   val env = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst, E.projOvcs ovcs)
	       in (env, css2)
	       end
	     | f_dec (A.DecDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emptyContextSensitiveSyntaxError)
	       end

	   and f_declist []    = (E.emptyEnvironment, E.emptyContextSensitiveSyntaxError)
	     | f_declist [dec] = f_dec dec
	     | f_declist (dec :: decs) =
	       let val (env1, css1) = f_dec dec
		   val (env2, css2) = f_declist decs
	       in (E.SEQUENCE_ENV (env1, env2), E.uenvcss [css1, css2])
	       end

	   and f_decs (A.Decs (decs, _)) = f_declist decs
	     | f_decs (A.DecsDots pl)  =
	       let val env = f_partlist pl
	       in (env, E.emptyContextSensitiveSyntaxError)
	       end

	   (*and f_decslet (A.Decs (ds, _)) = map (fn x => f_dec x) ds
	     | f_decslet (A.DecsDots pl)  =
	       let val (env, _, cs) = f_partlist pl
	       in [(env, E.emptyEnvironment, cs)]
	       end*)

	   (* RETURNS: (Env.varenv, E.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
	   and f_valdescone (A.ValDescOne (labid, labtyp, _, lab, _)) =
	       let val (tv1, vids, cst1, css1) = f_labid labid
		   val (tv2, cst2, css2) = f_labtype labtyp
		   val c   = E.initTypeConstraint (T.consV tv1) (T.consV tv2) lab
		   val cst = E.consConstraint(lab, c) (E.unionConstraintsList [cst1, cst2])
		   val css = E.uenvcss [css1, css2]
	       in (E.toRECVids vids (L.singleton lab), cst, css)
	       end
	     | f_valdescone (A.ValDescOneDots pl) =
	       let val env = f_partlist pl
	       in (E.getVids env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.varenv, Env.emptyConstraint, Env.emptyContextSensitiveSyntaxError) *)
	   and f_valdesc (A.ValDesc (valdescs, _, _)) =
	       let val (vidss, csts, csss) = unzipThree (map f_valdescone valdescs)
		   val vids = E.unionEnvironmentList vidss
		   val cst  = E.unionConstraintsList csts
		   val css  = E.uenvcss csss
	       in (vids, cst, css)
	       end
	     | f_valdesc (A.ValDescDots pl) =
	       let val env = f_partlist pl
	       in (E.getVids env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* this is used to represent one single type description, for example the 'int' in 'eqtype int' *)
	   and f_typdescone (A.TypDescOne (datname, lab, _)) =
	       let val (s, v, tv, sv1, sv2, typs, tyvs, cst, css) = f_datname datname
		   (* NOTE: tyvs is not used *)
		   val id   = Option.getOpt (Option.map (fn (id, _) => id) v, I.dummyId)
		   val lab' = Option.getOpt (Option.map (fn (_, lab) => lab) v, lab)
		   val tn   = if benv andalso getBasis () then getTyNameString s else T.freshtyname ()
		   val c1   = E.initSequenceConstraint (T.SV sv1) (T.SV sv2) lab
		   val c2   = E.initTypeConstraint (T.consV tv) (T.C (T.NC (tn, T.DE id, lab'), T.SV sv1, lab')) lab'
		   val tnop = Option.map (fn (id, lab) => {id = id, lab = lab, kind = E.TYP, name = tn}) v
	       in (tnop, typs, E.consConstraint(lab, c1) (E.consConstraint(lab', c2) cst), css)
	       end
	     | f_typdescone (A.TypDescOneDots pl) =
	       let val env = f_partlist pl
	       in (NONE, E.getTyps env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* called when types or equality types are defined in a signature
	    *
	    * from the definition: spec ::= [...] | type typdesc | eqtype typdesc | [...] *
	    *                   typdesc ::= tyvarsec tycon <and typdesc> *
	    *)
	   and f_typdesc (A.TypDesc (typdescs, _, _)) =
	       let val _ = D.printDebug 2 D.AZE "generating constraints for A.TypDesc"

		   (* we have f_typdescone over the type descriptions, as there might be multiple of them.
		    * If there are then each type description will be represented by f_typsescone via the map.
                    * We then call unzipFour which puts all of the first element in the tuple of each f_typdescone in a list,
                    * and then all the second elements, third, and fourth (we union these in the next lines) *)
		   val (tnss, typss, constraints, csss) = unzipFour (map f_typdescone typdescs)

		   (* because we have all first to fourth elements of the dypdescs now in a list, we have to union them *)
		   val typs = E.unionEnvironmentList typss

		   (* unions a list of CONSTRAINTS values
		    * that is, unions a list of lists of constraints *)
		   val constarint  = E.unionConstraintsList constraints

		   (* unions a list of context sensitive syntax errors *)
		   val css  = E.unionContextSensitiveErrorList csss

		   (* the list tnss contains items of the form NONE or SOME(x)
		    * mapPartial (fn x => x) will remove all items of the form NONE from the list and return a list of the x values of items with the SOME(x) form *)
		   val tns  = List.mapPartial (fn x => x) tnss

	       in (tns, typs, constraint, css)
	       end
	     | f_typdesc (A.TypDescDots pl) =
	       let val env = f_partlist pl
	       in ([], E.getTyps env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.env, Env.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
	   and f_tdrdescone (A.TdrDescOne (datname, labtyp, _, lab, _)) =
	       let val (s, v, tv1, sv1, sv2, typs, tyvs, cst1, css1) = f_datname datname
		   val (tv2, cst2, css2) = f_labtype labtyp
		   val c1  = E.initTypeConstraint (T.consV tv1) (T.consV tv2) lab
		   val c2  = E.initSequenceConstraint (T.SV sv1) (T.SV sv2) lab
		   val c3  = E.LET_CONSTRAINT (E.SEQUENCE_ENV (E.projTyvs tyvs, E.CONSTRAINT_ENV cst2))
		   val env = E.ENVDEP (EL.initExtLab (E.consEnvironmentTypenames typs) lab)
		   val cst = E.conscsts (lab, [c1, c2]) (E.consConstraint(L.dummyLab, c3) cst1)
		   val css = E.uenvcss [css1, css2]
	       in (env, cst, css)
	       end
	     | f_tdrdescone (A.TdrDescOneDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.env, E.emptyContextSensitiveSyntaxError) *)
	   and f_tdrdesclist []                    = (E.emptyEnvironment, E.emptyContextSensitiveSyntaxError)
	     | f_tdrdesclist [tdrdesc]             =
	       let val (env, cst, css) = f_tdrdescone tdrdesc
	       in (E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst, env), css)
	       end
	     | f_tdrdesclist (tdrdesc :: tdrdescs) =
	       let val (env1, cst, css1) = f_tdrdescone tdrdesc
		   val (env2, css2) = f_tdrdesclist tdrdescs
		   val css = E.uenvcss [css1, css2]
	       in (E.SEQUENCE_ENV (E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst, env1), env2), css)
	       end

	   (* RETURNS: (Env.env, E.emptyContextSensitiveSyntaxError) *)
	   and f_tdrdesc (A.TdrDesc (tdrdescs, _, _)) = f_tdrdesclist tdrdescs
	       (*let val (typss, csts, csss) = unzipThree (map f_tdrdescone tdrdescs)
		   val typs = E.unionEnvironmentList typss
		   val cst  = E.unionConstraintsList csts
		   val css  = E.uenvcss csss
	       in (typs, cst, css)
	       end*)
	     | f_tdrdesc (A.TdrDescDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.varenv, Env.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
	   and f_excdescone (A.ExcDescOne (ident, lab, _)) =
	       let val (tv, cons, cst, css) = f_identpat ident
		   val c = E.initTypeConstraint (T.consV tv) (T.constyexception' lab (T.DE I.dummyId)) lab
	       in (E.toEX0Vids cons (L.singleton lab), E.consConstraint(lab, c) cst, css)
	       end
	     | f_excdescone (A.ExcDescOf (labid, labtyp, _, lab, _)) =
	       let val (tv1, cons, cst1, css1) = f_labid labid
		   val (tv2, cst2, css2) = f_labtype labtyp
		   val ty  = T.constyexception' lab (T.DE I.dummyId)
		   val c   = E.initTypeConstraint (T.consV tv1) (T.consTyArrowTy (T.consV tv2) ty lab (T.DE I.dummyId)) lab
		   val cst = E.consConstraint(lab, c) (E.unionConstraintsList [cst1, cst2])
		   val css = E.uenvcss [css1, css2]
	       in (E.toEX1Vids cons (L.singleton lab), cst, css)
	       end
	     | f_excdescone (A.ExcDescOneDots pl) =
	       let val env = f_partlist pl
	       in (E.getVids env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.varenv, Env.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
	   and f_excdesc (A.ExcDesc (exdescs, _, _)) =
	       let val (vidss, csts, csss) = unzipThree (map f_excdescone exdescs)
		   val vids = E.unionEnvironmentList vidss
		   val cst  = E.unionConstraintsList csts
		   val css  = E.uenvcss csss
	       in (vids, cst, css)
	       end
	     | f_excdesc (A.ExcDescDots pl) =
	       let val env = f_partlist pl
	       in (E.getVids env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.tyvar, Env.varenv, Env.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
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
		   val c1   = E.initTypeConstraint (T.consV tv1) (T.constyarrow' tv2 tv lab (T.DE I.dummyId)) lab
		   val c2   = E.initClassConstraint clv2 (CL.consDA1 ()) lab
		   val c3   = E.initClassConstraint clv1 clv2 lab1
		   val c4   = E.initClassConstraint clv1 (CL.consDAT ()) lab2
		   val cst  = E.unionConstraintsList [cst1, cst2]
		   val css  = E.uenvcss [css1, css2]
		   val cst' = E.conscsts (lab, [c1, c2]) (E.consConstraint(lab1, c3) (E.consConstraint(lab2, c4) cst))
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
	       in (tv, E.getVids env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.tyvar list, Env.varenv, Env.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
	   and f_condesc (A.ConDesc (condescs, _, _)) =
	       let val (tvs, conss, csts, csss) = unzipFour (map f_condescone condescs)
		   val cons = E.unionEnvironmentList conss
		   val cst  = E.unionConstraintsList csts
		   val css  = E.uenvcss csss
	       in (tvs, cons, cst, css)
	       end
	     | f_condesc (A.ConDescDots pl) =
	       let val env = f_partlist pl
	       in ([], E.getVids env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: ((Id.id, Ty.tyname) option, Env.typenv, E.varenv * Id.idl option, E.emptyConstraint, E.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
	   and f_datdescone (A.DatDescOne (datname, consdesc, _, lab, _)) =
	       let val (s, v, tv, sv1, sv2, typs, tyvs, cst1, css1) = f_datname datname
		   val (tvs, cons, cst2, css2) = f_condesc consdesc
		   val tn   = if benv andalso getBasis () then getTyNameString s else T.freshtyname ()
		   val id   = Option.getOpt (Option.map (fn (i, _) => i) v, I.dummyId)
		   val lab' = Option.getOpt (Option.map (fn (_, l) => l) v, lab)
		   val c1   = E.initSequenceConstraint (T.SV sv1) (T.SV sv2) lab
		   val c2   = E.initTypeConstraint (T.consV tv) (T.C (T.NC (tn, T.DE id, lab'), T.SV sv1, lab')) lab'
		   val c3   = E.LET_CONSTRAINT (E.SEQUENCE_ENV (E.projTyvs tyvs, E.CONSTRAINT_ENV cst2))
		   val cs   = map (fn x => E.initTypeConstraint (T.consV x) (T.consV tv) lab) tvs
		   val cst  = E.consConstraint(lab, c1) (E.consConstraint(lab', c2) cst1)
		   val cst' = E.conscsts (lab, cs) (E.singleConstraint (L.dummyLab, c3))
		   val tnop = Option.map (fn (id, lab) => {id = id, lab = lab, kind = E.DAT, name = tn}) v
		   val typs = E.toTYCONTyps typs E.emvar false (L.singleton lab)
		   (*val cs4 = checkTyVarInc (A.gettyvarDatName dn) (A.getlabDatName dn) (A.gettyvarConDesc cd)*)
	       in (tnop, typs, (cons, v), cst, cst', E.uenvcss [css1, css2])
	       end
	     | f_datdescone (A.DatDescOneDots pl) =
	       let val env = f_partlist pl
	       in (NONE, E.getTyps env, (E.getVids env, NONE), E.emptyConstraint, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: ((Id.id, Ty.tyname) option, Env.typenv, (E.varenv * Id.idl option) list, E.emptyConstraint, E.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
	   and f_datdesc (A.DatDesc (datdescs, _, _)) =
	       let val (tnss, typss, conss, csts, csts', csss) = unzipSix (map f_datdescone datdescs)
		   val typs = E.unionEnvironmentList typss
		   val cst  = E.unionConstraintsList csts
		   val cst' = E.unionConstraintsList csts'
		   val css  = E.uenvcss csss
		   val tns  = List.mapPartial (fn x => x) tnss
	       in (tns, typs, conss, cst, cst', css)
	       end
	     | f_datdesc (A.DatDescDots pl) =
	       let val env = f_partlist pl
	       in ([], E.getTyps env, [(E.getVids env, NONE)], E.emptyConstraint, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.strenv, E.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
	   and f_strdescone (A.StrDescOne (strid, labsigexp, _, lab, _)) =
	       let val (ev1, strs, cst1, css1) = f_strid strid
		   val (ev2, cst2, css2) = f_labsigexp labsigexp
		   val c   = E.initEnvConstraint (E.consENVVAR ev1 lab) (E.consENVVAR ev2 lab) lab
		   val cst = E.consConstraint(lab, c) (E.unionConstraintsList [cst1, cst2])
		   val css = E.uenvcss [css1, css2]
	       in (strs, cst, css)
	       end
	     | f_strdescone (A.StrDescOneDots pl) =
	       let val env = f_partlist pl
	       in (E.getStrs env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.strenv, E.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
	   and f_strdesc (A.StrDesc (strdescs, _, _)) =
	       let val (strss, csts, csss) = unzipThree (map f_strdescone strdescs)
		   val strs = E.unionEnvironmentList strss
		   val cst  = E.unionConstraintsList csts
		   val css  = E.uenvcss csss
	       in (strs, cst, css)
	       end
	     | f_strdesc (A.StrDescDots pl) =
	       let val env = f_partlist pl
	       in (E.getStrs env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (E.env, E.emptyContextSensitiveSyntaxError) *)
	   and f_speconesmltes (A.SpecValue (valdesc, _, lab, _)) =
	       let val (vids, cst1, css) = f_valdesc valdesc
		   val (tyvs, cst2) = f_typevarvallist (A.gettyvarValDesc valdesc)
		   val env1 = E.ENVPOL (tyvs, E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst1, E.projVids vids))
		   val env2 = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst2, env1)
		   val ev   = E.freshEnvVar ()
		   val c    = E.initEnvConstraint (E.consENVVAR ev lab) env2 lab
		   val env3 = E.SEQUENCE_ENV (E.CONSTRAINT_ENV (E.singleConstraint (lab, c)), E.ENVDEP (EL.initExtLab (E.consENVVAR ev lab) lab))
	       in (env3, css)
	       end
	     | f_speconesmltes spec = f_specone spec

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_speconesmlteslist []        = (E.emptyEnvironment, E.emptyContextSensitiveSyntaxError)
	     | f_speconesmlteslist [x]       = f_speconesmltes x
	     | f_speconesmlteslist (x :: xs) =
	       let val (env1, css1) = f_speconesmltes x
		   val (env2, css2) = f_speconesmlteslist xs
	       in (E.SEQUENCE_ENV (env1, env2), E.uenvcss [css1, css2])
	       end

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_specsmltes (A.Spec (specs, _)) =
	       let val (env, css) = f_speconesmlteslist specs
	       (*(2010-04-19)These gets might pose problem with SEQUENCE_ENV and ENVOPN.
		* They pose problem.  We need to build before and then check at unification
		* time that we don't have duplicates for signatures only.*)
	       (*val csc1 = if E.isEnvC env2 then consCSM [E.getVids env2, E.getCons env2] false else []*)
	       (*val csc2 = if E.isEnvC env2 then consCSM [E.getTyps env2] false else []*)
	       in (env, css)
	       end
	     | f_specsmltes (A.SpecDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.tvenv, Env.cst) *)
	   and f_typevarspec (A.TypeVar (_, n, _, lab, _)) =
	       let val _   = D.printDebug 2 D.AZE ("in f_typevarspec - generating constraints for A.TypeVar (lab = "^Int.toString(L.toInt(lab))^")")
		   (* generate some fresh type variables *)
		   val tv1  = T.freshtyvar ()
		   val tv2  = T.freshtyvar ()
		   (* what does this actually do? singenv = single environment? *)
		   val tyvs = E.consSingleEnv (n, [E.consBindMono n (tv1, true) (CL.consTYVAR ()) lab])
		   (* generate a type constraint (how is this actually used later?) *)
		   val c    = E.initTypeConstraint (T.consV tv1) (T.V (tv2, SOME (n, lab), T.POLY)) lab
	       in (tyvs, E.singleConstraint (lab, c))
	       end
	     | f_typevarspec A.TypeVarDots = (E.emtv, E.emptyConstraint)

	   (* RETURNS: (Env.tvenv, Env.cst) *)
	   and f_typevarspeclist [] = (E.emtv, E.emptyConstraint)
	     | f_typevarspeclist [typevar] = f_typevarspec typevar
	     | f_typevarspeclist (typevar ::  typevars) =
	       let val (tyvs1, cst1) = f_typevarspec typevar
		   val (tyvs2, cst2) = f_typevarspeclist typevars
	       in (E.unionEnvironmentList [tyvs1, tyvs2], E.unionConstraintsList [cst1, cst2])
	       end

	   (* value definitions inside a signature *)
	   and f_specone (A.SpecValue (valdesc, _, lab, _)) =
	       let val _   = D.printDebugFunc 2 D.AZE (fn _ => ("in f_specone - generating constraints for A.SpecValue (lab = "^Int.toString(L.toInt(lab))^")"))
		   val (vids, cst1, css) = f_valdesc valdesc
		   (*(2010-06-23)Before we were using f_typevarvallist which generates binders
		    * of explicit type variables, now we use binders of implicit type variables
		    * tagged with the labels of the implicit type variables they're coming from. *)
		   val (tyvs, cst2) = f_typevarspeclist (A.gettyvarValDesc valdesc)
		   val env1 = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst2, E.projTyvs tyvs)
		   val env2 = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst1, E.ENVDEP (EL.initExtLab (E.projVids vids) lab))
		   (*val env3 = E.LOCAL_ENV (env1, env2)*)
		   val env3 = E.ENVPOL (E.emtv, E.LOCAL_ENV (E.ENVDEP (EL.initExtLab env1 lab), env2))
		   val ev1  = E.freshEnvVar ()
		   val ev2  = E.freshEnvVar ()
		   val c1   = E.initEnvConstraint (E.consENVVAR ev1 L.dummyLab) env3 L.dummyLab
		   val c2   = E.initEnvConstraint (E.consENVVAR ev2 lab) (E.consENVVAR ev1 lab) lab
		   val cst  = E.consConstraint(L.dummyLab, c1) (E.singleConstraint (lab, c2))
		   val env4 = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst, E.ENVDEP (EL.initExtLab (E.consENVVAR ev2 lab) lab))
	       (*(2010-06-17)Don't we want to use ENVPOL on vids?
		* The problem is that when we check a signature against a structure
		* then we want the explicit type variables to be explicit to check
		* that the structure is at least as general as the signature. *)
	       (* NOTE: we have to bind the explicit type variables. *)
	       in (env4, css)
	       end

	     (* type declarations inside a signature *)
	     | f_specone (A.SpecType (typdesc, _, lab, _)) =
	       let val (tns, typs, constraints, css) = f_typdesc typdesc
		   val env  = E.SEQUENCE_ENV (E.CONSTRAINT_ENV constraints, E.updateInfoTypenames tns (E.consEnvironmentTypenames typs))
		   val ev   = E.freshEnvVar ()
		   val c    = E.initEnvConstraint (E.consENVVAR ev lab) env lab
		   val env' = E.SEQUENCE_ENV (E.CONSTRAINT_ENV (E.singleConstraint (lab, c)), E.ENVDEP (EL.initExtLab (E.consENVVAR ev lab) lab))
	       in (env', css)
	       end

	     (* equality types declarations inside a signature *)
	     | f_specone (A.SpecEqtype (typdesc, _, lab, _)) =
	       let val _ = D.printDebug 2 D.AZE ("generating constraints for A.SpecEqtype (lab = "^Int.toString(L.toInt lab)^")")

		   (* we call f_typdesc so that we can (get/generate)? information about the typdesc (type description? what does that mean?)
		    * the first element of the tuple represents xxx
		    * the second element of the tuple represents xxx
		    * the third element of the tuple represents the constraints that have been built from (???)
		    * the fourth element of the tuple represents context sensitive syntax error information
		    *)
		   val (tns, typs, constraints, contextSensitiveSyntaxError) = f_typdesc typdesc


		   val env  = E.SEQUENCE_ENV (E.CONSTRAINT_ENV constraints, E.updateInfoTypenames tns (E.consEnvironmentTypenames typs))

		   (* generate fresh environment variables that we use later *)
		   val envVar   = E.freshEnvVar ()

		   (* equivalent to ENV_CONSTRAINT (EL.initExtLab (ENVVAR (envVar, lab), env) lab) *)
		   (* equivalent to ENV_CONSTRAINT <need to know what initExtLab really is!>*)
		   val c = E.initEnvConstraint (E.consENVVAR envVar lab) env lab

		   val env' = E.SEQUENCE_ENV (E.CONSTRAINT_ENV (E.singleConstraint (lab, c)), E.ENVDEP (EL.initExtLab (E.consENVVAR envVar lab) lab))
	       in (env', contextSensitiveSyntaxError)
	       end
	     | f_specone (A.SpecTdr (tdrdesc, _, lab, _)) =
	       let val (env, css) = f_tdrdesc tdrdesc
		   val ev1  = E.freshEnvVar ()
		   val ev2  = E.freshEnvVar ()
		   val c1   = E.initEnvConstraint (E.consENVVAR ev1 L.dummyLab) env L.dummyLab
		   val c2   = E.initEnvConstraint (E.consENVVAR ev2 lab) (E.consENVVAR ev1 lab) lab
		   val env1 = E.CONSTRAINT_ENV (E.consConstraint(L.dummyLab, c1) (E.singleConstraint (lab, c2)))
		   val env2 = E.SEQUENCE_ENV (env1, E.ENVDEP (EL.initExtLab (E.consENVVAR ev2 lab) lab))
	       (* We can't have an ENVDEP here because we can have an error in
		* the type part that does not need the spec to exist.
		* It is the same for all the specs because of the type parts. *)
	       in (env2, css)
	       end

	     (* exception declaration inside a signature *)
	     | f_specone (A.SpecException (exdesc, _, lab, _)) =
	       let val (cons, cst, css) = f_excdesc exdesc
		   val env  = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst, E.ENVDEP (EL.initExtLab (E.projVids cons) lab))
		   val ev1  = E.freshEnvVar ()
		   val ev2  = E.freshEnvVar ()
		   val c1   = E.initEnvConstraint (E.consENVVAR ev1 L.dummyLab) env L.dummyLab
		   val c2   = E.initEnvConstraint (E.consENVVAR ev2 lab) (E.consENVVAR ev1 lab) lab
		   val cst' = E.consConstraint(L.dummyLab, c1) (E.singleConstraint (lab, c2))
		   val env' = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst', E.ENVDEP (EL.initExtLab (E.consENVVAR ev2 lab) lab))
	       in (env', css)
	       end
	     | f_specone (A.SpecDat (datdesc, _, lab, _)) =
	       let
		   val _ = D.printDebug 2 D.AZE ("generating constraints for A.SpecDat (lab = "^Int.toString(L.toInt lab)^")")

		   val (tns, typs, conss, cst1, cst2, css) = f_datdesc datdesc
		   val envs = map (fn (cons, SOME idl) => E.DATATYPE_CONSTRUCTOR_ENV (idl, E.ENVPOL (E.emtv, E.projVids cons))
				    | (cons, NONE) => E.ENVPOL (E.emtv, E.projVids cons)) conss
		   val env1 = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst2, E.envsToSeq envs)
		   val env2 = E.updateInfoTypenames tns (E.consEnvironmentTypenames typs)
		   val env3 = E.SEQUENCE_ENV (E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst1, E.ENVDEP (EL.initExtLab env2 lab)), env1)
		   val ev1  = E.freshEnvVar ()
		   val ev2  = E.freshEnvVar ()
		   val c1   = E.initEnvConstraint (E.consENVVAR ev1 L.dummyLab) env3 L.dummyLab
		   val c2   = E.initEnvConstraint (E.consENVVAR ev2 lab) (E.consENVVAR ev1 lab) lab
		   val cst  = E.consConstraint(L.dummyLab, c1) (E.singleConstraint (lab, c2))
		   val env4 = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst, E.ENVDEP (EL.initExtLab (E.consENVVAR ev2 lab) lab))
	       in (env4, css)
	       end
	     | f_specone (A.SpecStr (strdesc, _, lab, _)) =
	       let val (strs, cst, css) = f_strdesc strdesc
		   val env  = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst, E.ENVDEP (EL.initExtLab (E.projStrs strs) lab))
		   val ev1  = E.freshEnvVar ()
		   val ev2  = E.freshEnvVar ()
		   val c1   = E.initEnvConstraint (E.consENVVAR ev1 L.dummyLab) env L.dummyLab
		   val c2   = E.initEnvConstraint (E.consENVVAR ev2 lab) (E.consENVVAR ev1 lab) lab
		   val env1 = E.CONSTRAINT_ENV (E.consConstraint(L.dummyLab, c1) (E.singleConstraint (lab, c2)))
		   val env2 = E.SEQUENCE_ENV (env1, E.ENVDEP (EL.initExtLab (E.consENVVAR ev2 lab) lab))
		   (*val _ = D.printdebug2 (L.printLab lab)*)
		   (*val _ = D.printdebug2 (E.printEnv env "")*)
	       in (env2, css)
	       end
	     | f_specone (A.SpecInc (labsigexp, _, lab, _)) =
	       let val (ev, cst, css) = f_labsigexp labsigexp
		   val ev'  = E.freshEnvVar ()
		   val c    = E.initEnvConstraint (E.consENVVAR ev' lab) (E.consENVVAR ev lab) lab
		   val env1 = E.CONSTRAINT_ENV (E.consConstraint(lab, c) cst)
		   val env2 = E.SEQUENCE_ENV (env1, E.ENVDEP (EL.initExtLab (E.consENVVAR ev' lab) lab))
		   val css' = if getBasis ()
			      then E.emptyContextSensitiveSyntaxError
			      else E.singcss (sorryCsS lab "include")
	       in (env2, css')
	       end
	     | f_specone (A.SpecIsi (_, _, lab, _)) =
	       let val css = if getBasis ()
			     then E.emptyContextSensitiveSyntaxError
			     else E.singcss (sorryCsS lab "include")
	       in (E.emptyEnvironment, css)
	       end
	     | f_specone (A.SpecRep (tycon, longtycon, _, lab, _)) =
	       let val (s, v, tv, sv, typs, cst1) = f_tyconbind tycon
		   val (tfv, cst2) = f_longtycon longtycon
		   val c   = E.initFunctionTypeConstraint (T.TFV tfv) (T.TFC (T.SV sv, T.consV tv, lab)) lab
		   val cst = E.consConstraint(lab, c) (E.unionConstraintsList [cst1, cst2])
		   val opn = case A.longtyconToLid longtycon of
				 NONE => E.emopn
			       | SOME lid => E.singOEnv (lid, lab, E.DRE)
		   val env1 = case v of SOME idl => E.DATATYPE_CONSTRUCTOR_ENV (idl, E.ENVOPN opn) | NONE => E.ENVOPN opn
		   val env2 = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst, E.SEQUENCE_ENV (E.consEnvironmentTypenames typs, env1))
		   val ev   = E.freshEnvVar ()
		   val c    = E.initEnvConstraint (E.consENVVAR ev lab) env2 lab
		   val env3 = E.SEQUENCE_ENV (E.CONSTRAINT_ENV (E.singleConstraint (lab, c)), E.ENVDEP (EL.initExtLab (E.consENVVAR ev lab) lab))
	       in (env3, E.emptyContextSensitiveSyntaxError)
	       end
	     | f_specone (A.SpecSha (spec, longtyconeq, _, lab, _)) =
	       let val (env1, css) = f_spec spec
		   val env2 = f_longtyconeq longtyconeq
		   (*(2010-07-02)This is not the correct thing to do for sharing.*)
		   val ev   = E.freshEnvVar ()
		   val ev1  = E.freshEnvVar ()
		   val ev2  = E.freshEnvVar ()
		   val c1   = E.initEnvConstraint (E.consENVVAR ev1 lab) env1 lab
		   val c2   = E.initEnvConstraint (E.consENVVAR ev2 lab) env2 lab
		   val c3   = E.SHARING_CONSTRAINT (ev1, ev, ev2, lab)
		   (*val c3   = E.SIGNATURE_CONSTRAINT (ev1, SOME ev, ev2, NONE, lab, E.WHR)*)
		   val env3 = E.SEQUENCE_ENV (E.CONSTRAINT_ENV (E.singcsts (lab, [c1, c2])), E.CONSTRAINT_ENV (E.singleConstraint (lab, c3)))
		   val env4 = E.SEQUENCE_ENV (env3, E.consENVVAR ev lab)
		   val css' = if getBasis ()
			      then E.emptyContextSensitiveSyntaxError
			      else E.singcss (poorlyCs lab "sharing")
	       in (env4, E.uenvcss [css, css'])
	       end
	     | f_specone (A.SpecSsi (spec, longstrideq, _, lab, _)) =
	       let val (env, css) = f_spec spec
		   val css' = if getBasis ()
			      then E.emptyContextSensitiveSyntaxError
			      else E.singcss (poorlyCs lab "sharing")
		   val env' = E.newEnvVar lab
	       in (E.SEQUENCE_ENV (env, env'), E.uenvcss [css, css'])
	       end
	     | f_specone (A.SpecOneDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: Env.env *)
	   and f_longtyconeq (A.LongTyConEq (longtycons, _, lab, _)) =
	       let val (tfvs, envs, csts) = unzipThree (map f_longtyconbind longtycons)
		   val env = E.uenvEnv envs
		   val cst = E.unionConstraintsList csts
		   val tfc = T.TFC (T.newSV (), T.newV (), lab)
		   val cs  = map (fn tfv => E.initFunctionTypeConstraint (T.TFV tfv) tfc lab) tfvs
	       in E.SEQUENCE_ENV (E.CONSTRAINT_ENV (E.conscsts (lab, cs) cst), env)
	       end
	     | f_longtyconeq (A.LongTyConEqDots pl) =
	       let val env = f_partlist pl
	       in env
	       end

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_speconelist []        = (E.emptyEnvironment, E.emptyContextSensitiveSyntaxError)
	     | f_speconelist [x]       = f_specone x
	     | f_speconelist (x :: xs) =
	       let val (env1, css1) = f_specone x
		   val (env2, css2) = f_speconelist xs
	       in (E.SEQUENCE_ENV (env1, env2), E.uenvcss [css1, css2])
	       end

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_spec (A.Spec (specs, _)) =
	       let val (env, css) = f_speconelist specs
	       (*(2010-04-19)These gets might pose problem with SEQUENCE_ENV and ENVOPN.
		* They pose problem.  We need to build before and then check at unification
		* time that we don't have duplicates for signatures only.*)
	       (*val csc1 = if E.isEnvC env2 then consCSM [E.getVids env2, E.getCons env2] false else []*)
	       (*val csc2 = if E.isEnvC env2 then consCSM [E.getTyps env2] false else []*)
	       in (env, css)
	       end
	     | f_spec (A.SpecDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_strbind (A.StrBind (strbinds, _, _)) =
	       let val (strs, cst, css) = f_strbindonelist strbinds
	       in (E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst, E.projStrs strs), css)
	       end
	     | f_strbind (A.StrBindDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.strenv, Env.cst, Env.css) *)
	   and f_strbindonelist [] = (E.emstr, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	     | f_strbindonelist [x] = f_strbindone x
	     | f_strbindonelist (x :: xs) =
	       let val (strs1, cst1, css1) = f_strbindone x
		   val (strs2, cst2, css2) = f_strbindonelist xs
		   val strs = E.unionEnvironmentList [strs1, strs2]
		   val cst  = E.unionConstraintsList [cst1, cst2]
		   val css  = E.uenvcss [css1, css2]
	       in (strs, cst, css)
	       end

	   (* RETURNS: (Env.strenv, Env.cst, Env.css) *)
	   and f_strbindone (A.StrBindOneOp (strid, labsigexp, labstrexp, _, lab, _)) =
	       let val (ev1, cst1, css1) = f_labstrexp labstrexp
		   val (ev2, cst2, css2) = f_labsigexp labsigexp
		   val (ev3, strs, cst3, css3) = f_strid strid
		   val c    = E.SIGNATURE_CONSTRAINT (ev2, NONE, ev1, SOME ev3, lab)
		   val cst  = E.singleConstraint (lab, c)
		   val css  = E.uenvcss [css1, css2, css3]
		   val env  = E.SEQUENCE_ENV (E.CONSTRAINT_ENV (E.unionConstraintsList [cst1, cst2]), E.CONSTRAINT_ENV cst)
		   val cst' = E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env)
	       in (strs, E.unionConstraintsList [cst3, cst'], css)
	       end
	     | f_strbindone (A.StrBindOneTr (strid, labsigexp, labstrexp, _, lab, _)) =
	       let val (ev1, cst1, css1) = f_labstrexp labstrexp
		   val (ev2, cst2, css2) = f_labsigexp labsigexp
		   val (ev3, strs, cst3, css3) = f_strid strid
		   val c    = E.SIGNATURE_CONSTRAINT (ev2, SOME ev3, ev1, NONE, lab)
		   val cst  = E.singleConstraint (lab, c)
		   val css  = E.uenvcss [css1, css2, css3]
		   val env  = E.SEQUENCE_ENV (E.CONSTRAINT_ENV (E.unionConstraintsList [cst1, cst2]), E.CONSTRAINT_ENV cst)
		   val cst' = E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env)
	       in (strs, E.unionConstraintsList [cst3, cst'], css)
	       end
	     | f_strbindone (A.StrBindOne (strid, labstrexp, _, lab, _)) =
	       let val (ev1, strs, cst1, css1) = f_strid strid
		   val (ev2, cst2, css2) = f_labstrexp labstrexp
		   val c   = E.initEnvConstraint (E.consENVVAR ev1 lab) (E.consENVVAR ev2 lab) lab
		   val cst = E.consConstraint(lab, c) (E.unionConstraintsList [cst1, cst2])
		   val css = E.uenvcss [css1, css2]
	       in (strs, cst, css)
	       end
	     | f_strbindone (A.StrBindOneDots pl) =
	       let val env = f_partlist pl
	       in (E.getStrs env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_strdec (A.StrDec (strdecones, lab, _)) =
	       f_strdeconelist strdecones
	     | f_strdec (A.StrDecDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_strdeconelist [] = (E.emptyEnvironment, E.emptyContextSensitiveSyntaxError)
	     | f_strdeconelist [strdec] = f_strdecone strdec
	     | f_strdeconelist (strdec :: strdecs) =
	       let val (env1, css1) = f_strdecone strdec
		   val (env2, css2) = f_strdeconelist strdecs
	       in (E.SEQUENCE_ENV (env1, env2), E.uenvcss [css1, css2])
	       end

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_strdecone (A.StrDecOneDec decs) = f_decs decs
	     | f_strdecone (A.StrDecOneStr (strbind, _, _)) = f_strbind strbind
	     | f_strdecone (A.StrDecOneLoc (strdec1, strdec2, _, lab, _)) =
	       let val (env1, css1) = f_strdec strdec1
		   val (env2, css2) = f_strdec strdec2
		   val ev1  = E.freshEnvVar ()
		   val ev2  = E.freshEnvVar ()
		   val c1   = E.initEnvConstraint (E.consENVVAR ev1 L.dummyLab) env1 L.dummyLab
		   val c2   = E.initEnvConstraint (E.consENVVAR ev2 lab) (E.consENVVAR ev1 lab) lab
		   val env3 = E.CONSTRAINT_ENV (E.consConstraint(L.dummyLab, c1) (E.singleConstraint (lab, c2)))
		   val env4 = E.ENVDEP (EL.initExtLab (E.consENVVAR ev2 lab) lab)
		   val env5 = E.SEQUENCE_ENV (env3, E.LOCAL_ENV (env4, env2))
	       in (env5 (*E.LOCAL_ENV (env1, env2)*), E.uenvcss [css1, css2])
	       end
	     | f_strdecone (A.StrDecOneFun (funbind, _, lab, _)) = f_funbind funbind
	     | f_strdecone (A.StrDecOneDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.envvar, Env.cst, Env.css) *)
	   and f_labstrexp (A.LabStrExp (strexp, _, _, lab, _)) =
	       let val (ev, cst, css) = f_strexp strexp
		   val ev' = E.freshEnvVar ()
		   val c   = E.initEnvConstraint (E.consENVVAR ev' lab) (E.consENVVAR ev lab) lab
	       in (ev', E.consConstraint(lab, c) cst, css)
	       end
	     | f_labstrexp (A.LabStrExpDots pl) =
	       let val ev  = E.freshEnvVar ()
		   val env = f_partlist pl
	       in (ev, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.envvar, Env.cst, Env.css) *)
	   and f_strexp (A.StrExpBasic (strdec, _, lab, _)) =
	       let val (env, css) = f_strdec strdec
		   val env = E.updateILab lab env
		   val ev1 = E.freshEnvVar ()
		   val ev2 = E.freshEnvVar ()
		   val c1  = E.initEnvConstraint (E.consENVVAR ev1 lab) (E.consENVVAR ev2 lab) lab
		   val c2  = E.initEnvConstraint (E.consENVVAR ev2 L.dummyLab) env L.dummyLab
	       in (ev1, E.consConstraint(lab, c1) (E.singleConstraint (L.dummyLab, c2)), css)
	       end
	     | f_strexp (A.StrExpId (longstrid, lab, _)) =
	       let val (ev, cst) = f_longstrid longstrid
		   val ev' = E.freshEnvVar ()
		   val c   = E.initEnvConstraint (E.consENVVAR ev' lab) (E.consENVVAR ev lab) lab
	       in (ev', E.consConstraint(lab, c) cst, E.emptyContextSensitiveSyntaxError)
	       end
	     | f_strexp (A.StrExpOp (labstrexp, labsigexp, _, lab, _)) =
	       let val (ev1, cst1, css1) = f_labstrexp labstrexp
		   val (ev2, cst2, css2) = f_labsigexp labsigexp
		   val ev  = E.freshEnvVar ()
		   val cst = E.unionConstraintsList [cst1, cst2]
		   val css = E.uenvcss [css1, css2]
		   val c   = E.SIGNATURE_CONSTRAINT (ev2, NONE, ev1, SOME ev, lab)
		   val env = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst, E.CONSTRAINT_ENV (E.singleConstraint (lab, c)))
	       in (ev, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), css)
	       end
	     | f_strexp (A.StrExpTr (labstrexp, labsigexp, _, lab, _)) =
	       let val (ev1, cst1, css1) = f_labstrexp labstrexp
		   val (ev2, cst2, css2) = f_labsigexp labsigexp
		   val ev  = E.freshEnvVar ()
		   val cst = E.unionConstraintsList [cst1, cst2]
		   val css = E.uenvcss [css1, css2]
		   val c   = E.SIGNATURE_CONSTRAINT (ev2, SOME ev, ev1, NONE, lab)
		   val env = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst, E.CONSTRAINT_ENV (E.singleConstraint (lab, c)))
	       in (ev, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), css)
	       end
	     | f_strexp (A.StrExpFExp (funid, labstrexp, _, lab, _)) =
	       let val (ev0, ev1, cst1) = f_funid funid
		   val (ev2, cst2, css) = f_labstrexp labstrexp
		   val ev  = E.freshEnvVar ()
		   val c   = E.FUNCTOR_CONSTRAINT (ev0, ev1, ev2, ev, lab)
		   val env = E.SEQUENCE_ENV (E.CONSTRAINT_ENV (E.unionConstraintsList [cst1, cst2]), E.CONSTRAINT_ENV (E.singleConstraint (lab, c)))
	       in (ev, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), css)
	       end
	     | f_strexp (A.StrExpFDec (funid, strdec, _, lab, _)) =
	       let val (ev1, ev2, cst) = f_funid funid
		   val (env, css) = f_strdec strdec
		   val ev   = E.freshEnvVar ()
		   val ev'  = E.freshEnvVar ()
		   val c1   = E.initEnvConstraint (E.consENVVAR ev' lab) env lab
		   val c2   = E.FUNCTOR_CONSTRAINT (ev1, ev2, ev', ev, lab)
		   val env1 = E.SEQUENCE_ENV (E.CONSTRAINT_ENV (E.singleConstraint (lab, c1)), E.CONSTRAINT_ENV (E.singleConstraint (lab, c2)))
		   val env2 = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst, env1)
	       in (ev, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env2), css)
	       end
	     | f_strexp (A.StrExpLocal (strdec, labstrexp, _, lab, _)) =
	       let val (env1, css1) = f_strdec strdec
		   val (ev, cst2, css2) = f_labstrexp labstrexp
		   val ev' = E.freshEnvVar ()
		   val c   = E.initEnvConstraint (E.consENVVAR ev' lab) (E.consENVVAR ev lab) lab
		   val env = E.SEQUENCE_ENV (env1, E.CONSTRAINT_ENV cst2)
		   val cst = E.consConstraint(L.dummyLab, E.LET_CONSTRAINT env) (E.singleConstraint (lab, c))
		   val css = E.uenvcss [css1, css2]
	       in (ev', cst, css)
	       end
	     | f_strexp (A.StrExpDots pl) =
	       let val env = f_partlist pl
		   val ev  = E.freshEnvVar ()
	       in (ev, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.tyfvar, Env.env, Env.cst) *)
	   and f_longtyconbind longtycon =
	       case A.longtyconToLid longtycon of
		   NONE => (T.freshtyfvar (), E.emptyEnvironment, E.emptyConstraint)
		 | SOME lid =>
		   let val lab = I.getLabId lid
		       val tfv = T.freshtyfvar ()
		       val (cst, env) = E.genLongEnv lid (T.consTFV tfv)
		   in (tfv, env, cst)
		   end

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
	       in (NONE, sv, E.emtv, E.emptyConstraint)
	       end

	   (* RETURNS: (Id.lid option, Ty.tyfvar, Env.cst, Env.css) *)
	   and f_ltreadescone (A.LTReaDOne (ldatname, labtyp, _, lab, _)) =
	       let val (lidop, sv, tyvs, cst1) = f_ldatname ldatname
		   val (tv, cst2, css) = f_labtype labtyp
		   val tfv = T.freshtyfvar ()
		   val c1  = E.initFunctionTypeConstraint (T.TFV tfv) (T.TFC (T.SV sv, T.consV tv, lab)) lab
		   val c2  = E.LET_CONSTRAINT (E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst1, E.SEQUENCE_ENV (E.projTyvs tyvs, E.CONSTRAINT_ENV cst2)))
		   (*val cs3 = checkTyVarInc (A.gettyvarLDatName dn) (A.getlabLDatName dn) (A.gettyvarLabType ty)*)
	       in (lidop, tfv, E.consConstraint(lab, c1) (E.singleConstraint (L.dummyLab, c2)), css)
	       end
	     | f_ltreadescone (A.LTReaDOneDots pl) =
	       let val env = f_partlist pl
		   val tfv = T.freshtyfvar ()
	       in (NONE, tfv, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.longtyp list, Env.cst, Env.css) *)
	   and f_ltreadesc (A.LTReaDesc (ltreadescs, _, lab, _)) =
	       foldr (fn (ltreadesc, (reas, cst, css)) =>
			 case f_ltreadescone ltreadesc of
			     (SOME lid, tfv, cst0, css0) =>
			     let val longid = {lid = lid, sem = T.consTFV tfv, class = CL.consTYCON (), lab = lab}
				 val cst1 = E.unionConstraintsList [cst, cst0]
				 val css1 = E.uenvcss [css, css0]
			     in ((longid, L.singleton lab, L.empty, CD.empty) :: reas, cst1, css1)
			     end
			   | (NONE, _, cst0, css0) => (reas, E.unionConstraintsList [cst, cst0], E.uenvcss [css, css0]))
		     ([], E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
		     ltreadescs
	     | f_ltreadesc (A.LTReaDescDots pl) =
	       let val env = f_partlist pl
	       in ([], E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.envvar, Env.cst, Env.css) *)
	   and f_labsigexp (A.LabSigExp (sigexp, _, _, lab, _)) =
	       let val (ev, cst, css) = f_sigexp sigexp
		   val ev' = E.freshEnvVar ()
		   val c   = E.initEnvConstraint (E.consENVVAR ev' lab) (E.consENVVAR ev lab) lab
	       in (ev', E.consConstraint(lab, c) cst, css)
	       end
	     | f_labsigexp (A.LabSigExpDots pl) =
	       let val ev  = E.freshEnvVar ()
		   val env = f_partlist pl
	       in (ev, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.envvar, Env.cst, Env.css) *)
	   and f_sigexp (A.SigExpBasic (spec, _, lab, _)) =
	       let val (env, css) = f_spec spec
		   val env = E.updateILab lab env
		   val ev1 = E.freshEnvVar ()
		   val ev2 = E.freshEnvVar ()
		   val c1  = E.initEnvConstraint (E.consENVVAR ev1 L.dummyLab) env L.dummyLab
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
		   val c2  = E.initEnvConstraint (E.consENVVAR ev2 lab) (E.consENVVAR ev1 lab) lab
	       in (ev2, E.consConstraint(L.dummyLab, c1) (E.singleConstraint (lab, c2)), css)
	       end
	     | f_sigexp (A.SigExpId (sigid, lab, _)) =
	       let val (ev, cst) = f_sigid sigid
		   val ev' = E.freshEnvVar ()
		   val c   = E.initEnvConstraint (E.consENVVAR ev' lab) (E.consENVVAR ev lab) lab
	       in (ev', E.consConstraint(lab, c) cst, E.emptyContextSensitiveSyntaxError)
	       end
	     (*| f_sigexp (A.SigExpRea (labsigexp, rea, _, lab, _)) =
	       let val (ev1, cst1, css1) = f_labsigexp labsigexp
		   (*(2010-07-07)We shouldn't do that for the where clauses as they are in fact
		    * sequences of where clauses and here we treat them as a bloc without any
		    * precedence, which is going to pose problem at constraint solving. *)
		   val (env2, cst2, css2) = f_ltreadesc rea
		   val ev  = E.freshEnvVar ()
		   val ev' = E.freshEnvVar ()
		   val c1  = E.initEnvConstraint (E.consENVVAR ev lab) (E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst2, env2)) lab
		   val c2  = E.SIGNATURE_CONSTRAINT (ev1, SOME ev', ev, NONE, lab, E.WHR)
		   val env = E.SEQUENCE_ENV (E.CONSTRAINT_ENV (E.consConstraint(lab, c1) cst1), E.CONSTRAINT_ENV (E.singleConstraint (lab, c2)))
	       in (ev', E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.uenvcss [css1, css2])
	       end*)
	     | f_sigexp (A.SigExpRea (labsigexp, rea, _, lab, _)) =
	       let val (ev1, cst1, css1) = f_labsigexp labsigexp
		   val (reas, cst2, css2) = f_ltreadesc rea
		   val ev   = E.freshEnvVar ()
		   val ev'  = E.freshEnvVar ()
		   val cst  = E.unionConstraintsList [cst1, cst2]
		   val css  = E.uenvcss [css1, css2]
		   val c1   = E.initEnvConstraint (E.consENVVAR ev' lab) (E.consENVVAR ev1 lab) lab
		   val env1 = foldl (fn (rea, env) => E.ENVWHR (env, rea)) (E.consENVVAR ev' lab) reas
		   val env2 = E.SEQUENCE_ENV (E.CONSTRAINT_ENV (E.consConstraint(lab, c1) cst), env1)
		   val c2   = E.initEnvConstraint (E.consENVVAR ev lab) env2 lab
	       in (ev, E.singleConstraint (lab, c2), css)
	       end
	     | f_sigexp (A.SigExpDots pl) =
	       let val env = f_partlist pl
		   val ev  = E.freshEnvVar ()
	       in (ev, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.sigenv, Env.cst, Env.css) *)
	   and f_sigbindone (A.SigBindOne (sigid, labsigexp, _, lab, _)) =
	       let val (ev1, sigs, cst1) = f_sigidbind sigid
		   val (ev2, cst2, css2) = f_labsigexp labsigexp
		   val c = E.initEnvConstraint (E.consENVVAR ev1 lab) (E.consENVVAR ev2 lab) lab
	       in (sigs, E.consConstraint(lab, c) (E.unionConstraintsList [cst1, cst2]), css2)
	       end
	     | f_sigbindone (A.SigBindOneDots pl) =
	       let val env = f_partlist pl
	       in (E.getSigs env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.sigenv, Env.cst, Env.css) *)
	   and f_sigbind (A.SigBind (sigbinds, _, _)) =
	       let val (sigss, csts, csss) = unzipThree (map f_sigbindone sigbinds)
		   val sigs = E.unionEnvironmentList sigss
		   val cst  = E.unionConstraintsList csts
		   val css  = E.uenvcss csss
	       in (sigs, cst, css)
	       end
	     | f_sigbind (A.SigBindDots pl) =
	       let val env = f_partlist pl
	       in (E.getSigs env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.funenv, Env.cst, Env.css) *)
	   and f_funbindone (A.FunBindO (funid, strid, labsigexp, labstrexp, _, lab, _)) =
	       let val (ev0, ev1, funs, cst1) = f_funidbind funid
		   val (ev2, strs, cst2, css2) = f_strid strid
		   val (ev3, cst3, css3) = f_labsigexp labsigexp
		   val (ev4, cst4, css4) = f_labstrexp labstrexp
		   val c1   = E.initEnvConstraint (E.consENVVAR ev2 lab) (E.consENVVAR ev3 lab) lab (* strid has signature labsigexp            *)
		   val c2   = E.initEnvConstraint (E.consENVVAR ev0 lab) (E.consENVVAR ev2 lab) lab (* the functor takes the strid as parameter *)
		   val c3   = E.initEnvConstraint (E.consENVVAR ev1 lab) (E.consENVVAR ev4 lab) lab (* the functor returns the labstrexp        *)
		   val env0 = E.updateIFct true (E.projStrs strs) (* We mark the environment as being the parameter of a functor *)
		   val env1 = E.CONSTRAINT_ENV (E.consConstraint(lab, c1) (E.unionConstraintsList [cst2, cst3]))
		   val env2 = E.SEQUENCE_ENV (E.SEQUENCE_ENV (env1, env0), E.CONSTRAINT_ENV cst4)
		   val cst  = E.consConstraint(L.dummyLab, E.LET_CONSTRAINT env2) (E.conscsts (lab, [c2, c3]) cst1)
		   val css  = E.uenvcss [css2, css3, css4]
	       in (funs, cst, css)
	       end
	     | f_funbindone (A.FunBindOO (funid, strid, labsigexp1, labsigexp2, labstrexp, _, lab, _)) =
	       let val (ev0, ev1, funs, cst1) = f_funidbind funid
		   val (ev2, strs, cst2, css2) = f_strid strid
		   val (ev3, cst3, css3) = f_labsigexp labsigexp1
		   val (ev4, cst4, css4) = f_labsigexp labsigexp2
		   val (ev5, cst5, css5) = f_labstrexp labstrexp
		   val c1   = E.initEnvConstraint (E.consENVVAR ev2 lab) (E.consENVVAR ev3 lab) lab (* strid has signature labsigexp            *)
		   val c2   = E.initEnvConstraint (E.consENVVAR ev0 lab) (E.consENVVAR ev2 lab) lab (* the functor takes the strid as parameter *)
		   val c3   = E.SIGNATURE_CONSTRAINT (ev4, NONE, ev5, SOME ev1, lab)                       (* the functor returns the labstrexp        *)
		   val env0 = E.updateIFct true (E.projStrs strs) (* We mark the environment as being the parameter of a functor *)
		   val env1 = E.CONSTRAINT_ENV (E.consConstraint(lab, c1) (E.unionConstraintsList [cst2, cst3]))
		   val env2 = E.CONSTRAINT_ENV (E.unionConstraintsList [cst4, cst5])
		   val env3 = E.CONSTRAINT_ENV (E.conscsts (lab, [c2, c3]) cst1)
		   val env4 = E.SEQUENCE_ENV (E.SEQUENCE_ENV (env1, env0), E.SEQUENCE_ENV (env2, env3))
		   val cst  = E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env4)
		   val css  = E.uenvcss [css2, css3, css4, css5]
	       in (funs, cst, css)
	       end
	     | f_funbindone (A.FunBindOT (funid, strid, labsigexp1, labsigexp2, labstrexp, _, lab, _)) =
	       let val (ev0, ev1, funs, cst1) = f_funidbind funid
		   val (ev2, strs, cst2, css2) = f_strid strid
		   val (ev3, cst3, css3) = f_labsigexp labsigexp1
		   val (ev4, cst4, css4) = f_labsigexp labsigexp2
		   val (ev5, cst5, css5) = f_labstrexp labstrexp
		   val c1   = E.initEnvConstraint (E.consENVVAR ev2 lab) (E.consENVVAR ev3 lab) lab (* strid has signature labsigexp            *)
		   val c2   = E.initEnvConstraint (E.consENVVAR ev0 lab) (E.consENVVAR ev2 lab) lab (* the functor takes the strid as parameter *)
		   val c3   = E.SIGNATURE_CONSTRAINT (ev4, SOME ev1, ev5, NONE, lab)                       (* the functor returns the labstrexp        *)
		   val env0 = E.updateIFct true (E.projStrs strs) (* We mark the environment as being the parameter of a functor *)
		   val env1 = E.CONSTRAINT_ENV (E.consConstraint(lab, c1) (E.unionConstraintsList [cst2, cst3]))
		   val env2 = E.CONSTRAINT_ENV (E.unionConstraintsList [cst4, cst5])
		   val env3 = E.CONSTRAINT_ENV (E.conscsts (lab, [c2, c3]) cst1)
		   val env4 = E.SEQUENCE_ENV (E.SEQUENCE_ENV (env1, env0), E.SEQUENCE_ENV (env2, env3))
		   val cst  = E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env4)
		   val css  = E.uenvcss [css2, css3, css4, css5]
	       in (funs, cst, css)
	       end
	     | f_funbindone (A.FunBindOS (funid, spec, labstrexp, _, lab, _)) =
	       let val (ev0, ev1, funs, cst1) = f_funidbind funid
		   val (env, css2) = f_spec spec
		   val (ev3, cst3, css3) = f_labstrexp labstrexp
		   val ev2  = E.freshEnvVar ()
		   val env' = E.updateIFct true env (* We mark the environment as being the parameter of a functor *)
		   val c1   = E.initEnvConstraint (E.consENVVAR ev2 lab) env' lab                   (* bind the specs to ev2                   *)
		   val c2   = E.initEnvConstraint (E.consENVVAR ev0 lab) (E.consENVVAR ev2 lab) lab (* the functor takes the spec as parameter *)
		   val c3   = E.initEnvConstraint (E.consENVVAR ev1 lab) (E.consENVVAR ev3 lab) lab (* the functor returns the labstrexp       *)
		   val env1 = E.SEQUENCE_ENV (E.CONSTRAINT_ENV (E.singleConstraint (lab, c1)), E.consENVVAR ev2 lab)
		   val env2 = E.SEQUENCE_ENV (env1, E.CONSTRAINT_ENV cst3)
		   val cst  = E.consConstraint(L.dummyLab, E.LET_CONSTRAINT env2) (E.conscsts (lab, [c2, c3]) cst1)
		   val css  = E.uenvcss [css2, css3]
	       in (funs, cst, css)
	       end
	     | f_funbindone (A.FunBindOSO (funid, spec, labsigexp, labstrexp, _, lab, _)) =
	       let val (ev0, ev1, funs, cst1) = f_funidbind funid
		   val (env, css2) = f_spec spec
		   val (ev3, cst3, css3) = f_labsigexp labsigexp
		   val (ev4, cst4, css4) = f_labstrexp labstrexp
		   val ev2  = E.freshEnvVar ()
		   val env' = E.updateIFct true env (* We mark the environment as being the parameter of a functor *)
		   val c1   = E.initEnvConstraint (E.consENVVAR ev2 lab) env' lab                   (* bind the specs to ev2                   *)
		   val c2   = E.initEnvConstraint (E.consENVVAR ev0 lab) (E.consENVVAR ev2 lab) lab (* the functor takes the spec as parameter *)
		   val c3   = E.SIGNATURE_CONSTRAINT (ev3, NONE, ev4, SOME ev1, lab)                       (* the functor returns the labstrexp       *)
		   val env1 = E.SEQUENCE_ENV (E.CONSTRAINT_ENV (E.singleConstraint (lab, c1)), E.consENVVAR ev2 lab)
		   val env2 = E.CONSTRAINT_ENV (E.unionConstraintsList [cst3, cst4])
		   val env3 = E.CONSTRAINT_ENV (E.conscsts (lab, [c2, c3]) cst1)
		   val env4 = E.SEQUENCE_ENV (env2, env3)
		   val cst  = E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT (E.SEQUENCE_ENV (env1, env4)))
		   val css  = E.uenvcss [css2, css3, css4]
	       in (funs, cst, css)
	       end
	     | f_funbindone (A.FunBindOST (funid, spec, labsigexp, labstrexp, _, lab, _)) =
	       let val (ev0, ev1, funs, cst1) = f_funidbind funid
		   val (env, css2) = f_spec spec
		   val (ev3, cst3, css3) = f_labsigexp labsigexp
		   val (ev4, cst4, css4) = f_labstrexp labstrexp
		   val ev2  = E.freshEnvVar ()
		   val env' = E.updateIFct true env (* We mark the environment as being the parameter of a functor *)
		   val c1   = E.initEnvConstraint (E.consENVVAR ev2 lab) env' lab                   (* bind the specs to ev2                   *)
		   val c2   = E.initEnvConstraint (E.consENVVAR ev0 lab) (E.consENVVAR ev2 lab) lab (* the functor takes the spec as parameter *)
		   val c3   = E.SIGNATURE_CONSTRAINT (ev3, SOME ev1, ev4, NONE, lab)                       (* the functor returns the labstrexp       *)
		   val env1 = E.SEQUENCE_ENV (E.CONSTRAINT_ENV (E.singleConstraint (lab, c1)), E.consENVVAR ev2 lab)
		   val env2 = E.CONSTRAINT_ENV (E.unionConstraintsList [cst3, cst4])
		   val env3 = E.CONSTRAINT_ENV (E.conscsts (lab, [c2, c3]) cst1)
		   val env4 = E.SEQUENCE_ENV (env2, env3)
		   val cst  = E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT (E.SEQUENCE_ENV (env1, env4)))
		   val css  = E.uenvcss [css2, css3, css4]
		   (*val _    = D.printdebug2 ("[functor]")*)
	       in (funs, cst, css)
	       end
	     | f_funbindone (A.FunBindODots parts) =
	       let val env = f_partlist parts
	       in (E.getFuns env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.funenv, Env.cst, Env.css) *)
	   and f_funbind (A.FunBind (funbinds, _, _)) =
	       let val (funss, csts, csss) = unzipThree (map f_funbindone funbinds)
		   val funs = E.unionEnvironmentList funss
		   val cst  = E.unionConstraintsList csts
		   val css  = E.uenvcss csss
	       in (E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst, E.projFuns funs), css)
	       end
	     | f_funbind (A.FunBindDots parts) =
	       let val env = f_partlist parts
	       in (env, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.env, Env.cst, Env.css) *)
	   and f_sigdec (A.SigDec (sigbind, _, _)) =
	       let val (sigs, cst, css) = f_sigbind sigbind
		   val env = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst, E.projSigs sigs)
	       in (env, css)
	       end
	     | f_sigdec (A.SigDecDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.css) *)
	   and f_afile (A.AFile (file, _, lab, _)) =
	       let val css = if OS.FileSys.access (file, [OS.FileSys.A_READ])
				andalso
				not (OS.FileSys.isDir file)
				andalso
				not (OS.FileSys.isLink file)
			     then E.emptyContextSensitiveSyntaxError
			     else E.singcss (accessCs lab)
	       in css
	       end
	     | f_afile A.AFileDots = E.emptyContextSensitiveSyntaxError

	   (* RETURNS: (E.env, Env.css) *)
	   and f_smltes (A.SmlTesDec  (atopdec, _, _)) = f_atopdec atopdec
	     | f_smltes (A.SmlTesSpec (spec, _, _))    = f_specsmltes spec
	     (*(2010-06-17)The SMLTES spec is different because we need to directly close value specs
	      * and not wait to match them against structures. *)
	     | f_smltes (A.SmlTesUse  (af, _, _)) = (E.emptyEnvironment, f_afile af)
	     | f_smltes (A.SmlTesSBas (af, _, _)) = (E.emptyEnvironment, f_afile af)
	     | f_smltes (A.SmlTesCBas _)  = (E.emptyEnvironment, E.emptyContextSensitiveSyntaxError)
	     | f_smltes (A.SmlTesQuote _) = (E.emptyEnvironment, E.emptyContextSensitiveSyntaxError)
	     | f_smltes (A.SmlTesType (id, _, _)) = (E.ENVPTY id, E.emptyContextSensitiveSyntaxError)
	     | f_smltes (A.SmlTesDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (E.env, Env.css) *)
	   and f_atopdec (A.ATopDecStr strdec) = f_strdec strdec
	     | f_atopdec (A.ATopDecSig sigdec) = f_sigdec sigdec
	     | f_atopdec (A.ATopDecDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (E.env, Env.css) *)
	   and f_topdecone (A.TopDecOneTes (x, _)) = f_smltes x
	     | f_topdecone (A.TopDecOneDec (x, _)) = f_atopdec x
	     | f_topdecone (A.TopDecOneDots pl)    =
	       let val env = f_partlist pl
	       in (env, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (E.env, Env.css) *)
	   and f_topdeconelist [] = (E.emptyEnvironment, E.emptyContextSensitiveSyntaxError)
	     | f_topdeconelist [x] = f_topdecone x
	     | f_topdeconelist (x :: xs) =
	       let val (env1, css1) = f_topdecone x
		   val (env2, css2) = f_topdeconelist xs
	       in (E.SEQUENCE_ENV (env1, env2), E.uenvcss [css1, css2])
	       end

	   (* RETURNS: (E.env, Env.css) *)
	   and f_topdec (A.TopDec xs) = f_topdeconelist xs
	     | f_topdec (A.TopDecDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (E.env, Env.css) *)
	   and f_progone (A.ProgOneDec td) = f_topdec td
	     | f_progone (A.ProgOneExp (exp, v, _, lab, _)) =
	       let val (tv, cst1, css) = f_exp exp
		   val (tyvs, cst2) = f_typevarvallist (A.gettyvarExp exp)
		   val tv'  = T.freshtyvar ()
		   val c    = E.initTypeConstraint (T.consV tv') (T.consV tv) lab
		   val cst  = E.consConstraint(lab, c) cst1
		   val vids = E.consSingleEnv (v, [E.consBindPoly v (T.consV tv') (CL.consVAL ()) lab])
		   val env1 = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst, E.projVids (E.closeVids vids (V.nonexpExp exp)))
		   val env2 = E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst2, E.ENVPOL (tyvs, env1))
	       (*val (idV, cs1) = closeTypedTerm (A.gettyvarExp exp) [] (E.getTyvs env2) cs0 idG'*)
	       in (env2, css)
	       end
	     | f_progone (A.ProgOneParse (s, _, lab, _)) =
	       (* Here we generate an environmnet variable because the file is not parsable
		* and it might generate binding errors. *)
	       (E.newEnvVar lab, E.singcss (E.CSSPARS (L.singleton lab, s)))
	     | f_progone (A.ProgOneFile (af, _)) = (E.emptyEnvironment, f_afile af)
	     | f_progone (A.ProgOneDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (E.env, Env.css) *)
	   and f_progonelist []  =  (E.emptyEnvironment, E.emptyContextSensitiveSyntaxError)
	     | f_progonelist [x] = f_progone x
	     | f_progonelist (x :: xs) =
	       let val (env1, css1) = f_progone x
		   val (env2, css2) = f_progonelist xs
	       in (E.SEQUENCE_ENV (env1, env2), E.uenvcss [css1, css2])
	       end

	   (* RETURNS: (E.env, Env.css) *)
	   and f_prog (A.Prog tdl) = f_progonelist tdl
	     | f_prog (A.ProgDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (E.env) *)
	   and f_progfile file env1 env2 =
	       E.ENVFIL (file, env1, fn () => env2)

	   (* RETURNS: (E.env, Env.css) *)
	   and f_proglist [] = (E.emptyEnvironment, E.emptyContextSensitiveSyntaxError)
	     | f_proglist [(x, file, false, _)] =
	       let val (env, css) = f_prog x
	       in (f_progfile file env E.emptyEnvironment, css)
	       end
	     | f_proglist [(x, file, true, _)] =
	       if benv
	       then let val _ = setBasis true
			val (env, css) = f_prog x
			val _ = setBasis false
		    in (f_progfile file env E.emptyEnvironment, css)
		    end
	       else (E.emptyEnvironment, E.emptyContextSensitiveSyntaxError)
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
			val c    = E.initTypeConstraint ty1 ty2 L.dummyLab
			val bind = E.consBindPoly id ty1 class0 L.dummyLab
		    in (bind, c)
		    end
		  | bindOne (id, "false") =
		    let val ty1  = T.newV ()
			val ty2  = T.constybool' L.dummyLab T.BB
			val c    = E.initTypeConstraint ty1 ty2 L.dummyLab
			val bind = E.consBindPoly id ty1 class0 L.dummyLab
		    in (bind, c)
		    end
		  | bindOne (id, "nil") =
		    let val ty1  = T.newV ()
			val ty2  = T.constylist' (T.freshtyvar ()) L.dummyLab T.BB
			val c    = E.initTypeConstraint ty1 ty2 L.dummyLab
			val bind = E.consBindPoly id ty1 class0 L.dummyLab
		    in (bind, c)
		    end
		  | bindOne (id, "ref") =
		    let val tv   = T.freshtyvar ()
			val ty1  = T.newV ()
			val ty2  = T.consTyArrowTy (T.consV tv) (T.constyref' tv L.dummyLab T.BB) L.dummyLab T.BB
			val c    = E.initTypeConstraint ty1 ty2 L.dummyLab
			val bind = E.consBindPoly id ty1 class1 L.dummyLab
		    in (bind, c)
		    end
		  | bindOne _ = raise EH.DeadBranch "this constructor can be rebound"
		val (binds, cs) = ListPair.unzip (map bindOne inascid)
		val env' = if List.null binds
			   then env
			   else let val env1 = E.projVids (E.bindToEnv binds)
				    val cst  = E.singcsts (L.dummyLab, cs)
				in E.SEQUENCE_ENV (E.SEQUENCE_ENV (E.CONSTRAINT_ENV cst, E.ENVPOL (E.emtv, env1)), env)
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
