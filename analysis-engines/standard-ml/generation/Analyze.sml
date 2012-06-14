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

(* The first argument should be the output of analyze.  Program
 * identifiers are represented by integers in the output of
 * analyze, so the second argument is a mapping from the original
 * program identifiers to these integers.  This function has
 * builtin knowledge of types for all identifiers in the SML
 * initial basis as well as a number from the SML Basis Library.
 * The output is the input with additional constraints added to
 * enforce these types. *)
fun buildin (env, css) ascid true =
    let val (typeNameEnv, cst1) = NA.getTypename ascid
	val (vids, cst2) = OP.getOpType ascid
	val cst  = E.unionConstraintsList [cst1, cst2]
	val env' = E.ROW_ENV (E.CONSTRAINT_ENV cst, E.ENVPOL (E.emtv, E.updateTypeNames typeNameEnv (E.projValueIds vids)))
    in if E.isEmptyIdEnv vids andalso E.isEmptyIdEnv typeNameEnv
       then (env, css)
       else (E.ROW_ENV (env', env), css)
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
		    in E.unionContextSensitiveSyntaxErrors [css, css']
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
    in E.unionContextSensitiveSyntaxErrors [css1, css2]
    end

fun checkTypeVarInc typeVarsbind labs typeVars =
    let val (labs, ids) =
	    foldl (fn (A.TypeVar (_, id, _, lab, _), (labs, ids)) =>
		      (L.cons lab labs, I.add id ids)
		    | (A.EqualityTypeVar (_, id, _, lab, _), (labs, ids)) =>
		      (L.cons lab labs, I.add id ids)
		    | (A.TypeVarDots, x) => x)
		  (labs, I.empty)
		  typeVarsbind
	fun f [] = E.emptyContextSensitiveSyntaxError
	  | f ((A.TypeVar (_, id, _, lab, _)) :: xs) =
	    if I.isin id ids
	    then f xs
	    else ((E.CSSINCL (L.cons lab labs))::(f xs))
	  | f ((A.EqualityTypeVar (_, id, _, lab, _)) :: xs) =
	    if I.isin id ids
	    then f xs
	    else ((E.CSSINCL (L.cons lab labs))::(f xs))
	  | f (A.TypeVarDots :: xs) = f xs
    in f typeVars
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
	      E.unionContextSensitiveSyntaxErrors [createDiffNbArgFun (A.getLabsFValLab fvalbindone), css])
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

	val typenamesTop : string list ref = ref T.typenames

	fun getTypenameString s = if Tools.isin s (!typenamesTop)
				then (typenamesTop := #2 (Tools.remove s (!typenamesTop));
				      T.getTypenameString s)
				else T.freshTypename ()

	fun f_partlist _ = raise EH.TODO "no description, raised in the 'f_partlist' function of Analyze.sml"


	   and f_scon (A.SconInt (s, v, _, lab, _)) =
	       if benv (* We bind the integer to the "Int" overloading class *)
	       then let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.SconInt. v = "^(Int.toString (I.toInt v))^", lab = "^(Int.toString(L.toInt(lab))^")"))
			val sv = T.freshRowVar ()
			val tv = T.freshTypeVar  ()
			val a  = E.genAccIoEm (E.consAccId (I.ID (v, lab)) (T.ROW_VAR sv) (CL.consOC ()) lab) lab
			val c  = E.initTypeConstraint (T.consTYPE_VAR tv) (T.TYPE_POLY (T.ROW_VAR sv, T.freshidor (), T.POLY, T.CONSTANT (s, v, lab), lab, T.EQUALITY_TYPE)) lab
			val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION "Equality type constraint (EQUALITY_TYPE) generated for a SconInt"
		    in (tv, E.singcsts (lab, [E.ACCESSOR_CONSTRAINT a, c]), E.emptyContextSensitiveSyntaxError)
		    end
	       else let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.SconInt. benv = "^Bool.toString(benv)^", lab = "^(Int.toString(L.toInt(lab))^")"))
			val tv = T.freshTypeVar ()
			val c  = E.initTypeConstraint (T.consTYPE_VAR tv) (T.constyint lab) lab
		    in (tv, E.singleConstraint (lab, c), E.emptyContextSensitiveSyntaxError)
		    end
	     | f_scon (A.SconWord (s, v, _, lab, _)) =
	       if benv (* We bind the word to the "Word" overloading class *)
	       then let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.SconWord. benv = "^Bool.toString(benv)^", lab = "^(Int.toString(L.toInt(lab))^")"))
			val sv = T.freshRowVar ()
			val tv = T.freshTypeVar  ()
			val a  = E.genAccIoEm (E.consAccId (I.ID (v, lab)) (T.ROW_VAR sv) (CL.consOC ()) lab) lab
			val c  = E.initTypeConstraint (T.consTYPE_VAR tv) (T.TYPE_POLY (T.ROW_VAR sv, T.freshidor (), T.POLY, T.CONSTANT (s, v, lab), lab, T.EQUALITY_TYPE)) lab
		    in (tv, E.singcsts (lab, [E.ACCESSOR_CONSTRAINT a, c]), E.emptyContextSensitiveSyntaxError)
		    end
	       else let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.SconWord. benv = "^Bool.toString(benv)^", lab = "^(Int.toString(L.toInt(lab))^")"))
			val tv = T.freshTypeVar ()
			val c  = E.initTypeConstraint (T.consTYPE_VAR tv) (T.constyword lab) lab
		    in (tv, E.singleConstraint (lab, c), E.emptyContextSensitiveSyntaxError)
		    end
	     | f_scon (A.SconReal (s, v, _, lab, _)) =
	       if benv (* We bind the real to the "Real" overloading class *)
	       then let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.SconReal (benv = "^Bool.toString(benv)^", s = "^s^", v = "^I.printId(v)^", lab = "^(Int.toString(L.toInt(lab))^")"))
			val sv = T.freshRowVar ()
			val tv = T.freshTypeVar  ()
			(* should the accessor contain some kind of equality constraint?
			 * or should we just look up the equality constraint in the type constraint somehow? *)
			val a  = E.genAccIoEm (E.consAccId (I.ID (v, lab)) (T.ROW_VAR sv) (CL.consOC ()) lab) lab
			val c  = E.initTypeConstraint (T.consTYPE_VAR tv) (T.TYPE_POLY (T.ROW_VAR sv, T.freshidor (), T.POLY, T.CONSTANT (s, v, lab), lab, T.NOT_EQUALITY_TYPE)) lab
			val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION "Equality type constraint (NOT_EQUALITY_TYPE) generated for a SconReal"
		    in (tv, E.singcsts (lab, [E.ACCESSOR_CONSTRAINT a, c]), E.emptyContextSensitiveSyntaxError)
		    end
	       else let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.SconReal (benv = "^Bool.toString(benv)^", s = "^s^", v = "^I.printId(v)^", lab = "^(Int.toString(L.toInt(lab))^")"))
			val tv = T.freshTypeVar ()
			val c  = E.initTypeConstraint (T.consTYPE_VAR tv) (T.constyreal lab) lab
			val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("A.SconReal constraints generated (tv = "^Int.toString(T.typeVarToInt tv)^")")
		    in (tv, E.singleConstraint (lab, c), E.emptyContextSensitiveSyntaxError)
		    end
	     | f_scon (A.SconString (s, v, _, lab, _)) =
	       if benv (* We bind the string to the "String" overloading class *)
	       then let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.SconString. benv = "^Bool.toString(benv)^", lab = "^(Int.toString(L.toInt(lab))^")"))
			val sv = T.freshRowVar ()
			val tv = T.freshTypeVar  ()
			val a  = E.genAccIoEm (E.consAccId (I.ID (v, lab)) (T.ROW_VAR sv) (CL.consOC ()) lab) lab
			val c  = E.initTypeConstraint (T.consTYPE_VAR tv) (T.TYPE_POLY (T.ROW_VAR sv, T.freshidor (), T.POLY, T.CONSTANT (s, v, lab), lab, T.EQUALITY_TYPE)) lab
		    in (tv, E.singcsts (lab, [E.ACCESSOR_CONSTRAINT a, c]), E.emptyContextSensitiveSyntaxError)
		    end
	       else let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.SconString. benv = "^Bool.toString(benv)^", lab = "^(Int.toString(L.toInt(lab))^")"))
			val tv = T.freshTypeVar ()
			val c  = E.initTypeConstraint (T.consTYPE_VAR tv) (T.constystring lab) lab
		    in (tv, E.singleConstraint (lab, c), E.emptyContextSensitiveSyntaxError)
		    end
	     | f_scon (A.SconChar (s, v, _, lab, _)) =
	       if benv (* We bind the char to the "Char" overloading class *)
	       then let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.SconChar. benv = "^Bool.toString(benv)^", lab = "^(Int.toString(L.toInt(lab))^")"))
			val sv = T.freshRowVar ()
			val tv = T.freshTypeVar  ()
			val a  = E.genAccIoEm (E.consAccId (I.ID (v, lab)) (T.ROW_VAR sv) (CL.consOC ()) lab) lab
			val c  = E.initTypeConstraint (T.consTYPE_VAR tv) (T.TYPE_POLY (T.ROW_VAR sv, T.freshidor (), T.POLY, T.CONSTANT (s, v, lab), lab, T.EQUALITY_TYPE)) lab
		    in (tv, E.singcsts (lab, [E.ACCESSOR_CONSTRAINT a, c]), E.emptyContextSensitiveSyntaxError)
		    end
	       else let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.SconChar. benv = "^Bool.toString(benv)^", lab = "^(Int.toString(L.toInt(lab))^")"))
			val tv = T.freshTypeVar ()
			val c  = E.initTypeConstraint (T.consTYPE_VAR tv) (T.constychar lab) lab
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
	     | f_scon A.SconDots = (D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.SconDots"); (T.freshTypeVar (), E.emptyConstraint, E.emptyContextSensitiveSyntaxError))

	   (* RETURNS: (Ty.typeVar, Env.varenv, E.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
	   and f_labid (A.LabId (ident, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.LabId")
		   val (tv, vids, cst, contextSensitiveSyntaxError) = f_identpat ident
		   val tv' = T.freshTypeVar ()
		   val c   = E.initTypeConstraint (T.consTYPE_VAR tv') (T.consTYPE_VAR tv) lab
	       in (tv', vids, E.consConstraint (lab, c) cst, contextSensitiveSyntaxError)
	       end
	     | f_labid (A.LabIdDots pl) =
	       let val env = f_partlist pl
	       in (T.freshTypeVar (), E.getValueIds env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.envvar, Env.strenv, E.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
	   and f_strid (A.StrId (s, v, _, lab, _)) =
	       (D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.StrId");
	       if String.isPrefix "_" s
	       then (E.freshEnvVar (), E.emstr, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       else let val ev1  = E.freshEnvVar ()
			val ev2  = E.freshEnvVar ()
			val strs = E.consSingleEnv (v, [E.consBindPoly {id=v,
									typeOfId=(E.consENV_VAR ev2 lab),
									classOfId=(CL.consSTR ()),
									labelOfConstraint=lab}])
			val c    = E.initEnvConstraint (E.ENV_VAR (ev1, lab)) (E.ENV_VAR (ev2, lab)) lab
		    in (ev1, strs, E.singleConstraint (lab, c), E.emptyContextSensitiveSyntaxError)
		    end)
	     | f_strid A.StrIdDots =
	       (E.freshEnvVar (), E.emstr, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)

	   (* RETURNS: (Env.envvar, Env.envvar, Env.cst) *)
	   and f_funid (A.FunId (s, v, _, lab, _)) =
	       (D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.FunId (f_funid function)");
	       if String.isPrefix "_" s
	       then (E.freshEnvVar (), E.freshEnvVar (), E.emptyConstraint)
	       else let val lid  = I.idToLid v lab
			val ev1  = E.freshEnvVar ()
			val ev2  = E.freshEnvVar ()
			val env1 = E.consENV_VAR ev1 lab
			val env2 = E.consENV_VAR ev2 lab
			val a    = E.genAccIfEm (E.consAccId lid (env1, env2) (CL.consFUNC ()) lab) lab
		    in (ev1, ev2, E.singleConstraint (lab, E.ACCESSOR_CONSTRAINT a))
		    end)
	     | f_funid A.FunIdDots = (E.freshEnvVar (), E.freshEnvVar (), E.emptyConstraint)

	   (* RETURNS: (Env.envvar, Env.envvar, Env.funenv, E.emptyConstraint) *)
	   and f_funidbind (A.FunId (s, v, _, lab, _)) =
	       (D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.FunId (f_funidbind function)");
	       if String.isPrefix "_" s
	       then (E.freshEnvVar (), E.freshEnvVar (), E.emfun, E.emptyConstraint)
	       else let val ev1  = E.freshEnvVar ()
			val ev2  = E.freshEnvVar ()
			val ev1' = E.freshEnvVar ()
			val ev2' = E.freshEnvVar ()
			val env1 = E.consENV_VAR ev1' lab
			val env2 = E.consENV_VAR ev2' lab
			val funs = E.consSingleEnv (v, [E.consBindPoly {id=v,
									typeOfId=(env1, env2),
									classOfId=(CL.consFUNC ()),
										   labelOfConstraint=lab}])
			val c1   = E.initEnvConstraint (E.ENV_VAR (ev1, lab)) (E.ENV_VAR (ev1', lab)) lab
			val c2   = E.initEnvConstraint (E.ENV_VAR (ev2, lab)) (E.ENV_VAR (ev2', lab)) lab
		    in (ev1, ev2, funs, E.singcsts (lab, [c1, c2]))
		    end)
	     | f_funidbind A.FunIdDots =
	       (E.freshEnvVar (), E.freshEnvVar (), E.emfun, E.emptyConstraint)

	   (* RETURNS: (Env.envvar, Env.cst) *)
	   and f_sigid (A.SigId (s, v, _, lab, _)) =
	       (D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.SigId (f_sigid function)");
	       if String.isPrefix "_" s
	       then (E.freshEnvVar (), E.emptyConstraint)
	       else let val lid = I.idToLid v lab
			val ev  = E.freshEnvVar ()
			val a   = E.genAccIiEm (E.consAccId lid (E.consENV_VAR ev lab) (CL.consSIG ()) lab) lab
		    in (ev, E.singleConstraint (lab, E.ACCESSOR_CONSTRAINT a))
		    end)
	     | f_sigid A.SigIdDots = (E.freshEnvVar (), E.emptyConstraint)

	   (* RETURNS: (Env.envvar, Env.sigenv, Env.cst) *)
	   and f_sigidbind (A.SigId (s, v, _, lab, _)) =
	       (D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.SigId (f_sigidbind function)");
	       if String.isPrefix "_" s
	       then (E.freshEnvVar (), E.emsig, E.emptyConstraint)
	       else let val ev1  = E.freshEnvVar ()
			val ev2  = E.freshEnvVar ()
			val sigs = E.consSingleEnv (v, [E.consBindPoly {id=v,
									typeOfId=(E.consENV_VAR ev2 lab),
									classOfId=(CL.consSIG ()),
									labelOfConstraint=lab}])
			val c    = E.initEnvConstraint (E.consENV_VAR ev1 lab) (E.consENV_VAR ev2 lab) lab
		    in (ev1, sigs, E.singleConstraint (lab, c))
		    end)
	     | f_sigidbind A.SigIdDots = (E.freshEnvVar (), E.emsig, E.emptyConstraint)

	   (* RETURNS: (Ty.typeVar, Env.cst) *)
	   and f_longidexp longid =
	       (D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints inside f_longidexp function (longid="^(A.printAstLongId longid)^")");
	       case A.longidToLid longid of
		   NONE => (D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION "in f_longidexp - result of `A.longidToLid longid` was NONE";
			    (T.freshTypeVar (), L.dummyLab, CL.newClassVar (), E.emptyConstraint))
		 | SOME lid =>
		   let
		       val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("in f_longidexp - result of `A.longidToLid longid` was SOME lid - "^(I.printLid lid));
		       val lab = I.getLabId lid
		       (* NOTE: We want all the labels from lid *)
		       val tv  = T.freshTypeVar ()
		       val class  = CL.newClassVar ()
		       val accessor   = E.initValueIDAccessor (E.consAccId lid (T.consTYPE_VAR tv) class lab) lab
		       val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("               - lab = "^Int.toString(L.toInt lab)^", tv = "^Int.toString(T.typeVarToInt tv));
		   in (tv, lab, class, E.singleConstraint (lab, E.ACCESSOR_CONSTRAINT accessor))
		   end)

	   (* RETURNS: (Ty.typeVar, Env.varenv, E.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
	   and f_pconpat (A.PconBool (s, v, reg, lab, nxt)) =
	       (D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.PconBool");
	       if getBasis ()
	       then f_identpat (A.Ident (s, v, reg, lab, nxt))
	       else let val tv = T.freshTypeVar ()
			val c  = E.initTypeConstraint (T.consTYPE_VAR tv) (T.constybool lab) lab
		    in (tv, E.emvar, E.singleConstraint (lab, c), E.singcss (reboundCs lab s))
		    end)
	     | f_pconpat (A.PconRef (s, v, reg, lab, nxt)) =
	       (D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.PconRef");
	       if getBasis ()
	       then f_identpat (A.Ident (s, v, reg, lab, nxt))
	       else let val tv  = T.freshTypeVar ()
			val tv' = T.freshTypeVar ()
			val c   = E.initTypeConstraint (T.consTYPE_VAR tv) (T.consTyArrowTy (T.consTYPE_VAR tv') (T.constyref tv' lab) lab T.OTHER_CONS) lab
		    in (tv, E.emvar, E.singleConstraint (lab, c), E.singcss (reboundCs lab s))
		    end)
	     | f_pconpat (A.PconNil (s, v, reg, lab, nxt)) =
	       (D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.PconNil");
	       if getBasis ()
	       then f_identpat (A.Ident (s, v, reg, lab, nxt))
	       else let val tv  = T.freshTypeVar ()
			val tv' = T.freshTypeVar ()
			val c   = E.initTypeConstraint (T.consTYPE_VAR tv) (T.constylist tv' lab) lab
		    in (tv, E.emvar, E.singleConstraint (lab, c), E.singcss (reboundCs lab s)) end)
	     | f_pconpat A.PconDots = (T.freshTypeVar (), E.emvar, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)

	   (* RETURNS: (Ty.typeVar, Env.varenv, E.emptyConstraint, Env.emptyContextSensitiveSyntaxError) *)
	   and f_identpat (A.Ident (str, id, _, lab, _)) =
	       (D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.Ident (str=\""^str^"\")");
	       if String.isPrefix "_" str
	       then (T.freshTypeVar (), E.emvar, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       else let val tv1  = T.freshTypeVar ()
			val tv2  = T.freshTypeVar ()
			val vids = E.consSingleEnv (id, [E.consBindPoly {id=id,
									 typeOfId=(T.consTYPE_VAR tv2),
									 classOfId=(CL.consVAL ()),
									 labelOfConstraint=lab}])
			val c    = E.initTypeConstraint (T.consTYPE_VAR tv1) (T.consTYPE_VAR tv2) lab
		    in (tv1, vids, E.singleConstraint (lab, c), E.emptyContextSensitiveSyntaxError)
		    end)
	     | f_identpat (A.IdentPcon pc) = f_pconpat pc
	     | f_identpat A.IdentDots = (T.freshTypeVar (), E.emvar, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)

	   (* RETURNS: (Ty.typeVar, Env.env, Env.emptyContextSensitiveSyntaxError) *)
	   and f_longidpat (A.LongIdId ident) = f_identpat ident
	     | f_longidpat longid =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints inside f_longidpat function");
		   val (tv, lab, cl, cst) = f_longidexp longid
		   val c = E.initClassConstraint cl (CL.consCO0 ()) lab
	       in (tv, E.emvar, E.consConstraint (lab, c) cst, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.envvar, Env.cst) *)
	   and f_longstrid longstrid =
	       (D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints inside f_longstrid function");
	       case A.longstridToLid longstrid of
		   NONE => (E.freshEnvVar (), E.emptyConstraint)
		 | SOME lid =>
		   let val lab = I.getLabId lid
		       (* NOTE: We want all the labels from lid *)
		       val ev  = E.freshEnvVar ()
		       val a   = E.genAccIsEm (E.consAccId lid (E.consENV_VAR ev lab) (CL.consSTR ()) lab) lab
		   in (ev, E.singleConstraint (lab, E.ACCESSOR_CONSTRAINT a))
		   end)

	   (* RETURNS: (Ty.tyfvar, Env.cst) *)
	   and f_longtycon longtycon =
	       (D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints inside f_longtycon function");
	       case A.longtyconToLid longtycon of
		   NONE => (T.freshTypeFunctionVar (), E.emptyConstraint)
		 | SOME lid =>
		   let val lab = I.getLabId lid
		       val typeFunctionVar = T.freshTypeFunctionVar ()
		       val a   = E.genAccItEm (E.consAccId lid (T.consTYPE_FUNCTION_VAR typeFunctionVar) (CL.consTYCON ()) lab) lab
		   in (typeFunctionVar, E.singleConstraint (lab, E.ACCESSOR_CONSTRAINT a))
		   end)

	   (* RETURNS: (Ty.typeVar, Env.cst, Env.css) *)
	   and f_labexp (A.LabExp (exp, _, _, lab, _)) =
	       let
		   val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.LabExp (lab = "^(Int.toString(L.toInt(lab))^")"))
		   val (tv, cst, contextSensitiveSyntaxError) = f_exp exp
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION "*** LabExp constraints ***"
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ((#red D.colors)^(E.printConstraints cst))
		   val tv' = T.freshTypeVar ()
		   val c   = E.initTypeConstraint (T.consTYPE_VAR tv') (T.consTYPE_VAR tv) lab
		   val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("A.LabExp constraints generated (tv = "^Int.toString(T.typeVarToInt tv)^", "
						   ^"tv' = "^Int.toString(T.typeVarToInt tv')
						   ^", lab = "^(Int.toString(L.toInt(lab))^")"))
	       in (tv', E.consConstraint (lab, c) cst, contextSensitiveSyntaxError)
	       end
	     | f_labexp (A.LabExpDots pl) =
	       let val tv  = T.freshTypeVar ()
		   val env = f_partlist pl
	       in (tv, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.labvar, (string, Label.label) option, Env.cst) *)
	   and f_tylab (A.TyLab (s, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.TyLab");
		   val lv = T.freshLabelVar ()
		   val c  = E.initLabelConstraint (T.LABEL_VAR lv) (T.LC (s, lab)) lab
	       in (lv, SOME (s, lab), E.singleConstraint (lab, c))
	       end
	     | f_tylab A.TyLabDots =
	       (T.freshLabelVar (), NONE, E.emptyConstraint)

	   (* RETURNS: (((string, Label.label) option, Ty.fieldvar), Env.emptyConstraint, Env.emptyContextSensitiveSyntaxError) *)
	   and f_expfield (A.ExpField (tylab, labexp, _, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.ExpField");
		   val (lv, lop, cst1)  = f_tylab tylab
		   val (tv, cst2, css2) = f_labexp labexp
		   val rv = T.freshFieldVar ()
		   val c  = E.initFieldConstraint (T.FIELD_VAR rv) (T.FC (T.LABEL_VAR lv, T.consTYPE_VAR tv, lab)) lab
	       in ((lop, rv), E.consConstraint (lab, c) (E.unionConstraintsList [cst1, cst2]), css2)
	       end
	     | f_expfield (A.ExpFieldDots pl) =
	       let val rv  = T.freshFieldVar ()
		   val env = f_partlist pl
	       in ((NONE, rv), E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.typeVar, Env.cst, Env.css) *)
	   and f_seqexp (A.SeqExp (labexps, labexp, _, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.SeqExp");
		   val (tv2, cst2, css2) = f_labexp labexp
		   val (_, csts, csss) = unzipThree (map f_labexp labexps)
		   val cst = E.unionConstraintsList csts
		   val contextSensitiveSyntaxError = E.unionContextSensitiveSyntaxErrors csss
		   val tv  = T.freshTypeVar ()
		   val c   = E.initTypeConstraint (T.consTYPE_VAR tv) (T.consTYPE_VAR tv2) lab
	       in (tv, E.consConstraint (lab, c) (E.unionConstraintsList [cst, cst2]), E.unionContextSensitiveSyntaxErrors [contextSensitiveSyntaxError, css2])
	       end
	     | f_seqexp (A.SeqExpSl (parts, labexp, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.SeqExpSl");
		   val env1 = f_partlist parts
		   val (tv2, cst2, css2) = f_labexp labexp
		   val tv = T.freshTypeVar ()
		   val c  = E.initTypeConstraint (T.consTYPE_VAR tv) (T.consTYPE_VAR tv2) lab
	       in (tv, E.conscsts (lab, [E.LET_CONSTRAINT env1, c]) cst2, css2)
	       end
	     | f_seqexp (A.SeqExpDots pl) =
	       let val tv  = T.freshTypeVar ()
		   val env = f_partlist pl
	       in (tv, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.typeVar, Env.cst, Env.css) *)
	   and f_atexp (A.AtExpId id) =
	       let
		   val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.AtExpId (calling f_longidexp with parameter id)")
		   val (tv, _, _, cst) = f_longidexp id
		   val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("A.AtExpId constraints generated (tv = "^Int.toString(T.typeVarToInt tv)^"). They are:\n"^((#red D.colors)^(E.printConstraints cst)))
	       in (tv, cst, E.emptyContextSensitiveSyntaxError)
	       end
	     | f_atexp (A.AtExpScon sc) = (D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.AtExpScon"); f_scon sc)
	     | f_atexp (A.AtExpTuple (expl, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.AtExpTuple (lab = "^(Int.toString(L.toInt(lab))^")"))
		   val tv = T.freshTypeVar ()
		   val (tvl, csts, csss) = unzipThree (map f_labexp expl)
		   val cst = E.unionConstraintsList csts
		   val contextSensitiveSyntaxError = E.unionContextSensitiveSyntaxErrors csss
		   val c   = E.initTypeConstraint (T.consTYPE_VAR tv) (T.constytuple tvl lab) lab
	       in (tv, E.consConstraint (lab, c) cst, contextSensitiveSyntaxError)
	       end
	     | f_atexp (A.AtExpRecord (expfields, _, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.AtExpRecord (lab = "^(Int.toString(L.toInt(lab))^")"))
		   val tv = T.freshTypeVar ()
		   val (xs, csts, csss) = unzipThree (map f_expfield expfields)
		   val (lops, rts) = ListPair.unzip xs
		   val cst  = E.unionConstraintsList csts
		   val contextSensitiveSyntaxError  = E.unionContextSensitiveSyntaxErrors csss
		   val css' = consCSMlab (List.mapPartial (fn x => x) lops)
		   val c    = E.initTypeConstraint (T.consTYPE_VAR tv) (T.constyrecord rts (T.noflex ()) lab) lab
	       in (tv, E.consConstraint (lab, c) cst, E.unionContextSensitiveSyntaxErrors [contextSensitiveSyntaxError, css'])
	       end
	     | f_atexp (A.AtExpSlRec (expfields, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.AtExpSlRec (lab = "^(Int.toString(L.toInt(lab))^")"))
		   val tv  = T.freshTypeVar ()
		   val (xs, csts, csss) = unzipThree (map f_expfield expfields)
		   val (lops, rts) = ListPair.unzip xs
		   val cst  = E.unionConstraintsList csts
		   val contextSensitiveSyntaxError  = E.unionContextSensitiveSyntaxErrors csss
		   val contextSensitiveSyntaxError' = consCSMlab (List.mapPartial (fn x => x) lops)
		   (* We condiser that as a widlcard because it is incomplete: *)
		   val c    = E.initTypeConstraint (T.consTYPE_VAR tv) (T.constyrecord rts (T.consflex lab) lab) lab
	       in (tv, E.consConstraint (lab, c) cst, E.unionContextSensitiveSyntaxErrors [contextSensitiveSyntaxError, contextSensitiveSyntaxError'])
	       end
	     | f_atexp (A.AtExpLet (decs, labexp, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.AtExpLet (lab = "^(Int.toString(L.toInt(lab))^")"))
		   val tv   = T.freshTypeVar ()
		   val (env1, css1) = f_decs decs
		   val (tv2, cst2, css2) = f_labexp labexp
		   val env = E.envsToSeq [env1, E.CONSTRAINT_ENV cst2]
		   val c   = E.initTypeConstraint (T.consTYPE_VAR tv) (T.consTYPE_VAR tv2) lab
	       in (tv, E.consConstraint (L.dummyLab, E.LET_CONSTRAINT env) (E.singleConstraint (lab, c)), E.unionContextSensitiveSyntaxErrors [css1, css2])
	       end
	     | f_atexp (A.AtExpDLet (decs, seqexp, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.AtExpDLet (lab = "^(Int.toString(L.toInt(lab))^")"))
		   val tv   = T.freshTypeVar ()
		   val (env1, contextSensitiveSyntaxError1) = f_decs decs
		   val (tv2, cst2, contextSensitiveSyntaxError2) = f_seqexp seqexp
		   val env = E.envsToSeq [env1, E.CONSTRAINT_ENV cst2]
		   val c   = E.initTypeConstraint (T.consTYPE_VAR tv) (T.consTYPE_VAR tv2) lab
	       in (tv, E.consConstraint (L.dummyLab, E.LET_CONSTRAINT env) (E.singleConstraint (lab, c)), E.unionContextSensitiveSyntaxErrors [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2])
	       end
	     | f_atexp (A.AtExpParen (labexp, _, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.AtExpParen (lab = "^(Int.toString(L.toInt(lab))^")"))
		   val (tv, cst, contextSensitiveSyntaxError) = f_labexp labexp
		   val tv' = T.freshTypeVar ()
		   val c   = E.initTypeConstraint (T.consTYPE_VAR tv') (T.consTYPE_VAR tv) lab
	       in (tv', E.consConstraint (lab, c) cst, contextSensitiveSyntaxError)
	       end
	     | f_atexp (A.AtExpList (labexps, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.AtExpList (lab = "^(Int.toString(L.toInt(lab))^")"))
		   val tv   = T.freshTypeVar ()
		   val tv'  = T.freshTypeVar ()
		   val (tvs, csts, csss) = unzipThree (map f_labexp labexps)
		   val cst  = E.unionConstraintsList csts
		   val contextSensitiveSyntaxError  = E.unionContextSensitiveSyntaxErrors csss
		   val cs'  = map (fn x => E.initTypeConstraint (T.consTYPE_VAR tv') (T.consTYPE_VAR x) lab) tvs
		   val c    = E.initTypeConstraint (T.consTYPE_VAR tv) (T.constylist tv' lab) lab
	       in (tv, E.conscsts (lab, c :: cs') cst, contextSensitiveSyntaxError)
	       end
	     | f_atexp (A.AtExpProj (tylab, _, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.AtExpProj (lab = "^(Int.toString(L.toInt(lab))^")"))
		   val (lv, _, cst) = f_tylab tylab
		   val tv  = T.freshTypeVar ()
		   val tv' = T.freshTypeVar ()
		   val rv  = T.freshFieldVar ()
		   val ty  = (T.constyrecord [rv] (T.consflex lab) lab)
		   val c1  = E.initTypeConstraint (T.consTYPE_VAR tv) (T.consTyArrowTy ty (T.consTYPE_VAR tv') lab T.OTHER_CONS) lab
		   val c2  = E.initFieldConstraint (T.FIELD_VAR rv) (T.FC (T.LABEL_VAR lv, T.consTYPE_VAR tv', lab)) lab
	       in (tv, E.conscsts (lab, [c1, c2]) cst, E.emptyContextSensitiveSyntaxError)
	       end
	     | f_atexp (A.AtExpSeq (seqseq, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.AtExpSeq (lab = "^(Int.toString(L.toInt(lab))^")"))
		   val (tv, cst, contextSensitiveSyntaxError) = f_seqexp seqseq
		   val tv' = T.freshTypeVar ()
		   val c   = E.initTypeConstraint (T.consTYPE_VAR tv') (T.consTYPE_VAR tv) lab
	       in (tv', E.consConstraint (lab, c) cst, contextSensitiveSyntaxError)
	       end
	     | f_atexp (A.AtExpQuote (quotes, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.AtExpQuote (lab = "^(Int.toString(L.toInt(lab))^")"))
		   val (tvs, csts, csss) = unzipThree (map f_quote quotes)
		   val tv  = T.freshTypeVar ()
		   val tv' = T.freshTypeVar ()
		   val cst = E.unionConstraintsList csts
		   val contextSensitiveSyntaxError = E.unionContextSensitiveSyntaxErrors csss
		   val cs' = map (fn x => E.initTypeConstraint (T.consTYPE_VAR tv') (T.consTYPE_VAR x) lab) tvs
		   val c   = E.initTypeConstraint (T.consTYPE_VAR tv) (T.constylist tv' lab) lab
	       in (tv, E.conscsts (lab, c :: cs') cst, contextSensitiveSyntaxError)
	       end
	     | f_atexp (A.AtExpDots pl) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.AtExpDots")
		   val tv  = T.freshTypeVar ()
		   val env = f_partlist pl
	       in (tv, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* When evaluation: `^(1)`, SML/NJ returns:
	    *   val it = [QUOTE "",ANTIQUOTE 1,QUOTE ""] : int SMLofNJ.frag list
	    *
	    * For this example, for the antiquote:
	    *   tv  = int
	    *   tv' = type of the antiquote *)

	   (* RETURNS: (Ty.typeVar, Env.cst, Env.css) *)
	   and f_quote (A.Quote (_, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.Quote");
		   val tv1 = T.freshTypeVar ()
		   val tv2 = T.freshTypeVar ()
		   val c   = E.initTypeConstraint (T.consTYPE_VAR tv1) (T.constyfrag tv2 lab) lab
	       in (tv1, E.singleConstraint (lab, c), E.emptyContextSensitiveSyntaxError)
	       end
	     | f_quote (A.Antiquote(exp, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.Antiquote");
		   val (tv, cst, contextSensitiveSyntaxError) = f_exp exp
		   val tv' = T.freshTypeVar ()
		   val c   = E.initTypeConstraint (T.consTYPE_VAR tv') (T.constyfrag tv lab) lab
	       in (tv', E.consConstraint (lab, c) cst, contextSensitiveSyntaxError)
	       end
	     | f_quote (A.QuoteDots pl) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.QuoteDots");
		   val tv  = T.freshTypeVar ()
		   val env = f_partlist pl
	       in (tv, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.typeVar, Env.cst, Env.css) *)
	   and f_exp (A.ExpAtExp atexp) =
	       (D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION "generating constraints for A.ExpAtExp (just returns result of atexp applied to f_atexp)";
		f_atexp atexp)
	     | f_exp (A.ExpFn (match, _, lab, _)) =
	       let
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.ExpFn (lab = "^(Int.toString(L.toInt(lab))^")"))
		   val (tvs, cst, contextSensitiveSyntaxError) = f_match match
		   val tv = T.freshTypeVar ()
		   val cs = map (fn x => E.initTypeConstraint (T.consTYPE_VAR tv) (T.consTYPE_VAR x) lab) tvs
	       in (tv, E.conscsts (lab, cs) cst, contextSensitiveSyntaxError)
	       end
	     | f_exp (A.ExpApp (exp, atexp, _, _, _, lab, _)) =
	       let
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.ExpApp (lab = "^(Int.toString(L.toInt(lab))^")"))
 		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ((#green D.colors)^"left hand side of application...")
		   val (tv1, cst1, contextSensitiveSyntaxError1) = f_exp exp
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ((#red D.colors)^"right hand side of application...")
		   val (tv2, cst2, contextSensitiveSyntaxError2) = f_atexp atexp

		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("printing constraints for left hand side of application...\n"^(#green D.colors)^(E.printConstraints cst1))
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("printing constraints for right hand side of application...\n"^(#red D.colors)^(E.printConstraints cst2))

		   (* look at the right hand side of the application - does it contain an equality type? *)
		   val rhsEqualityStatuses = E.stripEqualityStatusFromConstraints (List.foldr (op @) [] (E.getConstraintItems cst2))
		   val foundEqualityStatus = List.find (fn x => (if x = T.NOT_EQUALITY_TYPE then true else false)) rhsEqualityStatuses

		   val _ = case foundEqualityStatus of
			       (* no NOT_EQUALITY_TYPE constructor found in the right hand side, carry on *)
			       NONE => ()

			     (* If yes, then the left hand side (the accessor) should be constrained to be an equality type also.
			      * It's currently of UNKNOWN status, so that will need to be removed and NOT_EQUALITY_TYPE put in its place *)
			     | SOME x =>
			       D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("something being used in an application is not an equaltiy type, propagating the constraint")
			       val cst1 = E.createEqualityTypeConstraints cst1 lab T.NOT_EQUALITY_TYPE

		   val tv  = T.freshTypeVar ()
		   val c   = E.initTypeConstraint (T.consTYPE_VAR tv1) (T.constyarrow tv2 tv lab) lab
	       in (tv, E.consConstraint (lab, c) (E.unionConstraintsList [cst1, cst2]), E.unionContextSensitiveSyntaxErrors [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2])
	       end
	     | f_exp (A.ExpCase (labexp, match, _, _, lab, _)) =
	       let
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION "generating constraints for A.ExpCase"
		   val tv  = T.freshTypeVar ()
		   val tv' = T.freshTypeVar ()
		   val (tv1, cst1, contextSensitiveSyntaxError1) = f_labexp labexp
		   val (tvs, cst2, contextSensitiveSyntaxError2) = f_match match
		   val cs  = map (fn x => E.initTypeConstraint (T.consTYPE_VAR tv') (T.consTYPE_VAR x) lab) tvs
		   val c   = E.initTypeConstraint (T.consTYPE_VAR tv') (T.constyarrow tv1 tv lab) lab
	       in (tv, E.conscsts (lab, c :: cs) (E.unionConstraintsList [cst1, cst2]), E.unionContextSensitiveSyntaxErrors [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2])
	       end
	     | f_exp (A.ExpConsList (id, labexp1, labexp2, reg, lab, nxt)) =
	       if benv
	       then f_exp (A.ExpOp ("::", id, labexp1, labexp2, reg, lab, nxt))
	       else let
			val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION "generating constraints for A.ExpConsList"
			val (tv1, cst1, contextSensitiveSyntaxError1) = f_labexp labexp1
			val (tv2, cst2, contextSensitiveSyntaxError2) = f_labexp labexp2
			val tv = T.freshTypeVar ()
			val c1 = E.initTypeConstraint (T.consTYPE_VAR tv)  (T.constylist tv1 lab) lab
			val c2 = E.initTypeConstraint (T.consTYPE_VAR tv2) (T.constylist tv1 lab) lab
		    in (tv, E.conscsts (lab, [c1, c2]) (E.unionConstraintsList [cst1, cst2]), E.unionContextSensitiveSyntaxErrors [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2])
		    end
	     | f_exp (A.ExpOp (st, id, labexp1, labexp2, _, lab, _)) =
	       let

		   (* note that we should only generate equality constraints here if we are
		    * dealing with the equals operator. We do not want to generate these constraints
		    * if, for example, we are dealing with the + operator
		    *)

		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.ExpOp (st = \"" ^ st ^ "\", lab="^Int.toString(L.toInt lab)^")")
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("**********  ExpOp left hand side...  **********")
		   val (tv1, cst1, contextSensitiveSyntaxError1) = f_labexp labexp1
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION  ("**********  ExpOp right hand side... **********")
		   val (tv2, cst2, contextSensitiveSyntaxError2) = f_labexp labexp2
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("done left and right of expop\nprinting constraints for cst1 (tv ="^(Int.toString(T.typeVarToInt(tv1)))^")...\n"^(#red D.colors)^(E.printConstraints cst1))
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("printing constraints for cst2...\n"^(#green D.colors)^(E.printConstraints cst2))
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("ExpOp type variables are tv1 = "^(Int.toString(T.typeVarToInt(tv1)))^", tv2 = "^(Int.toString(T.typeVarToInt(tv2))))
		   val ty  = T.newTYPE_VAR ()
		   val ti  = T.constytuple [tv1, tv2] lab
		   val tvo = T.freshTypeVar ()
 		   val c   = E.initTypeConstraint ty (T.consTyArrowTy ti (T.consTYPE_VAR tvo) lab T.OTHER_CONS) lab
		   (* we need to fin all of the equality type constraints in cst1 and cst2
		    * *all* of the equality type constraints *must* be EQUALITY_TYPE. If any are NOT_EQUALITY_TYPE
		    * then we cannot compare the left hand side and the right hand side for equality *)
		   (* val equalityTypeVars = [(E.getEqualityTypeVars cst1), (E.getEqualityTypeVars cst2)] *)
		   (* fun printeqtvs [] = "" *)
		   (*   | printeqtvs (h::t) = Int.toString(T.typeVarToInt(h))^","^(printeqtvs t) *)
		   (* val _ = D.printDebugFeature D.AZE D.EQUALITY_TYPES ("eqty in list of equalityTypeVars = "^(printeqtvs equalityTypeVars)) *)

		   (* Right, so above we have collected all of the equality type variables that have
		    * been generated for things like SconInt and SconReal.
		    *
		    * For each one of those equality type variables, we need a constraint that constraints
		    * then all to be mapped to EQUALITY_TYPE in the state, and not NOT_EQUALITY_TYPE
		    *
		    * I can either create a bunch of individual constraints of put all of the equality
		    * type variables in a list. What is the best option here?
		    *
		    * Let's try individual constraints to start with.
		    *)

		   (* val eqVar = T.freshEqualityTypeVar () *)
		   (* val allEqualityTypeVars = equalityTypeVars@equalityTypeVars2 *)
		   (* val equalityConstraints =  List.map (fn eqtv => E.initEqualityTypeConstraint (Ty.consTYPE_VAR eqtv) (Ty.EQUALITY_TYPE_STATUS (Ty.EQUALITY_TYPE)) lab) allEqualityTypeVars *)

		   val equalityConstraints = E.createEqualityTypeConstraints cst1 lab T.EQUALITY_TYPE
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("printing constraints for equalityConstraints...\n"^(#red D.colors)^(E.printConstraints equalityConstraints))
		   val equalityConstraints2 = E.createEqualityTypeConstraints cst2 lab T.EQUALITY_TYPE
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("printing constraints for equalityConstraints2...\n"^(#green D.colors)^(E.printConstraints equalityConstraints2))

		   val allConstraints1 = E.unionConstraintsList [cst1, equalityConstraints]
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("printing constraints for allConstraints1...\n"^(#red D.colors)^(E.printConstraints allConstraints1))
		   val allConstraints2 = E.unionConstraintsList [cst2, equalityConstraints2]
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("printing constraints for allConstraints2...\n"^(#red D.colors)^(E.printConstraints allConstraints2))

		   val a   = E.initValueIDAccessor (E.consAccId (I.ID (id, lab)) ty (CL.consVAL ()) lab) lab

		   (* this was the print method used when we were generating a list of constraints rather than of type datatype constructor CONSTRAINTS *)
		   (* val _ = D.printDebugFeature D.AZE D.EQUALITY_TYPES ("FINAL EXP OP CONSTRAINTS:\n"^(#yellow D.colors)^ (E.printConstraints(E.conscsts (lab, (equalityConstraints@equalityConstraints2@[c, E.ACCESSOR_CONSTRAINT a])) (E.unionConstraintsList [cst1, cst2])))) *)

		   (* this was the method used where everything worked aside from test126. This was before the bugfix where some constraints were getting lost *)
		   (* val _ = D.printDebugFeature D.AZE D.EQUALITY_TYPES ("FINAL EXP OP CONSTRAINTS:\n"^(#yellow D.colors)^ (E.printConstraints (E.conscsts (lab, ([c, E.ACCESSOR_CONSTRAINT a])) (E.unionConstraintsList [cst1, cst2, equalityConstraints2, equalityConstraints])))) *)

		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("ExpOp constraints generated\n")

	       (* i think we want something like this but it breaks the constraint generator in the case of code1.sml. Why? The constraints should be exactly the same for that case?? *)
	       in
		   if st = "="
		   then (tvo, E.conscsts (lab, ([c, E.ACCESSOR_CONSTRAINT a])) (E.unionConstraintsList [allConstraints1, allConstraints2]) , E.unionContextSensitiveSyntaxErrors [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2])
		   else (tvo, E.conscsts (lab, [c, E.ACCESSOR_CONSTRAINT a]) (E.unionConstraintsList [cst1, cst2]), E.unionContextSensitiveSyntaxErrors [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2])
	       (* when we keep cst1 and cst2 everything seems to work apart from test126, which seems to blow up *)
	       (* in (tvo, E.conscsts (lab, ([c, E.ACCESSOR_CONSTRAINT a])) (E.unionConstraintsList [cst1, cst2, equalityConstraints, equalityConstraints2]), E.unionContextSensitiveSyntaxErrors [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2]) *)
	       end
	     | f_exp (A.ExpOr (labexp1, labexp2, _, lab, _)) =
	       let
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION "generating constraints for A.ExpOr"
		   val (tv1, cst1, contextSensitiveSyntaxError1) = f_labexp labexp1
		   val (tv2, cst2, contextSensitiveSyntaxError2) = f_labexp labexp2
		   val tvo = T.freshTypeVar ()
		   val c1  = E.initTypeConstraint (T.consTYPE_VAR tvo) (T.constybool lab) lab
		   val c2  = E.initTypeConstraint (T.consTYPE_VAR tv1) (T.constybool lab) lab
		   val c3  = E.initTypeConstraint (T.consTYPE_VAR tv2) (T.constybool lab) lab
	       in (tvo, E.conscsts (lab, [c1, c2, c3]) (E.unionConstraintsList [cst1, cst2]), E.unionContextSensitiveSyntaxErrors [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2])
	       end
	     | f_exp (A.ExpAnd (labexp1, labexp2, _, lab, _)) =
	       let
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION "generating constraints for A.ExpAnd"
		   val (tv1, cst1, contextSensitiveSyntaxError1) = f_labexp labexp1
		   val (tv2, cst2, contextSensitiveSyntaxError2) = f_labexp labexp2
		   val tvo = T.freshTypeVar ()
		   val c1  = E.initTypeConstraint (T.consTYPE_VAR tvo) (T.constybool lab) lab
		   val c2  = E.initTypeConstraint (T.consTYPE_VAR tv1) (T.constybool lab) lab
		   val c3  = E.initTypeConstraint (T.consTYPE_VAR tv2) (T.constybool lab) lab
	       in (tvo, E.conscsts (lab, [c1, c2, c3]) (E.unionConstraintsList [cst1, cst2]), E.unionContextSensitiveSyntaxErrors [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2])
	       end
	     | f_exp (A.ExpTyped (labexp, labtyp, _, lab, _)) =
	       let
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION "generating constraints for A.ExpTyped"
		   val tv = T.freshTypeVar ()
		   val (tv1, cst1, contextSensitiveSyntaxError1) = f_labexp labexp
		   val (tv2, eqtv, cst2, contextSensitiveSyntaxError2) = f_labtype labtyp
		   val c1 = E.initTypeConstraint (T.consTYPE_VAR tv) (T.consTYPE_VAR tv1) lab
		   val c2 = E.initTypeConstraint (T.consTYPE_VAR tv) (T.consTYPE_VAR tv2) lab
	       in (tv, E.conscsts (lab, [c1, c2]) (E.unionConstraintsList [cst1, cst2]), E.unionContextSensitiveSyntaxErrors [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2])
	       end
	     | f_exp (A.ExpIte (labexp1, labexp2, labexp3, _, lab, _)) =
	       let
		   val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.ExpIte")
		   (* get the type variables, constraint on types and E.ocss *)
		   val (tv1, cst1, contextSensitiveSyntaxError1) = f_labexp labexp1
		   val (tv2, cst2, contextSensitiveSyntaxError2) = f_labexp labexp2
		   val (tv3, cst3, contextSensitiveSyntaxError3) = f_labexp labexp3

		   (* constrain the condition of the if statement to be of type bool*)
		   val c1  = E.initTypeConstraint (T.consTYPE_VAR tv1) (T.constybool lab) lab

		   (* generate a fresh type variable and constrain the true/false branches
		    * of the if statemnt to be of the same type *)
		   val tv = T.freshTypeVar ()
		   val c2  = E.initTypeConstraint (T.consTYPE_VAR tv) (T.consTYPE_VAR tv2) lab
		   val c3  = E.initTypeConstraint (T.consTYPE_VAR tv) (T.consTYPE_VAR tv3) lab

		   val cst = E.conscsts (lab, [c1, c2, c3]) (E.unionConstraintsList [cst1, cst2, cst3])
		   val contextSensitiveSyntaxError = E.unionContextSensitiveSyntaxErrors [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2, contextSensitiveSyntaxError3]
		   val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("A.ExpIte constraints generated\n")
	       in (tv, cst, contextSensitiveSyntaxError)
	       end
	     | f_exp (A.ExpWhile (labexp1, labexp2, _, _, lab, _)) =
	       let
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION "generating constraints for A.ExpWhile"
		   val tv = T.freshTypeVar ()
		   val (tv1, cst1, contextSensitiveSyntaxError1) = f_labexp labexp1
		   val (tv2, cst2, contextSensitiveSyntaxError2) = f_labexp labexp2
		   val c1 = E.initTypeConstraint (T.consTYPE_VAR tv1) (T.constybool lab) lab
		   val c2 = E.initTypeConstraint (T.consTYPE_VAR tv) (T.constytuple [] lab) lab
	       in (tv, E.conscsts (lab, [c1, c2]) (E.unionConstraintsList [cst1, cst2]), E.unionContextSensitiveSyntaxErrors [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2])
	       end
	     | f_exp (A.ExpRaise (labexp, _, lab, _)) =
	       let
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION "generating constraints for A.ExpRaise"
		   val (tv, cst, contextSensitiveSyntaxError) = f_labexp labexp
		   val tv' = T.freshTypeVar ()
		   val c   = E.initTypeConstraint (T.consTYPE_VAR tv) (T.constyexception lab) lab
	       in (tv', E.consConstraint (lab, c) cst, contextSensitiveSyntaxError)
	       end
	     | f_exp (A.ExpHandle (labexp, match, _, lab, _)) =
	       let
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION "generating constraints for A.ExpHandle"
		   val (tv1, cst1, contextSensitiveSyntaxError1) = f_labexp labexp
		   val (tvs, cst2, contextSensitiveSyntaxError2) = f_match match
		   val ty  = T.newTYPE_VAR ()
		   val tv  = T.freshTypeVar ()
		   val cs  = map (fn x => E.initTypeConstraint ty (T.consTYPE_VAR x) lab) tvs
		   val c1  = E.initTypeConstraint ty (T.consTyArrowTy (T.constyexception lab) (T.consTYPE_VAR tv1) lab T.OTHER_CONS) lab
		   val c2  = E.initTypeConstraint (T.consTYPE_VAR tv) (T.consTYPE_VAR tv1) lab
	       in (tv, E.conscsts (lab, c1 :: c2 :: cs) (E.unionConstraintsList [cst1, cst2]), E.unionContextSensitiveSyntaxErrors [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2])
	       end
	     | f_exp (A.ExpDots pl) =
	       let
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION "generating constraints for A.ExpDots"
		   val tv  = T.freshTypeVar ()
		   val env = f_partlist pl
	       in (tv, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.typeVar list, Env.cst, Env.css) *)
	   and f_match (A.Match (mrules, _, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.Match");
		   val (tvs, csts, csss) = unzipThree (map f_mrule mrules)
		   val cst = E.unionConstraintsList csts
		   val contextSensitiveSyntaxError = E.unionContextSensitiveSyntaxErrors csss
	       in (tvs, cst, contextSensitiveSyntaxError)
	       end
	     | f_match (A.MatchDots pl) =
	       let val env = f_partlist pl
	       in ([], E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.typeVar, Env.cst, Env.css) *)
	   and f_mrule (A.Mrule (labpat, labexp, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.Mrule");
		   val tv  = T.freshTypeVar ()
		   val (tv1, vids, cst1, contextSensitiveSyntaxError1) = f_labpat labpat
		   val (tv2, cst2, contextSensitiveSyntaxError2) = f_labexp labexp
		   val env = E.ROW_ENV (E.ROW_ENV (E.CONSTRAINT_ENV cst1, E.projValueIds (E.toMonoValueIds vids (L.singleton lab))), E.CONSTRAINT_ENV cst2)
		   val c   = E.initTypeConstraint (T.consTYPE_VAR tv) (T.constyarrow tv1 tv2 lab) lab
		   val cst = E.consConstraint (L.dummyLab, E.LET_CONSTRAINT env) (E.singleConstraint (lab, c))
		   val contextSensitiveSyntaxError = E.unionContextSensitiveSyntaxErrors [clearRebound contextSensitiveSyntaxError1, contextSensitiveSyntaxError2]
	       in (tv, cst, contextSensitiveSyntaxError)
	       end
	     | f_mrule (A.MruleDots pl) =
	       let val tv  = T.freshTypeVar ()
		   val env = f_partlist pl
	       in (tv, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* f_typevarval: For type variables. For example
	    *
	    * str is the type variable, eg
	    * "'a" in the code: "datatype 'a t = C of 'a | D of int -> int"
	    *)
	   and f_typevar (A.TypeVar (str, id, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.TypeVar (str=\""^str^"\")")
		   val tv = T.freshTypeVar ()
		   val eqTypeVar = T.freshEqualityTypeVar ()
		   val a  = E.genAccIeEm (E.consAccId (I.ID (id, lab)) tv (CL.consTYVAR ()) lab) lab
	       in (SOME id, tv, eqTypeVar, E.singleConstraint (lab, E.ACCESSOR_CONSTRAINT a))
	       end
	     | f_typevar (A.EqualityTypeVar (str, id, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.EqualityTypeVar (f_typevar; str=\""^str^"\")")
		   val tv = T.freshTypeVar ()
		   val eqTypeVar = T.freshEqualityTypeVar ()

		   (* jpirie: perhaps some constraint should be built here for equality types
		    * At the moment this function just does the same as TypeVar, so we
		    * have no difference between the two. The way to pass information
		    * down that we have in fact encountered an equality type variable
		    * seems to be to generate a constraint, which then becomes part of
		    * cst, and is passed down that way so we can actually see the costraints
		    * when we get to dealing with the A.ConBindOf constructor in another
		    * function.
		    *)
		   val c   = E.initEqualityTypeConstraint (T.consEQUALITY_TYPE_VAR eqTypeVar) (T.EQUALITY_TYPE_STATUS(T.EQUALITY_TYPE)) lab
		   val a  = E.genAccIeEm (E.consAccId (I.ID (id, lab)) tv (CL.consTYVAR ()) lab) lab

	       (* jpirie: I'm adding in the constraint I created above, but two things are using the same label?
		* is that right? Or is that wrong? Other examples use dummylab?
		* Maybe that's actually right, look at the debug output for other examples and labels
		* do seem to sometimes get bound sometimes to two things (usually one of which is an accessor) *)
	       in (SOME id, tv, eqTypeVar, (E.consConstraint (lab, c) (E.singleConstraint (lab, E.ACCESSOR_CONSTRAINT a))))
	       end
	     | f_typevar A.TypeVarDots =
	       let val tv = T.freshTypeVar ()
		   val eqTypeVar = T.freshEqualityTypeVar ()
	       in (NONE, tv, eqTypeVar, E.emptyConstraint)
	       end

	   (* RETURNS: (Ty.typeVar, Env.cst, Env.css) *)
	   and f_labtype (A.LabType (typ, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.LabType");
		   val (tv, eqTypeVar, cst, contextSensitiveSyntaxError) = f_type typ

		   (* we create a new type variable tv' and constraint it to be equal to
		    * tv that we get from f_type. That way we get this label (the label
		    * of f_labtype) in the constraints *)
		   val newTypeVar = T.freshTypeVar ()
		   val newTypeVarConstraint   = E.initTypeConstraint (T.consTYPE_VAR newTypeVar) (T.consTYPE_VAR tv) lab

		   (* same story for the equality type variable - new constraint *)
		   val newEqTypeVar = T.freshEqualityTypeVar ()
		   val newEqTypeVarConstraint  = E.initEqualityTypeConstraint (T.consEQUALITY_TYPE_VAR newEqTypeVar) (T.consEQUALITY_TYPE_VAR eqTypeVar) lab

	       in (newTypeVar, newEqTypeVar, E.conscsts (lab, [newTypeVarConstraint, newEqTypeVarConstraint]) cst, contextSensitiveSyntaxError)
	       end
	     | f_labtype (A.LabTypeDots pl) =
	       let val tv  = T.freshTypeVar ()
		   val eqtv  = T.freshEqualityTypeVar ()
		   val env = f_partlist pl
	       in (tv, eqtv, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (((string, Label.label) option, Ty.fieldvar), Env.emptyConstraint, Env.emptyContextSensitiveSyntaxError) *)
	   and f_tyfield (A.TyField (tylab, labtyp, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.TyField");
		   val (lv, lop, cst1)  = f_tylab tylab
		   val (tv, eqtv, cst2, contextSensitiveSyntaxError2) = f_labtype labtyp
		   val rv = T.freshFieldVar ()
		   val c  = E.initFieldConstraint (T.FIELD_VAR rv) (T.FC (T.LABEL_VAR lv, T.consTYPE_VAR tv, lab)) lab
	       in ((lop, rv), E.consConstraint (lab, c) (E.unionConstraintsList [cst1, cst2]), contextSensitiveSyntaxError2)
	       end
	     | f_tyfield (A.TyFieldDots pl) =
	       let val rv  = T.freshFieldVar ()
		   val env = f_partlist pl
	       in ((NONE, rv), E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.typeVar, Env.cst, Env.css) *)
	   and f_type (A.TypeOneVar typeVar) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.TypeOneVar");
		   val (_, tv, eqTypeVar, cst) = f_typevar typeVar
	       (* we don't make a new type variable tv' and constrain it to be equal to tv (and a similar
		* story for the equality type variable) because there is no new label *)
	       in (tv, eqTypeVar, cst, E.emptyContextSensitiveSyntaxError)
	       end
	     | f_type (A.TypeArrow (labtyp1, labtyp2, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.TypeArrow");
		   val (tv1, eqtv, cst1, contextSensitiveSyntaxError1) = f_labtype labtyp1
		   val (tv2, eqtv, cst2, contextSensitiveSyntaxError2) = f_labtype labtyp2
		   val tv = T.freshTypeVar ()
		   val eqTypeVar  = T.freshEqualityTypeVar ()
		   val c  = E.initTypeConstraint (T.consTYPE_VAR tv) (T.constyarrow tv1 tv2 lab) lab
	       in (tv, eqTypeVar, E.consConstraint (lab, c) (E.unionConstraintsList [cst1, cst2]), E.unionContextSensitiveSyntaxErrors [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2])
	       end
	     | f_type (A.TypeTuple (labtyps, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.TypeTuple");
		   val (tvs, eqtvs, csts, csss) = unzipFour (map f_labtype labtyps)
		   val tv  = T.freshTypeVar ()
		   val eqTypeVar  = T.freshEqualityTypeVar ()
		   val cst = E.unionConstraintsList csts
		   val contextSensitiveSyntaxError = E.unionContextSensitiveSyntaxErrors csss
		   val c   = E.initTypeConstraint (T.consTYPE_VAR tv) (T.constytuple tvs lab) lab
	       in (tv, eqTypeVar, E.consConstraint (lab, c) cst, contextSensitiveSyntaxError)
	       end
	     | f_type (A.TypeRecord (tyfields, _, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.TypeRecord");
		   val (xs, csts, csss) = unzipThree (map f_tyfield tyfields)
		   val (lops, rts) = ListPair.unzip xs
		   val tv   = T.freshTypeVar ()
		   val eqTypeVar  = T.freshEqualityTypeVar ()
		   val cst  = E.unionConstraintsList csts
		   val contextSensitiveSyntaxError  = E.unionContextSensitiveSyntaxErrors csss
		   val contextSensitiveSyntaxError' = consCSMlab (List.mapPartial (fn x => x) lops)
		   val c    = E.initTypeConstraint (T.consTYPE_VAR tv) (T.constyrecord rts (T.noflex ()) lab) lab
	       in (tv, eqTypeVar, E.consConstraint (lab, c) cst, E.unionContextSensitiveSyntaxErrors [contextSensitiveSyntaxError, contextSensitiveSyntaxError'])
	       end
	     | f_type (A.TypeSlRec (tyfields, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.TypeSlRec");
		   val (xs, csts, csss) = unzipThree (map f_tyfield tyfields)
		   val (lops, rts) = ListPair.unzip xs
		   val tv   = T.freshTypeVar ()
		   val eqTypeVar  = T.freshEqualityTypeVar ()
		   val cst  = E.unionConstraintsList csts
		   val contextSensitiveSyntaxError  = E.unionContextSensitiveSyntaxErrors csss
		   val contextSensitiveSyntaxError' = consCSMlab (List.mapPartial (fn x => x) lops)
		   val c    = E.initTypeConstraint (T.consTYPE_VAR tv) (T.constyrecord rts (T.consflex lab) lab) lab
	       in (tv, eqTypeVar, E.consConstraint (lab, c) cst, E.unionContextSensitiveSyntaxErrors [contextSensitiveSyntaxError, contextSensitiveSyntaxError'])
	       end
	     | f_type (A.TypeTyCon (typseq, longtycon, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.TypeTyCon")
		   val (sv, cst1, contextSensitiveSyntaxError1) = f_typeRow typseq
		   val (typeFunctionVar, cst2) = f_longtycon longtycon
		   val tv  = T.freshTypeVar ()
		   val tv' = T.freshTypeVar ()
		   val eqTypeVar  = T.freshEqualityTypeVar ()
		   val sv' = T.freshRowVar ()
		   val c1  = E.initFunctionTypeConstraint (T.TYPE_FUNCTION_VAR typeFunctionVar) (T.TFC (T.ROW_VAR sv', T.consTYPE_VAR tv', lab)) lab
		   val c2  = E.initTypeConstraint (T.consTYPE_VAR tv) (T.consTYPE_VAR tv') lab
		   val c3  = E.initRowConstraint (T.ROW_VAR sv) (T.ROW_VAR sv') lab
	       in (tv, eqTypeVar, E.conscsts (lab, [c1, c2, c3]) (E.unionConstraintsList [cst1, cst2]), contextSensitiveSyntaxError1)
	       end
	     | f_type (A.TypeParen (labtyp, _, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.TypeParen")
		   val (tv, eqtv, cst, contextSensitiveSyntaxError) = f_labtype labtyp
		   val tv' = T.freshTypeVar ()
		   val eqTypeVar  = T.freshEqualityTypeVar ()
		   val c   = E.initTypeConstraint (T.consTYPE_VAR tv') (T.consTYPE_VAR tv) lab
	       in (tv', eqTypeVar, E.consConstraint (lab, c) cst, contextSensitiveSyntaxError)
	       end
	     | f_type (A.TypeDots pl) =
	       let val tv  = T.freshTypeVar ()
 		   val eqTypeVar  = T.freshEqualityTypeVar ()
		   val env = f_partlist pl
	       in (tv, eqTypeVar, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (T.rowVar, E.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
	   and f_typeRow (A.TypeRowOne (typ, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.TypeRowOne")
		   val (tv, eqTypeVar, cst, contextSensitiveSyntaxError) = f_type typ
		   val sv = T.freshRowVar ()
		   val c  = E.initRowConstraint (T.ROW_VAR sv) (T.ROW_C (T.constuple [tv] lab, T.noflex (), lab)) lab
	       in (sv, E.consConstraint (lab, c) cst, contextSensitiveSyntaxError)
	       end
	     | f_typeRow (A.TypeRowEm (_, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.TypeRowEm")
		   val sv = T.freshRowVar ()
		   val c  = E.initRowConstraint (T.ROW_VAR sv) (T.ROW_C ([], T.noflex (), lab)) lab
	       in (sv, E.singleConstraint (lab, c), E.emptyContextSensitiveSyntaxError)
	       end
	     | f_typeRow (A.TypeRowSeq (labtyps, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.TypeRowSeq")
		   val (svs, eqtvs, csts, csss) = unzipFour (map f_labtype labtyps)
		   val sv  = T.freshRowVar ()
		   val cst = E.unionConstraintsList csts
		   val contextSensitiveSyntaxError = E.unionContextSensitiveSyntaxErrors csss
		   val c   = E.initRowConstraint (T.ROW_VAR sv) (T.ROW_C (T.constuple svs lab, T.noflex (), lab)) lab
	       in (sv, E.consConstraint (lab, c) cst, contextSensitiveSyntaxError)
	       end
	     | f_typeRow (A.TypeRowDots pl) =
	       let val sv  = T.freshRowVar ()
		   val env = f_partlist pl
	       in (sv, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.typeVar, Env.varenv, E.emptyConstraint, Env.css) *)
	   and f_labpat (A.LabPat (pat, _, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.LabPat")
		   val (tv, vids, cst, contextSensitiveSyntaxError) = f_pat pat
		   val tv' = T.freshTypeVar ()
		   val c   = E.initTypeConstraint (T.consTYPE_VAR tv') (T.consTYPE_VAR tv) lab
	       in (tv', vids, E.consConstraint (lab, c) cst, contextSensitiveSyntaxError)
	       end
	     | f_labpat (A.LabPatDots pl) =
	       let val env = f_partlist pl
		   val tv  = T.freshTypeVar ()
	       in (tv, E.getValueIds env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.typeVar, Env.varenv, E.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
	   and f_identty (A.IdentTyId ident) = f_identpat ident
	     | f_identty (A.IdentTyTy (labid, labtyp, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.IdentTyTy")
		   val (tv1, vids, cst1, contextSensitiveSyntaxError1) = f_labid labid
		   val (tv2, eqtv, cst2, contextSensitiveSyntaxError2) = f_labtype labtyp
		   val tv = T.freshTypeVar ()
		   val c1 = E.initTypeConstraint (T.consTYPE_VAR tv) (T.consTYPE_VAR tv1) lab
		   val c2 = E.initTypeConstraint (T.consTYPE_VAR tv) (T.consTYPE_VAR tv2) lab
	       in (tv, vids, E.conscsts (lab, [c1, c2]) (E.unionConstraintsList [cst1, cst2]), E.unionContextSensitiveSyntaxErrors [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2])
	       end
	     | f_identty (A.IdentTyDots pl) =
	       let val env = f_partlist pl
		   val tv  = T.freshTypeVar ()
	       in (tv, E.getValueIds env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.typeVar, Env.varenv, E.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
	   and f_labidty (A.LabIdTy (identty, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.LabIdTy")
		   val (tv, vids, cst, contextSensitiveSyntaxError) = f_identty identty
		   val tv' = T.freshTypeVar ()
		   val c   = E.initTypeConstraint (T.consTYPE_VAR tv') (T.consTYPE_VAR tv) lab
	       in (tv', vids, E.consConstraint (lab, c) cst, contextSensitiveSyntaxError)
	       end
	     | f_labidty (A.LabIdTyDots pl) =
	       let val env = f_partlist pl
		   val tv  = T.freshTypeVar ()
	       in (tv, E.getValueIds env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (((string, Label.label) option, Ty.fieldvar option, Label.label option), Env.varenv, Env.emptyConstraint, Env.emptyContextSensitiveSyntaxError) *)
	   and f_patfield (A.PatField (tylab, labpat, _, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.PatField")
		   val (lv, lop, cst1) = f_tylab tylab
		   val (tv, vids, cst2, contextSensitiveSyntaxError2) = f_labpat labpat
		   val rv = T.freshFieldVar ()
		   val c  = E.initFieldConstraint (T.FIELD_VAR rv) (T.FC (T.LABEL_VAR lv, T.consTYPE_VAR tv, lab)) lab
	       in ((lop, SOME rv, NONE), vids, E.consConstraint (lab, c) (E.unionConstraintsList [cst1, cst2]), contextSensitiveSyntaxError2)
	       end
	     | f_patfield (A.PatFieldId (identty, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.PatFieldId")
		   val (tv, vids, cst, contextSensitiveSyntaxError) = f_identty identty
		   val rv   = T.freshFieldVar ()
		   val llop = A.getlabstIdentTy identty
		   val cst' = case llop of
				 NONE => E.emptyConstraint
			       | SOME (l1, l2, s) =>
				 let val lv = T.freshLabelVar ()
				     val c1 = E.initLabelConstraint (T.LABEL_VAR lv) (T.LC (s, l1)) l1
				     val c2 = E.initFieldConstraint (T.FIELD_VAR rv) (T.FC (T.LABEL_VAR lv, T.consTYPE_VAR tv, l2)) l2
				 in E.consConstraint (l1, c1) (E.singleConstraint (l2, c2))
				 end
		   val lop  = case llop of NONE => NONE | SOME (_, l, s) => SOME (s, l)
	       in ((lop, SOME rv, NONE), vids, E.unionConstraintsList [cst, cst'], contextSensitiveSyntaxError)
	       end
	     | f_patfield (A.PatFieldAs (labidty, labpat, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.PatFieldAs")
		   val (tv1, vids1, cst1, contextSensitiveSyntaxError1) = f_labidty labidty
		   val (tv2, vids2, cst2, contextSensitiveSyntaxError2) = f_labpat labpat
		   val tv   = T.freshTypeVar ()
		   val rv   = T.freshFieldVar ()
		   val c1   = E.initTypeConstraint (T.consTYPE_VAR tv) (T.consTYPE_VAR tv1) lab
		   val c2   = E.initTypeConstraint (T.consTYPE_VAR tv) (T.consTYPE_VAR tv2) lab
		   val labs = L.cons lab (A.getlabelsLabIdTy labidty)
		   val llop = A.getlabstLabIdTy labidty
		   val cst  = case llop of
				 NONE => E.emptyConstraint
			       | SOME (l1, l2, s) =>
				 let val lv = T.freshLabelVar ()
				     val c1 = E.initLabelConstraint (T.LABEL_VAR lv) (T.LC (s, l1)) l1
				     val c2 = E.initFieldConstraint (T.FIELD_VAR rv) (T.FC (T.LABEL_VAR lv, T.consTYPE_VAR tv, l2)) l2
				 in E.consConstraint (l1, c1) (E.singleConstraint (l2, c2))
				 end
		   val lop' = case llop of NONE => NONE | SOME (_, l, s) => SOME (s, l)
		   val cst  = E.conscsts (lab, [c1, c2]) (E.unionConstraintsList [cst1, cst2, cst])
		   val contextSensitiveSyntaxError  = E.unionContextSensitiveSyntaxErrors [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2]
	       in ((lop', SOME rv, NONE), E.unionEnvList [E.toRECValueIds vids1 labs, vids2], cst, contextSensitiveSyntaxError)
	       end
	     | f_patfield (A.PatFieldWild (r, l, n)) = ((NONE, NONE, SOME l), E.emvar, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	     | f_patfield (A.PatFieldDots pl) =
	       let val env = f_partlist pl
	       in ((NONE, NONE, NONE), E.getValueIds env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.typeVar, Env.varenv, Env.cst, Env.css) *)
	   and f_atpat (A.AtPatWild _) = (T.freshTypeVar (), E.emvar, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	     | f_atpat (A.AtPatId longid) = f_longidpat longid
	     | f_atpat (A.AtPatScon scon) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.AtPatScon")
		   val (tv, cst, contextSensitiveSyntaxError) = f_scon scon
		   val contextSensitiveSyntaxError' = case A.isPatScon scon of
				  NONE     => E.emptyContextSensitiveSyntaxError
				| SOME lab => E.singcss (E.CSSREAL (L.singleton lab))
	       in (tv, E.emvar, cst, E.unionContextSensitiveSyntaxErrors [contextSensitiveSyntaxError, contextSensitiveSyntaxError'])
	       end
	     | f_atpat (A.AtPatTuple (labpats, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.AtPatTuple")
		   val tv   = T.freshTypeVar ()
		   val (tvs, vidss, csts, csss) = unzipFour (map f_labpat labpats)
		   val vids = E.unionEnvList    vidss
		   val cst  = E.unionConstraintsList csts
		   val contextSensitiveSyntaxError  = E.unionContextSensitiveSyntaxErrors csss
		   val c    = E.initTypeConstraint (T.consTYPE_VAR tv) (T.constytuple tvs lab) lab
	       in (tv, vids, E.consConstraint (lab, c) cst, contextSensitiveSyntaxError)
	       end
	     | f_atpat (A.AtPatRecord (patfields, _, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.AtPatRecord")
		   val tv   = T.freshTypeVar ()
		   val (xs, vidss, csts, csss) = unzipFour (map f_patfield patfields)
		   val (lops, rvs, flexs) = unzipThree xs
		   val vids = E.unionEnvList vidss
		   val cst  = E.unionConstraintsList csts
		   val contextSensitiveSyntaxError  = E.unionContextSensitiveSyntaxErrors csss
		   val contextSensitiveSyntaxError' = consCSMlab (List.mapPartial (fn x => x) lops)
		   val rvs' = List.mapPartial (fn x => x) rvs
		   val flex = case List.mapPartial (fn x => x) flexs of
				  []  => T.noflex ()
				| [x] => T.consflex x
				| _   => raise EH.DeadBranch "Only one flex per record"
		   val c    = E.initTypeConstraint (T.consTYPE_VAR tv) (T.constyrecord rvs' flex lab) lab
	       in (tv, vids, E.consConstraint (lab, c) cst, E.unionContextSensitiveSyntaxErrors [contextSensitiveSyntaxError, contextSensitiveSyntaxError'])
	       end
	     | f_atpat (A.AtPatParen (labpat, _, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.AtPatParan")
		   val (tv, vids, cst, contextSensitiveSyntaxError) = f_labpat labpat
		   val tv' = T.freshTypeVar ()
		   val c   = E.initTypeConstraint (T.consTYPE_VAR tv') (T.consTYPE_VAR tv) lab
	       in (tv', vids, E.consConstraint (lab, c) cst, contextSensitiveSyntaxError)
	       end
	     | f_atpat (A.AtPatList (labpats, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.AtPAtList")
		   val tv   = T.freshTypeVar ()
		   val tv'  = T.freshTypeVar ()
		   val (tvs, vidss, csts, csss) = unzipFour (map f_labpat labpats)
		   val vids = E.unionEnvList vidss
		   val cst  = E.unionConstraintsList csts
		   val contextSensitiveSyntaxError  = E.unionContextSensitiveSyntaxErrors csss
		   val cs   = map (fn x => E.initTypeConstraint (T.consTYPE_VAR tv') (T.consTYPE_VAR x) lab) tvs
		   val c    = E.initTypeConstraint (T.consTYPE_VAR tv) (T.constylist tv' lab) lab
	       in (tv, vids, E.conscsts (lab, c :: cs) cst, contextSensitiveSyntaxError)
	       end
	     | f_atpat (A.AtPatOr (labpats, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.AtPatOr")
		   val (tvs, vidss, csts, csss) = unzipFour (map f_labpat labpats)
		   val vids = E.unionEnvList vidss (* do something else here: treatAtPatOr envs *)
		   val cst  = E.unionConstraintsList csts
		   val contextSensitiveSyntaxError  = E.unionContextSensitiveSyntaxErrors csss
		   val tv   = T.freshTypeVar ()
		   val cs   = map (fn x => E.initTypeConstraint (T.consTYPE_VAR tv) (T.consTYPE_VAR x) lab) tvs
	       in (tv, vids, E.conscsts (lab, cs) cst, contextSensitiveSyntaxError)
	       end
	     | f_atpat (A.AtPatDots pl) =
	       let val env = f_partlist pl
		   val tv  = T.freshTypeVar ()
	       in (tv, E.getValueIds env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.typeVar, Env.varenv, Env.cst, Env.css) *)
	   and f_pat (A.PatAtPat atpat) = f_atpat atpat
	     | f_pat (A.PatApp (longid, atpat, _, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.PatApp")
		   val (tv1, vids1, cst1, contextSensitiveSyntaxError1) = f_atpat atpat
		   val (tv2, lab', cl, cst2) = f_longidexp longid
		   val tv  = T.freshTypeVar ()
		   val c1  = E.initTypeConstraint (T.consTYPE_VAR tv2) (T.constyarrow' tv1 tv lab T.PATTERN_CONS) lab
		   val c2  = E.initClassConstraint cl (CL.consCO1 ()) lab
		   val cst = if A.isLongIdent longid
			     then E.singleConstraint (lab', E.initClassConstraint cl (CL.consCON ()) lab')
			     else E.emptyConstraint
	       in (tv, vids1, E.conscsts (lab, [c1, c2]) (E.unionConstraintsList [cst1, cst2, cst]), contextSensitiveSyntaxError1)
	       end
	     | f_pat (A.PatConsList (v, labpat1, labpat2, reg, lab, next)) =
	       (D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.PatConsList");
	       if benv
	       then f_pat (A.PatOp ("::", v, labpat1, labpat2, reg, lab, next))
	       else let val (tv1, vids1, cst1, contextSensitiveSyntaxError1) = f_labpat labpat1
			val (tv2, vids2, cst2, contextSensitiveSyntaxError2) = f_labpat labpat2
			val tv  = T.freshTypeVar ()
			val c1  = E.initTypeConstraint (T.consTYPE_VAR tv) (T.constylist tv1 lab) lab
			val c2  = E.initTypeConstraint (T.consTYPE_VAR tv) (T.consTYPE_VAR tv2) lab
			val cst = E.conscsts (lab, [c1, c2]) (E.unionConstraintsList [cst1, cst2])
			val contextSensitiveSyntaxError = E.unionContextSensitiveSyntaxErrors [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2]
		    in (tv, E.unionEnvList [vids1, vids2], cst, contextSensitiveSyntaxError)
		    end)
	     | f_pat (A.PatOp (st, v, labpat1, labpat2, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.PatOp")
		   val (tv1, vids1, cst1, contextSensitiveSyntaxError1) = f_labpat labpat1
		   val (tv2, vids2, cst2, contextSensitiveSyntaxError2) = f_labpat labpat2
		   val ty  = T.newTYPE_VAR ()
		   val ti  = T.constytuple [tv1, tv2] lab
		   val tvo = T.freshTypeVar ()
		   val c   = E.initTypeConstraint ty (T.consTyArrowTy ti (T.consTYPE_VAR tvo) lab T.OTHER_CONS) lab
		   val a   = E.initValueIDAccessor (E.consAccId (I.ID (v, lab)) ty (CL.consDA1 ()) lab) lab
		   val cst = E.conscsts (lab, [c, E.ACCESSOR_CONSTRAINT a]) (E.unionConstraintsList [cst1, cst2])
		   val contextSensitiveSyntaxError = E.unionContextSensitiveSyntaxErrors [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2]
	       in (tvo, E.unionEnvList [vids1, vids2], cst, contextSensitiveSyntaxError)
	       end
	     | f_pat (A.PatTyped (labpat, labtyp, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.PatTyped")
		   val tv = T.freshTypeVar ()
		   val (tv1, vids, cst1, contextSensitiveSyntaxError1) = f_labpat labpat
		   val (tv2, eqtv, cst2, contextSensitiveSyntaxError2) = f_labtype labtyp
		   val c1  = E.initTypeConstraint (T.consTYPE_VAR tv) (T.consTYPE_VAR tv1) lab
		   val c2  = E.initTypeConstraint (T.consTYPE_VAR tv) (T.consTYPE_VAR tv2) lab
		   val cst = E.conscsts (lab, [c1, c2]) (E.unionConstraintsList [cst1, cst2])
		   val contextSensitiveSyntaxError = E.unionContextSensitiveSyntaxErrors [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2]
	       in (tv, vids, cst, contextSensitiveSyntaxError)
	       end
	     | f_pat (A.PatAs (labidty, labpat, _, lab, _)) = (* For this one and PatFieldAs we need to constrain the envs to a value variables. *)
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.PatAs")
		   val (tv1, vids1, cst1, contextSensitiveSyntaxError1) = f_labidty labidty
		   val (tv2, vids2, cst2, contextSensitiveSyntaxError2) = f_labpat labpat
		   (*val clv  = CL.newClassVar ()*)
		   val tv   = T.freshTypeVar ()
		   val c1   = E.initTypeConstraint (T.consTYPE_VAR tv) (T.consTYPE_VAR tv1) lab
		   val c2   = E.initTypeConstraint (T.consTYPE_VAR tv) (T.consTYPE_VAR tv2) lab
		   (*val c3   = E.initClassConstraint clv (CL.consPAT ()) lab*)
		   val labs = L.cons lab (A.getlabelsLabIdTy labidty)
		   val cst  = E.conscsts (lab, [c1, c2(*, c3*)]) (E.unionConstraintsList [cst1, cst2])
		   val contextSensitiveSyntaxError  = E.unionContextSensitiveSyntaxErrors [contextSensitiveSyntaxError1, contextSensitiveSyntaxError2]
		   (*val vids = E.toCLSValueIds vids1 clv labs*)
		   val vids = E.toPATValueIds vids1 labs
	       in (tv, E.unionEnvList [vids, vids2], cst, contextSensitiveSyntaxError)
	       end
	     | f_pat (A.PatDots pl) =
	       let val tv  = T.freshTypeVar ()
		   val env = f_partlist pl
	       in (tv, E.getValueIds env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.typeVar, Env.varenv, Env.cst, Env.css) *)
	   and f_conbind (A.ConBind (ident, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION "generating constraints for A.ConBind"
		   val (tv, cons, cst, css) = f_identpat ident
	       in (tv, E.toDA0ValueIds cons L.empty, cst, css)
	       end

	     (* f_conbind: A.ConBindOf
	      * this function is used when we see a constructor being defined with a type, such as in dataype constructors:
	      * datatype x = a of b | c of d
	      * When the above code fragment is fed to the analysis engine the below function (f_conbind(A.ConBindOf ...)) is called
	      * with one of the binding expressions (for example 'a of b' or 'c of d' in the above example) and produces constraints
	      * for that datatype constructor.
	      *)
	     | f_conbind (A.ConBindOf (labid, labtyp, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION "generating constraints for A.ConBindOf"

		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ((#green D.colors)^"left hand side of 'of'...")
		   (* this is the code to the left hand side of the 'of' (a constructor name) *)
		   val (tv1, cons, cst1, css1) = f_labid labid

		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ((#red D.colors)^"right hand side of 'of'...")
		   (* this is the code to the right hand side of the 'of' (a type) *)
		   val (tv2, eqtv, cst2, css2) = f_labtype labtyp

		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("printing constraints for left hand side of 'of'...\n"^(#green D.colors)^(E.printConstraints cst1))
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("printing constraints for right hand side of 'of'...\n"^(#green D.colors)^(E.printConstraints cst2))

		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("The constraints generated for the type of the datatype constructor are:\n"^(#red D.colors)^(E.printConstraints cst2))

		   val (lab1, lab2) = Option.getOpt (A.getLabelsIdLabId labid, (lab, lab))
		   val clv1 = CL.newClassVar ()
		   val clv2 = CL.newClassVar ()
		   val tv   = T.freshTypeVar ()

		   (* should I generate an equality constraint here? The labels are going to be wrong if I try to use the type constraint,
		    * beacuse it won't take the ''a or whatever into account. They would be very similar constraints, but the labels would
		    * indeed have to be different. Is the best way to do this to create an equality constraint here? Might just be! *)
		   (* ORIGINAL: *)
		   val eqtv = T.freshEqualityTypeVar()
		   val eqtv2 = T.freshEqualityTypeVar()
		   (* perhaps what should happen here is that we should use T.constyarrow' again but just have a different label set so that
		    * we get the right errors? Don't know how the unification process would have to be changed for that but would certainly
		    * have to deal with the solving of this new kind of constraint. Also, the labels that I need are going to need to come
		    * from both the left and right hand side of the 'of' expression which tv1 and tv2 currently represent respectively.
		    * Perhaps the return type of f_labid and f_labtype will have to be edited to return equality type variables as well?
		    * Is this something that should be done? *)
		   (* val equalityConstraint = E.initEqualityTypeConstraint (T.consEQUALITY_TYPE_VAR eqtv) (T.consTYPE_VAR tv1) lab *)
		   (* val equalityConstraint2 = E.initEqualityTypeConstraint (T.consEQUALITY_TYPE_VAR eqtv2) (T.consTYPE_VAR tv2) lab *)

		   val c1   = E.initTypeConstraint (T.consTYPE_VAR tv1) (T.constyarrow' tv2 tv lab (T.DECLARATION_CONS I.dummyId)) lab
		   val c2   = E.initClassConstraint clv2 (CL.consDA1 ()) lab
		   val c3   = E.initClassConstraint clv1 clv2 lab1
		   val c4   = E.initClassConstraint clv1 (CL.consDAT ()) lab2
		   val cst  = E.unionConstraintsList [cst1, cst2]
		   val css  = E.unionContextSensitiveSyntaxErrors [css1, css2]
		   val cst' = E.conscsts (lab, [c1, c2]) (E.consConstraint (lab1, c3) (E.consConstraint (lab2, c4) cst))
		   (*(2010-06-24)I don't believe labid's label is necessary here, but
		    * this is what the old implementation had recorded in the database. *)
		   (*val labs = L.cons lab (A.getLabelLabId labid)*)
		   (*val cons = E.toDA1ValueIds cons labs*)
	       (* We actually don't need labs to constrain cons to be a DAT. *)
	       in (tv, E.toCLSValueIds cons clv1 L.empty, cst', css)
	       end
	     | f_conbind (A.ConBindNoOf (ident, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION "generating constraints for A.ConBindNoOf"
		   val (tv, cons, cst, css) = f_identpat ident
		   val tv' = T.freshTypeVar ()
	       in (tv', E.toDATValueIds cons L.empty, cst, css)
	       end
	     | f_conbind (A.ConBindDots pl) =
	       let val env = f_partlist pl
		   val tv  = T.freshTypeVar ()
	       in (tv, E.getValueIds env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.typeVar list, Env.varenv, Env.cst, Env.css) *)
	   and f_conbindseq (A.ConBindSeq conbinds) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION "generating constraints for A.ConBindSeq"
		   val (tvs, conss, csts, csss) = unzipFour (map f_conbind conbinds)
		   val cons = E.unionEnvList conss
		   val cst  = E.unionConstraintsList csts
		   val css  = E.unionContextSensitiveSyntaxErrors csss
	       in (tvs, cons, cst, css)
	       end
	     | f_conbindseq (A.ConBindSeqDots pl) =
	       let val env = f_partlist pl
	       in ([], E.getValueIds env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.typeVar, Ty.tvenv, Env.cst) *)
	   and f_typevarbind (A.TypeVar (_, n, _, lab, _)) =
	       let val _   = D.printDebug 2 D.AZE ("in f_typevarbind - generating constraints for A.TypeVar (lab = "^Int.toString(L.toInt(lab))^")")
		   val tv1  = T.freshTypeVar ()
		   val tv2  = T.freshTypeVar ()
		   val tyvs = E.consSingleEnv (n, [E.consBindMono n (tv1, true) (CL.consTYVAR ()) lab])
		   val c    = E.initTypeConstraint (T.consTYPE_VAR tv1) (T.TYPE_VAR (tv2, SOME (n, lab), T.POLY, T.UNKNOWN)) lab
	       in (tv2, tyvs, E.singleConstraint (lab, c))
	       end
	     | f_typevarbind (A.EqualityTypeVar (_, n, _, lab, _)) =
	       let val _   = D.printDebug 2 D.AZE ("in f_typevarbind - generating constraints for A.EqualityTypeVar (f_typevarbind; lab = "^Int.toString(L.toInt(lab))^")")
		   val tv1  = T.freshTypeVar ()
		   val tv2  = T.freshTypeVar ()
		   val tyvs = E.consSingleEnv (n, [E.consBindMono n (tv1, true) (CL.consTYVAR ()) lab])
		   val c    = E.initTypeConstraint (T.consTYPE_VAR tv1) (T.TYPE_VAR (tv2, SOME (n, lab), T.POLY, T.EQUALITY_TYPE)) lab
	       in (tv2, tyvs, E.singleConstraint (lab, c))
	       end
	     | f_typevarbind A.TypeVarDots = (T.freshTypeVar (), E.emtv, E.emptyConstraint)

	   (* RETURNS: (Ty.typeVar, Ty.tvenv, Env.cst) *)
	   and f_labtypevarbind (A.LabTypeVar (typeVar, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.LabTypeVar")
		   val (tv, tyvs, cst) = f_typevarbind typeVar
		   val tv' = T.freshTypeVar ()
		   val c   = E.initTypeConstraint (T.consTYPE_VAR tv) (T.consTYPE_VAR tv') lab
	       in (tv', tyvs, E.consConstraint (lab, c) cst)
	       end
	     | f_labtypevarbind (A.LabTypeVarDots typeVars) =
	       (T.freshTypeVar (), E.emtv, E.emptyConstraint)

	   (* RETURNS: (Ty.rowVar, Ty.tvenv, Env.cst) *)
	   and f_typevarseq (A.TypeVarSeqOne (typeVar, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.TypeVarSeqOne")
		   val (tv, tyvs, cst) = f_typevarbind typeVar
		   val sv = T.freshRowVar ()
		   val sq = T.ROW_C (T.constuple [tv] lab, T.noflex (), lab)
		   val c  = E.initRowConstraint (T.ROW_VAR sv) sq lab
	       in (sv, tyvs, E.consConstraint (lab, c) cst)
	       end
	     | f_typevarseq (A.TypeVarSeqEm (_, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.TypeVarSeqEm")
		   val sv = T.freshRowVar ()
		   val sq = T.ROW_C ([], T.noflex (), lab)
		   val c  = E.initRowConstraint (T.ROW_VAR sv) sq lab
	       in (sv, E.emtv, E.singleConstraint (lab, c))
	       end
	     | f_typevarseq (A.TypeVarSeqSeq (typevars, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.TypeVarSeqSeq")
		   val (tvs, tyvss, csts) = unzipThree (map f_labtypevarbind typevars)
		   val tyvs = E.unionEnvList tyvss
		   val cst  = E.unionConstraintsList csts
		   val sv   = T.freshRowVar ()
		   val sq   = T.ROW_C (T.constuple tvs lab, T.noflex (), lab)
		   val c    = E.initRowConstraint (T.ROW_VAR sv) sq lab
	       in (sv, tyvs, E.consConstraint (lab, c) cst)
	       end
	     | f_typevarseq (A.TypeVarSeqDots typeVars) =
	       let val (_, tyvss, csts) = unzipThree (map f_typevarbind typeVars)
		   val tyvs = E.unionEnvList tyvss
		   val cst  = E.unionConstraintsList csts
		   val sv   = T.freshRowVar ()
	       in (sv, tyvs, cst)
	       end

	   (* RETURNS: (Ty.typenv, Ty.varenv * Id.idl option, Env.cst, Env.cst, Env.css) *)
	   and f_datbind (A.DatBind (datname, conbindseq, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION "generating constraints for A.DatBind"
		   val (tvs, cons, cst1, css1) = f_conbindseq conbindseq
		   val (s, v, tv, sv1, sv2, typeNameEnv, tyvs, cst2, css2) = f_datname datname
		   val typename   = if benv andalso getBasis () then getTypenameString s else T.freshTypename ()
		   val id   = Option.getOpt (Option.map (fn (i, _) => i) v, I.dummyId)
		   val lab' = Option.getOpt (Option.map (fn (_, l) => l) v, lab)
		   (* (2010-06-10)We do that so that for datatypes, the end points will not be
		    * the equal signs but the datatype names, in the case of constructor clashes.*)
		   val c1   = E.initRowConstraint (T.ROW_VAR sv1) (T.ROW_VAR sv2) lab
		   val c2   = E.initTypeConstraint (T.consTYPE_VAR tv) (T.TYPE_CONSTRUCTOR (T.NC (typename, T.DECLARATION_CONS id, lab'), T.ROW_VAR sv1, lab', T.UNKNOWN)) lab'
		   val c2'  = E.initTypeConstraint (T.consTYPE_VAR tv) (T.consTypenameVar lab) lab
		   (*(2010-06-21)c2' is similar to c2 but weaker.  It just constrains tv to be of the
		    * form T.TYPE_CONSTRUCTOR.*)
		   val c3   = E.LET_CONSTRAINT (E.ROW_ENV (E.projExplicitTypeVars tyvs, E.CONSTRAINT_ENV cst1))
		   val cs   = map (fn x => E.initTypeConstraint (T.consTYPE_VAR x) (T.consTYPE_VAR tv) lab) tvs
		   val cst  = E.conscsts (lab, [c1, c2']) (E.consConstraint (lab', c2) cst2)
		   val cst' = E.conscsts (lab, cs) (E.singleConstraint (L.dummyLab, c3))
		   val css3 = checkTypeVarInc (A.getTypeVarDatName datname) (A.getlabDatName datname) (A.getTypeVarConbindseq conbindseq)
		   val css  = E.unionContextSensitiveSyntaxErrors [css1, css2, css3]
	       in (E.toTYCONTypeNameEnv typeNameEnv E.emvar false (L.singleton lab), (cons, v), cst, cst', css)
	       end
	     | f_datbind (A.DatBindDots pl) =
	       let val env = f_partlist pl
	       in (E.getTypeNameEnv env, (E.getValueIds env, NONE), E.emptyConstraint, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.typenv, (Ty.varenv * Id.idl option) list, Env.cst, Env.css) *)
	   and f_datbindseq (A.DatBindSeq (datbinds, _, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION "generating constraints for A.DatBindSeq"
		   val (typeNameEnvs, conss, csts1, csts2, csss) = unzipFive (map f_datbind datbinds)
		   val typeNameEnv = E.unionEnvList typeNameEnvs
		   val cst1 = E.unionConstraintsList csts1
		   val cst2 = E.unionConstraintsList csts2
		   val css  = E.unionContextSensitiveSyntaxErrors csss
		   val css1 = consCSM typeNameEnv
		   val css2 = consCSM (E.unionEnvList (map (fn (cons, _) => cons) conss))
	       in (typeNameEnv, conss, cst1, cst2, E.unionContextSensitiveSyntaxErrors [css, css1, css2])
	       end
	     | f_datbindseq (A.DatBindSeqDots pl) =
	       let val env = f_partlist pl
	       in (E.getTypeNameEnv env, [(E.getValueIds env, NONE)], E.emptyConstraint, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.varenv, Env.cst, Env.cst, Env.css) *)
	   and f_valbindcore (A.ValBindCore (labpat, labexp, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.ValBindCore (lab = "^Int.toString(L.toInt(lab))^")")
		   val (tv1, vids, cst1, css1) = f_labpat labpat
		   val (tv2, cst2, css2) = f_labexp labexp
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION "**** constraints for A.ValBindCore - cst1 ***"
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ((#red D.colors)^(E.printConstraints cst1))
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION "**** constraints for A.ValBindCore - cst2 ***"
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ((#green D.colors)^(E.printConstraints cst2))
		   val vids' = E.closeValueIds vids (V.nonexpLabExp labexp)
		   val c     = E.initTypeConstraint (T.consTYPE_VAR tv1) (T.consTYPE_VAR tv2) lab
		   val cst   = E.consConstraint (lab, c) (cst2)
		   val css   = E.unionContextSensitiveSyntaxErrors [clearRebound css1, css2]
		   val _     = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("A.ValBindCore constraints generated (tv1="^Int.toString(T.typeVarToInt tv1)^", tv2="^Int.toString(T.typeVarToInt tv2)^")\n")
	       in (vids', cst1, cst, css)
	       end
	     | f_valbindcore (A.ValBindCoreDots pl) =
	       let val env = f_partlist pl
	       in (E.getValueIds env, E.emptyConstraint, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* f_valbindseq
	    * this function is called when a variable is defined, for example
	    * the code 'val x = 1.0' will cause this function to be called *)
	   and f_valbindseq (A.ValBindSeq (valbindcores, _, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.ValBindSeq (lab = UNKNOWN)")
		   val (vidss, csts1, csts2, csss) = unzipFour (map f_valbindcore valbindcores)
		   val vids = E.unionEnvList vidss
		   val cst1 = E.unionConstraintsList csts1
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION "**** constraints for A.ValBindSeq - cst1 ***"
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ((#red D.colors)^(E.printConstraints cst1))
		   val cst2 = E.unionConstraintsList csts2
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION "**** constraints for A.ValBindSeq - cst2 ***"
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ((#green D.colors)^(E.printConstraints cst2))
		   val css  = E.unionContextSensitiveSyntaxErrors csss
	       in (vids, cst1, cst2, css)
	       end
	     | f_valbindseq (A.ValBindSeqDots pl) =
	       let val env = f_partlist pl
	       in (E.getValueIds env, E.emptyConstraint, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_valbind (A.ValBindRec (valbindseq, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.ValBindRec (lab = "^Int.toString(L.toInt(lab))^")")
		   val (vids, cst1, cst2, css) = f_valbindseq valbindseq
		   val labs  = L.singleton lab
		   val vids' = E.toMonoValueIds (E.toRECValueIds vids labs) labs
		   val css1  = consCSM vids
		   val css2  = map (fn x => E.CSSFREC (L.cons lab x)) (A.isExpFnValBindSeq valbindseq)
		   val env   = E.ROW_ENV (E.CONSTRAINT_ENV cst1, E.ROW_ENV (E.projValueIds vids', E.CONSTRAINT_ENV cst2))
	       in (env, E.unionContextSensitiveSyntaxErrors [css, css1, css2])
	       end
	     | f_valbind (A.ValBind valbindseq) =
	       let val (vids, cst1, cst2, css) = f_valbindseq valbindseq
		   val env = E.ROW_ENV (E.CONSTRAINT_ENV (E.unionConstraintsList [cst1, cst2]), E.projValueIds vids)
	       in (env, css)
	       end
	     | f_valbind (A.ValBindDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.typeVar, Env.varenv, Env.cst, Env.css) *)
	   and f_labatpat (A.LabAtPat (atpat, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.LabAtPat")
		   val (tv, vids, cst, css) = f_atpat atpat
		   val tv' = T.freshTypeVar ()
		   val c   = E.initTypeConstraint (T.consTYPE_VAR tv) (T.consTYPE_VAR tv') lab
	       in (tv', vids, E.consConstraint (lab, c) cst, css)
	       end
	     | f_labatpat (A.LabAtPatDots pl) =
	       let val tv  = T.freshTypeVar ()
		   val env = f_partlist pl
	       in (tv, E.getValueIds env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.typeVar, Env.varenv, Env.varenv, Env.cst, Env.css) *)
	   and f_fmatch (A.FMatchId (ident, _, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.FMatchId")
		   val (tv, vids, cst, css) = f_identpat ident
	       in (tv, vids, E.emvar, cst, css)
	       end
	     | f_fmatch (A.FMatchApp (fmatch, labatpat, _, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.FMatchApp")
		   val (tv1, vids, vids1, cst1, css1) = f_fmatch fmatch
		   val (tv2, vids2, cst2, css2) = f_labatpat labatpat
		   val tv  = T.freshTypeVar ()
		   val c   = E.initTypeConstraint (T.consTYPE_VAR tv1) (T.constyarrow tv2 tv lab) lab
		   val cst = E.consConstraint (lab, c) (E.unionConstraintsList [cst1, cst2])
		   val css = E.unionContextSensitiveSyntaxErrors [css1, clearRebound css2]
	       in (tv, vids, E.unionEnvList [vids1, vids2], cst, css)
	       end
	     | f_fmatch (A.FMatchSlApp (fmatch, labatpat, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.FMatchSlApp")
		   val (tv1, vids, vids1, cst1, css1) = f_fmatch fmatch
		   val (tv2, vids2, cst2, css2) = f_labatpat labatpat
		   val tv  = T.freshTypeVar ()
		   val cst = E.unionConstraintsList [cst1, cst2]
		   val css = E.unionContextSensitiveSyntaxErrors [css1, clearRebound css2]
	       in (tv, vids, E.unionEnvList [vids1, vids2], cst, css)
	       end
	     | f_fmatch (A.FMatchNoApp (fmatch, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.FMatchNoApp")
		   val (_, vids, vids1, cst1, css1) = f_fmatch fmatch
		   val tv = T.freshTypeVar ()
	       in (tv, vids, vids1, cst1, css1)
	       end
	     | f_fmatch A.FMatchDots =
	       let val tv = T.freshTypeVar ()
	       in (tv, E.emvar, E.emvar, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.typeVar, Env.varenv, Env.varenv, Env.cst, Env.css) *)
	   and f_labfmatch (A.LabFMatch (fmatch, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.LabFMatch")
		   val (tv, vids, vids', cst, css) = f_fmatch fmatch
		   val tv' = T.freshTypeVar ()
		   val c   = E.initTypeConstraint (T.consTYPE_VAR tv) (T.consTYPE_VAR tv') lab
	       in (tv', vids, vids', E.consConstraint (lab, c) cst, css)
	       end
	     | f_labfmatch (A.LabFMatchSl (fmatch, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.LabFMatchSl")
		   val (tv, vids, vids', cst, css) = f_fmatch fmatch
		   val tv' = T.freshTypeVar ()
	       in (tv', vids, vids', cst, css)
	       end
	     | f_labfmatch A.LabFMatchDots =
	       let val tv = T.freshTypeVar ()
	       in (tv, E.emvar, E.emvar, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.typeVar, Env.varenv, Env.varenv, Env.cst, Env.css) *)
	   and f_fmatchty (A.FMatchT fmatch) = f_labfmatch fmatch
	     | f_fmatchty (A.FMatchTTy (fmatch, labtyp, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.FMatchTTy")
		   val (tv1, vids1, vids1', cst1, css1) = f_labfmatch fmatch
		   val (tv2, eqtv, cst2, css2) = f_labtype labtyp
		   val tv  = T.freshTypeVar ()
		   val c1  = E.initTypeConstraint (T.consTYPE_VAR tv) (T.consTYPE_VAR tv1) lab
		   val c2  = E.initTypeConstraint (T.consTYPE_VAR tv) (T.consTYPE_VAR tv2) lab
		   val cst = E.conscsts (lab, [c1, c2]) (E.unionConstraintsList [cst1, cst2])
		   val css = E.unionContextSensitiveSyntaxErrors [css1, css2]
	       in (tv, vids1, vids1', cst, css)
	       end
	     | f_fmatchty A.FMatchTDots =
	       let val tv = T.freshTypeVar ()
	       in (tv, E.emvar, E.emvar, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.varenv, Env.cst, Env.css) *)
	   and f_fvalbindcore (A.FValBindCore (fmatch, labexp, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.FValBindCore")
		   val (tv1, vids, vids', cst1, css1) = f_fmatchty fmatch
		   val (tv2, cst2, css2) = f_labexp labexp
		   val labs = L.singleton lab
		   val c1   = E.initTypeConstraint (T.consTYPE_VAR tv1) (T.consTYPE_VAR tv2) lab
		   val env  = E.ROW_ENV (E.CONSTRAINT_ENV cst1, E.projValueIds (E.toMonoValueIds vids' L.empty))
		   val c2   = E.LET_CONSTRAINT (E.ROW_ENV (env, E.CONSTRAINT_ENV cst2))
		   val cst  = E.consConstraint (lab, c1) (E.singleConstraint (L.dummyLab, c2))
		   val css  = E.unionContextSensitiveSyntaxErrors [css1, css2]
		   val vids = E.toRECValueIds (E.toMonoValueIds vids L.empty) L.empty
	       (*(2010-06-11)NOTE: We've got a lot of L.empty here!?*)
	       in (vids, cst, css)
	       end
	     | f_fvalbindcore (A.FVBCoreDots pl) =
	       let val env = f_partlist pl
	       in (E.getValueIds env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.varenv, Env.cst, Env.css) *)
	   and f_fvalbindone (A.FValBindOne (fvalbindcores, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.FValBindOne")
		   val (vidss, csts, csss) = unzipThree (map f_fvalbindcore fvalbindcores)
		   val vids = E.unionEnvList vidss
		   val cst  = E.unionConstraintsList csts
		   val css  = E.unionContextSensitiveSyntaxErrors csss
		   val cst' = E.allEqualValueIds vids
		   val css' = sameNameFun vids
	       in (vids, E.unionConstraintsList [cst, cst'], E.unionContextSensitiveSyntaxErrors [css, css'])
	       end
	     | f_fvalbindone (A.FVBOneDots pl) =
	       let val env = f_partlist pl
	       in (E.getValueIds env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_fvalbind (A.FValBind (fvalbindones, _, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.FValBind")
		   val (vidss, csts, csss) = unzipThree (map f_fvalbindone fvalbindones)
		   val vids = E.unionEnvList vidss
		   val cst  = E.unionConstraintsList csts
		   val css  = E.unionContextSensitiveSyntaxErrors csss
		   val env  = E.ROW_ENV (E.projValueIds vids, E.CONSTRAINT_ENV cst)
		   val css1 = createDiffNbArgFuns fvalbindones
		   val css2 = consCSMfval vidss
	       in (env, E.unionContextSensitiveSyntaxErrors [css, css1, css2])
	       end
	     | f_fvalbind (A.FValBindDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emptyContextSensitiveSyntaxError)
	       end

	   (* f_tyconbind: For type constructors. For example,
	    *
	    * datatype Operators = ADD | IF | LESS_THAN
	    * type mytype = int
	    *
	    * str is the name of the type constructor, eg
	    * 1. 'Operators' 'datatype Operators = ADD of int*int | IF | LESS_THAN'
	    * 2. 'mytype' in 'type mytype = int'
	    *)
	   and f_tyconbind (A.TyCon (str, id, _, lab, _)) =
	       let
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.TyCon (str=\""^str^"\")")
		   (* generate new type variable, row variable and type function variable *)
		   val freshTypeVar   = T.freshTypeVar  ()
		   val freshRowVar   = T.freshRowVar ()
 		   val typeFunctionVar  = T.freshTypeFunctionVar ()

		   (* constructs an environment from the id of the type constructor typename to a polymorphic binding *
		    * the type of the binding will be (T.TYPEFUNCTIONVAR, E.typeNameKind, ref (SplayMapFn (OrdId).map, bool)) *)
		   val typeNameEnv = E.consSingleEnv (id,
					       [E.consBindPoly
						    {id = id,
						     typeOfId=(T.TYPE_FUNCTION_VAR typeFunctionVar,
							       E.TYPE,
							       ref (E.emvar, false)),
						    classOfId=(CL.consTYCON ()), (* class is a type constructor *)
						    labelOfConstraint=lab}])      (* label of the constraint *)
		   (*(2010-06-10)NOTE: the false abaove is because we still don't know v's constructors.*)
		   val c    = E.initFunctionTypeConstraint (T.TYPE_FUNCTION_VAR typeFunctionVar) (T.TFC (T.ROW_VAR freshRowVar, T.consTYPE_VAR freshTypeVar, lab)) lab
	       in
		   (str, SOME (id, lab), freshTypeVar, freshRowVar, typeNameEnv, E.singleConstraint (lab, c))
	       end
	     | f_tyconbind A.TyConDots =
	       let val tv = T.freshTypeVar  ()
		   val sv = T.freshRowVar ()
	       in ("", NONE, tv, sv, E.emtyp, E.emptyConstraint)
	       end

	   (* RETURNS: (string, Id.id option, Ty.typeVar, Ty.rowVar, Ty.rowVar, Env.typenv, Env.tvenv, Env.cst, Env.css) *)
	   and f_datname (A.DatName (typvarseq, tycon, _, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION "generating constraints for A.DatName"
		   val (sv1, tyvs, cst1) = f_typevarseq typvarseq

		   (* call f_tyconbind, which deals with the new binding of type constructors *)
		   val (str, idLabelPair, tv2, sv2, typeNameEnv, cst2) = f_tyconbind tycon
		   val css = consCSM tyvs
	       (* NOTE: sv1 and sv2 have to be equal *)
	       in
		   (* v is an option (Option.map used on it in f_typdescone *)
		   (str, idLabelPair, tv2, sv2, sv1, typeNameEnv, tyvs, E.unionConstraintsList [cst1, cst2], css)
	       end
	     | f_datname A.DatNameDots =
	       let val tv  = T.freshTypeVar ()
		   val sv1 = T.freshRowVar ()
		   val sv2 = T.freshRowVar ()
	       in ("", NONE, tv, sv1, sv2, E.emtyp, E.emtv, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.typenv, Env.cst, Env.css) *)
	   and f_typbind (A.TypBind (datname, labtyp, _, lab, _)) =
	       let
		   val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION "generating constraints for A.TypBind"
		   val (_, _, tv1, sv1, sv1', typeNameEnv, tyvs, cst1, css1) = f_datname datname
		   val (tv2, eqtv, cst2, css2) = f_labtype labtyp
		   val c1  = E.initTypeConstraint (T.consTYPE_VAR tv1) (T.consTYPE_VAR tv2) lab
		   val c2  = E.initRowConstraint (T.ROW_VAR sv1) (T.ROW_VAR sv1') lab
		   val c3  = E.LET_CONSTRAINT (E.ROW_ENV (E.projExplicitTypeVars tyvs, E.CONSTRAINT_ENV cst2))
		   val cst = E.conscsts (lab, [c1, c2]) (E.consConstraint (L.dummyLab, c3) cst1)
		   val css = E.unionContextSensitiveSyntaxErrors [css1, css2]
	       in (typeNameEnv, cst, css)
	       end
	     | f_typbind (A.TypBindDots pl) =
	       let val env = f_partlist pl
	       in (E.getTypeNameEnv env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.typenv, Env.cst, Env.css) *)
	   and f_typbindseq (A.TypBindSeq (typbinds, _, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION "generating constraints for A.TypBindSeq"
		   val (typeNameEnvs, csts, csss) = unzipThree (map f_typbind typbinds)
		   val typeNameEnv = E.unionEnvList typeNameEnvs
		   val cst  = E.unionConstraintsList csts
		   val css  = E.unionContextSensitiveSyntaxErrors csss
		   val css1 = consCSM typeNameEnv
	       in (typeNameEnv, cst, E.unionContextSensitiveSyntaxErrors [css, css1])
	       end
	     | f_typbindseq (A.TypBindSeqDots pl) =
	       let val env = f_partlist pl
	       in (E.getTypeNameEnv env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.varenv, Env.cst, Env.css) *)
	   and f_exbind (A.ExBind (ident, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.ExBind")
		   val (tv, cons, cst, css) = f_identpat ident
		   val c = E.initTypeConstraint (T.consTYPE_VAR tv) (T.constyexception' lab (T.DECLARATION_CONS I.dummyId)) lab
	       in (E.toEX0ValueIds cons (L.singleton lab), E.consConstraint(lab, c) cst, css)
	       end
	     | f_exbind (A.ExBindOf (labid, labtyp, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.ExBindOf")
		   val (tv1, eqtv, cst1, css1) = f_labtype labtyp
		   val (tv2, cons, cst2, css2) = f_labid labid
		   val (lab1, lab2) = Option.getOpt (A.getLabelsIdLabId labid, (lab, lab))
		   val clv1 = CL.newClassVar ()
		   val clv2 = CL.newClassVar ()
		   val ty   = T.constyexception' lab (T.DECLARATION_CONS I.dummyId)
		   val c1   = E.initTypeConstraint (T.consTYPE_VAR tv2) (T.consTyArrowTy (T.consTYPE_VAR tv1) ty lab (T.DECLARATION_CONS I.dummyId)) lab
		   val c2   = E.initClassConstraint clv2 (CL.consEX1 ()) lab
		   val c3   = E.initClassConstraint clv1 clv2 lab1
		   val c4   = E.initClassConstraint clv1 (CL.consEXC ()) lab2
		   val cst  = E.unionConstraintsList [cst1, cst2]
		   val css  = E.unionContextSensitiveSyntaxErrors [css1, css2]
		   val cst' = E.conscsts (lab, [c1, c2]) (E.consConstraint(lab1, c3) (E.consConstraint(lab2, c4) cst))
		   (*(2010-06-24)Same remark for labs as in f_conbind.*)
		   (*val labs = L.cons lab (A.getLabelLabId labid)*)
		   (*val cons = E.toEX1ValueIds cons labs*)
	       (* We actually don't need labs to constrain cons to be MONO and EXC.
		* The whole binding shouldn't be constrained by labs but only the status.
		* And the we should pass 71 and 73 :D *)
	       in (E.toCLSValueIds (E.toMonoValueIds cons L.empty) clv1 L.empty, cst', css)
	       end
	     | f_exbind (A.ExBindEq (labid, longid, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.ExBindEq")
		   val (tv1, cons, cst1, css1) = f_labid labid
		   val (tv2, _, cl, cst2) = f_longidexp longid
		   val clv  = CL.newClassVar ()
		   val c1   = E.initTypeConstraint (T.consTYPE_VAR tv2) (T.consTYPE_VAR tv1) lab
		   val c2   = E.initClassConstraint cl clv lab
		   val c3   = E.initClassConstraint cl (CL.consEXC ()) lab
		   (* NOTE: the class could be a EX0 or a EX1 - a EXC *)
		   val cst  = E.conscsts (lab, [c1, c2]) (E.unionConstraintsList [cst1, cst2])
		   (* NOTE: we build c like that so that the accessor in cst2 is resolved before c3
		    * and we bind the class cl (and clv) to the correct EX0 or EX1 before checking
		    * that it is a EXC. *)
		   val c    = E.LET_CONSTRAINT (E.ROW_ENV (E.CONSTRAINT_ENV cst, E.CONSTRAINT_ENV (E.singleConstraint (lab, c3))))
		   val labs = L.singleton lab
	       in (E.toCLSValueIds (E.toMonoValueIds cons labs) clv labs, E.singleConstraint (L.dummyLab, c), css1)
	       end
	     | f_exbind (A.ExBindNo (ident, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.ExBindNo")
		   val (tv, cons, cst, css) = f_identpat ident
	       in (E.toEX0ValueIds cons L.empty, cst, css)
	       end
	     | f_exbind (A.ExBindDots pl) =
	       let val env = f_partlist pl
	       in (E.getValueIds env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.varenv, Env.cst, Env.css) *)
	   and f_exbindseq (A.ExBindSeq (exbinds, _, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.EqBindSeq")
		   val (conss, csts, csss) = unzipThree (map f_exbind exbinds)
		   val cons = E.unionEnvList conss
		   val cst  = E.unionConstraintsList csts
		   val css  = E.unionContextSensitiveSyntaxErrors csss
		   val css1 = consCSM cons
	       in (cons, cst, E.unionContextSensitiveSyntaxErrors [css, css1])
	       end
	     | f_exbindseq (A.ExBindSeqDots pl) =
	       let val env = f_partlist pl
	       in (E.getValueIds env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Id.lid list) *)
	   and f_longstrseq (A.LongStrSeq (ids, _)) =
	       List.mapPartial (fn x => A.longstridToLid x) ids
	     | f_longstrseq (A.LongStrSeqDots pl) = []

	   (* RETURNS: (Ty.rowVar, Env.ocenv, Env.cst) *)
	   and f_classbind (A.Class (s, v, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.Class (f_classbind function)")
		   val sv1  = T.freshRowVar ()
		   val sv2  = T.freshRowVar ()
		   val c    = E.initRowConstraint (T.ROW_VAR sv1) (T.ROW_VAR sv2) lab
		   val ovcs = E.consSingleEnv (v, [E.consBindPoly {id=v,
								   typeOfId=(T.ROW_VAR sv2),
								   classOfId=(CL.consOC ()),
								   labelOfConstraint=lab}])
	       in (sv1, ovcs, E.singleConstraint (lab, c))
	       end
	     | f_classbind A.ClassDots =
	       let val sv = T.freshRowVar ()
	       in (sv, E.emoc, E.emptyConstraint)
	       end

	   (* RETURNS: (Ty.rowVar, Env.ocenv, Env.cst) *)
	   and f_labclassbind (A.LabClass (class, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.LabClass (f_labclassbind function)")
		   val (sv, ovcs, cst) = f_classbind class
		   val sv' = T.freshRowVar ()
		   val c   = E.initRowConstraint (T.ROW_VAR sv') (T.ROW_VAR sv) lab
	       in (sv', ovcs, E.consConstraint(lab, c) cst)
	       end
	     | f_labclassbind (A.LabClassDots pl) =
	       let val env = f_partlist pl
	       in (T.freshRowVar (), E.emoc, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env))
	       end

	   (* RETURNS: (Ty.rowVar, Env.cst) *)
	   and f_class (A.Class (s, v, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.Class (f_class function)")
		   val sv = T.freshRowVar ()
		   val a  = E.genAccIoEm (E.consAccId (I.ID (v, lab)) (T.ROW_VAR sv) (CL.consOC ()) lab) lab
	       in (sv, E.singleConstraint (lab, E.ACCESSOR_CONSTRAINT a))
	       end
	     | f_class A.ClassDots = (T.freshRowVar (), E.emptyConstraint)

	   (* RETURNS: (Ty.rowVar, Env.cst) *)
	   and f_labclass (A.LabClass (class, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.LabClass (f_labclass function)")
		   val (sv, cst) = f_class class
		   val sv' = T.freshRowVar ()
		   val c   = E.initRowConstraint (T.ROW_VAR sv') (T.ROW_VAR sv) lab
	       in (sv', E.consConstraint(lab, c) cst)
	       end
	     | f_labclass (A.LabClassDots pl) =
	       let val env = f_partlist pl
	       in (T.freshRowVar (), E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env))
	       end

	   (* RETURNS: (Ty.typeVar, Env.cst, Env.css) *)
	   and f_tyclass (A.TyClassCl (labclass, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.TyClassCl")
		   val (sv, cst) = f_labclass labclass
		   val tv  = T.freshTypeVar ()
		   val lid = Option.getOpt (A.getlabidLabclass labclass, (I.dummyId, L.dummyLab))
		   val c   = E.initTypeConstraint (T.consTYPE_VAR tv) (T.TYPE_POLY (T.ROW_VAR sv, T.freshidor (), T.POLY, T.VALUE lid, lab, T.NOT_EQUALITY_TYPE)) lab
	       in (tv, E.consConstraint(lab, c) cst, E.emptyContextSensitiveSyntaxError)
	       end
	     | f_tyclass (A.TyClassTy (typ, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.TyClassTy")
		   val (tv, eqTypeVar, cst, css) = f_type typ
		   val tv' = T.freshTypeVar ()
		   val c   = E.initTypeConstraint (T.consTYPE_VAR tv') (T.consTYPE_VAR tv) lab
	       in (tv', E.consConstraint(lab, c) cst, css)
	       end
	     | f_tyclass (A.TyClassDots pl) =
	       let val env = f_partlist pl
		   val tv  = T.freshTypeVar ()
	       in (tv, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.rowVar, Env.cst, Env.css) *)
	   and f_labtyclass (A.LabTyClass (tyclass, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.LabTyClass")
		   val (tv, cst, css) = f_tyclass tyclass
		   val tv' = T.freshTypeVar ()
		   val c   = E.initTypeConstraint (T.consTYPE_VAR tv') (T.consTYPE_VAR tv) lab
	       in (tv', E.consConstraint(lab, c) cst, css)
	       end
	     | f_labtyclass (A.LabTyClassDots pl) =
	       let val env = f_partlist pl
	       in (T.freshTypeVar (), E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.rowVar, Env.cst, Env.css) *)
	   and f_tyclassseq (A.TyClassSeqOne (tyclass, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.TyClassSeqOne")
		   val (tv, cst, css) = f_tyclass tyclass
		   val sv = T.freshRowVar ()
		   val c  = E.initRowConstraint (T.ROW_VAR sv) (T.ROW_C (T.constuple [tv] lab, T.noflex (), lab)) lab
	       in (sv, E.consConstraint(lab, c) cst, E.emptyContextSensitiveSyntaxError)
	       end
	     | f_tyclassseq (A.TyClassSeqEm (_, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.TyClassSeqEm")
		   val sv = T.freshRowVar ()
		   val c  = E.initRowConstraint (T.ROW_VAR sv) (T.ROW_C ([], T.noflex (), lab)) lab
	       in (sv, E.singleConstraint (lab, c), E.emptyContextSensitiveSyntaxError)
	       end
	     | f_tyclassseq (A.TyClassSeqSeq (labtyclasss, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.TyClassSeqSeq")
		   val (tvs, csts, csss) = unzipThree (map f_labtyclass labtyclasss)
		   val sv  = T.freshRowVar ()
		   val cst = E.unionConstraintsList csts
		   val css = E.unionContextSensitiveSyntaxErrors csss
		   val c   = E.initRowConstraint (T.ROW_VAR sv) (T.ROW_C (T.constuple tvs lab, T.noflex (), lab)) lab
	       in (sv, E.consConstraint(lab, c) cst, css)
	       end
	     | f_tyclassseq (A.TyClassSeqDots pl) =
	       let val env = f_partlist pl
		   val sv  = T.freshRowVar ()
	       in (sv, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   and f_typevarval (A.TypeVar (_, n, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.TypeVar")
		   val _   = D.printDebug 2 D.AZE ("in f_typevarval - generating constraints for A.TypeVar (lab = "^Int.toString(L.toInt(lab))^")")
		   val tv   = T.freshTypeVar ()
		   val tyvs = E.consSingleEnv (n, [E.consBindMono n (tv, false) (CL.consTYVAR ()) lab])
		   val c    = E.initTypeConstraint (T.consTYPE_VAR tv) (T.EXPLICIT_TYPE_VAR (n, T.freshTypeVar (), lab, T.UNKNOWN)) lab
	       in (tyvs, E.singleConstraint (lab, c))
	       end
	     | f_typevarval (A.EqualityTypeVar (_, n, _, lab, _)) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.EqualityTypeVar (f_typevarval)")
		   val _   = D.printDebug 2 D.AZE ("in f_typevarval - generating constraints for A.TypeVar (lab = "^Int.toString(L.toInt(lab))^")")
		   val tv   = T.freshTypeVar ()
		   val tyvs = E.consSingleEnv (n, [E.consBindMono n (tv, false) (CL.consTYVAR ()) lab])
		   val c    = E.initTypeConstraint (T.consTYPE_VAR tv) (T.EXPLICIT_TYPE_VAR (n, T.freshTypeVar (), lab, T.EQUALITY_TYPE)) lab
	       in (tyvs, E.singleConstraint (lab, c))
	       end
	     | f_typevarval A.TypeVarDots = (E.emtv, E.emptyConstraint)

	   (* RETURNS: (Env.tvenv, Env.cst) *)
	   and f_typevarvallist [] = (E.emtv, E.emptyConstraint)
	     | f_typevarvallist [typevar] = f_typevarval typevar
	     | f_typevarvallist (typevar :: typevars) =
	       let val (tyvs1, cst1) = f_typevarval typevar
		   val (tyvs2, cst2) = f_typevarvallist typevars
	       in (E.unionEnvList [tyvs1, tyvs2], E.unionConstraintsList [cst1, cst2])
	       end

	   (* RETURNS: (Env.tvenv, Env.cst) *)
	   and f_labtypevarval (A.LabTypeVar (typeVar, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.LabTypeVar")
		   val (tyvs, cst) = f_typevarval typeVar
	       (* NOTE: the label is not used. *)
	       in (tyvs, cst)
	       end
	     | f_labtypevarval (A.LabTypeVarDots typeVars) = (E.emtv, E.emptyConstraint)

	   (* RETURNS: (Env.tvenv, Env.cst) *)
	   and f_typeVarseqval (A.TypeVarSeqOne (typeVar, _, lab, _)) = f_typevarval typeVar
	     | f_typeVarseqval (A.TypeVarSeqEm (_, lab, _)) = (E.emtv, E.emptyConstraint)
	     | f_typeVarseqval (A.TypeVarSeqSeq (typevars, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.TypeVarSeqSeq (f_typeVarseqval function)")
		   val (tyvss, csts) = ListPair.unzip (map f_labtypevarval typevars)
		   val tyvs = E.unionEnvList tyvss
		   val cst  = E.unionConstraintsList csts
	       in (tyvs, cst)
	       end
	     | f_typeVarseqval (A.TypeVarSeqDots typeVars) =
	       let val (tyvss, csts) = ListPair.unzip (map f_typevarval typeVars)
		   val tyvs = E.unionEnvList tyvss
		   val cst  = E.unionConstraintsList csts
	       in (tyvs, cst)
	       end

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_dec (A.DecVal (typeVarseq, valbind, _, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.DecVal")
		   val (env, css) = f_valbind valbind
		   val (tyvs1, cst1) = f_typeVarseqval typeVarseq
		   val (tyvs2, cst2) = f_typevarvallist (A.getTypeVarValBind valbind)
		   (*val (idV, cs') = closeTypedTerm (A.gettypeVarValBind vb)
						   (A.gettypeVarTypeVarSeq tvsq)
						   (E.getExplicitTypeVars env)
						   cs
						   (E.unionEnvList [idG, idR])*)
		   val tyvs = E.plusenv tyvs2 tyvs1
		   val cst  = E.unionConstraintsList [cst1, cst2]
		   val env' = E.ROW_ENV (E.CONSTRAINT_ENV cst, E.ENVPOL (tyvs, env))
		   val _  = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION "finished generating constraints for A.DecVal"
	       in (env', css)
	       end
	     | f_dec (A.DecFVal (typeVarseq, fvalbind, _, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.DecFVal")
		   val (env, css) = f_fvalbind fvalbind
		   val (tyvs1, cst1) = f_typeVarseqval typeVarseq
		   val (tyvs2, cst2) = f_typevarvallist (A.getTypeVarFValBind fvalbind)
		   val tyvs = E.plusenv tyvs2 tyvs1
		   val cst  = E.unionConstraintsList [cst1, cst2]
		   val env' = E.ROW_ENV (E.CONSTRAINT_ENV cst, E.ENVPOL (tyvs, env))
	       in (env', css)
	       end
	     | f_dec (A.DecDatType (datbinds, _, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.DecDatType")
		   val (typeNameEnv, conss, cst1, cst2, css) = f_datbindseq datbinds
		   val envs = map (fn (cons, SOME idl) => E.DATATYPE_CONSTRUCTOR_ENV (idl, E.ENVPOL (E.emtv, E.projValueIds cons))
				    | (cons, NONE) => E.ENVPOL (E.emtv, E.projValueIds cons)) conss
		   val env1 = E.ROW_ENV (E.CONSTRAINT_ENV cst2, E.envsToSeq envs)
		   val env2 = E.ROW_ENV (E.ROW_ENV (E.CONSTRAINT_ENV cst1, E.consEnvTypeNames typeNameEnv), env1)
	       in (env2, css)
	       end
	     | f_dec (A.DecDatWith (datbind, typbind, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.DecDatWith")
		   val (typeNameEnv1, conss, cst1, cst1', css1) = f_datbindseq datbind
		   val (typeNameEnv2, cst2, css2) = f_typbindseq typbind
		   val css  = E.unionContextSensitiveSyntaxErrors [css1, css2]
		   val envs = map (fn (cons, SOME idl) => E.DATATYPE_CONSTRUCTOR_ENV (idl, E.ENVPOL (E.emtv, E.projValueIds cons))
				    | (cons, NONE) => E.ENVPOL (E.emtv, E.projValueIds cons)) conss
		   val env1 = E.ROW_ENV (E.CONSTRAINT_ENV cst1', E.envsToSeq envs)
		   val env2 = E.ROW_ENV (E.CONSTRAINT_ENV cst1, E.consEnvTypeNames typeNameEnv1)
		   val env3 = E.ROW_ENV (E.CONSTRAINT_ENV cst2, E.consEnvTypeNames typeNameEnv2)
		   val env4 = E.ROW_ENV (env2, E.ROW_ENV (env3, env1))
	       in (env4, css)
	       end
	     | f_dec (A.DecDatRep (tycon, longtycon, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.DecDatRep")
		   val (s, v, tv, sv, typeNameEnv, cst1) = f_tyconbind tycon
		   val (typeFunctionVar, cst2) = f_longtycon longtycon
		   val c   = E.initFunctionTypeConstraint (T.TYPE_FUNCTION_VAR typeFunctionVar) (T.TFC (T.ROW_VAR sv, T.consTYPE_VAR tv, lab)) lab
		   val cst = E.consConstraint(lab, c) (E.unionConstraintsList [cst1, cst2])
		   val opn = case A.longtyconToLid longtycon of
				 NONE => E.emopn
			       | SOME lid => E.singOEnv (lid, lab, E.DATATYPE_REPLICATION)
		   val env1 = case v of SOME idl => E.DATATYPE_CONSTRUCTOR_ENV (idl, E.ENVOPN opn) | NONE => E.ENVOPN opn
		   val env2 = E.ROW_ENV (E.CONSTRAINT_ENV cst, E.ROW_ENV (E.consEnvTypeNames typeNameEnv, env1))
	       in (env2, E.emptyContextSensitiveSyntaxError)
	       end
	     | f_dec (A.DecType (typbindseq, _, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.DecType")
		   val (typeNameEnv, cst, css) = f_typbindseq typbindseq
	       in (E.ROW_ENV (E.CONSTRAINT_ENV cst, E.consEnvTypeNames typeNameEnv), css)
	       end
	     | f_dec (A.DecEx (exbindseq, _, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.DecEx")
		   val (cons, cst, css) = f_exbindseq exbindseq
	       in (E.ROW_ENV (E.CONSTRAINT_ENV cst, E.projValueIds cons), css)
	       end
	     | f_dec (A.DecOpen (longstrseq, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.DecOpen")
		   val opns = foldl (fn (lid, opn) => E.addOEnv (lid, I.getLabId lid (*lab*), E.OPENED_STRUCT) opn)
				    (* NOTE: We can't use lab for all of then because then we can't
				     * distinguish between the ones that are IN and OUT during unification. *)
				    E.emopn
				    (f_longstrseq longstrseq)
	       in (E.projOpns opns, E.emptyContextSensitiveSyntaxError)
	       end
	     | f_dec (A.DecLocal (decs1, decs2, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.DecLocal")
		   val (env1, css1) = f_decs decs1
		   val (env2, css2) = f_decs decs2
		   val ev   = E.freshEnvVar ()
		   val c    = E.initEnvConstraint (E.consENV_VAR ev L.dummyLab) env1 L.dummyLab
		   val env3 = E.CONSTRAINT_ENV (E.singleConstraint (L.dummyLab, c))
		   val env4 = E.ENVDEP (EL.initExtLab (E.consENV_VAR ev lab) lab)
		   val env5 = E.ROW_ENV (env3, E.LOCAL_ENV (env4, env2))
	       in (env5 (*E.LOCAL_ENV (env1, env2)*), E.unionContextSensitiveSyntaxErrors [css1, css2])
	       end
	     | f_dec (A.DecAbsType (datbinds, decs, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.DecAbsType")
		   val (typeNameEnv1, conss, cst1, cst1', css1) = f_datbindseq datbinds
		   val (env2, css2) = f_decs decs
		   val envs = map (fn (cons, SOME idl) => E.DATATYPE_CONSTRUCTOR_ENV (idl, E.ENVPOL (E.emtv, E.projValueIds cons))
				    | (cons, NONE) => E.ENVPOL (E.emtv, E.projValueIds cons)) conss
		   val env3 = E.ROW_ENV (E.CONSTRAINT_ENV cst1', E.envsToSeq envs)
		   val env4 = E.LOCAL_ENV (env3, env2)
		   val env5 = E.ROW_ENV (E.ROW_ENV (E.CONSTRAINT_ENV cst1, E.consEnvTypeNames typeNameEnv1), env4)
		   val css0 = if getBasis ()
			      then E.emptyContextSensitiveSyntaxError
			      else E.singcss (notFullyCs lab "abstype")
		   val css  = E.unionContextSensitiveSyntaxErrors [css1, css2, css0]
	       in (env5, css)
	       end
	     | f_dec (A.DecAbsWith (datbinds, typbinds, decs, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.DecAbsWith")
		   val (typeNameEnv1, conss, cst1, cst1', css1) = f_datbindseq datbinds
		   val (typeNameEnv2, cst2, css2) = f_typbindseq typbinds
		   val (env3, css3) = f_decs decs
		   val envs = map (fn (cons, SOME idl) => E.DATATYPE_CONSTRUCTOR_ENV (idl, E.ENVPOL (E.emtv, E.projValueIds cons))
				    | (cons, NONE) => E.ENVPOL (E.emtv, E.projValueIds cons)) conss
		   val env4 = E.ROW_ENV (E.CONSTRAINT_ENV cst1', E.envsToSeq envs)
		   val env5 = E.ROW_ENV (E.CONSTRAINT_ENV cst1,  E.consEnvTypeNames typeNameEnv1)
		   val env6 = E.ROW_ENV (E.CONSTRAINT_ENV cst2,  E.consEnvTypeNames typeNameEnv2)
		   val env7 = E.ROW_ENV (env5, E.ROW_ENV (env6, E.LOCAL_ENV (env4, env3)))
		   val css0 = if getBasis ()
			      then E.emptyContextSensitiveSyntaxError
			      else E.singcss (notFullyCs lab "abstype")
		   val css  = E.unionContextSensitiveSyntaxErrors [css1, css2, css3, css0]
	       in (env7, css)
	       end
	     | f_dec (A.DecInfix (i, identseq, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.DecInfix")
		   val css1 = if getBasis ()
			      then E.emptyContextSensitiveSyntaxError
			      else E.singcss (notFullyCs lab "infix")
		   val css2 = if 0 <= i andalso i <= 9
			      then E.emptyContextSensitiveSyntaxError
			      else E.singcss (fixityCs lab)
	       in (E.emptyEnv, E.unionContextSensitiveSyntaxErrors [css1, css2])
	       end
	     | f_dec (A.DecInfixr (i, identseq, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.DecInfixr")
		   val css1 = if getBasis ()
			      then E.emptyContextSensitiveSyntaxError
			      else E.singcss (notFullyCs lab "infixr")
		   val css2 = if 0 <= i andalso i <= 9
			      then E.emptyContextSensitiveSyntaxError
			      else E.singcss (fixityCs lab)
	       in (E.emptyEnv, E.unionContextSensitiveSyntaxErrors [css1, css2])
	       end
	     | f_dec (A.DecNonfix (identseq, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.DecNonfix")
		   val css = if getBasis ()
			     then E.emptyContextSensitiveSyntaxError
			     else E.singcss (notFullyCs lab "nonfix")
	       in (E.emptyEnv, css)
	       end
	     | f_dec (A.DecOverload (labid, labtyp, labTypeVar, tyclassseq, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.DecOverload")
		   val (tv1, vids, cst1, css1) = f_labid labid
		   val (tv2, eqtv, cst2, css2) = f_labtype labtyp
		   val (tv3, tyvs, cst3) = f_labtypevarbind labTypeVar
		   val (sv4, cst4, css4) = f_tyclassseq tyclassseq
		   val lid  = Option.getOpt (A.getlabidLabId labid, (I.dummyId, L.dummyLab))
		   val c1   = E.initTypeConstraint (T.consTYPE_VAR tv3) (T.TYPE_POLY (T.ROW_VAR sv4, T.freshidor (), T.POLY, T.VALUE lid, lab, T.UNKNOWN)) lab
		   val c2   = E.initTypeConstraint (T.consTYPE_VAR tv1) (T.consTYPE_VAR tv2) lab
		   val cst  = E.conscsts (lab, [c1, c2]) (E.unionConstraintsList [cst1, cst3, cst4])
		   val css  = E.unionContextSensitiveSyntaxErrors [css1, css2, css4]
		   val env1 = E.ROW_ENV (E.CONSTRAINT_ENV cst2, E.projValueIds (E.toRECValueIds vids (L.singleton lab)))
		   val env2 = E.ROW_ENV (E.CONSTRAINT_ENV cst, E.ENVPOL (tyvs, env1))
	       in (env2, css)
	       end
	     | f_dec (A.DecClass (labclass, tyclassseq, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.DecClass")
		   val (sv1, ovcs, cst1) = f_labclassbind labclass
		   val (sv2, cst2, css2) = f_tyclassseq tyclassseq
		   val c   = E.initRowConstraint (T.ROW_VAR sv1) (T.ROW_VAR sv2) lab
		   val cst = E.consConstraint(lab, c) (E.unionConstraintsList [cst1, cst2])
		   val env = E.ROW_ENV (E.CONSTRAINT_ENV cst, E.projOverloadingClasses ovcs)
	       in (env, css2)
	       end
	     | f_dec (A.DecDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emptyContextSensitiveSyntaxError)
	       end

	   and f_declist []    = (E.emptyEnv, E.emptyContextSensitiveSyntaxError)
	     | f_declist [dec] =
	       let
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION (D.sep1'^"generating constraints for a dec:\n"^(A.printAstDec dec)^D.sep1')
		   val decResult = f_dec dec
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION (D.sep1'^"generated the constraints for the dec"^D.sep1')
	       in
		   decResult
	       end
	     | f_declist (dec :: decs) =
	       let val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION "generating constraints for an element in a last of decs (in f_declist)"
		   val (env1, css1) = f_dec dec
		   val (env2, css2) = f_declist decs
	       in (E.ROW_ENV (env1, env2), E.unionContextSensitiveSyntaxErrors [css1, css2])
	       end

	   and f_decs (A.Decs (decs, _)) = f_declist decs
	     | f_decs (A.DecsDots pl)  =
	       let val env = f_partlist pl
	       in (env, E.emptyContextSensitiveSyntaxError)
	       end

	   (*and f_decslet (A.Decs (ds, _)) = map (fn x => f_dec x) ds
	     | f_decslet (A.DecsDots pl)  =
	       let val (env, _, cs) = f_partlist pl
	       in [(env, E.emptyEnv, cs)]
	       end*)

	   (* RETURNS: (Env.varenv, E.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
	   and f_valdescone (A.ValDescOne (labid, labtyp, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.ValDescOne")
		   val (tv1, vids, cst1, css1) = f_labid labid
		   val (tv2, eqtv, cst2, css2) = f_labtype labtyp
		   val c   = E.initTypeConstraint (T.consTYPE_VAR tv1) (T.consTYPE_VAR tv2) lab
		   val cst = E.consConstraint(lab, c) (E.unionConstraintsList [cst1, cst2])
		   val css = E.unionContextSensitiveSyntaxErrors [css1, css2]
	       in (E.toRECValueIds vids (L.singleton lab), cst, css)
	       end
	     | f_valdescone (A.ValDescOneDots pl) =
	       let val env = f_partlist pl
	       in (E.getValueIds env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.varenv, Env.emptyConstraint, Env.emptyContextSensitiveSyntaxError) *)
	   and f_valdesc (A.ValDesc (valdescs, _, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.ValDesc")
		   val (vidss, csts, csss) = unzipThree (map f_valdescone valdescs)
		   val vids = E.unionEnvList vidss
		   val cst  = E.unionConstraintsList csts
		   val css  = E.unionContextSensitiveSyntaxErrors csss
	       in (vids, cst, css)
	       end
	     | f_valdesc (A.ValDescDots pl) =
	       let val env = f_partlist pl
	       in (E.getValueIds env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* this is used to represent one single type description, for example the 'int' in 'eqtype int' *)
	   and f_typdescone (A.TypDescOne (datname, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.TypDescOne")
		   val (str, idLabelPair, tv, sv1, sv2, typeNameEnv, tyvs, cst, css) = f_datname datname
		   (* NOTE: tyvs is not used *)
		   val id   = Option.getOpt (Option.map (fn (id, _) => id) idLabelPair, I.dummyId)
		   val lab' = Option.getOpt (Option.map (fn (_, lab) => lab) idLabelPair, lab)
		   val typename   = if benv andalso getBasis () then getTypenameString str else T.freshTypename ()
		   val c1   = E.initRowConstraint (T.ROW_VAR sv1) (T.ROW_VAR sv2) lab

		   (* the equality type status would actually depend on what it's binding really (ie whether eqtype or not) *)
		   val c2   = E.initTypeConstraint (T.consTYPE_VAR tv) (T.TYPE_CONSTRUCTOR (T.NC (typename, T.DECLARATION_CONS id, lab'), T.ROW_VAR sv1, lab', T.UNKNOWN)) lab'

		   (* Option.map: maps NONE to NONE and SOME(v) to SOME(f v)
		    * idLabelPair comes from f_tyconbind, where the id and label of A.TyCon are paired *)
		   val typenameOption = Option.map (fn (id, lab) => {id = id, lab = lab, kind = E.TYPE, name = typename}) idLabelPair
	       in (typenameOption, typeNameEnv, E.consConstraint(lab, c1) (E.consConstraint(lab', c2) cst), css)
	       end
	     | f_typdescone (A.TypDescOneDots pl) =
 	       let val env = f_partlist pl
	       in (NONE, E.getTypeNameEnv env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* called when types or equality types are defined in a signature
	    *
	    * from the definition: typdesc ::= typeVarsec tycon <and typdesc>
	    *                         spec ::= [...] | type typdesc | eqtype typdesc | [...] *
	    *
	    *)
	   and f_typdesc (A.TypDesc (typdescs, _, _)) =
	       let val _ = D.printDebug 2 D.AZE "generating constraints for A.TypDesc"

		   (* we have f_typdescone over the type descriptions, as there might be multiple of them.
		    * If there are then each type description will be represented by f_typeNameEnvescone via the map.
		    * We then call unzipFour which puts all of the first element in the tuple of each f_typdescone in a list,
		    * and then all the second elements, third, and fourth (we union these in the next lines) *)
		   (* could typeNameEnvs be a list of environments? Why is it a list of environments? *)
		   val (typenamesOption, typeNameEnvs, constraints, csss) = unzipFour (map f_typdescone typdescs)

		   (* because we have all first to fourth elements of the typdescs now in a list, we have to union them *)
		   val typeNameEnv = E.unionEnvList typeNameEnvs

		   (* unions a list of CONSTRAINTS values
		    * that is, unions a list of lists of constraints *)
		   val constraint  = E.unionConstraintsList constraints

		   (* unions a list of context sensitive syntax errors *)
		   val css  = E.unionContextSensitiveSyntaxErrors csss

		   (* the list typenameOption contains items of the form NONE or SOME(x)
		    * mapPartial (fn x => x) will remove all items of the form NONE from the list and return a list of the x values of items with the SOME(x) form *)
		   val typename  = List.mapPartial (fn x => x) typenamesOption

	       in (typename,  typeNameEnv, constraint, css)
	       end
	     | f_typdesc (A.TypDescDots pl) =
	       let val env = f_partlist pl
	       in ([], E.getTypeNameEnv env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.env, Env.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
	   and f_tdrdescone (A.TdrDescOne (datname, labtyp, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.TdrDescOne")
		   val (s, v, tv1, sv1, sv2, typeNameEnv, tyvs, cst1, css1) = f_datname datname
		   val (tv2, eqtv, cst2, css2) = f_labtype labtyp
		   val c1  = E.initTypeConstraint (T.consTYPE_VAR tv1) (T.consTYPE_VAR tv2) lab
		   val c2  = E.initRowConstraint (T.ROW_VAR sv1) (T.ROW_VAR sv2) lab
		   val c3  = E.LET_CONSTRAINT (E.ROW_ENV (E.projExplicitTypeVars tyvs, E.CONSTRAINT_ENV cst2))
		   val env = E.ENVDEP (EL.initExtLab (E.consEnvTypeNames typeNameEnv) lab)
		   val cst = E.conscsts (lab, [c1, c2]) (E.consConstraint(L.dummyLab, c3) cst1)
		   val css = E.unionContextSensitiveSyntaxErrors [css1, css2]
	       in (env, cst, css)
	       end
	     | f_tdrdescone (A.TdrDescOneDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.env, E.emptyContextSensitiveSyntaxError) *)
	   and f_tdrdesclist []                    = (E.emptyEnv, E.emptyContextSensitiveSyntaxError)
	     | f_tdrdesclist [tdrdesc]             =
	       let val (env, cst, css) = f_tdrdescone tdrdesc
	       in (E.ROW_ENV (E.CONSTRAINT_ENV cst, env), css)
	       end
	     | f_tdrdesclist (tdrdesc :: tdrdescs) =
	       let val (env1, cst, css1) = f_tdrdescone tdrdesc
		   val (env2, css2) = f_tdrdesclist tdrdescs
		   val css = E.unionContextSensitiveSyntaxErrors [css1, css2]
	       in (E.ROW_ENV (E.ROW_ENV (E.CONSTRAINT_ENV cst, env1), env2), css)
	       end

	   (* RETURNS: (Env.env, E.emptyContextSensitiveSyntaxError) *)
	   and f_tdrdesc (A.TdrDesc (tdrdescs, _, _)) = f_tdrdesclist tdrdescs
	       (*let val (typeNameEnvs, csts, csss) = unzipThree (map f_tdrdescone tdrdescs)
		   val typeNameEnv = E.unionEnvList typeNameEnvs
		   val cst  = E.unionConstraintsList csts
		   val css  = E.unionContextSensitiveSyntaxErrors csss
	       in (typeNameEnv, cst, css)
	       end*)
	     | f_tdrdesc (A.TdrDescDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.varenv, Env.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
	   and f_excdescone (A.ExcDescOne (ident, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.ExcDescOne")
		   val (tv, cons, cst, css) = f_identpat ident
		   val c = E.initTypeConstraint (T.consTYPE_VAR tv) (T.constyexception' lab (T.DECLARATION_CONS I.dummyId)) lab
	       in (E.toEX0ValueIds cons (L.singleton lab), E.consConstraint(lab, c) cst, css)
	       end
	     | f_excdescone (A.ExcDescOf (labid, labtyp, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.ExcDescOf")
		   val (tv1, cons, cst1, css1) = f_labid labid
		   val (tv2, eqtv, cst2, css2) = f_labtype labtyp
		   val ty  = T.constyexception' lab (T.DECLARATION_CONS I.dummyId)
		   val c   = E.initTypeConstraint (T.consTYPE_VAR tv1) (T.consTyArrowTy (T.consTYPE_VAR tv2) ty lab (T.DECLARATION_CONS I.dummyId)) lab
		   val cst = E.consConstraint(lab, c) (E.unionConstraintsList [cst1, cst2])
		   val css = E.unionContextSensitiveSyntaxErrors [css1, css2]
	       in (E.toEX1ValueIds cons (L.singleton lab), cst, css)
	       end
	     | f_excdescone (A.ExcDescOneDots pl) =
	       let val env = f_partlist pl
	       in (E.getValueIds env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.varenv, Env.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
	   and f_excdesc (A.ExcDesc (exdescs, _, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.EscDesc")
		   val (vidss, csts, csss) = unzipThree (map f_excdescone exdescs)
		   val vids = E.unionEnvList vidss
		   val cst  = E.unionConstraintsList csts
		   val css  = E.unionContextSensitiveSyntaxErrors csss
	       in (vids, cst, css)
	       end
	     | f_excdesc (A.ExcDescDots pl) =
	       let val env = f_partlist pl
	       in (E.getValueIds env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.typeVar, Env.varenv, Env.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
	   and f_condescone (A.ConDescOneId (ident, _)) =
	       let val (tv, cons, cst, css) = f_identpat ident
	       in (tv, E.toDA0ValueIds cons L.empty, cst, css)
	       end
	     | f_condescone (A.ConDescOneOf (labid, labtyp, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.ConDescOneOf")
		   val (tv1, cons, cst1, css1) = f_labid labid
		   val (tv2, eqtv, cst2, css2) = f_labtype labtyp
		   val (lab1, lab2) = Option.getOpt (A.getLabelsIdLabId labid, (lab, lab))
		   val clv1 = CL.newClassVar ()
		   val clv2 = CL.newClassVar ()
		   val tv   = T.freshTypeVar ()
		   val c1   = E.initTypeConstraint (T.consTYPE_VAR tv1) (T.constyarrow' tv2 tv lab (T.DECLARATION_CONS I.dummyId)) lab
		   val c2   = E.initClassConstraint clv2 (CL.consDA1 ()) lab
		   val c3   = E.initClassConstraint clv1 clv2 lab1
		   val c4   = E.initClassConstraint clv1 (CL.consDAT ()) lab2
		   val cst  = E.unionConstraintsList [cst1, cst2]
		   val css  = E.unionContextSensitiveSyntaxErrors [css1, css2]
		   val cst' = E.conscsts (lab, [c1, c2]) (E.consConstraint(lab1, c3) (E.consConstraint(lab2, c4) cst))
	       (*val cons = E.toDA1ValueIds cons (L.singleton lab)*)
	       in (tv, E.toCLSValueIds cons clv1 L.empty, cst', css)
	       end
	     | f_condescone (A.ConDescOneNoOf (ident, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.ConDescOneNoOf")
		   val (tv, cons, cst, css) = f_identpat ident
		   val tv' = T.freshTypeVar ()
	       in (tv, E.toDATValueIds cons L.empty, cst, css)
	       end
	     | f_condescone (A.ConDescOneDots pl) =
	       let val env = f_partlist pl
		   val tv  = T.freshTypeVar ()
	       in (tv, E.getValueIds env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Ty.typeVar list, Env.varenv, Env.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
	   and f_condesc (A.ConDesc (condescs, _, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.ConDesc")
		   val (tvs, conss, csts, csss) = unzipFour (map f_condescone condescs)
		   val cons = E.unionEnvList conss
		   val cst  = E.unionConstraintsList csts
		   val css  = E.unionContextSensitiveSyntaxErrors csss
	       in (tvs, cons, cst, css)
	       end
	     | f_condesc (A.ConDescDots pl) =
	       let val env = f_partlist pl
	       in ([], E.getValueIds env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: ((Id.id, Ty.Typename) option, Env.typenv, E.varenv * Id.idl option, E.emptyConstraint, E.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
	   and f_datdescone (A.DatDescOne (datname, consdesc, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.DatDescOne")
		   val (s, v, tv, sv1, sv2, typeNameEnv, tyvs, cst1, css1) = f_datname datname
		   val (tvs, cons, cst2, css2) = f_condesc consdesc
		   val typename   = if benv andalso getBasis () then getTypenameString s else T.freshTypename ()
		   val id   = Option.getOpt (Option.map (fn (i, _) => i) v, I.dummyId)
		   val lab' = Option.getOpt (Option.map (fn (_, l) => l) v, lab)
		   val c1   = E.initRowConstraint (T.ROW_VAR sv1) (T.ROW_VAR sv2) lab
		   val c2   = E.initTypeConstraint (T.consTYPE_VAR tv) (T.TYPE_CONSTRUCTOR (T.NC (typename, T.DECLARATION_CONS id, lab'), T.ROW_VAR sv1, lab', T.UNKNOWN)) lab'
		   val c3   = E.LET_CONSTRAINT (E.ROW_ENV (E.projExplicitTypeVars tyvs, E.CONSTRAINT_ENV cst2))
		   val cs   = map (fn x => E.initTypeConstraint (T.consTYPE_VAR x) (T.consTYPE_VAR tv) lab) tvs
		   val cst  = E.consConstraint(lab, c1) (E.consConstraint(lab', c2) cst1)
		   val cst' = E.conscsts (lab, cs) (E.singleConstraint (L.dummyLab, c3))
		   val typenameOption = Option.map (fn (id, lab) => {id = id, lab = lab, kind = E.DATATYPE, name = typename}) v
		   val typeNameEnv = E.toTYCONTypeNameEnv typeNameEnv E.emvar false (L.singleton lab)
		   (*val cs4 = checkTypeVarInc (A.gettypeVarDatName dn) (A.getlabDatName dn) (A.gettypeVarConDesc cd)*)
	       in (typenameOption, typeNameEnv, (cons, v), cst, cst', E.unionContextSensitiveSyntaxErrors [css1, css2])
	       end
	     | f_datdescone (A.DatDescOneDots pl) =
	       let val env = f_partlist pl
	       in (NONE, E.getTypeNameEnv env, (E.getValueIds env, NONE), E.emptyConstraint, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: ((Id.id, Ty.tyname) option, Env.typenv, (E.varenv * Id.idl option) list, E.emptyConstraint, E.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
	   and f_datdesc (A.DatDesc (datdescs, _, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.DatDesc")
		   val (typenameOption, typeNameEnvs, conss, csts, csts', csss) = unzipSix (map f_datdescone datdescs)
		   val typeNameEnv = E.unionEnvList typeNameEnvs
		   val cst  = E.unionConstraintsList csts
		   val cst' = E.unionConstraintsList csts'
		   val css  = E.unionContextSensitiveSyntaxErrors csss
		   val typename  = List.mapPartial (fn x => x) typenameOption
	       in (typename, typeNameEnv, conss, cst, cst', css)
	       end
	     | f_datdesc (A.DatDescDots pl) =
	       let val env = f_partlist pl
	       in ([], E.getTypeNameEnv env, [(E.getValueIds env, NONE)], E.emptyConstraint, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.strenv, E.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
	   and f_strdescone (A.StrDescOne (strid, labsigexp, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.StrDescOne")
		   val (ev1, strs, cst1, css1) = f_strid strid
		   val (ev2, cst2, css2) = f_labsigexp labsigexp
		   val c   = E.initEnvConstraint (E.consENV_VAR ev1 lab) (E.consENV_VAR ev2 lab) lab
		   val cst = E.consConstraint(lab, c) (E.unionConstraintsList [cst1, cst2])
		   val css = E.unionContextSensitiveSyntaxErrors [css1, css2]
	       in (strs, cst, css)
	       end
	     | f_strdescone (A.StrDescOneDots pl) =
	       let val env = f_partlist pl
	       in (E.getStructs env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.strenv, E.emptyConstraint, E.emptyContextSensitiveSyntaxError) *)
	   and f_strdesc (A.StrDesc (strdescs, _, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.StrDesc")
		   val (strss, csts, csss) = unzipThree (map f_strdescone strdescs)
		   val strs = E.unionEnvList strss
		   val cst  = E.unionConstraintsList csts
		   val css  = E.unionContextSensitiveSyntaxErrors csss
	       in (strs, cst, css)
	       end
	     | f_strdesc (A.StrDescDots pl) =
	       let val env = f_partlist pl
	       in (E.getStructs env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (E.env, E.emptyContextSensitiveSyntaxError) *)
	   and f_speconesmltes (A.SpecValue (valdesc, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.SpecValue")
		   val (vids, cst1, css) = f_valdesc valdesc
		   val (tyvs, cst2) = f_typevarvallist (A.getTypeVarValDesc valdesc)
		   val env1 = E.ENVPOL (tyvs, E.ROW_ENV (E.CONSTRAINT_ENV cst1, E.projValueIds vids))
		   val env2 = E.ROW_ENV (E.CONSTRAINT_ENV cst2, env1)
		   val ev   = E.freshEnvVar ()
		   val c    = E.initEnvConstraint (E.consENV_VAR ev lab) env2 lab
		   val env3 = E.ROW_ENV (E.CONSTRAINT_ENV (E.singleConstraint (lab, c)), E.ENVDEP (EL.initExtLab (E.consENV_VAR ev lab) lab))
	       in (env3, css)
	       end
	     | f_speconesmltes spec = f_specone spec

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_speconesmlteslist []        = (E.emptyEnv, E.emptyContextSensitiveSyntaxError)
	     | f_speconesmlteslist [x]       = f_speconesmltes x
	     | f_speconesmlteslist (x :: xs) =
	       let val (env1, css1) = f_speconesmltes x
		   val (env2, css2) = f_speconesmlteslist xs
	       in (E.ROW_ENV (env1, env2), E.unionContextSensitiveSyntaxErrors [css1, css2])
	       end

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_specsmltes (A.Spec (specs, _)) =
	       let val (env, css) = f_speconesmlteslist specs
	       (*(2010-04-19)These gets might pose problem with ROW_ENV and ENVOPN.
		* They pose problem.  We need to build before and then check at unification
		* time that we don't have duplicates for signatures only.*)
	       (*val csc1 = if E.isEnvC env2 then consCSM [E.getValueIds env2, E.getCons env2] false else []*)
	       (*val csc2 = if E.isEnvC env2 then consCSM [E.getTypeNameEnv env2] false else []*)
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
		   val tv1  = T.freshTypeVar ()
		   val tv2  = T.freshTypeVar ()
		   val tyvs = E.consSingleEnv (n, [E.consBindMono n (tv1, true) (CL.consTYVAR ()) lab])
		   (* generate a type constraint (how is this actually used later?) *)
		   val c    = E.initTypeConstraint (T.consTYPE_VAR tv1) (T.TYPE_VAR (tv2, SOME (n, lab), T.POLY, T.UNKNOWN)) lab
	       in (tyvs, E.singleConstraint (lab, c))
	       end
	     | f_typevarspec (A.EqualityTypeVar (_, n, _, lab, _)) =
	       let val _   = D.printDebug 2 D.AZE ("in f_typevarspec - generating constraints for A.EqualityTypeVar (f_typevarspec; lab = "^Int.toString(L.toInt(lab))^")")
		   (* generate some fresh type variables *)
		   val tv1  = T.freshTypeVar ()
		   val tv2  = T.freshTypeVar ()
		   val tyvs = E.consSingleEnv (n, [E.consBindMono n (tv1, true) (CL.consTYVAR ()) lab])
		   (* generate a type constraint (how is this actually used later?) *)
		   val c    = E.initTypeConstraint (T.consTYPE_VAR tv1) (T.TYPE_VAR (tv2, SOME (n, lab), T.POLY, T.UNKNOWN)) lab
	       in (tyvs, E.singleConstraint (lab, c))
	       end
	     | f_typevarspec A.TypeVarDots = (E.emtv, E.emptyConstraint)

	   (* RETURNS: (Env.tvenv, Env.cst) *)
	   and f_typevarspeclist [] = (E.emtv, E.emptyConstraint)
	     | f_typevarspeclist [typevar] = f_typevarspec typevar
	     | f_typevarspeclist (typevar ::  typevars) =
	       let val (tyvs1, cst1) = f_typevarspec typevar
		   val (tyvs2, cst2) = f_typevarspeclist typevars
	       in (E.unionEnvList [tyvs1, tyvs2], E.unionConstraintsList [cst1, cst2])
	       end

	   (* value definitions inside a signature *)
	   and f_specone (A.SpecValue (valdesc, _, lab, _)) =
	       let val _   = D.printDebugFunc 2 D.AZE (fn _ => ("in f_specone - generating constraints for A.SpecValue (lab = "^Int.toString(L.toInt(lab))^")"))
		   val (vids, cst1, css) = f_valdesc valdesc
		   (*(2010-06-23)Before we were using f_typevarvallist which generates binders
		    * of explicit type variables, now we use binders of implicit type variables
		    * tagged with the labels of the implicit type variables they're coming from. *)
		   val (tyvs, cst2) = f_typevarspeclist (A.getTypeVarValDesc valdesc)
		   val env1 = E.ROW_ENV (E.CONSTRAINT_ENV cst2, E.projExplicitTypeVars tyvs)
		   val env2 = E.ROW_ENV (E.CONSTRAINT_ENV cst1, E.ENVDEP (EL.initExtLab (E.projValueIds vids) lab))
		   (*val env3 = E.LOCAL_ENV (env1, env2)*)
		   val env3 = E.ENVPOL (E.emtv, E.LOCAL_ENV (E.ENVDEP (EL.initExtLab env1 lab), env2))
		   val ev1  = E.freshEnvVar ()
		   val ev2  = E.freshEnvVar ()
		   val c1   = E.initEnvConstraint (E.consENV_VAR ev1 L.dummyLab) env3 L.dummyLab
		   val c2   = E.initEnvConstraint (E.consENV_VAR ev2 lab) (E.consENV_VAR ev1 lab) lab
		   val cst  = E.consConstraint(L.dummyLab, c1) (E.singleConstraint (lab, c2))
		   val env4 = E.ROW_ENV (E.CONSTRAINT_ENV cst, E.ENVDEP (EL.initExtLab (E.consENV_VAR ev2 lab) lab))
	       (*(2010-06-17)Don't we want to use ENVPOL on vids?
		* The problem is that when we check a signature against a structure
		* then we want the explicit type variables to be explicit to check
		* that the structure is at least as general as the signature. *)
	       (* NOTE: we have to bind the explicit type variables. *)
	       in (env4, css)
	       end

	     (* type declarations inside a signature *)
	     | f_specone (A.SpecType (typdesc, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.SpecType")
		   val (typenames, typeNameEnv, constraints, css) = f_typdesc typdesc
		   val env  = E.ROW_ENV (E.CONSTRAINT_ENV constraints, E.updateInfoTypeNames typenames (E.consEnvTypeNames typeNameEnv))
		   val ev   = E.freshEnvVar ()
		   val c    = E.initEnvConstraint (E.consENV_VAR ev lab) env lab
		   val env' = E.ROW_ENV (E.CONSTRAINT_ENV (E.singleConstraint (lab, c)), E.ENVDEP (EL.initExtLab (E.consENV_VAR ev lab) lab))
	       in (env', css)
	       end

	     (* equality types declarations inside a signature *)
	     | f_specone (A.SpecEqtype (typdesc, _, lab, _)) =
	       let val _ = D.printDebug 2 D.AZE ("generating constraints for A.SpecEqtype (lab = "^Int.toString(L.toInt lab)^")")

		   (* we call f_typdesc so that we can (get/generate)? information about the typdesc (type description? what does that mean?)
		    * the first element of the tuple represents the list of typenames for the new equality type
		    * the second element of the tuple represents the environment holding typname information (see f_scecone(A.TyCon [...]))
		    * the third element of the tuple represents the constraints that have been built from (???)
		    * the fourth element of the tuple represents context sensitive syntax error information
		    *)
		   val (typenames, typeNameEnv, constraints, contextSensitiveSyntaxError) = f_typdesc typdesc

		   val env  = E.ROW_ENV (E.CONSTRAINT_ENV constraints, E.updateInfoTypeNames typenames (E.consEnvTypeNames typeNameEnv))

		   val envVar   = E.freshEnvVar ()
		   (* val eqVar    = T.freshEqualityTypeVar () *)

		   (* equivalent to ENV_CONSTRAINT (EL.initExtLab (ENVVAR (envVar, lab), env) lab)
		    * do we really want to generate an environment constraint here? what type of
		    * constraint would it be best to generate to show that we actually are dealing with
		    * an equality type?
		    *)
		   val c1 = E.initEnvConstraint (E.consENV_VAR envVar lab) env lab
		   (* val c2 = E.initEqualityTypeConstraint (Ty.consEQUALITY_TYPE_VAR eqVar) (Ty.EQUALITY_TYPE_STATUS (Ty.EQUALITY_TYPE)) lab *)

		   (* val env1 = E.CONSTRAINT_ENV (E.consConstraint (L.dummyLab, c1) (E.singleConstraint (lab, c2))) *)
		   (* val env2 = E.ROW_ENV (env1, E.ENVDEP (EL.initExtLab (E.consENV_VAR envVar lab) lab)) *)

		   (* in (env2, contextSensitiveSyntaxError) *)

		   val env' = E.ROW_ENV (E.CONSTRAINT_ENV (E.singleConstraint (lab, c1)),
					 E.ENVDEP (EL.initExtLab (E.consENV_VAR envVar lab) lab))
	       in (env', contextSensitiveSyntaxError)

	       end
	     | f_specone (A.SpecTdr (tdrdesc, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.SpecTdr")
		   val (env, css) = f_tdrdesc tdrdesc
		   val ev1  = E.freshEnvVar ()
		   val ev2  = E.freshEnvVar ()
		   val c1   = E.initEnvConstraint (E.consENV_VAR ev1 L.dummyLab) env L.dummyLab
		   val c2   = E.initEnvConstraint (E.consENV_VAR ev2 lab) (E.consENV_VAR ev1 lab) lab
		   val env1 = E.CONSTRAINT_ENV (E.consConstraint (L.dummyLab, c1) (E.singleConstraint (lab, c2)))
		   val env2 = E.ROW_ENV (env1, E.ENVDEP (EL.initExtLab (E.consENV_VAR ev2 lab) lab))
	       (* We can't have an ENVDEP here because we can have an error in
		* the type part that does not need the spec to exist.
		* It is the same for all the specs because of the type parts. *)
	       in (env2, css)
	       end

	     (* exception declaration inside a signature *)
	     | f_specone (A.SpecException (exdesc, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.SpecException")
		   val (cons, cst, css) = f_excdesc exdesc
		   val env  = E.ROW_ENV (E.CONSTRAINT_ENV cst, E.ENVDEP (EL.initExtLab (E.projValueIds cons) lab))
		   val ev1  = E.freshEnvVar ()
		   val ev2  = E.freshEnvVar ()
		   val c1   = E.initEnvConstraint (E.consENV_VAR ev1 L.dummyLab) env L.dummyLab
		   val c2   = E.initEnvConstraint (E.consENV_VAR ev2 lab) (E.consENV_VAR ev1 lab) lab
		   val cst' = E.consConstraint(L.dummyLab, c1) (E.singleConstraint (lab, c2))
		   val env' = E.ROW_ENV (E.CONSTRAINT_ENV cst', E.ENVDEP (EL.initExtLab (E.consENV_VAR ev2 lab) lab))
	       in (env', css)
	       end
	     | f_specone (A.SpecDat (datdesc, _, lab, _)) =
	       let
		   val _ = D.printDebug 2 D.AZE ("generating constraints for A.SpecDat (lab = "^Int.toString(L.toInt lab)^")")

		   val (typenames, typeNameEnv, conss, cst1, cst2, css) = f_datdesc datdesc
		   val envs = map (fn (cons, SOME idl) => E.DATATYPE_CONSTRUCTOR_ENV (idl, E.ENVPOL (E.emtv, E.projValueIds cons))
				    | (cons, NONE) => E.ENVPOL (E.emtv, E.projValueIds cons)) conss
		   val env1 = E.ROW_ENV (E.CONSTRAINT_ENV cst2, E.envsToSeq envs)
		   val env2 = E.updateInfoTypeNames typenames (E.consEnvTypeNames typeNameEnv)
		   val env3 = E.ROW_ENV (E.ROW_ENV (E.CONSTRAINT_ENV cst1, E.ENVDEP (EL.initExtLab env2 lab)), env1)
		   val ev1  = E.freshEnvVar ()
		   val ev2  = E.freshEnvVar ()
		   val c1   = E.initEnvConstraint (E.consENV_VAR ev1 L.dummyLab) env3 L.dummyLab
		   val c2   = E.initEnvConstraint (E.consENV_VAR ev2 lab) (E.consENV_VAR ev1 lab) lab
		   val cst  = E.consConstraint(L.dummyLab, c1) (E.singleConstraint (lab, c2))
		   val env4 = E.ROW_ENV (E.CONSTRAINT_ENV cst, E.ENVDEP (EL.initExtLab (E.consENV_VAR ev2 lab) lab))
	       in (env4, css)
	       end
	     | f_specone (A.SpecStr (strdesc, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.SpecStr")
		   val (strs, cst, css) = f_strdesc strdesc
		   val env  = E.ROW_ENV (E.CONSTRAINT_ENV cst, E.ENVDEP (EL.initExtLab (E.projStructs strs) lab))
		   val ev1  = E.freshEnvVar ()
		   val ev2  = E.freshEnvVar ()
		   val c1   = E.initEnvConstraint (E.consENV_VAR ev1 L.dummyLab) env L.dummyLab
		   val c2   = E.initEnvConstraint (E.consENV_VAR ev2 lab) (E.consENV_VAR ev1 lab) lab
		   val env1 = E.CONSTRAINT_ENV (E.consConstraint(L.dummyLab, c1) (E.singleConstraint (lab, c2)))
		   val env2 = E.ROW_ENV (env1, E.ENVDEP (EL.initExtLab (E.consENV_VAR ev2 lab) lab))
		   (*val _ = D.printdebug2 (L.printLab lab)*)
		   (*val _ = D.printdebug2 (E.printEnv env "")*)
	       in (env2, css)
	       end
	     | f_specone (A.SpecInc (labsigexp, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.SpecInc")
		   val (ev, cst, css) = f_labsigexp labsigexp
		   val ev'  = E.freshEnvVar ()
		   val c    = E.initEnvConstraint (E.consENV_VAR ev' lab) (E.consENV_VAR ev lab) lab
		   val env1 = E.CONSTRAINT_ENV (E.consConstraint(lab, c) cst)
		   val env2 = E.ROW_ENV (env1, E.ENVDEP (EL.initExtLab (E.consENV_VAR ev' lab) lab))
		   val css' = if getBasis ()
			      then E.emptyContextSensitiveSyntaxError
			      else E.singcss (sorryCsS lab "include")
	       in (env2, css')
	       end
	     | f_specone (A.SpecIsi (_, _, lab, _)) =
	       let val css = if getBasis ()
			     then E.emptyContextSensitiveSyntaxError
			     else E.singcss (sorryCsS lab "include")
	       in (E.emptyEnv, css)
	       end
	     | f_specone (A.SpecRep (tycon, longtycon, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.SpecRep")
		   val (s, v, tv, sv, typeNameEnv, cst1) = f_tyconbind tycon
		   val (typeFunctionVar, cst2) = f_longtycon longtycon
		   val c   = E.initFunctionTypeConstraint (T.TYPE_FUNCTION_VAR typeFunctionVar) (T.TFC (T.ROW_VAR sv, T.consTYPE_VAR tv, lab)) lab
		   val cst = E.consConstraint(lab, c) (E.unionConstraintsList [cst1, cst2])
		   val opn = case A.longtyconToLid longtycon of
				 NONE => E.emopn
			       | SOME lid => E.singOEnv (lid, lab, E.DATATYPE_REPLICATION)
		   val env1 = case v of SOME idl => E.DATATYPE_CONSTRUCTOR_ENV (idl, E.ENVOPN opn) | NONE => E.ENVOPN opn
		   val env2 = E.ROW_ENV (E.CONSTRAINT_ENV cst, E.ROW_ENV (E.consEnvTypeNames typeNameEnv, env1))
		   val ev   = E.freshEnvVar ()
		   val c    = E.initEnvConstraint (E.consENV_VAR ev lab) env2 lab
		   val env3 = E.ROW_ENV (E.CONSTRAINT_ENV (E.singleConstraint (lab, c)), E.ENVDEP (EL.initExtLab (E.consENV_VAR ev lab) lab))
	       in (env3, E.emptyContextSensitiveSyntaxError)
	       end
	     | f_specone (A.SpecSha (spec, longtyconeq, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.SpecSha")
		   val (env1, css) = f_spec spec
		   val env2 = f_longtyconeq longtyconeq
		   (*(2010-07-02)This is not the correct thing to do for sharing.*)
		   val ev   = E.freshEnvVar ()
		   val ev1  = E.freshEnvVar ()
		   val ev2  = E.freshEnvVar ()
		   val c1   = E.initEnvConstraint (E.consENV_VAR ev1 lab) env1 lab
		   val c2   = E.initEnvConstraint (E.consENV_VAR ev2 lab) env2 lab
		   val c3   = E.SHARING_CONSTRAINT (ev1, ev, ev2, lab)
		   (*val c3   = E.SIGNATURE_CONSTRAINT (ev1, SOME ev, ev2, NONE, lab, E.WHR)*)
		   val env3 = E.ROW_ENV (E.CONSTRAINT_ENV (E.singcsts (lab, [c1, c2])), E.CONSTRAINT_ENV (E.singleConstraint (lab, c3)))
		   val env4 = E.ROW_ENV (env3, E.consENV_VAR ev lab)
		   val css' = if getBasis ()
			      then E.emptyContextSensitiveSyntaxError
			      else E.singcss (poorlyCs lab "sharing")
	       in (env4, E.unionContextSensitiveSyntaxErrors [css, css'])
	       end
	     | f_specone (A.SpecSsi (spec, longstrideq, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.SpecSsi")
		   val (env, css) = f_spec spec
		   val css' = if getBasis ()
			      then E.emptyContextSensitiveSyntaxError
			      else E.singcss (poorlyCs lab "sharing")
		   val env' = E.newEnvVar lab
	       in (E.ROW_ENV (env, env'), E.unionContextSensitiveSyntaxErrors [css, css'])
	       end
	     | f_specone (A.SpecOneDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: Env.env *)
	   and f_longtyconeq (A.LongTyConEq (longtycons, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.LongTyConEq")
		   val (typeFunctionVars, envs, csts) = unzipThree (map f_longtyconbind longtycons)
		   val env = E.unionEnv envs
		   val cst = E.unionConstraintsList csts
		   val tfc = T.TFC (T.newROW_VAR (), T.newTYPE_VAR (), lab)
		   val cs  = map (fn typeFunctionVar => E.initFunctionTypeConstraint (T.TYPE_FUNCTION_VAR typeFunctionVar) tfc lab) typeFunctionVars
	       in E.ROW_ENV (E.CONSTRAINT_ENV (E.conscsts (lab, cs) cst), env)
	       end
	     | f_longtyconeq (A.LongTyConEqDots pl) =
	       let val env = f_partlist pl
	       in env
	       end

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_speconelist []        = (E.emptyEnv, E.emptyContextSensitiveSyntaxError)
	     | f_speconelist [x]       = f_specone x
	     | f_speconelist (x :: xs) =
	       let val (env1, css1) = f_specone x
		   val (env2, css2) = f_speconelist xs
	       in (E.ROW_ENV (env1, env2), E.unionContextSensitiveSyntaxErrors [css1, css2])
	       end

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_spec (A.Spec (specs, _)) =
	       let val (env, css) = f_speconelist specs
	       (*(2010-04-19)These gets might pose problem with ROW_ENV and ENVOPN.
		* They pose problem.  We need to build before and then check at unification
		* time that we don't have duplicates for signatures only.*)
	       (*val csc1 = if E.isEnvC env2 then consCSM [E.getValueIds env2, E.getCons env2] false else []*)
	       (*val csc2 = if E.isEnvC env2 then consCSM [E.getTypeNameEnv env2] false else []*)
	       in (env, css)
	       end
	     | f_spec (A.SpecDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_strbind (A.StrBind (strbinds, _, _)) =
	       let val (strs, cst, css) = f_strbindonelist strbinds
	       in (E.ROW_ENV (E.CONSTRAINT_ENV cst, E.projStructs strs), css)
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
		   val strs = E.unionEnvList [strs1, strs2]
		   val cst  = E.unionConstraintsList [cst1, cst2]
		   val css  = E.unionContextSensitiveSyntaxErrors [css1, css2]
	       in (strs, cst, css)
	       end

	   (* RETURNS: (Env.strenv, Env.cst, Env.css) *)
	   and f_strbindone (A.StrBindOneOp (strid, labsigexp, labstrexp, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.StrBindOneOp")
		   val (ev1, cst1, css1) = f_labstrexp labstrexp
		   val (ev2, cst2, css2) = f_labsigexp labsigexp
		   val (ev3, strs, cst3, css3) = f_strid strid
		   val c    = E.SIGNATURE_CONSTRAINT (ev2, NONE, ev1, SOME ev3, lab)
		   val cst  = E.singleConstraint (lab, c)
		   val css  = E.unionContextSensitiveSyntaxErrors [css1, css2, css3]
		   val env  = E.ROW_ENV (E.CONSTRAINT_ENV (E.unionConstraintsList [cst1, cst2]), E.CONSTRAINT_ENV cst)
		   val cst' = E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env)
	       in (strs, E.unionConstraintsList [cst3, cst'], css)
	       end
	     | f_strbindone (A.StrBindOneTr (strid, labsigexp, labstrexp, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.StrBindOneTr")
		   val (ev1, cst1, css1) = f_labstrexp labstrexp
		   val (ev2, cst2, css2) = f_labsigexp labsigexp
		   val (ev3, strs, cst3, css3) = f_strid strid
		   val c    = E.SIGNATURE_CONSTRAINT (ev2, SOME ev3, ev1, NONE, lab)
		   val cst  = E.singleConstraint (lab, c)
		   val css  = E.unionContextSensitiveSyntaxErrors [css1, css2, css3]
		   val env  = E.ROW_ENV (E.CONSTRAINT_ENV (E.unionConstraintsList [cst1, cst2]), E.CONSTRAINT_ENV cst)
		   val cst' = E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env)
	       in (strs, E.unionConstraintsList [cst3, cst'], css)
	       end
	     | f_strbindone (A.StrBindOne (strid, labstrexp, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.StrBindOne")
		   val (ev1, strs, cst1, css1) = f_strid strid
		   val (ev2, cst2, css2) = f_labstrexp labstrexp
		   val c   = E.initEnvConstraint (E.consENV_VAR ev1 lab) (E.consENV_VAR ev2 lab) lab
		   val cst = E.consConstraint(lab, c) (E.unionConstraintsList [cst1, cst2])
		   val css = E.unionContextSensitiveSyntaxErrors [css1, css2]
	       in (strs, cst, css)
	       end
	     | f_strbindone (A.StrBindOneDots pl) =
	       let val env = f_partlist pl
	       in (E.getStructs env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_strdec (A.StrDec (strdecones, lab, _)) =
	       f_strdeconelist strdecones
	     | f_strdec (A.StrDecDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_strdeconelist [] = (E.emptyEnv, E.emptyContextSensitiveSyntaxError)
	     | f_strdeconelist [strdec] = f_strdecone strdec
	     | f_strdeconelist (strdec :: strdecs) =
	       let val (env1, css1) = f_strdecone strdec
		   val (env2, css2) = f_strdeconelist strdecs
	       in (E.ROW_ENV (env1, env2), E.unionContextSensitiveSyntaxErrors [css1, css2])
	       end

	   (* RETURNS: (Env.env, Env.css) *)
	   and f_strdecone (A.StrDecOneDec decs) = f_decs decs
	     | f_strdecone (A.StrDecOneStr (strbind, _, _)) = f_strbind strbind
	     | f_strdecone (A.StrDecOneLoc (strdec1, strdec2, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.StrDecOneLoc")
		   val (env1, css1) = f_strdec strdec1
		   val (env2, css2) = f_strdec strdec2
		   val ev1  = E.freshEnvVar ()
		   val ev2  = E.freshEnvVar ()
		   val c1   = E.initEnvConstraint (E.consENV_VAR ev1 L.dummyLab) env1 L.dummyLab
		   val c2   = E.initEnvConstraint (E.consENV_VAR ev2 lab) (E.consENV_VAR ev1 lab) lab
		   val env3 = E.CONSTRAINT_ENV (E.consConstraint(L.dummyLab, c1) (E.singleConstraint (lab, c2)))
		   val env4 = E.ENVDEP (EL.initExtLab (E.consENV_VAR ev2 lab) lab)
		   val env5 = E.ROW_ENV (env3, E.LOCAL_ENV (env4, env2))
	       in (env5 (*E.LOCAL_ENV (env1, env2)*), E.unionContextSensitiveSyntaxErrors [css1, css2])
	       end
	     | f_strdecone (A.StrDecOneFun (funbind, _, lab, _)) = f_funbind funbind
	     | f_strdecone (A.StrDecOneDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.envvar, Env.cst, Env.css) *)
	   and f_labstrexp (A.LabStrExp (strexp, _, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.LabStrExp")
		   val (ev, cst, css) = f_strexp strexp
		   val ev' = E.freshEnvVar ()
		   val c   = E.initEnvConstraint (E.consENV_VAR ev' lab) (E.consENV_VAR ev lab) lab
	       in (ev', E.consConstraint(lab, c) cst, css)
	       end
	     | f_labstrexp (A.LabStrExpDots pl) =
	       let val ev  = E.freshEnvVar ()
		   val env = f_partlist pl
	       in (ev, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.envvar, Env.cst, Env.css) *)
	   and f_strexp (A.StrExpBasic (strdec, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.StrExpBasic")
		   val (env, css) = f_strdec strdec
		   val env = E.updateILab lab env
		   val ev1 = E.freshEnvVar ()
		   val ev2 = E.freshEnvVar ()
		   val c1  = E.initEnvConstraint (E.consENV_VAR ev1 lab) (E.consENV_VAR ev2 lab) lab
		   val c2  = E.initEnvConstraint (E.consENV_VAR ev2 L.dummyLab) env L.dummyLab
	       in (ev1, E.consConstraint(lab, c1) (E.singleConstraint (L.dummyLab, c2)), css)
	       end
	     | f_strexp (A.StrExpId (longstrid, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.StrExpId")
		   val (ev, cst) = f_longstrid longstrid
		   val ev' = E.freshEnvVar ()
		   val c   = E.initEnvConstraint (E.consENV_VAR ev' lab) (E.consENV_VAR ev lab) lab
	       in (ev', E.consConstraint(lab, c) cst, E.emptyContextSensitiveSyntaxError)
	       end
	     | f_strexp (A.StrExpOp (labstrexp, labsigexp, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.StrExpOp")
		   val (ev1, cst1, css1) = f_labstrexp labstrexp
		   val (ev2, cst2, css2) = f_labsigexp labsigexp
		   val ev  = E.freshEnvVar ()
		   val cst = E.unionConstraintsList [cst1, cst2]
		   val css = E.unionContextSensitiveSyntaxErrors [css1, css2]
		   val c   = E.SIGNATURE_CONSTRAINT (ev2, NONE, ev1, SOME ev, lab)
		   val env = E.ROW_ENV (E.CONSTRAINT_ENV cst, E.CONSTRAINT_ENV (E.singleConstraint (lab, c)))
	       in (ev, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), css)
	       end
	     | f_strexp (A.StrExpTr (labstrexp, labsigexp, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.StrExpTr")
		   val (ev1, cst1, css1) = f_labstrexp labstrexp
		   val (ev2, cst2, css2) = f_labsigexp labsigexp
		   val ev  = E.freshEnvVar ()
		   val cst = E.unionConstraintsList [cst1, cst2]
		   val css = E.unionContextSensitiveSyntaxErrors [css1, css2]
		   val c   = E.SIGNATURE_CONSTRAINT (ev2, SOME ev, ev1, NONE, lab)
		   val env = E.ROW_ENV (E.CONSTRAINT_ENV cst, E.CONSTRAINT_ENV (E.singleConstraint (lab, c)))
	       in (ev, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), css)
	       end
	     | f_strexp (A.StrExpFExp (funid, labstrexp, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.StrExpFExp")
		   val (ev0, ev1, cst1) = f_funid funid
		   val (ev2, cst2, css) = f_labstrexp labstrexp
		   val ev  = E.freshEnvVar ()
		   val c   = E.FUNCTOR_CONSTRAINT (ev0, ev1, ev2, ev, lab)
		   val env = E.ROW_ENV (E.CONSTRAINT_ENV (E.unionConstraintsList [cst1, cst2]), E.CONSTRAINT_ENV (E.singleConstraint (lab, c)))
	       in (ev, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), css)
	       end
	     | f_strexp (A.StrExpFDec (funid, strdec, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.StrExpFDec")
		   val (ev1, ev2, cst) = f_funid funid
		   val (env, css) = f_strdec strdec
		   val ev   = E.freshEnvVar ()
		   val ev'  = E.freshEnvVar ()
		   val c1   = E.initEnvConstraint (E.consENV_VAR ev' lab) env lab
		   val c2   = E.FUNCTOR_CONSTRAINT (ev1, ev2, ev', ev, lab)
		   val env1 = E.ROW_ENV (E.CONSTRAINT_ENV (E.singleConstraint (lab, c1)), E.CONSTRAINT_ENV (E.singleConstraint (lab, c2)))
		   val env2 = E.ROW_ENV (E.CONSTRAINT_ENV cst, env1)
	       in (ev, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env2), css)
	       end
	     | f_strexp (A.StrExpLocal (strdec, labstrexp, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.StrExpLocal")
		   val (env1, css1) = f_strdec strdec
		   val (ev, cst2, css2) = f_labstrexp labstrexp
		   val ev' = E.freshEnvVar ()
		   val c   = E.initEnvConstraint (E.consENV_VAR ev' lab) (E.consENV_VAR ev lab) lab
		   val env = E.ROW_ENV (env1, E.CONSTRAINT_ENV cst2)
		   val cst = E.consConstraint(L.dummyLab, E.LET_CONSTRAINT env) (E.singleConstraint (lab, c))
		   val css = E.unionContextSensitiveSyntaxErrors [css1, css2]
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
		   NONE => (T.freshTypeFunctionVar (), E.emptyEnv, E.emptyConstraint)
		 | SOME lid =>
		   let val lab = I.getLabId lid
		       val typeFunctionVar = T.freshTypeFunctionVar ()
		       val (cst, env) = E.genLongEnv lid (T.consTYPE_FUNCTION_VAR typeFunctionVar)
		   in (typeFunctionVar, env, cst)
		   end

	   (* RETURNS: (Id.lid option) *)
	   and f_longtyconwhere longtycon =
	       case A.longtyconToLid longtycon of
		   NONE => NONE
		 | SOME lid => SOME lid

	   (* RETURNS: (Id.lid option, Ty.rowVar, Env.env, Env.cst) *)
	   and f_ldatname (A.LDatName (typevarseq, longtycon, _, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.LDatName")
		   val (sv, tyvs, cst1) = f_typevarseq typevarseq
		   val lidop = f_longtyconwhere longtycon
	       in (lidop, sv, tyvs, cst1)
	       end
	     | f_ldatname A.LDatNameDots =
	       let val sv  = T.freshRowVar ()
	       in (NONE, sv, E.emtv, E.emptyConstraint)
	       end

	   (* RETURNS: (Id.lid option, Ty.tyfvar, Env.cst, Env.css) *)
	   and f_ltreadescone (A.LTReaDOne (ldatname, labtyp, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.LTReaDOne")
		   val (lidop, sv, tyvs, cst1) = f_ldatname ldatname
		   val (tv, eqtv, cst2, css) = f_labtype labtyp
		   val typeFunctionVar = T.freshTypeFunctionVar ()
		   val c1  = E.initFunctionTypeConstraint (T.TYPE_FUNCTION_VAR typeFunctionVar) (T.TFC (T.ROW_VAR sv, T.consTYPE_VAR tv, lab)) lab
		   val c2  = E.LET_CONSTRAINT (E.ROW_ENV (E.CONSTRAINT_ENV cst1, E.ROW_ENV (E.projExplicitTypeVars tyvs, E.CONSTRAINT_ENV cst2)))
		   (*val cs3 = checkTypeVarInc (A.gettypeVarLDatName dn) (A.getlabLDatName dn) (A.gettypeVarLabType ty)*)
	       in (lidop, typeFunctionVar, E.consConstraint(lab, c1) (E.singleConstraint (L.dummyLab, c2)), css)
	       end
	     | f_ltreadescone (A.LTReaDOneDots pl) =
	       let val env = f_partlist pl
		   val typeFunctionVar = T.freshTypeFunctionVar ()
	       in (NONE, typeFunctionVar, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.longtyp list, Env.cst, Env.css) *)
	   and f_ltreadesc (A.LTReaDesc (ltreadescs, _, lab, _)) =
	       (D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.LTReaDesc");
	       foldr (fn (ltreadesc, (reas, cst, css)) =>
			 case f_ltreadescone ltreadesc of
			     (SOME lid, typeFunctionVar, cst0, css0) =>
			     let val longid = {lid = lid, sem = T.consTYPE_FUNCTION_VAR typeFunctionVar, class = CL.consTYCON (), lab = lab}
				 val cst1 = E.unionConstraintsList [cst, cst0]
				 val css1 = E.unionContextSensitiveSyntaxErrors [css, css0]
			     in ((longid, L.singleton lab, L.empty, CD.empty) :: reas, cst1, css1)
			     end
			   | (NONE, _, cst0, css0) => (reas, E.unionConstraintsList [cst, cst0], E.unionContextSensitiveSyntaxErrors [css, css0]))
		     ([], E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
		     ltreadescs)
	     | f_ltreadesc (A.LTReaDescDots pl) =
	       let val env = f_partlist pl
	       in ([], E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.envvar, Env.cst, Env.css) *)
	   and f_labsigexp (A.LabSigExp (sigexp, _, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.LabSigExp")
		   val (ev, cst, css) = f_sigexp sigexp
		   val ev' = E.freshEnvVar ()
		   val c   = E.initEnvConstraint (E.consENV_VAR ev' lab) (E.consENV_VAR ev lab) lab
	       in (ev', E.consConstraint(lab, c) cst, css)
	       end
	     | f_labsigexp (A.LabSigExpDots pl) =
	       let val ev  = E.freshEnvVar ()
		   val env = f_partlist pl
	       in (ev, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.envvar, Env.cst, Env.css) *)
	   and f_sigexp (A.SigExpBasic (spec, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.StrExpBasic")
		   val (env, css) = f_spec spec
		   val env = E.updateILab lab env
		   val ev1 = E.freshEnvVar ()
		   val ev2 = E.freshEnvVar ()
		   val c1  = E.initEnvConstraint (E.consENV_VAR ev1 L.dummyLab) env L.dummyLab
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
		   val c2  = E.initEnvConstraint (E.consENV_VAR ev2 lab) (E.consENV_VAR ev1 lab) lab
	       in (ev2, E.consConstraint(L.dummyLab, c1) (E.singleConstraint (lab, c2)), css)
	       end
	     | f_sigexp (A.SigExpId (sigid, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.StrExpId")
		   val (ev, cst) = f_sigid sigid
		   val ev' = E.freshEnvVar ()
		   val c   = E.initEnvConstraint (E.consENV_VAR ev' lab) (E.consENV_VAR ev lab) lab
	       in (ev', E.consConstraint(lab, c) cst, E.emptyContextSensitiveSyntaxError)
	       end
	     (*| f_sigexp (A.SigExpRea (labsigexp, rea, _, lab, _)) =
	       let val (ev1, cst1, css1) = f_labsigexp labsigexp
		   (*(2010-07-07)We shouldn't do that for the where clauses as they are in fact
		    * rows of where clauses and here we treat them as a bloc without any
		    * precedence, which is going to pose problem at constraint solving. *)
		   val (env2, cst2, css2) = f_ltreadesc rea
		   val ev  = E.freshEnvVar ()
		   val ev' = E.freshEnvVar ()
		   val c1  = E.initEnvConstraint (E.consENV_VAR ev lab) (E.ROW_ENV (E.CONSTRAINT_ENV cst2, env2)) lab
		   val c2  = E.SIGNATURE_CONSTRAINT (ev1, SOME ev', ev, NONE, lab, E.WHR)
		   val env = E.ROW_ENV (E.CONSTRAINT_ENV (E.consConstraint(lab, c1) cst1), E.CONSTRAINT_ENV (E.singleConstraint (lab, c2)))
	       in (ev', E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.unionContextSensitiveSyntaxErrors [css1, css2])
	       end*)
	     | f_sigexp (A.SigExpRea (labsigexp, rea, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.SigExpRea")
		   val (ev1, cst1, css1) = f_labsigexp labsigexp
		   val (reas, cst2, css2) = f_ltreadesc rea
		   val ev   = E.freshEnvVar ()
		   val ev'  = E.freshEnvVar ()
		   val cst  = E.unionConstraintsList [cst1, cst2]
		   val css  = E.unionContextSensitiveSyntaxErrors [css1, css2]
		   val c1   = E.initEnvConstraint (E.consENV_VAR ev' lab) (E.consENV_VAR ev1 lab) lab
		   val env1 = foldl (fn (rea, env) => E.ENVWHR (env, rea)) (E.consENV_VAR ev' lab) reas
		   val env2 = E.ROW_ENV (E.CONSTRAINT_ENV (E.consConstraint(lab, c1) cst), env1)
		   val c2   = E.initEnvConstraint (E.consENV_VAR ev lab) env2 lab
	       in (ev, E.singleConstraint (lab, c2), css)
	       end
	     | f_sigexp (A.SigExpDots pl) =
	       let val env = f_partlist pl
		   val ev  = E.freshEnvVar ()
	       in (ev, E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env), E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.sigenv, Env.cst, Env.css) *)
	   and f_sigbindone (A.SigBindOne (sigid, labsigexp, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.SigBindOne")
		   val (ev1, sigs, cst1) = f_sigidbind sigid
		   val (ev2, cst2, css2) = f_labsigexp labsigexp
		   val c = E.initEnvConstraint (E.consENV_VAR ev1 lab) (E.consENV_VAR ev2 lab) lab
	       in (sigs, E.consConstraint(lab, c) (E.unionConstraintsList [cst1, cst2]), css2)
	       end
	     | f_sigbindone (A.SigBindOneDots pl) =
	       let val env = f_partlist pl
	       in (E.getSigs env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.sigenv, Env.cst, Env.css) *)
	   and f_sigbind (A.SigBind (sigbinds, _, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.SigBind")
		   val (sigss, csts, csss) = unzipThree (map f_sigbindone sigbinds)
		   val sigs = E.unionEnvList sigss
		   val cst  = E.unionConstraintsList csts
		   val css  = E.unionContextSensitiveSyntaxErrors csss
	       in (sigs, cst, css)
	       end
	     | f_sigbind (A.SigBindDots pl) =
	       let val env = f_partlist pl
	       in (E.getSigs env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.funenv, Env.cst, Env.css) *)
	   and f_funbindone (A.FunBindO (funid, strid, labsigexp, labstrexp, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.FunBindO")
		   val (ev0, ev1, funs, cst1) = f_funidbind funid
		   val (ev2, strs, cst2, css2) = f_strid strid
		   val (ev3, cst3, css3) = f_labsigexp labsigexp
		   val (ev4, cst4, css4) = f_labstrexp labstrexp
		   val c1   = E.initEnvConstraint (E.consENV_VAR ev2 lab) (E.consENV_VAR ev3 lab) lab (* strid has signature labsigexp            *)
		   val c2   = E.initEnvConstraint (E.consENV_VAR ev0 lab) (E.consENV_VAR ev2 lab) lab (* the functor takes the strid as parameter *)
		   val c3   = E.initEnvConstraint (E.consENV_VAR ev1 lab) (E.consENV_VAR ev4 lab) lab (* the functor returns the labstrexp        *)
		   val env0 = E.updateIArgOfFunctor true (E.projStructs strs) (* We mark the environment as being the parameter of a functor *)
		   val env1 = E.CONSTRAINT_ENV (E.consConstraint(lab, c1) (E.unionConstraintsList [cst2, cst3]))
		   val env2 = E.ROW_ENV (E.ROW_ENV (env1, env0), E.CONSTRAINT_ENV cst4)
		   val cst  = E.consConstraint(L.dummyLab, E.LET_CONSTRAINT env2) (E.conscsts (lab, [c2, c3]) cst1)
		   val css  = E.unionContextSensitiveSyntaxErrors [css2, css3, css4]
	       in (funs, cst, css)
	       end
	     | f_funbindone (A.FunBindOO (funid, strid, labsigexp1, labsigexp2, labstrexp, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.FunBindOO")
		   val (ev0, ev1, funs, cst1) = f_funidbind funid
		   val (ev2, strs, cst2, css2) = f_strid strid
		   val (ev3, cst3, css3) = f_labsigexp labsigexp1
		   val (ev4, cst4, css4) = f_labsigexp labsigexp2
		   val (ev5, cst5, css5) = f_labstrexp labstrexp
		   val c1   = E.initEnvConstraint (E.consENV_VAR ev2 lab) (E.consENV_VAR ev3 lab) lab (* strid has signature labsigexp            *)
		   val c2   = E.initEnvConstraint (E.consENV_VAR ev0 lab) (E.consENV_VAR ev2 lab) lab (* the functor takes the strid as parameter *)
		   val c3   = E.SIGNATURE_CONSTRAINT (ev4, NONE, ev5, SOME ev1, lab)                       (* the functor returns the labstrexp        *)
		   val env0 = E.updateIArgOfFunctor true (E.projStructs strs) (* We mark the environment as being the parameter of a functor *)
		   val env1 = E.CONSTRAINT_ENV (E.consConstraint(lab, c1) (E.unionConstraintsList [cst2, cst3]))
		   val env2 = E.CONSTRAINT_ENV (E.unionConstraintsList [cst4, cst5])
		   val env3 = E.CONSTRAINT_ENV (E.conscsts (lab, [c2, c3]) cst1)
		   val env4 = E.ROW_ENV (E.ROW_ENV (env1, env0), E.ROW_ENV (env2, env3))
		   val cst  = E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env4)
		   val css  = E.unionContextSensitiveSyntaxErrors [css2, css3, css4, css5]
	       in (funs, cst, css)
	       end
	     | f_funbindone (A.FunBindOT (funid, strid, labsigexp1, labsigexp2, labstrexp, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.FunBindOT")
		   val (ev0, ev1, funs, cst1) = f_funidbind funid
		   val (ev2, strs, cst2, css2) = f_strid strid
		   val (ev3, cst3, css3) = f_labsigexp labsigexp1
		   val (ev4, cst4, css4) = f_labsigexp labsigexp2
		   val (ev5, cst5, css5) = f_labstrexp labstrexp
		   val c1   = E.initEnvConstraint (E.consENV_VAR ev2 lab) (E.consENV_VAR ev3 lab) lab (* strid has signature labsigexp            *)
		   val c2   = E.initEnvConstraint (E.consENV_VAR ev0 lab) (E.consENV_VAR ev2 lab) lab (* the functor takes the strid as parameter *)
		   val c3   = E.SIGNATURE_CONSTRAINT (ev4, SOME ev1, ev5, NONE, lab)                       (* the functor returns the labstrexp        *)
		   val env0 = E.updateIArgOfFunctor true (E.projStructs strs) (* We mark the environment as being the parameter of a functor *)
		   val env1 = E.CONSTRAINT_ENV (E.consConstraint(lab, c1) (E.unionConstraintsList [cst2, cst3]))
		   val env2 = E.CONSTRAINT_ENV (E.unionConstraintsList [cst4, cst5])
		   val env3 = E.CONSTRAINT_ENV (E.conscsts (lab, [c2, c3]) cst1)
		   val env4 = E.ROW_ENV (E.ROW_ENV (env1, env0), E.ROW_ENV (env2, env3))
		   val cst  = E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT env4)
		   val css  = E.unionContextSensitiveSyntaxErrors [css2, css3, css4, css5]
	       in (funs, cst, css)
	       end
	     | f_funbindone (A.FunBindOS (funid, spec, labstrexp, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.FunBindOS")
		   val (ev0, ev1, funs, cst1) = f_funidbind funid
		   val (env, css2) = f_spec spec
		   val (ev3, cst3, css3) = f_labstrexp labstrexp
		   val ev2  = E.freshEnvVar ()
		   val env' = E.updateIArgOfFunctor true env (* We mark the environment as being the parameter of a functor *)
		   val c1   = E.initEnvConstraint (E.consENV_VAR ev2 lab) env' lab                   (* bind the specs to ev2                   *)
		   val c2   = E.initEnvConstraint (E.consENV_VAR ev0 lab) (E.consENV_VAR ev2 lab) lab (* the functor takes the spec as parameter *)
		   val c3   = E.initEnvConstraint (E.consENV_VAR ev1 lab) (E.consENV_VAR ev3 lab) lab (* the functor returns the labstrexp       *)
		   val env1 = E.ROW_ENV (E.CONSTRAINT_ENV (E.singleConstraint (lab, c1)), E.consENV_VAR ev2 lab)
		   val env2 = E.ROW_ENV (env1, E.CONSTRAINT_ENV cst3)
		   val cst  = E.consConstraint(L.dummyLab, E.LET_CONSTRAINT env2) (E.conscsts (lab, [c2, c3]) cst1)
		   val css  = E.unionContextSensitiveSyntaxErrors [css2, css3]
	       in (funs, cst, css)
	       end
	     | f_funbindone (A.FunBindOSO (funid, spec, labsigexp, labstrexp, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.FunBindOSO")
		   val (ev0, ev1, funs, cst1) = f_funidbind funid
		   val (env, css2) = f_spec spec
		   val (ev3, cst3, css3) = f_labsigexp labsigexp
		   val (ev4, cst4, css4) = f_labstrexp labstrexp
		   val ev2  = E.freshEnvVar ()
		   val env' = E.updateIArgOfFunctor true env (* We mark the environment as being the parameter of a functor *)
		   val c1   = E.initEnvConstraint (E.consENV_VAR ev2 lab) env' lab                   (* bind the specs to ev2                   *)
		   val c2   = E.initEnvConstraint (E.consENV_VAR ev0 lab) (E.consENV_VAR ev2 lab) lab (* the functor takes the spec as parameter *)
		   val c3   = E.SIGNATURE_CONSTRAINT (ev3, NONE, ev4, SOME ev1, lab)                       (* the functor returns the labstrexp       *)
		   val env1 = E.ROW_ENV (E.CONSTRAINT_ENV (E.singleConstraint (lab, c1)), E.consENV_VAR ev2 lab)
		   val env2 = E.CONSTRAINT_ENV (E.unionConstraintsList [cst3, cst4])
		   val env3 = E.CONSTRAINT_ENV (E.conscsts (lab, [c2, c3]) cst1)
		   val env4 = E.ROW_ENV (env2, env3)
		   val cst  = E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT (E.ROW_ENV (env1, env4)))
		   val css  = E.unionContextSensitiveSyntaxErrors [css2, css3, css4]
	       in (funs, cst, css)
	       end
	     | f_funbindone (A.FunBindOST (funid, spec, labsigexp, labstrexp, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.FunBindOST")
		   val (ev0, ev1, funs, cst1) = f_funidbind funid
		   val (env, css2) = f_spec spec
		   val (ev3, cst3, css3) = f_labsigexp labsigexp
		   val (ev4, cst4, css4) = f_labstrexp labstrexp
		   val ev2  = E.freshEnvVar ()
		   val env' = E.updateIArgOfFunctor true env (* We mark the environment as being the parameter of a functor *)
		   val c1   = E.initEnvConstraint (E.consENV_VAR ev2 lab) env' lab                   (* bind the specs to ev2                   *)
		   val c2   = E.initEnvConstraint (E.consENV_VAR ev0 lab) (E.consENV_VAR ev2 lab) lab (* the functor takes the spec as parameter *)
		   val c3   = E.SIGNATURE_CONSTRAINT (ev3, SOME ev1, ev4, NONE, lab)                       (* the functor returns the labstrexp       *)
		   val env1 = E.ROW_ENV (E.CONSTRAINT_ENV (E.singleConstraint (lab, c1)), E.consENV_VAR ev2 lab)
		   val env2 = E.CONSTRAINT_ENV (E.unionConstraintsList [cst3, cst4])
		   val env3 = E.CONSTRAINT_ENV (E.conscsts (lab, [c2, c3]) cst1)
		   val env4 = E.ROW_ENV (env2, env3)
		   val cst  = E.singleConstraint (L.dummyLab, E.LET_CONSTRAINT (E.ROW_ENV (env1, env4)))
		   val css  = E.unionContextSensitiveSyntaxErrors [css2, css3, css4]
		   (*val _    = D.printdebug2 ("[functor]")*)
	       in (funs, cst, css)
	       end
	     | f_funbindone (A.FunBindODots parts) =
	       let val env = f_partlist parts
	       in (E.getFunctors env, E.emptyConstraint, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.funenv, Env.cst, Env.css) *)
	   and f_funbind (A.FunBind (funbinds, _, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.FunBind")
		   val (funss, csts, csss) = unzipThree (map f_funbindone funbinds)
		   val funs = E.unionEnvList funss
		   val cst  = E.unionConstraintsList csts
		   val css  = E.unionContextSensitiveSyntaxErrors csss
	       in (E.ROW_ENV (E.CONSTRAINT_ENV cst, E.projFunctors funs), css)
	       end
	     | f_funbind (A.FunBindDots parts) =
	       let val env = f_partlist parts
	       in (env, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.env, Env.cst, Env.css) *)
	   and f_sigdec (A.SigDec (sigbind, _, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.SigDec")
		   val (sigs, cst, css) = f_sigbind sigbind
		   val env = E.ROW_ENV (E.CONSTRAINT_ENV cst, E.projSigs sigs)
	       in (env, css)
	       end
	     | f_sigdec (A.SigDecDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (Env.css) *)
	   and f_afile (A.AFile (file, _, lab, _)) =
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.AFile")
		   val css = if OS.FileSys.access (file, [OS.FileSys.A_READ])
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
	     | f_smltes (A.SmlTesUse  (af, _, _)) = (E.emptyEnv, f_afile af)
	     | f_smltes (A.SmlTesSBas (af, _, _)) = (E.emptyEnv, f_afile af)
	     | f_smltes (A.SmlTesCBas _)  = (E.emptyEnv, E.emptyContextSensitiveSyntaxError)
	     | f_smltes (A.SmlTesQuote _) = (E.emptyEnv, E.emptyContextSensitiveSyntaxError)
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
	   and f_topdeconelist [] = (E.emptyEnv, E.emptyContextSensitiveSyntaxError)
	     | f_topdeconelist [x] = f_topdecone x
	     | f_topdeconelist (x :: xs) =
	       let val (env1, css1) = f_topdecone x
		   val (env2, css2) = f_topdeconelist xs
	       in (E.ROW_ENV (env1, env2), E.unionContextSensitiveSyntaxErrors [css1, css2])
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
	       let val _   = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("generating constraints for A.ProgOneExp")
		   (* this is the type variable that's used in the accessor
		    * we use this later on when creating a type constraint
		    * why do we need this later on? How can I use this type variable?
		    * Is it through this type variable that the accessors know what they are accessing?
		    *)
		   val (tv, cst1, css) = f_exp exp
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("ProgOneExp cst1 is: \n"^(E.printConstraints cst1))
		   val (tyvs, cst2) = f_typevarvallist (A.getTypeVarExp exp)
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("ProgOneExp cst2 is: \n"^(E.printConstraints cst2))
		   val tv'  = T.freshTypeVar ()
		   val c    = E.initTypeConstraint (T.consTYPE_VAR tv') (T.consTYPE_VAR tv) lab
		   val cst  = E.consConstraint(lab, c) cst1
		   val vids = E.consSingleEnv (v, [E.consBindPoly {id=v,
								   typeOfId=(T.consTYPE_VAR tv'),
								   classOfId=(CL.consVAL ()),
								   labelOfConstraint=lab}])
		   val _ = D.printDebugFeature D.AZE D.CONSTRAINT_GENERATION ("ProgOneExp constraints in env1 are: \n"^(E.printConstraints cst))
		   val env1 = E.ROW_ENV (E.CONSTRAINT_ENV cst, E.projValueIds (E.closeValueIds vids (V.nonexpExp exp)))
		   val env2 = E.ROW_ENV (E.CONSTRAINT_ENV cst2, E.ENVPOL (tyvs, env1))
	       in (env2, css)
	       end
	     | f_progone (A.ProgOneParse (s, _, lab, _)) =
	       (* Here we generate an environmnet variable because the file is not parsable
		* and it might generate binding errors. *)
	       (E.newEnvVar lab, E.singcss (E.CSSPARS (L.singleton lab, s)))
	     | f_progone (A.ProgOneFile (af, _)) = (E.emptyEnv, f_afile af)
	     | f_progone (A.ProgOneDots pl) =
	       let val env = f_partlist pl
	       in (env, E.emptyContextSensitiveSyntaxError)
	       end

	   (* RETURNS: (E.env, Env.css) *)
	   and f_progonelist []  =  (E.emptyEnv, E.emptyContextSensitiveSyntaxError)
	     | f_progonelist [x] = f_progone x
	     | f_progonelist (x :: xs) =
	       let val (env1, css1) = f_progone x
		   val (env2, css2) = f_progonelist xs
	       in (E.ROW_ENV (env1, env2), E.unionContextSensitiveSyntaxErrors [css1, css2])
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
	   and f_proglist [] = (E.emptyEnv, E.emptyContextSensitiveSyntaxError)
	     | f_proglist [(x, file, false, _)] =
	       let val (env, css) = f_prog x
	       in (f_progfile file env E.emptyEnv, css)
	       end
	     | f_proglist [(x, file, true, _)] =
	       if benv
	       then let val _ = setBasis true
			val (env, css) = f_prog x
			val _ = setBasis false
		    in (f_progfile file env E.emptyEnv, css)
		    end
	       else (E.emptyEnv, E.emptyContextSensitiveSyntaxError)
	     | f_proglist ((x, file, false, _) :: xs) =
	       let val (env1, css1) = f_prog x
		   val (env2, css2) = f_proglist xs
		   val env = f_progfile file env1 env2
	       in (env, E.unionContextSensitiveSyntaxErrors [css1, css2])
	       end
	     | f_proglist ((x, file, true, _) :: xs) =
	       if benv
	       then let val _ = setBasis true
			val skalpelDebugValue = !(D.debug)
			val _ = D.debug := false
			val (env1, css1) = f_prog x
			val _ = setBasis false
			val _ = D.debug := skalpelDebugValue
			val (env2, css2) = f_proglist xs
			val env = f_progfile file env1 env2
		    in (env, E.unionContextSensitiveSyntaxErrors [css1, css2])
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
		    let val ty1  = T.newTYPE_VAR ()
			val ty2  = T.constybool' L.dummyLab T.BUILTIN_BASIS_CONS
			val c    = E.initTypeConstraint ty1 ty2 L.dummyLab
			val bind = E.consBindPoly {id=id,
						   typeOfId=ty1,
						   classOfId=class0,
						   labelOfConstraint=L.dummyLab}
		    in (bind, c)
		    end
		  | bindOne (id, "false") =
		    let val ty1  = T.newTYPE_VAR ()
			val ty2  = T.constybool' L.dummyLab T.BUILTIN_BASIS_CONS
			val c    = E.initTypeConstraint ty1 ty2 L.dummyLab
			val bind = E.consBindPoly {id=id,
						   typeOfId=ty1,
						   classOfId=class0,
						   labelOfConstraint=L.dummyLab}
		    in (bind, c)
		    end
		  | bindOne (id, "nil") =
		    let val ty1  = T.newTYPE_VAR ()
			val ty2  = T.constylist' (T.freshTypeVar ()) L.dummyLab T.BUILTIN_BASIS_CONS
			val c    = E.initTypeConstraint ty1 ty2 L.dummyLab
			val bind = E.consBindPoly {id=id,
						   typeOfId=ty1,
						   classOfId=class0,
						   labelOfConstraint=L.dummyLab}
		    in (bind, c)
		    end
		  | bindOne (id, "ref") =
		    let val tv   = T.freshTypeVar ()
			val ty1  = T.newTYPE_VAR ()
			val ty2  = T.consTyArrowTy (T.consTYPE_VAR tv) (T.constyref' tv L.dummyLab T.BUILTIN_BASIS_CONS) L.dummyLab T.BUILTIN_BASIS_CONS
			val c    = E.initTypeConstraint ty1 ty2 L.dummyLab
			val bind = E.consBindPoly {id=id,
						   typeOfId=ty1,
						   classOfId=class1,
						   labelOfConstraint=L.dummyLab}
		    in (bind, c)
		    end
		  | bindOne _ = raise EH.DeadBranch "this constructor can be rebound"
		val (binds, cs) = ListPair.unzip (map bindOne inascid)
		val env' = if List.null binds
			   then env
			   else let val env1 = E.projValueIds (E.bindToEnv binds)
				    val cst  = E.singcsts (L.dummyLab, cs)
				in E.ROW_ENV (E.ROW_ENV (E.CONSTRAINT_ENV cst, E.ENVPOL (E.emtv, env1)), env)
				end
	    in env'
	    end

	   val (env, css) = f_progs prog
	   val env' = bindPcon env

    in (env', css)
    end

(* the int in generateConstraints is:
 * 0 is we don't want any initial environment at all
 * 1 if we want the builtin environment
 * 2 if we want to use the basis.sml environment *)
fun generateConstraints prog nenv = generateConstraints' prog (I.emAssoc, false) nenv


(**************************************)
(*           'FULL' ANALYZE           *)
(**************************************)

(* ths integer is as for generateConstraints *)
fun fullConsGen progs ascid nenv =
    let val benv    = case nenv of 1 => true | 2 => true | _ => false
	val pack1   = (ascid, true)
	val pack2   = ([], false)
	val envcss1 = generateConstraints' progs pack1 nenv
	val envcss2 = buildin envcss1 ascid benv
    in envcss2
    end


end
