(* Copyright 2009 2010 2012 Heriot-Watt University
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
 *  o Date:        24 May 2010
 *  o File name:   Minimisation.sml
 *  o Description: Defines the Min structure which has signature MIN
 *      and which is the structure containing our minimisation algo.
 *)


structure Min :> MIN = struct

(* shorten the names of the structures that we are going to use *)
structure A   = AstSML
structure L   = Label
structure D   = Debug
structure S   = Slicing
structure E   = Env
structure FI  = Filter
structure EH  = ErrorHandler
structure CD  = LongId
structure ST  = StateEnv
structure U   = Unif(ST)
structure EK  = ErrorKind
structure VT  = VTimer
structure ERR = Error

val currentBackground = ref (NONE : (VT.timer * ERR.export' * E.envContextSensitiveSyntaxPair * A.packs * int) option)
(* error being minimized *)
val currentError = ref (NONE : (ERR.error * LargeInt.int * int) option)

(* sets the currentError ref given an error and a counter for the error *)
fun setCurrentError error counter =
    case !currentBackground of
	SOME (timer, funout, env, parse, size) =>
	let val time = VT.getMilliTime timer
	in currentError := SOME (error, time, counter)
	end
      | _ => ()

fun setCurrentBackground timer (SOME funout) env parse error counter =
    (currentBackground := SOME (timer, funout, env, parse, L.length (ERR.getL error));
     setCurrentError error counter)
  | setCurrentBackground timer NONE env parse error counter = ()

fun getCurrentErrorCounter () =
    case !currentError of
	SOME (error, time, counter) => SOME counter
      | NONE => NONE

fun reportError error =
    case (!currentBackground, !currentError) of
	(SOME (timer, funout, env, parse as (ast, _, _), size), SOME (err, time, counter)) =>
	let val time' = VT.getMilliTime timer
	    val size1 = L.length (ERR.getL error)
	    val size2 = L.length (ERR.getL err)
	in if (time' - time > 20000 andalso size1 < size2) (* either 20s have passed *)
	      orelse
	      (time' - time > 1000 andalso 100 - ((size1 * 100) div size2) > 20) (* or the error is 20% smaller *)
	   then let val id   = ERR.freshError ()
		    val err' = ERR.setT (ERR.setReg (ERR.setSlice ast (ERR.setE (ERR.setI error id) [ERR.getI err])) true) time'
		    val _    = funout [err'] parse env counter (LargeInt.toInt time')
		    val _    = setCurrentError err' (counter + 1)
		in true
		end
	   else false
	end
      | _ => false

fun finalError error counter =
    case !currentError of
	SOME (err, time, counter) =>
	let val id1 = ERR.freshError ()
	    val id2 = ERR.getI err
	in (ERR.setE (ERR.setI error id1) [id2], counter)
	end
      | _ => (error, counter)

fun lazyunbind4 [] _ labs keep err _ = (labs, keep, err)
  | lazyunbind4 (x :: xs) css labs keep err ast =
    let (*val _ = D.printdebug2 ("[lazy]")*)
	val (labs', keep', err') =
	    if L.disjoint labs x orelse L.subseteq x keep
	    then (labs, keep, err)
	    else case U.unif css (FI.cons (SOME labs) (SOME x)) (U.MIN err) of
		     U.Success _ =>
		     if L.isSingle x
		     then (labs, L.union x keep, err)
		     else (labs, keep, err)
		   | U.Error (err, _) =>
		     ((*D.printdebug2 (ERR.printOneXmlErr (ERR.setSlice ast err) "" true ^ "\n" ^
				     L.toString x ^ "\n" ^
				     L.toString labs ^ "\n" ^
				     L.toString (ERR.getL err));*)
		      reportError err;
		      (L.inter (ERR.getL err) labs, keep, err))
    in lazyunbind4 xs css labs' keep' err' ast
    end

fun lazyunbind bindings css labs err ast = lazyunbind4 bindings css labs (L.empty ()) err ast

fun select1 labs = L.remFirst labs

(* A random selection of the next label to try to eliminate *)
fun select2 labs =
    let val n = L.fromInt (Int.fromLarge (Time.toSeconds (Time.now ())))
    in if L.isin n labs
       then (SOME n, L.delete n labs)
       else select1 labs
    end
    handle Overflow => select1 labs

fun select labs = select1 labs

(* above is the old implementation of reduce1 *)
fun reduce1 css done todo err =
    let val (lop, ll) = select todo
    in case lop of
	   NONE => (done, err)
	 | SOME l =>
	   let val totest  = L.union done ll
	       val filters = FI.cons (SOME totest) (SOME (L.singleton l))
	   in case U.unif css filters (U.MIN err) of
		  U.Error (err, _) =>
		  ((*D.printdebug2 ("[" ^ L.printelt l ^ "]" ^ "removed");*)
		   reportError err;
		   reduce1 css done (L.inter (ERR.getL err) ll) err)
		| U.Success state =>
		  ((*D.printdebug2 ("[" ^ L.printelt l ^ "]" ^ "kept");*)
		   reduce1 css (L.cons l done) ll err)
	   end
    end

fun reduce css done todo err = reduce1 css done todo err

(* in todo we should test first the labels in the csbindings *)
(* it does not seem to work quite well *)
fun minimize4 err (envContextSensitiveSyntaxPair as (env, css)) lazy (parse as (ast, _, _)) timer export counter =
    let val _        = setCurrentBackground timer export envContextSensitiveSyntaxPair parse err counter
	val labs     = ERR.getL err
	val done     = (L.empty ()) (*EK.getLabsEk (ERR.getK err)*)
	val filters1 = FI.cons (SOME labs) NONE
	(*val _        = D.printdebug2 (E.printEnv env "")*)
	val env      = E.filterEnv env labs
	(*val _        = D.printdebug2 (E.printEnv env "")*)
	val err      =
	    case U.unif env filters1 (U.MIN err) of
		U.Error (err, _) => err
	      | U.Success _ =>
		let
		    val err = ERR.printOneXmlErr (ERR.setSlice ast err) "" true
		in raise EH.DeadBranch ("Error! The unification algorithm terminated in a success state, but an error was generated previously! "^
					"Hint: It has been found previously to be the case that this error is cased by a bug solely in the unification algorithm where some labels are not propagated correctly...")
		end
	val labs    = ERR.getL err
	(*val labs = ERR.getL err*)
	(*(2010-06-18)We need 'done' because for arity clashes we keep the accessors without their binding
	 * and the builtin basis is labelled by the dummy label so it is never going to be filtered out
	 * and so an error such as datatype 'a int = T of int int, will result in the minimal error
	 * [<..>] int where the 'int' is then the one from the builtin basis.*)
	val (bindings, inenv) = E.getbindings env
	(*val _ = map (fn labs => D.printdebug2 (L.toString labs)) bindings*)
	val (labs1, keep, err1) = lazyunbind bindings env labs err ast
	(*val env = E.filterEnv env (L.union labs keep)*)
	(*val _ = D.printdebug2 (S.printSlice (S.slice ast (L.union labs1 keep)) true)*)
	val (labs2, err2) = reduce env (L.union keep done) (L.diff keep labs1) err1
	(*val _ = D.printdebug2 (S.printSlice (S.slice ast labs2) true)*)
	val filters2 = FI.cons (SOME labs2) NONE
	(*val _ = D.printdebug2 ("[last]")*)
	(*val env = E.filterEnv env labs2*)
	val err = case U.unif env filters2 (U.MIN err2) of
		      U.Error (err, _) => err
		    | U.Success _ =>
		      let val msg = "after minimisation, the error should still be an error"
			  val err = ERR.printOneXmlErr (ERR.setSlice ast err) "" true
			  val _   =
			      D.printdebug2
				  (msg ^
				   "\n" ^ err ^
				   "\nlabs:  " ^ L.toString labs  ^
				   (*"\n" ^ S.printSlice (S.slice ast labs) true ^*)
				   "\nlabs1: " ^ L.toString labs1 ^
				   "\nkeep:  " ^ L.toString keep  ^
				   "\nlabs2: " ^ L.toString labs2 ^
				   "\n" ^ S.printSlice (S.slice ast labs2) true)
		      in raise EH.DeadBranch msg
		      end
	val _ = D.printdebug1 ("[minimisation]")
	val (err', counter') = finalError err counter
    in (err', counter')
    end
(* is it because of the VAL constraints that it does not work *)

fun minimize err css ast timer export counter =
    minimize4 err css true ast timer export counter
(* done filters is not used in minimize4 *)

fun minimizeall [] syn _ _ _ _ _ = syn
  | minimizeall (err :: xs) syn cs ast timer export counter =
    let (*val _ = D.printdebug2 ("--" ^ (L.toString ll) ^ "\n")*)
	val errl = minimizeall xs syn cs ast timer export counter
	(*val (ll', ids', errk') = minimize ll cs*)
    in if ERR.alreadyone errl err
       then errl
       else #2 (ERR.mindone errl (#1 (minimize err cs ast timer export counter)))
    end

fun minimizeallkind errl cst ast timer export counter =
    let (*val _ = D.printdebug2 (Int.toString (List.length errl))*)
	(*val _ = print "minimization is running...\n"*)
    in (fn (sem, syn) => minimizeall sem syn cst ast timer export counter)
	   (ERR.sepsemsyn errl)
    end

end
