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
 *)

(** Contains the minimisation algorithm, opaquely constrained by refstruct{MIN}. *)
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

(** Represents the error being minimized as a ref value. *)
val currentError = ref (NONE : (ERR.error * LargeInt.int * int) option)

(** Sets the currentError ref given an error and a counter for the error. *)
fun setCurrentError error counter =
    case !currentBackground of
	SOME (timer, funout, env, parse, size) => currentError := SOME (error, VT.getMilliTime timer, counter)
      | _ => ()

(** Sets the #currentBackground values to those given as the arguments. *)
fun setCurrentBackground timer (SOME funout) env parse error counter =
    (currentBackground := SOME (timer, funout, env, parse, L.length (ERR.getL error));
     setCurrentError error counter)
  | setCurrentBackground timer NONE env parse error counter = ()

(** Gets the counter information inside the current error (#currentError). *)
fun getCurrentErrorCounter () =
    case !currentError of
	SOME (error, time, counter) => SOME counter
      | NONE => NONE

(** Reports an error using the function 'funout', specified in the second component of the #currentBackground ref tuple. *)
fun reportError error =
    case (!currentBackground, !currentError) of
	(SOME (timer, funout, env, parse as (ast, _, _), size), SOME (err, time, counter)) =>
	let val time' = VT.getMilliTime timer
	    (** Length of the labels given in the 'error' parameter to #reportError. *)
	    val size1 = L.length (ERR.getL error)
	    (** Length of the labels given in the 'err' portion of #currentError. *)
	    val size2 = L.length (ERR.getL err)
	in if (time' - time > 20000 andalso size1 < size2) (* either 20s have passed *)
	      orelse
	      (time' - time > 1000 andalso 100 - ((size1 * 100) div size2) > 20) (* or the error is 20% smaller *)
	   then let val err' = ERR.setT (ERR.setReg (ERR.setSlice ast (ERR.setE (ERR.setI error (ERR.freshError ())) [ERR.getI err])) true) time'
		    val _    = funout [err'] parse env counter (LargeInt.toInt time')
		    val _    = setCurrentError err' (counter + 1)
		in true
		end
	   else false
	end
      | _ => false

(** Returns the final error generated (after minimisation has finished) and the time taken to generate it. *)
fun finalError error counter =
    case !currentError of
	SOME (err, time, counter) => (ERR.setE (ERR.setI error (ERR.freshError ())) [ERR.getI err], counter)
      | _ => (error, counter)

(** Attempts to removes labels associated with bindings (disconnecting binders from accessors. *)
fun lazyunbind [] _ labs keep err _ = (labs, keep, err)
  | lazyunbind (x :: xs) css labs keep err ast =
    let val (labs', keep', err') =
	    if L.disjoint labs x orelse L.subseteq x keep
	    then (labs, keep, err)
	    else case U.unif css (FI.cons (SOME labs) (SOME x)) (U.MIN err) of
		     U.Success _ => (labs, if L.isSingle x then L.union x keep else keep, err)
		   | U.Error (err, _) => (reportError err; (L.inter (ERR.getL err) labs, keep, err))
    in lazyunbind xs css labs' keep' err' ast
    end

(** Extracts a single label from a label set using #Label.remFirst. *)
fun select labs = L.remFirst labs

(** Removes labels from the label set in todo, and repeatedly runs the unification algorithm to check whether the error still exists. *)
fun reduce css done todo err =
    let val (lop, ll) = select todo
    in case lop of
	   NONE => (done, err)
	 | SOME l =>
	   let val totest  = L.union done ll
	       val filters = FI.cons (SOME totest) (SOME (L.singleton l))
	   in case U.unif css filters (U.MIN err) of
		  U.Error (err, _) =>
		   (reportError err;
		    reduce css done (L.inter (ERR.getL err) ll) err)
		| U.Success state =>
		  reduce css (L.cons l done) ll err
	   end
    end

(** Function that does minimisation.
 * In todo we should test first the labels in the csbindings, it does not seem to work quite well. *)
fun minimize err (envContextSensitiveSyntaxPair as (env, css)) (parse as (ast, _, _)) timer export counter =
    let val _        = setCurrentBackground timer export envContextSensitiveSyntaxPair parse err counter
	val labs     = ERR.getL err

	(** The set of labels that we have verified to be part of the error.
	 * Initially the empty set. *)
	val done     = L.empty
	val filters1 = FI.cons (SOME labs) NONE

	(** A filtered environment, filtered by the labels in the error giving in the argument to #minimize. *)
	val env      = E.filterEnv env labs

	(** Holds the new error reproted by the minimisation algorithm for the
	 * filtered environment #env. Throws atn exception if minimisation returns success. *)
	val err      =
	    case U.unif env filters1 (U.MIN err) of
		U.Error (err, _) => err
	      | U.Success _ =>
		let
		    val err = ERR.printOneXmlErr (ERR.setSlice ast err) "" true
		in raise EH.DeadBranch ("Error! The unification algorithm terminated in a success state, but an error was generated previously! "^
					"Hint: It has been found previously to be the case that this error is cased by a bug solely in the unification algorithm where some labels are not propagated correctly...")
		end

	(** Holds the new labels reported by the error received from the minimisation algorithm. *)
	val labs    = ERR.getL err
	(*(2010-06-18)We need 'done' because for arity clashes we keep the accessors without their binding
	 * and the builtin basis is labelled by the dummy label so it is never going to be filtered out
	 * and so an error such as datatype 'a int = T of int int, will result in the minimal error
	 * [<..>] int where the 'int' is then the one from the builtin basis.*)

	val (bindings, inenv) = E.getbindings env
	val (labs1, keep, err1) = lazyunbind bindings env labs L.empty err ast
	val (labs2, err2) = reduce env (L.union keep done) (L.diff keep labs1) err1

	(** Holds a new set of filters with the labels in labs2 assigned to 'keep', and no binding labels. *)
	val filters2 = FI.cons (SOME labs2) NONE
	val err = case U.unif env filters2 (U.MIN err2) of
		      U.Error (err, _) => err
		    | U.Success _ =>
		      let val err' = ERR.printOneXmlErr (ERR.setSlice ast err) "" true
			  val _   =
			      D.printdebug2
				  ("after minimisation, the error should still be an error" ^
				   "\n" ^ err' ^
				   "\nlabs:  " ^ L.toString labs  ^
				   (*"\n" ^ S.printSlice (S.slice ast labs) true ^*)
				   "\nlabs1: " ^ L.toString labs1 ^
				   "\nkeep:  " ^ L.toString keep  ^
				   "\nlabs2: " ^ L.toString labs2 ^
				   "\n" ^ S.printSlice (S.slice ast labs2) true)
		      in err
		      end
	val _ = D.printdebug1 ("[minimisation]")
	val (err', counter') = finalError err counter
    in (err', counter')
    end

(** Calls the minimize function (#minimize) on all the errors in the list in the first argument. *)
fun minimizeall [] syn _ _ _ _ _ = syn
  | minimizeall (err :: xs) syn cs ast timer export counter =
    let val errl = minimizeall xs syn cs ast timer export counter
    in if ERR.alreadyone errl err
       then errl
       else #2 (ERR.mindone errl (#1 (minimize err cs ast timer export counter)))
    end

(** Minimises all the semantic errors that are in the list in the first argument.
 * Uses #Error.sepsemsyn to identify which error are semantic and which are syntactic. *)
fun minimizeallkind errl cst ast timer export counter =
    (fn (sem, syn) => minimizeall sem syn cst ast timer export counter)
	   (ERR.sepsemsyn errl)
end
