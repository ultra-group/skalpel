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
 *  o File name:   Enumeration.sml
 *)

(** Contains the enumeration algorithm.  The file defines the structure Enum which has the signature ENUM. *)
functor Enum (SS : SEARCHSPACE) = struct

structure I   = Id
structure E   = Env
structure M   = Min
structure D   = Debug
structure L   = Label
structure S   = Slicing (* This is used in minone for debugging purposes *)
structure FI  = Filter
structure CD  = LongId
structure SE  = StateEnv
structure U   = Unif(SE)
structure EK  = ErrorKind
structure VT  = VTimer
structure EH  = ErrorHandler
structure ERR = Error

(** Calls L.singleton on each element of the set that's given in the argument. *)
fun flatset xs = L.foldr (fn (x, y) => (L.singleton x) :: y) [] xs

fun enumdisjoint _ [] = NONE
  | enumdisjoint x (err :: ys) =
    case ERR.getK err of
	EK.FreeIdent => enumdisjoint x ys
      | EK.Warning _ => enumdisjoint x ys
      | EK.Parsing _ => enumdisjoint x ys
      | _ => if L.disjoint x (ERR.getL err)
	     then SOME (ERR.getL err)
	     else enumdisjoint x ys

fun minone errs err envcss timer (parse as (ast, _, _)) filter =
    if ERR.alreadyone errs err
    then ((L.empty, NONE, errs))
    (* should we return labs (in err) only for the first one? *)
    else let
	val (err1, _) = M.minimize err envcss parse timer NONE 0 (* done is actually never used in minimize! (2009-11-23)*)
	     val err2 = ERR.setM (ERR.setT (ERR.setI err1 (ERR.freshError ())) (VT.getMilliTime timer)) true
	     val bmerge = true (*merging of slices*)
	     val err3 = ERR.setReg (ERR.setSlice ast err2) bmerge
	     (*val err2 = ERR.setT err1 (VT.getMilliTime timer)*)
	     val labs = ERR.getL err3
	     val outs = L.diff (FI.filtertodos (FI.cons NONE filter) labs) labs
	     val _    = if L.isEmpty outs
	     		then ()
	     		else (D.printdebug2 (ERR.printOneXmlErr (ERR.setSlice ast err3) "" true ^ "\n" ^
	     				     FI.toString (FI.cons NONE filter) ^ "\n" ^
	     				     L.toString labs ^ "\n" ^
	     				     L.toString outs);
	     		      raise EH.DeadBranch "DeadBranch79")
	     val (newerr, errs') = ERR.mindone errs err3
	 in (ERR.getL err3, newerr, errs')
	 end

fun minOne errs err envcss timer parse filer NONE counter =
    let val (labs, errop, found) = minone errs err envcss timer parse filer
    in (labs, errop, found, counter)
    end
  | minOne errs err envcss timer (parse as (ast, _, _)) filter (SOME funout) counter =
    if ERR.alreadyone errs err
    then (D.printdebug2 (ERR.printOneXmlErr (ERR.setSlice ast err) "" true ^ "\n" ^
			 L.toString (FI.filtertodos (FI.cons filter NONE) (ERR.getL err)));
	  app (fn err => D.printdebug2 (ERR.printOneXmlErr err "" true))
	      (ERR.setSlices ast errs);
	  raise EH.DeadBranch "The slicer already found a smaller or equal error")
    (* should we return labs (in err) only for the first one? *)
    else let val id   = ERR.freshError ()
	     val time = VT.getMilliTime timer
	     val err1 = ERR.setT (ERR.setReg (ERR.setSlice ast (ERR.setI err id)) true) time
	     val _    = funout [err1] parse envcss counter (Int.fromLarge time)
	     val (err2, counter') = M.minimize err1 envcss parse timer (SOME funout) (counter + 1) (* done is actually never used in minimize! (2009-11-23)*)
	     val err3 = ERR.setM (ERR.setT (ERR.setReg (ERR.setSlice ast err2) true) (VT.getMilliTime timer)) true
	     val (newerr, errs') = ERR.mindone errs err3
	     (*val newerr' = Option.map (fn err => ERR.setI (ERR.setE err [id1]) id2) newerr*)
	 in (ERR.getL err3, newerr, errs', counter')
	 end

(** Prints a list of filters that are passed in as an argument *)
fun printfilters [] = ""
  | printfilters [x] = L.toString x
  | printfilters (x :: xs) = L.toString x ^ "  " ^ printfilters xs

(** The identity function. *)
fun reorderFilters1 filters = filters

(** Use the function which just returns the set of filters. *)
fun reorderFilters filters = reorderFilters1 filters

(** Generates a new free error, either FreeOpen or FreeIdent depending on the argument. *)
fun newFreeErr (free as ((id, lab), b)) timer (ast, _, _) =
    let val id   = ERR.freshError ()
	val time = VT.getMilliTime timer
	val labs = L.singleton lab
	val ek   = if b then EK.FreeOpen else EK.FreeIdent
	val err1 = ERR.consPreError id labs CD.empty ek L.empty
	val err2 = ERR.setT (ERR.setReg (ERR.setSlice ast err1) true) time
    in err2
    end

fun addFree [] free timer ast =
    let val new = newFreeErr free timer ast
    in ([new], [new])
    end
  | addFree (err :: errors) (free as ((id, lab), b)) timer ast =
    (case ERR.getK err of
	 EK.FreeIdent =>
	 let val labs1 = ERR.getL err
	     val labs2 = L.singleton lab
	 in if L.subseteq labs1 labs2
	    then ((err :: errors), [])
	    else if L.subseteq labs2 labs1
	    then let val new = newFreeErr free timer ast
		 in (new :: errors, [new])
		 end
	    else let val (errors, news) = addFree errors free timer ast
		 in (err :: errors, news)
		 end
	 end
       | _ => let val (errors, news) = addFree errors free timer ast
	      in (err :: errors, news)
	      end)

fun addFrees errors [] timer ast = (errors, [])
  | addFrees errors (free :: frees) timer ast =
    let val (errors, news1) = addFree errors free timer ast
	val (errors, news2) = addFrees errors frees timer ast
    in (errors, news1 @ news2)
    end

(** The current implementation of the enumeration algorithm. *)
fun enum4 (envcss as (env, css)) errors searchspace timelimit program =
    let val timer = VT.startTimer ()
	(* found:       are the found errors so far *)
	(* searchspace: is the set of filters still to test and the set of unsuccessful filters  *)
	fun run found searchspace =
	    case SS.getOneFilter searchspace of
		NONE => (found, false)
	      | SOME (filter, searchspace) =>
		if VT.stillMilliTime timer timelimit
		then (found, false)
		else (case enumdisjoint filter found of (* we do that instead of running again the unification - only diff with the previous algo *)
			 SOME labs => run found (SS.buildFilters filter labs searchspace false)
		       | NONE =>
			 ((*D.printdebug2 (FI.toString (FI.cons NONE (SOME filter)));*)
			  case U.unif env (FI.cons NONE (SOME filter)) U.DBENUM of
			      U.Success state =>
			      let val frees = SE.getValStateFree state
				  val (found, _) = addFrees found frees timer program
 			      in run found (SS.addSuccess filter searchspace)
			      end
			    (* if the vars of y = (filter cs filter) are disjoint from the vars of cs \ y then we can remove y from cs - it might not be worth testing that *)
			    | U.Error (err, state) =>
			      let val frees = SE.getValStateFree state
				  val (found, _) = addFrees found frees timer program
				  val (labs'', _, found') = minone found err envcss timer program (SOME filter)
				  val searchspace' = SS.buildFilters filter labs'' searchspace true
				  val searchspace'' = reorderFilters searchspace'
			      in run found' searchspace''
			      end))
    in case SS.getOneFilter searchspace of (* we check if we already have searchspace from the syntactic errors
					    * if we don't then we have to search for a semantic one
					    * if we do then we can use these searchspace for our search *)
	   NONE => (case U.unif env (FI.cons NONE NONE) U.DBENUM of
		      U.Success state =>
		      let val frees = SE.getValStateFree state
			  val (errors, _) = addFrees errors frees timer program
		      in (errors, false)
		      end
		    | U.Error (err, state) =>
		      let val frees = SE.getValStateFree state
			  val (errors, _) = addFrees errors frees timer program
			  val (labs', _, errs) = minone errors err envcss timer program NONE
		      in run errs (SS.flatLabs labs')
		      end)
	 | _ => run errors searchspace
    end

(** This is now only used by our database.
 * Is this still the case after the JSON conversion?. *)
fun enum  envcss errors filters timelimit program = enum4 envcss errors filters timelimit program

(** In initEnum and runEnum, if we got a success then we don't need to keep
 * searching for free identifiers because it means we found them all.
 * In initEnum we don't need to do that because it is the first time that
 * the unification is called, but in runEnum we could use done which indicates
 * the filters for which the solver succeeded: if done is not empty then we
 * don't look for free ids. *)
fun initEnum (envcss as (env, _)) found searchspace timer parse export counter =
    case SS.getOneFilter searchspace of
	NONE => (case U.unif env (FI.cons NONE NONE) U.ENUM of
		     U.Success state =>
		     let val frees = SE.getValStateFree state
			 val (found, news) = addFrees found frees timer parse
		     in (news, found, SS.emSearchSpace, counter, false)
		     end
		   | U.Error (err, state) =>
		     let val frees = SE.getValStateFree state
			 val (found, news) = addFrees found frees timer parse
			 val (labs', errop, found', counter') =
			     minOne found err envcss timer parse NONE export counter
			 val errs = [Option.valOf errop] handle Option => []
		     in (news @ errs, found', SS.flatLabs labs', counter', true)
		     end)
      | _ => ([], found, searchspace, counter, true)

(** Runs the enumeration algorithm.
 * found:       are the found errors so far.
 * searchspace: is the set of filters still to test and the set of unsuccessful filters. *)
fun runEnum (envcss as (env, _)) found searchspace timelimit timer ast export counter =
    case SS.getOneFilter searchspace of
	NONE => ([], found, searchspace, counter, false)
      | SOME (filter, searchspace') =>
	if VT.stillMilliTime timer timelimit
	then ([], found, searchspace, counter, false) (* false because we want to stop *)
	else ((*D.printdebug2 ("--->" ^ L.toString filter ^ "\n");*)
	      (*map (fn err => print (ERR.printOneErr err "      ")) (ERR.getMinErrors found);*)
	      case enumdisjoint filter ((*ERR.getMinErrors*) found) of
		  (*(2010-06-09)NOTE: I had to desactivate that (getMinErrors) because now we use
		   * the remove field, not for merging record clash errors but for presenting non
		   * minimal error and refining them.*)
		  SOME err =>
		  let val searchspace'' = SS.buildFilters filter err searchspace' false
		  in runEnum envcss found searchspace'' timelimit timer ast export counter
		  end
		| NONE   =>
		  (case U.unif env (FI.cons NONE (SOME filter)) U.ENUM of
		       U.Success _ => runEnum envcss found (SS.addSuccess filter searchspace') timelimit timer ast export counter
		     | U.Error (err, _) =>
		       let val (labs'', errop, found', counter') = minOne found err envcss timer ast (SOME filter) export counter
			   val searchspace'' = SS.buildFilters filter labs'' searchspace' true
			   val errs = [Option.valOf errop] handle Option => []
		       in (errs, found', searchspace'', counter', true)
		       end))

(** In a file without syntax errors, errsynt is empty and labs is also empty, creating the initial filter? *)
fun preEnum (envcss as (env, css)) (ast, _, _) =
    let
	(** Helper function to the #preEnum phase. *)
	fun conv (E.CSSMULT ll) = SOME (ERR.consErrorNoRB ERR.dummyId ll CD.empty (EK.MultiOcc  NONE)) (*TODO: this is temporary - before all the assumptions move to long assumptions*)
	  | conv (E.CSSCVAR ll) = SOME (ERR.consErrorNoRB ERR.dummyId ll CD.empty (EK.ValVarApp NONE))
	  | conv (E.CSSEVAR ll) = SOME (ERR.consErrorNoRB ERR.dummyId ll CD.empty (EK.ExcIsVar  NONE))
	  | conv (E.CSSECON ll) = SOME (ERR.consErrorNoRB ERR.dummyId ll CD.empty (EK.ExcIsDat NONE))
	  | conv (E.CSSINCL ll) = SOME (ERR.consErrorNoRB ERR.dummyId ll CD.empty EK.Inclusion)
	  | conv (E.CSSAPPL ll) = SOME (ERR.consErrorNoRB ERR.dummyId ll CD.empty EK.AppNotApp)
	  | conv (E.CSSFNAM ll) = SOME (ERR.consErrorNoRB ERR.dummyId ll CD.empty EK.DiffFunName)
	  | conv (E.CSSFARG ll) = SOME (ERR.consErrorNoRB ERR.dummyId ll CD.empty EK.DiffNbArgFun)
	  | conv (E.CSSTYVA ll) = SOME (ERR.consErrorNoRB ERR.dummyId ll CD.empty EK.FreeTypeVarTop)
	  | conv (E.CSSLEFT ll) = SOME (ERR.consErrorNoRB ERR.dummyId ll CD.empty EK.AsPatVar)
	  | conv (E.CSSFREC ll) = SOME (ERR.consErrorNoRB ERR.dummyId ll CD.empty EK.FnRecExp)
	  | conv (E.CSSREAL ll) = SOME (ERR.consErrorNoRB ERR.dummyId ll CD.empty EK.RealInPat)
	  | conv (E.CSSFREE ll) = SOME (ERR.consErrorNoRB ERR.dummyId ll CD.empty EK.FreeIdent)
	  | conv (E.CSSWARN (ll, s)) = SOME (ERR.consErrorNoRB ERR.dummyId ll CD.empty (EK.Warning s))
	  | conv (E.CSSPARS (ll, s)) = SOME (ERR.consErrorNoRB ERR.dummyId ll CD.empty (EK.Parsing s))
	(* errsynt - syntax error *)
	val (errsynt, labs) =
	    foldl (fn (err, (errs, labs)) =>
		      let val err   = ERR.setReg (ERR.setSlice ast (ERR.setM err true)) true
			  (* We don't set the id because it is set by mindone' *)
			  val errs' = #2 (ERR.mindone' errs err)
			  val labs' = case ERR.getK err of
					  EK.FreeIdent => labs
					| EK.Warning _ => labs
					| EK.Parsing _ => labs
					| _ => L.union (ERR.getL err) labs
		      in (errs', labs')
		      end)
		  ([], L.empty)
		  (List.mapPartial conv css)
    in (errsynt, SS.flatLabs labs) (* enum cs errsynt (flatset labs) timelimit program*)
    end

end
