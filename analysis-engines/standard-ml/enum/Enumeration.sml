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
 *  o File name:   Enumeration.sml
 *  o Description: Contains the enumeration algorithm.  The file defines
 *      the structure Enum which has the signature ENUM.
 *)

functor Enum (SS : SEARCHSPACE) = struct

(* STRUCTURES *)

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

(* EXCEPTIONS *)

exception occextodo
exception occexdone

(* FUNCTIONS *)

(* printing *)

(*fun filter [] _ = []
  | filter cs [] = cs
  | filter ((c as (lab, cst)) :: cs) (x :: xs) =
    if lab = x
    then filter cs xs
    else
	if lab < x
	then c :: (filter cs (x :: xs))
        else filter (c :: cs) xs

fun projlab [] _ = []
  | projlab _ [] = []
  | projlab ((c as (lab, cst)) :: cs) (x :: xs) =
    if lab = x
    then c :: (projlab cs xs)
    else
	if lab < x
	then projlab cs (x :: xs)
        else projlab (c :: cs) xs*)

fun flatset xs = L.foldr (fn (x, y) => (L.singleton x) :: y) [] xs

fun occtodo [] = ()
  | occtodo (x :: xs) =
    let fun f [] = false
          | f (y :: ys) =
	    if (L.subseteq x y) orelse (L.subseteq y x)
	    then ((print ((L.toString x) ^ "\n" ^
			  (L.toString y) ^ "\n"));
		  true)
	    else f ys
    in if f xs then raise occextodo else occtodo xs
    end

fun occdone [] = ()
  | occdone ((x, u, _) :: xs) =
    let fun f [] = false
          | f ((y, v, _) :: ys) =
            if ((L.subseteq x y) andalso (L.subseteq u v))
	       orelse ((L.subseteq y x) andalso (L.subseteq v u))
            then ((print ((L.toString x) ^ "\n" ^
			  (L.toString u) ^ "\n" ^
			  (L.toString y) ^ "\n" ^
			  (L.toString v) ^ "\n"));
		  true)
            else f ys
    in if f xs then raise occexdone else occdone xs
    end


(* haack's one *)
(* computes the new set of filters *)
fun mintodo1 labl labll labl' =
    L.foldr (fn (x, y) =>
		let val ll = L.cons x labl
		in if L.exsubseteq ll labll
		   then y
		   else ll :: y
		end) labll labl'

fun mintodo2 labl labll labl' =
    L.foldr (fn (x, y) =>
		let
		    val ll = L.cons x labl
		in
		    if L.exsubseteq ll labll
		    then y
		    else ll :: y
		end) labll labl' (*(L.delete L.dummylab labl')
(* TODO: the best might be to not enumerate dummylab at all *)*)

fun mintodo3 filter filters labs done =
    L.foldr (fn (x, y) =>
		let val ll = L.cons x filter
		in if L.exsubseteq ll filters
		      orelse
		      L.exsubseteq ll done
		   then y
		   else ll :: y
		end) filters labs

fun mintodo3' filter filters labs done =
    L.foldr (fn (x, y) =>
		let val ll = L.cons x filter
		in if L.exsubseteq ll filters
		      orelse
		      L.exsubseteq ll done
		   then y
		   else y @ [ll]
		end) filters labs

fun enumdisjoint _ [] = NONE
  | enumdisjoint x (err :: ys) =
    case ERR.getK err of
	EK.FreeIdent => enumdisjoint x ys
      | EK.Warning _ => enumdisjoint x ys
      | EK.Parsing _ => enumdisjoint x ys
      | _ => if L.disjoint x (ERR.getL err)
	     then ((*D.printdebug2 (">>" ^ L.toString (L.ord (ERR.getR err)) ^ "\n");*)
		   SOME (ERR.getL err))
	     else enumdisjoint x ys

(* update some special filters: the ones that record when unification will succeed *)
(*fun minfilter x [] = [x]
  | minfilter x (y :: ys) =
    if L.subseteq x y
    then (D.printdebug2 ("(1)"); x :: ys)
    else
	if L.subset y x
	then (D.printdebug2 ("(2)");  y :: ys)
	else y :: (minfilter x ys)*)
(*fun minfilter x xs = if L.isSingle x then x :: xs else xs*)
(* It seems that all these checkings are for nothing *)
fun minfilter x xs = x :: xs

fun minone errs err envcss timer (parse as (ast, _, _)) filter =
    if ERR.alreadyone errs err
    then (D.printdebug2 (ERR.printOneXmlErr (ERR.setSlice ast err) "" true ^ "\n" ^
		 L.toString (FI.filtertodos (FI.cons filter NONE) (ERR.getL err)));
	  app (fn err => D.printdebug2 (ERR.printOneXmlErr err "" true))
	      (ERR.setSlices ast errs);
	  raise EH.DeadBranch "";
	  (L.empty, SOME err, errs))
    (* should we return labs (in err) only for the first one? *)
    else let (*val _ = D.printdebug2 (ERR.printOneXmlErr (ERR.setSlice ast err) "" true)*)
             (*val _ = raise EH.DeadBranch ""*)
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
			      raise EH.DeadBranch "")
	     val (newerr, errs') = ERR.mindone errs err3
	 (*val _ = D.printdebug2 (case newerr of NONE => raise EH.DeadBranch ""
					       | SOME err => ERR.printOneErr err "")*)
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

(* prints a list of filters that are passed in as an argument *)
fun printfilters [] = ""
  | printfilters [x] = L.toString x
  | printfilters (x :: xs) = L.toString x ^ "  " ^ printfilters xs

(*********************************************************************)
(* vincent's one - doesn't use tail recursion - can we improve that? *)
(* the list grows too quickly *)
(*fun mintodo2 labl labll labl' =
    let
	fun g _ [] = []
	  | g y (x :: xs) =
	    let
		val xss = g y xs
	    in
		if L.disjoint y x
		then foldr
			 (fn (u, v) =>
			     if L.exsubseteq u v
			     then v
			     else u :: v)
			 xss
			 (map (fn u => L.cons u x) y)
		else
		    if L.subseteqin x xss
		    then xss
		    else x :: xss
	    end
    in g labl' (labl :: labll)
    end

fun mintodo3 labl labll labl' =
    let
	fun g _ ys [] = ys
	  | g y ys (x :: xs) =
	    if L.disjoint y x
	    then
		let
		    val xx  = map (fn u => L.cons u x) y
		    val xx' = foldl (fn (u, v) => if L.subseteqin u ys then v else u :: v) [] xx
		in (g y (xx' @ ys) xs)
		end
	    else
		if L.subseteqin x ys
		then g y ys xs
		else g y (x :: ys) xs
    in List.rev (g labl' [] (labl :: labll))
    end*)
(*********************************************************************)

(*
fun enum1 cs csm timelimit =
    let val timer = VT.startTimer ()
	fun run errl labll =
	    case labll of
		[]        => (errl, false)
	      | (x :: xs) =>
		if VT.stillMilliTime timer timelimit
		then (errl, true)
		else (case enumdisjoint x errl of (* we do that instead of running again the minimization - only diff with the previous algo *)
			  SOME y => run errl (mintodo2 x xs y)
			| NONE   =>
			  case U1.unif cs (FI.cons NONE (SOME x)) of
			      U1.Success _  => run errl xs (* if the vars of y = (filter cs x) are disjoint from the vars of cs \ y then we can remove y from cs - it might not be worth testing that *)
			    | U1.Error (err, _) =>
			      let
				  (* the whole procedure is time consuming *)
				  (*val _ = occdone errl*)
				  (*val _ = occtodo labll*)
				  (*val _ = print ((printError errl) ^ "\n--\n" ^ (printlabss labll) ^ "\n--------\n")*)
				  (*val _ = print "-\n"*)
				  (*val _ = print "+EB "*)
				  val errl' = #2 (ERR.mindone errl err)
				  (*val _ = print "+EM "*)
				  (*val _ = print "+\n"*)
				  val labll' = mintodo2 x xs (ERR.getL err)
			      (*val _ = print ("-" ^ (Int.toString (List.length labll')))*)
			      (*val _ = print "+EE "*)
			      (*val _ = print "++\n"*)
			      in run errl' labll'
			      end)
    in case U1.unif cs (FI.cons NONE NONE) of
	   U1.Success state => ((*D.printdebug2 ((S1.printState state) ^ "*********\n");*)
				(*D.printdebug2 ("--SUCCESS\n");*)
				(csm, false))
	 | U1.Error (err, _) => run (#2 (ERR.mindone csm err)) (flatset (ERR.getL err))
    end

fun enum2 cs csm timelimit =
    let val timer = VT.startTimer ()
	fun run errl labll =
	    case labll of
		[]        => (errl, false)
	      | (x :: xs) =>
		if VT.stillMilliTime timer timelimit
		then (errl, true)
		else case enumdisjoint x errl of (* we do that instead of running again the unification - only diff with the previous algo *)
			 SOME y => run errl (mintodo2 x xs y)
		       | NONE   =>
			 case U2.unif cs (FI.cons NONE (SOME x)) of
			     U2.Success _                        => run errl xs (* if the vars of y = (filter cs x) are disjoint from the vars of cs \ y then we can remove y from cs - it might not be worth testing that *)
			   | U2.Error (err, _) =>
			     let val errl' = #2 (ERR.mindone errl err)
				 val labll' = mintodo2 x xs (ERR.getL err)
			     in run errl' labll'
			     end
    in case U2.unif cs (FI.cons NONE NONE) of
	   U2.Success state => (csm, false)
	 | U2.Error (err, _) => run (#2 (ERR.mindone csm err)) (flatset (ERR.getL err))
    end

fun enum3 cs csm timelimit =
    let val done = []
        val timer = VT.startTimer ()
	(* found:   are the found errors so far         *)
	(* filters: is the set of filters still to test *)
	(* done:    is the set of unsuccessful filters  *)
	fun run found filters done =
	    case filters of
		[]        => (found, done, false)
	      | (x :: xs) =>
		if VT.stillMilliTime timer timelimit
		then (found, done, true)
		else ((*D.printdebug2 ("-+-+-\n" ^ printfilters filters ^ "\n-+-+-\n");*)
		      (*D.printdebug2 ("--" ^ L.toString x ^ "\n");*)
		      case enumdisjoint x found of (* we do that instead of running again the unification - only diff with the previous algo *)
			  SOME y => run found (mintodo3 x xs y done) done
			| NONE   =>
			  (case U2.unif cs (FI.cons NONE (SOME x)) of
			       U2.Success _ => run found xs (minfilter x done)
			     (* if the vars of y = (filter cs x) are disjoint from the vars of cs \ y then we can remove y from cs - it might not be worth testing that *)
			     | U2.Error (err, _) =>
			       let (*val _ = D.printdebug2 ("--" ^ L.toString labs' ^ "\n")*)
				   val found' = #2 (ERR.mindone found err)
				   val filters' = mintodo3 x xs (ERR.getL err) done
			       in run found' filters' done
			       end))
    in case U2.unif cs (FI.cons NONE NONE) of
	   U2.Success state => (csm, done, false)
	 | U2.Error (err, _) => (run (#2 (ERR.mindone csm err))
				     (flatset (ERR.getL err))
				     done)
    end
*)

(* returns filters just as they were *)
fun reorderFilters1 filters = filters

(* reverses the list of filters *)
fun reorderFilters2 filters = List.rev filters

(* returns the second half of the filters followed by the first half in reverse order *)
fun reorderFilters3 filters =
    let val n   = List.length filters div 2
	val f1  = List.take (filters, n)
	val f2  = List.drop (filters, n)
    in f2 @ (List.rev f1)
    end

(* more filter jumbling *)
fun reorderFilters4 filters =
    let val n   = List.length filters div 2
	val f1  = List.take (filters, n)
	val f2  = List.drop (filters, n)
	val m   = List.length f2 div 2
	val f21 = List.take (f2, m)
	val f22 = List.drop (f2, m)
    in f22 @ (List.rev f1) @ f21
    end

(* use the function which just returns the set of filters *)
fun reorderFilters filters = reorderFilters1 filters

fun newFreeErr (free as ((id, lab), b)) timer (ast, _, _) =
    let val id   = ERR.freshError ()
	val time = VT.getMilliTime timer
	val labs = L.singleton lab
	val ek   = if b then EK.FreeOpen else EK.FreeIdent
	val err1 = ERR.consPreError id labs CD.empty ek L.empty L.dummyLab
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

(* This is now only used by our database. *)
fun enum  envcss errors filters timelimit program =
    enum4 envcss errors filters timelimit program

(* In initEnum and runEnum, if we got a success then we don't need to keep
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

(* found:       are the found errors so far *)
(* searchspace: is the set of filters still to test and the set of unsuccessful filters  *)
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

fun preEnum (envcss as (env, css)) (ast, _, _) =
    let fun conv (E.CSSMULT ll) = SOME (ERR.consErrorNoRB ERR.dummyId ll CD.empty (EK.MultiOcc  NONE)) (*TODO: this is temporary - before all the assumptions move to long assumptions*)
	  | conv (E.CSSCVAR ll) = SOME (ERR.consErrorNoRB ERR.dummyId ll CD.empty (EK.ValVarApp NONE))
	  | conv (E.CSSEVAR ll) = SOME (ERR.consErrorNoRB ERR.dummyId ll CD.empty (EK.ExcIsVar  NONE))
	  | conv (E.CSSECON ll) = SOME (ERR.consErrorNoRB ERR.dummyId ll CD.empty (EK.ExcIsDat NONE))
	  | conv (E.CSSINCL ll) = SOME (ERR.consErrorNoRB ERR.dummyId ll CD.empty EK.Inclusion)
	  | conv (E.CSSAPPL ll) = SOME (ERR.consErrorNoRB ERR.dummyId ll CD.empty EK.AppNotApp)
	  | conv (E.CSSFNAM ll) = SOME (ERR.consErrorNoRB ERR.dummyId ll CD.empty EK.DiffFunName)
	  | conv (E.CSSFARG ll) = SOME (ERR.consErrorNoRB ERR.dummyId ll CD.empty EK.DiffNbArgFun)
	  | conv (E.CSSTYVA ll) = SOME (ERR.consErrorNoRB ERR.dummyId ll CD.empty EK.FreeTyVarTop)
	  | conv (E.CSSLEFT ll) = SOME (ERR.consErrorNoRB ERR.dummyId ll CD.empty EK.AsPatVar)
	  | conv (E.CSSFREC ll) = SOME (ERR.consErrorNoRB ERR.dummyId ll CD.empty EK.FnRecExp)
	  | conv (E.CSSREAL ll) = SOME (ERR.consErrorNoRB ERR.dummyId ll CD.empty EK.RealInPat)
	  | conv (E.CSSFREE ll) = SOME (ERR.consErrorNoRB ERR.dummyId ll CD.empty EK.FreeIdent)
	  | conv (E.CSSWARN (ll, s)) = SOME (ERR.consErrorNoRB ERR.dummyId ll CD.empty (EK.Warning s))
	  | conv (E.CSSPARS (ll, s)) = SOME (ERR.consErrorNoRB ERR.dummyId ll CD.empty (EK.Parsing s))
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