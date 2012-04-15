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


(* ====== MINIMISATION ALGORITHMS ====== *)


(*
(*fun gettvbind   xs = map (fn (_, x) => x) xs*)
fun getlabsbind xs state =
    foldr (fn (tv, labs) =>
	      case ST.getValStateTv state tv of
		  NONE => labs
		| SOME (_, labs', _, _) => L.union labs' labs)
	  L.empty
	  xs

fun disjfilters _ [] = false
  | disjfilters labs (x :: xs) =
    if L.disjoint labs x
    then true
    else disjfilters labs xs
*)

(*(* this is buggy and not used anymore *)
fun unbind [] _ _ done _ = done
  | unbind (bd :: bindings) css labs done keep =
    let
	val totest = L.diff done labs
	val state0 = S.initState ()
	val state1 = S.initState ()
	val _ = U.unifstate' css (SOME totest) NONE state0 (SOME state1) false
	val (keep1, rej1) =
	    foldr
		(fn ((lab, tv), (keep, rej)) =>
		    let	val _ = ((*D.printdebug2 ("-" ^ L.printelt lab ^ " " ^ T.printtyvar tv ^ " " ^ L.toString done ^ "\n");*) ())
		    in case U.unif css (SOME (L.delete lab totest)) NONE of
			   U.Success _ => ((*D.printdebug2 ("keep " ^ L.toString (getlabsbind [tv] state1) ^ "\n");*)
					    (L.union (getlabsbind [tv] state1) keep,
					     rej))
			 | U.Error   _ => ((*D.printdebug2 ("rej  " ^ L.toString (getlabsbind [tv] state1) ^ "\n");*)
					    (*D.printdebug2 (S.printState state1);*)
					    (keep,
					     L.union (getlabsbind [tv] state1) rej))
		    end)
		(L.empty, L.empty) (* false if we want to reject the binding *)
		bd
	val keep1' = L.union keep keep1
	val rej1'  = L.diff keep1' (L.union rej1 done) (*L.diff keep1 rej1*)
	(*val _ = D.printdebug2 ("rejs:" ^ L.toString rej1' ^ "\n")*)
	val rej2 = unbind bindings css labs rej1' keep1'
    in rej2
    end*)

(*(* this is buggy and not used anymore *)
fun lazyunbind1 [] _ _ done _ = done
  | lazyunbind1 (bd :: bindings) css labs done keep =
    let
	val totest = L.diff done labs
	val state0 = S.initState ()
	val state1 = S.initState ()
	val _ = U.unifstate' css (SOME totest) NONE state0 (SOME state1) false
	val (keep1, rej1) =
	    let
		val bdl = getlabbind bd            (* bind labs  *)
		val bdt = gettvbind bd             (* bind types *)
		val ctx = getlabsbind bdt state1   (* context    *)
		val ubd = L.union (L.ord bdl) ctx (* unbound    *)
	    in case U.unif css (SOME (L.diff ubd totest)) NONE of
		   U.Success _ => (ubd, L.empty)
		 | U.Error   _ => (L.empty, ubd)
	    end
	val keep1' = L.union keep keep1
	val rej1'  = L.diff keep1' (L.union rej1 done) (*L.diff keep1 rej1*)
	(*val _ = D.printdebug2 ("rejs:" ^ L.toString rej1' ^ "\n")*)
	val rej2 = lazyunbind1 bindings css labs rej1' keep1'
    in rej2
    end*)

(*
(* computes the ones we can remove *)
fun lazyunbind2 [] _ _ done = done
  | lazyunbind2 (bd :: bindings) css labs done =
    let val filters = FI.cons (SOME labs) (SOME (L.union done bd))
	val rej = case U.unif css filters of
		      U.Success _ => done
		    | U.Error (err, _) => L.union done bd (*WHY CANT WE PUT THAT IN THERE TOO? L.diff (ERR.getL err) labs (see test 55)*)
    in lazyunbind2 bindings css labs rej
    end*)


(*
(* computes the ones we have to keep *)
fun lazyunbind3 [] _ labs _ = labs
  | lazyunbind3 (bd :: bindings) css labs ast =
    let (*val _ = D.printdebug2 ("unbind: " ^ L.toString bd)*)
	val labs' =
	    if L.disjoint labs bd
	    then labs
	    else case U.unif css (FI.cons (SOME labs) (SOME bd)) of
		     U.Success _ => labs
		   | U.Error (err, _) =>
		     ((*D.printdebug2 (ERR.printOneXmlErr (ERR.setSlice ast err) "" true);*)
		      (*D.printdebug2 ("removed " ^ L.toString bd);*)
		      L.inter (ERR.getL err) labs)
    in lazyunbind3 bindings css labs' ast
    end*)

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

fun lazyunbind bindings css labs err ast = lazyunbind4 bindings css labs L.empty err ast

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

(*fun reduce1 css donefilters done todo =
    let
    	(*val _ = D.printdebug2 (">>z>" ^ L.toString done ^ " - " ^ L.toString todo ^ "\n")*)
	val (lop, ll) = select todo
    in case lop of
	   NONE => done
	 | SOME l =>
	   let
	       (*val _ = D.printdebug2 (">>" ^ L.printelt l)*)
	       val totest = L.union done ll
	       (*val _ = D.printdebug2 (L.printelt l)*)
	   in if false (*disjfilters totest donefilters*) (* ARGGGHHHHH!!!!! *)
	      then (D.printdebug2 (">>>>>>>>>>>>>>>>>" ^ L.printelt l ^ "\n");
		    reduce1 css donefilters (L.cons l done) ll)
	      else case U.unif css (SOME totest) NONE of
		       U.Error (err, _) =>
		       ((*D.printdebug2 (">>error " ^ L.printelt l);*)
			reduce1 css
				donefilters
				done
				(L.inter (ERR.getL err) ll))
		     | U.Success _ => reduce1 css donefilters (L.cons l done) ll
	   end
    end*)

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

(*
 fun badCase1 _ =
     if L.isSingle fst
     then reduce2 css filters (L.union done fst) snd stack
     else
	 let
	     val (done', stack') = reduce2 css filters done fst (L.union snd stack)
	 in reduce2 css filters done' snd stack'
	 end

 fun badCase3 _ = (* this one test if snd can be discarded *)
     let
	 val totest' = L.unions [done, fst, stack]
     in if disjfilters totest' filters
	then
	    if L.isSingle fst
	    then reduce2 css filters (L.union done fst) snd stack
	    else reduce2 css
			 filters
			 (reduce2 css filters done fst (L.union snd stack))
			 snd
			 stack
	else case U.unif css (SOME totest') NONE of
		 U.Error _ =>
		 if L.isSingle fst
		 then L.union done fst
		 else reduce2 css filters done fst stack
	       | U.Success _  =>
		 if L.isSingle fst
		 then reduce2 css filters (L.union done fst) snd stack
		 else reduce2 css
			      filters
			      (reduce2 css filters done fst (L.union snd stack))
			      snd
			      stack
     end
 *)

(*
fun reduce2 css donefilters done todo stack =
    let val (fst, snd) = L.splitIn2 todo
	(*val (snd, fst) = L.splitInTwo2 todo*)
	(*val _ = D.printdebug2 (">>" ^ L.toString fst ^ " >>" ^ L.toString snd ^ " >>" ^ L.toString stack ^ "\n")*)
	fun badCase2 _ = (* this one test if snd can be discarded *)
	    if L.isSingle fst andalso L.isEmpty snd
	    then (L.union done fst, stack)
	    else let val totest' = L.unions [done, fst, stack]
		     val filters = FI.cons (SOME totest') NONE
		 in case U.unif css filters of
			U.Error (err, _) =>
			((*D.printdebug2 ("snd:" ^ Int.toString (L.length snd));*)
			 reduce2 css
				 donefilters
				 (L.inter (ERR.getL err) done)
				 (L.inter (ERR.getL err) fst)
				 (L.inter (ERR.getL err) stack))
		      | U.Success _ =>
			let val (done', stack') = reduce2 css donefilters done fst (L.union snd stack)
			in reduce2 css donefilters done' snd (L.diff snd stack')
			end
		 end
	fun badCase3 _ = (* this one test if snd can be discarded *)
	    if L.isSingle fst andalso L.isEmpty snd
	    then (L.union done fst, stack)
	    else let val totest' = L.unions [done, fst, stack]
		     val filters = FI.cons (SOME totest') NONE
		 in case U.unif css filters of
			U.Error (err, _) =>
			((*D.printdebug2 ("snd:" ^ Int.toString (L.length snd));*)
			 reduce2 css
				 donefilters
				 (L.inter (ERR.getL err) done)
				 (L.inter (ERR.getL err) fst)
				 (L.inter (ERR.getL err) stack))
		      | U.Success _ =>
			let val (lop, ll) = select fst
			in case lop of
			       NONE => raise EH.DeadBranch ""
			     | SOME l =>
			       let val totest  = L.unions [done, ll, snd, stack]
				   val filters = FI.cons (SOME totest) NONE
			       in case U.unif css filters of
				      U.Error (err, _) =>
				      let val (done', stack') =
					      reduce2 css
						      donefilters
						      (L.inter (ERR.getL err) done)
						      (L.inter (ERR.getL err) ll)
						      (L.inter (ERR.getL err) stack)
				      in reduce2 css donefilters done' (L.inter (ERR.getL err) snd) (L.diff snd stack')
				      end
				    | U.Success _ =>
				      let val (done', stack') = reduce2 css donefilters (L.cons l done) ll (L.union snd stack)
				      in reduce2 css donefilters done' snd (L.diff snd stack')
				      end
			       end
			end
		 end
    in if L.isEmpty fst (* means that snd is empty too *)
       then (done, stack)
       else let (* let's try removing fst *)
	       val totest  = L.unions [done, snd, stack]
	       val filters = FI.cons (SOME totest) NONE
	   in case U.unif css filters of
		  U.Error (err, _) => (* we can then discard fst *)
		  ((*D.printdebug2 ("fst:" ^ Int.toString (L.length fst));*)
		   reduce2 css
			   donefilters
			   (L.inter (ERR.getL err) done)
			   (L.inter (ERR.getL err) snd)
			   (L.inter (ERR.getL err) stack))
		| U.Success _ => (* some labels in fst are important *)
		  badCase3 ()
	   end
    end*)


(*
fun selectTwo labs =
    let val (x, labs1) = L.remFirst labs
	val (y, labs2) = L.remFirst labs1
    in (x, y, labs2) end

fun selectThree labs =
    let val (x, labs1) = L.remFirst labs
	val (y, labs2) = L.remFirst labs1
	val (z, labs3) = L.remFirst labs2
    in (x, y, z, labs3) end*)

(*
fun reduce' css donefilters done ll l freduce =
    let val totest  = L.union done ll
	val filters = FI.cons (SOME totest) NONE
    in case U.unif css filters of
	   U.Error (err, _) =>
	   freduce css donefilters done (L.inter (ERR.getL err) ll)
	 | U.Success _ => freduce css donefilters (L.cons l done) ll
    end*)

(*
(* Tries to remove 2 labels at a time *)
fun reduce3 css donefilters done todo =
    case selectTwo todo of
	(SOME l, SOME k, ll) =>
	let val totest = L.union done ll
	in case U.unif css (FI.cons (SOME totest) NONE) of
	       U.Error (err, _) => (* removed l and k *)
	       reduce3 css donefilters done (L.inter (ERR.getL err) ll)
	     | U.Success _ => (* can't removed l and k at the same time *)
	       (case U.unif css (FI.cons (SOME (L.cons k totest)) NONE) of
		    U.Error (err, _) => (* removed l *)
		    reduce' css donefilters done (L.inter (ERR.getL err) ll) k reduce3
		  | U.Success _ => (* can't remove l *)
		    reduce' css donefilters (L.cons l done) ll k reduce3)
	end
      | (SOME l, NONE, ll) => reduce' css donefilters done ll l reduce3
      | (NONE, NONE, _) => done
      | _ => raise EH.DeadBranch ""*)

(*
(* Tries to remove 3 labels at a time *)
fun reduce4 css donefilters done todo =
    case selectThree todo of
	(SOME l, SOME k, SOME j, ll) =>
	let val totest = L.union done ll
	in case U.unif css (FI.cons (SOME totest) NONE) of
	       U.Error (err, _) => (* removed l k j *)
	       reduce4 css donefilters done (L.inter (ERR.getL err) ll)
	     | U.Success _ => (* can't remove l k j at the same time *)
	       (case U.unif css (FI.cons (SOME (L.cons k (L.cons j totest))) NONE) of
		    U.Error (err, _) => (* removed l *)
		    let val ll1 = L.inter (ERR.getL err) ll
			val totest1 = L.union done ll1
		    in case U.unif css (FI.cons (SOME (L.cons j totest1)) NONE) of
			   U.Error (err, _) => (* remove k *)
			   reduce' css donefilters done (L.inter (ERR.getL err) ll1) j reduce4
			 | U.Success _ => (* can't remove k, let's try to remove j *)
			   reduce' css donefilters (L.cons k done) ll1 j reduce4
		    end
		  | U.Success _ => (* can't remove l, let's try to remove k and j*)
		    (case U.unif css (FI.cons (SOME (L.cons l (L.cons j totest))) NONE) of
			 U.Error (err, _) => (* removed k *)
			 reduce' css donefilters (L.cons l done) (L.inter (ERR.getL err) ll) j reduce4
		       | U.Success _ => (* can't remove k either, let's try to remove j *)
			 reduce' css donefilters (L.cons l (L.cons k done)) ll j reduce4))
	end
      | (SOME l, SOME k, NONE, ll) =>
	let val totest = L.union done ll
	in case U.unif css (FI.cons (SOME totest) NONE) of
	       U.Error (err, _) => (* removed l and k *)
	       reduce4 css donefilters done (L.inter (ERR.getL err) ll)
	     | U.Success _ => (* can't remove l and k at the same time *)
	       (case U.unif css (FI.cons (SOME (L.cons k totest)) NONE) of
		    U.Error (err, _) => (* removed l *)
		    reduce' css donefilters done (L.inter (ERR.getL err) ll) k reduce4
		  | U.Success _ => (* can't remove l *)
		    reduce' css donefilters (L.cons l done) ll k reduce4)
	end
      | (SOME l, NONE, NONE, ll) => reduce' css donefilters done ll l reduce4
      | (NONE, NONE, NONE, _) => done
      |  _ => raise EH.DeadBranch ""*)

fun reduce css done todo err = reduce1 css done todo err

(* in todo we should test first the labels in the csbindings *)
(* it does not seem to work quite well *)
fun minimize4 err (envContextSensitiveSyntaxPair as (env, css)) lazy (parse as (ast, _, _)) timer export counter =
    let val _        = setCurrentBackground timer export envContextSensitiveSyntaxPair parse err counter
	val labs     = ERR.getL err
	val done     = L.empty (*EK.getLabsEk (ERR.getK err)*)
	val filters1 = FI.cons (SOME labs) NONE
	(*val _        = D.printdebug2 (E.printEnv env "")*)
	val env      = E.filterEnv env labs
	(*val _        = D.printdebug2 (E.printEnv env "")*)
	val err      =
	    case U.unif env filters1 (U.MIN err) of
		U.Error (err, _) => err
	      | _ => let val msg = "before minimisation, the error should be an error"
			 val err = ERR.printOneXmlErr (ERR.setSlice ast err) "" true
			 val _   = D.printdebug2 (msg ^ "\n" ^ err ^ "\n" ^ L.toString labs)
		     in raise EH.DeadBranch msg
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
				   "\n" ^ L.toString labs  ^
				   (*"\n" ^ S.printSlice (S.slice ast labs) true ^*)
				   "\n" ^ L.toString labs1 ^
				   "\n" ^ L.toString keep  ^
				   "\n" ^ L.toString labs2 ^
				   "\n" ^ S.printSlice (S.slice ast labs2) true)
		      in raise EH.DeadBranch msg
		      end
	val _ = D.printdebug1 ("[minimisation]")
	val (err', counter') = finalError err counter
    in (err', counter')
    end
(* is it because of the VAL constraints that it does not work *)

(*fun reducein css donefilters done todo1 todo2 =
    let val (lop, ll) = select todo1
    in case lop of
	   NONE => reduce css donefilters done todo2
	 | SOME l =>
	   let val totest  = L.union (L.union done ll) todo2
	       val filters = FI.cons (SOME totest) (SOME (L.singleton l))
	   in case U.unif css filters of
		  U.Error (err, _) =>
		  reducein css donefilters done (L.inter (ERR.getL err) ll) (L.inter (ERR.getL err) todo2)
		| U.Success state =>
		  reducein css donefilters (L.cons l done) ll todo2
	   end
    end

(*(2010-04-01)Seems to be slower than minimize4 - see testcase 62*)
fun minimize5 err done donefilters css lazy ast =
    let val todo = ERR.getL err
	val labs = L.union todo done
	val filters1 = FI.cons (SOME labs) NONE
	val err = case U.unif css filters1 of
		      U.Error (err, _) => err
		    | _ => (D.printdebug2 (ERR.printOneXmlErr (ERR.setSlice ast err) "" true ^ "\n" ^ L.toString labs);
			    raise EH.DeadBranch "before minimisation, the error should be an error")
	val (bindings, inenv) = E.getbindings css
	val (labs1, keep) = lazyunbind bindings css labs ast
	val labs1' = L.diff keep labs1
	val todo1  = L.inter inenv labs1'
	val todo2  = L.diff inenv labs1'
	val labs2  = reducein css donefilters (L.union keep done) todo1 todo2
	val filters2 = FI.cons (SOME labs2) NONE
	val err = case U.unif css filters2 of
		      U.Error (err, _) => err
		    | U.Success _ =>
		      (D.printdebug2
			   (ERR.printOneXmlErr (ERR.setSlice ast err) "" true ^
			    "\n" ^ L.toString labs  ^
			    "\n" ^ L.toString labs1 ^
			    "\n" ^ L.toString keep  ^
			    "\n" ^ L.toString labs2 ^
			    "\n" ^ S.printSlice (S.slice ast labs2) true);
		       raise EH.DeadBranch "after minimisation, the error should still be an error")
    in err
    end*)

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
