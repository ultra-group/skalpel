(* Copyright 2010 2011 2012 Heriot-Watt University
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
 *  o Date:        22 June 2010
 *  o File name:   Label.sml
 *  o Description: This file defines the signature structure Label.
 *      It has signature LABEL and is used to deal with labels.
 *)


(*SML-TES-USE-FILE ../utils/sources.tes*)
(*SML-TES-USE-FILE OrdKey.sml*)
(*SML-TES-USE-FILE Label.sig*)


structure Label :> LABEL = struct

(*structure S = IntCBTSet
structure S = IntCBTHCSet
structure S = IntCBTHCSet2
structure S = IntCBTHCSet3*)

(*structure S = IntListSet*)
(*structure S = SplaySetFn (OrdKey)*)
structure S  = BinarySetFn (OrdKey)
structure EH = ErrorHandler
structure D  = Debug

(* val debugString = S.debugString *)


type label  = S.item
type labels = S.set


(* Printing *)


fun printsetgen xs f = "[" ^ #1 (S.foldr (fn (t, (s, c)) => (f t ^ c ^ s, ",")) ("", "") xs) ^ "]"
fun printLab l = Int.toString l
fun toString set = printsetgen set printLab


(* *********** *)

val dummyLab   = 0
val builtinLab = 1
val firstLab   = 2
val nextlab    = ref firstLab

(* set and get functions for labels *)
fun setNextLab n = nextlab := n
fun getlab    () = !nextlab

(* generates a new label *)
fun freshlab  () =
    let
	val _ = D.printDebug 3 D.LABEL "generating fresh label"
	val x = !nextlab
    in
	(nextlab := !nextlab + 1; x)
    end

(* resets the next label back to the firstLab value (constant) *)
fun resetNext () = setNextLab firstLab

val empty     = S.empty
val foldr     = S.foldr
val foldl     = S.foldl
val isEmpty   = S.isEmpty
val length    = S.numItems
val singleton = S.singleton
val toList    = S.listItems

val eqTypeWordLabels = ref S.empty
val eqTypeLabels     = ref S.empty

(* returns an integer from a value of type Label.label *)
fun toInt   lab = lab

(* returns a label of type Label.label given an integer *)
fun fromInt lab = lab

(* increments the label counter *)
fun nextLabel lab = lab + 1

(* gets the nth next label number *)
fun nextLabN lab 0 = lab
  | nextLabN lab n = if n < 0 then raise EH.DeadBranch "" else (nextLabN lab (n-1)) + 1

(* removes element x frmo the set set *)
fun delete x set = S.delete (set, x) handle LibBase.NotFound => set

(* checks whether two labels are equal *)
fun eq x (y : label) = (x = y)

(* finds which label is of a lesser value *)
fun min x y = Int.min (x, y)

(* returns the first element that it finds by passing in a function which
 * will always return true *)
fun getfirst set = S.find (fn _ => true) set

(* removes the first element that it finds in the set *)
fun remFirst set =
    let val elt = getfirst set
    in case elt of
	   NONE => (NONE, set)
	 | SOME x => (SOME x, delete x set)
    end

(* checks that the set ll has only one element *)
fun isSingle ll = length ll = 1

(* determines if two sets are equal *)
fun equal set1 set2 = S.equal (set1, set2)

(* union of sets *)
fun union set1 set2 = S.union (set1, set2)

(* adds setop (an OPTION) to set, if NONE then just returns original set *)
fun unionop setop set = case setop of NONE => set | SOME x => S.union (x, set)

(* generalised union *)
fun unions sets =
    (List.foldr (fn (set1, set2) => union set1 set2)
		(List.hd sets)
		(List.tl sets))
    handle Empty => S.empty

(* adds x to the set set. The output order is different in this case *)
fun cons x set = S.add' (x, set)

(* creates a new list from xs *)
fun ord xs = S.addList (S.empty, xs)

(* return the intersection of two sets *)
fun inter set1 set2 = S.intersection (set1, set2)

fun interList list set = toList (inter (ord list) set)

(* checks if set1 is a subset of set2 *)
fun subseteq set1 set2 = S.isSubset (set1, set2)

(* checks if set1 is a *strict* subset of set2 *)
fun subset set1 set2 = S.isSubset (set1, set2) andalso S.numItems set1 < S.numItems set2

(* it is true if one of the element of the second argument is a subseteq of the first argument *)
fun exsubseteq set sets = List.exists (fn set' => subseteq set' set) sets

fun subseteqin set sets = List.exists (fn set' => subseteq set set') sets

(* checks whether x is in set *)
fun isin x set = S.member (set, x)

(**
  param l: int
  param ll: int list
  returns (lll, llr): (int list * int list)
  such that ll = lll @ llr
  and forall l' in lll, l' < l
  and forall l' in llr, l' >= l
*)
fun split x set = S.partition (fn y => case S.Key.compare (x, y) of
					       LESS    => false
					     | EQUAL   => false
					     | GREATER => true) set

(* returns the set difference (set2 \ set1) *)
fun diff set1 set2 = S.difference (set2, set1)

(* returns whether any members of set1 are in set2 *)
fun disjoint set1 set2 = S.isEmpty (S.intersection (set1, set2))

(* checks whether l is equal to ll where ll has only one element *)
(* can't this just be done with l=ll? *)
fun isinone l ll = isin l ll andalso length ll = 1

fun getpairs set1 set2 = foldr (fn (x, y) =>
				   (foldr (fn (u, v) => (ord [x, u]) :: v) [] set1) @ y)
			       []
			       set2

fun getpairss sets = #1 (List.foldr (fn (set, (pairs, sets)) =>
					((getpairs set sets) @ pairs, union set sets))
				    ([], empty)
				    sets)

fun restrictLim set lim = S.filter (fn x => if x < lim then true else false) set

fun splitIn2 set =
    let fun push elt pset = pset := S.add (!pset, elt)
	fun next counter  = counter := !counter + 1
	val total = S.numItems set
	val half  = ceil (Real.fromInt total / Real.fromInt 2)
	val fst   = ref S.empty
	val count = ref 0
	val snd   = S.filter (fn elt => if !count < half
					then (next count;
					      push elt fst;
					      false)
					else true) set
    in (!fst, snd)
    end

fun splitIn2' set =
    let fun push elt pset = pset := S.add (!pset, elt)
	fun next counter  = counter := !counter + 1
	val total = S.numItems set
	val half  = total div 2
	val fst   = ref S.empty
	val count = ref 0
	val snd   = S.filter (fn elt => if !count < half
					then (next count;
					      push elt fst;
					      false)
					else true) set
    in (!fst, snd)
    end

fun split2 first next set =
    (S.filter (fn x => x >= first andalso x < next) set,
     S.filter (fn x => x < first orelse x >= next) set)

fun compareLab (lab1, lab2) = Int.compare (lab1, lab2)
fun compare (set1, set2) = S.compare (set1, set2)

end



(* INTEGER SET - FOR THE SAKE OF TESTING THE SLICER *)

structure Label2 :> LABEL = struct

structure O  = OrdKey
structure S  = SplayMapFn(O)
structure EH = ErrorHandler

type label  = O.ord_key
type map    = label S.map
type labels = {from : map, to : map, size : int}

exception DONE of bool

val dummyLab   = 0
val builtinLab = 1
val firstLab   = 2

fun toInt   lab = lab
fun fromInt lab = lab

val nextlab = ref firstLab
fun nextLabel lab = lab + 1

fun nextLabN lab 0 = lab
  | nextLabN lab n = if n < 0 then raise EH.DeadBranch "" else (nextLabN lab (n-1)) + 1

fun setNextLab lab = nextlab := lab
fun resetNext () = setNextLab firstLab

fun eq lab1 (lab2 : label) = (lab1 = lab2)
fun min lab1 lab2 = Int.min (lab1, lab2)

val empty = let val map : map = S.empty in {from = map, to = map, size = 0} end

val eqTypeLabels = ref empty
val eqTypeWordLabels = ref empty

fun singleton lab =
    let val map = S.insert (S.empty, lab, lab)
    in {from = map, to = map, size = 1}
    end

fun isin lab (labels as {from = mfrom, to = mto, size}) =
    (S.appi (fn (f, t) =>
		if lab < f
		then raise DONE false
		else if f <= lab andalso lab <= t
		then raise DONE true
		else ())
	    mfrom;
     false)
    handle DONE b => b

fun isinone lab (labels as {from = mfrom, to = mto, size}) =
    isin lab labels andalso size = 1

fun cons lab (labels as {from = mfrom, to = mto, size}) =
    case (S.find (mfrom, lab + 1), S.find (mto, lab - 1)) of
	(SOME t, SOME f) =>
	(* we join to sets *)
	let val from = #1 (S.remove (#1 (S.remove (mfrom, lab + 1)), f))
	    val to   = #1 (S.remove (#1 (S.remove (mto, lab - 1)), t))
	in {from = S.insert (from, f, t), to = S.insert (to, t, f), size = size - 1}
	end
      (* we extend a set from the beginning *)
      | (SOME t, NONE) =>
	let val from = #1 (S.remove (mfrom, lab + 1))
	    val to   = #1 (S.remove (mto, t))
	in {from = S.insert (from, lab, t), to = S.insert (to, t, lab), size = size}
	end
      (* we extend a set from the end *)
      | (NONE, SOME f) =>
	let val from = #1 (S.remove (mfrom, f))
	    val to   = #1 (S.remove (mfrom, lab - 1))
	in {from = S.insert (from, f, lab), to = S.insert (to, lab, f), size = 1}
	end
      (* lab is then either in a set of does not extend any set *)
      | (NONE, NONE) =>
	if isin lab labels
	then labels
	else {from = S.insert (mfrom, lab, lab), to = S.insert (mto, lab, lab), size = size + 1}

fun add f t labels =
    S.foldli (fn (f', t', (labels, bf, bt)) => raise EH.TODO)
	     (S.empty, false, false)
	     labels

fun delete lab (labels as {from = mfrom, to = mto, size}) = raise EH.TODO

fun isEmpty  {from, to, size} = (size = 0)
fun isSingle {from, to, size} =
    size = 1 andalso S.foldli (fn (f, t, b) => b andalso f = t) true from
fun length   {from, to, size} =
    S.foldli (fn (f, t, n) => n + (t - f + 1)) 0 from

fun union labels {from, to, size} =
    S.foldli (fn (f, t, {from, to, size}) => raise EH.TODO)
	     labels
	     from


fun diff     labels1 labels2 = raise EH.TODO
fun inter    labels1 labels2 = raise EH.TODO
fun disjoint labels1 labels2 = raise EH.TODO
fun subset   labels1 labels2 = raise EH.TODO
fun subseteq labels1 labels2 = raise EH.TODO

fun split  lab labels = raise EH.TODO
fun split2 lab1 lab2 labels = raise EH.TODO
fun splitIn2  labels = raise EH.TODO
fun splitIn2' labels = raise EH.TODO

fun compareLab (lab1, lab2) = raise EH.TODO
fun compare (set1, set2) = raise EH.TODO

fun ord    labelss = raise EH.TODO
fun unions labelss = raise EH.TODO

fun toList labels = raise EH.TODO

fun remFirst labels = raise EH.TODO

fun foldr f init labels = raise EH.TODO

fun exsubseteq labels labelss = raise EH.TODO

fun printLab lab = Int.toString lab

fun toString labels = raise EH.TODO


end



(* INTEGER SET - FOR THE SAKE OF TESTING THE SLICER *)

(*structure Label3 :> LABEL = struct

structure O  = OrdKey
structure S  = SplayMapFn(O)
structure EH = ErrorHandler

type label  = O.ord_key
type map    = (label * label ref) S.map

(* We represent {1,2,3,4,7,8,10} as ({1->(4,SOME(7)),7->(8,SOME(10)),10->(10,NONE)},3) *)

type labels = {sets : map, size : int}

exception DONE of bool
exception UPD  of labels

val dummyLab   = 0
val builtinLab = 1
val firstLab   = 2

fun toInt   lab = lab
fun fromInt lab = lab

val nextlab = ref firstLab
fun nextLabel lab = lab + 1

fun nextLabN lab 0 = lab
  | nextLabN lab n = if n < 0 then raise EH.DeadBranch "" else (nextLabN lab (n-1)) + 1

fun setNextLab lab = nextlab := lab
fun resetNext () = setNextLab firstLab

fun eq lab1 (lab2 : label) = (lab1 = lab2)
fun min lab1 lab2 = Int.min (lab1, lab2)

val empty = let val map : map = S.empty in {sets = map, size = 0} end

fun singleton lab =
    let val map = S.insert (S.empty, lab, (lab, NONE))
    in {sets = map, size = 1}
    end

fun isin lab (labels as {sets, size}) =
    (S.appi (fn (from, (to, next)) =>
		if lab < from
		then raise DONE false
		else if from <= lab andalso lab <= to
		then raise DONE true
		else ())
	    sets;
     false)
    handle DONE b => b

fun isinone lab (labels as {sets, size}) =
    isin lab labels andalso size = 1

fun cons lab (labels as {sets, size}) =
    (S.appi (fn (from, (to, SOME next)) =>
		if lab < from - 1
		then raise UPD {sets = S.insert (sets, lab, (lab, SOME from)), size = size + 1}
		else if lab = from - 1
		then let val sets1 = #1 (S.remove (sets, from))
			 val sets2 = S.insert (sets1, lab, (to, SOME next))
		     in raise UPD {sets = sets2, size = size}
		     end
		else if lab >= from andalso lab <= to
		then raise UPD labels
		else if lab = to + 1 andalso lab < next - 1
		then let val sets1 = #1 (S.remove (sets, from))
			 val sets2 = S.insert (sets1, from, (lab, SOME next))
		     in raise UPD {sets = sets2, size = size}
		     end
		else if lab = to + 1 andalso lab = next - 1
		then let val sets1 = #1 (S.remove (sets, from))
			 val (sets2, tn) = S.remove (sets1, next)
			 val sets3 = S.insert (sets2, from, tn)
		     in raise UPD {sets = sets3, size = size - 1}
		     end
		else if lab > to + 1 andalso lab < next - 1
		then let val sets1 = #1 (S.remove (sets, from))
			 val sets2 = S.insert (sets1, from, (to, SOME lab))
			 val sets3 = S.insert (sets2, lab, (lab, SOME next))
		     in raise UPD {sets = sets3, size = size + 1}
		     end
		else ()
	      | (from, (to, NONE)) =>
		if lab < from - 1
		then raise UPD {sets = S.insert (sets, lab, (lab, SOME from)), size = size + 1}
		else if lab = from - 1
		then let val sets1 = #1 (S.remove (sets, from))
			 val sets2 = S.insert (sets1, lab, (to, NONE))
		     in raise UPD {sets = sets2, size = size}
		     end
		else if lab >= from andalso lab <= to
		then raise UPD labels
		else if lab = to + 1
		then let val sets1 = #1 (S.remove (sets, from))
			 val sets2 = S.insert (sets1, from, (lab, NONE))
		     in raise UPD {sets = sets2, size = size}
		     end
		else let val sets1 = #1 (S.remove (sets, from))
			 val sets2 = S.insert (sets1, from, (to, SOME lab))
			 val sets3 = S.insert (sets2, lab, (lab, NONE))
		     in raise UPD {sets = sets3, size = size + 1}
		     end)
	    sets;
     (* if not UPD is raised then it means that the label set is empty *)
     singleton lab)
    handle UPD labels => labels

fun delete lab (labels as {sets, size}) =
    (S.appi (fn (from, (to, SOME next)) =>
		if lab < from
		then raise UPD labels
		else if lab = from andalso from = to
		then let val sets1 = #1 (S.remove (sets, from))
		     in raise UPD {sets = sets1, size = size - 1}
		     end
		else if lab = from andalso from < to
		then let val sets1 = #1 (S.remove (sets, from))
			 val sets2 = S.insert (sets1, from + 1, (to, SOME next))
		     in raise UPD {sets = sets2, size = size}
		     end
		else if lab > from andalso lab = to
		then let val sets1 = #1 (S.remove (sets, from))
			 val sets2 = S.insert (sets1, from, (to - 1, SOME next))
		     in raise UPD {sets = sets2, size = size}
		     end
		else if lab > from andalso lab < to
		then let val sets1 = #1 (S.remove (sets, from))
			 val sets2 = S.insert (sets1, from, (lab - 1, SOME (lab + 1)))
			 val sets3 = S.insert (sets2, lab + 1, (to, SOME next))
		     in raise UPD {sets = sets3, size = size + 1}
		     end
		else if lab > to andalso lab < next
		then raise UPD labels
		else if lab = next
		then case S.find (sets, next) of
			 NONE => raise EH.DeadBranch ""
		       | SOME (to2, next2) =>
			 if next = to2
			 then let val sets1 = #1 (S.remove (sets, next))
				  val sets2 = #1 (S.remove (sets1, from))
				  val sets3 = S.insert (sets2, from, (to, next2))
			      in raise UPD {sets = sets3, size = size - 1}
			      end
			 else let val sets1 = #1 (S.remove (sets, next))
				  val sets2 = #1 (S.remove (sets1, from))
				  val sets3 = S.insert (sets2, from, (to, SOME (lab + 1)))
				  val sets4 = S.insert (sets3, next, (to2, next2))
			      in raise UPD {sets = sets4, size = size}
			      end
		else ()
	      | (from, (to, NONE)) =>
		if lab < from
		then raise UPD labels
		else if lab = from andalso from = to
		then let val sets1 = #1 (S.remove (sets, from))
		     in raise UPD {sets = sets1, size = size - 1}
		     end
		else if lab = from andalso from < to
		then let val sets1 = #1 (S.remove (sets, from))
			 val sets2 = S.insert (sets1, from + 1, (to, NONE))
		     in raise UPD {sets = sets2, size = size}
		     end
		else if lab > from andalso lab = to
		then let val sets1 = #1 (S.remove (sets, from))
			 val sets2 = S.insert (sets1, from, (to - 1, NONE))
		     in raise UPD {sets = sets2, size = size}
		     end
		else if lab > from andalso lab < to
		then let val sets1 = #1 (S.remove (sets, from))
			 val sets2 = S.insert (sets1, from, (lab - 1, SOME (lab + 1)))
			 val sets3 = S.insert (sets2, lab + 1, (to, NONE))
		     in raise UPD {sets = sets3, size = size + 1}
		     end
		else ())
	    sets;
     labels)
    handle UPD labels => labels

fun add from to (labels as {sets, size}) =
    (S.appi (fn (from1, (to1, SOME next)) =>
		if to < from1 - 1
		then let val sets1 = S.insert (sets, from, (to, SOME from1))
		     in raise UPD {sets = sets1, size = size + 1}
		     end
		else if to < next - 1 andalso from <= to1 + 1
		then let val min   = Int.min (from, from1)
			 val max   = Int.max (to, to1)
			 val sets1 = #1 (S.remove (sets, from1))
			 val sets2 = S.insert (sets1, min, (max, SOME next))
		     in raise UPD {sets = sets2, size = size}
		     end
		else if from > to1 + 1 andalso to < next - 1
		then let val sets1 = #1 (S.remove (sets, from1))
			 val sets2 = S.insert (sets1, from1, (to1, SOME from))
			 val sets3 = S.insert (sets2, from, (to, SOME next))
		     in raise UPD {sets = sets3, size = size + 1}
		     end
		else ()
	      | (from1, (to1, NONE)) => ())
	    sets;
     labels)
    handle UPD labels => labels

fun union labels {sets, size} =
    S.foldli (fn (from, to, {sets, size}) => raise EH.TODO)
	     labels
	     sets

fun isEmpty  {sets, size} = (size = 0)
fun isSingle {sets, size} =
    size = 1 andalso S.foldli (fn (from, (to, _), b) => from = to andalso b) true sets
fun length   {sets, size} =
    S.foldli (fn (from, (to, _), n) => n + (to - from + 1)) 0 sets

fun diff     labels1 labels2 = raise EH.TODO
fun inter    labels1 labels2 = raise EH.TODO
fun disjoint labels1 labels2 = raise EH.TODO
fun subset   labels1 labels2 = raise EH.TODO
fun subseteq labels1 labels2 = raise EH.TODO

fun split  lab labels = raise EH.TODO
fun split2 lab1 lab2 labels = raise EH.TODO
fun splitIn2  labels = raise EH.TODO
fun splitIn2' labels = raise EH.TODO

fun compareLab (lab1, lab2) = raise EH.TODO
fun compare (set1, set2) = raise EH.TODO

fun ord    labelss = raise EH.TODO
fun unions labelss = raise EH.TODO

fun toList labels = raise EH.TODO

fun remFirst labels = raise EH.TODO

fun foldr f init labels = raise EH.TODO

fun exsubseteq labels labelss = raise EH.TODO

fun printLab lab = Int.toString lab

fun toString labels = raise EH.TODO

end*)
