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
 *)

(** Opaquely constraned by LABEL and is used to represent program locations. *)
structure Label :> LABEL = struct

structure S  = BinarySetFn (OrdKey)
structure EH = ErrorHandler
structure D  = Debug

(** A program position. Declared to be of type S.item. *)
type label  = S.item

(** A set of program position, declared as an S.set. *)
type labels = S.set

(** Prints a set of labels. *)
fun printsetgen xs f = "[" ^ #1 (S.foldr (fn (t, (s, c)) => (f t ^ c ^ s, ",")) ("", "") xs) ^ "]"

(** Prints out a label
 * Uses Int.toString. *)
fun printLab l = Int.toString l

(** Pritns out a set of labels using #printsetgen. *)
fun toString set = printsetgen set printLab

(** A dummy label, we represent this with the number 0. *)
val dummyLab   = 0

(** A built-in label, represented by the number 1. *)
val builtinLab = 1

(** The first number to start labelling from, set to 2. *)
val firstLab   = 2

(** A reference to the next label that we can use which hasn't been used before.
 * Initially set to be a reference to #firstLab but updated by #setNextLab. *)
val nextlab    = ref firstLab

(** Sets the #nextlab variabe to the number passed in as an argument. *)
fun setNextLab n = nextlab := n

(** Accesser function for #nextlab. *)
fun getlab    () = !nextlab

(** Returns the number of a fresh label.
 * Returns the value of #nextlab and increments its value by one. *)
fun freshlab  () =
    let
	val x = !nextlab
    in
	(nextlab := !nextlab + 1; x)
    end

(** Resets the #nextlab back to the #firstLab value (constant). *)
fun resetNext () = setNextLab firstLab

(** The empty label set. *)
val empty     = S.empty

(** Folding right over labels. *)
val foldr     = S.foldr

(** Folding left over labels. *)
val foldl     = S.foldl

(** A function to test whether a label set is empty. *)
val isEmpty   = S.isEmpty

(** For determining the length of a label set. *)
val length    = S.numItems

(** A singleton value. *)
val singleton = S.singleton

(** Function for turning a set of labels into a list of labels. *)
val toList    = S.listItems

(** Returns an integer from a value of type Label.label. *)
fun toInt   lab = lab

(** Returns a label of type Label.label given an integer. *)
fun fromInt lab = lab

(** Increments the label counter by one. *)
fun nextLabel lab = lab + 1

(** Gets the nth next label number. *)
fun nextLabN lab 0 = lab
  | nextLabN lab n = if n < 0 then raise EH.DeadBranch "" else (nextLabN lab (n-1)) + 1

(** Removes an element from a set. *)
fun delete x set = S.delete (set, x) handle LibBase.NotFound => set

(** Tests whether two labels are equal.
 * \returns true if the labels are equal, false otherwise *)
fun eq x (y : label) = (x = y)

(** Given two integers, returns the lowest vaulue one. *)
fun min x y = Int.min (x, y)

(** Returns the first value in the set given as an argument. *)
fun getfirst set = S.find (fn _ => true) set

(** Removes the first element in a set given as an argument. *)
fun remFirst set =
    let val elt = getfirst set
    in case elt of
	   NONE => (NONE, set)
	 | SOME x => (SOME x, delete x set)
    end

(** Checks that the set given as an argument has only one element. *)
fun isSingle ll = length ll = 1

(** Determines if two sets are equal. *)
fun equal set1 set2 = S.equal (set1, set2)

(** Unions sets. *)
fun union set1 set2 = S.union (set1, set2)

(** Adds setop (an OPTION) to set, if NONE then just returns original set. *)
fun unionop setop set = case setop of NONE => set | SOME x => S.union (x, set)

(** Generalised union. *)
fun unions sets =
    (List.foldr (fn (set1, set2) => union set1 set2)
		(List.hd sets)
		(List.tl sets))
    handle Empty => S.empty

(** Adds an element to a set given as an argument. *)
fun cons x set = S.add' (x, set)

(** Creates a new set given a list of elements. *)
fun ord xs = S.addList (S.empty, xs)

(** Return the intersection of two sets. *)
fun inter set1 set2 = S.intersection (set1, set2)

(** Returns the intersection of a list and a set as a list. *)
fun interList list set = toList (inter (ord list) set)

(** Tests if the set in the first argument is a subset of the set given as the second argument. *)
fun subseteq set1 set2 = S.isSubset (set1, set2)

(** Tests if the set in the first argument is a \b script subset of the set given as the second argument. *)
fun subset set1 set2 = S.isSubset (set1, set2) andalso S.numItems set1 < S.numItems set2

(** Tests that all elements in 'sets' are a subset of 'set'. *)
fun exsubseteq set sets = List.exists (fn set' => subseteq set' set) sets

(** Test sthat 'set' is a subsets of all sets in the 'sets' argument. *)
fun subseteqin set sets = List.exists (fn set' => subseteq set set') sets

(** Tests whether an element is in a set. *)
fun isin x set = S.member (set, x)

(** Splits a list into two pieces.
 * \returns (lll,llr) s.t. $\forall$ l' in lll, l' < x and $\forall$ l' in llr, l' >= x *)
fun split x set = S.partition (fn y => case S.Key.compare (x, y) of
					       LESS    => false
					     | EQUAL   => false
					     | GREATER => true) set

(** Returns the set difference (set2 \ set1). *)
fun diff set1 set2 = S.difference (set2, set1)

(** Returns whether any members of set1 are in set2. *)
fun disjoint set1 set2 = S.isEmpty (S.intersection (set1, set2))

(** Checks whether l is equal to ll where ll has only one element. *)
fun isinone l ll = isin l ll andalso length ll = 1

(** Returns elements of a set that are less than the value specified in the 'lim' argument. *)
fun restrictLim set lim = S.filter (fn x => if x < lim then true else false) set

(** Splits a set into two halves (using ceil and the '/' operator). *)
fun splitIn2 set =
    let
	(** Adds an element 'elt' to the set 'pset'. *)
	fun push elt pset = pset := S.add (!pset, elt)
	(** Increments the counter argument. *)
	fun next counter  = counter := !counter + 1
	(** Find the total number of items in the set. *)
	val total = S.numItems set
	(** Locate the midpoint. *)
	val half  = ceil (Real.fromInt total / Real.fromInt 2)
	(** Set the first half value initially to a ref of the empty set. *)
	val fst   = ref S.empty
	(** A counter used to track how many elements we have moved into #fst so far. *)
	val count = ref 0
	(** Second half of the set that we are splitting in two. *)
	val snd   = S.filter (fn elt => if !count < half
					then (next count;
					      push elt fst;
					      false)
					else true) set
    in (!fst, snd)
    end

(** Splits a set into two halves (using div). *)
fun splitIn2' set =
    let
	(** Adds an element 'elt' to the set 'pset'. *)
	fun push elt pset = pset := S.add (!pset, elt)
	(** Increments the counter argument. *)
	fun next counter  = counter := !counter + 1
	(** Find the total number of items in the set. *)
	val total = S.numItems set
	(** Locate the midpoint. *)
	val half  = total div 2
	(** Set the first half value initially to a ref of the empty set. *)
	val fst   = ref S.empty
	(** A counter used to track how many elements we have moved into #fst so far. *)
	val count = ref 0
	(** Second half of the set that we are splitting in two. *)
	val snd   = S.filter (fn elt => if !count < half
					then (next count;
					      push elt fst;
					      false)
					else true) set
    in (!fst, snd)
    end

(** Splits the set argument into two by setting those less than the value in 'next' in one half and the rest in the other half. *)
fun split2 first next set =
    (S.filter (fn x => x >= first andalso x < next) set,
     S.filter (fn x => x < first orelse x >= next) set)

(** Compares two labels using Int.compare. *)
fun compareLab (lab1, lab2) = Int.compare (lab1, lab2)

(** Compares two sets of labels. *)
fun compare (set1, set2) = S.compare (set1, set2)

end


(** A label set using integers (using ord_key). *)
structure Label2 :> LABEL = struct

structure O  = OrdKey
structure S  = SplayMapFn(O)
structure EH = ErrorHandler

(** Using an ord_key for the type of label. *)
type label  = O.ord_key

(** A map of labels. *)
type map    = label S.map

type labels = {from : map, to : map, size : int}

exception DONE of bool

(** A dummy label, represented by 0. *)
val dummyLab   = 0

(** A built-in label, reperesnted by 1. *)
val builtinLab = 1

(** The first label that we use when assigning to progarm points, set to 2. *)
val firstLab   = 2

(** Converts a label to an integer. *)
fun toInt   lab = lab

(** Converts from an integer to a label. *)
fun fromInt lab = lab

(** Returns the next fresh label.
 * Initially set to #firstLab. *)
val nextlab = ref firstLab

(** Increments the argument by one. *)
fun nextLabel lab = lab + 1

fun nextLabN lab 0 = lab
  | nextLabN lab n = if n < 0 then raise EH.DeadBranch "" else (nextLabN lab (n-1)) + 1

(** Sets the #nextlab value to be the same as the argument. *)
fun setNextLab lab = nextlab := lab

(** Resets the #nextlab value to #firstLab. *)
fun resetNext () = setNextLab firstLab

(** Tests whether two labels are equal. *)
fun eq lab1 (lab2 : label) = (lab1 = lab2)

(** Returns the smaller of the two labels given as arguments. *)
fun min lab1 lab2 = Int.min (lab1, lab2)

(** The empty set of labels. *)
val empty = let val map : map = S.empty in {from = map, to = map, size = 0} end

(** Returns a set of labels with just one element - the argument. *)
fun singleton lab =
    let val map = S.insert (S.empty, lab, lab)
    in {from = map, to = map, size = 1}
    end

(** Tests whether the argument label is in the labels argument. *)
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

(** Tests that the 'lab' argument is present in the labals set, and that the size of the labels is 1. *)
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
    S.foldli (fn (f', t', (labels, bf, bt)) => raise EH.TODO "no description, raised in the 'add' function of Label.sml")
	     (S.empty, false, false)
	     labels

(** A dummy function declared to satisfy the signature, raises an exception. *)
fun delete lab (labels as {from = mfrom, to = mto, size}) = raise EH.TODO "no description, raised in the 'delete' function of Label.sml"

(** Returns whether or not a set of labels is empty. *)
fun isEmpty  {from, to, size} = (size = 0)
fun isSingle {from, to, size} =
    size = 1 andalso S.foldli (fn (f, t, b) => b andalso f = t) true from

(** Returns legth of a set of labels. *)
fun length   {from, to, size} =
    S.foldli (fn (f, t, n) => n + (t - f + 1)) 0 from

(** A dummy function declared to satisfy the signature, raises an exception. *)
fun union labels {from, to, size} =
    S.foldli (fn (f, t, {from, to, size}) => raise EH.TODO "no description, raised in the 'union' function of Label.sml")
	     labels
	     from


(** A dummy function declared to satisfy the signature, raises an exception. *)
fun diff     labels1 labels2 = raise EH.TODO "no description, raised in the 'diff' function of Label.sml"
(** A dummy function declared to satisfy the signature, raises an exception. *)
fun inter    labels1 labels2 = raise EH.TODO "no description, raised in the 'inter' function of Label.sml"
(** A dummy function declared to satisfy the signature, raises an exception. *)
fun disjoint labels1 labels2 = raise EH.TODO "no description, raised in the 'disjoint' function of Label.sml"
(** A dummy function declared to satisfy the signature, raises an exception. *)
fun subset   labels1 labels2 = raise EH.TODO "no description, raised in the 'subset' function of Label.sml"
(** A dummy function declared to satisfy the signature, raises an exception. *)
fun subseteq labels1 labels2 = raise EH.TODO "no description, raised in the 'subseteq' function of Label.sml"

(** A dummy function declared to satisfy the signature, raises an exception. *)
fun split  lab labels = raise EH.TODO "no description, raised in the 'split' function of Label.sml"
(** A dummy function declared to satisfy the signature, raises an exception. *)
fun split2 lab1 lab2 labels = raise EH.TODO "no description, raised in the 'split2' function of Label.sml"
(** A dummy function declared to satisfy the signature, raises an exception. *)
fun splitIn2  labels = raise EH.TODO "no description, raised in the 'splitIn2' function of Label.sml"
(** A dummy function declared to satisfy the signature, raises an exception. *)
fun splitIn2' labels = raise EH.TODO "no description, raised in the 'splitIn2'' function of Label.sml"

(** A dummy function declared to satisfy the signature, raises an exception. *)
fun compareLab (lab1, lab2) = raise EH.TODO "no description, raised in the 'compareLab' function of Label.sml"
(** A dummy function declared to satisfy the signature, raises an exception. *)
fun compare (set1, set2) = raise EH.TODO "no description, raised in the 'compare' function of Label.sml"

(** A dummy function declared to satisfy the signature, raises an exception. *)
fun ord    labelss = raise EH.TODO "no description, raised in the 'ord' function of Label.sml"
(** A dummy function declared to satisfy the signature, raises an exception. *)
fun unions labelss = raise EH.TODO "no description, raised in the 'unions' function of Label.sml"

(** A dummy function declared to satisfy the signature, raises an exception. *)
fun toList labels = raise EH.TODO "no description, raised in the 'toList' function of Label.sml"

(** A dummy function declared to satisfy the signature, raises an exception. *)
fun remFirst labels = raise EH.TODO "no description, raised in the 'remFirst' function of Label.sml"

(** A dummy function declared to satisfy the signature, raises an exception. *)
fun foldr f init labels = raise EH.TODO "no description, raised in the 'foldr' function of Label.sml"

(** A dummy function declared to satisfy the signature, raises an exception. *)
fun exsubseteq labels labelss = raise EH.TODO "no description, raised in the 'exsubseteq' function of Label.sml"

(** A dummy function declared to satisfy the signature, raises an exception. *)
fun printLab lab = Int.toString lab

(** A dummy function declared to satisfy the signature, raises an exception. *)
fun toString labels = raise EH.TODO "no description, raised in the 'toString' function of Label.sml"


end
