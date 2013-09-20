(* Copyright 2009 2010 Heriot-Watt University
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
 *  o Date:        25 May 2010
 *  o File name:   OrdSet.sml
 *  o Description: Defines the structure OrdSet which has signature ORDSET.
 *)

(** A structure for ordered sets, constraint opaquely by the refstruct{ORDSET} signature. *)
structure OrdSet :> ORDSET = struct

structure S = BinarySetFn (OrdKey)

(** Set element, set to S.item. *)
type elt  = S.item

(** An ordered set, created using S.set *)
type ordset  = S.set

(** A list of sets. *)
type ordsets = S.set list

(** A label in this set is an #elt value. *)
type label   = elt

(** A set of labels. *)
type labels  = ordset

(** Prints out a set. *)
fun printsetgen xs f = "[" ^ #1 (S.foldr (fn (t, (s, c)) => (f t ^ c ^ s, ",")) ("", "") xs) ^ "]"


(** Prints out a set element. *)
fun printelt l = Int.toString l

(** A toString function for sets, calls #printsetgen. *)
fun toString set = printsetgen set printelt

(** A dummy label, set to zero. *)
val dummylab = 0

(** Declare a value for the first label, we start labelling from 1. *)
val firstlab = 1

(** Used to label terms, will give a fresh label. *)
val nextlab = ref firstlab

(** Sets the next label to use to the argument. *)
fun setnextlab n = nextlab := n

(** Accessor function for the next label. *)
fun getlab () = !nextlab

(** Gets a fresh label.
 * Will return a label that has not been used before to label a term.
 *)
fun freshlab () = let val x = !nextlab in (nextlab := !nextlab + 1; x) end

(** Set empty function to be the same as S.empty. *)
val empty     = S.empty

(** Set foldr function to be the same as S.foldr. *)
val foldr     = S.foldr

(** Set foldl function to be the same as S.foldl. *)
val foldl     = S.foldl

(** Set imEmpty function to be the same as S.isEmpty. *)
val isEmpty   = S.isEmpty

(** Set length function to be the same as S.numItems. *)
val length    = S.numItems

(** Set singleton function to be the same as S.singleton. *)
val singleton = S.singleton

(** Set toList function to be the same as S.listItems. *)
val toList    = S.listItems

(** Deletes an element x from a set.
 * In the case the element is not found, we return the same set back.
 *)
fun delete x set = S.delete (set, x) handle LibBase.NotFound => set

(** Returns the first element in a set. *)
fun getfirst set = S.find (fn _ => true) set

(** Removes the first element in a set. *)
fun remfirst set =
    let
	val elt = getfirst set
    in case elt of
	   NONE => (NONE, set)
	 | SOME x => (SOME x, delete x set)
    end

(** Tests whether a set has only one element.
 * \returns True if the set only has one element, false otherwise. *)
fun isSingle ll = length ll = 1

(** Given two sets, tests whether they are equal.
 * \returns True if the sets are equal, false otherwise. *)
fun equal set1 set2 = S.equal (set1, set2)

(** Unions two sets. *)
fun concat set1 set2 = S.union (set1, set2)

(** Unions two sets where the first set is a set option.
 * In the case that the option is NONE we return the original set, otherwise we union.
 *)
fun concatop setop set = case setop of NONE => set | SOME x => S.union (x, set)

(** Union a list of sets. *)
fun concats xs = List.foldr (fn (x, xs) => concat x xs) S.empty xs

(** Given a value and a set, will add a value to that set. *)
fun cons x set = S.add' (x, set)

(** Creates an ordered set from a list of integers (list of #elt values). *)
fun ord xs = S.addList (S.empty, xs)

(** Calculates the intersection of two sets. *)
fun inter set1 set2 = S.intersection (set1, set2)

(** Takes the intersection of a list of #elt values and a set. *)
fun interList list set = toList (inter (ord list) set)

(** Given two sets, will test that the first set is a subset of the second.
 * \returns True if the first set is a subset of the second, false otherwise. *)
fun subseteq set1 set2 = S.isSubset (set1, set2)

(** Given two sets, will test that the first set is a scrict subset of the second.
 * \returns True if the first set is a scrict subset of the second, false otherwise. *)
fun subset set1 set2 = S.isSubset (set1, set2) andalso S.numItems set1 < S.numItems set2 (*not (S.equal (set1, set2))*)

(** Checks that the second set is a subset of the first.
 * \returns true if the second argument is a subset of the first, false otherwise.
 *)
fun exsubseteq y []        = false
  | exsubseteq y [x]       = if subseteq x y then true else false
  | exsubseteq y (x :: xs) = if subseteq x y then true else exsubseteq y xs

(** Checks that the first set is a subset of the second.
 * \returns true if the first argument is a subset of the second, false otherwise.
 *)
fun subseteqin y []        = false
  | subseteqin y [x]       = if subseteq y x then true else false
  | subseteqin y (x :: xs) = if subseteq y x then true else subseteqin y xs

(** Given a value and a set will check whether the value is a member of the set.
 * \returns True if the element is a member of the set, false otherwise. *)
fun isin x set = S.member (set, x)

(** Splits a set into two sets, based on a value given as an argument.
 * Given a value V and a set, will return a pair of sets - the values
 * in the set which are greater than or equal to V, and the values in
 * the set which are less than V. *)
fun splitList x set = S.partition (fn y => case S.Key.compare (x, y) of
					       LESS    => false
					     | EQUAL   => false
					     | GREATER => true) set

(** Given two sets s1 and s2, with return s2 \ s1. *)
fun difference set1 set2 = S.difference (set2, set1)

(** Given two sets, will check whether they are disjoint.
 * \returns True if the sets given in the arguments are disjoint,
 * false otherwise.
 *)
fun disjoint set1 set2 = S.isEmpty (S.intersection (set1, set2))

(** Checks that the first #elt argument is in the second argument (a set), and that the size of the set is one. *)
fun isinone l ll = isin l ll andalso length ll = 1

(** Cretes a set of pairs from two sets given as arguments. *)
fun getpairs set1 set2 = foldr
			     (fn (x, y) =>
				 (foldr (fn (u, v) => (ord [x, u]) :: v) [] set1) @ y)
			     []
			     set2

(** Creates a set of pairs out of a list of sets. *)
fun getpairss sets = #1 (List.foldr
			     (fn (set, (pairs, sets)) =>
				       ((getpairs set sets) @ pairs, concat set sets))
			     ([], empty)
			     sets)

(** Filters a set out such that each elemnt of the set given as an argument is strictly less than the lim argument. *)
fun restrictLim set lim = S.filter (fn x => if x < lim then true else false) set

(** Splits a set into two halves. *)
fun splitInTwo1 set =
    let
	(** Adds an element elt to the set pset. *)
	fun push elt pset = pset := S.add (!pset, elt)

        (** Increments the counter argument. *)
	fun next counter  = counter := !counter + 1

	(* Gets the total number of items in the set. *)
	val total = S.numItems set

	(* Calculates the ceil of half of the total. *)
	val half  = ceil (Real.fromInt total / Real.fromInt 2)

	(** A ref value, set initially to the empty set. *)
	val fst   = ref S.empty

	(** A ref value, set initially to 0. *)
	val count = ref 0

	(* Filters out elements in the set such that the counter is less than half the set. *)
	val snd   = S.filter (fn elt => if !count < half
					then (next count;
					      push elt fst;
					      false)
					else true) set
    in (!fst, snd)
    end

(** Splits a set into two halves. *)
fun splitInTwo2 set =
    let
	(** Adds an element elt to the set pset. *)
	fun push elt pset = pset := S.add (!pset, elt)

        (** Increments the counter argument. *)
	fun next counter  = counter := !counter + 1

	(* Gets the total number of items in the set. *)
	val total = S.numItems set

	(* Calculates the half way point of the set using div. *)
	val half  = total div 2

	(** The first value of the set, initially set to empty. *)
	val fst   = ref S.empty

	(** The counter, a ref value set initially to 0 used in the bulid of the set. *)
	val count = ref 0

	(* Filters out elements in the set such that the counter is less than half the set. *)
	val snd   = S.filter (fn elt => if !count < half
					then (next count;
					      push elt fst;
					      false)
					else true) set
    in (!fst, snd)
    end

(** Splits a set
 * \returns a pair such that the first half is the set filtered s.t. x >= first and x < next value in the first half, with the rest in the second half of the pair. *)
fun splitInterv first next set =
    (S.filter (fn x => x >= first andalso x < next) set,
     S.filter (fn x => x < first orelse x >= next) set)

end
