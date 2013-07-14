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


structure OrdSet :> ORDSET = struct

structure S = BinarySetFn (OrdKey)

(* type declarations *)
type elt     = S.item
type ordset  = S.set
type ordsets = S.set list
type label   = elt
type labels  = ordset

(* Printing *)
fun printsetgen xs f = "[" ^ #1 (S.foldr (fn (t, (s, c)) => (f t ^ c ^ s, ",")) ("", "") xs) ^ "]"
fun printelt l = Int.toString l
fun toString set = printsetgen set printelt

(* label values and functions *)
val dummylab = 0
val firstlab = 1
val nextlab = ref firstlab
fun setnextlab n = nextlab := n
fun getlab () = !nextlab
fun freshlab () = let val x = !nextlab in (nextlab := !nextlab + 1; x) end

(* localise functions *)
val empty     = S.empty
val foldr     = S.foldr
val foldl     = S.foldl
val isEmpty   = S.isEmpty
val length    = S.numItems
val singleton = S.singleton
val toList    = S.listItems

(* typical set funtions *)
fun delete x set = S.delete (set, x) handle LibBase.NotFound => set

(** Returns the first element in a set *)
fun getfirst set = S.find (fn _ => true) set

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

fun concatop setop set = case setop of NONE => set | SOME x => S.union (x, set)

fun concats xs = List.foldr (fn (x, xs) => concat x xs) S.empty xs

(* Given a value and a set, will add a value to that set. *)
fun cons x set = S.add' (x, set)

fun ord xs = S.addList (S.empty, xs)

(* Calculates the intersection of two sets. *)
fun inter set1 set2 = S.intersection (set1, set2)

fun interList list set = toList (inter (ord list) set)

(** Given two sets, will test that the first set is a subset of the second.
 * \returns True if the first set is a subset of the second, false otherwise. *)
fun subseteq set1 set2 = S.isSubset (set1, set2)

(** Given two sets, will test that the first set is a scrict subset of the second.
 * \returns True if the first set is a scrict subset of the second, false otherwise. *)
fun subset set1 set2 = S.isSubset (set1, set2) andalso S.numItems set1 < S.numItems set2 (*not (S.equal (set1, set2))*)

(* it is true is one of the element of the second argument is a subseteq of the first argument *)
fun exsubseteq y []        = false
  | exsubseteq y [x]       = if subseteq x y then true else false
  | exsubseteq y (x :: xs) = if subseteq x y then true else exsubseteq y xs

fun subseteqin y []        = false
  | subseteqin y [x]       = if subseteq y x then true else false
  | subseteqin y (x :: xs) = if subseteq y x then true else subseteqin y xs

(** Given a value and a set will check whether the value is a member of the set.
 * \returns True if the element is a member of the set, false otherwise. *)
fun isin x set = S.member (set, x)

(**
  param l: int
  param ll: int list
  returns (lll, llr): (int list * int list)
  such that ll = lll @ llr
  and forall l' in lll, l' < l
  and forall l' in llr, l' >= l
*)
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

fun isinone l ll = isin l ll andalso length ll = 1

fun getpairs set1 set2 = foldr
			     (fn (x, y) =>
				 (foldr (fn (u, v) => (ord [x, u]) :: v) [] set1) @ y)
			     []
			     set2

fun getpairss sets = #1 (List.foldr
			     (fn (set, (pairs, sets)) =>
				       ((getpairs set sets) @ pairs, concat set sets))
			     ([], empty)
			     sets)

fun restrictLim set lim = S.filter (fn x => if x < lim then true else false) set

fun splitInTwo1 set =
    let
	fun push elt pset = pset := S.add (!pset, elt)
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

fun splitInTwo2 set =
    let
	fun push elt pset = pset := S.add (!pset, elt)
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

fun splitInterv first next set =
    (S.filter (fn x => x >= first andalso x < next) set,
     S.filter (fn x => x < first orelse x >= next) set)

end
