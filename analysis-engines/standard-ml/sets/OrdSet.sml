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
 *  o Date:        25 May 2010
 *  o File name:   OrdSet.sml
 *  o Description: Defines the structure OrdSet which has signature ORDSET.
 *)


structure OrdSet :> ORDSET = struct

(*structure S = IntCBTSet
structure S = IntCBTHCSet
structure S = IntCBTHCSet2
structure S = IntCBTHCSet3*)

(*structure S = IntListSet*)
structure S = BinarySetFn (OrdKey)
(*structure S = SplaySetFn (OrdKey)*)

(* val debugString = S.debugString *)

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

fun getfirst set = S.find (fn _ => true) set

fun remfirst set =
    let
	val elt = getfirst set
    in case elt of
	   NONE => (NONE, set)
	 | SOME x => (SOME x, delete x set)
    end

fun isSingle ll = length ll = 1

fun equal set1 set2 = S.equal (set1, set2)

fun concat set1 set2 = S.union (set1, set2)

fun concatop setop set = case setop of NONE => set | SOME x => S.union (x, set)

fun concats xs = List.foldr (fn (x, xs) => concat x xs) S.empty xs

fun cons x set = S.add' (x, set)

fun ord xs = S.addList (S.empty, xs)

fun inter set1 set2 = S.intersection (set1, set2)

fun interList list set = toList (inter (ord list) set)

fun subseteq set1 set2 = S.isSubset (set1, set2)

fun subset set1 set2 = S.isSubset (set1, set2) andalso S.numItems set1 < S.numItems set2 (*not (S.equal (set1, set2))*)

(* it is true is one of the element of the second argument is a subseteq of the first argument *)
fun exsubseteq y []        = false
  | exsubseteq y [x]       = if subseteq x y then true else false
  | exsubseteq y (x :: xs) = if subseteq x y then true else exsubseteq y xs

fun subseteqin y []        = false
  | subseteqin y [x]       = if subseteq y x then true else false
  | subseteqin y (x :: xs) = if subseteq y x then true else subseteqin y xs

(**
  param e: 'a
  param le: 'a list
  returns b: boolean
  such that b iff e in le
*)
fun isin x set = S.member (set, x)

(**
  param l: int
  param ll: int list
  returns (lll, llr): (int list * int list)
  such that ll = lll @ llr
  and forall l' in lll, l' < l
  and forall l' in llr, l' >= l
*)
fun splitList x set = S.partition (fn y => case S.Key.compare (x, y) of
					       LESS    => false
					     | EQUAL   => false
					     | GREATER => true) set

(**
  param ll1: int list
  param ll2: int list
  returns ll: int list
  such that ll2 \ ll1 = ll
*)
fun difference set1 set2 = S.difference (set2, set1)

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
