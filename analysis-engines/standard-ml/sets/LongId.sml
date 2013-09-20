(* Copyright 2009 2010 2011 Heriot-Watt University
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
 *  o File name:   LongId.sml
 *)

(** Defined to deal with long identifiers - opaquely constrained by LONGID. *)
structure LongId :> LONGID = struct

structure I  = Id
structure L  = Label
structure LI = OrdLid
structure CD = BinarySetFn(LI)

(** Defined as an LI.ord_key is Id.id which is an int. *)
type key     = LI.ord_key

(** Used to check the testcase database. *)
type keyOut  = int list * int

(** A set of long identifiers. *)
type set     = CD.set

(** Takes in k of type Id.lid and returns something of type key (LI.ord_key) *)
fun buildKey k = k

fun inKey (ids, id) =
    foldr (fn (x, lid) =>
	      I.LID ((I.fromInt x, L.dummyLab),
		     lid,
		     L.dummyLab))
	  (I.ID (I.fromInt id, L.dummyLab))
	  ids

fun outKey (I.ID (id, _)) = ([], I.toInt id)
  | outKey (I.LID ((id, _), lid, _)) =
    let val (xs, x) = outKey lid
	val y = I.toInt id
    in (y :: xs, x) end

(** Empty set of long identifiers. *)
val empty = CD.empty
(** Folding left over long identifiers. *)
val foldl = CD.foldl
(** Folding right over long identifiers. *)
val foldr = CD.foldr
(** Tests whether two sets of long identifiers are equal. *)
fun equal        set1 set2 = CD.equal        (set1, set2)
(** Tests whether one set of long identifiers is a subset of another. *)
fun subset       set1 set2 = CD.isSubset     (set1, set2) andalso not (equal set1 set2)
(** Tests whether one set of long identifiers is a *strict* subset of another. *)
fun subseteq     set1 set2 = CD.isSubset     (set1, set2)
(** Unions two sets of long identifiers. *)
fun union        set1 set2 = CD.union        (set1, set2)
(** Intersection of two sets of long identifiers. *)
fun inter        set1 set2 = CD.intersection (set1, set2)
(** Difference of two sets of long identifiers. *)
fun difference   set1 set2 = CD.difference   (set2, set1) (* removes set1 from set2 *)
(** Tests whether a set of long identifiers is empty. *)
fun isEmpty   set = CD.isEmpty set
(** Returns the length of a set of long identifiers. *)
fun length    set = CD.numItems set
(** Checks that the length of the set of long identifiers is 1. *)
fun isSingle  set = length set = 1
(** Converts a set of long identifiers to a list. *)
fun toList    set = CD.listItems set
(** A singleton value of a long identifier. *)
fun singleton x = CD.singleton x
(** Constructs a singleton long identifier set. *)
fun sing x lab = singleton (buildKey (I.idToLid x lab))
(** Adds a long identifier to a set. *)
fun add x set = CD.add' (x, set)
(** Constructs a set of long identifiers from a list of long identifiers. *)
fun ord xs = CD.addList (empty, xs)
(** Tests whether a long identifier is a member of a set of long identifiers. *)
fun isin x set = CD.member (set, x)

fun toOutList set = map (fn x => outKey x) (CD.listItems set)

(** Generates a set with dummy labels. *)
fun inSet xs = CD.addList (empty, map (fn x => inKey x) xs)

(** Generalisied union of sets *)
fun unions xs = List.foldr (fn (x, xs) => union x xs) empty xs

fun toLong deps = I.foldr (fn (x, set) => add (I.idToLid x L.dummyLab) set) empty deps

(** Long sets are generated from the Id.set using dummy labels. *)
fun diffShort deps set = difference (toLong deps) set

(** Tests whether the intersection of the deps and the set is empty. *)
fun disjShort deps set = isEmpty (inter (toLong deps) set)

(** The foldr is always going to get us the empty list
 * we can get the same output in the test files via an list of integers? *)
fun printsetgen xs f = "[" ^ #1 (CD.foldr (fn (t, (s, c)) => (f t ^ c ^ s, ",")) ("", "") xs) ^ "]"

(** Converts a set of long identifiers to a string using #printsetgen. *)
fun toString set = printsetgen set I.printLid

(** Same as #toString. *)
fun toStringOut set = printsetgen set I.printLidOut

(** Converts a set of long identifiers to a string using #printsetgen in JSON format. *)
fun toJsonStringOut set = printsetgen set I.printJsonLidOut

(** Converts a list of long identifiers to a list of strings using #printsetgen in JSON format. *)
fun toStringList set ascid =
    CD.foldr (fn (x, y) => (I.printLidSt x ascid) :: y) [] set

(** Converts a list of long identifiers to a string using #printsetgen in JSON format. *)
fun toStringListSt set ascid =
    #1 (List.foldr
	    (fn (x, (y, b)) => (x ^ b ^ y, ", "))
	    ("", "")
	    (toStringList set ascid))

end
