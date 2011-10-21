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
 *  o File name:   LongId.sml
 *  o Description: This file defines the structure LongId to deal with
 *      long identifiers.  The structure LongId has signature LONGID.
 *)


structure LongId :> LONGID = struct

(* shorten the names of structures *)
structure I  = Id
structure L  = Label
structure LI = OrdLid
structure CD = BinarySetFn(LI)

(* LI.ord_key is Id.id which is an int *)
type key     = LI.ord_key
type keyOut  = int list * int
type set     = CD.set

(* takes in k of type Id.lid and returns something of type key (LI.ord_key) *)
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

(* standard set functions such as foldl, foldr, subset, union etc *)
val empty = CD.empty
val foldl = CD.foldl
val foldr = CD.foldr
fun equal        set1 set2 = CD.equal        (set1, set2)
fun subset       set1 set2 = CD.isSubset     (set1, set2) andalso not (equal set1 set2)
fun subseteq     set1 set2 = CD.isSubset     (set1, set2)
fun union        set1 set2 = CD.union        (set1, set2)
fun inter        set1 set2 = CD.intersection (set1, set2)
fun difference   set1 set2 = CD.difference   (set2, set1) (* removes set1 from set2 *)
fun isEmpty   set = CD.isEmpty set
fun length    set = CD.numItems set
fun isSingle  set = length set = 1
fun toList    set = CD.listItems set
fun singleton x = CD.singleton x
fun sing x lab = singleton (buildKey (I.idToLid x lab))
fun add x set = CD.add' (x, set)
fun ord xs = CD.addList (empty, xs)
fun isin x set = CD.member (set, x)

fun toOutList set = map (fn x => outKey x) (CD.listItems set)
fun inSet xs = CD.addList (empty, map (fn x => inKey x) xs)

(* generalisied union of sets *)
fun unions xs = List.foldr (fn (x, xs) => union x xs) empty xs

fun toLong deps = I.foldr (fn (x, set) => add (I.idToLid x L.dummyLab) set) empty deps

(*fun unionShort deps set = union (toLong deps) set*)

fun diffShort deps set = difference (toLong deps) set

fun disjShort deps set = isEmpty (inter (toLong deps) set)

(* print functions for the structure *)
fun printlistgen xs f = "[" ^ #1 (List.foldr (fn (t, (s, c)) => (f t ^ c ^ s, ",")) ("", "") xs) ^ "]"

fun printsetgen xs f = "[" ^ #1 (CD.foldr (fn (t, (s, c)) => (f t ^ c ^ s, ",")) ("", "") xs) ^ "]"

(*fun toStringIdList xs = printlistgen xs I.printId

fun toStringKey (is, i) = "(" ^ toStringIdList is ^ "," ^ I.printId i ^ ")"

fun toStringIdSt id ascid = I.printId' id ascid*)

(*fun toStringKeySt (is, i) ascid =
    foldr (fn (x, y) => toStringIdSt x ascid ^ "." ^ y) "" is ^
    toStringIdSt i ascid*)

fun toString set = printsetgen set I.printLid

fun toStringOut set = printsetgen set I.printLidOut

fun toStringList set ascid =
    CD.foldr (fn (x, y) => (I.printLidSt x ascid) :: y) [] set

fun toStringListSt set ascid =
    #1 (List.foldr
	    (fn (x, (y, b)) => (x ^ b ^ y, ", "))
	    ("", "")
	    (toStringList set ascid))

end
