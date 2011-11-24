(* Copyright 2009 Heriot-Watt University
 * Copyright 2010 Heriot-Watt University
 * Copyright 2011 Heriot-Watt University
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
 *  o File name:   Ident.sml
 *  o Description: This file defines the structure Id to deal with
 *      identifiers which has signature ID.
 *)


structure Id :> ID = struct

structure L  = Label
structure S  = BinarySetFn(OrdKey) (* S stands for Set *)
structure IS = SplayMapFn(OrdKey) (* int    -> string *)
structure SI = SplayMapFn(OrdStr) (* string -> int    *)

type id       = int
type idl      = id * L.label

(* long identifiers *)
datatype lid  = ID  of idl
	      | LID of idl * lid * L.label


type assoc    = {assocId : string IS.map, assocSt : int SI.map}
type assocOut = (id * string) list

(* new binary tree *)
type set      = S.set

(* constructs a new lid *)
fun consLidOp id lab _ NONE = ID (id, lab)
  | consLidOp id lab1 lab2 (SOME lid) = LID ((id, lab1), lid, lab2)

(* returns the label of the (id, label) pair) *)
fun getLabIdL (_, l) = L.singleton l

(* returns the label from a long id *)
fun getTopLab (ID (_, lab))     = lab
  | getTopLab (LID (_, _, lab)) = lab

(* returns the label id from a long id *)
fun getLabId (ID (_, lab))     = lab
  | getLabId (LID (_, lid, _)) = getLabId lid

(* returns the lid value from a long id *)
fun getTopIdl (ID idl)          = idl
  | getTopIdl (LID (idl, _, _)) = idl

(* returns the actual (id * label) pair from a long id *)
fun getLeftId (ID idl)                = idl
  | getLeftId (LID ((id, lab), _, _)) = (id, lab)

(* returns a record containing label information *)
fun getLabs (ID i) = getLabIdL i
  | getLabs (LID (i, lid, l)) =
    L.cons l (L.union (getLabIdL i) (getLabs lid))

(* check if id1=id2 and l1=l1 in ID (id1, l1) id2 l2, and return
   an option with that result *)
fun getSubLid (x as ID (id, lab)) i l =
    if id = i andalso L.eq lab l
    then SOME x
    else NONE
  | getSubLid (x as LID ((id, lab), lid, _)) i l =
    if id = i andalso L.eq lab l
    then SOME x
    else getSubLid lid i l

(* puts an id and a label into the lid datatype *)
fun idToLid id lab = ID (id, lab)

(* returns whether the argument is a long idetifier or not *)
fun isLong (ID  _) = false
  | isLong (LID _) = true

val dummyId = 0

val nextid      = ref 1
fun setnextid n = nextid := n
fun getid    () = !nextid
fun freshId  () = let val x = !nextid in (nextid := !nextid + 1; x) end
fun resetIds () = setnextid 1

(* converts id to and from an integer *)
fun toInt   id = id
fun fromInt id = id

(* compare and equality test functions *)
fun compare (id1, id2) = Int.compare (id1, id2)
fun eqId id1 id2 = case compare (id1, id2) of
		       EQUAL => true
		     | _ => false

(* set functions from ORD_SET *)
val foldr     = S.foldr
val foldl     = S.foldl
val app       = S.app
val foldrIds  = foldr
val foldrDom  = foldr
val isEmpty   = S.isEmpty
val empty     = S.empty
val singleton = S.singleton
val toList    = S.listItems
fun add  x set = S.add    (set, x)
fun isin x set = S.member (set, x)
fun union      set1 set2 = S.union        (set1, set2)
fun inter      set1 set2 = S.intersection (set1, set2)
fun difference set1 set2 = S.difference   (set2, set1)

(* constructs a new list *)
fun ord xs = S.addList (empty, xs)

(* new record *)
val emAssoc = {assocId = IS.empty, assocSt = SI.empty}

(* updates the association. If it's not present, create an insertion *)
fun updateAssoc str (assoc as {assocId, assocSt}) =
    case SI.find (assocSt, str) of
	NONE => let val id       = freshId ()
		    val assocSt' = SI.insert (assocSt, str, id)
		    val assocId' = IS.insert (assocId, id, str)
		in (id, {assocId = assocId', assocSt = assocSt'})
		end
      | SOME id => (id, assoc)

(* finds an id in the binary tree *)
fun lookupId id {assocId, assocSt} = IS.find (assocId, id)
fun lookupSt st {assocId, assocSt} = SI.find (assocSt, st)

(* take out the association in the list of 3-tuples, leaving a pair *)
fun outAssoc {assocId, assocSt} =
    IS.foldri (fn (id, st, assoc) => (id, st) :: assoc)
	      []
	      assocId

(* puts the association information into the nodes of the IS and SI binary trees *)
fun inAssoc assoc =
    List.foldr (fn ((id, st), {assocId, assocSt}) =>
		   {assocId = IS.insert (assocId, id, st),
		    assocSt = SI.insert (assocSt, st, id)})
	       emAssoc
	       assoc


(* PRINTING SECTION *)


fun printlistgen xs f = "[" ^ #1 (List.foldr (fn (t, (s, c)) => (f t ^ c ^ s, ",")) ("", "") xs) ^ "]"
fun printsetgen  xs f = "[" ^ #1 (S.foldr    (fn (t, (s, c)) => (f t ^ c ^ s, ",")) ("", "") xs) ^ "]"

fun printId id = Int.toString id
fun printId' id ascid = Option.getOpt (lookupId id ascid, printId id)

fun printIdList xs = printlistgen xs printId
fun printIdList' xs ascid = printlistgen xs (fn x => printId' x ascid)

fun printIdL (i, l) =
    "(" ^ printId i ^ "," ^ (*Int.toString l*) L.printLab l ^ ")"
fun printIdL' (i, l) assoc =
    "(" ^ printId' i assoc ^ "," ^ L.printLab l ^ ")"
fun printIdLSt (i, _) assoc =
    printId' i assoc

fun printLid (ID i)          = "ID"   ^ printIdL   i
  | printLid (LID (i, j, l)) = "LID(" ^ printIdL   i ^
			       ","    ^ printLid   j ^
			       ","    ^ L.printLab l ^ ")"

fun printLid' (ID i)          assoc = "ID"   ^ printIdL'  i assoc
  | printLid' (LID (i, j, l)) assoc = "LID(" ^ printIdL'  i assoc ^
				      ","    ^ printLid'  j assoc ^
				      ","    ^ L.printLab l       ^ ")"

fun printLidSt (ID i)          asc = printIdLSt i asc
  | printLidSt (LID (i, j, l)) asc = printIdLSt i asc ^ "." ^
				     printLidSt j asc

fun printIdLOut (i, _) = printId i
fun getLidOut (ID i)          = ([], i)
  | getLidOut (LID (i, j, l)) =
    let val (xs, x) = getLidOut j
    in (i :: xs, x) end
fun printLidOut lid =
    let val (xs, x) = getLidOut lid
    in "(" ^ printlistgen xs printIdLOut ^ "," ^ printIdLOut x ^ ")"
    end

fun printJsonLidOut lid =
    let val (xs, x) = getLidOut lid
    in "{\"listgen\": " ^ printlistgen xs printIdLOut ^ ", \"Idl\": " ^ printIdLOut x ^ "}"
    end

fun toString set = printsetgen set (fn x => printId x)

(*fun toStringList set ascid =
    S.foldr (fn (x, y) => (printId' x ascid) :: y) [] set*)

(*fun toStringSt' set assoc = printsetgen set (fn x => printId' x assoc)*)

fun printAssoc {assocId, assocSt} =
    let fun printIdSt id st = "(" ^ printId id ^ "," ^ st ^ ")"
    in "[" ^ #1 (IS.foldri (fn (id, st, (out, c)) =>
			       (printIdSt id st ^ c ^ out, ","))
			   ("", "")
			   assocId) ^ "]"
    end

fun printAssoc' {assocId, assocSt} =
    let fun printIdSt id st = "(" ^ printId id ^ "," ^ "\"" ^ st ^ "\"" ^ ")"
    in "[" ^ #1 (IS.foldri (fn (id, st, (out, c)) =>
			       (printIdSt id st ^ c ^ out, ","))
			   ("", "")
			   assocId) ^ "]"
    end

fun printJsonAssoc {assocId, assocSt} =
    let fun printIdSt id st = "\"" ^ st ^ "\""
    in "[" ^ #1 (IS.foldri (fn (id, st, (out, c)) =>
			       (printIdSt id st ^ c ^ out, ","))
			   ("", "")
			   assocId) ^ "]"
    end

end
