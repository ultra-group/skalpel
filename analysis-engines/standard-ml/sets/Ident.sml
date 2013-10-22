(* Copyright 2009 2010 2011 2012 Heriot-Watt University
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
 *  o File name:   Ident.sml
 *)

(** Deals with identifiers, opaquely constrained by ID. *)
structure Id :> ID = struct

structure L  = Label
structure S  = BinarySetFn(OrdKey) (* S stands for Set *)
structure IS = SplayMapFn(OrdKey) (* int    -> string *)
structure SI = SplayMapFn(OrdStr) (* string -> int    *)

(** An identifier is declared as an integer. *)
type id       = int

(** A labelled identifier - a pair of an #id and a #Label.label. *)
type labelledId = id * L.label

(** Long identifier datatype.
 * Has the following constructors:
 * \arg ID - A labelled identifier (#labelledId).
 * \arg LID - A triple of a #labelledId, another constructor of this datatype, and a #Label.label. *)
datatype lid  = ID  of labelledId
	      | LID of labelledId * lid * L.label


type assoc    = {assocId : string IS.map, assocSt : int SI.map}

(** We need assocOut to check the testcase database, assocOut is the same as assoc. *)
type assocOut = (id * string) list

(** Represented by a binary tree. *)
type set      = S.set

(** Constructs a new #lid value *)
fun consLidOp id lab _ NONE = ID (id, lab)
  | consLidOp id lab1 lab2 (SOME lid) = LID ((id, lab1), lid, lab2)

(** Returns the label of a #labelledId value. *)
fun getLabIdL (_, l) = L.singleton l

(** Returns the label from a #lid value. *)
fun getTopLab (ID (_, lab))     = lab
  | getTopLab (LID (_, _, lab)) = lab

(** Returns the label id from a #lid.
 * Uses #getLabId to get labels from the LID constructor of #lid. *)
fun getLabId (ID (_, lab))     = lab
  | getLabId (LID (_, lid, _)) = getLabId lid

(** Returns the #labelledId value from a #lid. *)
fun getTopIdl (ID labelledId)          = labelledId
  | getTopIdl (LID (labelledId, _, _)) = labelledId

(** Returns the actual (id * label) pair from a #lid. *)
fun getLeftId (ID labelledId)                = labelledId
  | getLeftId (LID ((id, lab), _, _)) = (id, lab)

(** Returns a record containing label information. *)
fun getLabs (ID i) = getLabIdL i
  | getLabs (LID (i, lid, l)) = L.unionsCons [l] [(getLabIdL i), (getLabs lid)]

(** Check if id1=id2 and l1=l1 in ID (id1, l1) id2 l2, and return an option with that result *)
fun getSubLid (x as ID (id, lab)) i l =
    if id = i andalso L.eq lab l
    then SOME x
    else NONE
  | getSubLid (x as LID ((id, lab), lid, _)) i l =
    if id = i andalso L.eq lab l
    then SOME x
    else getSubLid lid i l

(** Constructs as ID constructor of #lid given an id and a label. *)
fun idToLid id lab = ID (id, lab)

(** Returns whether the argument is a long idetifier (LID constructor of #lid) or a short one. *)
fun isLong (ID  _) = false
  | isLong (LID _) = true

(** A dummy identifier, represented by the number 0. *)
val dummyId = 0

(** A ref value used to get the next identifier, initially set to 1. *)
val nextid      = ref 1

(** Sets the next identifier to the value given as an argument. *)
fun setnextid n = nextid := n

(** Gets the next identdifier. *)
fun getid    () = !nextid

(** Gets a fresh identifier by returning #nextid and incrementing its value. *)
fun freshId  () =
    let
	val x = !nextid
    in (nextid := !nextid + 1; x)
    end

(** Resets identifier count by setting #nextid back to 1 using #setnextid. *)
fun resetIds () = setnextid 1

(** Converts an identifier to an integer. *)
fun toInt   id = id

(** Converts an integer from an identfier. *)
fun fromInt id = id

(** Compares two identfiers using Int.compare. *)
fun compare (id1, id2) = Int.compare (id1, id2)

(** Chcecks whether two identifiers are equal. *)
fun eqId id1 id2 = case compare (id1, id2) of
		       EQUAL => true
		     | _ => false

(** Fold right using binary set foldr. *)
val foldr     = S.foldr
(** Fold left using binary set left. *)
val foldl     = S.foldl
(** Application using binary set app. *)
val app       = S.app
(** Tests if a set of identifiers is empty. *)
val isEmpty   = S.isEmpty
(** The empty identifier set. *)
val empty     = S.empty
(** A singleton value of an identifier. *)
val singleton = S.singleton
(** Creates a list of identifiers from a set. *)
val toList    = S.listItems
(** Adds an element to a set. *)
fun add  x set = S.add    (set, x)
(** Tests whether an element is in a set. *)
fun isin x set = S.member (set, x)
(** Unions two sets. *)
fun union      set1 set2 = S.union        (set1, set2)
(** Takes the intersection of two sets. *)
fun inter      set1 set2 = S.intersection (set1, set2)
(** Calculates the difference of two sets. *)
fun difference set1 set2 = S.difference   (set2, set1)

(** Constructs a new list. *)
fun ord xs = S.addList (empty, xs)

(** New record. *)
val emAssoc = {assocId = IS.empty, assocSt = SI.empty}

(** Updates the association. If it's not present, create an insertion. *)
fun updateAssoc str (assoc as {assocId, assocSt}) =
    case SI.find (assocSt, str) of
	NONE =>
	let
	    val id       = freshId ()
	    val assocSt' = SI.insert (assocSt, str, id)
	    val assocId' = IS.insert (assocId, id, str)
	in (id, {assocId = assocId', assocSt = assocSt'})
	end
      | SOME id => (id, assoc)

(** Finds an identifier given an integer in the binary tree. *)
fun lookupId id {assocId, assocSt} = IS.find (assocId, id)

(** Finds an identifier given an string in the binary tree. *)
fun lookupSt st {assocId, assocSt} = SI.find (assocSt, st)

(** Take out the association in the list of 3-tuples, leaving a pair. *)
fun outAssoc {assocId, assocSt} =
    IS.foldri (fn (id, st, assoc) => (id, st) :: assoc)
	      []
	      assocId

(** Puts the association information into the nodes of the IS and SI binary trees. *)
fun inAssoc assoc =
    List.foldr (fn ((id, st), {assocId, assocSt}) =>
		   {assocId = IS.insert (assocId, id, st),
		    assocSt = SI.insert (assocSt, st, id)})
	       emAssoc
	       assoc

(** Prints a list of identifiers. *)
fun printlistgen xs f = "[" ^ #1 (List.foldr (fn (t, (s, c)) => (f t ^ c ^ s, ",")) ("", "") xs) ^ "]"
(** Prints a set of idientifiers. *)
fun printsetgen  xs f = "[" ^ #1 (S.foldr    (fn (t, (s, c)) => (f t ^ c ^ s, ",")) ("", "") xs) ^ "]"

(** Prints out an identifier. *)
fun printId id = Int.toString id

(** Prints out an identifier id if it exists in the ascid map. *)
fun printId' id ascid = Option.getOpt (lookupId id ascid, printId id)

(** Prins a list of identifiers. *)
fun printIdList xs = printlistgen xs printId

(** Prints out a list of identifiers id if that exist in the ascid map. *)
fun printIdList' xs ascid = printlistgen xs (fn x => printId' x ascid)

(** Prints a labelled identifier. *)
fun printIdL (i, l) =
    "(" ^ printId i ^ "," ^ L.printLab l ^ ")"

(** Prints a labelled identifier if it exists in the association map. *)
fun printIdL' (i, l) assoc =
    "(" ^ printId' i assoc ^ "," ^ L.printLab l ^ ")"

(** Prints a identifier (gives as a string) if it exists in the association map using #printId'. *)
fun printIdLSt (i, _) assoc =
    printId' i assoc

(** Converts a #lid value to a string. *)
fun printLid (ID i)          = "ID"   ^ printIdL   i
  | printLid (LID (i, j, l)) = "LID(" ^ printIdL   i ^
			       ","    ^ printLid   j ^
			       ","    ^ L.printLab l ^ ")"

(** Converts a #lid value to a string if it exists in the association map. *)
fun printLid' (ID i)          assoc = "ID"   ^ printIdL'  i assoc
  | printLid' (LID (i, j, l)) assoc = "LID(" ^ printIdL'  i assoc ^
				      ","    ^ printLid'  j assoc ^
				      ","    ^ L.printLab l       ^ ")"

(** Converts a #lid value to a string if it exists in the association map using #printIdSt. *)
fun printLidSt (ID i)          asc = printIdLSt i asc
  | printLidSt (LID (i, j, l)) asc = printIdLSt i asc ^ "." ^
				     printLidSt j asc

(** Prints the identifier of a #labelledId. *)
fun printIdLOut (i, _) = printId i

(** Returns the identifiers of a #lid. *)
fun getLidOut (ID i)          = ([], i)
  | getLidOut (LID (i, j, l)) =
    let val (xs, x) = getLidOut j
    in (i :: xs, x) end

(** Prints the identifiers of a #lid. *)
fun printLidOut lid =
    let val (xs, x) = getLidOut lid
    in "(" ^ printlistgen xs printIdLOut ^ "," ^ printIdLOut x ^ ")"
    end

(** Prints the identifiers of a #lid in JSON format. *)
fun printJsonLidOut lid =
    let val (xs, x) = getLidOut lid
    in "{\"listgen\": " ^ printlistgen xs printIdLOut ^ ", \"Idl\": " ^ printIdLOut x ^ "}"
    end

(** Prints a set of identifiers using #printId. *)
fun toString set = printsetgen set (fn x => printId x)

(** Prints an association. *)
fun printAssoc {assocId, assocSt} =
    let
	(** Prints id and a string arguments. *)
	fun printIdSt id st = "(" ^ printId id ^ "," ^ st ^ ")"
    in "[" ^ #1 (IS.foldri (fn (id, st, (out, c)) =>
			       (printIdSt id st ^ c ^ out, ","))
			   ("", "")
			   assocId) ^ "]"
    end

(** Prints an association with string around the string value. *)
fun printAssoc' {assocId, assocSt} =
    let
	(** Prints id and a string arguments. *)
	fun printIdSt id st = "(" ^ printId id ^ "," ^ "\"" ^ st ^ "\"" ^ ")"
    in "[" ^ #1 (IS.foldri (fn (id, st, (out, c)) =>
			       (printIdSt id st ^ c ^ out, ","))
			   ("", "")
			   assocId) ^ "]"
    end

(** Prints an association in JSON format. *)
fun printJsonAssoc {assocId, assocSt} =
    let
	(** Prints id and a string arguments. *)
	fun printIdSt id st = "{\"id\": " ^ (printId id) ^ ", \"str\": \"" ^ st ^ "\"}"
    in "[" ^ #1 (IS.foldri (fn (id, st, (out, c)) =>
			       (printIdSt id st ^ c ^ out, ","))
			   ("", "")
			   assocId) ^ "]"
    end

end
