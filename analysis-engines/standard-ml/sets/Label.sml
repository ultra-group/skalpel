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

(** A structure where labels are stored in hash tables to boolean values.
 * (2013-10-07-13:44) The boolean values could be used to speed up minimisation, but this has not yet been tested.
 *)
structure Label :> LABEL =
struct

structure S  = HashTable
structure EH = ErrorHandler
structure D = Debug

val unionSizes = ref []

(** A program position. Declared to be of type S.item. *)
type label = int

fun printLab l = Int.toString l

(** A set of program position, declared as an S.set. *)
type ('a, 'b) labels = ('a, 'b) S.hash_table option

(** A dummy label, represeneted by the number 0. *)
val dummyLab = 0

(** A built-in label, represented by the number 1. *)
val builtinLab = 1

(** The first number to start labelling from, set to 2. *)
val firstLab   = 2

(** A reference to the next label that we can use which hasn't been used before.
  * Initially set to be a reference to #firstLab but updated by #setNextLab. *)
val nextlab    = ref firstLab

(** Tests whether two labels are equal. *)
fun eq lab1 (lab2 : label) = (lab1 = lab2)

(** Returns a label of type Label.label given an integer as an argument. *)
fun fromInt lab = lab

(** Compares two labels for equality. *)
fun compareLab (lab1, lab2) = Int.compare (lab1, lab2)

(** Increments the label by one. *)
fun nextLabel lab = lab + 1

(** Tests if the hash table has only one element. *)
fun isSingle NONE = false
  | isSingle (SOME labelMap) = (S.numItems labelMap) = 1

(** Gets the nth next label number. *)
fun nextLabN lab 0 = lab
  | nextLabN lab n = if n < 0 then raise EH.DeadBranch "" else (nextLabN lab (n-1)) + 1

(** Tests whether a map of labels is empty. *)
fun isEmpty NONE = true
  | isEmpty (SOME labelMap) = (S.numItems labelMap) = 0

(** Sets the #nextlab value to be the same as the argument. *)
fun setNextLab lab = nextlab := lab

(** Gets the length of a map of labels to booleans. *)
val length = fn x => case x of
			 NONE => 0
		       | SOME y => S.numItems y

(** Given two labels, returns the lowest vaulue one using Int.min. *)
fun min x y = Int.min (x, y)

(** Resets the #nextlab back to the #firstLab value (constant). *)
fun resetNext () = setNextLab firstLab

(** Returns an integer from a value of type Label.label. *)
fun toInt   lab = lab

(** Tests whether a label is in the hash table. *)
fun isin key NONE = false
  | isin key (SOME table) = case S.find table key of NONE => false | _ => true

(** A reference to the next label that we can use which hasn't been used before.
 * Initially set to be a reference to #firstLab but updated by #setNextLab. *)
val nextlab    = ref firstLab

(** Returns the number of a fresh label.
 * Returns the value of #nextlab and increments its value by one. *)
fun freshlab  () =
    let
	val x = !nextlab
    in
	(nextlab := !nextlab + 1; x)
    end

(** An exception raised when we don't find an element in the hash table. *)
exception noneHere

fun empty () = NONE
fun bigEmpty () = NONE

(** Inserts a label into an (empty ()) hash table, returning the table. *)
fun singleton key =
    let
	val table = S.mkTable (Word.fromInt, (op =)) (1, noneHere)
	val _ = S.insert table (key, true)
    in
	SOME table
    end

(** Adds an element to a set given as an argument. *)
fun cons key NONE = singleton key
  | cons key (SOME hashTable) =
    let
	val newHashTable = S.copy hashTable
	val _ = S.insert newHashTable (key,true)
    in
	 SOME newHashTable
    end

fun printHashTable [] = ""
  | printHashTable ((key,value)::h2::t) = "(" ^ (Int.toString key) ^ "," ^ (Bool.toString value) ^ "), " ^ (printHashTable (h2::t))
  | printHashTable ((key,value)::t) = "(" ^ (Int.toString key) ^ "," ^ (Bool.toString value) ^ ")" ^ (printHashTable t)

fun toString NONE = ""
  | toString (SOME hashTable) = printHashTable (S.listItemsi hashTable)


(** Returns the intersection of two sets. *)
fun inter NONE NONE = NONE
  | inter (SOME x) NONE = NONE
  | inter NONE (SOME x) = NONE
  | inter (SOME set1) (SOME set2) =
    let
	val newSet1 = S.copy set1
	val _ = S.filteri (fn (x,y) => S.inDomain set2 x) newSet1
    in
	SOME newSet1
    end

(** Tests if the set in the first argument is a subset of the set given as the second argument. *)
fun subset NONE NONE = true
  | subset NONE x = true
  | subset x NONE = false
  | subset (SOME set1) (SOME set2) = S.foldi (fn (a,b,c) => S.inDomain set2 a andalso c) true set1

(** Tests if the set in the first argument is a strict subset of the set given as the second argument. *)
fun subseteq NONE NONE = false
  | subseteq x NONE = false
  | subseteq NONE x = true
  | subseteq (SOME set1) (SOME set2) = subset (SOME set1) (SOME set2) andalso (S.numItems set1 < S.numItems set2)

(** Tests whether two label hash tables are disjoint. *)
(* fun disjoint set1 set2 = (S.foldi (fn (a,b,c) => not (S.inDomain set2 a)) true (empty ())) *)

(** Tests whether two label hash tables are disjoint. *)
fun disjoint (SOME set1) (SOME set2) =
    not (S.foldi (fn (a,b,c) => S.inDomain set2 a orelse c) false set1)
    (* let *)

    (* 	val result = inter (SOME set1) (SOME set2) *)
    (* in *)
    (* 	(length result) = 0 *)
    (* end *)
    | disjoint _ _ = true

fun toList NONE = []
  | toList (SOME table) =
    let
	fun getKeys [] = []
	  | getKeys ((k,v)::t) = k::(getKeys t)
    in
	getKeys (S.listItemsi table)
    end

(** Removes an alement from a set. *)
fun delete x NONE = NONE
  | delete x (SOME set) =
    let
	val copy = S.copy set
	val _ = (S.remove copy x; ()) handle _ => ()
    in
	SOME copy
    end

(** Tests whether a key is equal to ll where ll has only one element. *)
fun isinone key NONE = false
  | isinone key (SOME ll) = case S.find ll key of NONE => false | _ => (S.numItems ll = 1)

(** Unions sets. *)
fun union NONE NONE = NONE
  | union (SOME x) NONE = SOME (S.copy x)
  | union NONE (SOME x) = SOME (S.copy x)
  | union (SOME hashTable1) (SOME hashTable2) =
    let
	(** Previously we made a new table of initial size max(size hashTable1, size hashTable2), but this is slower than using S.copy apparently. *)
	val newHashTable = S.copy hashTable1
	(* val _ = unionSizes := (((length hashTable1) + (length hashTable2))::(!unionSizes)) *)
	val _ = S.appi (fn (x,y) => S.insert newHashTable (x,y)) hashTable2
    in
	SOME newHashTable
    end

fun destructiveUnion (SOME hashTable) (SOME setToAdd) = S.appi (fn (x,y) => S.insert hashTable (x,y)) setToAdd
  | destructiveUnion _ _ = ()

(** Generalised union.
 * We go through the list to find the largest hashTable, and then use S.copy of that.
 * This has been shown to be more efficient than traversing through, finding the biggest size,
 * making a new hash table of that size then just destructively unioning through the list (this
 * speedup is not large, but enough to justify the additonal complexity here. *)
fun unions list =
    let
	fun locateBiggest counter biggestSoFar indexLocated [] = indexLocated
	  | locateBiggest counter biggestSoFar indexLocated (h::t) =
	    let
		val newLength = length h
	    in
		if newLength > biggestSoFar
		then locateBiggest (counter+1) newLength counter t
		else locateBiggest (counter+1) biggestSoFar indexLocated t
	    end

	val biggest = locateBiggest 0 0 0 list

	val newTable = S.copy (Option.valOf (List.nth (list, biggest)))

	fun unionRemaining counter [] = ()
	  | unionRemaining counter (h::t) =
	    if counter = biggest
	    then unionRemaining (counter+1) t
	    else (destructiveUnion (SOME newTable) h; unionRemaining (counter+1) t)

	val _ = unionRemaining 0 list
    in
	SOME newTable
    end handle Option => NONE

(** Generalised union and cons.
 * We go through the list to find the largest hashTable, and then use S.copy of that.
 * This has been shown to be more efficient than traversing through, finding the biggest size,
 * making a new hash table of that size then just destructively unioning through the list (this
 * speedup is not large, but enough to justify the additonal complexity here. *)
fun unionsCons ordElements list =
    let
	fun locateBiggest counter biggestSoFar indexLocated [] = indexLocated
	  | locateBiggest counter biggestSoFar indexLocated (h::t) =
	    let
		val newLength = length h
	    in
		if newLength > biggestSoFar
		then locateBiggest (counter+1) newLength counter t
		else locateBiggest (counter+1) biggestSoFar indexLocated t
	    end

	val biggest = locateBiggest 0 0 0 list

	val newTable = S.copy (Option.valOf (List.nth (list, biggest)))

	fun unionRemaining counter [] = ()
	  | unionRemaining counter (h::t) =
	    if counter = biggest
	    then unionRemaining (counter+1) t
	    else (destructiveUnion (SOME newTable) h; unionRemaining (counter+1) t)

	val _ = List.map (fn x => S.insert newTable (x,true)) ordElements
	val _ = unionRemaining 0 list
    in
	SOME newTable
    end

(** Tests all tables in 'tables' is a subset of 'table'. *)
fun exsubseteq table [] = true
  | exsubseteq table (h::t) = subset h table andalso (exsubseteq table t)

(** Creates a new hash table given a list of keys. *)
fun ord list =
    let
	val table = S.mkTable (Word.fromInt, (op =)) (List.length list, noneHere)
	val _ = List.map (fn x => S.insert table (x,true)) list
    in
	SOME table
    end

(** Splits a list into two pieces.
 * \returns (lll,llr) s.t. $\forall$ l' in lll, l' < x and $\forall$ l' in llr, l' >= x *)
fun split x NONE = (NONE,NONE)
  | split x (SOME table) =
    let
	val table'  = S.copy table
	val table'' = S.copy table
	val _ = S.filteri (fn (k,v) => if k < x then true else false) table'
	val _ = S.filteri (fn (k,v) => if k >= x then true else false) table''
    in
	(SOME table', SOME table'')
    end

(** Splits the set argument into two by setting those less than the value in 'next' in one half and the rest in the other half. *)
fun split2 first next NONE = (NONE,NONE)
  | split2 first next (SOME table) =
    let
	val table' = S.copy table
	val table'' = S.copy table
	val _ = S.filteri (fn (k,v) => if k >= first andalso k < next then true else false) table'
	val _ = S.filteri (fn (k,v) => if k < first orelse k >= next then true else false) table''
    in
	(SOME table', SOME table'')
    end

(** Splits a table into two pieces, returning two label maps. *)
fun splitIn2 NONE = (NONE, NONE)
  | splitIn2 (SOME table) =
    let
	val table' = S.copy table
	val tableCopy = S.copy table
	val counter = ref 0
	val half  = ceil (Real.fromInt (S.numItems table) / Real.fromInt 2)
	val _ = S.filteri (fn (k,v) => if (!counter < half)
				       then (counter := (!counter+1); true)
				       else (counter := (!counter+1); false))
			  table'
	val _ = counter := 0
	val _ = S.filteri (fn (k,v) => if (!counter >= half)
				       then (counter := (!counter+1); true)
				       else (counter := (!counter+1); false))
			  tableCopy
    in
	(SOME table',SOME tableCopy)
    end

(** Removes a (key,value) pair in the hash table given as an argument.
 * Returns element removed and rest of the hash table as a pair. *)
fun remFirst NONE = (NONE, NONE)
  | remFirst (SOME table) =
    let
	val counter = ref 0
	val label = ref dummyLab
	val table' = S.copy table
	val _ = S.filteri (fn (k,v) => if (!counter = 0)
				      then ((counter := !counter +1);
					    label := k;
					    false)
				      else true) table'
    in
	if !label = dummyLab
	then (NONE, SOME table)
	else (SOME (!label), SOME table')
    end

(** Compares two hash tables using . *)
fun compare (NONE, NONE) = EQUAL
  | compare (x, NONE) = GREATER
  | compare (NONE, x) = LESS
  | compare (SOME table1, SOME table2) =
    let
	val table1Items = S.listItemsi table1
	val table2Items = S.listItemsi table2
	fun compare' [] [] = EQUAL
	  | compare' (h::t) [] = GREATER
	  | compare' [] (h::t) = LESS
	  | compare' ((h,_)::t) ((h',_)::t') = if h = h'
					       then compare' t t'
					       else
						   (if h < h'
						    then LESS
						    else GREATER)
    in
	compare' table1Items table2Items
    end

(** Folds right over the keys in a map. *)
fun foldr f lab NONE = lab
  | foldr f lab (SOME map) =
    let
	val items = S.listItemsi map
	fun foldr' f lab [] = lab
	  | foldr' f lab ((key,value)::t) = f (key, foldr' f lab t)
    in
	foldr' f lab items
    end

(** Returns the set difference (set2 \ set1). *)
fun diff NONE NONE = NONE
  | diff (SOME x) NONE = NONE
  | diff NONE (SOME x) = SOME (S.copy x)
  | diff (SOME set1) (SOME set2) =
    let
	val tableCopy = S.copy set2
	val _ = S.mapi (fn (x,y) => if (S.find tableCopy x <> NONE) then (S.remove tableCopy x; ()) else ()) set1
    in
	SOME tableCopy
    end

end
