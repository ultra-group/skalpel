(* Copyright 2010 Heriot-Watt University
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
 *  o Authors:     Vincent Rahli, John Pirie
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        05 July 2010
 *  o File name:   SeachSpace.sml
 *  o Description: Contains the SearchSpace structure for the
 *      searchspace of the enumeration algorithm.
 *)



(*(2010-04-22)searchSpace is going to be the type of the unification searchspace.
 * It looks like that for example:
 * [   1,      4,      5,      7,      8]
 *    / \     / \             /
 *  [2,  4] [5,  9]         [8]
 *       |
 *     [10]
 *
 * So that for example, [1,4,10] is a filter to test.
 *)
(*datatype searchSpace = SPACE of searchSpace LM.map*)

(* ================================================ *)
(* == First SearchSpace structure based on lists == *)
(* ================================================ *)

structure SearchSpace :> SEARCHSPACE = struct

structure L = Label

structure LabSet = struct type ord_key = L.labels val compare = L.compare end
structure SL = BinarySetFn(LabSet) (* Set Labels *)

(* space   is the actual search space and
 * success is the filters for which the solver succeeded. *)
type searchSpace = {space : L.labels list, success : SL.set}

(* Success is the set of filters for which we haven't found any error,
 * meaning that for each filter in success, the filtering of the constraint
 * set using the filter is solvable. *)


(* Empty searchspace *)
val emSpace = []
val emSuccess = SL.empty
val emSearchSpace = {space = emSpace, success = emSuccess}

(* Get the size of a searchspace *)
fun getSizeSearchSpace {space, success} =
    (length space, SL.numItems success)

(* Transform the size of a searchspace *)
fun toStringSizeSearchSpace searchspace =
    let val (space, success) = getSizeSearchSpace searchspace
    in "{space = " ^ Int.toString space ^ ", success = " ^ Int.toString success ^ "}"
    end

(* Prints the size of a searchspace *)
fun printSizeSearchSpace searchspace =
    print (toStringSizeSearchSpace searchspace ^ "\n")

(* Transform a set of labels into a searchSpace where each filter is a singleton *)
fun flatLabs labs =
    L.foldr (fn (lab, {space, success}) =>
		{space = (L.singleton lab) :: space, success = success})
	    emSearchSpace
	    labs

(* Returns one filter from the searchspace *)
fun getOneFilter {space = [], success} = NONE
  | getOneFilter {space = labs :: searchspace, success} =
    SOME (labs, {space = searchspace, success = success})

(* Adds one filter to a searchspace *)
fun addToSearchSpace labs {space, success} =
    {space   = labs :: (List.filter (fn labs' => not (L.subset labs labs')) space),
     success = success}

(* Adds one filter to the end of a searchspace *)
fun addToSearchSpaceEnd labs {space, success} =
    {space   = (List.filter (fn labs' => not (L.subset labs labs')) space) @ [labs],
     success = success}

(* Test whether or not a filter is already in a searchspace*)
fun isInSearchSpace labels {space, success} =
    L.exsubseteq labels space
    orelse
    SL.exists (fn labs => L.subseteq labs labels) success

(* Adds new filters to the searchspace, build from the first argument (filter)
 * and the second one (error found). *)
fun buildFilters filter error searchspace b =
    ((*printSizeSearchSpace searchspace;*)
     (* Uncomment the above line to print the size of a searchspace while slicing *)
     L.foldr (fn (lab, searchspace') =>
		 let val labels = L.cons lab filter
		 in if isInSearchSpace labels searchspace
		    then searchspace'
		    else if b
		    then addToSearchSpace labels searchspace'
		    else addToSearchSpaceEnd labels searchspace'
		 end) searchspace error)

fun getSuccess {space, success} = SL.listItems success

fun addSuccess labels {space, success} =
    {space = space, success = SL.add (success, labels)}

fun addSuccess' labels {space, success} =
    {space   = List.filter (fn labs => not (L.subseteq labels labs)) space,
     success = SL.add (success, labels)}

end



(* ================================================ *)
(* == Second SearchSpace structure based on sets == *)
(* ================================================ *)

(* The problem with this structure is that it is not the order of the database. *)
structure SearchSpace' :> SEARCHSPACE = struct

structure L = Label

fun invComp EQUAL   = EQUAL
  | invComp LESS    = GREATER
  | invComp GREATER = LESS

structure LabSet = struct type ord_key = L.labels val compare = invComp o L.compare end
structure SL = BinarySetFn(LabSet) (* Set Labels *)

type searchSpace = {space : SL.set, success : SL.set}


(* Empty searchspace *)
val emSpace = SL.empty
val emSuccess = SL.empty
val emSearchSpace = {space = emSpace, success = emSuccess}


(* Transform a set of labels into a searchSpace where each filter is a singleton *)
fun flatLabs labs =
    L.foldr (fn (lab, {space, success}) =>
		{space   = SL.add (space, L.singleton lab),
		 success = success})
	    emSearchSpace
	    labs

(* Returns one filter from the searchspace *)
fun getOneFilter {space, success} =
    (case SL.find (fn _ => true) space of
	 NONE => NONE
       | SOME labs => SOME (labs, {space = SL.delete (space, labs), success = success}))
    handle LibBase.NotFound => NONE

(* Adds one filter to a searchspace *)
fun addToSearchSpace labels {space, success} =
    {space = SL.add (space, labels), success = success}

(* Test whether or not a filter is already in a searchspace*)
fun isInSearchSpace labels {space, success} =
    SL.exists (fn labs => L.subseteq labs labels) space
    orelse
    SL.exists (fn labs => L.subseteq labs labels) success

(* Adds new filters to the searchspace, build from the first argument (filter)
 * and the second one (error found). *)
fun buildFilters filter error searchspace b =
    L.foldr (fn (lab, searchspace') =>
		let val labels = L.cons lab filter
		in if isInSearchSpace labels searchspace
		   then searchspace'
		   else addToSearchSpace labels searchspace'
		end) searchspace error

fun getSuccess {space, success} = SL.listItems success

fun addSuccess labels {space, success} =
    {space = space, success = SL.add (success, labels)}

end



(* ======================================================== *)
(* == Third SearchSpace structure using a red black tree == *)
(* ======================================================== *)

structure SearchSpaceRbs :> SEARCHSPACE = struct

structure L = Label

structure LabSet = struct type ord_key = L.labels val compare = L.compare end
structure SL = BinarySetFn(LabSet) (* Set Labels *)

structure rbs = RedBlackSetFn(LabSet)

(* space   is the actual search space and
 * success is the filters for which the solver succeeded. *)
type searchSpace = {space : rbs.set, success : SL.set}

(* Success is the set of filters for which we haven't found any error,
 * meaning that for each filter in success, the filtering of the constraint
 * set using the filter is solvable. *)

(* Empty searchspace *)
val emSpace = rbs.empty
val emSuccess = SL.empty
val emSearchSpace = {space = emSpace, success = emSuccess}

(* Get the size of a searchspace *)
fun getSizeSearchSpace {space, success} =
    (rbs.numItems space, SL.numItems success)

(* Transform the size of a searchspace *)
fun toStringSizeSearchSpace searchspace =
    let val (space, success) = getSizeSearchSpace searchspace
    in "{space = " ^ Int.toString space ^ ", success = " ^ Int.toString success ^ "}"
    end

(* Prints the size of a searchspace *)
fun printSizeSearchSpace searchspace =
    print (toStringSizeSearchSpace searchspace ^ "\n")

(* Transform a set of labels into a searchSpace where each filter is a singleton *)

fun flatLabs labs =
    L.foldr (fn (lab, {space, success}) =>
		{space   = rbs.add (space, L.singleton lab),  success = success})
	    emSearchSpace
	    labs

(* Returns one filter from the searchspace *)
fun getOneFilter {space, success} =
    (case rbs.find (fn _ => true) space of
         NONE => NONE
       | SOME labs => SOME (labs, {space = rbs.delete (space, labs), success = success}))
    handle LibBase.NotFound => NONE


(* Adds one filter to a searchspace *)
fun addToSearchSpace labs {space, success} =
    {space   = rbs.add (space, labs),
     success = success}

(* Adds one filter to the end of a searchspace *)
fun addToSearchSpaceEnd labs {space, success} =
    {space   = rbs.add (space, labs),
     success = success}


(* Test whether or not a filter is already in a searchspace*)
fun isInSearchSpace labels {space, success} =
    rbs.member (space, labels)
    orelse
    SL.exists (fn labs => L.subseteq labs labels) success


(* Adds new filters to the searchspace, build from the first argument (filter)
 * and the second one (error found). *)
fun buildFilters filter error searchspace b =
    ((*printSizeSearchSpace searchspace;*)
     (* Uncomment the above line to print the size of a searchspace while slicing *)
     L.foldr (fn (lab, searchspace') =>
		 let val labels = L.cons lab filter
		 in if isInSearchSpace labels searchspace
		    then searchspace'
		    else if b
		    then addToSearchSpace labels searchspace'
		    else addToSearchSpaceEnd labels searchspace'
		 end) searchspace error)


fun getSuccess {space, success} = SL.listItems success

fun addSuccess labels {space, success} =
    {space = space, success = SL.add (success, labels)}

end
