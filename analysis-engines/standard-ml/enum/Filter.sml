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
 *  o File name:   Filter.sml
 *  o Description: Contains the Filter structure which has signature
 *      FILTER.  This is the structure to deal with filters used to
 *      to filter constraints during enumeration.
 *)



structure Filter :> FILTER = struct

structure L  = Label
structure LM = SplayMapFn(OrdKey)

type filter    = L.labels option
type filters   = {keep : filter, bind : filter}
datatype state = IN | OUT | BIND

fun cons keep bind = {keep = keep, bind = bind}

fun addLab {keep, bind} lab =
    cons (Option.map (fn labels => L.cons lab labels) keep) bind

fun testtodo {keep, bind} l =
    (
     (L.isin l (Option.valOf keep) handle Option => true)
     andalso
     (not (L.isin l (Option.valOf bind)) handle Option => true)
    ) orelse (L.eq l L.dummyLab)

fun getStateKeep (SOME labs) lab =
    if L.isin lab labs
    then IN
    else OUT
  | getStateKeep NONE _ = IN

fun getStateBind (filters as {keep, bind}) lab =
    case bind of
	SOME labs =>
	if L.isin lab labs
	then BIND
	else getStateKeep keep lab
      | NONE => getStateKeep keep lab

fun getStateLab filters lab =
    if L.eq lab L.dummyLab
    then IN
    else getStateBind filters lab

fun getStateLabs filters labs =
    L.foldr (fn (lab, BIND) => BIND
	      | (lab, OUT) =>
		if getStateLab filters lab = BIND
		then BIND
		else OUT
	      | (lab, IN) => getStateLab filters lab)
	    IN
	    labs

(* true if all the ll are not filtered out (in projlab out of filter) *)
fun testtodos filters ll =
    L.foldr (fn (l, b) => b andalso testtodo filters l) true ll

(* true if at least one in ll is not filtered out (in projlab out of filter) *)
(* this is used in filteridenv to test if at least one branch of a function definition
 * is still in the environment *)
fun testonetodos filters ll =
    L.foldr (fn (l, b) => b orelse testtodo filters l) false ll

fun filtertodos filters ll =
    L.foldr (fn (x, xs) => if testtodo filters x then L.cons x xs else xs)
	    L.empty
	    ll


(* The previous filtertodos seems to be faster than this one *)
(*fun filtertodos projlab filter ll =
    let val labs1 = case projlab of
			SOME labs => L.inter labs ll
		      | NONE => ll
	val labs2 = case filter of
			SOME labs => L.difference labs labs1
		      | NONE => labs1
    in labs2
    end*)


(*(*Gets the first filter in the search space.*)
 fun getFirstFilter searchspace =*)


(* printing functions for filters *)
fun printFilter NONE        = "NONE"
  | printFilter (SOME labs) = "SOME(" ^ L.toString labs ^ ")"

fun toString {keep, bind} =
    "{keep = " ^ printFilter keep ^
    ",bind = " ^ printFilter bind ^ "}"

end
