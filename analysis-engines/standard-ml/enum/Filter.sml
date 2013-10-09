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
 *  o Date:        24 May 2010
 *  o File name:   Filter.sml
 *)

(** Contains the Filter structure which has signature FILTER (opaque constraint).
 * This is the structure to deal with filters used to to filter constraints during enumeration. *)
structure Filter :> FILTER = struct

structure L  = Label
structure LM = SplayMapFn(OrdKey)

(** A filter is defined as an option of #Label.labels. *)
type filter    = (L.label, bool) L.labels option

(** Filters is a record, we have keep, and bind, each of type #filter.
 * \arg keep: the labels we wish to keep.
 * \arg bind: the labels we wish to keep as bindings. *)
type filters   = {keep : filter, bind : filter}

(** Represents which state the label has in computation.
 * States can be one of these three forms:
 * \arg IN If a label is part of a computation.
 * \arg OUT If a label is not part of a computation but its bindings are needed
 * \arg BIND If a label is not part of a computation at all
*)
datatype state = IN | OUT | BIND




(* In all these functions the first filter is a filter in
* and the second filter is a filter out, meaning:
* - a filter in is the label set which are allowed (if NONE then all are allowed)
* - a filter out is the label set which are not allowed (if NONE then all are allowed) *)




(** Creates filters from two labels sets for keep and bind. *)
fun cons keep bind = {keep = keep, bind = bind}

(** Adds a label to the keep part of a filter. *)
fun addLab {keep, bind} lab =
    cons (Option.map (fn labels => L.cons lab labels) keep) bind

(** Tests if the label is in the first filter (or NONE) and not in the second filter (or NONE). *)
fun testtodo {keep, bind} l =
    (
     (L.isin l (Option.valOf keep) handle Option => true)
     andalso
     (not (L.isin l (Option.valOf bind)) handle Option => true)
    ) orelse (L.eq l L.dummyLab)

(** If the label in the second argument is in the set given in the first argument, return IN, otherwise return OUT.
 * If we see OUT then that means that this label should be filtered out. *)
fun getStateKeep (SOME labs) lab =
    if L.isin lab labs
    then IN
    else OUT
  | getStateKeep NONE _ = IN

(** If the label in the second argument is in the bind set of the filters given as the first argument, return BIND, otherwise check the keep set with #getStateKeep. *)
fun getStateBind (filters as {keep, bind}) lab =
    case bind of
	SOME labs =>
	if L.isin lab labs
	then BIND
	else getStateKeep keep lab
      | NONE => getStateKeep keep lab

(** Tests the state of a label in a computation, its state is entirely defined by the filters. *)
fun getStateLab filters lab =
    if L.eq lab L.dummyLab
    then IN
    else getStateBind filters lab

(** Returns the state of the labels in the 'labs argument' (either IN, OUT or BIND).
 * \returns a #state constructor. *)
fun getStateLabs filters labs =
    L.foldr (fn (lab, BIND) => BIND
	      | (lab, OUT) =>
		if getStateLab filters lab = BIND
		then BIND
		else OUT
	      | (lab, IN) => getStateLab filters lab)
	    IN
	    labs

(** True if all the ll are not filtered out (in projlab out of filter). *)
fun testtodos filters ll =
    L.foldr (fn (l, b) => b andalso testtodo filters l) true ll

(** rue if at least one in ll is not filtered out (in projlab out of filter).
 * This is used in filteridenv to test if at least one branch of a function definition
 * is still in the environment *)
fun testonetodos filters ll =
    L.foldr (fn (l, b) => b orelse testtodo filters l) false ll

(** Filters the label set. *)
fun filtertodos filters ll =
    L.foldr (fn (x, xs) => if testtodo filters x then L.cons x xs else xs)
	    (L.empty ())
	    ll

(** Converts a #filter to a string. *)
fun printFilter NONE        = "NONE"
  | printFilter (SOME labs) = "SOME(" ^ L.toString labs ^ ")"

(** Converts #filters to a string. *)
fun toString {keep, bind} =
    "{keep = " ^ printFilter keep ^
    ",bind = " ^ printFilter bind ^ "}"

end
