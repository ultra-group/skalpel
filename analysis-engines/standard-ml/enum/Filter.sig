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
 *  o File name:   Filter.sig
 *  o Description: Defines the FILTER signature.
 *)


signature FILTER = sig

    type filter  = Label.labels option
    type filters = {keep : filter, (*Labels that we want to keep            *)
		    bind : filter} (*Labels that we want to keep as bindings*)
    (* These are the 3 states a label can have in a computation *)
    datatype state = IN   (*If a label is part of a computation                                *)
		   | OUT  (*If a label is not part of a computation but its bindings are needed*)
		   | BIND (*If a label is not part of a computation at all                     *)

    (* In all these functions the first filter is a filter in
     * and the second filter is a filter out, meaning:
     * - a filter in is the label set which are allowed (if NONE then all are allowed)
     * - a filter out is the label set which are not allowed (if NONE then all are allowed) *)

    (* Creates Filters *)
    val cons          : filter -> filter -> filters

    (* Adds a label to the keep part of a filter *)
    val addLab        : filters -> Label.label -> filters

    (* Tests if the label is in the first filter (or NONE) and
     * not in the second filter (or NONE). *)
    val testtodo      : filters -> Label.label -> bool

    (* Tests the state of a label in a computation.  Its state is
     * entirely defined by the filters. *)
    val getStateLab   : filters -> Label.label  -> state
    val getStateLabs  : filters -> Label.labels -> state

    (* Tests if all the labels are in the first filter (or NONE) and
     * not in the second filter (or NONE). *)
    val testtodos     : filters -> Label.labels -> bool

    (* Tests if at least one label is in the first filter (or NONE) and
     * not in the second filter (or NONE). *)
    val testonetodos  : filters -> Label.labels -> bool

    (* Filters the label set. *)
    val filtertodos   : filters -> Label.labels -> Label.labels

    val toString      : filters -> string

end
