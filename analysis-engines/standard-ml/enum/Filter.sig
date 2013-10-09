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
 *  o File name:   Filter.sig
 *)

(** Represents filters (label sets), used by refstruct{Filter} *)
signature FILTER = sig

    type filter  = (Label.label, bool) Label.labels option
    type filters = {keep : filter,
		    bind : filter}

    datatype state = IN
		   | OUT
		   | BIND

    val cons          : filter -> filter -> filters

    val addLab        : filters -> Label.label -> filters

    val testtodo      : filters -> Label.label -> bool

    val getStateLab   : filters -> Label.label  -> state
    val getStateLabs  : filters -> (Label.label, bool) Label.labels -> state

    val testtodos     : filters -> (Label.label, bool) Label.labels -> bool

    val testonetodos  : filters -> (Label.label, bool) Label.labels -> bool

    val filtertodos   : filters -> (Label.label, bool) Label.labels -> (Label.label, bool) Label.labels

    val toString      : filters -> string

end
