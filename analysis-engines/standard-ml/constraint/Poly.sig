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
 *  o Date:        25 May 2010
 *  o File name:   Poly.sig
 *)

(** A signature used by the refstruct{POLY} structure. *)
signature POLY = sig

    datatype mono = EXPANS of Expans.expans
		  | MONBIN of (Label.label, bool) Label.labels
    datatype poly = POLY
		  | MONO of mono list

    val getLabsMono       : mono -> (Label.label, bool) Label.labels * (Label.label, bool) Label.labels * LongId.set
    val getLabsPoly       : poly -> (Label.label, bool) Label.labels * (Label.label, bool) Label.labels * LongId.set
    val fromPolyToNonexp  : poly -> Expans.nonexp
    val fromNonexpToPoly  : Expans.nonexp -> poly
    val mergePoly         : poly -> poly -> poly
    val polyToMono        : poly -> (Label.label, bool) Label.labels -> poly
    val toPoly            : poly -> poly
    val isPoly            : poly -> bool
    val isMono            : poly -> bool
    val toString          : poly -> string

end
