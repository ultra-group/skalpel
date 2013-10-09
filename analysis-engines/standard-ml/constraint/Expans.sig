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
 *  o File name:   Expans.sig
 *)

(** Defines the signature EXPANS which is the signature to deal with expansive/non-expansive expressions. *)
signature EXPANS = sig

    datatype expans = Expexp of (Label.label, bool) Label.labels
		    | Expdep of Id.lid * (Label.label, bool) Label.labels

    datatype nonexp = Nonexp
		    | Expans of expans list

    val getLabsExpans      : expans       -> (Label.label, bool) Label.labels * Id.lid option
    val addnonexp          : nonexp       -> Label.label -> nonexp
    val genOneExpdep       : Id.id        -> Label.label -> nonexp
    val genOneExpans       : Label.label  -> nonexp
    val composeNonexp      : nonexp list  -> nonexp
    val genMulExpans       : Label.label list -> nonexp

    val printnonexp        : nonexp        -> string
    val printexpans        : expans        -> string
end
