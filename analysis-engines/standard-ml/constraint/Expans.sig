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
 *  o Date:        25 May 2010
 *  o File name:   Expans.sig
 *  o Description: Defines the signature EXPANS which is the signature
 *      to deal with expansive/non-expansive expressions.
 *)


signature EXPANS = sig

    datatype expans = Expexp of Label.labels   (* is expansive without context dependency *)
		    | Expdep of Id.lid       * (* identifier of the variable that makes the expression expansive *)
				Label.labels   (* other labels involved in the expansiveness                     *)
    (* expansive if elt is a value variable *)

    datatype nonexp = Nonexp                (* is not expansive *)
		    | Expans of expans list

    val getLabsExpans      : expans       -> Label.labels * Id.lid option
    val addnonexp          : nonexp       -> Label.label -> nonexp
    val genOneExpdep       : Id.id        -> Label.label -> nonexp
    val genOneExpans       : Label.label  -> nonexp
    val composeNonexp      : nonexp list  -> nonexp
    val genMulExpans       : Label.label list -> nonexp

    (*val getSetStatusNonexp : nonexp       -> Label.labels*)

    (*(* returns the expans if
     * - it is an Expdep such that the id is not in the set and force (second labels) is not empty
     * - or the sigs are non empty
     * - or it is an Expexp *)
    val restrictExpans     : expans -> Id.set -> expans option
    val restrictNonexp     : nonexp -> Id.set -> nonexp*)

    val printnonexp        : nonexp        -> string
    val printexpans        : expans        -> string
end
