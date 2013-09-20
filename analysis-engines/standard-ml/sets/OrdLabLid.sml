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
 *  o Date:        13 July 2010
 *  o File name:   OrdLabLid.sml
 *  o Description: Defines the structure OrdLabLid of ordered pairs:
 *      label and long identifier.
 *)

(** A key that can be used in maps, defining a type of a key and a comparison function.
 * Translucently constrained by the refstruct{ORD_KEY} signature. *)
structure OrdLabLid : ORD_KEY = struct

(* shorten the names of some structures *)
structure O = OrdLid
structure L = Label
structure I = Id

(** A tuple of a label and a labelled identifier #Id.lid. *)
type ord_key = L.label * I.lid

(** \fn compare (ordkeyValue1,ordkeyValue2)
 * A comparison function which will compare two ord_key values.
 * Uses #Label.compareLab to compare the labels of each key first,
 * then checks the labelled identifiers.
 * \param ord_keyValue1 First key to compare.
 * \param ord_keyValue2 Second key to compare.
 * *)
fun compare ((lab1, lid1), (lab2, lid2)) =
    case L.compareLab (lab1, lab2) of
	EQUAL => O.compare (lid1, lid2)
      | x => x

end
