(* Copyright 2009 2010 2012 Heriot-Watt University
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
 *  o File name:   OrdIdl.sml
 *  o Description: Defines the structure OrdIdl of ordered identifiers
 *      tagged with a label.
 *)

(** A key that can be used in maps, defining a type of a key and a comparison function.
 * Translucently constrained by the refstruct{ORD_KEY} signature. *)
structure OrdIdl : ORD_KEY = struct

(* shorten names of structures *)
structure I = Id
structure L = Label

(** Set the new key type to be a labelled identifier (#Id.labelledId) *)
type ord_key = I.labelledId

(** \fn compare (ordkeyValue1,ordkeyValue2)
 * A comparison function which will compare two ord_key values.
 * Uses #Id.compare to compare the id of each key first,
 * then checks the labels
 * \param ord_keyValue1 First key to compare.
 * \param ord_keyValue2 Second key to compare.
 * *)
fun compare ((id1, lab1), (id2, lab2)) =
    case I.compare (id1, id2) of
	EQUAL => L.compareLab (lab1, lab2)
      | x => x

end
