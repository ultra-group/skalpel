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
 *  o File name:   OrdLid.sml
 *  o Description: Defines the structure OrdLid of ordered long
 *      identifiers.
 *)

(** A key that can be used in maps, defining a type of a key and a comparison function.
 * Translucently constrained by the refstruct{ORD_KEY} signature. *)
structure OrdLid : ORD_KEY = struct

(* structure abbreviations *)
structure I = Id

(** Define the key as a labelled idintefier (see #Ident.lid) *)
type ord_key = I.lid

(** \fn compare (lid1,lid2)
 * Comparison function for labelled identifiers.
 * A labelled identifier is deemed to be greater than an
 * identifier. In the case of two constructors of the same datatype we
 * compare the id values inside using #Ident.compare.
 *)
fun compare (I.LID _, I.ID _) = GREATER
  | compare (I.ID _, I.LID _) = LESS
  | compare (I.ID (id1, _), I.ID (id2, _)) =
    I.compare (id1, id2)
  | compare (I.LID ((id1, _), lid1, _),
	     I.LID ((id2, _), lid2, _)) =
    (case I.compare (id1, id2) of
	 EQUAL => compare (lid1, lid2)
       | x => x)

end
