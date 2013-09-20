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
 *  o File name:   OrdId.sml
 *  o Description: Defines the structure OrdId of ordered identifiers.
 *)

(** A key that can be used in maps, defining a type of a key and a comparison function.
 * Translucently constrained by the refstruct{ORD_KEY} signature. *)
structure OrdId : ORD_KEY =
struct

(** Declares this new type to be the same as #Id.id. *)
type ord_key = Id.id

(** Comparison function is set to the same as #Id.compare. *)
val compare = Id.compare
end
