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
 * along with Skalpeln.  If not, see <http://www.gnu.org/licenses/>.
 *
 *  o Authors:     Vincent Rahli
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        25 May 2010
 *  o File name:   OrdKey.sml
 *  o Description: Defines the structure OrdKey for ordered intergers.
 *      OrdKey's signature has to be translucent
 *)

(** A key that can be used in maps, defining a type of a key and a comparison function.
 * Translucently constrained by the refstruct{ORD_KEY} signature. *)
structure OrdKey : ORD_KEY =
struct

(** Define the key to be an integer. *)
type ord_key = int

(** Use the Int.compare function to compare keys. *)
val compare = Int.compare

end
