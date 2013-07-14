(* Copyright 2010 Heriot-Watt University
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
 *  o Date:        29 June 2010
 *  o File name:   OrdStr.sml
 *)

(** Has the signature ORD_KEY, defines a structure of ordered strings.
 * Regions of comments tags are stored in a #stack. *)
structure OrdStr : ORD_KEY = struct

(** The key, a string *)
type ord_key = string

(** A function to compare #ord_key values.
 * Uses String.compare. *)
val compare  = String.compare

end
