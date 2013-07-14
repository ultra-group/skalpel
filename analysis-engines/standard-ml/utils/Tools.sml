(* Copyright 2009 Heriot-Watt University
 * Copyright 2010 Heriot-Watt University
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
 * along with SMLTES.  If not, see <http://www.gnu.org/licenses/>.
 *
 *  o Authors:     Vincent Rahli
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        25 May 2010
 *  o File name:   Tools.sml
 *  o Description: Defines the structure Tools which has signature
 *      TOOLS and defines some useful functions to deal with lists.
 *)

(* Has the signature TOOLS, defines some basic operations such as union and set membership. *)
structure Tools :> TOOLS = struct

(** Checks if an argument x is already a member of list xs. *)
fun isin (x : ''a) xs = List.exists (fn y => x = y) xs

(** \fn subseteq list1 list2
 *Checks subsets of lists. *)
fun subseteq []        _  = true
  | subseteq (x :: xs) ys = isin x ys andalso subseteq xs ys

(** \fn disjoint list1 list2
 * Checks two lists are disjoint. *)
fun disjoint []        ys = true
  | disjoint (x :: xs) ys = not (isin x ys) andalso disjoint xs ys

(** Returns intersection of two lists. *)
fun inter xs ys = List.filter (fn x => isin x ys) xs

(** \fn union list1 list2
 * Perfoms the union of two lists. *)
fun union [] ys = ys
  | union (x :: xs) ys = if isin x ys then union xs ys else x :: (union xs ys)

(** \fn delocc list
 * Prints an alert to the user every time it notices duplicate elements in a list. *)
fun delocc [] = []
  | delocc (x :: xs) = if isin x xs then (print " !! OCC !! "; delocc xs) else x :: (delocc xs)

(** \fn remove list1 list2
 * Removes an element from a list. Returns a two-tuple of 1) whether the element was in the list, and 2) the new list. *)
fun remove _ [] = (false, [])
  | remove x (y :: xs) = if (x : ''a) = y then (true, xs) else (fn (u, v) => (u, y :: v)) (remove x xs)

(** Prints a list. *)
fun printlist xs f = "[" ^ #1 (foldr (fn (t, (s, c)) => (f t ^ c ^ s, ",")) ("", "") xs) ^ "]"

end
