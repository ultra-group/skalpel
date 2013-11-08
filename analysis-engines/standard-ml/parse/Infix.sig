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
 *  o File name:   Infix.sig
 *)

(** Defines the signature INFIX to deal with infix operators. *)
signature INFIX = sig

    type 'a pack = 'a * Reg.region (* Shouldn't that be a region list? *)
    type packstr = string pack
    datatype 'a tree = L of 'a pack
		     | O of packstr
		     | N of packstr * 'a tree * 'a tree

    val isInfix    : string -> bool

    val convert    : 'a tree list -> 'a tree

    val getLeft    : 'a tree -> Reg.region
    val getRight   : 'a tree -> Reg.region

    val newScope   : Label.label -> unit
    val rmScope    : Label.label -> unit

    val addInfixL  : string -> int -> unit
    val addInfixR  : string -> int -> unit
    val rmInfix    : string -> unit

    val reset      : unit -> unit

end
