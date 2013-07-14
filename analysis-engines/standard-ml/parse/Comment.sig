(* Copyright 2002 2009 2010 2012 Heriot-Watt University
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
 *  o Authors:     Vincent Rahli, Christian Haack
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        25 May 2010
 *  o File name:   Comment.sig
 *  o Description: Defines the siganture COMMENT for opening and
 *                 closing of comments and quotations.
 *)

(** The signature COMMENT, used by the structure refstruct{Comment}. *)
signature COMMENT = sig

    val ope      : Reg.pos -> unit
    val close    : unit    -> unit
    val isClosed : unit    -> bool
    val reset    : unit    -> unit
    val getTop   : unit    -> Reg.pos option

    val opeQ      : Reg.pos -> unit
    val closeQ    : unit    -> unit
    val isClosedQ : unit    -> bool
    val resetQ    : unit    -> unit
    val getTopQ   : unit    -> Reg.pos option

end
