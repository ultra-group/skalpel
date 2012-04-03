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


(*SKALPEL-USE Region.sig*)
(*SKALPEL-SPEC structure Reg : REG*)

signature COMMENT = sig

    (*? This is a basic structure which contains two stacks, one for
       comments and the other for quotations. The usual functions for
       use with stacks are involved here: ope (push), close (pop),
       isClosed (is stack empty?), reset (clear stack), getTop (peak). ?*)

    (* The below are for comment backets *)
    val ope      : Reg.pos -> unit
    val close    : unit    -> unit
    val isClosed : unit    -> bool
    val reset    : unit    -> unit
    val getTop   : unit    -> Reg.pos option

    (* The below are for quotations *)
    val opeQ      : Reg.pos -> unit
    val closeQ    : unit    -> unit
    val isClosedQ : unit    -> bool
    val resetQ    : unit    -> unit
    val getTopQ   : unit    -> Reg.pos option

end
