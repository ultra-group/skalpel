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
 *  o File name:   LexDefs.sig
 *)


(*SKALPEL-USE-FILE Region.sig*)
(*SKALPEL-SPEC structure Reg : REG*)

(** The signature LEXDEFS, used by the structure refstruct{LexDefs}. *)
signature LEXDEFS = sig

    exception LexError        of string * string * Reg.region list
    exception BadCharacter    of string * Reg.region list
    exception UnclosedComment of string * Reg.region list
    exception UnclosedString  of string * Reg.region list
    exception ClosedComment   of string * Reg.region list

    val setQuotation : bool -> unit
    val getQuotation : unit -> bool

    val handleLex : ('a -> 'b) -> 'a -> 'b

end
