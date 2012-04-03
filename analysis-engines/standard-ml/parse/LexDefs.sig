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
 *  o Description: Defines the signature LEXDEFS used to handle lexer
 *      errors.
 *)


(*SKALPEL-USE-FILE Region.sig*)
(*SKALPEL-SPEC structure Reg : REG*)

signature LEXDEFS = sig

    (* delclaration of exceptions *)
    exception LexError        of string * string * Reg.region list
    exception BadCharacter    of string * Reg.region list
    exception UnclosedComment of string * Reg.region list
    exception UnclosedString  of string * Reg.region list
    exception ClosedComment   of string * Reg.region list

    (* set and get methods for SML/NJ antiquote system *)
    val setQuotation : bool -> unit
    val getQuotation : unit -> bool

    (* a function which will handle any errors that orrur
     * in function application *)
    val handleLex : ('a -> 'b) -> 'a -> 'b
(* If 'f' is a function that uses the lexer generated from 'MiniML.lex'
 * (for a example a parser) and 'x' is some input for 'f', then 'f'
 * should be applied to 'x' using  the function 'handleLex', i.e.,
 * 'handleLex f x'. 'handleLex' initializes the lexer state before
 * applying 'f' to 'x' and handles exceptions that are raised by the
 * lexer.
 *)

end
