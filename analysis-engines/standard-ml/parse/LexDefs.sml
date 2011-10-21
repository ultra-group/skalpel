(* Copyright 2002 Heriot-Watt University
 * Copyright 2009 Heriot-Watt University
 * Copyright 2010 Heriot-Watt University
 *
 *
 * This file is part of the ULTRA SML Type Error Slicer (SMLTES) -
 * a Type Error Slicer for Standard ML written by the ULTRA Group of
 * Heriot-Watt University, Edinburgh.
 *
 * SMLTES is a free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * SMLTES is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with SMLTES.  If not, see <http://www.gnu.org/licenses/>.
 *
 *  o Authors:     Vincent Rahli, Christian Haack
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        25 May 2010
 *  o File name:   LexDefs.sml
 *  o Description: Defines the structure LexDefs which has signature
 *      LEXDEFS, to handle lexer errors.
 *)


structure LexDefs :> LEXDEFS = struct

(* shorten the names of structures *)
structure C = Comment
structure D = Debug
structure R = Reg

(* declare exceptions *)
exception LexError        of string * string * R.region list
exception BadCharacter    of string * R.region list
exception UnclosedComment of string * R.region list
exception UnclosedString  of string * R.region list
exception ClosedComment   of string * R.region list

(* used for SML/NJ antiquote system, modifier and acccessor methods *)
val quotation = ref false
fun setQuotation bool = quotation := bool
fun getQuotation ()   = !quotation

(* raises an exception with the msg, states the file and the regions
 * associated with the error *)

fun error (msg, file, regs) =
    raise LexError (file ^ ":" ^ msg, msg, regs)

(* various messeges detailing a problem in the code *)
val badcharStr = "ignoring bad character"
val uclcommStr = "unclosed comment at end of file"
val uclstrgStr = "unclosed string at end of file"
val unmatchStr = "unmatched closing comment"

(* handlers for:
 * - a bad chraracter
 * - an unclosed comment in a file
 * - an unclosed string in a file
 * - an unmatched close comment (empty comment stack) *)
fun handleLex f x =
    (C.reset (); f x)
    handle
    BadCharacter    (file, regs) => error (badcharStr, file, regs)
  | UnclosedComment (file, regs) => error (uclcommStr, file, regs)
  | UnclosedString  (file, regs) => error (uclstrgStr, file, regs)
  | ClosedComment   (file, regs) => error (unmatchStr, file, regs)

(* we don't catch ParseError *)

end
