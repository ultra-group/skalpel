(* Copyright 2002 2009 2010 Heriot-Watt University
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
 *  o File name:   LexDefs.sml
 *)

(** Defines the structure LexDefs which has the signature refstruct{LEXDEFS} to handle lexer errors. *)
structure LexDefs :> LEXDEFS = struct

(* shorten the names of structures *)
structure C = Comment
structure D = Debug
structure R = Reg

(** An exception for when a lexical error occurs. *)
exception LexError        of string * string * R.region list

(** We raise this exception when we encounter an unknown character. *)
exception BadCharacter    of string * R.region list

(** Raised when a comment is unclosed during parsing. *)
exception UnclosedComment of string * R.region list

(** Raised when a string is unclosed during parsing. *)
exception UnclosedString  of string * R.region list

(** Raised when we see a closing comment when one has not been opened. *)
exception ClosedComment   of string * R.region list

(** A boolean that can be set to true when encounting the SML/NJ antiquote system. *)
val quotation = ref false

(** Modifier function for the #quotation value. *)
fun setQuotation bool = quotation := bool

(** Accessor function for the #quotation value. *)
fun getQuotation ()   = !quotation

(** Raises an exception with the msg, states the file and the regions associated with the error. *)
fun error (msg, file, regs) =
    raise LexError (file ^ ":" ^ msg, msg, regs)

(** String used in an argument to raise #BadCharacter. *)
val badcharStr = "ignoring bad character"

(** String used in an argument to raise #UnclosedComment. *)
val uclcommStr = "unclosed comment at end of file"

(** String used in an argument to raise #UnclosedString. *)
val uclstrgStr = "unclosed string at end of file"

(** String used in an argument to raise #ClosedComment. *)
val unmatchStr = "unmatched closing comment"

(** Handles errors raised parsing.
 * We handle the following errors:
 * \arg a bad chraracter
 * \arg an unclosed comment in a file
 * \arg an unclosed string in a file
 * \arg an unmatched close comment (empty comment stack)
 * We do not catch ParseError here. *)
fun handleLex f x =
    (C.reset (); f x)
    handle
    BadCharacter    (file, regs) => error (badcharStr, file, regs)
  | UnclosedComment (file, regs) => error (uclcommStr, file, regs)
  | UnclosedString  (file, regs) => error (uclstrgStr, file, regs)
  | ClosedComment   (file, regs) => error (unmatchStr, file, regs)

end
