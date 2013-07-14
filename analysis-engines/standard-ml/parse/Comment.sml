(* Copyright 2009 2010 2002 Heriot-Watt University
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
 *  o File name:   Comment.sml
 *  o Description: Defines the structure Comment which has signature
 *      COMMENT.  Regions of comments tags are stored in a stack.
 *)

(** Has the signature COMMENT, handles comments found during parsing.
 * Regions of comments tags are stored in a #stack. *)
structure Comment :> COMMENT = struct

structure R  = Reg
structure EH = ErrorHandler

(** A stack to represent opening/closing comments. *)
type stack = R.pos list ref

(**********************)
(* Comments in a file *)
(**********************)

(** A stack to hold the comments which exist in a piece of code. *)
val comm : stack = ref []

(** Adds an opening comment to the #stack. *)
fun ope      p = comm := p :: !comm

(** Takes a comment off the #stack (throws an exception if stack is empty). *)
fun close    _ = case !comm of [] => raise EH.DeadBranch "trying to close a comment which has never been open"
			     | (_ :: ys) => comm := ys

(** Checks whether the comments are balanced.
 * \returns True if the comments are balanced, false otherwise. *)
fun isClosed _ = !comm = []

(** Resets the comment stack. *)
fun reset    _ = comm := []

(** Gets the first comment in the comment stack. *)
fun getTop   _ = case !comm of [] => NONE | (y :: _) => SOME y

(****************************)
(* SML/NJ anti-quote system *)
(****************************)

(** A stack to hold the SML/NJ anti-quotes which exist in a piece of code. *)
val quot : stack = ref []

(** Add an opening anti-quote to the #stack. *)
fun opeQ      p = quot := p :: !quot

(** Add a closing anti-quote to the #stack. *)
fun closeQ    _ = case !quot of [] => raise EH.DeadBranch "trying to close a quote which has never been open"
			      | (_ :: ys) => quot := ys

(** Checks whether the anti-quote stack is balanced.
 * \returns True if the #stack is empty, false otherwise *)
fun isClosedQ _ = !quot = []

(** Resets the anti-quote stack. *)
fun resetQ    _ = quot := []

(** Gets the first anti-quote in the anti-quote stack. *)
fun getTopQ   _ = case !quot of [] => NONE | (y :: _) => SOME y

end
