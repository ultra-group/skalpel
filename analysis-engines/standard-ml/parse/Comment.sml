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
 *  o File name:   Comment.sml
 *  o Description: Defines the structure Comment which has signature
 *      COMMENT.  Regions of comments tags are stored in a stack.
 *)


structure Comment :> COMMENT = struct

structure R  = Reg
structure EH = ErrorHandler

type stack = R.pos list ref

(**********************)
(* Comments in a file *)
(**********************)

(* a stack to hold the comments in a piece of code *)
val comm : stack = ref []

(* adds an opening comment to the stack *)
fun ope      p = comm := p :: !comm

(* takes a comment off the stack (throws an exception if stack is empty *)
fun close    _ = case !comm of [] => raise EH.DeadBranch "trying to close a comment which has never been open"
			     | (_ :: ys) => comm := ys

(* returns true if the comments are balanced, false otherwise *)
fun isClosed _ = !comm = []

(* resets the count of the comments *)
fun reset    _ = comm := []

(* gets the first comment in the list *)
fun getTop   _ = case !comm of [] => NONE | (y :: _) => SOME y

(****************************)
(* SML/NJ anti-quote system *)
(****************************)

(* initialise new stack *)
val quot : stack = ref []

(* add an open quote to the stack *)
fun opeQ      p = quot := p :: !quot

(* add a close quote to the stack *)
fun closeQ    _ = case !quot of [] => raise EH.DeadBranch "trying to close a quote which has never been open"
			      | (_ :: ys) => quot := ys

(* returns true if the stack is empty, false otherwise *)
fun isClosedQ _ = !quot = []

(* resets the count of the quotes *)
fun resetQ    _ = quot := []

(* gets the top quote from the list *)
fun getTopQ   _ = case !quot of [] => NONE | (y :: _) => SOME y

end
