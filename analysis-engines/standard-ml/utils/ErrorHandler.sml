(* Copyright 2009 2010 2012 Heriot-Watt University
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
 *  o File name:   ErrorHandler.sml
 *  o Description: Defines the structure ErrorHandler which has signature
 *      ERRORHANDLER, to deal with exceptions.
 *      Note that the SML/NJ structure LibBase does the job.
 *)

(** Has the signature ERROR_HANDLER, handles errors occurring when running Skalpel. *)
structure ErrorHandler :> ERROR_HANDLER =
struct

(** Raised when a problem is encountered during execution of Skalpel. *)
exception DeadBranch of string

(** Used when we reach code for features we do not yet handle. *)
exception TODO of string

(** Prints the given string argument, then returns it. *)
fun msg st = (print st; st)

end
