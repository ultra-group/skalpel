(* Copyright 2010 Heriot-Watt University
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
 *  o Date:        21 July 2010
 *  o File name:   ParseDefs.sml
 *)

(** Handles errors that can occur during parsing, constrained be refstruct{PARSEDEFS}. *)
structure ParseDefs :> PARSEDEFS = struct

structure R = Reg

(** An "exception raised when we encounter a parse error. *)
exception ParseError of string * Reg.region list

(** Used to hold whether a handle statement has been triggered from infpat in ML.grm.
 * If it has, then we want the parser to throw the error instead.  In
 * the event that it doesn't, this flag will indicate that there is
 * actually an error somewhere in the code, so that we don't just
 * generate a bad abstract syntax tree. We have generalised that to
 * more of our tailored error messages in our parser.  In general we
 * prefer relying on the errors thrown by ml-yacc. *)
type errorHandler = bool * string * R.region list

(** An initial value for the error handler. *)
val initErrorHandler = (false, "", [])

(** A reference value for #initErrorHandler. *)
val errorHandle : errorHandler ref = ref initErrorHandler

(** Creates an error handler from a boolean, a string and a region. *)
fun mkErrorHandler bool message regions = (bool, message, regions)

(** Modifier method for #errorHandle. *)
fun setErrorHandler value = errorHandle := value

(** Accessor method for #errorHandle. *)
fun getErrorHandler () = !errorHandle

(** Resets the error handler to the initial error handler. *)
fun resetErrorHandler () = setErrorHandler initErrorHandler

end
