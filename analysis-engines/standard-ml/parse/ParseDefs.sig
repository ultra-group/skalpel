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
 *  o File name:   ParseDefs.sig
 *)

(** The signature PARSEDEFS, used by the structure refstruct{ParseDefs}. *)
signature PARSEDEFS = sig

    exception ParseError of string * Reg.region list

    type errorHandler = bool * string * Reg.region list

    val mkErrorHandler    : bool -> string -> Reg.region list -> errorHandler

    val setErrorHandler   : errorHandler -> unit
    val getErrorHandler   : unit -> errorHandler
    val resetErrorHandler : unit -> unit

end
