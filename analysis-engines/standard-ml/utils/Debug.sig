(* Copyright 2009 Heriot-Watt University
 * Copyright 2010 Heriot-Watt University
 * Copyright 2011 Heriot-Watt University
 *
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
 *  o File name:   Debug.sig
 *  o Description: Defines the signature DEBUG for printing of debugging
 *      messages.
 *)


signature DEBUG = sig

    datatype debugFiles = JSON | UNIF | LABEL | TY

    val debugUnif   : int ref
    val debugJson   : int ref

    (* sets all debugging values to the integer value *)
    val setAllDebug : int -> unit

    val printDebug   : int -> debugFiles -> string -> unit

    val printdebug1 : string -> unit (* Does not print  *)
    val printdebug2 : string -> unit (* fancy printing  *)

end
