(* Copyright 2010 Heriot-Watt University
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
 *  o Authors:     Vincent Rahli
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        21 July 2010
 *  o File name:   ParseDefs.sml
 *  o Description: Defines the signature ParseDefs used to handle
 *      some parsing errors.
 *)


structure ParseDefs :> PARSEDEFS = struct

structure R = Reg

exception ParseError of string * Reg.region list

(* used to hold whether a handle statement has been triggered from infpat
 * in ML.grm. If it has, then we want the parser to throw the error instead.
 * In the event that it doesn't, this flag will indicate that there is actually
 * an error somewhere in the code, so that we don't just generate a bad abstract
 * syntax tree
 * We have generalised that to more of our tailored error messages in our parser.
 * In general we prefer relying on the errors thrown by ml-yacc. *)
type errorHandler = bool * string * R.region list

val initErrorHandler = (false, "", [])

val errorHandle : errorHandler ref = ref initErrorHandler

fun mkErrorHandler bool message regions = (bool, message, regions)

(* set and get functions *)
fun setErrorHandler value = errorHandle := value
fun getErrorHandler () = !errorHandle
fun resetErrorHandler () = setErrorHandler initErrorHandler


end
