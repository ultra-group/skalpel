(* Copyright 2009 Heriot-Watt University
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
 *  o Authors:     Vincent Rahli
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        25 May 2010
 *  o File name:   Solution.sml
 *  o Description: Defines the structure Solution which has signature
 *      SOLUTION and deals with the different verions of our slicer.
 *)


structure Solution :> SOLUTION = struct

(* 9 possible solutions to date *)
datatype sol = SOL1 | SOL2 | SOL3 | SOL4
	     | SOL5 | SOL6 | SOL7 | SOL8
	     | SOL9

(* the current solution *)
val sol = ref SOL9

(* get/set functions for the solution *)
fun setSol s  = sol := s
fun getSol () = !sol

(* converts SOLn to its integer equivalent *)
fun toInt SOL1 = 1
  | toInt SOL2 = 2
  | toInt SOL3 = 3
  | toInt SOL4 = 4
  | toInt SOL5 = 5
  | toInt SOL6 = 6
  | toInt SOL7 = 7
  | toInt SOL8 = 8
  | toInt SOL9 = 9

(* converts integer to SOLn value *)
fun fromInt 1 = SOL1
  | fromInt 2 = SOL2
  | fromInt 3 = SOL3
  | fromInt 4 = SOL4
  | fromInt 5 = SOL5
  | fromInt 6 = SOL6
  | fromInt 7 = SOL7
  | fromInt 8 = SOL8
  | fromInt 9 = SOL9
  | fromInt n =
    let val nst  = Int.toString n
	val csol = getSol ()
	val ssol = Int.toString (toInt csol)
	val st   =  nst ^ " is not an existing solution.\n" ^
		    "The defauls solution is " ^ ssol ^ ".\n"
	val _    = print st
    in csol end

(* checks if sol1 is less than sol2 *)
fun smaller sol1 sol2 = toInt sol1 <= toInt sol2

(* checks current solution is newer or same than sol *)
fun isAtLeast sol = smaller sol (getSol ())

fun toString () = "solution " ^ Int.toString (toInt (getSol ()))

end
