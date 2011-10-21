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
 *  o File name:   Solution.sig
 *  o Description: Defines the signature SOLUTION for the different
 *      versions of our slicer.
 *)


signature SOLUTION = sig

    datatype sol = SOL1 | SOL2 | SOL3 | SOL4
		 | SOL5 | SOL6 | SOL7 | SOL8
		 | SOL9
    (* SOL1: old constraints
     * SOL2: new constraints
     * SOL3: SOL2
     *     o new enumeration,
     *     o minimization2
     * SOL4: SOL3
     *     o minimization4
     * SOL5: SOL4
     *     o lazy minimization4
     * SOL6: SOL5
     *     o new compmono in Analyze,
     *     o new enumeration (includes miniminisation)
     * SOL7: SOL6
     *     o new reduce algo in minimisation
     * SOL8: SOL6
     *     o new bind constraints to handle open *)

    val setSol    : sol  -> unit
    val getSol    : unit -> sol
    val fromInt   : int  -> sol
    val toInt     : sol  -> int

    (* Checks if the current sol is at least as new as the argument *)
    val isAtLeast : sol -> bool

    (* Checks if the first solution is smaller or equal to the second one *)
    val smaller   : sol  -> sol -> bool

    (* Prints a solution *)
    val toString : unit -> string

end
