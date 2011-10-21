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
 *  o File name:   ErrorHandler.sml
 *  o Description: Defines the structure ErrorHandler which has signature
 *      ERROR_HANDLER, to deal with exceptions.
 *      Note that the SML/NJ structure LibBase does the job.
 *)


structure ErrorHandler :> ERROR_HANDLER = struct

(* for general kinds of problems, eg something which should be
   >= 0 is actually -1 *)
exception DeadBranch of string

(* for exceptions which are thrown as something isn't implemented yet *)
exception TODO

fun msg st = (print st; st)

(* raises the DeadBranch exception with the given string *)
fun throw st = raise DeadBranch st

end
