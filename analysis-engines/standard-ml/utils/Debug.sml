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
 *  o File name:   Debug.sml
 *  o Description: Defines the structure Debug which has signature DEBUG
 *      and which is used to print debugging messages.
 *)


structure Debug :> DEBUG = struct

(* separator definitions *)
val sep1  = "-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-"
val sep2  = "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-"
val sep1' = "\n" ^ sep1 ^ "\n"
val sep2' = "\n" ^ sep2 ^ "\n"

(* boolean values to control debug functions *)
val debugbool1 = false
val debugbool2 = true
val debugbool3 = true
(*val debugbool4 = false*)

(* debug functions which print text to the screen *)
fun printdebug1 st = if debugbool1 then print st else ()
fun printdebug2 st =
    if debugbool2
    then print (sep1' ^ st ^ sep2' ^ "\n")
    else ()
fun printdebug3 st = if debugbool3 then print st else ()
(*fun printdebug4 st = if debugbool4 then print st else ()*)

end
