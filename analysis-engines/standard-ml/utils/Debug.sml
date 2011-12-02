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
 *  o Authors:     Vincent Rahli, John Pirie
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        25 May 2010
 *  o File name:   Debug.sml
 *  o Description: Defines the structure Debug which has signature DEBUG
 *      and which is used to print debugging messages.
 *)


structure Debug :> DEBUG = struct

datatype debugFiles = JSON | UNIF | LABEL | TY

(* the greater the depth, the more detail the print statements give *)
val debugUnif  : int ref = ref 0
val debugJson  : int ref = ref 0
val debugLabel : int ref = ref 0
val debugTy    : int ref = ref 0

fun setAllDebug value =
    (debugUnif := value;
     debugJson := value;
     debugLabel:= value;
     debugTy   := value)

fun printDebug depth JSON str =
    if (!debugJson >= depth) then print ("("^(Int.toString depth)^") JsonParser.sml: " ^ str ^ "\n") else ()
  | printDebug depth UNIF str =
    if (!debugUnif >= depth) then print ("("^(Int.toString depth)^") Unification.sml: " ^ str ^ "\n") else ()
  | printDebug depth LABEL str =
    if (!debugUnif >= depth) then print ("("^(Int.toString depth)^") Label.sml: " ^ str ^ "\n") else ()
  | printDebug depth TY str =
    if (!debugUnif >= depth) then print ("("^(Int.toString depth)^") Ty.sml: " ^ str ^ "\n") else ()

(* separator definitions *)
val sep1  = "-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-"
val sep2  = "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-"
val sep1' = "\n" ^ sep1 ^ "\n"
val sep2' = "\n" ^ sep2 ^ "\n"

(* boolean values to control debug functions *)
val debugbool1 = false
val debugbool2 = true

(* debug functions which print text to the screen *)
fun printdebug1 st = if debugbool1 then print st else ()
fun printdebug2 st =
    if debugbool2
    then print (sep1' ^ st ^ sep2' ^ "\n")
    else ()

end
