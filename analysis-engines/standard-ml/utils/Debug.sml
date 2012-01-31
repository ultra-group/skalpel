(* Copyright 2009 2010 2011 2012 Heriot-Watt University
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

datatype debugFiles = JSON | UNIF | LABEL | TY | MLGRM | AZE | RUN | ENV | TEST

(* the greater the depth, the more detail the print statements give *)
val debugUnif  : int ref = ref 0
val debugJson  : int ref = ref 0
val debugLabel : int ref = ref 0
val debugTy    : int ref = ref 0
val debugGrm   : int ref = ref 0
val debugAze   : int ref = ref 0
val debugRun   : int ref = ref 0
val debugEnv   : int ref = ref 0
val debugTest  : int ref = ref 0

fun setAllDebug value =
    (debugUnif := value;
     debugJson := value;
     debugLabel:= value;
     debugTy   := value;
     debugGrm  := value;
     debugAze  := value;
     debugEnv  := value;
     debugTest  := value;
     debugRun  := value)

(* prints a debug statement, if the depth is <= what debug level is set then we print the string *)
fun printDebug depth JSON str =
    if (!debugJson >= depth) then print ("("^(Int.toString depth)^") JsonParser.sml: " ^ str ^ "\n") else ()
  | printDebug depth UNIF str =
    if (!debugUnif >= depth) then print ("("^(Int.toString depth)^") Unification.sml: " ^ str ^ "\n") else ()
  | printDebug depth LABEL str =
    if (!debugLabel >= depth) then print ("("^(Int.toString depth)^") Label.sml: " ^ str ^ "\n") else ()
  | printDebug depth TY str =
    if (!debugTy >= depth) then print ("("^(Int.toString depth)^") Ty.sml: " ^ str ^ "\n") else ()
  | printDebug depth MLGRM str =
    if (!debugGrm >= depth) then print ("("^(Int.toString depth)^") Ml.grm: " ^ str ^ "\n") else ()
  | printDebug depth AZE str =
    if (!debugAze >= depth) then print ("("^(Int.toString depth)^") Analyze.sml: " ^ str ^ "\n") else ()
  | printDebug depth RUN str =
    if (!debugRun >= depth) then print ("("^(Int.toString depth)^") RunSlicer.sml: " ^ str ^ "\n") else ()
  | printDebug depth ENV str =
    if (!debugEnv >= depth) then print ("("^(Int.toString depth)^") Env.sml: " ^ str ^ "\n") else ()
  | printDebug depth TEST str =
    if (!debugTest >= depth) then print ("("^(Int.toString depth)^") Tester.sml: " ^ str ^ "\n") else ()

(* the same as print debug but executes the function instead of displaying a string *)
(* this is so that we don't have to compute toString functions when we don't have to *)
fun printDebugFunc depth JSON func =
    if (!debugJson >= depth) then print ("("^(Int.toString depth)^") JsonParser.sml: " ^ (func ()) ^ "\n") else ()
  | printDebugFunc depth UNIF func =
    if (!debugUnif >= depth) then print ("("^(Int.toString depth)^") Unification.sml: " ^ (func ()) ^ "\n") else ()
  | printDebugFunc depth LABEL func =
    if (!debugLabel >= depth) then print ("("^(Int.toString depth)^") Label.sml: " ^ (func ()) ^ "\n") else ()
  | printDebugFunc depth TY func =
    if (!debugTy >= depth) then print ("("^(Int.toString depth)^") Ty.sml: " ^ (func ()) ^ "\n") else ()
  | printDebugFunc depth MLGRM func =
    if (!debugGrm >= depth) then print ("("^(Int.toString depth)^") Ml.grm: " ^ (func ()) ^ "\n") else ()
  | printDebugFunc depth AZE func =
    if (!debugAze >= depth) then print ("("^(Int.toString depth)^") Analyze.sml: " ^ (func ()) ^ "\n") else ()
  | printDebugFunc depth RUN func =
    if (!debugRun >= depth) then print ("("^(Int.toString depth)^") RunSlicer.sml: " ^ (func ()) ^ "\n") else ()
  | printDebugFunc depth ENV func =
    if (!debugEnv >= depth) then print ("("^(Int.toString depth)^") Env.sml: " ^ (func ()) ^ "\n") else ()
  | printDebugFunc depth TEST func =
    if (!debugTest >= depth) then print ("("^(Int.toString depth)^") Tester.sml: " ^ (func ()) ^ "\n") else ()


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
