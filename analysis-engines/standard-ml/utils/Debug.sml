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

datatype debugFiles = JSON | UNIF | LABEL | TY | MLGRM | AZE | RUN | ENV | TEST | PARSER
datatype debugFeature = EQUALITY_TYPES | CONSTRAINT_GENERATION | CONSTRAINT_SOLVING | TESTING | PARSING | STATE

(* below are ansi escape sequences, which can be used to colour
 * the output of text in terminals. Note that not all terminals
 * support this feature! *)
val colors={black="\^[[0;30m",red="\^[[0;31m", green="\^[[0;32m", yellow="\^[[0;33m", blue="\^[[0;34m", purple="\^[[0;35m", cyan="\^[[0;36m", white="\^[[0;37m"}
val boldColors={black="\^[[1;30m",red="\^[[1;31m", green="\^[[1;32m", yellow="\^[[1;33m", blue="\^[[1;34m", purple="\^[[1;35m", cyan="\^[[1;36m", white="\^[[1;37m"}
val underlineColors={black="\^[[4;30m",red="\^[[4;31m", green="\^[[4;32m", yellow="\^[[4;33m", blue="\^[[4;34m", purple="\^[[4;35m", cyan="\^[[4;36m", white="\^[[4;37m"}
val backgroundColors={black="\^[[40m",red="\^[[41m", green="\^[[42m", yellow="\^[[43m", blue="\^[[44m", purple="\^[[45m", cyan="\^[[46m", white="\^[[47m"}
val textReset="\^[[0m"

(* the greater the depth, the more detail the print statements give *)
val debugUnif    : int ref = ref 0
val debugParser  : int ref = ref 0
val debugJson    : int ref = ref 0
val debugLabel   : int ref = ref 0
val debugTy      : int ref = ref 0
val debugGrm     : int ref = ref 0
val debugAze     : int ref = ref 0
val debugRun     : int ref = ref 0
val debugEnv     : int ref = ref 0
val debugTest    : int ref = ref 0

val oneRunOnly = ref false
val debug = ref false
val debugEqualityTypes        : bool ref = ref false
val debugConstraintGeneration : bool ref = ref false
val debugConstraintSolving    : bool ref = ref false
val debugTesting              : bool ref = ref false
val debugParsing              : bool ref = ref false
val debugState                : bool ref = ref false

fun enableDebugFeature EQUALITY_TYPES = debugEqualityTypes := true
  | enableDebugFeature CONSTRAINT_GENERATION = debugConstraintGeneration := true
  | enableDebugFeature CONSTRAINT_SOLVING = debugConstraintSolving := true
  | enableDebugFeature PARSING = debugParsing := true
  | enableDebugFeature TESTING = debugTesting := true
  | enableDebugFeature STATE = debugState     := true

fun setAllDebug value =
    (debugUnif   := value;
     debugParser := value;
     debugJson   := value;
     debugLabel  := value;
     debugTy     := value;
     debugGrm    := value;
     debugAze    := value;
     debugEnv    := value;
     debugTest   := value;
     debugRun    := value)

fun printFilename JSON   = "JsonParser.sml"
  | printFilename UNIF   = "Unification.sml"
  | printFilename LABEL  = "Label.sml"
  | printFilename TY     = "Ty.sml"
  | printFilename MLGRM  = "ML.grm"
  | printFilename AZE    = "Analyze.sml"
  | printFilename RUN    = "Run.sml"
  | printFilename ENV    = "Env.sml"
  | printFilename TEST   = "Tester.sml"
  | printFilename PARSER = "Parser.sml"

(* this s hould later become printDebug and the old printDebug should go away *)
fun printDebugFeature file EQUALITY_TYPES stringFunction =
    if (!debug) andalso (!debugEqualityTypes) then print ("(EQUALITY_TYPES) "^printFilename file^": " ^ (stringFunction ()) ^ textReset ^ "\n") else ()
  | printDebugFeature file CONSTRAINT_GENERATION stringFunction =
    if (!debug) andalso (!debugConstraintGeneration) then print ("(CONSTRAINT_GENERATION) "^printFilename file^": " ^ (stringFunction()) ^ textReset ^ "\n") else ()
  | printDebugFeature file CONSTRAINT_SOLVING stringFunction =
    if (!debug) andalso (!debugConstraintSolving) then print ("(CONSTRAINT_SOLVING) "^printFilename file^": " ^ (stringFunction()) ^ textReset ^ "\n") else ()
  | printDebugFeature file TESTING stringFunction =
    if (!debug) andalso (!debugTesting) then print ("(TESTING) "^printFilename file^": " ^ (stringFunction()) ^ textReset ^ "\n") else ()
  | printDebugFeature file PARSING stringFunction =
    if (!debug) andalso (!debugParsing) then print ("(PARSING) "^printFilename file^": " ^ (stringFunction()) ^ textReset ^ "\n") else ()

  (* prints out the state
   * after the state is printed once, the debugState flag is turned off so we only see the state once
   * if we don't have this it's going to take a longp time to run skalpel *)
  | printDebugFeature file STATE stringFunction =
    if (!debug) andalso (!debugState) then (print ("(STATE) "^printFilename file^": " ^ (stringFunction()) ^ textReset ^ "\n"); debugState := false)  else ()

(* prints a debug statement, if the depth is <= what debug level is set then we print the string *)
fun printDebug depth JSON str =
    if (!debugJson >= depth) then print ("("^(Int.toString depth)^") JsonParser.sml: " ^ str ^ "\n") else ()
  | printDebug depth UNIF str =
    if (!debugUnif >= depth) then print ("("^(Int.toString depth)^") Unification.sml: " ^ str ^ "\n") else ()
  | printDebug depth PARSER str =
    if (!debugParser >= depth) then print ("("^(Int.toString depth)^") Parser.sml: " ^ str ^ "\n") else ()
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
  | printDebugFunc depth PARSER func =
    if (!debugParser >= depth) then print ("("^(Int.toString depth)^") Parser.sml: " ^ (func()) ^ "\n") else ()
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

fun checkOneRunOnly () =
    if (!oneRunOnly)
    then debug := false
    else ()

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
