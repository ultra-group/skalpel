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
datatype debugFeature = EQUALITY_TYPES | CONSTRAINT_PATH | CONSTRAINT_GENERATION | CONSTRAINT_SOLVING | TESTING | PARSING | STATE | PROGRAM_LABELLING | BASIS_LABELLING | TEMP

(* used to generate random colors for brackets of labelled program output *)
val rand = Random.rand (0,6)

(* below are ansi escape sequences, which can be used to colour
 * the output of text in terminals. Note that not all terminals
 * support this feature! *)
val colors= ref {black="\^[[0;30m",red="\^[[0;31m", green="\^[[0;32m", yellow="\^[[0;33m", blue="\^[[0;34m", purple="\^[[0;35m", cyan="\^[[0;36m", white="\^[[0;37m"}
val boldColors= ref {black="\^[[1;30m",red="\^[[1;31m", green="\^[[1;32m", yellow="\^[[1;33m", blue="\^[[1;34m", purple="\^[[1;35m", cyan="\^[[1;36m", white="\^[[1;37m"}
val underlineColors= ref {black="\^[[4;30m",red="\^[[4;31m", green="\^[[4;32m", yellow="\^[[4;33m", blue="\^[[4;34m", purple="\^[[4;35m", cyan="\^[[4;36m", white="\^[[4;37m"}
val backgroundColors= ref {black="\^[[0;40;37m",red="\^[[0;41;37m", green="\^[[0;42;37m", yellow="\^[[0;43;37m", blue="\^[[0;44;37m", purple="\^[[0;45;37m", cyan="\^[[0;46;37m", white="\^[[0;47;37m"}
val leafInBoxColors= ref {black="\^[[4;40;37m",red="\^[[4;41;37m", green="\^[[4;42;37m", yellow="\^[[4;43;37m", blue="\^[[4;44;37m", purple="\^[[4;45;37m", cyan="\^[[4;46;37m", white="\^[[4;47;37m"}
val textReset=ref "\^[[0m"

val oneRunOnly = ref false
val debug = ref false
val debugBasis                : bool ref = ref false
val debugTemp                 : bool ref = ref false
val debugEqualityTypes        : bool ref = ref false
val debugConstraintPath       : bool ref = ref false
val debugConstraintGeneration : bool ref = ref false
val debugConstraintSolving    : bool ref = ref false
val debugTesting              : bool ref = ref false
val debugParsing              : bool ref = ref false
val debugState                : bool ref = ref false
val debugProgramLabelling     : bool ref = ref false
val debugBasisLabelling       : bool ref = ref false

fun printReset str =
    print (str ^ (!textReset))

fun enableDebugFeature EQUALITY_TYPES = debugEqualityTypes := true
  | enableDebugFeature TEMP = debugTemp := true
  | enableDebugFeature CONSTRAINT_PATH = debugConstraintPath := true
  | enableDebugFeature CONSTRAINT_GENERATION = debugConstraintGeneration := true
  | enableDebugFeature CONSTRAINT_SOLVING = debugConstraintSolving := true
  | enableDebugFeature PARSING = debugParsing := true
  | enableDebugFeature TESTING = debugTesting := true
  | enableDebugFeature STATE = debugState     := true
  | enableDebugFeature PROGRAM_LABELLING = debugProgramLabelling     := true
  | enableDebugFeature BASIS_LABELLING   = debugBasisLabelling       := true

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
    if (!debug) andalso (!debugEqualityTypes) then print ("(EQUALITY_TYPES) "^printFilename file^": " ^ (stringFunction ()) ^ !textReset ^ "\n") else ()
  | printDebugFeature file CONSTRAINT_GENERATION stringFunction =
    if (!debug) andalso (!debugConstraintGeneration) then print ("(CONSTRAINT_GENERATION) "^printFilename file^": " ^ (stringFunction()) ^ !textReset ^ "\n") else ()
  | printDebugFeature file TEMP stringFunction =
    if (!debug) andalso (!debugTemp) then print ("(TEMP) "^printFilename file^": " ^ (stringFunction()) ^ !textReset ^ "\n") else ()
  | printDebugFeature file CONSTRAINT_PATH stringFunction =
    if (!debug) andalso (!debugConstraintPath) then print ("(CONSTRAINT_PATH) "^printFilename file^": " ^ (stringFunction()) ^ !textReset ^ "\n") else ()
  | printDebugFeature file CONSTRAINT_SOLVING stringFunction =
    if (!debug) andalso (!debugConstraintSolving) then print ("(CONSTRAINT_SOLVING) "^printFilename file^": " ^ (stringFunction()) ^ !textReset ^ "\n") else ()
  | printDebugFeature file TESTING stringFunction =
    if (!debug) andalso (!debugTesting) then print ("(TESTING) "^printFilename file^": " ^ (stringFunction()) ^ !textReset ^ "\n") else ()
  | printDebugFeature file PARSING stringFunction =
    if (!debug) andalso (!debugParsing) then print ("(PARSING) "^printFilename file^": " ^ (stringFunction()) ^ !textReset ^ "\n") else ()
  | printDebugFeature file PROGRAM_LABELLING stringFunction =
    if (!debug) andalso (!debugProgramLabelling) then
	let
	    val filename = (case OS.Process.getEnv "SKALPEL_LABELLED_PROGRAM" of
			      NONE => (print "Warning: Environment variable SKALPEL_LABELLED_PROGRAM not set! Defaulting to: /tmp/skalpel-labelled-program.tex"; "/tmp/skalpel-labelled-program.tex")
			    | SOME file => file)
	    val outputStream = TextIO.openOut filename
	in
	    (TextIO.output (outputStream, stringFunction());
	     TextIO.closeOut outputStream)
	end
    else ()
  | printDebugFeature file BASIS_LABELLING stringFunction =
    if (!debug) andalso (!debugBasisLabelling) then
	let
	    val filename = (case OS.Process.getEnv "SKALPEL_LABELLED_BASIS" of
			      NONE => (print "Warning: Environment variable SKALPEL_LABELLED_BASIS not set! Defaulting to: /tmp/skalpel-labelled-basis.tex"; "/tmp/skalpel-labelled-basis.tex")
			    | SOME file => file)
	    val outputStream = TextIO.openOut filename
	in
	    (TextIO.output (outputStream, stringFunction());
	     TextIO.closeOut outputStream)
	end
    else ()

  (* prints out the state
   * after the state is printed once, the debugState flag is turned off so we only see the state once
   * if we don't have this it's going to take a longp time to run skalpel *)
  | printDebugFeature file STATE stringFunction =
    if (!debug) andalso (!debugState) then (print ("(STATE) "^printFilename file^": " ^ (stringFunction()) ^ !textReset ^ "\n"); debugState := false)  else ()

fun printLabelledProgramString(x) =
    let
	val texColors = ["black", "red", "green", "blue", "cyan", "magenta", "yellow"]
	val randomNumber = Random.randNat rand
	val colorChosen = List.nth (texColors, (randomNumber mod (List.length texColors)))
    in
	"{\\color{" ^ colorChosen ^ "}\\Bigg[}" ^ x ^ "{\\color{" ^ colorChosen ^ "}\\Bigg]}"
    end

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
