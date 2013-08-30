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
 *)

(** A structure constrained opaquely by the signature refstruct{DEBUG}, used to aid debugging of Skalpel. *)
structure Debug :> DEBUG = struct

(** Represents the file that debugging is taking place in.
 * Constructors include:
 * \arg \b JSON. For debugging in JsonParser.sml.
 * \arg \b UNIF. For debugging in Unification.sml.
 * \arg \b LABEL. For debugging in Label.sml.
 * \arg \b TY. For debugging in Ty.sml.
 * \arg \b MLGRM. For debugging in ML.grm.
 * \arg \b AZE. For debugging in Analyze.sml.
 * \arg \b RUN. For debugging in RunSlicer.sml
 * \arg \b ENV. For debugging in Env.sml
 * \arg \b TEST. For debugging in Tester.sml
 * \arg \b PARSER. For debugging in Parser.sml
 * \arg \b BLANK. For when a filename is not to be printed. *)
datatype debugFiles = JSON | UNIF | LABEL | TY | MLGRM | AZE | RUN | ENV | TEST | PARSER | BLANK

(** Represents the file that debugging is taking place in.
 * Constructors include:
 * \arg \b EQUALITY_TYPES. For debug statements aiding in the
 *  development of the equality types feature of the standard ml
 *  language.
 * \arg \b CONSTRAINT_PATH. Shows the path taken through the constraint generator.
 * \arg \b CONSTRAINT_GENERATION. Prints out constraint generation information.
 * \arg \b CONSTRAINT_SOLVING. For debugging a constraint solving feature.
 * \arg \b TESTING. Used when debugging the test framework.
 * \arg \b PARSING. For debugging parser.
 * \arg \b STATE. Debugging information for the state (unifiers).
 * \arg \b PROGRAM_LABLELLING. Outputs a labelled program in latex format.
 * \arg \b BASIS_LABELLING. Gives a labelled basis output in latex to
 *  file specified in environment variable $SKALPEL_LABELLED_BASIS.
 * \arg \b TEMP. For debugging of temporary values. Debug statements
 *  with these values should \b not exist for long. *)
datatype debugFeature = EQUALITY_TYPES | CONSTRAINT_PATH | CONSTRAINT_GENERATION | CONSTRAINT_SOLVING | TESTING | PARSING | STATE | PROGRAM_LABELLING | BASIS_LABELLING | TEMP

(* used to generate random colors for brackets of labelled program output *)
(* val rand = Random.rand (0,6) *)

(** Declares a record of ansi colour codes (black,red,green,yellow,blue,purple,cyan,white).
 * Used in the debug framework to allow different strings to be represented in different colours.
 * Note that not all terminals support this feature. *)
val colors= ref {black="\^[[0;30m",red="\^[[0;31m", green="\^[[0;32m", yellow="\^[[0;33m", blue="\^[[0;34m", purple="\^[[0;35m", cyan="\^[[0;36m", white="\^[[0;37m"}

(** Declares a record of ansi bold colour codes (black,red,green,yellow,blue,purple,cyan,white).
 * Used in the debug framework to allow different strings to be represented in different colours.
 * Note that not all terminals support this feature. *)
val boldColors= ref {black="\^[[1;30m",red="\^[[1;31m", green="\^[[1;32m", yellow="\^[[1;33m", blue="\^[[1;34m", purple="\^[[1;35m", cyan="\^[[1;36m", white="\^[[1;37m"}

(** Declares a record of ansi underlined colour codes (black,red,green,yellow,blue,purple,cyan,white).
 * Used in the debug framework to allow different strings to be represented in different colours.
 * Note that not all terminals support this feature. *)
val underlineColors= ref {black="\^[[4;30m",red="\^[[4;31m", green="\^[[4;32m", yellow="\^[[4;33m", blue="\^[[4;34m", purple="\^[[4;35m", cyan="\^[[4;36m", white="\^[[4;37m"}

(** Declares a record of ansi background colour codes (black,red,green,yellow,blue,purple,cyan,white).
 * Used in the debug framework to allow different strings to be represented in different colours.
 * Note that not all terminals support this feature. *)
val backgroundColors= ref {black="\^[[0;40;37m",red="\^[[0;41;37m", green="\^[[0;42;37m", yellow="\^[[0;43;37m", blue="\^[[0;44;37m", purple="\^[[0;45;37m", cyan="\^[[0;46;37m", white="\^[[0;47;37m"}

(** Declares a ansi colour code sequences used with when displaying a error #LEAF (black,red,green,yellow,blue,purple,cyan,white).
 * See ExtReg.sml for LEAF constructor definition.
 * Note that not all terminals support this feature. *)
val leafInBoxColors= ref {black="\^[[4;40;37m",red="\^[[4;41;37m", green="\^[[4;42;37m", yellow="\^[[4;43;37m", blue="\^[[4;44;37m", purple="\^[[4;45;37m", cyan="\^[[4;46;37m", white="\^[[4;47;37m"}

(** Ansi colour reset code. *)
val textReset=ref "\^[[0m"

(** A boolean, when set to true debugging only happens on the first unification pass. *)
val oneRunOnly = ref false

(** A boolean enabled when any debug feature is used. *)
val debug = ref false

(** A boolean enabled to view basis debug statements. *)
val debugBasis                : bool ref = ref false

(** A boolean enabled to view temp debug statements. *)
val debugTemp                 : bool ref = ref false

(** A boolean enabled to view equality type debug statements. *)
val debugEqualityTypes        : bool ref = ref false

(** A boolean enabled to view constraint path debug statements. *)
val debugConstraintPath       : bool ref = ref false

(** A boolean enabled to view constraint generation debug statements. *)
val debugConstraintGeneration : bool ref = ref false

(** A boolean enabled to view constraint solving debug statements. *)
val debugConstraintSolving    : bool ref = ref false

(** A boolean enabled to view test framework debug statements. *)
val debugTesting              : bool ref = ref false

(** A boolean enabled to view parsing debug statements. *)
val debugParsing              : bool ref = ref false

(** A boolean enabled to view state (unifiers) debug statements. *)
val debugState                : bool ref = ref false

(** A boolean enabled to view program labelling statements. *)
val debugProgramLabelling     : bool ref = ref false

(** A boolean enabled to do basis program labelling. *)
val debugBasisLabelling       : bool ref = ref false


(** Appends #textReset to the string given as an argument *)
fun printReset str =
    print (str ^ (!textReset))

(** Given a #debugFeature constructor as an argument, will set its associated boolean flag to true. *)
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

(** Given a constructor of #debugFiles, prints the filename it stands to represent. *)
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
  | printFilename BLANK = ""

(** Prints out a debug statement.
 * \param file. A #debugFiles constructor.
 * \param debugFeature. The feature currently being debugged (a debugFeature constructor).
 * \param str. A function which given any argument returns the string
 *  to be printed. It is important that this dummy function exists
 *  otherwise string concatenation is completed whether or not the
 *  debug statement is enabled, leading to a performance hit on the
 *  core.
 *  Note that when printing the state we only do it once, otherwise it would take terribly long to debug anything. *)
fun printDebug file EQUALITY_TYPES stringFunction =
    if (!debug) andalso (!debugEqualityTypes) then print ("(EQUALITY_TYPES) "^printFilename file^": " ^ (stringFunction ()) ^ !textReset ^ "\n") else ()
  | printDebug file CONSTRAINT_GENERATION stringFunction =
    if (!debug) andalso (!debugConstraintGeneration) then print ("(CONSTRAINT_GENERATION) "^printFilename file^": " ^ (stringFunction()) ^ !textReset ^ "\n") else ()
  | printDebug file TEMP stringFunction =
    if (!debug) andalso (!debugTemp) then print ("(TEMP) "^printFilename file^": " ^ (stringFunction()) ^ !textReset ^ "\n") else ()
  | printDebug file CONSTRAINT_PATH stringFunction =
    if (!debug) andalso (!debugConstraintPath) then print ("(CONSTRAINT_PATH) "^printFilename file^": " ^ (stringFunction()) ^ !textReset ^ "\n") else ()
  | printDebug file CONSTRAINT_SOLVING stringFunction =
    if (!debug) andalso (!debugConstraintSolving) then print ("(CONSTRAINT_SOLVING) "^printFilename file^": " ^ (stringFunction()) ^ !textReset ^ "\n") else ()
  | printDebug file TESTING stringFunction =
    if (!debug) andalso (!debugTesting) then print ("(TESTING) "^printFilename file^": " ^ (stringFunction()) ^ !textReset ^ "\n") else ()
  | printDebug file PARSING stringFunction =
    if (!debug) andalso (!debugParsing) then print ("(PARSING) "^printFilename file^": " ^ (stringFunction()) ^ !textReset ^ "\n") else ()
  | printDebug file PROGRAM_LABELLING stringFunction =
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
  | printDebug file BASIS_LABELLING stringFunction =
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
  | printDebug file STATE stringFunction =
    if (!debug) andalso (!debugState) then (print ("(STATE) "^printFilename file^": " ^ (stringFunction()) ^ !textReset ^ "\n"); debugState := false)  else ()

(** Prints a labelled program string in a random color. *)
fun printLabelledProgramString(x) =
    let
	val texColors = ["black", "red", "green", "blue", "cyan", "magenta", "yellow"]
	val randomNumber = 4 (*Random.randNat rand*)
	val colorChosen = List.nth (texColors, (randomNumber mod (List.length texColors)))
    in
	"{\\color{" ^ colorChosen ^ "}\\Bigg[}" ^ x ^ "{\\color{" ^ colorChosen ^ "}\\Bigg]}"
    end

(** A function which sets debugging to false if we are only running once. *)
fun checkOneRunOnly () =
    if (!oneRunOnly)
    then debug := false
    else ()

(** A string that can be used during debugging to separate items for clarity. *)
val sep1  = "-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-"
(** A string that can be used during debugging to separate items for clarity. *)
val sep2  = "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-"
(** Appends new lines before and after #sep1. *)
val sep1' = "\n" ^ sep1 ^ "\n"
(** Appends new lines before and after #sep2. *)
val sep2' = "\n" ^ sep2 ^ "\n"

(** A deprecated boolean value to control debug functions. *)
val debugbool1 = false
(** A deprecated boolean value to control debug functions. *)
val debugbool2 = true

(** A deprecated debug printing functions which prints its argument if #debugbool1 is enabled.
 * \deprecated *)
fun printdebug1 st = if debugbool1 then print st else ()
(** A deprecated debug printing functions which prints its argument with separator strings if #debugbool2 is enabled.
 * \deprecated *)
fun printdebug2 st =
    if debugbool2
    then print (sep1' ^ st ^ sep2' ^ "\n")
    else ()

end
