(* Copyright 2009-2017 Heriot-Watt University
 * Copyright 2018 Christian Gregg
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
 *  o Authors:     Vincent Rahli, John Pirie, Christian Gregg
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        24 May 2010
 *  o File name:   RunSlicer.sml
 *  o Description: Contains the functions that serve as interface with
 *      the slicer.  The file defines the structure Slicer which has
 *      the signature Slicer.
 *)

(** \mainpage Documentation for the Skalpel Analysis Engine
 *
 * \section doc_sec Building this documentation
 *
 * In order to build this documentation, please follow these steps:
 *
 * 1 - Clone the doxygen repository to your system.
 *
 * 2 - Checkout a compatible version of the doxygen repository (if master fails to build)
 *
 * $ git checkout 929ea15c46c55562862181f59ae2c6b00c046dc0
 *
 * 3 - Apply the patch for SML support:
 *
 * $ git am < /path/to/skalpel/repo/documentation/doxygen-sml.patch
 *
 * 4 - Configure doxygen (specifying path to python2 - assuming /usr/bin/python2)
 *
 * $ ./configure --prefix `pwd` --python /usr/bin/python2
 *
 * 5 - Build doxygen:
 *
 * $ make
 *
 * 6 - Generate documentation (assuming directory is Standard ML analysis egnine repo directory).
 *
 * $ make documentation
 *
 * HTML pages will be in html/, man pages in man/, latex files in latex/
 *
 * \section update_sec Keeping this documentation updated
 *
 * To generate a new patch, commit your updated changes to the doxygen git repository and then execute (where SHA is most recent SHA tag):
 *
 * $ git format-patch -1 SHA
 *
 * Pleasae try to keep this documentation updated for the sanity of future Skalpel developers. ;-)
 *
 *)

structure Slicer : SLICER = struct

(* shorten the names of structures for use in the code *)
structure ER = Error
structure EH = ErrorHandler
structure D  = Debug
structure PP = Ppp

(** Set to true if we are building a binary for the webdemo. *)
val webdemo = ref false

(** Type of reported errors, set to JsonParser.error. *)
type error = JsonParser.error

(** Set to be the same as the error as defined in Tester (#Tester.error). *)
val error : error = Tester.error
(** Set to be the same as the myfilehtml as defined in Tester (#Tester.myfilehtml). *)
val myfilehtml     = Tester.myfilehtml

(** Datatype for determining whether the user wishes slices displayed in a terminal, has three constructors.
 * \arg NO_DISPLAY is for disabling slices shown in the terminal.
 * \arg NON_INTERACTIVE is to put all the slices in the termanal as soon as we find them.
 * \arg INTERACTIVE is not used yet. It perhaps should be added, so the user can cycle through slices. *)
datatype terminalSliceDisplay = NO_DISPLAY | NON_INTERACTIVE | INTERACTIVE

(** Setting for showing slices in the terminal; we set the default setting to NO_DISPLAY. *)
val terminalSlices : terminalSliceDisplay ref = ref NO_DISPLAY

(** A value which should not be manually edited, the git hash of the repository is automatically inserted here during compilation. *)
val SKALPEL_VERSION = "Built with Poly/ML on Sun 14 Jan 12:59:42 GMT 2018. Skalpel version: 41483cf059a7365a088650ee998930e6f124a8f5"

(** Takes a boolean value b, if true then we are generating a binary for the web demo. *)
fun setWebDemo b = webdemo := b

(** Enhance these messages with the messages from EH.DeadBranch (failure of the slicer)
 * and add a message saying if type errors have been discovered (success of the slicer)
 * - this applies to both the finishedLisp* and finishedPerl* functions. *)
fun finishedLispM x = "(setq skalpel-finished-message \"" ^ x ^ "\")"
(** A message for lisp indicatng the slicer terminated successfully (but applogises for it...?). *)
fun finishedLispMessage1 msg =
    case msg of
	"" => finishedLispM "slicer worked OK, sorry no debugging message"
      | _  => finishedLispM ("slicer worked OK, " ^ msg)
(** A message for lisp indicatng the slicer terminated with an error. *)
fun finishedLispMessage2 msg =
    case msg of
	"" => finishedLispM "slicer encountered an internal bug, sorry no debugging message"
      | _  => finishedLispM ("slicer encountered an internal bug, " ^ msg)
(** Builds the finished perl message. *)
fun finishedPerlM x = "$sml-tes-finished-message=\"" ^ x ^ "\""
(** A message for perl indicatng the slicer terminated with success (but appologises for it..?). *)
fun finishedPerlMessage1 msg =
    case msg of
	"" => finishedPerlM "slicer worked OK, sorry no debugging message" |
	_  => finishedPerlM ("slicer worked OK, " ^ msg)
(** A message for perl indicatng the slicer terminated with an error. *)
fun finishedPerlMessage2 msg =
    case msg of
	"" => finishedPerlM "slicer encountered an internal bug, sorry no debugging message"
      | _  => finishedPerlM ("slicer encountered an internal bug, " ^ msg)


(** Takes the basis flag and a file to be used as the basis. *)
fun getFileBasAndNum nenv filebas =
    (OS.FileSys.fileSize filebas;
     case nenv of
	 2 => (nenv, filebas)
       | _ => (nenv, ""))
    handle OS.SysErr (str, opt) => (case nenv of 2 => 1 | _ => nenv, "")

(** Called by preslicer' if Tester.slicergen returns NONE *)
fun getDebugProblem () =
    let
	(** Takes unit as an argument, and returns a string indicating a problem occurred. *)
	fun f () = "PROBLEM!\n"
    in (f, f, f, f, f, f)
    end

(** Returns a two tuple of a boolean (whether suff is a suffix of file), and the
 * file name without the suffix (the empty string if suff is not a suffix *)
fun getBoolFile file suff =
    if String.isSuffix suff file
    then (true, String.substring (file, 0, (String.size file) - (String.size suff)))
	 handle Subscript => (false, "")
    else (false, "")

(** If suff is a suffix of ffile, then an output file will be created from the
 * file name, concatenated with '-', the counter, and the suffix, which
 * contains the result of fdebug applied to str *)
fun genOutputFile (bfile, ffile) suff counter fdebug str filesin =
    if bfile
    then let
	     (** File to be used as output. *)
	     val file  = ffile  ^ "-" ^ Int.toString counter ^ suff
	     (** File to be used as output with a .tmp extension. *)
	     val file' = file ^ ".tmp"
	     (** Output stream. *)
	     val stout = TextIO.openOut file'
	     val _     = TextIO.output (stout, fdebug str)
	     val _     = TextIO.closeOut stout
	     val _     = OS.FileSys.rename {old = file', new = file}
	 in ()
	 end
    else ()

(** Generates the -finished file after the -counter files (see genOutputFile) have been generated. *)
fun genFinished (bfile, ffile) suff msg =
    if bfile
    then let
	     (** Filename to generate the finished file, indicating Skalpel has finished. *)
	     val fin   = ffile ^ "-finished" ^ suff
	     (** Filename to generate the finished file, indicating Skalpel has finished, with a .tmp extension.. *)
	     val fin'  = fin ^ ".tmp"
	     (** Output stream. *)
	     val stout = TextIO.openOut fin'
	     val _     = TextIO.output (stout, msg)
	     val _     = TextIO.closeOut stout
	     val _     = OS.FileSys.rename {old = fin', new = fin}
	 in
	     ()
	 end
	 handle IO.Io {name, function, cause} => TextIO.output (TextIO.stdErr, "Input/Output error. Cannot open or close the following file: "^ffile^"\n")
    else ()

(** Prints the integer value of iderror. *)
fun printIdError iderror = Int.toString (ER.idToInt iderror)

(** Prints the items in the errors list. *)
fun printErrors errors =
    "[" ^ #1 (foldr (fn (error, (st, sep)) =>
			(printIdError (ER.getI error) ^
			 "[" ^ #1 (foldr (fn (id, (st, sep)) => (printIdError id ^ sep ^ st, ",")) ("", "") (ER.getE error)) ^ "]" ^
			 sep ^ st, ","))
		    ("", "")
		    errors)
    ^ "]"

(** Prints out the error it found, and how long it took to find the error. *)
fun printFound counter errors time =
    print ("[Skalpel:" ^
	   " found counter=" ^ Int.toString counter ^
	   " time="          ^ Int.toString time    ^
	   " errors="        ^ printErrors  errors  ^ "]\n")

(** Calls the necessary functions to write the *.html, *.xml, *.sml etc to the system. *)
fun export nenv filebas (bhtml, fhtml) bfxml bfsml bfjson bflisp bfperl bfviz basisoverloading filesin
	   errs parse bmin times cs initlab name st counter time =
    let
	(** Holds result of calling #Tester.debuggingHTML with the errors we generated. *)
	val dbghtml  = Tester.debuggingHTML errs parse bmin times cs initlab true name true nenv basisoverloading ER.removeBasisSlice
	(** Holds result of calling #Tester.debuggingXML with the errors we generated. *)
	val dbgxml   = Tester.debuggingXML  errs parse bmin times cs initlab true name true nenv basisoverloading
	(** Holds result of calling #Tester.debuggingSML with the errors we generated. *)
	val dbgsml   = Tester.debuggingSML  errs parse bmin times cs initlab true name true nenv basisoverloading
	(** Holds result of calling #Tester.debuggingJSON with the errors we generated. *)
	val dbgjson  = Tester.debuggingJSON  errs parse bmin times cs initlab true name true nenv basisoverloading
	(** Holds result of calling #Tester.debuggingLISP with the errors we generated. *)
	val dbglisp  = Tester.debuggingLISP errs parse bmin times cs initlab true name true nenv basisoverloading
	(** Holds result of calling #Tester.debuggingPERL with the errors we generated. *)
	val dbgperl  = Tester.debuggingPERL errs parse bmin times cs initlab true name true nenv basisoverloading
	(** Holds result of calling #Tester.debuggingVIZ with the errors we generated. *)
	val dbgviz  = Tester.debuggingJSON errs parse bmin times cs initlab true name true nenv basisoverloading

	val dbghtml' = fn sep => dbghtml (fhtml ^ "-" ^ Int.toString counter ^ ".html") filebas false sep
	val _  = if (!terminalSlices <> NO_DISPLAY) then Tester.debuggingBASH errs parse bmin times cs initlab true name true nenv basisoverloading "" else ()
	val _ = if bhtml then dbghtml' st else ()
	val _ = genOutputFile bfxml  ".xml" counter dbgxml  st filesin
	val _ = genOutputFile bfsml  ".sml" counter dbgsml  st filesin
	val _ = genOutputFile bfjson  ""    counter dbgjson st filesin
	val _ = genOutputFile bflisp ".el"  counter dbglisp st filesin
	val _ = genOutputFile bfperl ".pl"  counter dbgperl st filesin
	val _ = genOutputFile bfviz ".viz"  counter dbgviz  st filesin
	val _ = printFound counter errs time
    in ()
    end

(** The primary function in this file which calls the functions necessary to run the slicer, including the main Testing.slicing function. *)
fun commslicerp' filebas filesin filehtml filexml filesml filejson filelisp fileperl fileviz nenv time tab sol min dev bcs searchspace basisoverloading =
	let val (nenv, filebas) = getFileBasAndNum nenv filebas

	(** Checks that the HTML file specified has a .html extension. *)
	val bfhtml = getBoolFile filehtml ".html"
	(** Checks that the XML file specified has a .xml extension. *)
	val bfxml  = getBoolFile filexml  ".xml"
	(** Checks that the SML file specified has a .sml extension. *)
	val bfsml  = getBoolFile filesml  ".sml"
	(** Checks that the JSON file specified has a no extension. *)
	val bfjson = (if filejson = "" then false else true, filejson)
	(** Checks that the Emacs lisp file specified has a .el extension. *)
	val bflisp = getBoolFile filelisp ".el"
	(** Checks that the PERL file specified has a .pl extension. *)
	val bfperl = getBoolFile fileperl ".pl"
	(** Checks that the VIZ file specified has a .viz extension *)
	val bfviz = getBoolFile fileviz ".viz"

	(* write the files to the system *)
	val fout   = export nenv filebas bfhtml bfxml bfsml bfjson bflisp bfperl bfviz basisoverloading (List.hd filesin)

	(* get various information from Tester, such as solution number *)
	val _      = Option.map (fn t => Tester.setTabSize t) tab
	val _      = Tester.settimelimit ((Int.toLarge time) handle Overflow => Tester.mytimelimit)

	(** Calls the slicing function in Tester.sml. *)
	fun run () = case Tester.slicing filebas filesin fout nenv (!webdemo) min dev bcs searchspace basisoverloading of
			 0 => (false, "this case should never happen")
		       | 1 => (true,  "it detected no errors (some might be undetected)"(*"program is typable"*))
		       | _ => (true,  "program has type or syntax errors")

	(* handle errors depending on the developer option *)
	val (bfm, msg) = if dev
			 then run ()
			      handle EH.TODO str => (TextIO.output (TextIO.stdErr, "TODO raised: " ^ str ^ "\n"); (false, str))
			 else run ()
			      handle EH.DeadBranch st => (TextIO.output (TextIO.stdErr, ("the slicer encountered an impossible case: "^st^"\n")); (false, st))
				   | EH.TODO str => (TextIO.output (TextIO.stdErr, "TODO raised: " ^ str ^ "\n"); (false, str))
				   | Fail st => (TextIO.output (TextIO.stdErr, "Error: " ^ st ^ "\n"); (false, ""))
				   | Subscript => (TextIO.output (TextIO.stdErr, "Error: Subscript\n"); (false, ""))
				   | _ => (TextIO.output (TextIO.stdErr, "the slicer failed for some reason\n"); (false, ""))

	(** Finished lisp messaege. *)
	val fmlisp = if bfm then finishedLispMessage1 msg else finishedLispMessage2 msg
	(** Finished PERL message. *)
	val fmperl = if bfm then finishedPerlMessage1 msg else finishedPerlMessage2 msg
	val _ = Option.map (fn _ => Tester.setTabSize (Tester.getTabSize())) tab
	val _ = Tester.settimelimit (Tester.gettimelimit ())

	(* generate the finished files *)
	val _ = genFinished bfhtml ".html" ""
	val _ = genFinished bfxml  ".xml"  ""
	val _ = genFinished bfsml  ".sml"  ""
	val _ = genFinished bflisp ".el"   fmlisp
	(* we remove the perl files if we are using the terminal front-end, and we don't need a finished perl file for that *)
	val _ = if (!terminalSlices) = NO_DISPLAY
		then genFinished bfperl ".pl"   fmperl
		else ()
	val _ = genFinished bfjson "" ""
	val _ = genFinished bfviz ".viz" ""

    in ()
    end

(** calls commslicerp', if we are not developing (not dev) then the error is handled, otherwise we leave the error so we can debug. *)
fun slicerCheckDevMode filebas filesin filehtml filexml filesml filejson filelisp fileperl fileviz nenv time tab sol min dev bcs searchspace basisoverloading =
    if dev
    then commslicerp' filebas filesin filehtml filexml filesml filejson filelisp fileperl fileviz nenv time tab sol min dev bcs searchspace basisoverloading
    else commslicerp' filebas filesin filehtml filexml filesml filejson filelisp fileperl fileviz nenv time tab sol min dev bcs searchspace basisoverloading

(** Called by the emacs interface; sets no tab and uses the solution from sol in utils/Solution.sml. *)
fun commslicerp  filebas filesin filehtml filexml filesml filelisp fileperl fileviz nenv time basisoverloading =
    slicerCheckDevMode filebas filesin filehtml filexml filesml "" filelisp fileperl fileviz nenv time NONE 9 true false false 1 basisoverloading

(** The full version of the slicer function with all arguments. *)
fun slicerFull [filebas, filein, filehtml, filexml, filesml, filejson, filelisp, fileperl, fileviz, basop, tlim, tab, sol, min, dev, bcs, searchSpace, basisoverloading] =
    let
	val mtl = Int.fromLarge Tester.mytimelimit
	val nop = Int.fromString  basop         handle Overflow => SOME 2
	val n   = Option.valOf    nop           handle Option   => 2
	val top = Int.fromString  tlim          handle Overflow => SOME mtl
	val t   = Option.valOf    top           handle Option   => mtl
	val mop = Bool.fromString min
	val m   = Option.valOf    mop           handle Option   => false
	val aop = Bool.fromString dev
	val a   = Option.valOf    aop           handle Option   => false
	val cop = Bool.fromString bcs
	val c   = Option.valOf    cop           handle Option   => false
	val pop = Int.fromString  searchSpace   handle Overflow => SOME 1
	val p   = Option.valOf    pop           handle Option   => 1
	val basisoverloading = Option.valOf(Int.fromString  basisoverloading)
	val _   =
	    slicerCheckDevMode
		filebas [filein] filehtml filexml filesml filejson filelisp fileperl fileviz
		n
		t
		(Int.fromString tab handle Overflow => NONE)
		9
		m
		a
		c
		p
		basisoverloading
    (*(2010-03-24)The tab one is for the tabulation size.  NONE is for when we don't want to change it. *)
    in OS.Process.success
    end
  | slicerFull _ = (print("Incorrect arguments specified. Run with --help to see arguments list"); OS.Process.failure)

(** Prints the legend that is used when we output slices into the terminal. *)
fun printLegend () =
	let
		val indent=""
		(** A function which prints the sting argument and appnds the string #Debug.textReset. *)
		fun printReset str = print (str^(!D.textReset))
	in
		(printReset ((#yellow (!D.underlineColors))^"Legend:\n\n");
		printReset ("  "^(#red (!D.backgroundColors))^"red highlights\n");
		printReset "  - Indicates that the highlighted code contributes to the error.\n\n";
		printReset ("  "^(#blue (!D.backgroundColors))^"blue" ^ (!D.textReset) ^ " / " ^ (#cyan (!D.backgroundColors)) ^ "cyan" ^ (!D.textReset) ^ " highlights \n");
		printReset "  - Indicates that the highlighted code is an endpoint of either\n";
		printReset  "\t - a type constructor clash\n\t - an arity clash\n\t - a record clash\n";
		printReset  "\t NOTE: The cyan used here is equivalent to the gray\n\t       used by other interfaces of the type error slicer.\n\n";
		printReset ("  "^(#green (!D.backgroundColors))^"green highlights\n");
		printReset "  - Indicates that the highlighted code is the endpoint of a record clash.\n\n";
		printReset  ("  "^(#purple (!D.backgroundColors))^"purple highlights\n");
		printReset "  - Indicates that the highlighted code either\n";
		printReset  "\t - provides information about an identifier\n\t - is expansive.\n\n";
		printReset ("  "^(#yellow (!D.backgroundColors))^"yellow highlights\n");
		printReset "  - Indicates that the type error slicer cannot parse the file.\n\n";
		printReset  ("  "^ (#purple (!D.underlineColors)) ^"underlined text\n");
		printReset "  - The use of the underline with no background highlighting is used to\n";
		printReset  "    signify that the underlined text may be irrelevant, but its presence\n";
		printReset  "    definitely contributes to the error.\n";
		printReset "  - The use of the underline can indicate on of two things;\n";
		printReset  "\t1. The application of a function to an argument (the underlined\n";
		printReset  "\t    code) takes part in an error.\n";
		printReset  "\t2. The underlined code is the unique argument of a type name\n";
		printReset  "\t   to make explicit that its arity is 1 (that is, it has 1 argument).\n";
		printReset "  - NOTE: The colour of the underline can be any of those above.\n";
		printReset "  - NOTE: This notation is equivalent to the box notation used in\n";
		printReset "          other interfaces to the type error slicer.\n\n";
		printReset ("  " ^ (#blue (!D.underlineColors)) ^ "foo" ^ (#red (!D.underlineColors)) ^ "underlined text" ^ (#blue (!D.underlineColors)) ^ "foo\n");
		printReset "  - The use of the strike through effect is to use to show that the\n";
		printReset "    highlighted code is in a nested underlined.\n";
		printReset "  - The colour of the nested box is that of the strike through and\n";
		printReset "    the colour of the external box is the colour of the underlines\n";
		printReset "    which immediately precede and follow the strike through.\n")
	end

(** A function which has been created so that the slicer can be used
 * in various ways without the need to program a new function every
 * time. Parameters have been kept mostly the same as the cmd-line
 * interface to avoid confusion. Currently assumes there are no spaces
 * in any directories.
 *)
fun smlTesStrArgs strArgs =
	let
	(* these are the arguments passed to slicerFull *in order* *)
	val filebas  = ref "../lib/basis.sml"
	(** The file to be used as input. *)
	val filein   = ref ""
	(** The filename for HTML slices. *)
	val filehtml = ref ""
	(** The filename for XML slices. *)
	val filexml  = ref ""
	(** The file name for SML slices. *)
	val filesml  = ref ""
	(** The file name for JSON slices. *)
	val filejson = ref ""
	(** The filename for lisp slices. *)
	val filelisp = ref ""
	(** The filename for perl slices. *)
	val fileperl = ref ""
	(** The filename for visualisation slices. *)
	val fileviz = ref ""
	(** Basis operator. *)
	val basop    = ref ""
	(** Holds the timelimit for Skalpel. *)
	val tlim     = ref ""
	val tab      = ref ""
	(** The solution number to use
	 * \deprecated We no longer have different 'solutions' in Skalpel. *)
	val sol      = ref ""
	val min      = ref ""
	(** Holds whether developer mode is enabled. *)
	val dev      = ref ""
	val bcs      = ref ""
	(** Value used to hold searchspace choice. *)
	val search = ref ""
	(** A boolean set to true if we are running the test database. *)
	val runtests = ref false
	(** A value holding whether a termnal slice viewing option has been set. *)
	val terminalSet = ref false
	(** A value indicating if the user needs to specify a file or not (e.g. they don't need to for showing the legend. *)
	val filesNeeded = ref true
	(** A value holding whether the user specified a basis file. *)
	val basisSpecified = ref false
	(** A value indicating if the user needs to specify an output file or not (e.g. they don't need to for showing the legend. *)
	val outputFilesNeeded = ref true
	(** A value holding the basis overloading value, a value used to control slice information from the basis. *)
	val basisoverloading = ref "1"

	(** Prints the help text for the user (gives command-line options). *)
	fun printHelp () =
	    print ("usage: skalpel [option ...] [FILE] \n\
				    \    FILE file taken as input to be sliced\n\
				    \    -l <file> place output in <file> in lisp format\n\
				    \    -h <file> place output in <file> in HTML format\n\
				    \    -s <file> place output in <file> in SML format\n\
				    \    -j <file> place output in <file> in JSON format\n\
				    \    -x <file> place output in <file> in XML format\n\
				    \    -p <file> place output in <file> in perl format\n\
				    \    -z <file> place output in <file> in visualisation format\n\
				    \    -t <timelimet> specify a numerical time limit\n\
				    \    -x <true/false> suppress exception handling (dev mode)\n\
				    \    -c <directory> Run analysis engine on tests in <directory>\n\
				    \    -e <0 | 1> toggles echo of slice display in terminal (0=no, 1=yes)\n\
				    \    -b <0 | 1 | 2 <file> > Set basis level as 0 (no basis), 1 (built in basis), 2 <file> (specify file as basis)\n\
				    \    -d PARSING \t shows debugging output during parsing various files\n\
				    \       NO_COLOURS \t doesn't put ANSI colour information in debug strings\n\
				    \       ONE_RUN \t give debug output only once (don't show during minimisation process)\n\
				    \       STATE   \t gives internal state output\n\
				    \       TESTING \t shows debugging info when running the test database\n\
				    \       CONSTRAINT_PATH \t shows path taken through the constraint generator\n\
				    \       CONSTRAINT_GENERATION \t shows constraint generation debugging information\n\
				    \       CONSTRAINT_SOLVING \t shows constraint solving debugging information\n\
				    \       PROGRAM_LABELLING <filename> \t gives a labelled program output in latex to file specified in environment variable $SKALPEL_LABELLED_PROGRAM\n\
				    \       BASIS \t applies debugging flags also to the basis file\n\
				    \       BASIS_LABELLING <filename> \t gives a labelled basis output in latex to file specified in environment variable $SKALPEL_LABELLED_BASIS\n\
				    \       EQUALITY_TYPES \t debugging information for equality types\n\
				    \       MINIMISATION \t debugging information for the minimisation algorithm\n\
				    \    -bo <0 | 1> If set to 1, hides basis slice in overloading errors\n\
				    \    -tab <tabwidth> define the tab width in user code regions\n\
				    \    -sol <solution> define solution to use (default 9)\n\
				    \    -min <true/false> if true, shows non-minimal errors\n\
				    \    --print-env <true/false> whether to print the environment\n\
				    \    --show-legend Shows the legend for notation and colour of slice display in the terminal\n\
				    \    --search-space <1,2,3> Use search space 1 (lists), 2 (sets), or 3 (red black tree)\n\
				    \    --help Show this help text\n");

	(** Split into tokens to allow for easy parsing. *)
	val split = String.tokens Char.isSpace strArgs

	(** Checks the file suffixes of the files the user has specified (.html for HTML output, etc.). *)
	fun checkFileSuffix () =
	    let
		val _ = if (String.isSuffix ".html" (!filehtml) orelse (!filehtml = ""))
			then ()
			else filehtml := (!filehtml) ^ ".html"
		val _ = if (String.isSuffix ".xml" (!filexml) orelse (!filexml = ""))
			then ()
			else filexml := (!filexml) ^ ".xml"
		val _ = if (String.isSuffix ".sml" (!filesml) orelse (!filesml = ""))
			then ()
			else filesml := (!filesml) ^ ".sml"
		val _ = if (String.isSuffix ".el" (!filelisp) orelse (!filelisp = ""))
			then ()
			else filelisp := (!filelisp) ^ ".el"
		val _ = if (String.isSuffix ".pl" (!fileperl) orelse (!fileperl = ""))
			then ()
			else fileperl := (!fileperl) ^ ".pl"
		val _ = if (String.isSuffix ".viz" (!fileviz) orelse (!fileviz = ""))
			then ()
			else fileviz := (!fileviz) ^ ".viz"
	    in
		()
	    end

	(** Parses the arguments specified on the command-line. *)
	fun parse [] = ()
	 |  parse ["--help"] = printHelp ()
	 |  parse ["-v"] = (filesNeeded := false; print (SKALPEL_VERSION ^ "\n"))
	 |  parse ["--show-legend"] = (filesNeeded:=false; printLegend())
	 |  parse [file] = (filein:=file)
	  (* have a 0/1/2 case for emacs ui *)
	 |  parse ("-b"::"0"::t) = (basisSpecified := true; basop:="0"; parse t)
	 |  parse ("-b"::"1"::t) = (basisSpecified := true; basop:="1"; parse t)
	 |  parse ("-b"::"2"::file::t) =
	    (* why do we have two filebas here? *)
	    (basisSpecified := true; Tester.myfilebas:=file; filebas:=file; basop:="2"; parse t)
	 |  parse ("-e"::"0"::t) =
	    (terminalSet := true; terminalSlices := NO_DISPLAY; parse t)
	 |  parse ("-e"::"1"::t) =
	    (terminalSet := true; terminalSlices := NON_INTERACTIVE; parse t)
	 |  parse ("-f"::str::t) = (filein:=str; parse t)
	 |  parse ("-h"::str::t) = (filehtml:=str; parse t)
	 |  parse ("-x"::str::t) = (filexml:=str; parse t)
	 |  parse ("-s"::str::t) = (filesml:=str; parse t)
	 |  parse ("-j"::str::t) = (filejson:=str; parse t)
	 |  parse ("-z"::str::t) = (fileviz:=str; parse t)
	 |  parse ("-l"::str::t) = (filelisp:=str; parse t)
	 |  parse ("-p"::str::t) = (fileperl:=str; parse t)
	 |  parse ("-b"::str::t) = (basop:=str; parse t)
	 |  parse ("-c"::str::t) = (runtests:=true;Tester.testFolder:=str;filesNeeded:=false;Tester.checktests []; parse t)
	 |  parse ("-bo"::str::t) = (basisoverloading:=str; parse t)
	 |  parse ("-t"::str::t) = (tlim:=str; parse t)
	 |  parse ("-tab"::str::t) = (tab:=str; parse t)
	 |  parse ("-sol"::str::t) = (sol:=str; parse t)
	 |  parse ("-min"::str::t) = (min:=str; parse t)
	 |  parse ("--print-env"::str::t) = (bcs:=str; parse t)
	 |  parse ("-search-space"::str::t) = (search:=str; parse t)
	 |  parse ("-d"::str::t) = (dev:="true";
			(* note that at this current time, no debugging information is printed for the basis.
			 * In Analyze.sml we turn off D.debug when looking at the basis, the user should really
			 * be able to toggle such an option, but for the moment this is simply disabled *)
			(case str
			of "NO_COLOURS" => (D.colors := {black="",red="",green="",yellow="",blue="",purple="",cyan="",white=""};
				D.boldColors := {black="",red="",green="",yellow="",blue="",purple="",cyan="",white=""};
				D.underlineColors := {black="",red="",green="",yellow="",blue="",purple="",cyan="",white=""};
				D.backgroundColors := {black="",red="",green="",yellow="",blue="",purple="",cyan="",white=""};
				D.textReset := "")
			| "EQUALITY_TYPES" => (D.debug := true; D.enableDebugFeature D.EQUALITY_TYPES)
			| "MINIMISATION" => (D.debug := true; D.enableDebugFeature D.MINIMISATION)
			| "PROGRAM_LABELLING" => (D.debug := true; D.enableDebugFeature D.PROGRAM_LABELLING)
			| "BASIS_LABELLING" => (D.debug := true; D.enableDebugFeature D.BASIS_LABELLING)
			| "BASIS" => (D.debug := true; D.debugBasis := true)
			| "CONSTRAINT_PATH" => (D.debug := true; D.enableDebugFeature D.CONSTRAINT_PATH)
			| "CONSTRAINT_GENERATION" => (D.debug := true; D.enableDebugFeature D.CONSTRAINT_GENERATION)
			| "CONSTRAINT_SOLVING" => (D.debug := true; D.enableDebugFeature D.CONSTRAINT_SOLVING)
			| "TESTING" => (D.debug := true; D.enableDebugFeature D.TESTING)
			| "PARSING" => (D.debug := true; D.enableDebugFeature D.PARSING)
			| "STATE" => (D.debug := true; D.enableDebugFeature D.STATE)
			| "ONE_RUN" => D.oneRunOnly := true
			| "TEMP" => (D.debug := true; D.enableDebugFeature D.TEMP)
			| str  => (print ("Unrecognised debugging feature: "^str^"\n"); raise Fail ("Unrecognised debugging feature: "^str)));
			outputFilesNeeded := false; parse t)
	 |  parse (opt::str::t) = (TextIO.output (TextIO.stdErr, "Unknown argument fed as input"); raise Fail "Unknown argument fed as input"; parse t)
	in
	(* parse the arguments *)
		if strArgs = "" then
			(printHelp (); raise Fail "No arguments specified.")
		else
		(parse split;

		(* check that the user specified an input file *)
		if (!filein = "" andalso !filesNeeded) then
			(print ("Error: No input file specified.");
			raise Fail("No input file specified"))
		else if (!filein = "") then
			OS.Process.success (* the user was checking tests *)
		else (
			(* display slices in the terminal by default *)
			if ((!filehtml)^(!filexml)^(!filesml)^(!filelisp)^(!filejson)^(!fileviz) = "" andalso
				!runtests = false andalso
				(!terminalSet = false orelse !terminalSlices <> NO_DISPLAY)
			) then
				if (!fileperl) = "" then
					(fileperl := "/tmp/output.pl"; terminalSlices := NON_INTERACTIVE)
				else (terminalSlices := NON_INTERACTIVE)
			else ();

			checkFileSuffix ();

			(* if a basis option hasn't been stated, then look for the SKALPEL_BASIS environment variable
			 * if we find one, use its contents
			 * if we do not, print a warning and default to -b 0 *)
			if (!basisSpecified = false) then
				case (OS.Process.getEnv "SKALPEL_BASIS")
				of NONE => (print "Error: Couldn't find basis file location in command line argument or environment variable (SKALPEL_BASIS).\n";
							raise Fail("No basis option specified"))
				|  SOME file => (Tester.myfilebas:=file; filebas:=file; basop:="2")
			else ();

			(* now all arguments are dereferenced and passed to slicerFull *)
			slicerFull [!filebas,
				!filein,
				!filehtml,
				!filexml,
				!filesml,
				!filejson,
				!filelisp,
				!fileperl,
				!fileviz,
				!basop,
				!tlim,
				!tab,
				!sol,
				!min,
				!dev,
				!bcs,
				!search,
				!basisoverloading];
			OS.Process.success))
	end
    handle Fail _ => OS.Process.failure

(** Used by the MLton and Poly/ML entry points. *)
fun slicerGen [] name = smlTesStrArgs ""
 |  slicerGen args name = smlTesStrArgs (foldr (fn (a,b) => a^" "^b) "" args)

(******************************************************************************
 *                           Entry point functions                            *
 ******************************************************************************)

(** Entry point for the SML/NJ compiler. *)
fun smlnjEntryPoint (binaryName, argumentList) = smlTesStrArgs (foldr (fn (x,y)=>x^" "^y) "" argumentList)
(** Entry point for the MLton compiler. *)
fun mltonEntryPoint () = slicerGen (CommandLine.arguments ()) (CommandLine.name ()) handle EH.DeadBranch str => (print ("Error: "^str); OS.Process.failure)
(** Entry point for the Poly/ML compiler. *)
fun polymlEntryPoint () = OS.Process.exit (slicerGen (CommandLine.arguments ()) (CommandLine.name ());
					   OS.Process.success)

end
