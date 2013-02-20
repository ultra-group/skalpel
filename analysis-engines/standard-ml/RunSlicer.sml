(* Copyright 2009 2010 2011 2012 2013 Heriot-Watt University
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
 *  o Date:        24 May 2010
 *  o File name:   RunSlicer.sml
 *  o Description: Contains the functions that serve as interface with
 *      the slicer.  The file defines the structure Slicer which has
 *      the signature Slicer.
 *)


structure Slicer : SLICER = struct

(* shorten the names of structures for use in the code *)
structure ER = Error
structure EH = ErrorHandler
structure D  = Debug
structure PP = Ppp

(* flag set if we are building the web demo binary *)
val webdemo = ref false
val data_tmp = PP.data_tmp

type error = JsonParser.error
val error : error = Tester.error

(* localise Tester functions *)
val myfilehtml     = Tester.myfilehtml

(* datatype for determining whether the user wishes slices displayed in a terminal *)
(* INTERACTIVE is not used yet. It perhaps should be added, so the user can cycle through slices *)
datatype terminalSliceDisplay = NO_DISPLAY | NON_INTERACTIVE | INTERACTIVE
val terminalSlices : terminalSliceDisplay ref = ref NO_DISPLAY

(* do not change the below line! We change it using sed in the makefile and insert the git hash *)
val SKALPEL_VERSION = "0.8"

(* takes a boolean value b, if true then we are generating a binary for the web demo *)
fun setWebDemo b = webdemo := b

(* Enhance these messages with the messages from EH.DeadBranch (failure of the slicer)
 * and add a message saying if type errors have been discovered (success of the slicer)
 * - this applies to both the finishedLisp* and finishedPerl* functions. *)
fun finishedLispM x = "(setq skalpel-finished-message \"" ^ x ^ "\")"
fun finishedLispMessage1 msg =
    case msg of
	"" => finishedLispM "slicer worked OK, sorry no debugging message"
      | _  => finishedLispM ("slicer worked OK, " ^ msg)
fun finishedLispMessage2 msg =
    case msg of
	"" => finishedLispM "slicer encountered an internal bug, sorry no debugging message"
      | _  => finishedLispM ("slicer encountered an internal bug, " ^ msg)

fun finishedPerlM x = "$sml-tes-finished-message=\"" ^ x ^ "\""
fun finishedPerlMessage1 msg =
    case msg of
	"" => finishedPerlM "slicer worked OK, sorry no debugging message" |
	_  => finishedPerlM ("slicer worked OK, " ^ msg)
fun finishedPerlMessage2 msg =
    case msg of
	"" => finishedPerlM "slicer encountered an internal bug, sorry no debugging message"
      | _  => finishedPerlM ("slicer encountered an internal bug, " ^ msg)


(* takes the basis flag and a file to be used as the basis*)
fun getFileBasAndNum nenv filebas =
    (OS.FileSys.fileSize filebas;
     case nenv of
	 2 => (nenv, filebas)
       | _ => (nenv, ""))
    handle OS.SysErr (str, opt) => (case nenv of 2 => 1 | _ => nenv, "")

(* called by preslicer' if Tester.slicergen returns NONE*)
fun getDebugProblem () =
    let fun f () = "PROBLEM!\n"
    in (f, f, f, f, f, f)
    end

(* remove this when test database is converted to json format *)

fun convertErrors currentError newName = Tester.convertErrors currentError newName
fun generateTests min max = Tester.generateTests min max

(* pass false to get the range of tests (eg [1-565])
 * pass true to see which tests are typeable *)
fun listtests  false      = Tester.listTests ()
  | listtests  true       = Tester.printTypables ()

(* returns a two tuple of a boolean (whether suff is a suffix of file), and the
 * file name without the suffix (the empty string if suff is not a suffix *)
fun getBoolFile file suff =
    if String.isSuffix suff file
    then (true, String.substring (file, 0, (String.size file) - (String.size suff)))
	 handle Subscript => (false, "")
    else (false, "")

(* if suff is a suffix of ffile, then an output file will be created from the
 * file name, concatenated with '-', the counter, and the suffix, which
 * contains the result of fdebug applied to str *)
fun genOutputFile (bfile, ffile) suff counter fdebug str filesin =
    if bfile
    then let val file  = ffile  ^ "-" ^ Int.toString counter ^ suff
	     val file' = file ^ ".tmp"
	     val stout = TextIO.openOut file'
	     val _     = TextIO.output (stout, fdebug str)
	     val _     = TextIO.closeOut stout
	     val _     = OS.FileSys.rename {old = file', new = file}
	 in
	     (* if (!terminalSlices) <> NO_DISPLAY andalso suff=".pl" *)
	     (* then *)
	     (* 	 (* this will probably not work on the windows operating system- need to check this! *) *)
	     (* 	 let *)
	     (* 	     val execAll = OS.Process.system("skalpel-perl-to-bash"^" "^filesin^" "^(ffile^suff)^" "^"; for FILE in "^ffile^"-"^(Int.toString counter)^".sh ; do if [ ${FILE:0:1} = \"/\" ]; then $FILE; else ./$FILE; fi; rm -f "^ffile^"*.sh; done;")  handle OS.SysErr (str, opt) => raise Fail str *)
	     (* 	 in *)
	     (* 	     () *)
	     (* 	 end *)
	     (* else () *)
	     ()
	 end
    else ()

(* generates the -finished file after the -counter files (see genOutputFile)
 * have been generated *)
fun genFinished (bfile, ffile) suff msg =
    if bfile
    then let val fin   = ffile ^ "-finished" ^ suff
	     val fin'  = fin ^ ".tmp"
	     val stout = TextIO.openOut fin'
	     val _     = TextIO.output (stout, msg)
	     val _     = TextIO.closeOut stout
	     val _     = OS.FileSys.rename {old = fin', new = fin}
	 in
	     ()
	 end
	 handle IO.Io {name, function, cause} => TextIO.output (TextIO.stdErr, "Input/Output error. Cannot open or close the following file: "^ffile^"\n")
    else ()

(* prints the integer value of iderror *)
fun printIdError iderror = Int.toString (ER.idToInt iderror)

(* prints the items in the errors list *)
fun printErrors errors =
    "[" ^ #1 (foldr (fn (error, (st, sep)) =>
			(printIdError (ER.getI error) ^
			 "[" ^ #1 (foldr (fn (id, (st, sep)) => (printIdError id ^ sep ^ st, ",")) ("", "") (ER.getE error)) ^ "]" ^
			 sep ^ st, ","))
		    ("", "")
		    errors)
    ^ "]"

(* prints out the error it found, and how long it took to find the error *)
fun printFound counter errors time =
    print ("[Skalpel:" ^
	   " found counter=" ^ Int.toString counter ^
	   " time="          ^ Int.toString time    ^
	   " errors="        ^ printErrors  errors  ^ "]\n")

(* calls the necessary functions to write the *.html, *.xml, *.sml etc
 * to the system *)
fun export nenv filebas (bhtml, fhtml) bfxml bfsml bfjson bflisp bfperl basisoverloading filesin
	   errs parse bmin times cs initlab name st counter time =
    let val dbghtml  = Tester.debuggingHTML errs parse bmin times cs initlab true name true nenv basisoverloading ER.removeBasisSlice
	val dbgxml   = Tester.debuggingXML  errs parse bmin times cs initlab true name true nenv basisoverloading
	val dbgsml   = Tester.debuggingSML  errs parse bmin times cs initlab true name true nenv basisoverloading
	val dbgjson  = Tester.debuggingJSON  errs parse bmin times cs initlab true name true nenv basisoverloading
	val dbglisp  = Tester.debuggingLISP errs parse bmin times cs initlab true name true nenv basisoverloading
	val dbgperl  = Tester.debuggingPERL errs parse bmin times cs initlab true name true nenv basisoverloading
	val dbghtml' = fn sep => dbghtml (fhtml ^ "-" ^ Int.toString counter ^ ".html") filebas false sep
	val dbgbash  = if (!terminalSlices <> NO_DISPLAY) then Tester.debuggingBASH errs parse bmin times cs initlab true name true nenv basisoverloading "" else ()
	val _ = if bhtml then dbghtml' st else ()
	val _ = genOutputFile bfxml  ".xml" counter dbgxml  st filesin
	val _ = genOutputFile bfsml  ".sml" counter dbgsml  st filesin
	val _ = genOutputFile bfjson  ""    counter dbgjson st filesin
	val _ = genOutputFile bflisp ".el"  counter dbglisp st filesin
	val _ = genOutputFile bfperl ".pl"  counter dbgperl st filesin
	val _ = printFound counter errs time
    in ()
    end

(* the primary function in this file which calls the functions necessary to
 * run the slicer, including the main Testing.slicing function *)
fun commslicerp' filebas filesin filehtml filexml filesml filejson filelisp fileperl nenv time tab sol min dev bcs searchspace basisoverloading =
    let val (nenv, filebas) = getFileBasAndNum nenv filebas

	(* check that the files have the correct extension*)
	val bfhtml = getBoolFile filehtml ".html"
	val bfxml  = getBoolFile filexml  ".xml"
	val bfsml  = getBoolFile filesml  ".sml"
	val bfjson = (if filejson = "" then false else true, filejson)
	val bflisp = getBoolFile filelisp ".el"
	val bfperl = getBoolFile fileperl ".pl"

	(* write the files to the system *)
	val fout   = export nenv filebas bfhtml bfxml bfsml bfjson bflisp bfperl basisoverloading (List.hd filesin)

	(* get various information from Tester, such as solution number *)
	val tmpsol = Tester.getsol ()
	val _      = Tester.setsol sol
	val tmptab = Tester.getTabSize ()
	val _      = Option.map (fn t => Tester.setTabSize t) tab
	val tmptm  = Tester.gettimelimit ()
	val _      = Tester.settimelimit ((Int.toLarge time) handle Overflow => Tester.mytimelimit)

	(* call the slicing function in tester *)
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
				   | _ => (TextIO.output (TextIO.stdErr, "the slicer failed for some reason\n"); (false, ""))

	(* if the slicer didn't fail, print the lisp/perl messages *)
	val fmlisp = if bfm then finishedLispMessage1 msg else finishedLispMessage2 msg
	val fmperl = if bfm then finishedPerlMessage1 msg else finishedPerlMessage2 msg
	val _ = Tester.setsol tmpsol
	val _ = Option.map (fn _ => Tester.setTabSize tmptab) tab
	val _ = Tester.settimelimit tmptm

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

    in ()
    end

(* calls commslicerp', if we are not developing (¬dev) then the error is
 * handled, otherwise we leave the error so we can debug *)
fun slicerCheckDevMode filebas filesin filehtml filexml filesml filejson filelisp fileperl nenv time tab sol min dev bcs searchspace basisoverloading =
    if dev
    then commslicerp' filebas filesin filehtml filexml filesml filejson filelisp fileperl nenv time tab sol min dev bcs searchspace basisoverloading
    else commslicerp' filebas filesin filehtml filexml filesml filejson filelisp fileperl nenv time tab sol min dev bcs searchspace basisoverloading

(* called by the emacs interface; sets no tab and uses the solution from sol in
 * utils/Solution.sml *)
fun commslicerp  filebas filesin filehtml filexml filesml filelisp fileperl nenv time basisoverloading =
    slicerCheckDevMode filebas filesin filehtml filexml filesml "" filelisp fileperl nenv time NONE (Tester.getsol ()) true false false 1 basisoverloading

(* the full version of the slicer function with all arguments *)
fun slicerFull [filebas, filein, filehtml, filexml, filesml, filejson, filelisp, fileperl, basop, tlim, tab, sol, min, dev, bcs, searchSpace, basisoverloading] =
    let
	val mtl = Int.fromLarge Tester.mytimelimit
	val mso = Tester.getsol ()
	val nop = Int.fromString  basop         handle Overflow => SOME 2
	val n   = Option.valOf    nop           handle Option   => 2
	val top = Int.fromString  tlim          handle Overflow => SOME mtl
	val t   = Option.valOf    top           handle Option   => mtl
	val sop = Int.fromString  sol           handle Overflow => SOME mso
	val s   = Option.valOf    sop           handle Option   => mso
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
		filebas [filein] filehtml filexml filesml filejson filelisp fileperl
		n
		t
		(Int.fromString tab handle Overflow => NONE)
		s
		m
		a
		c
		p
		basisoverloading
    (*(2010-03-24)The tab one is for the tabulation size.  NONE is for when we don't want to change it. *)
    in OS.Process.success
    end
  | slicerFull _ = (print("Incorrect arguments specified. Run with --help to see arguments list"); OS.Process.failure)

(* smltes will take in a record constructed by SlicerOptArgs and give
 * the output to slicerFull. The default arguments for this can be
 * found in the SlicerOptArgs file. *)
fun smltes ({fileBas     : string,
	     fileIn      : string,
	     fileHtml    : string,
	     fileXml     : string,
	     fileSml     : string,
	     fileJson    : string,
	     fileLisp    : string,
	     filePerl    : string,
	     tab         : int,
	     sol         : int,
	     min         : bool,
	     dev         : bool,
	     bcs         : bool,
	     searchSpace : int,
	     basOp       : int,
	     timeLim     : int}) =
    slicerFull [fileBas, fileIn, fileHtml, fileXml, fileSml, fileJson, fileLisp, filePerl,
		Int.toString  (basOp),
		Int.toString  (timeLim),
		Int.toString  (tab),
		Int.toString  (sol),
		Bool.toString (min),
		Bool.toString (dev),
		Bool.toString (bcs),
		Int.toString  (searchSpace)]


(* we open SlicerOptArgs so that the user does not have to open the
 * structure themselves to start using optional arguments to the
 * slicer. *)
open SlicerOptArgs


fun printLegend () =
    let
	val indent=""
	fun printReset str = print (str^(!D.textReset))
    in
	(printReset ((#yellow (!D.underlineColors))^"Legend:\n\n");
	 printReset ("  "^(#red (!D.backgroundColors))^"red highlights\n");
	 printReset "  - Indicates that the highlighted code contributes to the error.\n\n";
	 printReset ("  "^(#blue (!D.backgroundColors))^"blue" ^ (!D.textReset) ^ " / " ^ (#cyan (!D.backgroundColors)) ^ "cyan" ^ (!D.textReset) ^ " highlights \n");
	 printReset "  - Indicates that the highlighted code is an end point of either\n";
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
	 printReset "    which immediately precede and follow the strike through.\n"

	)
    end

(* Same as smltes but takes a list of parameters instead of a record *)
fun smlteslight list = smltes (optArg [list])

(* Same as smlteslight but with some default options *)
fun smltesdev list = smlteslight (list @ [BASOP 2, FILEBAS "../lib/basis.sml", FILEIN "test-prog.sml", DEV true])

(* A function which has been created so that the slicer can be used
 * in various ways without the need to program a new function every
 * time. Parameters have been kept mostly the same as the cmd-line
 * interface to avoid confusion.
 *
 * Currently assumes there are no spaces in any directories.
 *
 * This has been replaced by SlicerOptArgs, and should be removed at
 * some stage.
 *)
fun smlTesStrArgs strArgs =
    let
	(* these are the arguments passed to slicerFull *in order* *)
	val filebas  = ref "../lib/basis.sml"
	val filein   = ref ""
	val filehtml = ref ""
	val filexml  = ref ""
	val filesml  = ref ""
	val filejson = ref ""
	val filelisp = ref ""
	val fileperl = ref ""
	val basop    = ref ""
	val tlim     = ref ""
	val tab      = ref ""
	val sol      = ref ""
	val min      = ref ""
	val dev      = ref ""
	val bcs      = ref ""
	val search   = ref ""
	val runtests = ref false
	val terminalSet = ref false
	val filesNeeded = ref true
	val basisSpecified = ref false
	val outputFilesNeeded = ref true
	val basisoverloading = ref "1"

	fun printHelp () =
	    print ("usage: skalpel [option ...] [FILE] \n\
				    \    FILE file taken as input to be sliced\n\
				    \    -l <file> place output in <file> in lisp format\n\
				    \    -h <file> place output in <file> in HTML format\n\
				    \    -s <file> place output in <file> in SML format\n\
				    \    -j <file> place output in <file> in JSON format\n\
				    \    -x <file> place output in <file> in XML format\n\
				    \    -p <file> place output in <file> in perl format\n\
				    \    -t <timelimet> specify a numerical time limit\n\
				    \    -x <true/false> suppress exception handling (dev mode)\n\
				    \    -c <directory> Run analysis engine on tests in <directory>\n\
				    \    -e <0 | 1> toggles echo of slice display in terminal (0=no, 1=yes)\n\
				    \    -b <0 | 1 | 2 <file> > Set basis level as 0 (no basis), 1 (built in basis), 2 <file> (specify file as basis)\n\
				    \    -d PARSING \t shows debugging output during parsing various files\n\
				    \    -d NO_COLOURS \t doesn't put ANSI colour information in debug strings\n\
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
				    \    -bo <0 | 1> If set to 1, hides basis slice in overloading errors\n\
				    \    -tab <tabwidth> define the tab width in user code regions\n\
				    \    -sol <solution> define solution to use (default 9)\n\
				    \    -min <true/false> if true, shows non-minimal errors\n\
				    \    --print-env <true/false> whether to print the environment\n\
				    \    --show-legend Shows the legend for notation and colour of slice display in the terminal\n\
				    \    --search-space <1,2,3> Use search space 1 (lists), 2 (sets), or 3 (red black tree)\n\
				    \    --help Show this help text");

        (* split into tokens to allow for easy parsing *)
	val split = String.tokens Char.isSpace strArgs

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
	    in
		()
	    end

	fun parse [] = ()
 	  | parse [option] =
	    if option = "--help" then
		printHelp ()
	    else if option = "-v"
	    then (filesNeeded := false; print ("Version (git SHA1 hash): "^SKALPEL_VERSION))
	    else if option = "--show-legend"
	    then (filesNeeded:=false; printLegend())
	    else filein:=option
	  (* have a 0/1/2 case for emacs ui *)
	  | parse ("-b"::"0"::tail) =
	    (basisSpecified := true; basop:="0"; parse tail)
	  | parse ("-b"::"1"::tail) =
	    (basisSpecified := true; basop:="1"; parse tail)
	  | parse ("-b"::"2"::file::tail) =
	    (* why do we have two filebas here? *)
	    (basisSpecified := true; Tester.myfilebas:=file; filebas:=file; basop:="2"; parse tail)
	  | parse ("-e"::"0"::tail) =
	    (terminalSet := true; terminalSlices := NO_DISPLAY; parse tail)
	  | parse ("-e"::"1"::tail) =
	    (terminalSet := true; terminalSlices := NON_INTERACTIVE; parse tail)
	  | parse (option::str::tail)=
	     ((if option = "-f"
	     then filein:=str
	     else if option = "-h"
	     then filehtml:=str
	     else if option = "-x"
	     then filexml:=str
	     else if option = "-s"
	     then filesml:=str
	     else if option = "-j"
	     then filejson:=str
	     else if option = "-l"
	     then filelisp:=str
	     else if option = "-p" (*  *)
	     then fileperl:=str
	     else if option = "-b"
	     then basop:=str
	     else if option = "-c"
	     then (runtests := true; Tester.testFolder := str; filesNeeded := false; Tester.checktests [])
	     else if option = "-d"
	     then (dev:="true";
		   case str of
		       (* note that at this current time, no debugging information is printed for the basis.
			* In Analyze.sml we turn off D.debug when looking at the basis, the user should really
			* be able to toggle such an option, but for the moment this is simply disabled *)
		       "NO_COLOURS" => (D.colors := {black="",red="",green="",yellow="",blue="",purple="",cyan="",white=""};
					D.boldColors := {black="",red="",green="",yellow="",blue="",purple="",cyan="",white=""};
					D.underlineColors := {black="",red="",green="",yellow="",blue="",purple="",cyan="",white=""};
					D.backgroundColors := {black="",red="",green="",yellow="",blue="",purple="",cyan="",white=""};
					D.textReset := "")
		     | "EQUALITY_TYPES" => (D.debug := true; D.enableDebugFeature D.EQUALITY_TYPES)
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
		     | str  => (print ("Unrecognised debugging feature: "^str^"\n");
				raise Fail ("Unrecognised debugging feature: "^str));
		   outputFilesNeeded := false)
	     else if option = "-bo"
	     then basisoverloading:=str
	     else if option = "-t"
	     then tlim:=str
	     else if option = "-tab"
	     then tab:=str
	     else if option = "-sol"
	     then sol:=str
	     else if option = "-min"
	     then min:=str
	     else if option = "--print-env"
	     then bcs:=str
	     else if option = "--search-space"
	     then search:=str
	     else (TextIO.output (TextIO.stdErr, "Unknown argument fed as input");
		   raise Fail "Unknown argument fed as input"));
	      parse tail)
    in
	(* parse the arguments *)
	if strArgs = ""
	then (printHelp ();
	      raise Fail "No arguments specified.")
	else
	    (parse split;

	     (* check that the user specified an input file *)
	     if (!filein = "" andalso !filesNeeded = true)
	     then (print ("Error: No input file specified.");
		   raise Fail("No input file specified"))
	     else
		 if (!filein = "")
		 then OS.Process.success (* the user was checking tests *)
		 else (
		     (* display slices in the terminal by default *)
		     if (!filehtml^(!filexml)^(!filesml)^(!filelisp)^(!filejson) = ""
			 andalso !runtests = false andalso (!terminalSet = false orelse !terminalSlices <> NO_DISPLAY))
		     then if (!fileperl) = ""
			  then (fileperl := "/tmp/output.pl"; terminalSlices := NON_INTERACTIVE)
			  else (terminalSlices := NON_INTERACTIVE)
		     else ();

		     checkFileSuffix();

		     (* if a basis option hasn't been stated, then look for the SKALPEL_BASIS environment variable
		      * if we find one, use its contents
		      * if we do not, print a warning and default to -b 0 *)
		     if (!basisSpecified = false)
		     then case OS.Process.getEnv "SKALPEL_BASIS" of
			      NONE => (print "Error: Couldn't find basis file location in command line argument or environment variable (SKALPEL_BASIS).\n"; raise Fail("No basis option specified"))
			    | SOME file => (Tester.myfilebas:=file; filebas:=file; basop:="2")
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

fun smltesstr str = smlTesStrArgs ("--output true -b 1 " ^ str)

end
