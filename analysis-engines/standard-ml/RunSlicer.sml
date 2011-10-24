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
structure PP = Ppp

(* flag set if we are building the web demo binary *)
val webdemo = ref false
val data_tmp = PP.data_tmp

(* localise Tester functions *)
val error          = Tester.error
val myfilein       = Tester.myfilein
val myfilehtml     = Tester.myfilehtml
val myfilebas      = Tester.myfilebas

(* takes a boolean value b, if true then we are generating a binary for the web demo *)
fun setWebDemo b = webdemo := b

(* message relayed to the user if they try to use a function which has not yet been implemented *)
val messageTODO = "a feature has not been implemented yet"

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
	"" => finishedPerlM "slicer worked OK, sorry no debugging message"
      | _  => finishedPerlM ("slicer worked OK, " ^ msg)
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

(* the code below is commented as it can come in useful for special
 * configurations *)

(*fun preslicer' filebas filesin fileout nenv bprint bhtml =
    case Tester.slicergen filebas filesin nenv bprint of
	SOME (errl, parse as (_, _, ascid), bmin, times, envcss as (env, css), initlab) =>
	let val (nenv, filebas) = getFileBasAndNum nenv filebas
	    val name  = "dummy"
	    val dbg1  = Tester.debuggingXML  errl parse bmin times envcss initlab false name true nenv
	    val dbg2  = Tester.debuggingSML  errl parse bmin times envcss initlab false name true nenv
	    val dbg3  = Tester.debuggingLISP errl parse bmin times envcss initlab false name true nenv
	    val dbg4  = Tester.debuggingPERL errl parse bmin times envcss initlab false name true nenv
	    val dbg5  = Tester.debuggingHTML errl parse bmin times envcss initlab false name true nenv
	    val err   = Tester.buildError    errl parse bmin times envcss initlab false name true nenv
n	    val dbg5' = fn sep => (dbg5 fileout filebas bhtml sep; "")
	    val _     = Tester.assignError (err "")
            fun fev _ = (Env.printEnv env "") ^ "\n"
	in (dbg1, dbg2, dbg3, dbg4, dbg5', fev)
	end
      | NONE => getDebugProblem ()*)

(*fun preslicer filebas filesin fileout nenv bprint bhtml =
    let val (nenv, filebas) = getFileBasAndNum nenv filebas
    in preslicer' filebas filesin fileout nenv bprint bhtml
    end*)

(*fun pslicer filebas filesin fileout nenv bcs =
    let val (dbgxml, dbgsml, dbglisp, dbgperl, dbghtml, fev) =
	    preslicer filebas filesin fileout nenv true true
	val _ = case fileout of "" => "" | _ => dbghtml ""
    in print (Tester.testXML dbgxml);
       if bcs then print (fev ()) else ()
    end*)

(*fun slicer filebas filesin fileout nenv time =
    let val tmptm = Tester.gettimelimit ()
    in Tester.settimelimit (Int.toLarge time);
       pslicer filebas filesin fileout nenv false;
       Tester.settimelimit tmptm
    end*)

(*fun myslicer b n = pslicer myfilebas [myfilein] myfilehtml n b*)

(*fun myslicerp (_, [stb, stn]) =
    let val b = Option.valOf (Bool.fromString stb)
	    handle Option => raise EH.DeadBranch "the first argument should be a bool"
	val n = Option.valOf (Int.fromString stn)
	    handle Option => raise EH.DeadBranch "the second argument should be a int"
	val _ = myslicer b n
    in OS.Process.success
    end
  | myslicerp _ = OS.Process.failure*)


(*fun noslicer b n =
    let val tmptm = Tester.gettimelimit ()
    in Tester.settimelimit Tester.notimelimit;
       myslicer b n;
       Tester.settimelimit tmptm
    end*)

(*fun genslicer bcs n time =
    let val tmptm = Tester.gettimelimit ()
    in Tester.settimelimit (Int.toLarge time);
       myslicer bcs n;
       Tester.settimelimit tmptm
    end*)

(* adds a test to the database, with the number of the test, whether the slices
 * for the error are OK as a boolean, the name of the test, and the environment
 * integer as parameters *)
fun addtest testNum slicesCorrect testName env =
    if testNum < 1  (* check for an invalid test *)
    then ()
    else Tester.adderror myfilein testNum false slicesCorrect testName env

(* adds a test to the database in the same way as add test, but if the id
 * already exists it will be overwritten *)
fun replaceTest testNum slicesCorrect testName env =
    if testNum < 1 (* check for an invalid test number *)
    then ()
    else Tester.adderror myfilein testNum true slicesCorrect testName env

(* functions to delete and move tests *)
fun deltest numTest = Tester.delerror numTest
fun mvtest  oldNum newNum = Tester.mverror oldNum newNum false

(* tests is an integer list, give an empty list to run all test *)
(*fun checktests  _ = Tester.newchecktests ()*)
fun checktests tests      = Tester.checktests tests

(* run tests in integer list tests with time restriction time *)
fun runtests   tests timeLimit = Tester.runtests tests timeLimit

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
fun genOutputFile (bfile, ffile) suff counter fdebug str =
    if bfile
    then let val file  = ffile  ^ "-" ^ Int.toString counter ^ suff
	     val file' = file ^ ".tmp"
	     val stout = TextIO.openOut file'
	     val _     = TextIO.output (stout, fdebug str)
	     val _     = TextIO.closeOut stout
	     val _     = OS.FileSys.rename {old = file', new = file}
	 in () end
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
	 in () end
	 handle IO.Io {name, function, cause} => (print "cannot open or close one of the output file\n") (* cannot open or close *)
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
    print ("[TES:" ^
	   " found counter=" ^ Int.toString counter ^
	   " time="          ^ Int.toString time    ^
	   " errors="        ^ printErrors  errors  ^ "]\n")

(* calls the necessary functions to write the *.html, *.xml, *.sml etc
 * to the system *)
fun export nenv filebas (bhtml, fhtml) bfxml bfsml bflisp bfperl basisoverloading
	   errs parse bmin times cs initlab name st counter time =
    let val dbghtml  = Tester.debuggingHTML errs parse bmin times cs initlab true name true nenv basisoverloading Tester.removeBasisSlice
	val dbgxml   = Tester.debuggingXML  errs parse bmin times cs initlab true name true nenv basisoverloading
	val dbgsml   = Tester.debuggingSML  errs parse bmin times cs initlab true name true nenv basisoverloading
	val dbglisp  = Tester.debuggingLISP errs parse bmin times cs initlab true name true nenv basisoverloading
	val dbgperl  = Tester.debuggingPERL errs parse bmin times cs initlab true name true nenv basisoverloading
	val dbghtml' = fn sep => dbghtml (fhtml ^ "-" ^ Int.toString counter ^ ".html") filebas false sep
	val _ = if bhtml then dbghtml' st else ()
	val _ = genOutputFile bfxml  ".xml" counter dbgxml  st
	val _ = genOutputFile bfsml  ".sml" counter dbgsml  st
	val _ = genOutputFile bflisp ".el"  counter dbglisp st
	val _ = genOutputFile bfperl ".pl"  counter dbgperl st
	val _ = printFound counter errs time
    in ()
    end

(* the primary function in this file which calls the functions necessary to
 * run the slicer, including the main Testing.slicing function *)
fun commslicerp' filebas filesin filehtml filexml filesml filelisp fileperl nenv time tab sol min dev bcs searchspace basisoverloading =
    let val (nenv, filebas) = getFileBasAndNum nenv filebas

	(* check that the files have the correct extension*)
	val bfhtml = getBoolFile filehtml ".html"
	val bfxml  = getBoolFile filexml  ".xml"
	val bfsml  = getBoolFile filesml  ".sml"
	val bflisp = getBoolFile filelisp ".el"
	val bfperl = getBoolFile fileperl ".pl"

	(* write the files to the system *)
	val fout   = export nenv filebas bfhtml bfxml bfsml bflisp bfperl basisoverloading

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
			 else (run ())
			      handle EH.DeadBranch st => (print "the slicer encountered an impossible case\n"; (false, st))
				   | EH.TODO => (print ("the slicer failed because " ^ messageTODO ^ "\n"); (false, messageTODO))
				   | Fail st => (print ("Error: "^st^"\n"); (false, ""))
				   | _ => (print "the slicer failed for some reason\n"; (false, ""))

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
	val _ = genFinished bfperl ".pl"   fmperl
    in ()
    end

(* calls commslicerp', if we are not developing (¬dev) then the error is
 * handled, otherwise we leave the error so we can debug *)
fun slicerCheckDevMode filebas filesin filehtml filexml filesml filelisp fileperl nenv time tab sol min dev bcs searchspace basisoverloading =
    if dev
    then commslicerp' filebas filesin filehtml filexml filesml filelisp fileperl nenv time tab sol min dev bcs searchspace basisoverloading
    else commslicerp' filebas filesin filehtml filexml filesml filelisp fileperl nenv time tab sol min dev bcs searchspace basisoverloading
	 handle _ => (print "the slicer failed for some unknown reason\n"; ())

(* called by the emacs interface; sets no tab and uses the solution from sol in
 * utils/Solution.sml *)
fun commslicerp  filebas filesin filehtml filexml filesml filelisp fileperl nenv time basisoverloading =
    slicerCheckDevMode filebas filesin filehtml filexml filesml filelisp fileperl nenv time NONE (Tester.getsol ()) true false false 1 basisoverloading

(* the full version of the slicer function with all arguments *)
fun slicerFull [filebas, filein, filehtml, filexml, filesml, filelisp, fileperl, basop, tlim, tab, sol, min, dev, bcs, searchSpace, basisoverloading] =
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
		filebas [filein] filehtml filexml filesml filelisp fileperl
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

(* old invokation mechanism. Left in case we decide we want to use it in version 0.7 for any reason *)
fun slicerpp (_, [filebas, filein, filehtml, filexml, filesml, filelisp, fileperl, basop, tlim, tab, sol, min]) =
    (* the emacs user interface uses this case *)
    slicerFull   [filebas, filein, filehtml, filexml, filesml, filelisp, fileperl, basop, tlim, tab, sol, min, "",  "",  ""]
  | slicerpp (_, [filebas, filein, filehtml, filexml, filesml, filelisp, fileperl, basop, tlim]) =
    (* the command line uses this case *)
    slicerFull   [filebas, filein, filehtml, filexml, filesml, filelisp, fileperl, basop, tlim,   "", "", "",  "",  "",  ""]
  | slicerpp _ = slicerFull []

fun temp 1 (SOME nb) _ = Tester.vinnie nb
  | temp 2 _ (SOME file) =
    let val (b1, b2) = Tester.checkAstFile file
    in print (Bool.toString b1 ^ " " ^ Bool.toString b2 ^ "\n")
    end
  | temp 3 _ _ = Tester.checkAstDB ()
  | temp 4 _ _ = print (AstTest.genNewProgs ())
  | temp _ _ _ = ()


(* smltes will take in a record constructed by SlicerOptArgs and give
 * the output to slicerFull. The default arguments for this can be
 * found in the SlicerOptArgs file. *)
fun smltes ({fileBas     : string,
	     fileIn      : string,
	     fileHtml    : string,
	     fileXml     : string,
	     fileSml     : string,
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
    slicerFull [fileBas, fileIn, fileHtml, fileXml, fileSml, fileLisp, filePerl,
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
	val filein   = ref "test-prog.sml"
	val filehtml = ref ""
	val filexml  = ref ""
	val filesml  = ref ""
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
	val basisoverloading = ref "1"

	fun printHelp () =
	    print ("usage: slicer [option ...] FILE \n\
				    \    FILE file taken as input to be sliced\n\
				    \    -l <file> place output in <file> in lisp format\n\
				    \    -h <file> place output in <file> in html format\n\
				    \    -s <file> place output in <file> in sml format\n\
				    \    -x <file> place output in <file> in XML format\n\
				    \    -p <file> place output in <file> in perl format\n\
				    \    -t <timelimet> specify a numerical time limit\n\
				    \    -b <0 / 1 / 2 <file> > Set basis level as 0 (no basis), 1 (built in basis), 2 <file> (specify file as basis)\n\
				    \    -bo <0 | 1> If set to 1, hides basis slice in overloading errors\n\
				    \    -tab <tabwidth> define the tab width in user code regions\n\
				    \    -sol <solution> define solution to use (default 9)\n\
				    \    -min <true/false> if true, shows non-minimal errors\n\
				    \    --print-env <true/false> whether to print the environment\n\
				    \    --output <true/false> enable developer mode\n\
				    \    --search-space <1,2,3> Use search space 1 (lists), 2 (sets), or 3 (red black tree)\n\
				    \    --help Show this help text");

        (* split into tokens to allow for easy parsing *)
	val split = String.tokens Char.isSpace strArgs

	fun parse [] = ()
 	  | parse [option] =
	    if option = "--help" then
		printHelp ()
	    else
		filein:=option
	  (* have a 0/1/2 case for emacs ui *)
	  | parse ("-b"::"0"::file::tail) =
	    (filebas:=file; basop:="0"; parse tail)
	  | parse ("-b"::"1"::file::tail) =
	    (filebas:=file; basop:="1"; parse tail)
	  | parse ("-b"::"2"::file::tail) =
	    (filebas:=file; basop:="2"; parse tail)
	  | parse (option::str::tail)=
	     ((if option = "-f"
	     then filein:=str
	     else if option = "-h"
	     then filehtml:=str
	     else if option = "-x"
	     then filexml:=str
	     else if option = "-s"
	     then filesml:=str
	     else if option = "-l"
	     then filelisp:=str
	     else if option = "-p"
	     then fileperl:=str
	     else if option = "-b"
	     then basop:=str
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
	     else if option = "--output"
	     then dev:=str
	     else if option = "--print-env"
	     then bcs:=str
	     else if option = "--search-space"
	     then search:=str
	     else if option = "--check-tests"
	     then Tester.checktests []
	     else (print ("Unknown option: "^option); raise Fail "Unknown argument fed as input"));
	     parse tail)
    in
	(* parse the arguments *)
	if strArgs = ""
	then (printHelp ();
	      raise Fail "No arguments specified.")
	else
	(parse split;

	 (* check that the user specified an input file *)
	 if (!filein = "")
	 then (print ("Error: No input file specified.");
	       raise Fail("No input file specified"))
	 else ();

	 (* check that the user specified an output file *)
	 if (!filehtml^(!filexml)^(!filesml)^(!filelisp)^(!fileperl) = "")
	 then (print ("Error: No output files specified.");
	       raise Fail("No output files specified"))
	 else ();

	(* now all arguments are dereferenced and passed to slicerFull *)
	slicerFull [!filebas,
		    !filein,
		    !filehtml,
		    !filexml,
		    !filesml,
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
	OS.Process.success)
    end
    handle Fail _ => OS.Process.failure

fun smltesstr str = smlTesStrArgs ("--output true -b 1 " ^ str)

(* calculates what the free identifiers are in a given file filein with basis option b *)
(*fun getFreeIdentifiers filein b =
    let val _ = Ty.resetnexts ()
	val stmin = TextIO.openIn filein
	val (ast, m, ascid) = Parser.parse filein stmin Label.firstLab Id.emAssoc
	val _ = TextIO.closeIn stmin
	val proj = Ast.Progs [(ast, filein, false, m)]
	val (env, _, _) = Analyze.buildin (Analyze.generateConstraints proj 1) ascid b
	val _ = print (Env.printEnv env "")
	val _ = print (Id.printAssoc ascid)
	fun numericIdToIdentifierName numericId1 = Id.lookupId numericId1 ascid
	fun extractNames l = List.mapPartial numericIdToIdentifierName l
    in {freeUnknownIdentifiers = (extractNames o Id.toList o Env.dom o Env.getVids) env,
	maybeFreeConstructors  = (extractNames o Id.toList o Env.dom o Env.getVars) env,
	freeconstructors       = (extractNames o Id.toList o Env.dom o Env.getCons) env,
	freeTyNames            = (extractNames o Id.toList o Env.dom o Env.getTyps) env}
    end*)

end