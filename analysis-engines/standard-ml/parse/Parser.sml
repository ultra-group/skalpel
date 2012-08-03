(* Copyright 2002 2009 2010 2012 Heriot-Watt University
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
 *  o Authors:     Vincent Rahli, Chrisitan Haack
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        25 May 2010
 *  o File name:   Parser.sml
 *  o Description: Defines the structure Parser which has signature
 *      PARSER and is used to parse SML code and basis files.
 *)


structure Parser :> PARSER = struct

(* shorten the same of structures we use *)
structure A  = AstSML
structure R  = Reg
structure D  = Debug
structure F  = Infix
structure PD = ParseDefs
structure LD = LexDefs
structure LA = Label
structure EH = ErrorHandler

(* currently used to hold information about syntax errors *)
type messages      = (string * string * R.region list) list

(*exception ParsingError of int * messages*)

structure MLLrVals = MLLrValsFun(structure Token      = LrParser.Token)
structure MLLex    = MLLexFun   (structure Tokens     = MLLrVals.Tokens)
structure MLParser = JoinWithArg(structure ParserData = MLLrVals.ParserData
                                 structure Lex        = MLLex
                                 structure LrParser   = LrParser)

structure MLBLrVals = MLBLrValsFun(structure Token      = LrParser.Token)
structure MLBLex    = MLBLexFun   (structure Tokens     = MLBLrVals.Tokens)
structure MLBParser = JoinWithArg (structure ParserData = MLBLrVals.ParserData
                                   structure Lex        = MLBLex
                                   structure LrParser   = LrParser)

(* Turn that to true if compiling for the Online Demo.
 * This is so that the SKALPEL loading files are then ignored. *)
val webdemo = ref false

(* set/get methods for webdemo flag *)
fun setWebdemo b = webdemo := b
fun getWebdemo _ = !webdemo

(* sets a val for holding information about syntax errors *)
val messages : messages ref = ref []

(* add and print functions for the messages list *)
fun addmessage msg = messages := (!messages) @ [msg]
fun printmessages _ = app (fn (x, _, _) => print (x ^ "\n")) (!messages)

fun formatmessages messages lab =
    foldr (fn ((_, msg, reg), (lab, msgs)) => (LA.nextLabel lab, (msg, reg, lab, LA.nextLabel lab) :: msgs))
	  (lab, [])
	  messages

fun dummyparsing messages lab nasc =
    let val (m, xs) = formatmessages messages lab
	val progonelist = map (fn x => A.ProgOneParse x) xs
    in (A.Prog progonelist, m, nasc)
    end

(*fun parseMLB file stm n nasc =
      let val error = ref false
	  (* lexer argument: file name and start position *)
	  val lexarg = (file, ref (1, 1))
	  (* create a stream of lexical tokens *)
	  val lexstream = MLBParser.makeLexer
			      (fn n => TextIO.inputN(stm, n)) lexarg
	  (* initial parsing error messages *)
	  val _ = messages := []
	  (* a function for reporting syntax errors *)
	  fun syntaxError (msg, from, to) =
	      (fn extmsg => (error := true; addmessage extmsg))
		  (file ^ ":"  ^ R.printPos from ^ "-" ^ R.printPos to ^ ": " ^  msg,
		   msg,
		   [R.consReg from to])
	  (* build the AST, parameterized by its lowest node label *)
	  val (astFunction, _) =
	      LD.handleLex MLBParser.parse (15, lexstream, syntaxError, ())
	  (* label the nodes starting from n, the second parameter is the typevar substitution *)
	  val (ast, (m, asc)) = astFunction (n, nasc)
      in if !error
	 then dummyparsing (!messages) n nasc (*raise ParsingError (n, (!messages))*)
	 else (ast, m, asc)
      end
      handle LD.LexError x => dummyparsing [x] n nasc (*raise ParsingError (n, [x]) (* can we still get those? *)*)
	   | ParseError   => dummyparsing (!messages) n nasc (*raise ParsingError (n, (!messages)) (* can we still get those? *)*)
 *)

fun parse file inputStream nextNodeLabel nasc =
    let
	(* if the parser throws an exception, we can use this to see how far we got *)
	val posref = ref (1,1)
    in
	let val error = ref false
	    (* lexer argument: file name and start position *)
	    val lexarg = (file, posref)
	    (* create a stream of lexical tokens *)
	    val lexstream = MLParser.makeLexer
				(fn n => TextIO.inputN(inputStream, n)) lexarg
	    (* initial parsing error messages *)
	    val _ = messages := []
	    (* a function for reporting syntax errors *)
	    fun syntaxError (msg, from, to) =
		(fn extmsg => (error := true; addmessage extmsg))
		    (file ^ ":"  ^ R.printPos from ^ "-" ^ R.printPos to ^ ": " ^  msg,
		     msg,
		     [R.consReg from to])
	    (* build the AST, parameterized by its lowest node label *)
	    val (astFunction, testing) =
		LD.handleLex MLParser.parse (15, lexstream, syntaxError, ())
	    (* label the nodes starting from n, the second parameter is the typevar substitution *)
	    val (ast, (m, asc)) = astFunction (nextNodeLabel, nasc)
	    val (flag, str, reg) = PD.getErrorHandler ()
	in if !error
	   then dummyparsing (!messages) nextNodeLabel nasc
	   else if flag
	   then raise PD.ParseError (str, reg)
	   else (ast, m, asc)
	end
	handle LD.LexError x            => (print "LexError"; dummyparsing [x] nextNodeLabel nasc)
	     | PD.ParseError (msg, reg) => (print "ParseError"; dummyparsing [(msg, msg, reg)] nextNodeLabel nasc)
	     | _ => (print (". Region = ("^(Int.toString(#1(!posref)))^", "^(Int.toString(#2(!posref)))^")\n");
		     if (#2(!posref) = 1)
		     then (dummyparsing [(
			   "syntax error",
			   "our parser threw an exception, there is an error somewhere previously in the file (likely close) from the end of the highlighted region",
			   [{from= (#1(!posref)-1, #2(!posref)), (* highlight something for start of line regions *)
			     to= (#1(!posref)-1, #2(!posref)+999)}])]
					nextNodeLabel nasc)
		     else dummyparsing [(
			  "syntax error",
			  "our parser threw an exception, there is an error somewhere previously in the file (likely close) from the end of the highlighted region",
			  [{from= (#1(!posref), #2(!posref) -1), (* highlight something for end of line regions *)
			    to= !posref}])]
				       nextNodeLabel nasc)
    end

fun parseTes tesfile stin lab nasc =
    let val {dir, file} = OS.Path.splitDirFile tesfile
	(*val _ = D.printdebug2 ("dir:" ^ dir ^ "\n" ^ "file:" ^ file)*)
	fun getRegion st sub n =
	    let val (pref, _) = Substring.position sub (Substring.full st)
		val pref = Substring.string pref
		val c1   = String.size pref
		val c2   = c1 + String.size sub
		val pos1 = (n, c1 + 1)
		val pos2 = (n, c2)
	    in R.consReg pos1 pos2
	    end
	(*val stin = TextIO.openIn file*)
	fun gatherFiles n files lab =
	    case TextIO.inputLine stin of
		NONE => (lab, files)
	      | SOME line =>
		let fun getSmls [] l = (l, [])
		      | getSmls (x :: xs) l =
			let val next = LA.nextLabel l
			    val regs = getRegion line x n
			    (*val _    = D.printdebug2 ("(1) dir:" ^ dir ^ "\n" ^ "file:" ^ x)*)
			    val {dir = d, file = f} = OS.Path.splitDirFile x
			    val dir' = OS.Path.concat (dir, d)
			    val newfile = OS.Path.joinDirFile {dir = dir', file = f}
				handle OS.Path.InvalidArc => ""
			    (*val _       = D.printdebug2 ("newfile:" ^ newfile)*)
			    val af      = A.ProgOneFile (A.AFile (newfile, regs, l, next), next)
			    val (l', afs) = getSmls xs next
			in (l', af :: afs)
			end
		    (*if String.isSuffix ".sml" x orelse String.isSuffix ".tes" x
		      then
			  if OS.FileSys.access (x, [OS.FileSys.A_READ])
			  then x :: getSmls xs
			  else raise ParsingError (n, [("", "file does not exist", [getRegion line x n])])
		      else raise ParsingError (n, [("", "not a '.sml' or '.tes' file", [getRegion line x n])])*)

		    (* strips out spaces, tabs, and new line characters, puts the words from 'line' in a list *)
		    val toks = String.tokens (fn #" "  => true
					       | #"\t" => true
					       | #"\n" => true
					       | _     => false) line
		    val (l, fs) = getSmls toks lab
		in gatherFiles (n + 1) (files @ fs) l
		end
	val (nxt, afs) = gatherFiles 1 [] lab
    (*val _ = TextIO.closeIn stin*)
    in (A.Prog afs, nxt, nasc)
    end

(*fun printmessages messages = app (fn (x, _, _) => print (x ^ "\n")) messages*)

(*fun messagesToErrors messages =
      foldr
	  (fn ((_, msg, reg), (errl, slices)) =>
	      ((O.empty, O.empty, EK.Parsing msg) :: errl,
	       (A.ProgParse reg) :: slices))
	  ([], [])
	  messages*)

(*fun convertToFulls [] = []
    | convertToFulls (x :: xs) = (convertToFull x) :: (convertToFulls xs)*)

(*fun getFiles [] = []
    | getFiles (x :: xs) =
      let
	  val toks = String.tokens (fn #" "  => true
				     | #"\t" => true
				     | #"\n" => true
				     | _     => false) x
	  val f1 = case toks of
		       [x] => convertToFull x
		     | _   => EH.throw ""
	  val files =
	      if String.isSuffix ".tes" f1
	      then P.parseTes f1 n
	      else [f1]
      in files @ (getFiles xs)
      end*)

(*(* use file*)
 val useStringHead  = "(**SKALPEL-USE-FILE "
 val nUseStringHead = String.size useStringHead
 val useStringTail  = "*)\n"
 val nUseStringTail = String.size useStringTail
 fun minusNUse st   = (String.size st) - nUseStringHead - nUseStringTail
 fun isLineUse st   = String.isPrefix useStringHead st
		      andalso
		      String.isSuffix useStringTail st
 fun extractUse st  =
     let
	 val prepath  = String.substring (st, nUseStringHead, minusNUse st)
	 val prepaths = String.tokens (fn #" " => true | _ => false) prepath
     in case prepaths of
	    [x] =>
	    if OS.FileSys.access (x, [OS.FileSys.A_READ])
	    then USE x
	    else NCOM
	  | _ => NCOM
     end
     handle _ => NCOM
 (*******)

 (* clear basis *)
 val cbasStringHead  = "(**SKALPEL-CLEAR-BASIS"
 val nCbasStringHead = String.size cbasStringHead
 val cbasStringTail  = "*)\n"
 val nCbasStringTail = String.size cbasStringTail
 fun minusNCbas st   = (String.size st) - nCbasStringHead - nCbasStringTail
 fun isLineCbas st   = String.isPrefix cbasStringHead st
		       andalso
		       String.isSuffix cbasStringTail st
 fun extractCbas st  =
     let
	 val prepath  = String.substring (st, nCbasStringHead, minusNCbas st)
	 val prepaths = String.tokens (fn #" " => true | _ => false) prepath
     in case prepaths of
	    [] => CBAS
	  | _  => NCOM
     end
     handle _ => NCOM
 (*******)

 (* set basis *)
 val sbasStringHead  = "(**SKALPEL-SET-BASIS "
 val nSbasStringHead = String.size sbasStringHead
 val sbasStringTail  = "*)\n"
 val nSbasStringTail = String.size sbasStringTail
 fun minusNSbas st   = (String.size st) - nSbasStringHead - nSbasStringTail
 fun isLineSbas st   = String.isPrefix sbasStringHead st
		       andalso
		       String.isSuffix sbasStringTail st
 fun extractSbas st  =
     let
	 val prepath  = String.substring (st, nSbasStringHead, minusNSbas st)
	 val prepaths = String.tokens (fn #" " => true | _ => false) prepath
     in case prepaths of
	    [x] =>
	    if OS.FileSys.access (x, [OS.FileSys.A_READ])
	    then SBAS x
	    else NCOM
	  | _ => NCOM
     end
     handle _ => NCOM
 (*******)*)

(*fun extractOneCom line =
      if isLineUse line
      then extractUse line
      else
	  if isLineCbas line
	  then extractCbas line
	  else
	      if isLineSbas line
	      then extractSbas line
	      else NCOM*)

(*fun checkSmlTesComments file =
      let
	  fun extractSmlnjScriptPath stin =
	      case TextIO.inputLine stin of
		  NONE      => []
		| SOME line =>
		  case extractOneCom line of
		      NCOM => extractSmlnjScriptPath stin
		    | x => x :: (extractSmlnjScriptPath stin)
	  val stin = TextIO.openIn file
	  val path = extractSmlnjScriptPath stin
	  val _    = TextIO.closeIn stin
      in path
      end*)

(*fun getUsesSmlTesComment [] = []
    | getUsesSmlTesComment ((USE f) :: xs) = (f, true) :: (getUsesSmlTesComment xs)
    | getUsesSmlTesComment (_ :: xs) = getUsesSmlTesComment xs*)

(* gets rid of tabs and newline characters from input string st *)
(* we need to keep spaces, as we should support file paths with spaces *)
fun tokenizeSt st =
    String.tokens (fn #"\t" => true
		    | #"\n" => true
		    | _     => false) st


(* complains if the user specifies a file which is actually not a file *)
fun createOneErrFileAccess file fop =
    (NONE,
     [("",
       "not a file path: " ^ file,
       case fop of
	   NONE => []
	 | SOME r => [r])])

(* complains if file is already in list fnames below convertToFull function *)
fun createOneErrFileAlready file fop =
    (NONE,
     [("",
       "already used: " ^ file,
       case fop of
	   NONE => []
	 | SOME r => [r])])

(* echos more accurate errors for what the user has stated to be a file *)
fun convertToFull file fop fnames =
    case tokenizeSt file of
	[x] => (let
		    val _ = D.printDebugFeature D.PARSER D.PARSING (fn _ => "Getting full path of "^x^"...\n")
		    val f = OS.FileSys.fullPath x
		    val _ = D.printDebugFeature D.PARSER D.PARSING (fn _ => "Opening file with full path: "^x)
		in if OS.FileSys.isDir f orelse OS.FileSys.isLink f
		   then createOneErrFileAccess file fop
		   else if Tools.isin f fnames
		   then createOneErrFileAlready f fop
		   else (SOME f, [])
		end
		handle OS.SysErr (str,_) => (print ("OS.SysErr was raised with string: \""^str^"\" on file "^file^"\n");
					     createOneErrFileAccess file fop))
      | _ => createOneErrFileAccess file fop

(* returns a list of three-tuples with [(<file>, NONE, bas), ...] *)
fun newFilesToTreat files bas = map (fn f => (f, NONE, bas)) files

(* same as newFilesToTreat, but uses region information as an Option *)
fun newFilesRegToTreat xs = map (fn (f, r, b) => (f, SOME r, b)) xs

fun treatAFile file n nasc webdemo =
    let val instr = TextIO.openIn file
	val (prog, m, masc) =
	    if String.isSuffix ".tes" file
	    (* TES FILES *)
	    then parseTes file instr n nasc
	    (* OTHER FILES SUCH AS SML FILES *)
	    else (*if String.isSuffix ".mlb" file
		   then parseMLB file instr n nasc
		   else*) parse file instr n nasc
	val xs  = if webdemo
		  then []
		  else newFilesRegToTreat (A.extractFilesProg prog)
	val ret = (prog, xs, m, masc)
	val _   = TextIO.closeIn instr
    in ret
    end
    handle IO.Io {name, function, cause} =>
	   let val message = "cannot access file: " ^ file
	   in print (message ^ "\n"); raise EH.DeadBranch message
	   end

fun clearBasisFiles files false = files
  | clearBasisFiles [] true = []
  | clearBasisFiles ((prog, file, true, next) :: files) true =
    clearBasisFiles files true
  | clearBasisFiles ((prog, file, false, next) :: files) true =
    (prog, file, false, next) :: (clearBasisFiles files true)

(* consProgsSml handles the non basis files *)
fun consProgsSml [] n nasc fnames _ = ([], fnames, false, n, nasc)
  | consProgsSml ((file, opf, bas) :: files) n nasc fnames webdemo =
    (D.printDebugFeature D.PARSER D.PARSING (fn _ => "[Skalpel: parsing file: "^file^"]\n");
    case convertToFull file opf fnames of
	(NONE, xs) =>
	(* we carry on, so that if a file is not found eg in SKALPEL-USE-FIE, then the
	 * user gets an error about it *)
	let val (prog, m, masc) = dummyparsing xs n nasc
	    val (progs, fnames', clear, p, pasc) = consProgsSml files m masc fnames webdemo
	in (progs, fnames', clear, p, pasc)
	end
      | (SOME f1, _) =>
	let val (prog, xs, m, masc) = treatAFile f1 n nasc webdemo

	    val (progs1, fnames1, clear1, p, pasc) = consProgsSml xs m masc (fnames @ [f1]) webdemo
	    val clear = A.isClearBasisProg prog
	    val (progs2, fnames2, clear2, q, qasc) = consProgsSml files p pasc fnames1 webdemo
	    val progs3 = clearBasisFiles (progs1 @ [(prog, f1, bas, p)]) clear2
	in (progs3 @ progs2,
	    fnames2,
	    clear1 orelse clear2,
	    q,
	    qasc)
	end)
(*handle Io => EH.throw ("cannot access to file: " ^ file) (*consProgsSml files n asctv ascid*)*)

(*(* consProgsBas handles the basis files *)
 fun consProgsBas [] filesin n nasc = consProgsSml filesin n nasc false
   | consProgsBas (x :: xs) filesin n nasc =
     let
	 val file = convertToFull x NONE
	 val instr = TextIO.openIn file
	 val (ast, n1, nasc1) = P.parse file instr n nasc
	     handle P.ParsingError (_, y) => dummyparsing y n nasc (* n and x have to be the same *)
	 val _ = TextIO.closeIn instr
	 val prog = (ast, file, true, n1)
	 val (progs, parses, n2, nasc2) = consProgsBas xs filesin n1 nasc1 true
     in (prog :: progs, n2, nasc2)
     end
     handle Io => EH.throw ("cannot access to file: " ^ x) (*consProgsBas xs filesin n asctv ascid*)*)


(*fun consProgsSml filebas filesin n asctv ascid = consProgs' filesin n asctv ascid*)

(*fun consProgs' filebas 2 filesin n asctv ascid = consProgsBas [filebas] filesin n (asctv, ascid)
    | consProgs' _       _ filesin n asctv ascid = consProgsSml filesin n (asctv, ascid)*)

(* returns the first basisVal that it can find in a given list *)
fun checkABas [] = false
  | checkABas ((_, _, basisVal, _) :: xs) = basisVal orelse checkABas xs

fun consProgs filesbas filesin nextNodeLabel nasc basisVal webdemo =
    let val _ = F.reset ()
	val _ = LD.setQuotation false
	val _ = PD.resetErrorHandler ()
	val (progs, fnames, clear, m, masc) =
	    consProgsSml ((newFilesToTreat filesbas true) @
			  (newFilesToTreat filesin false))
			 nextNodeLabel
			 nasc
			 []
			 webdemo
	val basisVal' = case (checkABas progs, basisVal) of (false, 2) => 1 | _ => basisVal
	val _ = D.printDebugFeature D.PARSER D.PROGRAM_LABELLING (fn _ => Slicing.printSlice (A.Progs progs) true)
    in (A.Progs progs, m, masc, basisVal')
    end

    end



