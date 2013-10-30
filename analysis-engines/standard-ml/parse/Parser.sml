(* Copyright 2002 2009 2010 2012 2013 Heriot-Watt University
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
 *  o Authors:     Vincent Rahli, Chrisitan Haack, John Pirie
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        25 May 2010
 *  o File name:   Parser.sml
 *)

(** Used to parse SML code and basis files, opaquely constrained by refstruct{PARSER}. *)
structure Parser :> PARSER = struct

structure A  = AstSML
structure R  = Reg
structure D  = Debug
structure F  = Infix
structure PD = ParseDefs
structure LD = LexDefs
structure LA = Label
structure EH = ErrorHandler
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

(** Used to hold information about syntax errors. *)
type messages      = (string * string * R.region list) list

(** Set to true in the case we are compiling a binary for the webdemo, this is so that the Skalpel loading files are then ignored. *)
val webdemo = ref false

(** Set function for #webdemo. *)
fun setWebdemo b = webdemo := b
(** Accessor function for the #webdemo value. *)
fun getWebdemo _ = !webdemo

(** Holds information about syntax errors. *)
val messages : messages ref = ref []

(** Adds an error msg 'msg' to the #messages value. *)
fun addmessage msg = messages := (!messages) @ [msg]

(** Prints the messages we received during parsing. *)
fun printmessages _ = app (fn (x, _, _) => print (x ^ "\n")) (!messages)

(** Changes the format of the messages parameter.
 * The messages come in as a pair of a triple and a pair.
 * We format these messages as a list of pairs, with the label given in the parameter on the left hand side,
 * and on the right we have a quad-tuple built up from the messages argument. *)
fun formatmessages messages lab =
    foldr (fn ((_, msg, reg), (lab, msgs)) => (LA.nextLabel lab, (msg, reg, lab, LA.nextLabel lab) :: msgs))
	  (lab, [])
	  messages

(** Mapps #AstSML.ProgOneParse over the formatted messages. *)
fun dummyparsing messages lab nasc =
    let val (m, xs) = formatmessages messages lab
	val progonelist = map (fn x => A.ProgOneParse x) xs
    in (A.Prog progonelist, m, nasc)
    end

(** Parses an SML file.
 * \param Filename of the file we are parsing.
 * \param An input stream.
 * \param The next node label before execution.
 * \param An association list
 * \returns (abstract syntax tree, next node label after execution).
 *)
fun parse file inputStream nextNodeLabel nasc =
    let
	(** Stores our position in the file as we are parsing. If the parser explodes we can use this to see how far we got.
	 * Initially set to ref (1,1) indicating the start of the file. Probably we should have a better way to extract this
         * information from the parser? *)
	val posref = ref (1,1) in let

	(** A ref value which is set to true whenever we detect a parsing error. *)
	val error = ref false
	(** The lexer argument, a pair of the file name and start position. *)
	val lexarg = (file, posref)
	(** Creaes a stream of lexical tokens by calling makeLexer. *)
	val lexstream = MLParser.makeLexer (fn n => TextIO.inputN(inputStream, n)) lexarg

	(** Set the initial parsing error messages to empty. *)
	val _ = messages := []

	(** A function for reporting syntax errors, sets the #error flag to true, and adds the message given as an argument to the #messages list using #addmessage. *)
	fun syntaxError (msg, from, to) = (fn extmsg => (error := true; addmessage extmsg))
					      (file ^ ":"  ^ R.printPos from ^ "-" ^ R.printPos to ^ ": " ^  msg, msg, [R.consReg from to])

	(** Prints all tokens in a lexstream. *)
	fun printAllTokens lexstream =
	    let
		(** Prints a single token. *)
		fun printToken (LrParser.Token.TOKEN x) =
		    let
			(** Prints a region. *)
			fun printRegion (point1, point2) = ("(" ^ (Int.toString point1) ^ "," ^ (Int.toString point2) ^ ")")
	    		val term = #1 x
	    		val (p1,p2,p3) = #2 x
		    in
	    		(* print p1; *)
	    		print ((printRegion p2) ^ " " ^ (printRegion p3))
		    end

		(** Gets the top token and the remaining part of the stream as a pair. *)
		val (topToken,remainingStream) = MLBParser.Stream.get lexstream
		val _ = (print "New token: "; printToken topToken)
	    in
		printAllTokens remainingStream
	    end

	(** Parse the lexical stream of tokens and build up an abstract syntax tree. *)
	val (astFunction, testing) =
	    LD.handleLex MLParser.parse (15, lexstream, syntaxError, ())
	    (* handle _ => (TextIO.output (TextIO.stdErr, "Error: Unknown exception caught during parsing.\n"); *)
	    (* 				   print "----\nMessages: "; *)
	    (* 				   printmessages(); *)
	    (* 				   (* print "\n----\nToken(s): "; *) *)
	    (* 				   (* printAllTokens lexstream; *) *)
	    (* 				   (* printToken (#1 (MLBParser.Stream.get(lexstream))); *) *)
	    (* 				   (* printToken (#1(MLBParser.Stream.get(#2 (MLBParser.Stream.get(lexstream))))); *) *)
	    (* 				   print "\n----\n"; *)
	    (* 				   raise LrParser.ParseError) *)


	(** Label the nodes starting from n, the second parameter is the typevar substitution. *)
	val (ast, (m, asc)) = astFunction (nextNodeLabel, nasc)
	val (flag, str, reg) = PD.getErrorHandler ()
    in
	if !error
	then dummyparsing (!messages) nextNodeLabel nasc
	else if flag
	then raise PD.ParseError (str, reg)
	else (ast, m, asc)
    end
    handle LD.LexError x            => (TextIO.output (TextIO.stdErr, "Error: LexError\n") ; dummyparsing [x] nextNodeLabel nasc)
    	 | PD.ParseError (msg, reg) => (TextIO.output (TextIO.stdErr, "Error: ParseError\n"); dummyparsing [(msg, msg, reg)] nextNodeLabel nasc)
    	 | _ => (TextIO.output (TextIO.stdErr, "Error: Unknown catch-all exception raised.\n");
    		 if (#2(!posref) = 1)
    	 	 then (dummyparsing [("syntax error",
    	 			      "our parser threw an exception, there is an error somewhere in the file (likely close) prior to or inside of the highlighted region",
    	 			      [{from= (#1(!posref)-1, #2(!posref)), (* highlight something for start of line regions *)
    	 				to= (#1(!posref)-1, #2(!posref)+999)}])]
    	 			    nextNodeLabel nasc)
    	 	 else dummyparsing [("syntax error",
    	 			     "our parser threw an exception, there is an error somewhere in the file (likely close) prior to or inside the highlighted region",
    	 			     [{from= (#1(!posref), #2(!posref) -1), (* highlight something for line edn regions *)
    	 			       to= !posref}])]
    	 			   nextNodeLabel nasc)
    end

(** For parsing .tes control files. *)
fun parseTes tesfile stin lab nasc =
    let val {dir, file} = OS.Path.splitDirFile tesfile

	(** For getting regions of the .tes file *)
	fun getRegion st sub n =
	    let val (pref, _) = Substring.position sub (Substring.full st)
		val pref = Substring.string pref
		val c1   = String.size pref
		val c2   = c1 + String.size sub
		val pos1 = (n, c1 + 1)
		val pos2 = (n, c2)
	    in R.consReg pos1 pos2
	    end

	(** Gets a list of all the SML files that we have been given with their full path. *)
	fun gatherFiles n files lab =
	    case TextIO.inputLine stin of
		NONE => (lab, files)
	      | SOME line =>
		let
		    (** Helper function for #gatherFiles. *)
		    fun getSmls [] l = (l, [])
		      | getSmls (x :: xs) l =
			let
			    (** The next available label. *)
			    val next = LA.nextLabel l
			    (** Regions of the SML file lines. *)
			    val regs = getRegion line x n
			    val {dir = d, file = f} = OS.Path.splitDirFile x
			    val dir' = OS.Path.concat (dir, d)
			    (** The full path of the file to process. *)
			    val newfile = OS.Path.joinDirFile {dir = dir', file = f}
				handle OS.Path.InvalidArc => ""
			    val af      = A.ProgOneFile (A.AFile (newfile, regs, l, next), next)
			    val (l', afs) = getSmls xs next
			in (l', af :: afs)
			end

		    (** Strips out spaces, tabs, and new line characters, puts the words from 'line' in a list *)
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

(** Gets rid of tabs and newline characters from input string st (we need to keep spaces, as we should support file paths with spaces). *)
fun tokenizeSt st =
    String.tokens (fn #"\t" => true
		    | #"\n" => true
		    | _     => false) st


(** Complains if the user specifies a file which does not exist. *)
fun createOneErrFileAccess file fop =
    (NONE,
     [("",
       "not a file path: " ^ file,
       case fop of
	   NONE => []
	 | SOME r => [r])])

(** Complains if file is already in list #fnames below convertToFull function. *)
fun createOneErrFileAlready file fop =
    (NONE,
     [("",
       "already used: " ^ file,
       case fop of
	   NONE => []
	 | SOME r => [r])])

(** Echos more accurate errors for what the user has stated to be a file. *)
fun convertToFull file fop fnames =
    case tokenizeSt file of
	[x] => (let
		    val _ = D.printDebug D.PARSER D.PARSING (fn _ => "Getting full path of "^x^"...\n")
		    (** Full path of the file we are gathering. *)
		    val f = OS.FileSys.fullPath x
		    val _ = D.printDebug D.PARSER D.PARSING (fn _ => "Opening file with full path: "^x)
		in if OS.FileSys.isDir f orelse OS.FileSys.isLink f
		   then createOneErrFileAccess file fop
		   else if Tools.isin f fnames
		   then createOneErrFileAlready f fop
		   else (SOME f, [])
		end
		 handle OS.SysErr (str,_) => (TextIO.output (TextIO.stdErr, "OS.SysErr was raised with string: \""^str^"\" on file "^file^"\n");
					     createOneErrFileAccess file fop))
      | _ => createOneErrFileAccess file fop

fun newFilesToTreat files isBasis = map (fn f => (f, NONE, isBasis)) files

(** Same as #newFilesToTreat, but uses region information as an Option. *)
fun newFilesRegToTreat xs = map (fn (f, r, b) => (f, SOME r, b)) xs

(** Deals with parsing of a file, if it's a tes file calls #parseTes otherwise calls #parse. *)
fun treatAFile file n nasc webdemo =
    let val instr = TextIO.openIn file
	val (prog, m, masc) =
	    if String.isSuffix ".tes" file
	    (* TES FILES *)
	    then parseTes file instr n nasc
	    (* OTHER FILES SUCH AS SML FILES *)
	    else parse file instr n nasc
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

(** Removes the basis file from the file list given as input. *)
fun clearBasisFiles files false = files
  | clearBasisFiles [] true = []
  | clearBasisFiles ((prog, file, true, next) :: files) true =
    clearBasisFiles files true
  | clearBasisFiles ((prog, file, false, next) :: files) true =
    (prog, file, false, next) :: (clearBasisFiles files true)

(** Handles the non basis files. *)
fun consProgsSml [] n nasc fnames _ = ([], fnames, false, n, nasc)
  | consProgsSml ((file, opf, bas) :: files) n nasc fnames webdemo =
    (D.printDebug D.PARSER D.PARSING (fn _ => "[Skalpel: parsing file: "^file^"]\n");
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

(** Returns the first basisVal that it can find in a given list. *)
fun checkABas [] = false
  | checkABas ((_, _, basisVal, _) :: xs) = basisVal orelse checkABas xs

(** Builds all programs together. *)
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

	(** A list of all files that don't come from the basis. *)
	val nonBasisProgs =
	    List.foldl (fn (x,y) =>
			   case x of                          (* x comes from newFilesToTreat - a three tuple *)
			       (_,_,true,_) =>  y               (* true means this file is the basis *)
			     | (_,_,false,_) => x::y) [] progs  (* false means this file is NOT the basis *)
	(** A list of all basis files (should be just one?) *)
	val basisProgs =
	    List.foldl (fn (x,y) =>
			   case x of                            (* x comes from newFilesToTreat - a three tuple *)
			       (_,_,true,_) =>  x::y            (* true means this file is the basis *)
			     | (_,_,false,_) => y) [] progs     (* false means this file is NOT the basis *)

	(* print out labelled basis and non-basis files as per the user requested *)

	(* A temporary value holding the value of #Debug.debugBasis,
	 * used to hide debugging statements during basis processing. *)
	val skalpelDebugValue = !(D.debug)
	val _ = D.debug := !(D.debugBasis)
	val _ = D.printDebug D.PARSER D.BASIS_LABELLING (fn _ => ("\n"^(Slicing.printSlice (A.Progs basisProgs) true)^"\n"))
	val _ = D.debug := skalpelDebugValue
	val _ = D.printDebug D.PARSER D.PROGRAM_LABELLING (fn _ => ("\n"^(Slicing.printSlice (A.Progs nonBasisProgs) true)^"\n"))
    in (A.Progs progs, m, masc, basisVal')
    end

    end
