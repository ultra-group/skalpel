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
 *  o Authors:     Vincent Rahli
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        25 May 2010
 *  o File name:   Html.sml
 *  o Description: Defines the functor Html which has signature HTML
 *      and takes an argument of signature TAG.  The tags can be HTML
 *      tags as well as other tags.
 *)


functor Html (TA : TAG) :> HTML =
struct

(* shorten the name of structures *)
structure R   = Reg
structure S   = Slicing
structure O   = OrdSet
structure D   = Debug
structure I   = Id
structure CD  = LongId
structure EK  = ErrorKind
structure ER  = ExtReg
structure EH  = ErrorHandler
structure ERR = Error

(* three different css style sheets that we can set *)
val stylecss0 = "../lib/style.css"
val stylecss1 = "../lib/style1.css"
val stylecss2 = "../lib/style2.css"
val stylecss =  ref stylecss2

(* sets a style sheet (changes the ref value of stylecss) *)
fun setstylecss 1 = stylecss := stylecss1
  | setstylecss 2 = stylecss := stylecss2
  | setstylecss _ = ()

(* space and new line characters specified in the structure passed to this functor, TA *)
val space = TA.getSpace ()
val nline = TA.getBreakTag ()

(* 1 tab = 8 spaces *)
val mytab = space ^ space ^
	    space ^ space ^
	    space ^ space ^
	    space ^ space

val transfun = fn #"\t" => mytab
		| #" "  => space
		| #"\n" => ""
		| #"\\" => "\\"
		| #"\"" => "\""
		| x     => Char.toString x

(* returns part of a line - range (from, lgth) in string 'line' *)
fun getPartLine line from lgth n =
    let val st = "ERROR:\n"   ^
		 "  line:   " ^ line              ^ "\n" ^
		 "  from:   " ^ Int.toString from ^ "\n" ^
		 "  length: " ^ Int.toString lgth ^ "\n" ^
		 "  who:    " ^ Int.toString n    ^ "\n"
    in String.substring (line, from, lgth)
       handle Subscript => (print st; raise EH.DeadBranch "")
    end

fun transform stin stout regl bas (curNb, curLine) =
    let (* add or remove clW depending on if we want the overlaping of slices to show up *)
	fun treatLine line [] n =
	    let val st  = getPartLine line n ((String.size line) - n) 11
		val st' = String.translate transfun st
	    in st'
	    end
	  | treatLine line ((r as (ER.L ({from = (l1, c1), to = (l2, c2)}, col, w))) :: rs) n =
	    let val st1 = String.translate transfun (getPartLine line n (c1 - n - 1) 12)
		val st2 = String.translate transfun (getPartLine line (c1 - 1) (c2 - c1 + 1) 13)
	    in st1
	       ^ TA.getBTag col w 0
	       ^ st2
	       ^ TA.getETag ()
	       ^ treatLine line rs c2
	    end
	  | treatLine line ((r as (ER.H ({from = (l1, c1), to = (l2, c2)}, col, w))) :: rs) n =
	    let val st = String.translate transfun (getPartLine line n (c1 - n - 1) 14)
		val _  = if c1 = c2 then () else raise EH.DeadBranch ""
	    in st
	       ^ TA.getBTag col w 1
	       ^ TA.getETag ()
	       ^ treatLine line rs (c2 - 1)
	    end
	  | treatLine line ((r as (ER.N ({from = (l1, c1), to = (l2, c2)}, col, w, tl))) :: rs) n =
	    let val st1 = String.translate transfun (getPartLine line n (c1 - n - 1) 15)
		val st2 = treatLine (getPartLine line 0 c2 16) tl (c1 - 1) (* TODO: Do something else here *)
	    in st1
	       ^ TA.getBTag col w 2
	       ^ st2
	       ^ TA.getETag ()
	       ^ treatLine line rs c2
	    end
	fun getFromStin n =
	    let val lineop = if n = curNb
			     then SOME curLine
			     else TextIO.inputLine stin
	    in (case lineop of
		    NONE      => ()
		  | SOME line =>
		    let val locregs = ER.getExtRegLine n regl
			val st = treatLine line locregs 0 ^ nline
			val _ = if bas andalso List.null locregs
				then ()
				else TextIO.output (stout, st)
		    in getFromStin (n + 1)
		    end)
	    end
    in getFromStin (if curNb = 0 then 1 else curNb)
    end


fun readAndPrintLines stin stout curLine topLineOp curCol line bas =
    case topLineOp of
	NONE => (if bas andalso curCol = 0
		 then ()
		 else TextIO.output (stout, String.translate transfun line ^ nline);
		 case TextIO.inputLine stin of
		     NONE => (0, "")
		   | SOME line' => readAndPrintLines stin stout (curLine + 1) topLineOp 0 line' bas)
      | SOME topLine =>
	if curLine = topLine
	then (curCol, line)
	else if curLine > topLine
	then raise EH.DeadBranch ""
	else (if bas andalso curCol = 0
	      then ()
	      else TextIO.output (stout, String.translate transfun line ^ nline);
	      case TextIO.inputLine stin of
		  NONE => (0, " ") (* We do that because of test 311.
				    * It should only be for EOF syntax errors. *) (*raise EH.DeadBranch (EH.msg (Int.toString curLine ^ " " ^ Int.toString topLine))*)
		| SOME line' => readAndPrintLines stin stout (curLine + 1) topLineOp 0 line' bas)

fun transform2 _ _ [] _ (curLine, curCol) line = (curLine, curCol, line)
  | transform2 stin stout
	       (regl as ((ER.L (r as {from = (l1, c1), to = (l2, c2)}, col, w)) :: rs))
	       bas (curLine, curCol) line =
    if l1 > curLine
    then let val (curCol1, line1) = readAndPrintLines stin stout curLine (SOME l1) curCol line bas
	 in transform2 stin stout regl bas (l1, curCol1) line1
	 end
    else if l1 < curLine
    then raise EH.DeadBranch ""
    else (* l1 = curLine *)
	let val st1   = String.translate transfun (getPartLine line 0 (c1 - curCol - 1) 21)
	    val _     = TextIO.output (stout, st1)
	    val _     = TextIO.output (stout, TA.getBTag col w 0)
	    val line1 = getPartLine line (c1 - curCol - 1) (String.size line - c1 + curCol + 1) 22
	    val (curCol2, line2) = readAndPrintLines stin stout l1 (SOME l2) c1 line1 bas
	    val st2   = String.translate transfun (getPartLine line2 0 (c2 - curCol2 + 1) 23)
	    val _     = TextIO.output (stout, st2)
	    val _     = TextIO.output (stout, TA.getETag ())
	    val line3 = getPartLine line2 (c2 - curCol2 + 1) ((String.size line2) - c2 + curCol2 - 1) 24
	in transform2 stin stout rs bas (l2, c2) line3
	end
  | transform2 stin stout
	       (regl as ((ER.H ({from = (l1, c1), to = (l2, c2)}, col, w)) :: rs))
	       bas (curLine, curCol) line =
    if l1 > curLine
    then let val (curCol1, line1) = readAndPrintLines stin stout curLine (SOME l1) curCol line bas
	 in transform2 stin stout regl bas (l1, curCol1) line1
	 end
    else if l1 < curLine
    then raise EH.DeadBranch ""
    else (* l1 = curLine *)
	let val _     = if c1 = c2 andalso l1 = l2 then () else raise EH.DeadBranch ""
	    val st1   = String.translate transfun (getPartLine line 0 (c1 - curCol - 1) 31)
	    val _     = TextIO.output (stout, st1)
	    val _     = TextIO.output (stout, TA.getBTag col w 1)
	    val line1 = getPartLine line (c1 - curCol - 1) (String.size line - c1 + curCol + 1) 32
	    (*val _     = D.printdebug2 line1*)
	    val (curCol2, line2) = readAndPrintLines stin stout l1 (SOME l2) c1 line1 bas
	    (*val _     = D.printdebug2 line2*)
	    val st2   = String.translate transfun (getPartLine line2 0 (c2 - curCol2) 33)
	    (*val _     = D.printdebug2 st2*)
	    val _     = TextIO.output (stout, st2)
	    val _     = TextIO.output (stout, TA.getETag ())
	    val line3 = getPartLine line2 (c2 - curCol2) ((String.size line2) - c2 + curCol2) 34
	(*val _     = D.printdebug2 line3*)
	in transform2 stin stout rs bas (l2, c2 - 1) line3
	end
  | transform2 stin stout
	       (regl as ((ER.N ({from = (l1, c1), to = (l2, c2)}, col, w, eregs)) :: rs))
	       bas (curLine, curCol) line =
    if l1 > curLine
    then let val (curCol1, line1) = readAndPrintLines stin stout curLine (SOME l1) curCol line bas
	 in transform2 stin stout regl bas (l1, curCol1) line1
	 end
    else if l1 < curLine
    then raise EH.DeadBranch ""
    else (* l1 = curLine *)
	let val st1   = String.translate transfun (getPartLine line 0 (c1 - curCol - 1) 41)
	    val _     = TextIO.output (stout, st1)
	    val _     = TextIO.output (stout, TA.getBTag col w 2)
	    val line1 = getPartLine line (c1 - curCol - 1) (String.size line - c1 + curCol + 1) 42
	    (*val _     = D.printdebug2 line1*)
	    val (curLine2, curCol2, line2) = transform2 stin stout eregs bas (l1, c1 - 1) line1
	    val (curCol3, line3) = readAndPrintLines stin stout curLine2 (SOME l2) (curCol2 + 1) line2 bas
	    val st2   = String.translate transfun (getPartLine line3 0 (c2 - curCol3 + 1) 43)
	    val _     = TextIO.output (stout, st2)
	    val _     = TextIO.output (stout, TA.getETag ())
	    val line4 = getPartLine line3 (c2 - curCol3 + 1) ((String.size line3) - c2 + curCol3 - 1) 44
	(*val _     = D.printdebug2 (line4)*)
	in transform2 stin stout rs bas (l2, c2) line4
	end

fun transform2' stin stout eregs bas _ =
    case TextIO.inputLine stin of
	NONE => () (* regions should be empty *)
      | SOME line =>
	let val (curLine, curCol, line') =
		transform2 stin stout eregs bas (1, 0) line
	    val _ = readAndPrintLines stin stout curLine NONE curCol line' bas
	in ()
	end


(*fun findSt xs asc = #1 (O.foldr
			    (fn (x, (y, b)) =>
				((O.printeltst x asc)
				 ^ (if b then ", " else "")
				 ^ y, true))
			    ("", false)
			    xs)

fun longAsmp (nl, v) asc =
    (foldr (fn (x, y) => (O.printeltst x asc) ^ "." ^ y) "" nl)
    ^ (O.printeltst v asc)*)

fun getRestrictions _ (EK.MultiOcc   (SOME x)) asc = raise EH.DeadBranch "No multiocc error on long ids" (*toStringCds (CD.singleton x) asc ^ " is not datatype/exception constructor"*)
  | getRestrictions _ (EK.ValVarApp  (SOME x)) asc = raise EH.DeadBranch "No applied value variable in pattern error on long ids" (*toStringCds (CD.singleton x) asc ^ " is not datatype/exception constructor"*)
  | getRestrictions _ (EK.ConIsVar   (SOME x)) asc = raise EH.DeadBranch "No Constructor is a value variable error on long ids" (*toStringCds (CD.singleton x) asc ^ " is not datatype/exception constructor"*)
  (*| getRestrictions _ (EK.MultiOcc   NONE) _       = "none"
  | getRestrictions _ (EK.ValVarApp  NONE) _       = "none"
  | getRestrictions _ (EK.ConIsVar   NONE) _       = "none"*)
  | getRestrictions asmp _ asc                     = if CD.isEmpty asmp
						     then "none"
						     else
							 if CD.isSingle asmp
							 then CD.toStringListSt asmp asc ^ " is not a datatype/exception constructor"
							 else CD.toStringListSt asmp asc ^ " are not datatype/exception constructors"

(* gathers the lengths of each line and stres them in a list of tuples (lineNumber, length) *)
fun linesLengths file =
    let val stm = TextIO.openIn file
	fun count n =
	    case TextIO.inputLine stm of
		NONE      => []
	      | SOME line => (n, size line) :: (count (n+1))
	val ret = count 1
	val _   = TextIO.closeIn stm
    in ret
    end

(* returns the number of characters in line n *)
fun getSize [] _ = NONE
  | getSize ((n : int, s) :: xs) m =
    if n = m
    then SOME s
    else getSize xs m

fun lineByLineReg reg assoc =
    let val p1  = R.getFrom reg
	val p2  = R.getTo   reg
	val l1  = R.getPosLine p1
	val l2  = R.getPosLine p2
    in if l1 = l2
       then [reg]
       else if l1 > l2
       then raise EH.DeadBranch ""
       else let val col   = Option.valOf (getSize assoc l1)
		    handle Option => raise EH.DeadBranch ""
		val reg'  = R.consReg p1 (l1, col)
		val reg'' = R.consReg (l1+1, 1) p2
		val regs  = lineByLineReg reg'' assoc
	    in reg' :: regs
	    end
    end

(* This should not be needed anymore. *)
fun lineByLine [] _ = []
  | lineByLine ((ER.L (reg, col, w)) :: xs) assoc =
    let val regs = lineByLineReg reg assoc
    in (map (fn x => ER.L (x, col, w)) regs) @ (lineByLine xs assoc)
    end
  | lineByLine (x :: xs) assoc = x :: lineByLine xs assoc

(* translates internal representation of a slice to something HTML can
   actually understand. (Special text characters like \t removed and
   replaced by spaces etc *)
fun sliceToHtml sl =
    String.translate
	(fn #"\t"   => mytab
	  | #"\n"   => nline
	  | #" "    => space
	  | #"\""   => "\"" (* Why do we have to do that? *)
	  | #"\227" => "\227" (* sequence ldots and rdots *)
	  | #"\128" => "\128"
	  | #"\152" => "\152"
	  | #"\153" => "\153"
	  | #"\154" => "\154"
	  | #"\155" => "\155"
	  | #"\226" => "\226" (* old ldots and rdots *)
	  | #"\167" => "\167"
	  | #"\188" => "\188"
	  | #"\189" => "\189"
	  | #"\159" => "\159" (* new ldots and rdots *)
	  | #"\168" => "\168"
	  | #"\169" => "\169"
	  | x => Char.toString x) sl

(* checks that a file supplied to be the basis is likely to be valid *)
fun checkIsBas filebas (file : string) =
    case filebas of
	NONE => false
      | SOME f => f = file

(* outputs string st to an output stream *)
fun printTagged stout btag st =
    if btag
    then TextIO.output (stout, st)
    else ()

fun treatOneRegs stout filebas btag (file, regs) basisoverloading =
    let
	(* we transform the regions because a region coming from a parsing errors
         * are not always on one line
	 * maybe we should do that out of this structure but in Tester *)
	val regs' = lineByLine regs (linesLengths file)
	val stin  = TextIO.openIn file
	val f = #file (OS.Path.splitDirFile file)
    in
	(if (f = "basis.sml" andalso basisoverloading = 0)
	 then ()
	 else (printTagged stout btag ("<tr>" ^ "<td class=\"file\">" ^ "<code>" ^ f ^ "</code>" ^ "</td>" ^ "</tr>\n");
	       printTagged stout btag "<tr><td class=\"boxedF\"><code>\n";
	       transform2' stin stout regs' (checkIsBas filebas file) (0, "");
	       printTagged stout btag "</code></td></tr>\n");
	 TextIO.closeIn stin)
    end

fun treatRegs stout filebas btag xs basisoverloading =
    app (fn x => treatOneRegs stout filebas btag x basisoverloading) xs

(* get the classification of the error - such as free identifier *)
fun getKind err ascid btag =
    TA.getTitle (#2 (EK.printErrKind (ERR.getK err) ascid))

(* returns the context dependencies for a given error *)
fun getDependencies err ascid btag =
    TA.getTitle "Context dependencies" ^
    " " ^
    getRestrictions (ERR.getD err) (ERR.getK err) ascid

(* prints out a slice of a program *)
fun getSlice err btag ascid basisoverloading removeBasisSlice =
    let
	val sl = S.printSlice (ERR.getS err) true
    in
	TA.getTitle "Slice" ^
	nline ^
	(if btag then "<code>"    else "") ^
	" " ^
	(if String.isSubstring "OV" (#1(EK.printErrKind (ERR.getK err) ascid)) andalso basisoverloading = 0
	 then sliceToHtml (removeBasisSlice sl)
	 else sliceToHtml sl) ^
	(if btag then "</code>"   else "")
    end

(* forms the enumerated output html files (<name>-n.html) *)
fun transformOneErr stout filebas fileout err ascid btag basisoverloading removeBasisSlice =
    let (*val _       = D.printdebug2 (I.printAssoc ascid)*)
	(*val _       = D.printdebug2 (ERR.printOneXmlErr err "" false)*)
	val stkind  = getKind         err ascid btag
	(*val _       = D.printdebug2 (stkind)*)
	val stdeps  = getDependencies err ascid btag
	val stslice = getSlice        err       btag ascid basisoverloading removeBasisSlice
	val _ = printTagged
		    stout btag
		    ("<table class=\"slice\">" ^
		     "<tr>\n" ^
		     "<td height=\"100%\">" ^
		     "<input type=\"button\" value=\"<\" style=\"height:100%;width:5px;\">" ^
		     "</td>\n" ^
		     "<td>\n")
	val _ = TextIO.output (stout, stkind ^ "\n")
	val _ = printTagged stout btag "<br/>\n"
	val _ = printTagged
		    stout btag
		    ("<table>" ^
		     "<tr>" ^
		     "<td class=\"boxed\">\n" ^
		     "<table>\n")
	val _ = treatRegs stout filebas btag (ERR.getR err) basisoverloading
	val _ = printTagged
		    stout btag
		    ("</table>\n" ^
		     "</td>" ^
		     "</tr>" ^
		     "</table>\n")
	val _ = TextIO.output (stout, stdeps ^ "\n")
	val _ = printTagged stout btag "<br/>\n"
	val _ = TextIO.output (stout, stslice)
	val _ = printTagged
		    stout btag
		    ("</td>\n" ^
		     "<td height=\"100%\">" ^
		     "<input type=\"button\" value=\">\" style=\"height:100%;width:5px;\">" ^
		     "</td>\n" ^
		     "</tr>" ^
		     "</table>\n" ^
		     "<br/>" ^
		     "<br/>")
	val _ = TextIO.output (stout, "\n\n")
    in ()
    end

(* transforms an error slice into an HTML-representataion *)
fun transformErrSl fileout filebas errs ascid _ bhead btag basisoverloading removeBasisSlice =
    let val stout = TextIO.openOut fileout
	val code  = "<meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\" />"
	val style = "<link href=\"" ^ !stylecss ^  "\" rel=\"stylesheet\" type=\"text/css\" />"
	val errb  = "<h3>SML-TES found "
	val erre  = "</h3>\n<br/>\n\n"
	val _ = if bhead
		then TextIO.output (stout, "<head>\n" ^ style ^ "\n" ^ code ^ "\n</head>\n<body>\n\n")
		else ()
	(* main body of html representation starts here *)
	val _ = if bhead
		then case List.length errs of
			 0 => TextIO.output (stout, errb ^ "0 errors." ^ erre)
		       | 1 => TextIO.output (stout, errb ^ "1 error:" ^ erre)
		       | n => TextIO.output (stout, errb ^ Int.toString n ^ " errors:" ^ erre)
		else ()
	fun f xs = app (fn x => transformOneErr
				    stout
				    filebas
				    fileout
				    x
				    ascid
				    btag
				    basisoverloading
				    removeBasisSlice) xs
	val _ = f errs
	val _ = if bhead
		then TextIO.output (stout, "</body>\n")
		else ()
	val _ = TextIO.closeOut stout
    in ()
    end

end
