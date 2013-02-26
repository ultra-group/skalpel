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
 *  o File name:   ExtReg.sml
 *  o Description: Contains the extended regions which are regions
 *      extended with colours.  The file defines the structure ExtReg
 *      which has the signature EXTREG.
 *)


structure ExtReg :> EXTREG =
struct

(* shorten the names of structures *)
structure A  = AstSML
structure R  = Reg
structure L  = Label
structure D  = Debug
structure EK = ErrorKind
structure EH = ErrorHandler

(* used only in the testoverlap function. Why?
 * These are tags corresping to L, H, and N.  We can then easily
 * test the equality between node kinds. *)
datatype constr  = CL | CH | CN

(* the colours we use for slices *)
(* These should be changed to meaningful names:
 *   - Red    -> DEFAULT_ERROR_LOCATION
 *   - Blue   -> END_POINT_1
 *   - Purple -> END_POINT_2
 *   - Green  -> COMMON_RECORD_FIELD
 *   - Orange -> SPECIAL_ERROR_LOCATION
 *       (used to indicate that a location is part of an error
 *        because it forces the status of an identifier, or to
 *        indicate that locations forces the monomorphism of an
 *        id because of the value poly restriction)
 *   - Yellow -> FREE_IDENTIFIER
 *)
datatype color   = Red | Blue | Purple | Green | Orange | Yellow

(* weight...?
 * Weigths are use when combining slices to indicates the number
 * of slices a location is part of.   The weight will be higher
 * if it occurs in mode slices.  We can then highlight these
 * locations with, e.g., a darker color.  This can be useful
 * when we get multiple slices for a single programming error
 * because we know that the programming location has to be in
 * each and every slices, therefore it has to be in the dark red.
 *)
type weight      = int

(* declare a special type for a file *)
type file        = string

(* a tree of regions
 * - L stands for leaf, a usual error location.
 * - H stands for head, it's used to indicate that a no-space participate
 *   in an error, meaning that we've got no space at the left of a regions
 *   but if there was a white space then it would participate in the error.
 * - N stands for node, it's used for boxes.
 *)
datatype treeReg = L of R.region * color * weight
		 | H of R.region * color * weight
		 | N of R.region * color * weight * treeReg list

(* holds a full tree of regions for a file *)
type fileReg     = file * treeReg list

datatype highlighting = LEAF         (* background colour *)
		      | BOX          (* underline colour *)
                      | LEAF_IN_BOX  (* background and underline *)

(* holds the regions spanning accross multiple files in a list *)
type regs        = fileReg list

(* returns the first character of a colour from the colour datatype *)
fun printColor Red    = "R"
  | printColor Blue   = "B"
  | printColor Purple = "P"
  | printColor Green  = "G"
  | printColor Orange = "O"
  | printColor Yellow = "Y"

(* boolean value is to denote whether the colour should be boxed *)
fun printBashColor Red    LEAF = #red (!D.backgroundColors)
  | printBashColor Blue   LEAF = #blue (!D.backgroundColors)
  | printBashColor Purple LEAF = #cyan (!D.backgroundColors)
  | printBashColor Green  LEAF = #green (!D.backgroundColors)
  | printBashColor Orange LEAF = #purple (!D.backgroundColors)
  | printBashColor Yellow LEAF = #yellow (!D.backgroundColors)
  | printBashColor Red    BOX = #red (!D.underlineColors)
  | printBashColor Blue   BOX = #blue (!D.underlineColors)
  | printBashColor Purple BOX = #cyan (!D.underlineColors)
  | printBashColor Green  BOX = #green (!D.underlineColors)
  | printBashColor Orange BOX = #purple (!D.underlineColors)
  | printBashColor Yellow BOX = #yellow (!D.underlineColors)
  | printBashColor Red    LEAF_IN_BOX = #red (!D.leafInBoxColors)
  | printBashColor Blue   LEAF_IN_BOX = #blue (!D.leafInBoxColors)
  | printBashColor Purple LEAF_IN_BOX = #cyan (!D.leafInBoxColors)
  | printBashColor Green  LEAF_IN_BOX = #green (!D.leafInBoxColors)
  | printBashColor Orange LEAF_IN_BOX = #purple (!D.leafInBoxColors)
  | printBashColor Yellow LEAF_IN_BOX = #yellow (!D.leafInBoxColors)


(* returns the colour (same as printColor) but with the structure name prefixed *)
fun printSmlColor Blue   = "ExtReg.Blue"
  | printSmlColor Red    = "ExtReg.Red"
  | printSmlColor Purple = "ExtReg.Purple"
  | printSmlColor Green  = "ExtReg.Green"
  | printSmlColor Orange = "ExtReg.Orange"
  | printSmlColor Yellow = "ExtReg.Yellow"

(* returns regions for any constructor of treeReg *)
fun getReg (L (r, _, _))    = r
  | getReg (H (r, _, _))    = r
  | getReg (N (r, _, _, _)) = r

(*fun printExtReg (L (r, c)) ind     = ind ^ " - " ^ R.printReg r ^ " - " ^ printColor c
  | printExtReg (H (r, c)) ind     = ind ^ " + " ^ R.printReg r ^ " - " ^ printColor c
  | printExtReg (N (r, c, tl)) ind = ind ^ " - " ^ R.printReg r ^ " - " ^ printColor c ^ " .." ^ printExtRegList tl (ind ^ "  ")
and printExtRegList [] _           = ""
  | printExtRegList (t :: tl) ind  = "\n" ^ printExtReg t ind ^ printExtRegList tl ind

fun printExtRegSp x = printExtRegList x ""*)

(* prints the extended regions for L, H, and N constructuors of treeReg *)
fun printExtReg (L (r, c, w))     = "L(" ^ R.printReg r   ^
				    ","  ^ printColor c   ^
				    ","  ^ Int.toString w ^ ")"
  | printExtReg (H (r, c, w))     = "H(" ^ R.printReg r   ^
				    ","  ^ printColor c   ^
				    ","  ^ Int.toString w ^ ")"
  | printExtReg (N (r, c, w, tl)) = "N(" ^ R.printReg r   ^
				    ","  ^ printColor c   ^
				    ","  ^ Int.toString w ^
				    ",[" ^ printExtRegList tl ^ "])"

(* prints extended regions that have been put into a list by calling printExtReg *)
and printExtRegList []         = ""
  | printExtRegList [t]        = printExtReg t
  | printExtRegList (t :: tl)  = printExtReg t ^ "," ^ printExtRegList tl

(* prints extended regions for files in a list of tuples (file, regions) *)
fun printExtRegs [] = ""
  | printExtRegs [(file, regs)] =
    "(\"" ^ file ^ "\",[" ^ printExtRegList regs ^ "])"
  | printExtRegs ((file, regs) :: xs) =
    "(\"" ^ file ^ "\",[" ^ printExtRegList regs ^ "])" ^
    "," ^ printExtRegs xs

fun printOneRegs regs = "[" ^ printExtRegs regs ^ "]"

(* prints SML style extended regions (same as print printExtReg, but prints structure prefix *)
fun printSmlExtReg (L (r, c, w))     =
    "ExtReg.L(" ^ R.printSmlReg r ^ "," ^ printSmlColor c ^ "," ^ Int.toString w ^ ")"
  | printSmlExtReg (H (r, c, w))     =
    "ExtReg.H(" ^ R.printSmlReg r ^ "," ^ printSmlColor c ^ "," ^ Int.toString w ^ ")"
  | printSmlExtReg (N (r, c, w, tl)) =
    "ExtReg.N(" ^ R.printSmlReg r ^ "," ^ printSmlColor c ^ "," ^ Int.toString w ^ ",[" ^ printSmlExtRegList tl ^ "])"

(* for the same form as above, but when they are in a list *)
and printSmlExtRegList []         = ""
  | printSmlExtRegList [t]        = printSmlExtReg t
  | printSmlExtRegList (t :: tl)  = printSmlExtReg t ^ "," ^ printSmlExtRegList tl

(* prints SML style extended regions (same as print printExtReg, but prints structure prefix *)
fun printJsonExtReg (L (r, c, w))     =
    "{\"nodeType\": \"leaf\", " ^ R.printJsonReg r ^ ", \"color\": \"" ^ printColor c ^ "\", \"weight\": " ^ Int.toString w ^ "}"
  | printJsonExtReg (H (r, c, w))     =
    "{\"nodeType\": \"head\", " ^ R.printJsonReg r ^ ", \"color\": \"" ^ printColor c ^ "\", \"weight\": " ^ Int.toString w ^ "}"
  | printJsonExtReg (N (r, c, w, tl)) =
    "{\"nodeType\": \"node\", " ^ R.printJsonReg r ^ ", \"color\": \"" ^ printColor c ^ "\", \"weight\": " ^ Int.toString w ^ ", \"regionList\": [" ^ printJsonExtRegList tl ^ "]}"

and printJsonExtRegList []         = ""
  | printJsonExtRegList [t]        = printJsonExtReg t
  | printJsonExtRegList (t :: tl)  = printJsonExtReg t ^ "," ^ printJsonExtRegList tl

(* prints extended regions SML style for files in a list of tuples (file, regions) *)
fun printSmlExtRegs [] = ""
  | printSmlExtRegs [(file, regs)] =
    "(\"" ^ file ^ "\",[" ^ printSmlExtRegList regs ^ "])"
  | printSmlExtRegs ((file, regs) :: xs) =
    "(\"" ^ file ^ "\",[" ^ printSmlExtRegList regs ^ "])" ^
    "," ^ printSmlExtRegs xs

(* prints extended regions to be used with the JSON format *)
fun printJsonExtRegs [] = ""
  | printJsonExtRegs [(file, regs)] =
    "{\"fileName\": \"" ^ file ^ "\", \"regionList\": [" ^ printJsonExtRegList regs ^ "]}"
  | printJsonExtRegs ((file, regs) :: xs) =
    "{\"fileName\": \"" ^ file ^ "\", \"regionList\": [" ^ printJsonExtRegList regs ^ "]}, "^(printJsonExtRegs xs)

fun printLispExtReg (L (r, c, w))     = "(L " ^ R.printLispReg r ^ " " ^ printColor c ^ ")"
  | printLispExtReg (H (r, c, w))     = "(H " ^ R.printLispReg r ^ " " ^ printColor c ^ ")"
  | printLispExtReg (N (r, c, w, tl)) = "(N " ^ R.printLispReg r ^ " " ^ printColor c ^ " (" ^ printLispExtRegList tl ^ "))"
and printLispExtRegList []         = ""
  | printLispExtRegList [t]        = printLispExtReg t
  | printLispExtRegList (t :: tl)  = printLispExtReg t ^ " " ^ printLispExtRegList tl

fun printLispExtRegs [] = ""
  | printLispExtRegs [(file, regs)] =
    "(\"" ^ file ^ "\" . (" ^ printLispExtRegList regs ^ "))"
  | printLispExtRegs ((file, regs) :: xs) =
    "(\"" ^ file ^ "\" . (" ^ printLispExtRegList regs ^ "))" ^
    " " ^ printLispExtRegs xs

fun printPerlGenExtReg node r c regs =
    let val p1 = R.getFrom r
	val p2 = R.getTo r
	val l1 = R.getPosLine p1
	val c1 = R.getPosCol  p1
	val l2 = R.getPosLine p2
	val c2 = R.getPosCol  p2
    in "{node => "   ^ node            ^
       ", l1 => "    ^ Int.toString l1 ^
       ", c1 => "    ^ Int.toString c1 ^
       ", l2 => "    ^ Int.toString l2 ^
       ", c2 => "    ^ Int.toString c2 ^
       ", col => \"" ^ printColor c    ^ "\"" ^
       ", reg => "   ^ regs            ^ "}"
    end

fun printPerlExtReg (L (r, c, w))     = printPerlGenExtReg "\"L\"" r c "[]"
  | printPerlExtReg (H (r, c, w))     = printPerlGenExtReg "\"H\"" r c "[]"
  | printPerlExtReg (N (r, c, w, tl)) = printPerlGenExtReg "\"N\"" r c ("[" ^ printPerlExtRegList tl ^ "]")
and printPerlExtRegList []         = ""
  | printPerlExtRegList [t]        = printPerlExtReg t
  | printPerlExtRegList (t :: tl)  = printPerlExtReg t ^ ", " ^ printPerlExtRegList tl

fun printBashGenExtReg r c regs explodedLines previousLineNum =
    let val p1 = R.getFrom r
	val p2 = R.getTo r
	val l1 = R.getPosLine p1
	val c1 = R.getPosCol  p1
	val l2 = R.getPosLine p2
	val c2 = R.getPosCol  p2
    in
	 printCodeFragment (l1, c1) (l2, c2) explodedLines c previousLineNum
    end

(* and printSubRegions (currentLine, currentColumn) []        explodedLines moreRegions previousLineNum = NONE *)
(*   | printSubRegions (currentLine, currentColumn) (node::t) explodedLines moreRegions previousLineNum = *)
(*     case node of *)
(* 	(L (r, c, w)) => *)
(* 	let *)
(* 	    val endRegionLine = R.getPosLine(R.getTo r) *)
(* 	    val endRegionCol  = R.getPosCol(R.getTo r) *)
(* 	    val moreRegionsLine = (case moreRegions of NONE => ~1 | SOME(x,y) => x) *)
(* 	    val moreRegionsCol = (case moreRegions of NONE => ~1 | SOME(x,y) => y) *)
(* 	    val _  = print ("*********** " ^ (Int.toString (moreRegionsCol - endRegionCol)) ^ "*********************") *)
(* 	in *)
(* 	    (D.printDebugFeature D.BLANK D.TEMP (fn _ => "Checking (" ^ (Int.toString currentLine) ^ "," ^ (Int.toString(currentColumn)) ^ ") against current node region (" ^ (Int.toString (R.getPosLine(R.getFrom r))) ^ "," ^ (Int.toString (R.getPosCol(R.getFrom r))) ^ "). End region is (" ^ (Int.toString endRegionLine) ^ "," ^ (Int.toString endRegionCol) ^ "), and more regions is (" ^ (Int.toString moreRegionsLine) ^ "," ^ (Int.toString moreRegionsCol) ^ ")\n"); *)
(* 	     if R.getPosLine(R.getFrom r) = currentLine andalso R.getPosCol(R.getFrom r) = currentColumn *)
(* 	     then (printBashGenExtReg r (printBashColor c LEAF_IN_BOX) "[]" explodedLines (if moreRegionsCol - endRegionCol > 1 then moreRegions else (SOME(~1,~1)))  previousLineNum []; *)
(* 		   SOME(R.getPosLine(R.getTo r), R.getPosCol(R.getTo r))) *)
(* 	     else printSubRegions (currentLine, currentColumn) t explodedLines moreRegions previousLineNum) *)
(* 	end *)
(*       | (H (r, c, w)) => *)
(* 	(D.printDebugFeature D.BLANK D.TEMP (fn _ => "Checking (" ^ (Int.toString currentLine) ^ "," ^ (Int.toString(currentColumn)) ^ ") against current node region (" ^ (Int.toString (R.getPosLine(R.getFrom r))) ^ "," ^ (Int.toString (R.getPosCol(R.getFrom r))) ^ ")\n"); *)
(* 	if R.getPosLine(R.getFrom r) = currentLine andalso R.getPosCol(R.getFrom r) = currentColumn *)
(* 	then (printBashGenExtReg r (printBashColor c LEAF_IN_BOX) "[]" explodedLines (SOME(~1,~1)) previousLineNum []; *)
(* 	      SOME(R.getPosLine(R.getTo r), R.getPosCol(R.getTo r))) *)
(* 	else printSubRegions (currentLine, currentColumn) t explodedLines moreRegions previousLineNum) *)
(*       | (N (r, c, w, nodeRegionList)) => *)
(* 	(D.printDebugFeature D.BLANK D.TEMP (fn _ => "Checking (" ^ (Int.toString currentLine) ^ "," ^ (Int.toString(currentColumn)) ^ ") against current node region (" ^ (Int.toString (R.getPosLine(R.getFrom r))) ^ "," ^ (Int.toString (R.getPosCol(R.getFrom r))) ^ ")\n"); *)
(* 	if R.getPosLine(R.getFrom r) = currentLine andalso R.getPosCol(R.getFrom r) = currentColumn *)
(* 	then (printBashGenExtReg r (printBashColor c BOX) "[]" explodedLines (SOME(~1,~1)) previousLineNum nodeRegionList; *)
(* 	      SOME(R.getPosLine(R.getTo r), R.getPosCol(R.getTo r))) *)
(* 	else printSubRegions (currentLine, currentColumn) t explodedLines moreRegions previousLineNum) *)

and printSkipped explodedLines currentLine currentColumn previousLineNum color =
    let
	(* we need to subtract 1 because of list indexing *)
	val currentLineList = List.nth (explodedLines, currentLine-1)
	(* we need to subtract 1 because of list indexing *)
	val previousLineNum =  if currentLine = previousLineNum
			       then currentLine
			       else if (currentLine - previousLineNum > 1)
			      (* we add "..." to indicate skipped lines and print all the text that was previous in the line *)
			       then if (currentLine - previousLineNum = 2 andalso currentLine <> 1)
				    (* if there is only one skipped, display that number *)
				   then (print ((!D.textReset) ^ Int.toString(previousLineNum+1) ^ ":\t\t ...\n");
					 print ((!D.textReset) ^ Int.toString(currentLine) ^ ":\t\t" ^  (String.implode(List.take (currentLineList, currentColumn-1))) ^ color);
					 currentLine)
				    (* if more than one line was skipped, give the region of skipped line numbers *)
				    else (if currentLine <> 1
				          then print ((!D.textReset) ^ (if previousLineNum = ~1 then "1" else Int.toString(previousLineNum+1)) ^ "-" ^ Int.toString(currentLine-1) ^ ":\t\t ...\n")
				          else ();
					  print ((!D.textReset) ^ Int.toString(currentLine) ^ ":\t\t" ^  (String.implode(List.take (currentLineList, currentColumn-1))) ^ color);
					  currentLine)
			       (* there are no skipped lines so we just print all the text that was previous in the line *)
			       else if (currentLine - previousLineNum = 1)
			       then (print ((!D.textReset) ^ Int.toString(currentLine) ^ ":\t\t" ^  (String.implode(List.take (currentLineList, currentColumn-1))) ^ color); currentLine)
			       else previousLineNum 				      (* otherwise don't print anything extra *)
	val currentColumnChar = List.nth (currentLineList, currentColumn-1)
    in
	(currentLineList, previousLineNum, currentColumnChar)
    end


and printCodeFragment (l1, c1) (l2, c2) explodedLines color previousLineNum =
    let
	val _ = D.printDebugFeature D.BLANK D.TEMP (fn _ => "printing code fragment " ^ (Int.toString c1) ^ " to " ^ (Int.toString c2))
	val _ = if l1 <> previousLineNum then (printSkipped explodedLines l1 c1 previousLineNum color; ()) else ()

	fun printCodeFragmentHelper (l2, c2) explodedLines (currentLine, currentColumn) =
	    let
		val _ = ()
	    in
		 if currentColumn > c2
		 then ()
		 else (case Char.toString (List.nth (List.nth (explodedLines, currentLine-1), currentColumn-1)) of
			   "\\n" => print ((!D.textReset) ^ "\n") |
			   "\\t" => print ((!D.textReset) ^ "\t") |
			   x => print x;
		       printCodeFragmentHelper (l2, c2) explodedLines (currentLine, currentColumn+1))
	    end
    in
	(print color; printCodeFragmentHelper (l2, c2) explodedLines (l1, c1))
    end

(* if the end of the first region (r1) is on the same line as as the start
 * of the second region (r2), then we return SOME of the start position of
 * the second region, otherwise we return NONE *)
fun getNextLineRegs r1 r2 =
    let
	(* first region *)
	val r1_p1 = R.getFrom r1
	val r1_p2 = R.getTo r1
	val r1_l1 = R.getPosLine r1_p1
	val r1_c1 = R.getPosCol  r1_p1
	val r1_l2 = R.getPosLine r1_p2
	val r1_c2 = R.getPosCol  r1_p2

	(* second region *)
	val r2_p1 = R.getFrom r2
	val r2_p2 = R.getTo r2
	val r2_l1 = R.getPosLine r2_p1
	val r2_c1 = R.getPosCol  r2_p1
	val r2_l2 = R.getPosLine r2_p2
	val r2_c2 = R.getPosCol  r2_p2
    in
	(* if the ending of the first region is on the same line as the start of the second region *)
	if r1_l2 = r2_l1         (* if there are more regions to print on this line     *)
	then SOME (r2_l1, r2_c1) (* retun the (startLine, startChar) of second region   *)
	else NONE                (* there are no more regions on this line, return NONE *)
    end

fun printBashTreeRegNode _ [] explodedLines = raise EH.DeadBranch "Tried to print command line sub-region node which doesn't exist!"
  | printBashTreeRegNode (currentLine, currentColumn) (node::t) explodedLines =
    let
	val fromLine  = R.getPosLine(R.getFrom(getReg node))
	val fromCol   = R.getPosCol(R.getFrom(getReg node))
	val newCurrentLine   = R.getPosLine(R.getTo(getReg node))
	val newCurrentColumn = R.getPosCol(R.getTo(getReg node)) + 1
    in
	if currentLine = fromLine andalso currentColumn = fromCol
	then (case node of
		  (* we don't care about the previous line number for these nodes, so we set it to the current line *)
		  (L (r, c, w)) => printBashGenExtReg r (printBashColor c LEAF_IN_BOX) "[]" explodedLines (R.getPosLine(R.getFrom r))
		| (H (r, c, w)) => printBashGenExtReg r (printBashColor c LEAF_IN_BOX) "[]" explodedLines (R.getPosLine(R.getFrom r))
		| (N (r, c, w, nodeRegionList)) => printBashNode (R.getPosLine(R.getFrom r), R.getPosCol(R.getFrom r))    (* from          *)
								 (R.getPosLine(R.getTo r), R.getPosCol(R.getTo r))        (* to            *)
								 c                                                        (* color *)
								 nodeRegionList                                           (* extra regions *)
								 explodedLines;                                           (* file info     *)
	      (newCurrentLine, newCurrentColumn))
	else printBashTreeRegNode (currentLine, currentColumn) t explodedLines
    end

and inNodeRegionList (currentLine, currentColumn) [] = false
  | inNodeRegionList (currentLine, currentColumn) (node::t) =
    if currentLine = R.getPosLine(R.getFrom (getReg node)) andalso currentColumn = R.getPosCol(R.getFrom (getReg node))
    then true
    else inNodeRegionList (currentLine, currentColumn) t

and printBashNode (currentLine, currentColumn) (endLine, endColumn) color nodeRegionList explodedLines =
    if inNodeRegionList (currentLine, currentColumn) nodeRegionList
    then
	let
	    val (newCurrentLine, newCurrentColumn) = printBashTreeRegNode (currentLine, currentColumn) nodeRegionList explodedLines
	in
	    if newCurrentColumn > endColumn
	    then ()
	    else printBashNode (newCurrentLine, newCurrentColumn) (endLine, endColumn) color nodeRegionList explodedLines
	end
    else
	 (printCodeFragment (currentLine, currentColumn) (currentLine, currentColumn) explodedLines (printBashColor color BOX) currentLine;
	  if currentColumn = endColumn
	  then ()
	  else printBashNode (currentLine, currentColumn+1) (endLine, endColumn) color nodeRegionList explodedLines)

(* case L - we have a leaf. We use normal highlighting for this
 * case H - We make a box for this.
 * case N - we have a node. *)
fun printBashExtReg node explodedLines moreRegions previousLineNum =
    let
	val previousLineNum = if previousLineNum <> R.getPosLine(R.getFrom (getReg node)) andalso R.getPosCol (R.getFrom (getReg node)) <> 1
			      then (print ((!D.textReset));
				    printCodeFragment (R.getPosLine(R.getFrom (getReg node)), 1) (R.getPosLine(R.getFrom (getReg node)), R.getPosCol(R.getFrom (getReg node))-1)  explodedLines "" previousLineNum;
				    R.getPosLine(R.getFrom (getReg node)))
			      else previousLineNum
	val region = getReg node
    in
	(case node of
	     (L (r, c, w)) => printBashGenExtReg r (printBashColor c LEAF) "[]" explodedLines previousLineNum
	   | (H (r, c, w)) => printBashGenExtReg r (printBashColor c LEAF) "[]" explodedLines previousLineNum
	   | (N (r, c, w, nodeRegionList)) => printBashNode (R.getPosLine(R.getFrom r), R.getPosCol(R.getFrom r))  (* from          *)
							    (R.getPosLine(R.getTo r), R.getPosCol(R.getTo r))      (* to            *)
							    c                                                      (* color         *)
							    nodeRegionList                                         (* extra regions *)
							    explodedLines;                                         (* file info     *)

	case moreRegions of
	    (* if there are no more regions on this current line *)
	    NONE => (print (!D.textReset);
	 	     print (String.implode(List.drop(List.nth (explodedLines, R.getPosLine(R.getTo region)-1), R.getPosCol(R.getTo region)))))
	  | SOME (nextLine, nextColumn) =>
	    (print (!D.textReset);
	     (* we don't want to use previousLineNum here, we've already dealt with that in printBashGenExtReg *)
	     printCodeFragment (R.getPosLine(R.getTo region), R.getPosCol(R.getTo region)+1) (nextLine, nextColumn-1) explodedLines "" (R.getPosLine(R.getTo region))))
end

and printBashExtRegList []        explodedLines previousLineNum = ()
  (* give NONE as there cannot be any more regions to print at the end of this region *)
  | printBashExtRegList [t]       explodedLines previousLineNum = printBashExtReg t explodedLines NONE previousLineNum
  | printBashExtRegList (t :: tl) explodedLines previousLineNum = (printBashExtReg t explodedLines (getNextLineRegs (getReg t) (getReg(List.hd tl))) previousLineNum;
								   printBashExtRegList tl explodedLines (R.getPosLine (R.getTo (getReg t))))

(* getExplodedLines - returns an list of lists, each element is a line of
 * the input stream, which each element of that being a character from the line *)
fun getExplodedLines stream =
    case (TextIO.inputLine stream) of
	SOME line => (String.explode line)::(getExplodedLines stream)
	| NONE => []

fun printPerlExtRegs [] = ""
  | printPerlExtRegs [(file, regs)] =
	"{file => \"" ^ file ^ "\"" ^
	", regs => [" ^ printPerlExtRegList regs ^ "]}"
  | printPerlExtRegs ((file, regs) :: xs)  =
     "{file => \"" ^ file ^ "\"" ^
     ", regs => [" ^ printPerlExtRegList regs ^ "]}" ^
     ", " ^ printPerlExtRegs xs

fun printBashExtRegs [] = ()
  | printBashExtRegs [(file, regs)] =
    let
	val codeStream = TextIO.openIn file
	val explodedLines = getExplodedLines codeStream
    in
	(D.printReset( (#yellow (!D.underlineColors)) ^ file ^ ":\n");
	 (* we give ~1 as the previous line number so that we will always get the beginning of the first line *)
	 printBashExtRegList regs explodedLines ~1)
    end
  | printBashExtRegs ((file, regs) :: xs)  =
    let
	val codeStream = TextIO.openIn file
	val explodedLines = getExplodedLines codeStream
    in
	(D.printReset( (#yellow (!D.underlineColors)) ^ file ^ ":\n");
	 (* we give ~1 as the previous line number so that we will always get the beginning of the first line *)
	 printBashExtRegList regs explodedLines ~1;
	 printBashExtRegs xs)
    end

(*fun printColRegs []        = ""
  | printColRegs (x :: xs) = "<JBW val="  ^ (printExtRegPPSp x) ^ ">\n" ^ (printColRegs xs)*)

(* determines if two colors are the same *)
fun sameCols (c1 : color) (c2 : color) = c1 = c2

(* gathers regions which have colours other than red, yellow and orange *)
(* We should use gatherNonRedRegs instead of checkSameExtRegs to test the database *)
(* treeReg list -> Reg.reiong list *)
fun gatherNonRedRegs [] = []
  | gatherNonRedRegs ((L (_, Red,    _)) :: xs) = gatherNonRedRegs xs
  | gatherNonRedRegs ((L (_, Yellow, _)) :: xs) = gatherNonRedRegs xs
  | gatherNonRedRegs ((L (_, Orange, _)) :: xs) = gatherNonRedRegs xs
  | gatherNonRedRegs ((L (r, _,      _)) :: xs) = r :: (gatherNonRedRegs xs)
  | gatherNonRedRegs ((H (_, Red,    _)) :: xs) = gatherNonRedRegs xs
  | gatherNonRedRegs ((H (_, Yellow, _)) :: xs) = gatherNonRedRegs xs
  | gatherNonRedRegs ((H (_, Orange, _)) :: xs) = gatherNonRedRegs xs
  | gatherNonRedRegs ((H (r, _,      _)) :: xs) = r :: (gatherNonRedRegs xs)
  | gatherNonRedRegs ((N (_, Red,    _, eregs)) :: xs) = (gatherNonRedRegs eregs) @ (gatherNonRedRegs xs)
  | gatherNonRedRegs ((N (_, Yellow, _, eregs)) :: xs) = (gatherNonRedRegs eregs) @ (gatherNonRedRegs xs)
  | gatherNonRedRegs ((N (_, Orange, _, eregs)) :: xs) = (gatherNonRedRegs eregs) @ (gatherNonRedRegs xs)
  | gatherNonRedRegs ((N (r, _,      _, eregs)) :: xs) = r :: (gatherNonRedRegs eregs) @ (gatherNonRedRegs xs)

(* determines if regs1 and regs2 have equal regions (disragarding
   regions coloured red, yellow, and orange) *)
fun checkSimExtRegs regs1 regs2 =
    let val xs1 = gatherNonRedRegs regs1
	val xs2 = gatherNonRedRegs regs2
    in R.areEqualRegs xs1 xs2
    end

(* checks if two files have equal regions (disregarding
   regions coloured red, yellow and orange) *)
fun checkSimRegs [] [] = true
  | checkSimRegs [] _  = false
  | checkSimRegs _ []  = false
  | checkSimRegs ((f1 : file, eregs1) :: xs1) ((f2 : file, eregs2) :: xs2) =
    checkSimExtRegs eregs1 eregs2 andalso checkSimRegs xs1 xs2

(* checks if two items of the treeReg dataype have scrictly equal regions *)
fun checkSameExtRegs [] [] = true
  | checkSameExtRegs [] _  = false
  | checkSameExtRegs _  [] = false
  | checkSameExtRegs ((L (r1, c1, _)) :: rs1) ((L (r2, c2, _)) :: rs2) =
    R.checkSameRegs r1 r2 andalso
    sameCols c1 c2        andalso
    checkSameExtRegs rs1 rs2
  | checkSameExtRegs ((H (r1, c1, _)) :: rs1) ((H (r2, c2, _)) :: rs2) =
    R.checkSameRegs r1 r2 andalso
    sameCols c1 c2        andalso
    checkSameExtRegs rs1 rs2
  | checkSameExtRegs ((N (r1, c1, _, eregs1)) :: rs1) ((N (r2, c2, _, eregs2)) :: rs2) =
    R.checkSameRegs r1 r2    andalso
    sameCols c1 c2           andalso
    checkSameExtRegs rs1 rs2 andalso
    checkSameExtRegs eregs1 eregs2
  | checkSameExtRegs _ _ = false

(* checks if two files have strictly equal regions *)
fun checkSameRegs [] [] = true
  | checkSameRegs [] _  = false
  | checkSameRegs _ []  = false
  | checkSameRegs ((f1 : file, eregs1) :: xs1) ((f2 : file, eregs2) :: xs2) =
    (*f1 = f2 andalso*) checkSameExtRegs eregs1 eregs2 andalso checkSameRegs xs1 xs2

(* returns colours for any constructor of treeReg *)
fun getColor (L (_, c, _))    = c
  | getColor (H (_, c, _))    = c
  | getColor (N (_, c, _, _)) = c

(* returns weighting for any constructor of treeReg *)
fun getWeight (L (_, _, w))    = w
  | getWeight (H (_, _, w))    = w
  | getWeight (N (_, _, w, _)) = w

(* determines if the parameter is equal to Red *)
fun isRed Red = true
  | isRed _   = false

(* determines if the parameter is equal to Orarge *)
fun isOrange Orange = true
  | isOrange _      = false

(* determines if the parameter is equal to Yellow *)
fun isYellow Yellow = true
  | isYellow _      = false

(* determines if the parameter is a leaf of a tree *)
fun isLeaf (L _) = true
  | isLeaf _     = false

(* returns the extended regions in a given line number *)
fun getExtRegLine _ [] = []
  | getExtRegLine n (x :: xs) =
    if (R.getPosLine (R.getFrom (getReg x))) = n
    then x :: (getExtRegLine n xs)
    else getExtRegLine n xs

fun splitExtRegLine _ [] = ([], [])
  | splitExtRegLine n (x :: xs) =
    let val (xs1, xs2) = splitExtRegLine n xs
    in if (R.getPosLine (R.getFrom (getReg x))) = n
       then (x :: xs1, xs2)
       else (xs1, x :: xs2)
    end

(* the list is a list of extended region, the region is just a plain region *)
fun inclExtRegList [] _ = true
  | inclExtRegList (r' :: rs) r = R.inclReg (getReg r') r andalso inclExtRegList rs r

(* returns the biggest column from a list of extended regions *)
fun getEndExtRegList [] = raise EH.DeadBranch ""
  | getEndExtRegList [x] = R.getPosCol (R.getTo (getReg x))
  | getEndExtRegList (x :: xs) =
    let val c1 = R.getPosCol (R.getTo (getReg x))
	val c2 = getEndExtRegList xs
    in Int.max (c1, c2)
    end

(* r is a region and rs a list of extended regions *)
fun extendReg r rs =
    let val c    = getEndExtRegList rs
	val from = R.getFrom r
	val to   = R.getTo r
	val to'  = (R.getPosLine to, Int.max (R.getPosCol to, c))
    in R.consReg from to'
    end

(* deletes negative regions in treeReg *)
fun delNegReg [] = []
  | delNegReg ((N (r, c, w, tl)) :: xs) =
    if Reg.isReg r
    then (N (r, c, w, delNegReg tl)) :: (delNegReg xs)
    else (delNegReg tl) @ (delNegReg xs)
  | delNegReg ((L (r, c, w)) :: xs) =
    if Reg.isReg r
    then (L (r, c, w)) :: (delNegReg xs)
    else delNegReg xs
  | delNegReg ((H (r, c, w)) :: xs) =
    if Reg.isReg r
    then (H (r, c, w)) :: (delNegReg xs)
    else delNegReg xs

(* delete negative regions in a list of (file, region) tuples *)
fun delNegRegs [] = []
  | delNegRegs ((file, regs) :: xs) = (file, delNegReg regs) :: (delNegRegs xs)

(* check for overlapping regions *)
fun testOverLap r1 r2 _ _ =
    R.overlapReg r1 r2 orelse R.inclReg r1 r2 orelse R.inclReg r2 r1

(* check for overlapping regions *)
fun testOverLap r1 r2 (tr1 : constr) tr2 =
    tr1 = tr2 andalso (R.overlapReg r1 r2 orelse R.inclReg r1 r2 orelse R.inclReg r2 r1)

fun consWeightRegs2 r w _ [] = (w, [])
  | consWeightRegs2 r1 w1 tr ((L (r2, c2, w2)) :: regs) =
    if testOverLap r1 r2 tr CL
    then let val (w', regs') = consWeightRegs2 r1 (w1 + 1) tr regs
	 in (w', (L (r2, c2, w2 + 1)) :: regs')
	 end
    else let val (w', regs') = consWeightRegs2 r1 w1 tr regs
	 in (w', (L (r2, c2, w2)) :: regs')
	 end
  | consWeightRegs2 r1 w1 tr ((H (r2, c2, w2)) :: regs) =
    if testOverLap r1 r2 tr CH
    then let val (w', regs') = consWeightRegs2 r1 (w1 + 1) tr regs
	 in (w', (H (r2, c2, w2 + 1)) :: regs')
	 end
    else let val (w', regs') = consWeightRegs2 r1 w1 tr regs
	 in (w', (H (r2, c2, w2)) :: regs')
	 end
  | consWeightRegs2 r1 w1 tr ((N (r2, c2, w2, regs2)) :: regs) =
    if testOverLap r1 r2 tr CN
    then let val (w', regs2') = consWeightRegs2 r1 (w1 + 1) tr regs2
	     val (w'', regs') = consWeightRegs2 r1 w' tr regs
	 in (w'', (N (r2, c2, w2 + 1, regs2')) :: regs')
	 end
    else let val (w', regs2') = consWeightRegs2 r1 w1 tr regs2
	     val (w'', regs') = consWeightRegs2 r1 w' tr regs
	 in (w'', (N (r2, c2, w2, regs2')) :: regs')
	 end

fun consWeightRegs1 [] regs = ([], regs)
  | consWeightRegs1 ((L (r, c, w)) :: regs1) regs2 =
    let val (w', regs2') = consWeightRegs2 r w CL regs2
	val (regs1', regs2'') = consWeightRegs1 regs1 regs2'
    in ((L (r, c, w')) :: regs1', regs2'')
    end
  | consWeightRegs1 ((H (r, c, w)) :: regs1) regs2 =
    let val (w', regs2') = consWeightRegs2 r w CH regs2
	val (regs1', regs2'') = consWeightRegs1 regs1 regs2'
    in ((H (r, c, w')) :: regs1', regs2'')
    end
  | consWeightRegs1 ((N (r, c, w, regs)) :: regs1) regs2 =
    let val (regs', regs2') = consWeightRegs1 regs regs2
	val (w', regs2'') = consWeightRegs2 r w CN regs2'
	val (regs1', regs2''') = consWeightRegs1 regs1 regs2''
    in ((N (r, c, w', regs')) :: regs1', regs2''')
    end

fun consWeightRegs' f regs [] = (regs, [])
  | consWeightRegs' f1 regs1 ((f2 : file, regs2) :: xs) =
    if f1 = f2
    then
	let
	    val (regs1', regs2') = consWeightRegs1 regs1 regs2
	in (regs1', (f2, regs2') :: xs)
	end
    else
	let
	    val (regs1', xs') = consWeightRegs' f1 regs1 xs
	in (regs1', (f2, regs2) :: xs')
	end

fun consWeightRegs [] xs = ([], xs)
  | consWeightRegs ((f, regs) :: xs) ys =
    let val (regs', ys') = consWeightRegs' f regs ys
	val (xs', ys'') = consWeightRegs xs ys'
    in ((f, regs') :: xs', ys'')
    end

fun split (r1 as {from = f1, to = t1}) (* Red *)
	  (r2 as {from = f2, to = t2}) (* Blue *)
	  c1 c2 w1 w2 =
    let val r = R.consReg (R.upPos t2) t1
	val (rr', r') = if R.infPos f1 f2
			then ([L (R.consReg f1 (R.downPos f2), c1, w1)], L (r2, c2, w2))
			else ([], L (r2, c2, w2))
    in if R.isReg r then (rr' @ [r'], L (r, c1, w1)) else (rr', r')
    end

fun pushBar (r1 as {from = f1, to = t1}) (* push the bar (2) into the red leaf (1) *)
	    (r2 as {from = f2, to = t2})
	    c w1 w2 =
    let val r = R.consReg t2 t1
	val rr = if R.infPos f1 f2
		 then [L (R.consReg f1 (R.downPos f2), Red, w1), H (r2, c, w2)]
		 else [H (r2, c, w2)]
    in if R.isReg r then rr @ [L (r, Red, w1)] else rr
    end

fun sameSame t1 t2 r1 r2 xs col w1 w2 f bmerge =
    if R.strictInfReg r1 (R.downReg r2)
    then t1 :: t2 :: xs
    else if R.strictInfReg r2 (R.downReg r1)
    then t2 :: (f t1 xs)
    else if bmerge
    then f (L ((R.fusionReg r1 r2), col, Int.max (w1, w2))) xs
    else let val (ys, y) = split r1 r2 col col w1 w2 in ys @ (f y xs) end

and notEndEnd t1 t2 r1 r2 xs col1 col2 w1 w2 f =
    if R.strictInfReg r1 r2
    then t1 :: t2 :: xs
    else if R.strictInfReg r2 r1
    then t2 :: (f t1 xs)
    else let val (ys, y) = split r1 r2 col1 col2 w1 w2 in ys @ (f y xs) end

and endNotEnd t1 t2 r1 r2 xs col1 col2 w1 w2 f =
    if R.strictInfReg r1 r2
    then t1 :: t2 :: xs
    else if R.strictInfReg r2 r1
    then t2 :: (f t1 xs)
    else let val (ys, y) = split r2 r1 col2 col1 w2 w1 in ys @ (f y xs) end

fun simplify rl bmerge = (* bmerge is true if we want to merge regions *)
    let
	(*val _ = Debug.printdebug2 (printOneRegs rl)*)
	fun f (N (r, c, w, tl)) [] = [N (r, c, w, treatTree tl)]
	  | f t [] = [t]
	  | f (t as (N (r, c, w, tl))) (xss as (x :: xs)) =
	    if R.strictInfReg r (getReg x)
	    then (N (r, c, w, treatTree tl)) :: xss
	    else if R.strictInfReg (getReg x) r
	    then x :: (f t xs)
	    else (*if R.inclReg (getReg x) r
	    then (*(N (r, c, w, treatTree (x :: tl))) ::*) xss  (* Hack for infix operators - to fix *)
	    else*) (print (R.printRegList [r, getReg x]); raise EH.DeadBranch "")
	  | f (t as (H (r, c, w))) (xss as (x :: xs)) =
	    if R.infReg r (getReg x)
	    then t :: xss
	    else if R.strictInfReg (getReg x) r
	    then x :: (f t xs)
	    else if isLeaf x andalso (isRed (getColor x)(* orelse isYellow (getColor x)*))
	    then (pushBar (getReg x) r c (getWeight x) w) @ xs
	    else ((* Debug.printdebug2 (q`^(% r) ^(% (getReg x))`); *)
		  Debug.printdebug2 (R.printReg r ^ " " ^ R.printReg (getReg x));
		  (*f x xs*) raise EH.DeadBranch "")
	  (*else (Debug.printdebug2 ((printExtReg x "") ^ "\n"); raise EH.DeadBranch "")*)
	  | f (t as (L (r, c, w))) (xss as ((t' as (H (r', c', w'))) :: xs)) =
	    if R.strictInfReg r r'
	    then t :: xss
	    else if R.infReg r' r
	    then t' :: (f t xs)
	    else raise EH.DeadBranch ""
	  | f (t as (L (r, c, w))) (xss as ((t' as (N (r', c', w', tl'))) :: xs)) =
	    if R.strictInfReg r r'
	    then t :: xss
	    else if R.strictInfReg r' r
	    then t' :: (f t xs)
	    else raise EH.DeadBranch ""
	  | f (t as (L (r, Red,    w))) ((t' as (L (r', Red,    w'))) :: xs) = sameSame  t t' r r' xs Red    w w' f bmerge
	  | f (t as (L (r, Orange, w))) ((t' as (L (r', Orange, w'))) :: xs) = sameSame  t t' r r' xs Orange w w' f bmerge
	  | f (t as (L (r, Blue,   w))) ((t' as (L (r', Blue,   w'))) :: xs) = sameSame  t t' r r' xs Blue   w w' f bmerge
	  | f (t as (L (r, Purple, w))) ((t' as (L (r', Purple, w'))) :: xs) = sameSame  t t' r r' xs Purple w w' f bmerge
	  | f (t as (L (r, Green,  w))) ((t' as (L (r', Green,  w'))) :: xs) = sameSame  t t' r r' xs Green  w w' f bmerge
	  | f (t as (L (r, Yellow, w))) ((t' as (L (r', Yellow, w'))) :: xs) = sameSame  t t' r r' xs Yellow w w' f bmerge
	  | f (t as (L (r, Red,    w))) ((t' as (L (r', Yellow, w'))) :: xs) = notEndEnd t t' r r' xs Red    Yellow w w' f
	  | f (t as (L (r, Red,    w))) ((t' as (L (r', Orange, w'))) :: xs) = notEndEnd t t' r r' xs Red    Orange w w' f
	  | f (t as (L (r, Red,    w))) ((t' as (L (r', Blue,   w'))) :: xs) = notEndEnd t t' r r' xs Red    Blue   w w' f
	  | f (t as (L (r, Red,    w))) ((t' as (L (r', Purple, w'))) :: xs) = notEndEnd t t' r r' xs Red    Purple w w' f
	  | f (t as (L (r, Red,    w))) ((t' as (L (r', Green,  w'))) :: xs) = notEndEnd t t' r r' xs Red    Green  w w' f
	  | f (t as (L (r, Blue,   w))) ((t' as (L (r', Red,    w'))) :: xs) = endNotEnd t t' r r' xs Blue   Red    w w' f
	  | f (t as (L (r, Purple, w))) ((t' as (L (r', Red,    w'))) :: xs) = endNotEnd t t' r r' xs Purple Red    w w' f
	  | f (t as (L (r, Green,  w))) ((t' as (L (r', Red,    w'))) :: xs) = endNotEnd t t' r r' xs Green  Red    w w' f
	  | f (t as (L (r, Orange, w))) ((t' as (L (r', Red,    w'))) :: xs) = endNotEnd t t' r r' xs Orange Red    w w' f
	  | f (t as (L (r, Yellow, w))) ((t' as (L (r', Red,    w'))) :: xs) = endNotEnd t t' r r' xs Yellow Red    w w' f
	  | f (t as (L (r, Orange, w))) ((t' as (L (r', Blue,   w'))) :: xs) = notEndEnd t t' r r' xs Orange Blue   w w' f
	  | f (t as (L (r, Orange, w))) ((t' as (L (r', Purple, w'))) :: xs) = notEndEnd t t' r r' xs Orange Purple w w' f
	  | f (t as (L (r, Orange, w))) ((t' as (L (r', Green,  w'))) :: xs) = notEndEnd t t' r r' xs Orange Green  w w' f
	  | f (t as (L (r, Blue,   w))) ((t' as (L (r', Orange, w'))) :: xs) = endNotEnd t t' r r' xs Blue   Orange w w' f
	  | f (t as (L (r, Purple, w))) ((t' as (L (r', Orange, w'))) :: xs) = endNotEnd t t' r r' xs Purple Orange w w' f
	  | f (t as (L (r, Green,  w))) ((t' as (L (r', Orange, w'))) :: xs) = endNotEnd t t' r r' xs Green  Orange w w' f
	  | f (t as (L (r, Purple, w))) ((t' as (L (r', Blue,   w'))) :: xs) = notEndEnd t t' r r' xs Purple Blue   w w' f (* this should not happen - DeadBranch? *)
	  | f (t as (L (r, Purple, w))) ((t' as (L (r', Green,  w'))) :: xs) = notEndEnd t t' r r' xs Purple Green  w w' f (* this should not happen - DeadBranch? *)
	  | f (t as (L (r, Blue,   w))) ((t' as (L (r', Purple, w'))) :: xs) = notEndEnd t t' r r' xs Blue   Purple w w' f (* this should not happen - DeadBranch? *)
	  | f (t as (L (r, Blue,   w))) ((t' as (L (r', Green,  w'))) :: xs) = notEndEnd t t' r r' xs Blue   Green  w w' f (* this should not happen - DeadBranch? *)
	  | f (t as (L (r, Green,  w))) ((t' as (L (r', Blue,   w'))) :: xs) = notEndEnd t t' r r' xs Green  Blue   w w' f (* this should not happen - DeadBranch? *)
	  | f (t as (L (r, Green,  w))) ((t' as (L (r', Purple, w'))) :: xs) = notEndEnd t t' r r' xs Green  Purple w w' f (* this should not happen - DeadBranch? *)
	  | f (L (_, Yellow, _)) _        = raise EH.DeadBranch ""
	  | f _ ((L (_, Yellow, _)) :: _) = raise EH.DeadBranch ""
	and treatTree xs = foldl (fn (a, b) => f a b) [] xs
	fun treatRegs [] = []
	  | treatRegs ((file, regs) :: xs) =
	    ((*Debug.printdebug2 file;*)
	     (file, treatTree regs) :: (treatRegs xs))
    in treatRegs rl
    end

fun getCol' _ EK.Circularity                      = Red
  | getCol' _ (EK.MultiOcc   _)                   = Red
  | getCol' _ (EK.ValVarApp  _)                   = Red
  | getCol' _ (EK.ExcIsVar   _)                   = Red
  | getCol' _ (EK.ExcIsDat   _)                   = Red
  | getCol' _ (EK.ConIsVar   _)                   = Red
  | getCol' _ (EK.DatIsExc   _)                   = Red
  | getCol' _ (EK.TypeVarBind  _)                   = Red
  | getCol' _ EK.RigidWhere                       = Red
  | getCol' _ EK.Inclusion                        = Red
  | getCol' _ EK.AppNotApp                        = Red
  | getCol' _ EK.DiffFunName                      = Red
  | getCol' _ EK.DiffNbArgFun                     = Red
  | getCol' _ EK.FreeTypeVarTop                     = Red
  | getCol' _ EK.AsPatVar                         = Red
  | getCol' _ EK.FnRecExp                         = Red
  | getCol' _ EK.RealInPat                        = Red
  | getCol' _ EK.FreeIdent                        = Yellow
  | getCol' _ EK.FreeOpen                         = Yellow
  | getCol' _ (EK.Warning _)                      = Yellow
  | getCol' _ (EK.Parsing _)                      = Yellow
  | getCol' x (EK.ConsArgNApp (l1, l2))           =
    if x = l1 then Blue else if x = l2 then Purple else Red
  | getCol' x (EK.ConsNArgApp (l1, l2))           =
    if x = l1 then Blue else if x = l2 then Purple else Red
  | getCol' x (EK.ArityClash ((l1, _), (l2, _)))  =
    if x = l1 then Blue else if x = l2 then Purple else Red
  | getCol' x (EK.TyConsClash ((l1, _), (l2, _))) =
    if x = l1 then Blue else if x = l2 then Purple else Red
  | getCol' x (EK.EqTypeRequired (l1, l2)) =
    if x = l1 then Blue else if x = l2 then Purple else Red
  | getCol' x (EK.NotGenClash ((l1, _), (l2, _))) =
    if x = l1 then Blue else if x = l2 then Purple else Red
  | getCol' x (EK.TooGenSig ((l1, _), (l2, _), labs)) =
    if x = l2
    then Blue
    else if x = l1 orelse L.isin (L.fromInt x) (L.ord (map L.fromInt labs))
    then Purple
    else Red
  | getCol' x (EK.TyFunClash ((l1, _), (l2, _)))  =
    if x = l1 then Blue else if x = l2 then Purple else Red
  | getCol' x (EK.Overload ((l1, _), (l2, _), ltns))     =
    if x = l2
    then Blue
    else if L.isin (L.fromInt x) (L.ord (map (fn (l, _) => L.fromInt l) ltns))
    then Purple
    else Red
  | getCol' x (EK.OverloadCst ((l1, _, _), (l2, _), ltns))     = (* We should also highlight the label from the first parameter *)
    if x = l2
    then Blue
    else if x = l1 orelse L.isin (L.fromInt x) (L.ord (map (fn (l, _) => L.fromInt l) ltns))
    then Purple
    else Red
  | getCol' x (EK.OverloadClash ((l1, _, _), ltns1, (l2, _, _), ltns2)) =
    if x = l1 then Blue else if x = l2 then Purple else Red (* we might want to highlight the ltns's as well *)
  | getCol' x (EK.OverloadIdCst ((l1, _), ltns1, (l2, _, _), ltns2))     =
    if x = l2
    then Blue
    else if L.isin (L.fromInt x) (L.ord (map (fn (l, _) => L.fromInt l) ltns1))
    then Purple
    else Red
  | getCol' x (EK.LabTyClash (l1, l2, l3, l4))    =
    if L.isin (L.fromInt x) (L.ord (map (fn (l, _) => L.fromInt l) l1))
    then Blue
    else if L.isin (L.fromInt x) (L.ord (map (fn (l, _) => L.fromInt l) l2))
    then Purple
    else if L.isin (L.fromInt x) (L.ord (map (fn (l, _) => L.fromInt l) (l3 @ l4)))
    then Green
    else Red
  | getCol' x (EK.Unmatched ((l, _), ls, _))      =
    if L.isin (L.fromInt x) (L.singleton (L.fromInt l))
    then Blue
    else if L.isin (L.fromInt x) (L.ord (map (fn (l, _) => L.fromInt l) ls))
    then Purple
    else Red
  | getCol' x (EK.UnbWhere ((l, _), ls, _))      =
    if L.isin (L.fromInt x) (L.singleton (L.fromInt l))
    then Blue
    else if L.isin (L.fromInt x) (L.ord (map (fn (l, _) => L.fromInt l) ls))
    then Purple
    else Red
  | getCol' x (EK.MissConsSig ((l, _), ls))       =
    if L.isin (L.fromInt x) (L.singleton (L.fromInt l))
    then Blue
    else if L.isin (L.fromInt x) (L.ord (map (fn (l, _) => L.fromInt l) ls))
    then Purple
    else Red
  | getCol' x (EK.MissConsStr ((l, _), ls))       =
    if L.isin (L.fromInt x) (L.singleton (L.fromInt l))
    then Blue
    else if L.isin (L.fromInt x) (L.ord (map (fn (l, _) => L.fromInt l) ls))
    then Purple
    else Red
  | getCol' x (EK.DatTypClash (_, l1, l2))        =
    if x = l1 then Blue else if x = l2 then Purple else Red
  | getCol' x (EK.NonFlexWhere ((lab1, _), (lab2, _))) =
    if x = lab1 then Blue else if x = lab2 then Purple else Red
  | getCol' x (EK.IllFormedWhere ((lab1, _), (lab2, _))) =
    if x = lab1 then Blue else if x = lab2 then Purple else Red

fun getCol x (ek, bound) =
    let val col = getCol' (L.toInt x) ek
    in if L.isin x bound andalso isRed col
       then Orange
       else col
    end

fun recolor col Red = col
  | recolor _   col = col


(* This is to highlight an empty structure or an empty signature *)
fun highlightEmptyUnmatched regs l ((EK.Unmatched (_, ls, lab)), _) =
    if null ls (* empty structure or signature *)
       andalso (L.toInt l) = lab (* the structure we need to deal with *)
       andalso lab <> (L.toInt L.dummyLab) (* when lab is not useful, it can be dummy *)
    then let val (r1, r2) = case regs of
				[r1, r2] => (r1, r2)
			      | _ => raise EH.DeadBranch ""
	     val r = R.consReg (R.upPos (R.getTo r1)) (R.downPos (R.getFrom r2))
	 in [L (r, Purple, 1)]
	 end
    else []
  | highlightEmptyUnmatched regs l ((EK.UnbWhere (_, ls, lab)), _) =
    if null ls (* empty structure or signature *)
       andalso (L.toInt l) = lab (* the structure we need to deal with *)
       andalso lab <> (L.toInt L.dummyLab) (* when lab is not useful, it can be dummy *)
    then let val (r1, r2) = case regs of
				[r1, r2] => (r1, r2)
			      | _ => raise EH.DeadBranch ""
	     val r = R.consReg (R.upPos (R.getTo r1)) (R.downPos (R.getFrom r2))
	 in [L (r, Purple, 1)]
	 end
    else []
  | highlightEmptyUnmatched _ _ _ = []


(*
fun trickTyCon reg regl =
    let val left = R.getFrom reg
	fun throughregl [] = false
	  | throughregl ((L (r, _)) :: xs) =
	    if R.getFrom r = R.downPos left andalso R.getTo r = R.downPos left
	    then true
	    else throughregl xs
	  | throughregl ((H (r, _)) :: xs) =
	    if R.getFrom r = R.downPos left andalso R.getTo r = R.downPos left
	    then true
	    else throughregl xs
	  | throughregl ((N (r, _, _)) :: xs) =
	    if R.getFrom r = R.downPos left andalso R.getTo r = R.downPos left
	    then true
	    else throughregl xs
    in if throughregl regl
       then R.consReg (R.downPos left) (R.getTo reg) (R.getVis reg)
       else reg
    end*)

(*fun splitRegsToPushInN regs1 (* regions for the N *)
		       regs2 (* extended regions for inside N *)
		       col
		       occ =
    let
	fun cons [] [] = []
	  | cons [] _ = raise EH.DeadBranch ""
	  | cons (r :: rs) rs' =
	    let
		val line = R.getPosLine (R.getFrom r)
		val (rs1, rs2) = splitExtRegLine line rs'
		(*val _ = if inclExtRegList rs1 r
			then ()
			else (print (printExtRegList rs1 ^ " - " ^ R.printReg r ^ "\n");
			      raise EH.DeadBranch "")*)
		val r' = extendReg r rs1
	    in (N (r', col, occ, rs1)) :: (cons rs rs2)
	    end
    in cons regs1 regs2
    end*)

fun createRegsComponentsTuple [] = raise EH.DeadBranch ""
  | createRegsComponentsTuple [r] = []
  | createRegsComponentsTuple (r1 :: r2 :: rs) =
    let val posF = R.upPos (R.getTo r1)
	val posT = R.downPos (R.getFrom r2)
    in (R.consReg posF posT) :: (createRegsComponentsTuple (r2 :: rs))
    end

fun intIsALabInRecClash n ll =
    List.exists
	(fn (_, st) =>
	    case String.compare (st, Int.toString n) of
		EQUAL => true
	      | _ => false) ll

fun highlightTupleInRecClash reg eregs n lab ((EK.LabTyClash (l1, l2, l3, l4)), bound) weight =
    (case if L.isin lab (L.ord (map (fn (l, _) => L.fromInt l) l1))
	     andalso
	     intIsALabInRecClash n l1
	  then Blue
	  else if L.isin lab (L.ord (map (fn (l, _) => L.fromInt l) l2))
		  andalso
		  intIsALabInRecClash n l2
	  then Purple
	  else if L.isin lab (L.ord (map (fn (l, _) => L.fromInt l) (l3 @ l4)))
		  andalso
		  intIsALabInRecClash n (l3 @ l4)
	  then Green
	  else Red
      of
	 Red => eregs
       | col => [N (reg, col, weight, eregs)])
  | highlightTupleInRecClash _ eregs _ _ _ _ = eregs

fun unHighightTupleInRecClash lab col ((EK.LabTyClash (l1, l2, l3, l4)), bound) =
    if L.isin lab (L.ord (map (fn (l, _) => L.fromInt l) (l1 @ l2 @ l3 @ l4)))
    then Red
    else col
  | unHighightTupleInRecClash _ col _ = col


(* get top regions *)


(* on the right of r *)
fun getTopRegionsLongId (A.LongIdQual (A.StrId (_, _, r', _, _), _, _, _, _)) r =
    Reg.getRegionList (Reg.getTo r) (Reg.getFrom r')
  | getTopRegionsLongId (A.LongIdId (A.Ident (_, _, r', _, _))) r =
    Reg.getRegionList (Reg.getTo r) (Reg.getFrom r')
  | getTopRegionsLongId _ _ = []

(* on the left of r *)
fun getTopRegionsStrId (A.StrId (_, _, r', _, _)) r =
    Reg.getRegionList (Reg.getTo r') (Reg.getFrom r)
  | getTopRegionsStrId _ _ = []

(* on the left of r *)
fun getTopRegionsSigId (A.SigId (_, _, r', _, _)) r =
    Reg.getRegionList (Reg.getTo r') (Reg.getFrom r)
  | getTopRegionsSigId _ _ = []

(* on the right of r *)
fun getTopRegionsTypeRow (A.TypeRowOne (_, (r' :: _), _, _)) r =
    Reg.getRegionList (Reg.getTo r) (Reg.getFrom r')
  | getTopRegionsTypeRow (A.TypeRowEm (r', _, _)) r =
    Reg.getRegionList (Reg.getTo r) (Reg.getFrom r')
  | getTopRegionsTypeRow (A.TypeRowSeq (_, (r' :: _), _, _)) r =
    Reg.getRegionList (Reg.getTo r) (Reg.getFrom r')
  | getTopRegionsTypeRow _ _ = []

(* on the right of r *)
fun getTopRegionsTyClassSeq (A.TyClassSeqOne (_, (r' :: _), _, _)) r =
    Reg.getRegionList (Reg.getTo r) (Reg.getFrom r')
  | getTopRegionsTyClassSeq (A.TyClassSeqEm (r', _, _)) r =
    Reg.getRegionList (Reg.getTo r) (Reg.getFrom r')
  | getTopRegionsTyClassSeq (A.TyClassSeqSeq (_, (r' :: _), _, _)) r =
    Reg.getRegionList (Reg.getTo r) (Reg.getFrom r')
  | getTopRegionsTyClassSeq _ _ = []

(* on the left of r *)
fun getTopRegionsDatName (A.DatName (_, A.TyCon (_, _, r', _, _), _, _)) r =
    Reg.getRegionList (Reg.getTo r') (Reg.getFrom r)
  | getTopRegionsDatName _ _ = []

(* on the left of r *)
fun getTopRegionsLongTyCon (A.LongTyConQual (_, ltc, _, _, _)) r =
    getTopRegionsLongTyCon ltc r
  | getTopRegionsLongTyCon (A.LongTyConId (A.TyCon (_, _, r', _, _))) r =
    Reg.getRegionList (Reg.getTo r') (Reg.getFrom r)
  | getTopRegionsLongTyCon _ _ = []

(* on the left of r *)
fun getTopRegionsLDatName (A.LDatName (_, ltc, _, _)) r =
    getTopRegionsLongTyCon ltc r
  | getTopRegionsLDatName _ _ = []


(* Puts a red box around the regions *)
fun putRedBox regs (EK.FreeOpen, _) =
    map (fn (ereg as L (reg, Yellow, x)) => N (reg, Red, x, [ereg])
	  | ereg => ereg) regs
  | putRedBox regs _ = regs


(* return regions with fixed colors *)


fun mapYellow   xs = map (fn x => (L (x, Yellow,      1))) xs
fun mapRed      xs = map (fn x => (L (x, Red,         1))) xs
fun mapCol l ll xs = map (fn x => (L (x, getCol l ll, 1))) xs
fun mapColC col xs = map (fn x => (L (x, col,         1))) xs


(* check if all the regions are of a specific color *)


fun checkAllColTR (L (_, c, _))     c' = sameCols c c'
  | checkAllColTR (H (_, c, _))     c' = sameCols c c'
  | checkAllColTR (N (_, c, _, xs)) c' = sameCols c c'
					 andalso
					 checkAllCol xs c'
and checkAllCol xs c = List.all (fn x => checkAllColTR x c) xs

fun checkAllOrange xs = checkAllCol xs Orange


(* return the positions of a slice *)


fun getpos_progs (A.Progs xs) ll =
    getpos_proglist xs ll

and getpos_proglist []                      _  = []
  | getpos_proglist ((x, file, _, _) :: xs) ll =
    (case putRedBox (getpos_prog x ll) ll of
	[] => getpos_proglist xs ll
      | y  => (file, y) :: (getpos_proglist xs ll))

and getpos_prog (A.Prog tdl)           ll = getpos_progonelist tdl ll
  | getpos_prog (A.ProgDots pl)        ll = getpos_partlist pl ll

and getpos_progonelist []        _  = []
  | getpos_progonelist (x :: xs) ll = (getpos_progone x ll) @ (getpos_progonelist xs ll)

and getpos_afile (A.AFile (_, r, l, _)) ll = [L (r, getCol l ll, 1)]
  | getpos_afile A.AFileDots            _  = []

and getpos_abool (A.ABool (_, r, l, _)) ll = [L (r, getCol l ll, 1)]
  | getpos_abool A.ABoolDots            _  = []

and getpos_progone (A.ProgOneDec td)              ll = getpos_topdec td ll
  | getpos_progone (A.ProgOneExp (e, _, r, l, _)) ll = (L (r, getCol l ll, 1)) :: (getpos_exp e ll)
  | getpos_progone (A.ProgOneParse (_, rs, _, _)) ll = mapYellow rs
  | getpos_progone (A.ProgOneFile  (af, _))       ll = getpos_afile af ll
  | getpos_progone (A.ProgOneDots pl)             ll = getpos_partlist pl ll

and getpos_topdec (A.TopDec xs)     ll = getpos_topdeconelist xs ll
  | getpos_topdec (A.TopDecDots pl) ll = getpos_partlist pl ll

and getpos_topdeconelist []        _  = []
  | getpos_topdeconelist (x :: xs) ll = (getpos_topdecone x ll) @ (getpos_topdeconelist xs ll)

and getpos_topdecone (A.TopDecOneTes (x, _)) ll = getpos_smltes x ll
  | getpos_topdecone (A.TopDecOneDec (x, _)) ll = getpos_atopdec x ll
  | getpos_topdecone (A.TopDecOneDots pl)    ll = getpos_partlist pl ll

and getpos_smltes (A.SmlTesDec   (t,  rs, _)) ll = (mapRed rs) @ (getpos_atopdec t ll)
  | getpos_smltes (A.SmlTesSpec  (s,  rs, _)) ll = (mapRed rs) @ (getpos_spec s ll)
  | getpos_smltes (A.SmlTesUse   (af, rs, _)) ll = (mapRed rs) @ (getpos_afile af ll)
  | getpos_smltes (A.SmlTesSBas  (af, rs, _)) ll = (mapRed rs) @ (getpos_afile af ll)
  | getpos_smltes (A.SmlTesCBas  (rs, _, _))  _  = mapRed rs
  | getpos_smltes (A.SmlTesQuote (ab, rs, _)) ll = (mapRed rs) @ (getpos_abool ab ll)
  | getpos_smltes (A.SmlTesType  (st, rs, _)) ll = mapRed rs
  | getpos_smltes (A.SmlTesDots pl)           ll = getpos_partlist pl ll

and getpos_atopdec (A.ATopDecStr s)   ll = getpos_strdec s ll
  | getpos_atopdec (A.ATopDecSig s)   ll = getpos_sigdec s ll
  (*| getpos_atopdec (A.ATopDecFun f)   ll = getpos_fundec f ll*)
  | getpos_atopdec (A.ATopDecDots pl) ll = getpos_partlist pl ll

(*and getpos_fundec (A.FunDec (fb, r, l, _)) ll = (L (r, getCol l ll, 1)) :: (getpos_funbind fb ll)
  | getpos_fundec (A.FunDecDots pl)        ll = getpos_partlist pl ll*)

and getpos_funbind (A.FunBind (fbol, rl, _)) ll = getpos_funbindonelist fbol ll
  | getpos_funbind (A.FunBindDots pl)        ll = getpos_partlist pl ll

and getpos_funbindonelist xs ll = foldr (fn (x, y) => (getpos_funbindone x ll) @ y) [] xs

(* TODO: We still have to get the context of the structure ids. *)
and getpos_funbindone (A.FunBindO (fid, sid, si, se, rs, l, _))       ll =
    (mapCol l ll rs)          @
    (getpos_funid     fid ll) @
    (getpos_strid     sid ll) @
    (getpos_labsigexp si  ll) @
    (getpos_labstrexp se  ll)
  | getpos_funbindone (A.FunBindOO (fid, sid, si, si', se, rs, l, _)) ll =
    (mapCol l ll rs)           @
    (getpos_funid     fid  ll) @
    (getpos_strid     sid  ll) @
    (getpos_labsigexp si   ll) @
    (getpos_labsigexp si'  ll) @
    (getpos_labstrexp se   ll)
  | getpos_funbindone (A.FunBindOT (fid, sid, si, si', se, rs, l, _)) ll =
    (mapCol l ll rs)           @
    (getpos_funid     fid  ll) @
    (getpos_strid     sid  ll) @
    (getpos_labsigexp si   ll) @
    (getpos_labsigexp si'  ll) @
    (getpos_labstrexp se   ll)
  | getpos_funbindone (A.FunBindOS (fid, sp, se, rs, l, _))           ll =
    (mapCol l ll rs)          @
    (getpos_funid     fid ll) @
    (getpos_spec      sp  ll) @
    (getpos_labstrexp se  ll)
  | getpos_funbindone (A.FunBindOSO (fid, sp, si, se, rs, l, _))      ll =
    (mapCol l ll rs)          @
    (getpos_funid     fid ll) @
    (getpos_spec      sp  ll) @
    (getpos_labsigexp si  ll) @
    (getpos_labstrexp se  ll)
  | getpos_funbindone (A.FunBindOST (fid, sp, si, se, rs, l, _))      ll =
    (mapCol l ll rs)          @
    (getpos_funid     fid ll) @
    (getpos_spec      sp  ll) @
    (getpos_labsigexp si  ll) @
    (getpos_labstrexp se  ll)
  | getpos_funbindone (A.FunBindODots pl)                             ll =
    getpos_partlist pl ll

and getpos_sigdec (A.SigDec (sb, r, _)) ll =
    let val gpp = getpos_sigbind sb ll
	val c   = if checkAllOrange gpp
		  then Orange
		  else Red
    in (L (r, c, 1)) :: gpp
    end
  | getpos_sigdec (A.SigDecDots pl)     ll = getpos_partlist pl ll

and getpos_sigbind (A.SigBind (sbol, rl, _)) ll = getpos_sigbindonelist sbol ll
  | getpos_sigbind (A.SigBindDots pl)        ll = getpos_partlist pl ll

and getpos_sigbindonelist xs ll = foldr (fn (x, y) => (getpos_sigbindone x ll) @ y) [] xs

and getpos_sigbindone (A.SigBindOne (id, se, r, l, _)) ll =
    let val gp1 = getpos_sigid id ll
	val gp2 = getpos_labsigexp se ll
	val col = getCol l ll
	val c   = if checkAllOrange gp1
		     andalso checkAllOrange gp2
		     andalso isRed col
		  then Orange
		  else col
	val c'  = if isOrange c then Orange else Red
	val gp3 = mapColC c' (getTopRegionsSigId id r)
    in (L (r, c, 1)) :: gp1 @ gp2 @ gp3
    end
  | getpos_sigbindone (A.SigBindOneDots pl)            ll =
    getpos_partlist pl ll

and getpos_strdec (A.StrDec (xs, l, n)) ll = getpos_strdeconelist xs ll
  | getpos_strdec (A.StrDecDots pl)     ll = getpos_partlist pl ll

and getpos_strdeconelist xs ll = foldr (fn (x, y) => (getpos_strdecone x ll) @ y) [] xs

and getpos_strdecone (A.StrDecOneDec d)                    ll =
    getpos_decs d ll
  | getpos_strdecone (A.StrDecOneStr (sb, r, _))           ll =
    let val gpp = getpos_strbind sb ll
	val c   = if checkAllOrange gpp
		  then Orange
		  else Red
    in (L (r, c, 1)) :: gpp
    end
  | getpos_strdecone (A.StrDecOneLoc (sd1, sd2, rs, l, _)) ll =
    (mapCol l ll rs) @ (getpos_strdec sd1 ll) @ (getpos_strdec sd2 ll)
  | getpos_strdecone (A.StrDecOneFun (fb, r, l, _))        ll =
    (L (r, getCol l ll, 1)) :: (getpos_funbind fb ll)
  | getpos_strdecone (A.StrDecOneDots pl)                  ll =
    getpos_partlist pl ll

and getpos_strbind (A.StrBind (xs, rs, n)) ll =
    let val gpp = getpos_strbindonelist xs ll
	val c   = if checkAllOrange gpp
		  then Orange
		  else Red
    in (mapColC c rs) @ gpp
    end
  | getpos_strbind (A.StrBindDots pl)      ll =
    getpos_partlist pl ll

and getpos_strbindonelist xs ll = foldr (fn (x, y) => (getpos_strbindone x ll) @ y) [] xs

and getpos_strbindone (A.StrBindOneOp (id, si, se, rl, l, _)) ll =
    let val r   = case rl of
		      [r, _] => r
		    | _ => raise EH.DeadBranch "wrong number of regions"
	val gp1 = getpos_strid     id ll
	val gp2 = getpos_labsigexp si ll
	val gp3 = getpos_labstrexp se ll
	val gpp = gp1 @ gp2 @ gp3
	val col = getCol l ll
	val c   = if checkAllOrange gpp
		     andalso isRed col
		  then Orange
		  else col
	val c'  = if isOrange c then Orange else Red
	val gp4 = mapColC c' (getTopRegionsStrId id r)
    in (mapColC c rl) @ gpp @ gp4
    end
  | getpos_strbindone (A.StrBindOneTr (id, si, se, rl, l, _)) ll =
    let val r   = case rl of
		      [r, _] => r
		    | _ => raise EH.DeadBranch "wrong number of regions"
	val gp1 = getpos_strid     id ll
	val gp2 = getpos_labsigexp si ll
	val gp3 = getpos_labstrexp se ll
	val gpp = gp1 @ gp2 @ gp3
	val col = getCol l ll
	val c   = if checkAllOrange gpp
		     andalso isRed col
		  then Orange
		  else col
	val c'  = if isOrange c then Orange else Red
	val gp4 = mapColC c' (getTopRegionsStrId id r)
    in (mapColC c rl) @ gpp @ gp4
    end
  | getpos_strbindone (A.StrBindOne (id, se, r, l, _))     ll =
    let val gp1 = getpos_strid     id ll
	val gp2 = getpos_labstrexp se ll
	val col = getCol l ll
	val c   = if checkAllOrange gp1
		     andalso checkAllOrange gp2
		     andalso isRed col
		  then Orange
		  else col
	(* We do the next computation instead of having a labstrid.
	 * A labstrid is not really necessary here because we can only
	 * have one structure identifier on the left hand side of the =. *)
	val c'  = if isOrange c then Orange else Red
	val gpp = mapColC c' (getTopRegionsStrId id r)
	val gp  = (L (r, c, 1))
    in gp :: gpp @ gp1 @ gp2
    end
  | getpos_strbindone (A.StrBindOneDots pl)                   ll = getpos_partlist pl ll

and getpos_ltreadone (A.LTReaDOne (dn, ty, [r1, r2], l, _)) ll =
    (mapCol l ll [r1, r2]) @
    (getpos_ldatname dn ll) @
    (getpos_labtype ty ll)  @
    (mapRed (getTopRegionsLDatName dn r2))
  | getpos_ltreadone (A.LTReaDOne _)                        _  =
    raise EH.DeadBranch "not the correct number of regions"
  | getpos_ltreadone (A.LTReaDOneDots pl)                   ll =
    getpos_partlist pl ll

and getpos_ltreadonelist xs ll = foldr (fn (x, y) => (getpos_ltreadone x ll) @ y) [] xs

and getpos_ltreadesc (A.LTReaDesc (xs, rs, l, _)) ll = getpos_ltreadonelist xs ll
  | getpos_ltreadesc (A.LTReaDescDots pl)         ll = getpos_partlist pl ll

(*and getpos_treadone (A.TReaDOne (dn, ty, r, l, _)) ll =
    (L (r, getCol l ll, 1)) :: (getpos_datname dn ll) @ (getpos_labtype ty ll)
  | getpos_treadone (A.TReaDOneDots pl)            ll =
    getpos_partlist pl ll

and getpos_treadonelist xs ll = foldr (fn (x, y) => (getpos_treadone x ll) @ y) [] xs

and getpos_treadesc (A.TReaDesc (xs, rs, _)) ll = getpos_treadonelist xs ll
  | getpos_treadesc (A.TReaDescDots pl)      ll = getpos_partlist pl ll*)

and getpos_labsigexp (A.LabSigExp (e, rl, re, l, _)) ll =
    let val gp = getpos_sigexp e ll
    in (mapCol l ll rl) @ gp
    end
  | getpos_labsigexp (A.LabSigExpDots pl)            ll = getpos_partlist pl ll

and getpos_sigexp (A.SigExpBasic (sp, rl, l, _))     ll =
    (mapCol l ll rl)                  @
    (highlightEmptyUnmatched rl l ll) @
    (getpos_spec sp ll)
  | getpos_sigexp (A.SigExpId (id, _, _))            ll =
    getpos_sigid id ll
  | getpos_sigexp (A.SigExpRea (se, rea, rs, l, _))  ll =
    (mapCol l ll rs)          @
    (getpos_labsigexp se  ll) @
    (getpos_ltreadesc rea ll)
  | getpos_sigexp (A.SigExpDots pl)                  ll =
    getpos_partlist pl ll

and getpos_labstrexp (A.LabStrExp (e, rl, re, l, _)) ll =
    let val gp = getpos_strexp e ll
    in (mapCol l ll rl) @ gp
    end
  | getpos_labstrexp (A.LabStrExpDots pl)            ll = getpos_partlist pl ll

and getpos_strexp (A.StrExpBasic (sd, rl, l, _))     ll =
    (mapCol l ll rl)                  @
    (highlightEmptyUnmatched rl l ll) @
    (getpos_strdec sd ll)
  | getpos_strexp (A.StrExpId (id, _, _))            ll =
    getpos_longstrid id ll
  | getpos_strexp (A.StrExpOp (se, si, r, l, _))     ll =
    (L (r, getCol l ll, 1)) ::
    (getpos_labstrexp se ll) @
    (getpos_labsigexp si ll)
  | getpos_strexp (A.StrExpTr (se, si, r, l, _))     ll =
    (L (r, getCol l ll, 1)) ::
    (getpos_labstrexp se ll) @
    (getpos_labsigexp si ll)
  | getpos_strexp (A.StrExpFExp (id, se, rs, l, _))  ll =
    (mapCol l ll rs)         @
    (getpos_funid     id ll) @
    (getpos_labstrexp se ll)
  | getpos_strexp (A.StrExpFDec (id, sd, rs, l, _))  ll =
    (mapCol l ll rs)      @
    (getpos_funid  id ll) @
    (getpos_strdec sd ll)
  | getpos_strexp (A.StrExpLocal (sd, se, rs, l, _)) ll =
    (mapCol l ll rs)         @
    (getpos_strdec    sd ll) @
    (getpos_labstrexp se ll)
  | getpos_strexp (A.StrExpDots pl)                  ll =
    getpos_partlist pl ll

and getpos_spec (A.Spec (spol, _)) ll = getpos_speconelist spol ll
  | getpos_spec (A.SpecDots pl)    ll = getpos_partlist pl ll

and getpos_speconelist xs ll = foldr (fn (x, y) => (getpos_specone x ll) @ y) [] xs

and getpos_longtyconeq (A.LongTyConEq (xs, _, _, _)) ll = foldr (fn (x, y) => (getpos_longtycon x ll) @ y) [] xs
  | getpos_longtyconeq (A.LongTyConEqDots pl)        ll = getpos_partlist pl ll

and getpos_longstrideq (A.LongStrIdEq (xs, _, _)) ll = foldr (fn (x, y) => (getpos_longstrid x ll) @ y) [] xs
  | getpos_longstrideq (A.LongStrIdEqDots pl)     ll = getpos_partlist pl ll

and getpos_sigidseq (A.SigIdSeq (xs, _)) ll = foldr (fn (x, y) => (getpos_sigid x ll) @ y) [] xs
  | getpos_sigidseq (A.SigIdSeqDots pl)  ll = getpos_partlist pl ll

and getpos_specone (A.SpecValue (vd, r, l, _))     ll =
    let val gpp = getpos_valdesc vd ll
	val col = getCol l ll
	val c   = if checkAllOrange gpp
		     andalso isRed col
		  then Orange
		  else col
    in (L (r, c, 1)) :: gpp
    end
  | getpos_specone (A.SpecType (td, r, l, _))     ll =
    let val gpp = getpos_typdesc td ll
	val col = getCol l ll
	val c   = if checkAllOrange gpp
		     andalso isRed col
		  then Orange
		  else col
    in (L (r, c, 1)) :: gpp
    end
  | getpos_specone (A.SpecEqtype (td, r, l, _))     ll = (L (r, getCol l ll, 1)) :: (getpos_typdesc   td ll)
  | getpos_specone (A.SpecException (ed, r, l, _))     ll = (L (r, getCol l ll, 1)) :: (getpos_excdesc   ed ll)
  | getpos_specone (A.SpecTdr (td, r, l, _))     ll = (L (r, getCol l ll, 1)) :: (getpos_tdrdesc   td ll)
  | getpos_specone (A.SpecDat (dd, r, l, _))     ll = (L (r, getCol l ll, 1)) :: (getpos_datdesc   dd ll)
  | getpos_specone (A.SpecStr (sd, r, l, _))     ll =
    let val gpp = getpos_strdesc sd ll
	val col = getCol l ll
	val c   = if checkAllOrange gpp
		     andalso isRed col
		  then Orange
		  else col
    in (L (r, c, 1)) :: gpp
    end
  | getpos_specone (A.SpecInc (si, r, l, _))     ll = (L (r, getCol l ll, 1)) :: (getpos_labsigexp si ll)
  | getpos_specone (A.SpecIsi (si, r, l, _))     ll = (L (r, getCol l ll, 1)) :: (getpos_sigidseq  si ll)
  | getpos_specone (A.SpecRep (t, lt, rs, l, _)) ll = (mapCol l ll rs) @ (getpos_tycon t ll) @ (getpos_longtycon lt ll)
  | getpos_specone (A.SpecSha (s, lt, rs, l, _)) ll = (mapCol l ll rs) @ (getpos_spec s ll) @ (getpos_longtyconeq lt ll)
  | getpos_specone (A.SpecSsi (s, ls, rs, l, _)) ll = (mapCol l ll rs) @ (getpos_spec s ll) @ (getpos_longstrideq ls ll)
  | getpos_specone (A.SpecOneDots pl)            ll = getpos_partlist pl ll

and getpos_strdescone (A.StrDescOne (id, se, r, l, _)) ll =
    let val gp1 = getpos_strid  id ll
	val gp2 = getpos_labsigexp se ll
	val col = getCol l ll
	val c   = if checkAllOrange gp1
		     andalso checkAllOrange gp2
		     andalso isRed col
		  then Orange
		  else col
	val c'  = if isOrange c then Orange else Red
	val gpp = mapColC c' (getTopRegionsStrId id r)
    in (L (r, c, 1)) :: gp1 @ gp2 @ gpp
    end
  | getpos_strdescone (A.StrDescOneDots pl)            ll =
    getpos_partlist pl ll

and getpos_strdesconelist xs ll = foldr (fn (x, y) => (getpos_strdescone x ll) @ y) [] xs

and getpos_strdesc (A.StrDesc (sdol, rl, _)) ll = getpos_strdesconelist sdol ll
  | getpos_strdesc (A.StrDescDots pl)        ll = getpos_partlist pl ll

and getpos_condescone (A.ConDescOneId (id, _))           ll =
    getpos_ident id ll
  | getpos_condescone (A.ConDescOneOf (id, ty, r, l, _)) ll =
    let val gp1 = getpos_labid   id ll
	val gp2 = getpos_labtype ty ll
    in (L (r, getCol l ll, 1)) :: gp1 @ gp2
    end
  | getpos_condescone (A.ConDescOneNoOf (id, _))         ll = getpos_ident id ll
  | getpos_condescone (A.ConDescOneDots pl)              ll = getpos_partlist pl ll

and getpos_condesconelist xs ll = foldr (fn (x, y) => (getpos_condescone x ll) @ y) [] xs

and getpos_condesc (A.ConDesc (xs, rl, _)) ll = getpos_condesconelist xs ll
  | getpos_condesc (A.ConDescDots pl)      ll = getpos_partlist pl ll

and getpos_datdescone (A.DatDescOne (dn, cd, r, l, _)) ll =
    let val gp1 = getpos_datname dn ll
	val gp2 = getpos_condesc cd ll
	val gp3 = mapRed (getTopRegionsDatName dn r)
    in (L (r, getCol l ll, 1)) :: gp1 @ gp2 @ gp3
    end
  | getpos_datdescone (A.DatDescOneDots pl)            ll =
    getpos_partlist pl ll

and getpos_datdesconelist xs ll = foldr (fn (x, y) => (getpos_datdescone x ll) @ y) [] xs

and getpos_datdesc (A.DatDesc (ddol, rl, _)) ll = getpos_datdesconelist ddol ll
  | getpos_datdesc (A.DatDescDots pl)        ll = getpos_partlist pl ll

and getpos_valdescone (A.ValDescOne (id, ty, r, l, _)) ll =
    let val gp1 = getpos_labid   id ll
	val gp2 = getpos_labtype ty ll
	val col = getCol l ll
	val c   = if checkAllOrange gp1
		     andalso checkAllOrange gp2
		     andalso isRed col
		  then Orange
		  else col
    in (L (r, c, 1)) :: gp1 @ gp2
    end
  | getpos_valdescone (A.ValDescOneDots pl)            ll =
    getpos_partlist pl ll

and getpos_valdesconelist xs ll = foldr (fn (x, y) => (getpos_valdescone x ll) @ y) [] xs

and getpos_valdesc (A.ValDesc (vdol, rl, _)) ll = getpos_valdesconelist vdol ll
  | getpos_valdesc (A.ValDescDots pl)        ll = getpos_partlist pl ll

and getpos_typdescone (A.TypDescOne (dn, _, _)) ll = getpos_datname dn ll
  | getpos_typdescone (A.TypDescOneDots pl)     ll = getpos_partlist pl ll

and getpos_typdesconelist xs ll = foldr (fn (x, y) => (getpos_typdescone x ll) @ y) [] xs

and getpos_typdesc (A.TypDesc (tdol, rl, _)) ll = getpos_typdesconelist tdol ll
  | getpos_typdesc (A.TypDescDots pl)        ll = getpos_partlist pl ll

and getpos_tdrdescone (A.TdrDescOne (dn, ty, r, l, _)) ll =
    let val gp1 = getpos_datname dn ll
	val gp2 = getpos_labtype ty ll
	val gp3 = mapRed (getTopRegionsDatName dn r)
    in (L (r, getCol l ll, 1)) :: gp1 @ gp2 @ gp3
    end
  | getpos_tdrdescone (A.TdrDescOneDots pl)            ll =
    getpos_partlist pl ll

and getpos_tdrdesconelist xs ll = foldr (fn (x, y) => (getpos_tdrdescone x ll) @ y) [] xs

and getpos_tdrdesc (A.TdrDesc (tdol, rl, _)) ll = getpos_tdrdesconelist tdol ll
  | getpos_tdrdesc (A.TdrDescDots pl)        ll = getpos_partlist pl ll

and getpos_excdescone (A.ExcDescOne (id, _, _))       ll = getpos_ident id ll
  | getpos_excdescone (A.ExcDescOf (id, ty, r, l, _)) ll =
    let val gp1 = getpos_labid   id ll
	val gp2 = getpos_labtype ty ll
    in (L (r, getCol l ll, 1)) :: gp1 @ gp2
    end
  | getpos_excdescone (A.ExcDescOneDots pl)           ll =
    getpos_partlist pl ll

and getpos_excdesconelist xs ll = foldr (fn (x, y) => (getpos_excdescone x ll) @ y) [] xs

and getpos_excdesc (A.ExcDesc (edol, rl, _)) ll = getpos_excdesconelist edol ll
  | getpos_excdesc (A.ExcDescDots pl)        ll = getpos_partlist pl ll

and getpos_part (A.PartExp   e) ll = getpos_exp       e ll
  | getpos_part (A.PartDec   d) ll = getpos_dec       d ll
  | getpos_part (A.PartType  t) ll = getpos_type      t ll
  | getpos_part (A.PartSeq   s) ll = getpos_typeRow   s ll
  | getpos_part (A.PartPat   p) ll = getpos_pat       p ll
  | getpos_part (A.PartIdTy  i) ll = getpos_identty   i ll
  | getpos_part (A.PartTyCon t) ll = getpos_longtycon t ll
  | getpos_part (A.PartSpec  s) ll = getpos_specone   s ll
  | getpos_part (A.PartSige  s) ll = getpos_sigexp    s ll
  | getpos_part (A.PartStre  s) ll = getpos_strexp    s ll
  (*| getpos_part (A.PartFund  f) ll = getpos_fundec    f ll*)
  | getpos_part (A.PartSigd  s) ll = getpos_sigdec    s ll
  | getpos_part (A.PartStrd  s) ll = getpos_strdecone s ll
  | getpos_part (A.PartLgid  i) ll = getpos_longid    i ll
  | getpos_part (A.PartLgsid i) ll = getpos_longstrid i ll
  | getpos_part (A.PartSigid i) ll = getpos_sigid     i ll
  | getpos_part (A.PartTes   t) ll = getpos_smltes    t ll
  | getpos_part (A.PartClass c) ll = getpos_class     c ll

and getpos_partlist [] _         = []
  | getpos_partlist (p :: pl) ll = (getpos_part p ll) @ (getpos_partlist pl ll)

and getpos_scon (A.SconInt    (_, _, r, l, _)) ll = [L (r, getCol l ll, 1)]
  | getpos_scon (A.SconWord   (_, _, r, l, _)) ll = [L (r, getCol l ll, 1)]
  | getpos_scon (A.SconReal   (_, _, r, l, _)) ll = [L (r, getCol l ll, 1)]
  | getpos_scon (A.SconString (_, _, r, l, _)) ll = [L (r, getCol l ll, 1)]
  | getpos_scon (A.SconChar   (_, _, r, l, _)) ll = [L (r, getCol l ll, 1)]
  | getpos_scon A.SconDots                     _  = []

and getpos_pcon (A.PconBool (_, _, r, l, _)) ll = [L (r, getCol l ll, 1)]
  | getpos_pcon (A.PconRef  (_, _, r, l, _)) ll = [L (r, getCol l ll, 1)]
  | getpos_pcon (A.PconNil  (_, _, r, l, _)) ll = [L (r, getCol l ll, 1)]
  | getpos_pcon A.PconDots                   _  = []

and getpos_labid (A.LabId (id, rl, l, _)) ll =
    let val gp = getpos_ident id ll
    in (mapCol l ll rl) @ gp
    end
  | getpos_labid (A.LabIdDots pl)         ll = getpos_partlist pl ll

and getpos_ident (A.Ident (_, _, r, l, _)) ll = [L (r, getCol l ll, 1)]
  | getpos_ident (A.IdentPcon pc)          ll = getpos_pcon pc ll
  | getpos_ident A.IdentDots               _  = []

and getpos_labclass (A.LabClass (id, rl, l, _)) ll =
    let val gp = getpos_class id ll
    in (mapCol l ll rl) @ gp
    end
  | getpos_labclass (A.LabClassDots pl)         ll = getpos_partlist pl ll

and getpos_class (A.Class (_, _, r, l, _)) ll = [L (r, getCol l ll, 1)]
  | getpos_class A.ClassDots               _  = []

and getpos_strid (A.StrId (_, _, r, l, _)) ll = [L (r, getCol l ll, 1)]
  | getpos_strid A.StrIdDots               _  = []

and getpos_sigid (A.SigId (_, _, r, l, _)) ll = [L (r, getCol l ll, 1)]
  | getpos_sigid A.SigIdDots               _  = []

and getpos_funid (A.FunId (_, _, r, l, _)) ll = [L (r, getCol l ll, 1)]
  | getpos_funid A.FunIdDots               _  = []

and getpos_longstrid (A.LongStrIdQual (id, lid, r, l, _)) ll =
    let val gp1 = getpos_strid     id ll
	val gp2 = getpos_longstrid lid ll
	val gp = (L (r, getCol l ll, 1))
    in gp :: gp1 @ gp2
    end
  | getpos_longstrid (A.LongStrIdId id)                    ll = getpos_strid id ll
  | getpos_longstrid (A.LongStrIdDots pl)                  ll = getpos_partlist pl ll

and getpos_longid (A.LongIdQual (sid, lid, r, l, _)) ll =
    let val gp1 = getpos_strid sid ll
	val gp2 = getpos_longid lid ll
	val gp = (L (r, getCol l ll, 1))
    in gp :: gp1 @ gp2
    end
  | getpos_longid (A.LongIdId id) ll                    = getpos_ident id ll
  | getpos_longid (A.LongIdDots pl) ll                  = getpos_partlist pl ll

and getpos_longtycon (A.LongTyConQual (sid, ltc, r, l, _)) ll =
    let val gp1 = getpos_strid sid ll
	val gp2 = getpos_longtycon ltc ll
	val gp = (L (r, getCol l ll, 1))
    in gp :: gp1 @ gp2
    end
  | getpos_longtycon (A.LongTyConId tc) ll                    = getpos_tycon tc ll
  | getpos_longtycon (A.LongTyConDots pl) ll                  = getpos_partlist pl ll

and getpos_tylab (A.TyLab (_, r, l, _)) ll = [L (r, getCol l ll, 1)]
  | getpos_tylab A.TyLabDots _             = []

and getpos_tycon (A.TyCon (_, _, r, l, _)) ll = [L (r, getCol l ll, 1)]
  | getpos_tycon A.TyConDots _                = []

and getpos_typevar (A.TypeVar (_, _, r, l, _)) ll = [L (r, getCol l ll, 1)]
  | getpos_typevar (A.EqualityTypeVar (_, _, r, l, _)) ll = [L (r, getCol l ll, 1)]
  | getpos_typevar A.TypeVarDots _                = []

and getpos_typevarlist [] _           = []
  | getpos_typevarlist (tv :: tvl) ll = (getpos_typevar tv ll) @ (getpos_typevarlist tvl ll)

and getpos_labtyvar (A.LabTypeVar (tv, rl, l, _)) ll =
    let val gp = getpos_typevar tv ll
    in (mapCol l ll rl) @ gp
    end
  | getpos_labtyvar (A.LabTypeVarDots tvl) ll        = getpos_typevarlist tvl ll

and getpos_labtyvarlist [] _         = []
  | getpos_labtyvarlist (x :: xs) ll = (getpos_labtyvar x ll) @ (getpos_labtyvarlist xs ll)

and getpos_tyvarseq (A.TypeVarSeqOne (tv, r, l, _))   ll =
    let (*val _  = Debug.printdebug2 "foo"*)
	val gp = getpos_typevar tv ll
    in (*case tv of
	   A.TypeVarDots =>*) [N (r, getCol l ll, 1, gp)]
	 (*| _ => gp (* 2009-09-11: before it was just the N above, but that seems wrong, see test 3 *)*)
    end
  | getpos_tyvarseq (A.TypeVarSeqEm (r, l, _))        ll = [L (r, getCol l ll, 1)]
  | getpos_tyvarseq (A.TypeVarSeqSeq (tvl, rl, l, _)) ll =
    let val gp = getpos_labtyvarlist tvl ll
    in (mapCol l ll rl) @ gp
    end
  | getpos_tyvarseq (A.TypeVarSeqDots tvl)            ll = getpos_typevarlist tvl ll

and getpos_typelist [] _          = []
  | getpos_typelist (ty :: tl) ll = (getpos_type ty ll) @ (getpos_typelist tl ll)

and getpos_labtypelist [] _          = []
  | getpos_labtypelist (ty :: tl) ll = (getpos_labtype ty ll) @ (getpos_labtypelist tl ll)

and getpos_labtype (A.LabType (t, rl, l, _)) ll =
    let val gp = getpos_type t ll
    in (mapCol l ll rl) @ gp
    end
  | getpos_labtype (A.LabTypeDots pl) ll        = getpos_partlist pl ll

and getpos_tyfield (A.TyField (tl, lt, r, l, _)) ll =
    let val gp1 = (getpos_tylab tl ll)
	val gp2 = (getpos_labtype lt ll)
	val gp = (L (r, getCol l ll, 1))
    in gp :: gp1 @ gp2
    end
  | getpos_tyfield (A.TyFieldDots pl) ll            = getpos_partlist pl ll

and getpos_tyfieldlist []        _  = []
  | getpos_tyfieldlist (x :: xs) ll = (getpos_tyfield x ll) @ (getpos_tyfieldlist xs ll)

and getpos_type (A.TypeOneVar tv) ll                  = getpos_typevar tv ll
  | getpos_type (A.TypeArrow (ty1, ty2, r, l, _)) ll  =
    let val gp1 = getpos_labtype ty1 ll
	val gp2 = getpos_labtype ty2 ll
	val gp = (L (r, getCol l ll, 1))
    in gp :: gp1 @ gp2
    end
  | getpos_type (A.TypeTuple (tl, rl, l, _)) ll       =
    let val gp = getpos_labtypelist tl ll
    in (mapCol l ll rl) @ gp
    end
  | getpos_type (A.TypeRecord (trl, rl1, rl2, l, _)) ll =
    let val gp = getpos_tyfieldlist trl ll
    in (mapCol l ll (rl1 @ rl2)) @ gp
    end
  | getpos_type (A.TypeSlRec (trl, rl, l, _)) ll =
    let val gp = getpos_tyfieldlist trl ll
    in (mapCol l ll rl) @ gp
    end
  | getpos_type (A.TypeTyCon (ts, ltc, rl, l, _)) ll =
    let val gp1 = getpos_longtycon ltc ll
	val gp2 = getpos_typeRow ts ll
    in (mapCol l ll rl) @ gp1 @ gp2
    end
  | getpos_type (A.TypeParen (ty, r1, r2, l, _)) ll   =
    let val gp = getpos_labtype ty ll
    in (mapCol l ll [r1, r2]) @ gp
    end
  | getpos_type (A.TypeDots spl) ll                   = getpos_partlist spl ll

and getpos_typeRow (A.TypeRowOne (ty, rs, l, _)) ll =
    let val gpp = getpos_type ty ll
	val r'  = case rs of [r] => r | _ => raise EH.DeadBranch "" (*trickTyCon r gpp*)
	(*val gpp' = splitRegsToPushInN rs gpp (getCol l ll) 1*)
    in (*gpp'*) [N (r', getCol l ll, 1, gpp)]
    end
  | getpos_typeRow (A.TypeRowEm (r, l, _))       ll = [H (r, getCol l ll, 1)]
  | getpos_typeRow (A.TypeRowSeq (tl, rl, l, _)) ll =
    let val gp = (getpos_labtypelist tl ll)
    in (mapCol l ll rl) @ gp
    end
  | getpos_typeRow (A.TypeRowDots spl)           ll = getpos_partlist spl ll

and getpos_conbind (A.ConBind (id, _)) ll             = getpos_ident id ll
  | getpos_conbind (A.ConBindOf (id, ty, r, l, _)) ll =
    let val gp1 = (getpos_labid id ll)
	val gp2 = (getpos_labtype ty ll)
	val gp = (L (r, getCol l ll, 1))
    in gp :: gp1 @ gp2
    end
  | getpos_conbind (A.ConBindNoOf (id, _)) ll         = getpos_ident id ll
  | getpos_conbind (A.ConBindDots spl) ll             = getpos_partlist spl ll

and getpos_conbindlist [] _         = []
  | getpos_conbindlist (x :: xs) ll = (getpos_conbind x ll) @ (getpos_conbindlist xs ll)

and getpos_conbindseq (A.ConBindSeq tcl) ll     = getpos_conbindlist tcl ll
  | getpos_conbindseq (A.ConBindSeqDots spl) ll = getpos_partlist spl ll

and getpos_valbindcore (A.ValBindCore (p, e, r, l, _)) ll =
    let val gp1 = getpos_labpat p ll
	val gp2 = getpos_labexp e ll
	val gp  = L (r, getCol l ll, 1)
    in gp :: gp1 @ gp2
    end
  | getpos_valbindcore (A.ValBindCoreDots pl) ll          = getpos_partlist pl ll

and getpos_valbindcorelist [] _         = []
  | getpos_valbindcorelist (x :: xs) ll = (getpos_valbindcore x ll) @ (getpos_valbindcorelist xs ll)

(* we don't care about these regions *)
and getpos_valbindseq (A.ValBindSeq (vbl, rl, _)) ll = getpos_valbindcorelist vbl ll
  | getpos_valbindseq (A.ValBindSeqDots pl) ll      = getpos_partlist pl ll

and getpos_valbind (A.ValBindRec (vbs, r, _, _)) ll = (L (r, Red, 1)) :: (getpos_valbindseq vbs ll)
  | getpos_valbind (A.ValBind vbs)               ll = getpos_valbindseq vbs ll
  | getpos_valbind (A.ValBindDots pl)            ll = getpos_partlist pl ll

(* what do we do about the regions? *)
and getpos_datname (A.DatName (tvs, tn, rl, _)) ll =
    let val gp1 = getpos_tycon tn ll
	val gp2 = getpos_tyvarseq tvs ll
	val c   = if checkAllOrange gp1
		     andalso checkAllOrange gp2
		  then Orange
		  else Red
    in (mapColC c rl) @ gp1 @ gp2
    end
  | getpos_datname A.DatNameDots ll                = []

and getpos_ldatname (A.LDatName (tvs, tn, rl, _)) ll =
    let val gp1 = getpos_longtycon tn ll
	val gp2 = getpos_tyvarseq tvs ll
    in (mapRed rl) @ gp1 @ gp2
    end
  | getpos_ldatname A.LDatNameDots ll                = []

and getpos_datbind (A.DatBind (dn, tcs, r, l, _)) ll =
    let val gp1 = getpos_datname dn ll
	val gp2 = getpos_conbindseq tcs ll
	val gp3 = mapRed (getTopRegionsDatName dn r)
	val gp  = L (r, getCol l ll, 1)
    in gp :: gp1 @ gp2 @ gp3
    end
  | getpos_datbind (A.DatBindDots spl) ll            = getpos_partlist spl ll

and getpos_datbindlist [] _         = []
  | getpos_datbindlist (x :: xs) ll = (getpos_datbind x ll) @ (getpos_datbindlist xs ll)

(* we don't care about these regions *)
and getpos_datbindseq (A.DatBindSeq (dbl, rl, _)) ll = getpos_datbindlist dbl ll
  | getpos_datbindseq (A.DatBindSeqDots spl)      ll = getpos_partlist spl ll

(* 2009-09-11: We changed the line below because Joe does not like
 * the boxes around this kind of argument *)
(* 2009-09-28: changed it back to what it was because this need more careful treatment.
 * at least as for the other arguments. *)
and getpos_labatpat (A.LabAtPat (ap, r, l, _))    ll = [(N (r, getCol l ll, 1, getpos_atpat ap ll))]
  (*getpos_atpat ap ll*)
  | getpos_labatpat (A.LabAtPatDots pl)           ll = getpos_partlist pl ll

and getpos_fmatch (A.FMatchId (id, fix, _))             ll = getpos_ident id ll
  | getpos_fmatch (A.FMatchApp (fm, lap, rl, ra, l, _)) ll =
    let val col = (*if Reg.isVisList rl then Red else*) getCol l ll
	val (fix, gp1) =
	    case fm of
		A.FMatchId (A.IdentDots, fix, r) => (fix, [N (r, col, 1, [])])
	      | A.FMatchId (_, fix, _) => (fix, getpos_fmatch fm ll)
	      | _ => (false, getpos_fmatch fm ll)
	val gp2 =
	    case lap of
		(A.LabAtPatDots _) => (* otherwise it has a box anyway *)
		[N (ra, col, 1, getpos_labatpat lap ll)]
	      | _ => getpos_labatpat lap ll
	(*val gp2 = getpos_labatpat lap ll*)
	val gp0 = mapCol l ll rl
	val gpp = case (gp2, fix) of
		      ([N (r, c, w, rs)], true) => [N (r, c, w, gp0 @ gp1 @ rs)]
		    | _ => gp0 @ gp1 @ gp2
	(*val _ = D.printdebug2 (printExtRegList gp0)
	val _ = D.printdebug2 (printExtRegList gp1)
	val _ = D.printdebug2 (printExtRegList gp2)*)
    in gpp
    end
  | getpos_fmatch (A.FMatchSlApp (fm, lap, _))          ll =
    let val gp1 = getpos_fmatch fm ll
	val gp2 = getpos_labatpat lap ll
    in gp1 @ gp2
    end
  | getpos_fmatch (A.FMatchNoApp (fm, _))               ll = getpos_fmatch fm ll
  | getpos_fmatch A.FMatchDots                          _  = []

and getpos_labfmatch (A.LabFMatch (fm, rl, l, _)) ll =
    let val gp = getpos_fmatch fm ll
    in (mapCol l ll rl) @ gp
    end
  | getpos_labfmatch (A.LabFMatchSl (fm, _)) ll      = getpos_fmatch fm ll
  | getpos_labfmatch A.LabFMatchDots _               = []

and getpos_fmatchty (A.FMatchT fm)                  ll = getpos_labfmatch fm ll
  | getpos_fmatchty (A.FMatchTTy (fm, ty, r, l, n)) ll =
    let val gp1 = getpos_labfmatch fm ll
	val gp2 = getpos_labtype ty ll
	val gp = (L (r, getCol l ll, 1))
    in gp :: gp1 @ gp2
    end
  | getpos_fmatchty A.FMatchTDots                   _  = []

and getpos_fvalbindcore (A.FValBindCore (fm, e, r, l, _))      ll =
    let val gp1 = getpos_fmatchty fm ll
	val gp2 = getpos_labexp e ll
	val col = getCol l ll
	val c   = if checkAllOrange gp1
		     andalso checkAllOrange gp2
		     andalso isRed col
		  then Orange
		  else col
	val gp = (L (r, c, 1))
    in gp :: gp1 @ gp2
    end
  (*| getpos_fvalbindcore (A.FVBCoreTy (x, t, e, r1, r2, l, _)) ll =
    let val gpp1 = getpos_labfmatch x ll
	val gpp2 = getpos_labtype t ll
	val gpp3 = getpos_labexp e ll
	val gp1  = (L (r1, getCol l ll, 1))
	val gp2  = (L (r2, getCol l ll, 1))
    in gp1 :: gp2 :: gpp1 @ gpp2 @ gpp3
    end*)
  | getpos_fvalbindcore (A.FVBCoreDots pl)                    ll = getpos_partlist pl ll

and getpos_fvalbindcorelist [] _ = []
  | getpos_fvalbindcorelist (x :: xs) ll = (getpos_fvalbindcore x ll) @ (getpos_fvalbindcorelist xs ll)

and getpos_fvalbindone (A.FValBindOne (x, rl, l, _)) ll =
    let val gpx = getpos_fvalbindcorelist x ll
	val col = getCol l ll
	val c   = if checkAllOrange gpx
		     andalso isRed col
		  then Orange
		  else col
    in (mapColC c rl) @ gpx
    end
  | getpos_fvalbindone (A.FVBOneDots pl) ll             = getpos_partlist pl ll

and getpos_fvalbindonelist [] _         = []
  | getpos_fvalbindonelist (x :: xs) ll = (getpos_fvalbindone x ll) @ (getpos_fvalbindonelist xs ll)

(* we don't care about these regions *)
and getpos_fvalbind (A.FValBind (x, rl, _)) ll =
    getpos_fvalbindonelist x ll
  | getpos_fvalbind (A.FValBindDots pl) ll     = getpos_partlist pl ll

and getpos_typbind (A.TypBind (dn, ty, r, l, _)) ll =
    let val gp1 = getpos_datname dn ll
	val gp2 = getpos_labtype ty ll
	val gp3 = mapRed (getTopRegionsDatName dn r)
	val gp  = (L (r, getCol l ll, 1))
    in gp :: gp1 @ gp2 @ gp3
    end
  | getpos_typbind (A.TypBindDots pl) ll            = getpos_partlist pl ll

and getpos_typbindlist []        _  = []
  | getpos_typbindlist (x :: xs) ll = (getpos_typbind x ll) @ (getpos_typbindlist xs ll)

and getpos_typbindseq (A.TypBindSeq (tbl, rl, _)) ll = getpos_typbindlist tbl ll
  | getpos_typbindseq (A.TypBindSeqDots pl)       ll = getpos_partlist pl ll

and getpos_exbind (A.ExBind (id, l, _))           ll =
    (case getpos_ident id ll of
	 [L (r, c, w)] => [L (r, recolor c (getCol l ll), w)]
       | x => x)
  | getpos_exbind (A.ExBindOf (id, t, r, l, _))   ll =
    let val gp1 = getpos_labid id ll
	val gp2 = getpos_labtype t ll
	val gp  = (L (r, getCol l ll, 1))
    in gp :: gp1 @ gp2
    end
  | getpos_exbind (A.ExBindEq (id, sid, r, l, _)) ll =
    let val gp1 = getpos_labid id ll
	val gp2 = getpos_longid sid ll
	(* We do the next computation instead of having a lablongid.
	 * A lablongid is not really necessary here because we can only
	 * have one exception on the right hand side of the =. *)
	val gpp = mapRed (getTopRegionsLongId sid r)
	val gp  = (L (r, getCol l ll, 1))
    in gp :: gpp @ gp1 @ gp2
    end
  | getpos_exbind (A.ExBindNo (id, _))            ll = getpos_ident id ll
  | getpos_exbind (A.ExBindDots pl)               ll = getpos_partlist pl ll

and getpos_exbindlist []        _  = []
  | getpos_exbindlist (x :: xs) ll = (getpos_exbind x ll) @ (getpos_exbindlist xs ll)

and getpos_exbindseq (A.ExBindSeq (ebl, rl, _)) ll = getpos_exbindlist ebl ll
  | getpos_exbindseq (A.ExBindSeqDots pl)       ll = getpos_partlist pl ll

and getpos_longstridlist []        _  = []
  | getpos_longstridlist (x :: xs) ll = (getpos_longstrid x ll) @ (getpos_longstridlist xs ll)

and getpos_longstrseq (A.LongStrSeq (idl, _)) ll = getpos_longstridlist idl ll
  | getpos_longstrseq (A.LongStrSeqDots pl)   ll = getpos_partlist pl ll

and getpos_identlist []        _  = []
  | getpos_identlist (x :: xs) ll = (getpos_ident x ll) @ (getpos_identlist xs ll)

and getpos_identseq (A.IdentSeq (idl, _)) ll = getpos_identlist idl ll
  | getpos_identseq (A.IdentSeqDots pl)   ll = getpos_partlist pl ll

and getpos_labtyclasslist []        _  = []
  | getpos_labtyclasslist (x :: xs) ll = (getpos_labtyclass x ll) @ (getpos_labtyclasslist xs ll)

and getpos_tyclass (A.TyClassCl (cl, r, l, _)) ll = (L (r, getCol l ll, 1)) :: (getpos_labclass cl ll)
  | getpos_tyclass (A.TyClassTy (ty, _, _))    ll = getpos_type ty ll
  | getpos_tyclass (A.TyClassDots pl)          ll = getpos_partlist pl ll

and getpos_labtyclass (A.LabTyClass (t, rl, l, _)) ll =
    let val gp = getpos_tyclass t ll
    in (mapCol l ll rl) @ gp
    end
  | getpos_labtyclass (A.LabTyClassDots pl)        ll = getpos_partlist pl ll

and getpos_tyclassseq (A.TyClassSeqOne (ty, rs, l, _)) ll =
    let val gpp = getpos_tyclass ty ll
	val r'  = case rs of [r] => r | _ => raise EH.DeadBranch ""
    in [N (r', getCol l ll, 1, gpp)]
    end
  | getpos_tyclassseq (A.TyClassSeqEm (r, l, _))       ll = [H (r, getCol l ll, 1)]
  | getpos_tyclassseq (A.TyClassSeqSeq (tl, rl, l, _)) ll =
    let val gp = (getpos_labtyclasslist tl ll)
    in (mapCol l ll rl) @ gp
    end
  | getpos_tyclassseq (A.TyClassSeqDots spl)           ll = getpos_partlist spl ll

and getpos_dec (A.DecVal (tvs, vb, r, _))                 ll =
    (L (r, Red, 1)) ::
    (getpos_tyvarseq tvs ll) @
    (getpos_valbind  vb  ll)
  | getpos_dec (A.DecFVal (tvs, fvb, r, _))               ll =
    let val gp1 = getpos_tyvarseq tvs ll
	val gp2 = getpos_fvalbind fvb ll
	val c   = if checkAllOrange gp1
		     andalso checkAllOrange gp2
		  then Orange
		  else Red
    in (L (r, c, 1)) :: gp1 @ gp2
    end
  | getpos_dec (A.DecDatType (dbs, r, _))                 ll =
    (L (r, Red, 1)) :: (getpos_datbindseq dbs ll)
  | getpos_dec (A.DecDatWith (db, tb, rs, l, _))          ll =
    (mapCol l ll rs)          @
    (getpos_datbindseq db ll) @
    (getpos_typbindseq tb ll)
  | getpos_dec (A.DecDatRep (tc, ltc, rs, l, _))          ll =
    (mapCol l ll rs)      @
    (getpos_tycon tc  ll) @
    (getpos_longtycon ltc ll)
  | getpos_dec (A.DecType (tbs, r, _))                    ll =
    (L (r, Red, 1)) :: (getpos_typbindseq tbs ll)
  | getpos_dec (A.DecEx (ebs, r, _))                      ll =
    (L (r, Red, 1)) :: (getpos_exbindseq  ebs ll)
  | getpos_dec (A.DecOpen (ids, r, l, _))                 ll =
    (L (r, getCol l ll, 1)) :: (getpos_longstrseq ids ll)
  | getpos_dec (A.DecLocal (d1, d2, rs, l, _))            ll =
    (mapCol l ll rs)    @
    (getpos_decs d1 ll) @
    (getpos_decs d2 ll)
  | getpos_dec (A.DecAbsType (db, ds, rs, l, _))          ll =
    (mapCol l ll rs)          @
    (getpos_datbindseq db ll) @
    (getpos_decs       ds ll)
  | getpos_dec (A.DecAbsWith (b, t, d, rs, l, _))         ll =
    (mapCol l ll rs)         @
    (getpos_datbindseq b ll) @
    (getpos_typbindseq t ll) @
    (getpos_decs       d ll)
  | getpos_dec (A.DecInfix  (_, ids, r, l, _))            ll =
    (L (r, getCol l ll, 1)) :: (getpos_identseq ids ll)
  | getpos_dec (A.DecInfixr (_, ids, r, l, _))            ll =
    (L (r, getCol l ll, 1)) :: (getpos_identseq ids ll)
  | getpos_dec (A.DecNonfix (ids, r, l, _))               ll =
    (L (r, getCol l ll, 1)) :: (getpos_identseq ids ll)
  | getpos_dec (A.DecOverload (id, ty, tv, ts, rs, l, _)) ll =
    let val r = case rs of
		    [_, _, _, r] => r
		  | _ => raise EH.DeadBranch "Overload should have 4 terminals"
	val gpp = mapCol l ll rs
	val gpi = getpos_labid      id ll
	val gpt = getpos_labtype    ty ll
	val gpv = getpos_labtyvar   tv ll
	val gps = getpos_tyclassseq ts ll
	val gpw = mapRed (getTopRegionsTyClassSeq ts r)
    in gpp @ gpi @ gpt @ gpv @ gps @ gpw
    end
  | getpos_dec (A.DecClass (id, ts, r, l, _))             ll =
    let val gpi = getpos_labclass   id ll
	val gps = getpos_tyclassseq ts ll
	val gp  = L (r, getCol l ll, 1)
    in gp :: gpi @ gps
    end
  | getpos_dec (A.DecDots ptl)                            ll =
    getpos_partlist ptl ll

and getpos_declist [] _         = []
  | getpos_declist (x :: xs) ll = (getpos_dec x ll) @ (getpos_declist xs ll)

and getpos_decs (A.DecsDots pl)  ll = getpos_partlist pl ll
  | getpos_decs (A.Decs (dl, _)) ll = getpos_declist dl ll

and getpos_explist [] _         = []
  | getpos_explist (e :: el) ll = (getpos_exp e ll) @ (getpos_explist el ll)

and getpos_labexplist [] _         _ = []
  | getpos_labexplist (e :: el) ll x =
    let val gp = getpos_labexp e ll
	val fgpp = fn y => getpos_labexplist el ll y
    in case x of
	   NONE => gp @ (fgpp x)
	 | SOME ([], _, _) => raise EH.DeadBranch ""
	 | SOME (r :: rs, n, l) =>
	   (highlightTupleInRecClash r gp n l ll 1) @ (fgpp (SOME (rs, n + 1, l)))
    end

and getpos_labexp (A.LabExp (e, rl, re, l, _)) ll = (mapCol l ll rl) @ (getpos_exp e ll)
  | getpos_labexp (A.LabExpDots pl)            ll = getpos_partlist pl ll

and getpos_expfield (A.ExpField (tl, e, r, rlt, l, _)) ll =
    let val gp1 = getpos_tylab tl ll
	val gp2 = getpos_labexp e ll
	val gpp = mapRed rlt
	val gp  = (L (r, getCol l ll, 1))
    in gp :: gpp @ gp1 @ gp2
    end
  | getpos_expfield (A.ExpFieldDots pl) ll           = getpos_partlist pl ll

and getpos_expfieldlist [] _         = []
  | getpos_expfieldlist (x :: xs) ll = (getpos_expfield x ll) @ (getpos_expfieldlist xs ll)

and getpos_seqexp (A.SeqExp (el, e, r, rs, l, n)) ll =
    let val gp = getpos_labexplist (el @ [e]) ll NONE
    in (mapCol l ll (r :: rs)) @ gp
    end
  | getpos_seqexp (A.SeqExpSl (pl, e, r, l, n)) ll   =
    let val gp1 = getpos_partlist pl ll
	val gp2 = getpos_labexp e ll
    in (mapCol l ll [r]) @ gp1 @ gp2
    end
  | getpos_seqexp (A.SeqExpDots pl) ll               = getpos_partlist pl ll

and getpos_atexp (A.AtExpId id) ll                        = getpos_longid id ll
  | getpos_atexp (A.AtExpScon sc) ll                      = getpos_scon sc ll
  | getpos_atexp (A.AtExpTuple (el, rl, l, _)) ll         =
    let val regs = createRegsComponentsTuple rl (* NEW *)
	val gp   = getpos_labexplist el ll (SOME (regs, 1, l))
	val col  = unHighightTupleInRecClash l (getCol l ll) ll (* NEW *)
	val m    = fn x => (L (x, col, 1))
    in (map m rl) @ gp
    end
  | getpos_atexp (A.AtExpRecord (erl, rl1, rl2, l, _)) ll =
    let val gp = getpos_expfieldlist erl ll
    in (mapCol l ll (rl1 @ rl2)) @ gp
    end
  | getpos_atexp (A.AtExpSlRec (erl, rl, l, _)) ll =
    let val gp = getpos_expfieldlist erl ll
    in (mapCol l ll rl) @ gp
    end
  | getpos_atexp (A.AtExpLet (ds, e, rl, l, _)) ll        =
    let val gp1 = getpos_decs ds ll
	val gp2 = getpos_labexp e ll
    in (mapCol l ll rl) @ gp1 @ gp2
    end
  | getpos_atexp (A.AtExpDLet (ds, s, rl, l, _)) ll        =
    let val gp1 = getpos_decs ds ll
	val gp2 = getpos_seqexp s ll
    in (mapCol l ll rl) @ gp1 @ gp2
    end
  | getpos_atexp (A.AtExpParen (e, r1, r2, l, _)) ll      =
    let val gp = getpos_labexp e ll
    in (mapCol l ll [r1, r2]) @ gp
    end
  | getpos_atexp (A.AtExpList (el, rl, l, _)) ll          =
    let val gp = getpos_labexplist el ll NONE
    in (mapCol l ll rl) @ gp
    end
  | getpos_atexp (A.AtExpProj (tl, r, rt, l, n)) ll   =
    let (*val gpp = getpos_tylab tl ll*)
	val gp  = L (r, getCol l ll, 1)
	val gps = mapRed (Reg.getRegionList (Reg.getTo r) (Reg.getFrom rt))
	val gpp = case tl of
		      A.TyLabDots => [N (rt, Red, 1, getpos_tylab tl ll)]
		    | _             => getpos_tylab tl ll
    in gp :: gps @ gpp
    end
  | getpos_atexp (A.AtExpSeq (seq, rs, l, n)) ll          =
    let val gp = getpos_seqexp seq ll
    in (mapCol l ll rs) @ gp
    end
  | getpos_atexp (A.AtExpQuote (quotes, regs, lab, n)) ll =
    let val gp = getpos_quotes quotes ll
    in (mapCol lab ll regs) @ gp
    end
  | getpos_atexp (A.AtExpDots pl) ll                      = getpos_partlist pl ll

and getpos_quotes [] _ = []
  | getpos_quotes (quote :: quotes) ll =
    let val gp  = getpos_quote  quote  ll
	val gpp = getpos_quotes quotes ll
    in gp @ gpp
    end

and getpos_quote (A.Quote (_, reg, lab, _)) ll = [L (reg, getCol lab ll, 1)]
  | getpos_quote (A.Antiquote (exp, regs, lab, _)) ll =
    let val gp = getpos_exp exp ll
    in (mapCol lab ll regs) @ gp
    end
  | getpos_quote (A.QuoteDots parts) ll = getpos_partlist parts ll

and getpos_exp (A.ExpAtExp ae) ll                    = getpos_atexp ae ll
  | getpos_exp (A.ExpFn (m, r, l, _)) ll             =
    let val gp2 = (getpos_match m ll)
	val gp1 = (L (r, getCol l ll, 1))
    in gp1 :: gp2
    end
  | getpos_exp (A.ExpApp (e, a, rl, r1, r2, l, _)) ll =
    let val gp1 = case e of
		      (A.ExpDots []) => [N (r1, Red, 1, getpos_exp e ll)]
		    | _                => getpos_exp e ll
	val col = (*if Reg.isVisList rl then Red else*) getCol l ll
	val gp2 = case a of
		      (A.AtExpDots _) => [N (r2, col, 1, getpos_atexp a ll)]
		    | _                  => getpos_atexp a ll
	(* Before in the pattern it was: A.AtExpDots []. *)
    in (mapCol l ll rl) @ gp1 @ gp2
    end
  | getpos_exp (A.ExpCase (e, m, r1, r2, l, _)) ll   =
    let val gp1 = (getpos_labexp e ll)
	val gp2 = (getpos_match m ll)
    in (mapCol l ll [r1, r2]) @ gp1 @ gp2
    end
  | getpos_exp (A.ExpConsList (_, e1, e2, r, l, _)) ll  =
    let val gp1 = (getpos_labexp e1 ll)
	val gp2 = (getpos_labexp e2 ll)
	val gp = (L (r, getCol l ll, 1))
    in gp :: gp1 @ gp2
    end
  | getpos_exp (A.ExpOp (_, _, e1, e2, r, l, _)) ll  =
    let val gp1 = (getpos_labexp e1 ll)
	val gp2 = (getpos_labexp e2 ll)
	val gp = (L (r, getCol l ll, 1))
    in gp :: gp1 @ gp2
    end
  | getpos_exp (A.ExpOr (e1, e2, r, l, _)) ll  =
    let val gp1 = (getpos_labexp e1 ll)
	val gp2 = (getpos_labexp e2 ll)
	val gp = (L (r, getCol l ll, 1))
    in gp :: gp1 @ gp2
    end
  | getpos_exp (A.ExpAnd (e1, e2, r, l, _)) ll  =
    let val gp1 = (getpos_labexp e1 ll)
	val gp2 = (getpos_labexp e2 ll)
	val gp = (L (r, getCol l ll, 1))
    in gp :: gp1 @ gp2
    end
  | getpos_exp (A.ExpTyped (e, t, r, l, _)) ll       =
    let val gpe = getpos_labexp e ll
	val gpt = getpos_labtype t ll
	val gp = (L (r, getCol l ll, 1))
    in gp :: gpe @ gpt
    end
  | getpos_exp (A.ExpIte (e1, e2, e3, rl, l, _)) ll  =
    let val gp1 = getpos_labexp e1 ll
	val gp2 = getpos_labexp e2 ll
	val gp3 = getpos_labexp e3 ll
    in (mapCol l ll rl) @ gp1 @ gp2 @ gp3
    end
  | getpos_exp (A.ExpWhile (e1, e2, r1, r2, l, _)) ll =
    let val gp1 = getpos_labexp e1 ll
	val gp2 = getpos_labexp e2 ll
    in (mapCol l ll [r1, r2]) @ gp1 @ gp2
    end
  | getpos_exp (A.ExpRaise (e, r, l, n)) ll           =
    let val gpp = getpos_labexp e ll
	val gp  = L (r, getCol l ll, 1)
    in gp :: gpp
    end
  | getpos_exp (A.ExpHandle (e, m, r, l, n)) ll       =
    let val gp1 = getpos_labexp e ll
	val gp2 = getpos_match m ll
	val gp  = L (r, getCol l ll, 1)
    in gp :: gp1 @ gp2
    end
  | getpos_exp (A.ExpDots pl) ll                      = getpos_partlist pl ll

(* we don't care about these regions *)
and getpos_match (A.Match (l, rl, _)) ll = getpos_mrulelist l ll
  | getpos_match (A.MatchDots pl) ll     = getpos_partlist pl ll

and getpos_mrulelist [] _         = []
  | getpos_mrulelist (m :: ml) ll = (getpos_mrule m ll) @ (getpos_mrulelist ml ll)

and getpos_mrule (A.Mrule (p, e, r, l, _)) ll =
    let val gp1 = getpos_labpat p ll
	val gp2 = getpos_labexp e ll
	val gp = (L (r, getCol l ll, 1))
    in gp :: gp1 @ gp2
    end
  | getpos_mrule (A.MruleDots pl)  ll         = getpos_partlist pl ll

and getpos_patlist [] _         = []
  | getpos_patlist (p :: pl) ll = (getpos_pat p ll) @ (getpos_patlist pl ll)

and getpos_labpatlist []        _  _ = []
  | getpos_labpatlist (p :: pl) ll x =
    let val gp = getpos_labpat p ll
	val fgpp = fn y => getpos_labpatlist pl ll y
    in case x of
	   NONE => gp @ (fgpp x)
	 | SOME ([], _, _) => raise EH.DeadBranch ""
	 | SOME (r :: rs, n, l) =>
	   (highlightTupleInRecClash r gp n l ll 1) @ (fgpp (SOME (rs, n + 1, l)))
    end
(* The extra label above is the label of the node containing this list of patterns.
 * It is useful to highlight correctly a record clash in tuple cases.
 * The integer is to know the position in the list. *)

and getpos_labpat (A.LabPat (p, rl, rp, l, _)) ll =
    let val gp = getpos_pat p ll
    in (mapCol l ll rl) @ gp
    end
  | getpos_labpat (A.LabPatDots pl) ll = getpos_partlist pl ll

and getpos_identty (A.IdentTyId id)               ll = getpos_ident id ll
  | getpos_identty (A.IdentTyTy (id, t, r, l, _)) ll =
    let val gp1 = getpos_labid id ll
	val gp2 = getpos_labtype t ll
	val gp = L (r, getCol l ll, 1)
    in gp :: gp1 @ gp2
    end
  | getpos_identty (A.IdentTyDots pl)             ll = getpos_partlist pl ll

and getpos_labidty (A.LabIdTy (id, rl, l, _)) ll =
    let val gp = getpos_identty id ll
    in (mapCol l ll rl) @ gp
    end
  | getpos_labidty (A.LabIdTyDots pl) ll         = getpos_partlist pl ll

and getpos_patfield (A.PatField (tl, p, r, rl, l, _)) ll   =
    let val gp1 = getpos_tylab tl ll
	val gp2 = getpos_labpat p ll
	val gpp = mapCol l ll rl
	val gp  = (L (r, getCol l ll, 1))
    in gp :: gpp @ gp1 @ gp2
    end
  | getpos_patfield (A.PatFieldId (id, _)) ll          = getpos_identty id ll
  | getpos_patfield (A.PatFieldAs (id, p, r, l, _)) ll =
    let val gp1 = getpos_labidty id ll
	val gp2 = getpos_labpat p ll
	val gp = (L (r, getCol l ll, 1))
    in gp :: gp1 @ gp2
    end
  | getpos_patfield (A.PatFieldWild (r, l, _)) ll      = [(L (r, getCol l ll, 1))]
  | getpos_patfield (A.PatFieldDots pl) ll             = getpos_partlist pl ll

and getpos_patfieldlist xs ll = foldr (fn (x, y) => (getpos_patfield x ll) @ y) [] xs

and getpos_atpat (A.AtPatWild _) _                      = raise EH.DeadBranch ""
  | getpos_atpat (A.AtPatId id) ll                      = getpos_longid id ll
  | getpos_atpat (A.AtPatScon sc) ll                    = getpos_scon sc ll
  | getpos_atpat (A.AtPatTuple (pal, rl, l, _))      ll =
    let val regs = createRegsComponentsTuple rl (* NEW *)
	val gp   = getpos_labpatlist pal ll (SOME (regs, 1, l))
	val col  = unHighightTupleInRecClash l (getCol l ll) ll (* NEW *)
	val m    = fn x => (L (x, col, 1))
    in (map m rl) @ gp
    end
  | getpos_atpat (A.AtPatRecord (p, rl1, rl2, l, _)) ll =
    let val gp = getpos_patfieldlist p ll
    in (mapCol l ll (rl1 @ rl2)) @ gp
    end
  | getpos_atpat (A.AtPatParen (pa, r1, r2, l, _))   ll =
    let val gp = getpos_labpat pa ll
    in (mapCol l ll [r1, r2]) @ gp
    end
  | getpos_atpat (A.AtPatList (pal, rl, l, _))       ll =
    let val gp = getpos_labpatlist pal ll NONE
    in (mapCol l ll rl) @ gp
    end
  | getpos_atpat (A.AtPatOr (xs, rs, l, _))          ll =
    let val gp = getpos_labpatlist xs ll NONE
    in (mapCol l ll rs) @ gp
    end
  | getpos_atpat (A.AtPatDots pl) ll                    = getpos_partlist pl ll

and getpos_pat (A.PatAtPat atpat) ll                 = getpos_atpat atpat ll
  | getpos_pat (A.PatApp (id, a, rl, r, l, n')) ll   =
    let val gp1 = (getpos_longid id ll)
	val col = (*if Reg.isVisList rl then Red else*) getCol l ll
	val gp2 = case a of
		      (A.AtPatDots []) => [N (r, col, 1, getpos_atpat a ll)]
		    | _                  => getpos_atpat a ll
	(*val gp2 = (getpos_atpat a ll)*)
    in (mapCol l ll rl) @ gp1 @ gp2
    end
  | getpos_pat (A.PatConsList (_, p1, p2, r, l, _)) ll =
    let val gp1 = getpos_labpat p1 ll
	val gp2 = getpos_labpat p2 ll
	val gp = (L (r, getCol l ll, 1))
    in gp :: gp1 @ gp2
    end
  | getpos_pat (A.PatOp (_, _, p1, p2, r, l, _)) ll =
    let val gp1 = getpos_labpat p1 ll
	val gp2 = getpos_labpat p2 ll
	val gp = (L (r, getCol l ll, 1))
    in gp :: gp1 @ gp2
    end
  | getpos_pat (A.PatTyped (p, t, r, l, _)) ll      =
    let val gpp = getpos_labpat p ll
	val gpt = getpos_labtype t ll
	val gp = (L (r, getCol l ll, 1))
    in gp :: gpp @ gpt
    end
  | getpos_pat (A.PatAs (id, p, r, l, _)) ll        =
    let val gp1 = getpos_labidty id ll
	val gp2 = getpos_labpat p ll
	val gp  = (L (r, getCol l ll, 1))
    in gp :: gp1 @ gp2
    end
  | getpos_pat (A.PatDots pl) ll                    = getpos_partlist pl ll

end
