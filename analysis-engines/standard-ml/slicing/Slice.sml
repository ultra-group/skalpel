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
 *  o Authors:     Vincent Rahli
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        25 May 2010
 *  o File name:   Slicer.sml
 *  o Description: Defines the structure Slicing which has signature
 *      SLICING and contains our slicing algorithm.
 *)


structure Slicing :> SLICING = struct

(* abbreviate structures that we use *)
structure A  = AstSML
structure R  = Reg
structure L  = Label
structure D  = Debug
structure EH = ErrorHandler
structure SY = SymbSlice
structure PP = Ppp


(* printing section *)


(*┤├*)

val dots     = SY.dots      (* the string ".."*)
val ldots    = SY.ldots     (* mathematical left angle bracket *)
val ldotsLatex = SY.ldotsLatex     (* mathematical left angle bracket *)
val rdots    = SY.rdots     (* mathematcial right angle bracket *)
val rdotsLatex = SY.rdotsLatex     (* mathematcial right angle bracket *)
val splparen = SY.splparen  (* left white square bracket *)
val sprparen = SY.sprparen  (* right white square bracket *)

(* returns a five tuple of the five values listed above *)
fun getDots _ = (dots, ldots, rdots, splparen, sprparen)

fun toString sl =
    let val astst1 = PP.prettyPrintAst sl
	val astst2 = String.translate
			 (fn #"\"" => "\\\""
			   | #"\n" => ""
			   | #" "  => ""
			   | x     => Char.toString x)
			 astst1
	val astst3 = String.concatWith
			 ":"
			 (List.rev
			      (List.tl
				   (List.rev
					(String.fields
					     (fn #":" => true | _ => false)
					     astst2))
			       handle Empty => []))
    in astst3
    end

(* transforms a character into a string *)
fun transfun #"\t" = "\\t"
  | transfun #"\n" = "\\n"
  | transfun #"\\" = "\\"
  | transfun #"\"" = "\""
  | transfun x     = Char.toString x

(* printSlice' is the function which will print a program slice
 * slprog is the abstract syntax tree of the program
 * ident is the indentatation to be used when printing  (for readability)
 * sep is a boolean, if true we print separator strings (usually a space)
 *)
fun printSlice' slprog indent sep =
    let

	fun pind ind = if sep then ind ^ " "  else "" (* because a Parenthesis is 1 character *)
	fun dind ind = if sep then ind ^ "  " else "" (* because Dots are 2 characters        *)
	fun sind ind = if sep then ind ^ " "  else "" (* because a seq Parenthesis is 1 char  *)

	fun getLine []       = NONE
	  | getLine (r :: _) = SOME (R.getPosLine (R.getFrom r))

	fun getCol []       = NONE
	  | getCol (r :: _) = SOME (R.getPosCol (R.getFrom r))

	fun lengthToString n =
	    if n <= 1
	    then ""
	    else " " ^ lengthToString (n - 1)

	fun sepLines NONE     _        _        _   = ""
	  | sepLines _        NONE     _        _   = ""
	  | sepLines (SOME l) (SOME k) NONE     ind = 
	    if !(D.debugProgramLabelling)
	    then if l < k andalso sep
		 then "\\\\\n" ^ ind
		 else ""
	    else if l < k andalso sep
	    then "\n" ^ ind
	    else ""
	  | sepLines (SOME l) (SOME k) (SOME c) ind = 
	    if !(D.debugProgramLabelling)
	    then if l < k andalso sep
		 then "\\\\\n" ^ ind ^ lengthToString c
		 else ""
	    else if l < k andalso sep
	    then "\n" ^ ind ^ lengthToString c
	    else ""

	fun printProgs (A.Progs xs)                          ind = 
	    if !(D.debugProgramLabelling)
	    then ("\\documentclass[border=5pt]{article}\n " ^ 
		  "\\usepackage{amsmath}\n " ^
		  "\\usepackage{color}\n " ^
		  "\\usepackage{graphicx}\n " ^
		  "\\begin{document}\n " ^
		  "\\noindent " ^ printProgList xs ind)
	    else printProgList xs ind

	and printProgList []                                   _   = ""
	  | printProgList [(x, _, _, _)]                       ind = 
	    if !(D.debugProgramLabelling)
	    then ind ^ printProg x ind ^ "\n \\end{document}"
	    else ind ^ printProg x ind
	  | printProgList ((x, _, _, _) :: xs)                 ind = 
	    if !(D.debugProgramLabelling)
	    then ind ^ printProg x ind ^ "\n" ^ printProgList xs ind
	    else ind ^ printProg x ind ^ "\n" ^ printProgList xs ind

	and printProg (A.Prog xs)                            ind = #4 (printProgOneList xs ind)
	  | printProg (A.ProgDots pl)                        ind =
	    if !(D.debugProgramLabelling)
	    then ldotsLatex ^ #4 (printPartDots pl (pind ind)) ^ rdotsLatex
	    else ldots ^ #4 (printPartDots pl (pind ind)) ^ rdots

	and printProgOneList []                                _   = (NONE, NONE, NONE, "")
	  | printProgOneList (x :: xs)                         ind =
	    let
		val (l, k, c, x) = printProgOne x ind
		val (i, j, d, y) = printProgOneList xs ind
		val sep = sepLines k i d ind
	    in (l, j, c, "\\scalebox{0.5}{$ "^ x ^ "$}" ^ sep ^ y )
	    end

	and printAFile (A.AFile (f, r, l, _))                ind =
	    if !(D.debugProgramLabelling)
	    then (getLine [r], getLine [r], getCol [r], D.printLabelledProgramString("\\operatorname{"^f^"}")^"^{"^L.printLab(l)^"}")
	    else (getLine [r], getLine [r], getCol [r], f)
	  | printAFile A.AFileDots                           _   =
	    if !(D.debugProgramLabelling)
	    then (NONE, NONE, NONE, ldotsLatex ^ dots ^ rdotsLatex)
	    else (NONE, NONE, NONE, ldots ^ dots ^ rdots)

	and printABool (A.ABool (b, r, l, _))                ind =
	    if !(D.debugProgramLabelling)
	    then (getLine [r], getLine [r], getCol [r], D.printLabelledProgramString("\\operatorname{"^b^"}")^"^{"^L.printLab(l)^"}")
	    else (getLine [r], getLine [r], getCol [r], b)
	  | printABool A.ABoolDots                           _   =
	    if !(D.debugProgramLabelling)
	    then (NONE, NONE, NONE, ldotsLatex ^ dots ^ rdotsLatex)
	    else (NONE, NONE, NONE, ldots ^ dots ^ rdots)

	and printProgOne (A.ProgOneDec td)                   ind = printTopDec td ind
	  | printProgOne (A.ProgOneExp (e, _, _, label, _))      ind =
	    let
		val (l, k, c, x) = printExp e ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, D.printLabelledProgramString("\\operatorname{" ^ x ^ "}\\operatorname{;}")^"^{" ^ L.printLab(label) ^ "}")
		else (l, k, c, x ^ ";")
	    end
	  | printProgOne (A.ProgOneParse _)                  _   =
	    if !(D.debugProgramLabelling)
	    then (NONE, NONE, NONE, ldotsLatex ^ "\\operatorname{unparsable}" ^ rdotsLatex)
	    else (NONE, NONE, NONE, ldots ^ "unparsable" ^ rdots)
	  | printProgOne (A.ProgOneFile (af, _))             ind =
	    let
		val (l, k, c, x) = printAFile af ind
	    in (l, k, c, x)
	    end
	  | printProgOne (A.ProgOneDots pl)                  ind =
	    let
		val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printTopDec (A.TopDec xs)                        ind = printTopDecOneList xs ind
	  | printTopDec (A.TopDecDots pl)                    ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printTopDecOneList []                              _   = (NONE, NONE, NONE, "")
	  | printTopDecOneList (x :: xs)                       ind =
	    let val (l, k, c, x) = printTopDecOne x ind
		val (i, j, d, y) = printTopDecOneList xs ind
		val j'  = case j of NONE => k | _ => j
		val sep = sepLines k i d ind
	    in (l, j', c, x ^ sep ^ y)
	    end

	and printTopDecOne (A.TopDecOneTes (x, _))           ind = printSmlTes x ind
	  | printTopDecOne (A.TopDecOneDec (x, _))           ind = printATopDec x ind
	  | printTopDecOne (A.TopDecOneDots pl)              ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in 
		if !(D.debugProgramLabelling)
		then (l, k, c, ldots ^ x ^ rdots)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printSmlTes (A.SmlTesDec (s, rs, _))             ind =
	    let val (l, k, c, x) = printATopDec s ind
		val k' = case k of NONE => getLine rs | _ => k
	    in
		if !(D.debugProgramLabelling)
		then (getLine rs, k', getCol rs, "\\operatorname{(**SKALPEL-DEC " ^ x ^ "*)}")
		else (getLine rs, k', getCol rs, "(**SKALPEL-DEC " ^ x ^ "*)")
	    end
	  | printSmlTes (A.SmlTesSpec (s, rs, _))            ind =
	    let val (l, k, c, x) = printSpec s ind
		val k' = case k of NONE => getLine rs | _ => k
	    in
		if !(D.debugProgramLabelling)
		then (getLine rs, k', getCol rs, "\\operatorname{(**SKALPEL-SPEC " ^ x ^ "*)}")
		else (getLine rs, k', getCol rs, "(**SKALPEL-SPEC " ^ x ^ "*)")
	    end
	  | printSmlTes (A.SmlTesUse (af, rs, _))            ind =
	    let val (l, k, c, x) = printAFile af ind
		val k' = case k of NONE => getLine rs | _ => k
	    in
		if !(D.debugProgramLabelling)
		then (getLine rs, k', getCol rs, "\\operatorname{(**SKALPEL-USE-FILE " ^ x ^ "*)}")
		else (getLine rs, k', getCol rs, "(**SKALPEL-USE-FILE " ^ x ^ "*)")
	    end
	  | printSmlTes (A.SmlTesSBas (af, rs, _))           ind =
	    let val (l, k, c, x) = printAFile af ind
		val k' = case k of NONE => getLine rs | _ => k
	    in
		if !(D.debugProgramLabelling)
		then (getLine rs, k', getCol rs, "\\operatorname{(**SKALPEL-SET-BASIS " ^ x ^ "*)}")
		else (getLine rs, k', getCol rs, "(**SKALPEL-SET-BASIS " ^ x ^ "*)")
	    end
	  | printSmlTes (A.SmlTesCBas (rs, _, _))            ind =
	    if !(D.debugProgramLabelling)
	    then (getLine rs, getLine rs, getCol rs, "\\operatorname{(**SKALPEL-CLEAR-BASIS*)}")
	    else (getLine rs, getLine rs, getCol rs, "(**SKALPEL-CLEAR-BASIS*)")
	  | printSmlTes (A.SmlTesQuote (b, rs, _))            ind =
	    let val (l, k, c, x) = printABool b ind
		val k' = case k of NONE  => getLine rs | _ => k
	    in
		if !(D.debugProgramLabelling)
		then (getLine rs, k', getCol rs, "\\operatorname{(**SKALPEL-QUOTATION " ^ x ^ "*)}")
		else (getLine rs, k', getCol rs, "(**SKALPEL-QUOTATION " ^ x ^ "*)")
	    end
	  | printSmlTes (A.SmlTesType (st, rs, _))           ind =
	    if !(D.debugProgramLabelling)
	    then (getLine rs, getLine rs, getCol rs, "\\operatorname{(**SKALPEL-TYPE " ^ st ^ "*)}")
	    else (getLine rs, getLine rs, getCol rs, "(**SKALPEL-TYPE " ^ st ^ "*)")
	  | printSmlTes (A.SmlTesDots pl)                    ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printATopDec (A.ATopDecSig s)                  ind = printSigDec s ind
	  | printATopDec (A.ATopDecStr s)                  ind = printStrDec s ind
	  (*| printATopDec (A.TopDecOneFun f)                ind = printFunDec f ind*)
	  | printATopDec (A.ATopDecDots pl)                ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	(*and printFunDec (A.FunDec (fb, r, _, _))             ind =
	    let val (_, k, _, x) = printFunBind fb ind
	    in (getLine [r], k, getCol [r], "functor " ^ x)
	    end
	  | printFunDec (A.FunDecDots pl)                    ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
if !(D.debugProgramLabelling)
then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
else (l, k, c, ldots ^ x ^ rdots)
	    end*)

	and printFunBind (A.FunBind (fbol, _, _))            ind =
	    let val (l, k, c, x) = printFunBindOneList fbol (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdotsLatex)
	    end
	  | printFunBind (A.FunBindDots pl)                  ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end
		
	and printFunBindOneList []                             _   = (NONE, NONE, NONE, dots)
	  | printFunBindOneList (x :: xs)                      ind =
	    let val (l, k, c, x) = printFBO x (dind ind)
		val (i, j, d, y) = printFunBindOneList xs ind
		val sep = sepLines k i d ind
	    in (l, j, c, dots ^ x ^ sep ^ y)
	    end

	and printFBO (A.FunBindO (i, j, si, se, _, label, _))    ind =
	    let val (l, k, c, x) = printFunId     i  ind
		val (_, _, _, y) = printStrId     j  ind
		val (_, _, _, z) = printLabSigExp si ind
		val (_, j, _, w) = printLabStrExp se ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ " \\operatorname{(}" ^ y ^ " \\operatorname{:} "   ^ z ^ "\\operatorname{) = }"  ^ w ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ " (" ^ y ^ " : "   ^ z ^ ") = "  ^ w)
	    end
	  | printFBO (A.FunBindOO (i, j, s, t, se, _, label, _)) ind =
	    let val (l, k, c, x) = printFunId     i  ind
		val (_, _, _, y) = printStrId     j  ind
		val (_, _, _, z) = printLabSigExp s  ind
		val (_, _, _, w) = printLabSigExp t  ind
		val (_, j, _, u) = printLabStrExp se ind
	    in
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ " [\\operatorname{(}" ^ y ^ " \\operatorname{:} " ^ z ^ "\\operatorname{) :> }" ^ w ^ "\\operatorname{ = }" ^ u ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ " (" ^ y ^ " : " ^ z ^ ") :> " ^ w ^ " = " ^ u)
	    end
	  | printFBO (A.FunBindOT (i, j, s, t, se, _, label, _)) ind =
	    let val (l, k, c, x) = printFunId     i  ind
		val (_, _, _, y) = printStrId     j  ind
		val (_, _, _, z) = printLabSigExp s  ind
		val (_, _, _, w) = printLabSigExp t  ind
		val (_, j, _, u) = printLabStrExp se ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ " \\opeartorname{(}" ^ y ^ " \\operatorname{:} " ^ z ^ "\\operatorname{) :} " ^ w ^ " \\operatorname{=} " ^ u ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ " (" ^ y ^ " : " ^ z ^ ") : " ^ w ^ " = " ^ u)
	    end
	  | printFBO (A.FunBindOS (i, p, se, _, label, _))       ind =
	    let val (l, k, c, x) = printFunId     i  ind
		val (_, _, _, y) = printSpec      p  ind
		val (_, j, _, z) = printLabStrExp se ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ " [\\operatorname{(}" ^ y ^ "\\operatorname{) =} "  ^ z ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ " (" ^ y ^ ") = "  ^ z)
	    end
	  | printFBO (A.FunBindOSO (i, p, si, se, _, label, _))  ind =
	    let val (l, k, c, x) = printFunId     i  ind
		val (_, _, _, y) = printSpec      p  ind
		val (_, _, _, z) = printLabSigExp si ind
		val (_, j, _, w) = printLabStrExp se ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ " \\operatorname{(}" ^ y ^ "\\operatorname{) :>} " ^ z ^ " \\operatorname{=} " ^ w ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ " (" ^ y ^ ") :> " ^ z ^ " = " ^ w)
	    end
	  | printFBO (A.FunBindOST (i, p, si, se, _, label, _))  ind =
	    let val (l, k, c, x) = printFunId     i  ind
		val (_, _, _, y) = printSpec      p  ind
		val (_, _, _, z) = printLabSigExp si ind
		val (_, j, _, w) = printLabStrExp se ind
	    in
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ " \\operatorname{(}" ^ y ^ "\\operatorname{) :} " ^ z ^ " \\operatorname{=} " ^ w ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ " \\operatorname{(}" ^ y ^ "\\operatorname{) :} " ^ z ^ " \\operatorname{=} " ^ w ^ "")
	    end
	  | printFBO (A.FunBindODots pl)                     ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printSigDec (A.SigDec (sb, r, _))                ind =
	    let val (_, k, _, x) = printSigBind sb ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r], k, getCol [r], "\\operatorname{signature} " ^ x)
		else (getLine [r], k, getCol [r], "signature " ^ x)
	    end
	  | printSigDec (A.SigDecDots pl)                    ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printSigBind (A.SigBind (sbol, _, _))            ind =
	    let val (l, k, c, x) = printSigBindOneList sbol (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end
	  | printSigBind (A.SigBindDots pl)                  ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printSigBindOneList []                             _   = (NONE, NONE, NONE, dots)
	  | printSigBindOneList (x :: xs)                      ind =
	    let val (l, k, c, x) = printSigBindOne x (dind ind)
		val (i, j, d, y) = printSigBindOneList xs ind
		val j'  = case j of NONE => k | _ => j
		val sep = sepLines k i d ind
	    in (l, j', c, dots ^ x ^ sep ^ y)
	    end

	and printSigBindOne (A.SigBindOne (id, se, r, label, _)) ind =
	    let val (l1, k1, c1, x) = printSigId     id ind
		val (l2, k2, c2, y) = printLabSigExp se ind
		val k2'  = case k2 of NONE => getLine [r] | _ => k2
		val sep1 = sepLines k1 (getLine [r]) (getCol [r]) ind
		val sep2 = sepLines (getLine [r]) l2 c2 ind
		val esep = case sep1 of "" => " " | _ => ""
	    in 
		if !(D.debugProgramLabelling)
		then (l1, k2', c1, D.printLabelledProgramString( x ^ sep1 ^ esep ^ "\\operatorname{=} " ^ sep2 ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l1, k2', c1, x ^ sep1 ^ esep ^ "= " ^ sep2 ^ y)
	    end
	  | printSigBindOne (A.SigBindOneDots pl)            ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printStrDec (A.StrDec (xs, label, _))            ind =
	    let
		val (i, j, d, y) = printStrDecOneList xs ind
	    in
		if !(D.debugProgramLabelling)
		then (i, j, d, D.printLabelledProgramString( y ^ "")^"^{" ^ L.printLab(label) ^ "}")
		else (i, j, d, y)
	    end
	  | printStrDec (A.StrDecDots pl)                    ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printStrDecOneList []                              _   = (NONE, NONE, NONE, "")
	  | printStrDecOneList (x :: xs)                       ind =
	    let val (l, k, c, x) = printStrDecOne x ind
		val (i, j, d, y) = printStrDecOneList xs ind
		val j'  = case j of NONE => k | _ => j
		val sep = sepLines k i d ind
	    in (l, j', c, x ^ sep ^ y)
	    end

	and printStrDecOne (A.StrDecOneDec d)                ind =
	    let val (l, k, c, x) = printDecs d ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, k, c, x ^ "\\operatorname{;}") (* semicolon? *)
		else (l, k, c, x ^ ";")
	    end
	  | printStrDecOne (A.StrDecOneStr (sb, r, _))       ind =
	    let val (l, k, c, x) = printStrBind sb ind
		val k'  = case k of NONE => getLine [r] | _ => k
		val sep = sepLines (getLine [r]) l c ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r], k', getCol [r], "\\operatorname{structure} " ^ sep ^ x ^ "\\operatorname{;}") (* semicolon? *)
		else (getLine [r], k', getCol [r], "structure " ^ sep ^ x ^ ";") (* semicolon? *)
	    end
	  | printStrDecOne (A.StrDecOneFun (fb, r, label, _))    ind =
	    let val (l, k, c, x) = printFunBind fb ind
		val k'  = case k of NONE => getLine [r] | _ => k
		val sep = sepLines (getLine [r]) l c ind
	    in
		if !(D.debugProgramLabelling)
		then (getLine [r], k', getCol [r], D.printLabelledProgramString("\\operatorname{functor} " ^ sep ^ x ^ "\\operatorname{;}" ^ "")^"^{" ^ L.printLab(label)^"}") (* semicolon? *)
		else (getLine [r], k', getCol [r], "functor " ^ sep ^ x ^ ";") (* semicolon? *)
	    end
	  | printStrDecOne (A.StrDecOneLoc (s, t, rs, label, _)) ind =
	    let val (r1, r2, r3) =
		    case rs of
			[r1, r2, r3] => (r1, r2, r3)
		      | _ => raise EH.DeadBranch "not the correct number of regions"
		val (l1, k1, c1, x) = printStrDec s ind
		val (l2, k2, c2, y) = printStrDec t ind
		val k1'  = case k1 of NONE => getLine [r1] | _ => k1
		val k2'  = case k2 of NONE => getLine [r2] | _ => k2
		val sep1 = sepLines (getLine [r1]) l1 c1 ind
		val sep2 = sepLines k1' (getLine [r2]) (getCol [r2]) ind
		val sep3 = sepLines (getLine [r2]) l2 c2 ind
		val sep4 = sepLines k2' (getLine [r3]) (getCol [r3]) ind
		val isep = case sep2 of "" => " " | _ => ""
		val esep = case sep4 of "" => " " | _ => ""
	    in 
		if !(D.debugProgramLabelling)
		then (getLine rs,
		getLine (rev rs),
		getCol rs,
		D.printLabelledProgramString("\\operatorname{local} " ^ sep1 ^ x ^ sep2 ^ isep ^
		"\\operatorname{in} "    ^ sep3 ^ y ^ sep4 ^ esep ^
		"\\operatorname{end}" ^
		"")^"^{" ^ L.printLab(label)^"}")
		else (getLine rs,
		getLine (rev rs),
		getCol rs,
		"local " ^ sep1 ^ x ^ sep2 ^ isep ^
		"in "    ^ sep3 ^ y ^ sep4 ^ esep ^
		"end")
	    end
	  | printStrDecOne (A.StrDecOneDots pl)              ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printStrBind (A.StrBind (xs, _, _))              ind =
	    let val (l, k, c, x) = printStrBindOneList xs (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end
	  | printStrBind (A.StrBindDots pl)                  ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printStrBindOneList []                             _   = (NONE, NONE, NONE, dots)
	  | printStrBindOneList (x :: xs)                      ind =
	    let val (l, k, c, x) = printStrBO x (dind ind)
		val (i, j, d, y) = printStrBindOneList xs ind
		val sep = sepLines k i d ind
	    in (l, j, c, dots ^ x ^ sep ^ y)
	    end

	and printStrBO (A.StrBindOneOp (id, i, e, rs, label, _))  ind =
	    let val (r1, r2) =
		    case rs of
			[r1, r2] => (r1, r2)
		      | _ => raise EH.DeadBranch "not the correct number of regions"
		val (l1, k1, c1, x) = printStrId     id ind
		val (l2, k2, c2, y) = printLabSigExp i  ind
		val (l3, k3, c3, z) = printLabStrExp e  ind
		val k2'  = case k2 of NONE => getLine [r1] | _ => k2
		val k3'  = case k2 of NONE => getLine [r2] | _ => k3
		val sep1 = sepLines k1 (getLine [r1]) (getCol [r1]) ind
		val sep2 = sepLines (getLine [r1]) l2 c2 ind
		val sep3 = sepLines k2' (getLine [r2]) (getCol [r2]) ind
		val sep4 = sepLines (getLine [r2]) l3 c3 ind
	    in 
		if !(D.debugProgramLabelling)
		then (l1, k3', c1,
		      D.printLabelledProgramString( x ^ sep1 ^ " \\operatorname{:>} " ^ sep2 ^ y ^ sep3 ^ " \\operatorname{=} " ^ sep4 ^ z ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l1, k3', c1,
		      x ^ sep1 ^ " :> " ^ sep2 ^ y ^ sep3 ^ " = " ^ sep4 ^ z)
	    end
	  | printStrBO (A.StrBindOneTr (id, i, e, rs, label, _))  ind =
	    let val (r1, r2) =
		    case rs of
			[r1, r2] => (r1, r2)
		      | _ => raise EH.DeadBranch "not the correct number of regions"
		val (l1, k1, c1, x) = printStrId     id ind
		val (l2, k2, c2, y) = printLabSigExp i  ind
		val (l3, k3, c3, z) = printLabStrExp e  ind
		val k2'  = case k2 of NONE => getLine [r1] | _ => k2
		val k3'  = case k2 of NONE => getLine [r2] | _ => k3
		val sep1 = sepLines k1 (getLine [r1]) (getCol [r1]) ind
		val sep2 = sepLines (getLine [r1]) l2 c2 ind
		val sep3 = sepLines k2' (getLine [r2]) (getCol [r2]) ind
		val sep4 = sepLines (getLine [r2]) l3 c3 ind
	    in 
		if !(D.debugProgramLabelling)
		then (l1, k3', c1, D.printLabelledProgramString( x ^ sep1 ^ " \\operatorname{:} " ^ sep2 ^ y ^ sep3 ^ " \\operatorname{=} " ^ sep4 ^ z ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l1, k3', c1, x ^ sep1 ^ " : " ^ sep2 ^ y ^ sep3 ^ " = " ^ sep4 ^ z)
	    end
	  | printStrBO (A.StrBindOne (id, se, r, label, _))      ind =
	    let val (l1, k1, c1, x) = printStrId     id ind
		val (l2, k2, c2, y) = printLabStrExp se ind
		val k2'  = case k2 of NONE => getLine [r] | _ => k2
		val sep1 = sepLines k1 (getLine [r]) (getCol [r]) ind
		val sep2 = sepLines (getLine [r]) l2 c2 ind
	    in 
		if !(D.debugProgramLabelling)
		then (l1, k2', c1, D.printLabelledProgramString( x ^ sep1 ^ " \\operatorname{=} " ^ sep2 ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l1, k2', c1, x ^ sep1 ^ " = " ^ sep2 ^ y)
	    end
	  | printStrBO (A.StrBindOneDots pl)                 ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printAstLTReaDO (A.LTReaDOne (d, t, rs, label, _))   ind =
	    let val (r1, r2) =
		    case rs of
			[r1, r2] => (r1, r2)
		      | _ => raise EH.DeadBranch "not the correct number of regions"
		val (l1, k1, c1, x) = printLDatName d ind
		val (l2, k2, c2, y) = printLabType t ind
		val k1'  = case k1 of NONE => getLine [r1] | _ => k1
		val k2'  = case k2 of NONE => getLine [r2] | _ => k2
		val sep1 = sepLines (getLine [r1]) l1 c1 ind
		val sep2 = sepLines k1' (getLine [r2]) (getCol [r2]) ind
		val sep3 = sepLines (getLine [r2]) l2 c2 ind
	    in 
		if !(D.debugProgramLabelling)
		then (l1, k2', c1, D.printLabelledProgramString("\\operatorname{type} " ^ sep1 ^ x ^ sep2 ^ " \\operatorname{=} " ^ sep3 ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l1, k2', c1, "type " ^ sep1 ^ x ^ sep2 ^ " = " ^ sep3 ^ y)
	    end
	  | printAstLTReaDO (A.LTReaDOneDots pl)             ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printAstLTReaDOneList []                           _   = (NONE, NONE, NONE, dots)
	  | printAstLTReaDOneList (x :: xs)                    ind =
	    let val (l, k, c, x) = printAstLTReaDO x (dind ind)
		val (i, j, d, y) = printAstLTReaDOneList xs ind
		val j'  = case j of NONE => k | _ => j
		val sep = sepLines l k c ind
	    in (l, j', c, dots ^ x ^ sep ^ y)
	    end

	and printLTReaDesc (A.LTReaDesc (xs, _, label, _))         ind =
	    let val (l, k, c, x) = printAstLTReaDOneList xs (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, D.printLabelledProgramString( ldotsLatex ^ x ^ rdotsLatex ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, k, c, ldots ^ x ^ rdots)
	    end
	  | printLTReaDesc (A.LTReaDescDots pl)                ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	(*and printAstTReaDO (A.TReaDOne (d, t, r, _, _))      ind =
	    let val (l, k, c, x) = printDatName d ind
		val (_, j, _, y) = printLabType t ind
	    in (l, j, c, x ^ " = " ^ y)
	    end
	  | printAstTReaDO (A.TReaDOneDots pl)               ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
if !(D.debugProgramLabelling)
then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
else (l, k, c, ldots ^ x ^ rdots)
	    end*)

	(*and printAstTReaDOneList []                            _   = (NONE, NONE, NONE, dots)
	  | printAstTReaDOneList (x :: xs)                     ind =
	    let val (l, k, c, x) = printAstTReaDO x (dind ind)
		val (i, j, d, y) = printAstTReaDOneList xs ind
		val j'  = case j of NONE => k | _ => j
		val sep = sepLines l k c ind
	    in (l, j', c, dots ^ x ^ sep ^ y)
	    end

	and printTReaDesc (A.TReaDesc (xs, _, _))            ind =
	    let val (l, k, c, x) = printAstTReaDOneList xs (pind ind)
	    in
if !(D.debugProgramLabelling)
then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
else (l, k, c, ldots ^ x ^ rdots)
	    end
	  | printTReaDesc (A.TReaDescDots pl)                ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
if !(D.debugProgramLabelling)
then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
else (l, k, c, ldots ^ x ^ rdots)
	    end*)

	and printLabSigExp (A.LabSigExp (e, _, _, label, _))     ind =
	    let
		val (x1, x2, x3, x4) = printSigExp e ind
	    in
		if !(D.debugProgramLabelling)
		then (x1, x2, x3, D.printLabelledProgramString( x4 ^ "")^"^{" ^ L.printLab(label)^"}")
		else (x1, x2, x3, x4)
	    end
	  | printLabSigExp (A.LabSigExpDots pl)              ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printSigExp (A.SigExpBasic (sp, rs, label, _))       ind =
	    let val (r1, r2) =
		    case rs of
			[r1, r2] => (r1, r2)
		      | _ => raise EH.DeadBranch "not the correct number of regions"
		val (l, k, c, x) = printSpec sp ind
		val k'   = case k of NONE => getLine [r1] | _ => k
		val sep1 = sepLines (getLine [r1]) l c ind
		val sep2 = sepLines k' (getLine [r2]) (getCol [r2]) ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r1], getLine [r2], getCol [r1],
		      D.printLabelledProgramString("\\operatorname{sig} " ^ sep1 ^ x ^ sep2 ^ " \\operatorname{end}" ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine [r1], getLine [r2], getCol [r1],
		      "sig " ^ sep1 ^ x ^ sep2 ^ " end")
	    end
	  | printSigExp (A.SigExpId (id, label, _))              ind =
	    let
		val (x1,x2,x3,x4) = printSigId id ind
	    in
		if !(D.debugProgramLabelling)
		then (x1,x2,x3, D.printLabelledProgramString( x4 ^ "")^"^{" ^ L.printLab(label)^"}")
		else (x1,x2,x3, x4)
	    end
	  | printSigExp (A.SigExpRea (se, rea, rs, label, _))    ind =
	    let val r =
		    case rs of
			[r] => r
		      | _ => raise EH.DeadBranch "not the correct number of regions"
		val (l1, k1, c1, x) = printLabSigExp se  ind
		val (l2, k2, c2, y) = printLTReaDesc rea ind
		val sep1 = sepLines l1 (getLine [r]) (getCol [r]) ind
		val sep2 = sepLines (getLine [r]) k2 c2 ind
	    in 
		if !(D.debugProgramLabelling)
		then (l1, k2, c1,
		      D.printLabelledProgramString( x ^ sep1 ^ " \\operatorname{where} " ^ sep2 ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l1, k2, c1,
		      x ^ sep1 ^ " where " ^ sep2 ^ y)
	    end
	  | printSigExp (A.SigExpDots pl)                    ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printLabStrExp (A.LabStrExp (e, _, _, _, _))     ind = printStrExp e ind
	  | printLabStrExp (A.LabStrExpDots pl)              ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printStrExp (A.StrExpBasic (sd, rs, label, _))       ind =
	    let val (r1, r2) =
		    case rs of
			[r1, r2] => (r1, r2)
		      | _ => raise EH.DeadBranch "not the correct number of regions"
		val (l, k, c, x) = printStrDec sd ind
		val k'   = case k of NONE => getLine [r1] | _ => k
		val sep1 = sepLines (getLine [r1]) l c ind
		val sep2 = sepLines k' (getLine [r2]) (getCol [r2]) ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r1], getLine [r2], getCol [r1], D.printLabelledProgramString("\\operatorname{struct} " ^ sep1 ^ x ^ sep2 ^ " \\operatorname{end}" ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine [r1], getLine [r2], getCol [r1], "struct " ^ sep1 ^ x ^ sep2 ^ " end") 
	    end
	  | printStrExp (A.StrExpId (id, label, _))              ind =
	    let
		val (x1, x2, x3, x4) = printLongStrId id ind
	    in 
		if !(D.debugProgramLabelling)
		then (x1, x2, x3, D.printLabelledProgramString( x4 ^ "")^"^{" ^ L.printLab(label)^"}")
		else (x1, x2, x3, x4)
	    end
	  | printStrExp (A.StrExpOp (se, si, _, label, _))       ind =
	    let val (l, k, c, x) = printLabStrExp se ind
		val (_, j, _, y) = printLabSigExp si ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ " \\operatorname{:>} " ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ " :> " ^ y)
	    end
	  | printStrExp (A.StrExpTr (se, si, _, label, _))       ind =
	    let val (l, k, c, x) = printLabStrExp se ind
		val (_, j, _, y) = printLabSigExp si ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ " \\operatorname{:} " ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ " : " ^ y)
	    end
	  | printStrExp (A.StrExpFExp (id, se, _, label, _))     ind =
	    let val (l, k, c, x) = printFunId     id ind
		val (_, j, _, y) = printLabStrExp se ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x  ^ "\\operatorname{(}" ^ y ^ "\\operatorname{)}" ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x  ^ "(" ^ y ^ ")")
	    end
	  | printStrExp (A.StrExpFDec (id, sd, _, label, _))     ind =
	    let val (l, k, c, x) = printFunId id ind
		val (_, j, _, y) = printStrDec sd ind
	    in
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x  ^ "(" ^ y ^ ")" ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x  ^ "(" ^ y ^ ")")
	    end
	  | printStrExp (A.StrExpLocal (sd, se, rs, label, _))   ind =
	    let val (_, _, _, x) = printStrDec    sd ind
		val (_, _, _, y) = printLabStrExp se ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine rs, getLine (rev rs), getCol rs, D.printLabelledProgramString("\\operatorname{let} " ^ x ^ " \\operatorname{in} " ^ y ^ " \\operatorname{end}" ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine rs, getLine (rev rs), getCol rs, "let " ^ x ^ " in " ^ y ^ " end")
	    end
	  | printStrExp (A.StrExpDots pl)                    ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printSpec (A.Spec (spol, _))                     ind =
	    let val (l, k, c, x) = printSpecOneList spol (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end
	  | printSpec (A.SpecDots pl)                        ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printSpecOneList []                                _   = (NONE, NONE, NONE, "")
	  | printSpecOneList (x :: xs)                         ind =
	    let val (l, k, c, x) = printSpecOne x ind
		val (i, j, d, y) = printSpecOneList xs ind
		val j'  = case j of NONE => k | _ => j
		val sep = sepLines k i d ind
	    in (l, j', c, x ^ sep ^ y)
	    end

	and printLongTyConEqList []                            _   = (NONE, NONE, NONE, "")
	  | printLongTyConEqList [x]                           ind =
	    let val (l, k, c, x) = printLongTyCon x ind
	    in (l, k, c, x)
	    end
	  | printLongTyConEqList (x :: xs)                     ind =
	    let val (l, k, c, x) = printLongTyCon x ind
		val (i, j, d, y) = printLongTyConEqList xs ind
		val j'  = case j of NONE => k | _ => j
	    in 
		if !(D.debugProgramLabelling)
		then (l, j', c, x ^ " \\operatorname{=} " ^ y)
		else (l, j', c, x ^ " = " ^ y)
	    end

	and printLongTyConEq (A.LongTyConEq (xs, rs, label, _))    ind =
	    let val (l, k, c, x) = printLongTyConEqList xs ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, D.printLabelledProgramString( x ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, k, c, x)
	    end
	  | printLongTyConEq (A.LongTyConEqDots pl)            ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printLongStrIdEqList []                            _   = (NONE, NONE, NONE, "")
	  | printLongStrIdEqList [x]                           ind =
	    let val (l, k, c, x) = printLongStrId x ind
	    in (l, k, c, x)
	    end
	  | printLongStrIdEqList (x :: xs)                     ind =
	    let val (l, k, c, x) = printLongStrId x ind
		val (i, j, d, y) = printLongStrIdEqList xs ind
		val j'  = case j of NONE => k | _ => j
	    in 
		if !(D.debugProgramLabelling)
		then (l, j', c, x ^ " \\operatorname{=} " ^ y)
		else (l, j', c, x ^ " = " ^ y)
	    end

	and printLongStrIdEq (A.LongStrIdEq (xs, rs, _))     ind =
	    let val (l, k, c, x) = printLongStrIdEqList xs ind
	    in (l, k, c, x)
	    end
	  | printLongStrIdEq (A.LongStrIdEqDots pl)          ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printSpecOne (A.SpecValue (vd, r, label, _))           ind =
	    let val (l, k, c, x) = printValDesc vd ind
		val k'  = case k of NONE => getLine [r] | _ => k
		val sep = sepLines (getLine [r]) l c ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r], k', getCol [r], D.printLabelledProgramString("\\operatorname{val} " ^ sep ^ x ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine [r], k', getCol [r], "val " ^ sep ^ x)
	    end
	  | printSpecOne (A.SpecType (td, r, label, _))           ind =
	    let val (l, k, c, x) = printTypDesc td ind
		val k'  = case k of NONE => getLine [r] | _ => k
		val sep = sepLines (getLine [r]) l c ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r], k', getCol [r], D.printLabelledProgramString("\\operatorname{type} " ^ sep ^ x ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine [r], k', getCol [r], "type " ^ sep ^ x)
	    end
	  | printSpecOne (A.SpecEqtype (td, r, label, _))           ind =
	    let val (l, k, c, x) = printTypDesc td ind
		val k'  = case k of NONE => getLine [r] | _ => k
		val sep = sepLines (getLine [r]) l c ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r], k', getCol [r], D.printLabelledProgramString("\\operatorname{eqtype} " ^ sep ^ x ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine [r], k', getCol [r], "eqtype " ^ sep ^ x)
	    end
	  | printSpecOne (A.SpecException (ed, r, label, _))           ind =
	    let val (l, k, c, x) = printExcDesc ed ind
		val k'  = case k of NONE => getLine [r] | _ => k
		val sep = sepLines (getLine [r]) l c ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r], k', getCol [r], D.printLabelledProgramString("\\operatorname{exception} " ^ sep ^ x ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine [r], k', getCol [r], "exception " ^ sep ^ x)
	    end
	  | printSpecOne (A.SpecTdr (td, r, label, _))           ind =
	    let val (l, k, c, x) = printTdrDesc td ind
		val k'  = case k of NONE => getLine [r] | _ => k
		val sep = sepLines (getLine [r]) l c ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r], k', getCol [r], D.printLabelledProgramString("\\operatorname{type} " ^ sep ^ x ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine [r], k', getCol [r], "type " ^ sep ^ x)
	    end
	  | printSpecOne (A.SpecDat (dd, r, label, _))           ind =
	    let val (l, k, c, x) = printDatDesc dd ind
		val k'  = case k of NONE => getLine [r] | _ => k
		val sep = sepLines (getLine [r]) l c ind
	    in
		if !(D.debugProgramLabelling)
		then (getLine [r], k', getCol [r], D.printLabelledProgramString("\\operatorname datatype " ^ sep ^ x ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine [r], k', getCol [r], "datatype " ^ sep ^ x)
	    end
	  | printSpecOne (A.SpecStr (sd, r, label, _))           ind =
	    let val (l, k, c, x) = printStrDesc sd ind
		val k'  = case k of NONE => getLine [r] | _ => k
		val sep = sepLines (getLine [r]) l c ind
	    (*val _ = Debug.printdebug2 (Int.toString (Option.getOpt (getCol [r],  ~1)) ^ " - " ^
					 Int.toString (Option.getOpt (getLine [r], ~1)))*)
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r], k', getCol [r], D.printLabelledProgramString("\\operatorname{structure} " ^ sep ^ x ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine [r], k', getCol [r], "structure " ^ sep ^ x)
	    end
	  | printSpecOne (A.SpecInc (si, r, label, _))           ind =
	    let val (l, k, c, x) = printLabSigExp si ind
		val k'  = case k of NONE => getLine [r] | _ => k
		val sep = sepLines (getLine [r]) l c ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r], k', getCol [r], D.printLabelledProgramString("\\operatorname{include} " ^ sep ^ x ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine [r], k', getCol [r], "include " ^ sep ^ x)
	    end
	  | printSpecOne (A.SpecIsi (si, r, label, _))           ind =
	    let val (l, k, c, x) = printSigIdSeq si ind
		val k'  = case k of NONE => getLine [r] | _ => k
		val sep = sepLines (getLine [r]) l c ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r], k', getCol [r], D.printLabelledProgramString("\\operatorname{include} " ^ sep ^ x ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine [r], k', getCol [r], "include " ^ sep ^ x)
	    end
	  | printSpecOne (A.SpecRep (tc, ltc, rs, label, _))     ind =
	    let val (r1, r2, r3) =
		    case rs of
			[r1, r2, r3] => (r1, r2, r3)
		      | _ => raise EH.DeadBranch "not the correct number of regions"
		val (l1, k1, c1, x) = printTyCon tc ind
		val (l2, k2, c2, y) = printLongTyCon ltc ind
		val k1'  = case k1 of NONE => getLine [r1] | _ => k1
		val k2'  = case k2 of NONE => getLine [r3] | _ => k2
		val sep1 = sepLines (getLine [r1]) l1 c1 ind
		val sep2 = sepLines k1' (getLine [r2]) (getCol [r2]) ind
		val sep3 = sepLines (getLine [r2]) (getLine [r3]) (getCol [r3]) ind
		val sep4 = sepLines (getLine [r3]) l2 c2 ind
		val isep = case sep2 of "" => " " | _ => ""
		val esep = case sep3 of " "=> " " | _ => ""
	    in 
		if !(D.debugProgramLabelling)
		then (getLine rs, k2', getCol rs,
		      D.printLabelledProgramString("\\operatorname{datatype} " ^ sep1 ^ x ^ sep2 ^ isep ^
		      "\\operatorname{=} "        ^ sep3 ^ esep ^
		      "\\operatorname{datatype} " ^ sep4 ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine rs, k2', getCol rs,
		      "datatype " ^ sep1 ^ x ^ sep2 ^ isep ^
		      "= "        ^ sep3 ^ esep ^
		      "datatype " ^ sep4 ^ y)
	    end
	  | printSpecOne (A.SpecSha (sp, tce, rs, label, _))     ind =
	    let val (r1, r2) =
		    case rs of
			[r1, r2] => (r1, r2)
		      | _ => raise EH.DeadBranch "not the correct number of regions"
		val (l1, k1, c1, x) = printSpec sp ind
		val (l2, k2, c2, y) = printLongTyConEq tce ind
		val k2'  = case k2 of NONE => getLine [r2] | _ => k2
		val sep1 = sepLines k1 (getLine [r1]) (getCol [r1]) ind
		val sep2 = sepLines (getLine [r2]) l2 c2 ind
		val isep = case sep1 of "" => " " | _ => ""
	    in 
		if !(D.debugProgramLabelling)
		then (l1, k2', c1, D.printLabelledProgramString( x ^ sep1 ^ isep ^ "\\operatorname{sharing} \\operatorname{type} " ^ sep2 ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l1, k2', c1, x ^ sep1 ^ isep ^ "sharing type " ^ sep2 ^ y)
	    end
	  | printSpecOne (A.SpecSsi (sp, tsi, rs, label, _))     ind =
	    let val r =
		    case rs of
			[r] => r
		      | _ => raise EH.DeadBranch "not the correct number of regions"
		val (l1, k1, c1, x) = printSpec sp ind
		val (l2, k2, c2, y) = printLongStrIdEq tsi ind
		val k2'  = case k2 of NONE => getLine [r] | _ => k2
		val sep1 = sepLines k1 (getLine [r]) (getCol [r]) ind
		val sep2 = sepLines (getLine [r]) l2 c2 ind
		val isep = case sep1 of "" => " " | _ => ""
	    in 
		if !(D.debugProgramLabelling)
		then (l1, k2', c1, D.printLabelledProgramString( x ^ sep1 ^ isep ^ "\\operatorname{sharing} " ^ sep2 ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l1, k2', c1, x ^ sep1 ^ isep ^ "sharing " ^ sep2 ^ y)
	    end
	  | printSpecOne (A.SpecOneDots pl)                  ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printStrDesc (A.StrDesc (sdol, _, _))            ind = printStrDescOneList sdol ind
	  | printStrDesc (A.StrDescDots pl)                  ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printStrDescOneList []                             _   = (NONE, NONE, NONE, "")
	  | printStrDescOneList (x :: xs)                      ind =
	    let val (l, k, c, x) = printStrDescOne x ind
		val (i, j, d, y) = printStrDescOneList xs ind
		val j'  = case j of NONE => k | _ => j
		val sep = sepLines k i d ind
	    in (l, j', c, x ^ sep ^ y)
	    end

	and printStrDescOne (A.StrDescOne (id, se, r, label, _)) ind =
	    let val (l1, k1, c1, x) = printStrId     id ind
		val (l2, k2, c2, y) = printLabSigExp se ind
		val k2'  = case k2 of NONE => getLine [r] | _ => k2
		val sep1 = sepLines l1 (getLine [r]) (getCol [r]) ind
		val sep2 = sepLines (getLine [r]) l2 c2 ind
	    in 
		if !(D.debugProgramLabelling)
		then (l1, k2', c1, D.printLabelledProgramString( x ^ sep1 ^ " \\operatorname{:} " ^ sep2 ^ y ^ "")^"^{" ^ L.printLab(label)^"}") 
		else (l1, k2', c1, x ^ sep1 ^ " : " ^ sep2 ^ y)
	    end
	  | printStrDescOne (A.StrDescOneDots pl)            ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printDatDesc (A.DatDesc (ddol, _, _))            ind = printDatDescOneList ddol ind
	  | printDatDesc (A.DatDescDots pl)                  ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printDatDescOneList []                             _   = (NONE, NONE, NONE, "")
	  | printDatDescOneList (x :: xs)                      ind =
	    let val (l, k, c, x) = printDatDescOne x ind
		val (i, j, d, y) = printDatDescOneList xs ind
		val j'  = case j of NONE => k | _ => j
		val sep = sepLines k i d ind
	    in (l, j', c, x ^ sep ^ y)
	    end

	and printDatDescOne (A.DatDescOne (dn, cd, _, label, _)) ind =
	    let val (l, k, c, x) = printDatName dn ind
		val (_, j, _, y) = printConDesc cd ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ " \\operatorname{=} " ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ " = " ^ y)
	    end
	  | printDatDescOne (A.DatDescOneDots pl)            ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printConDesc (A.ConDesc (cdol, _, _))            ind =
	    let val (l, k, c, x) = printConDescOneList cdol ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end
	  | printConDesc (A.ConDescDots pl)                  ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printConDescOneList []                             _   = (NONE, NONE, NONE, dots)
	  | printConDescOneList (x :: xs)                      ind =
	    let val (l, k, c, x) = printConDescOne x ind
		val (i, j, d, y) = printConDescOneList xs ind
		val sep = sepLines k i d ind
	    in (l, j, c, dots ^ x ^ sep ^ y)
	    end

	and printConDescOne (A.ConDescOneId (id, _))         ind = printIdent id ind
	  | printConDescOne (A.ConDescOneOf (i, t, _, label, _)) ind =
	    let val (l, k, c, x) = printLabId i ind
		val (_, j, _, y) = printLabType t ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ " \\operatorname{of} " ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ " of " ^ y)
	    end
	  | printConDescOne (A.ConDescOneNoOf (i, _))        ind =
	    printIdent i ind
	  | printConDescOne (A.ConDescOneDots pl)            ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printValDesc (A.ValDesc (vdol, _, _))            ind = printValDescOneList vdol ind
	  | printValDesc (A.ValDescDots pl)                  ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printValDescOneList []                             _   = (NONE, NONE, NONE, "")
	  | printValDescOneList (x :: xs)                      ind =
	    let val (l, k, c, x) = printValDescOne x ind
		val (i, j, d, y) = printValDescOneList xs ind
		val sep = sepLines k i d ind
	    in (l, j, c, x ^ sep ^ y)
	    end

	and printValDescOne (A.ValDescOne (id, ty, _, label, _)) ind =
	    let val (l, k, c, x) = printLabId id ind
		val (_, j, _, y) = printLabType ty ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ " \\operatorname{:} " ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ " : " ^ y)
	    end
	  | printValDescOne (A.ValDescOneDots pl)            ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printTypDesc (A.TypDesc (tdol, _, _))            ind = printTypDescOneList tdol ind
	  | printTypDesc (A.TypDescDots pl)                  ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printTypDescOneList []                             _   = (NONE, NONE, NONE, "")
	  | printTypDescOneList (x :: xs)                      ind =
	    let val (l, k, c, x) = printTypDescOne x ind
		val (i, j, d, y) = printTypDescOneList xs ind
		val sep = sepLines k i d ind
	    in (l, j, c, x ^ sep ^ y)
	    end

	and printTypDescOne (A.TypDescOne (dn, label, _))        ind =
	    let
		val (x1, x2, x3, x4) = printDatName dn ind
	    in
		if !(D.debugProgramLabelling)
		then (x1, x2, x3, D.printLabelledProgramString( x4 ^ "")^"^{" ^ L.printLab(label)^"}")
		else (x1, x2, x3, x4)
	    end
	  | printTypDescOne (A.TypDescOneDots pl)            ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printTdrDesc (A.TdrDesc (tdol, _, _))            ind = printTdrDescOneList tdol ind
	  | printTdrDesc (A.TdrDescDots pl)                  ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printTdrDescOneList []                             _   = (NONE, NONE, NONE, "")
	  | printTdrDescOneList (x :: xs)                      ind =
	    let val (l, k, c, x) = printTdrDescOne x ind
		val (i, j, d, y) = printTdrDescOneList xs ind
		val sep = sepLines k i d ind
	    in (l, j, c, x ^ sep ^ y)
	    end

	and printTdrDescOne (A.TdrDescOne (d, t, _, label, _))   ind =
	    let val (l, k, c, x) = printDatName d ind
		val (_, j, _, y) = printLabType t ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ " \\operatorname{=} " ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ " = " ^ y)
	    end
	  | printTdrDescOne (A.TdrDescOneDots pl)            ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printExcDesc (A.ExcDesc (edol, _, _))            ind = printExcDescOneList edol ind
	  | printExcDesc (A.ExcDescDots pl)                  ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printExcDescOneList []                             _   = (NONE, NONE, NONE, "")
	  | printExcDescOneList (x :: xs)                      ind =
	    let val (l, k, c, x) = printExcDescOne x ind
		val (i, j, d, y) = printExcDescOneList xs ind
		val sep = sepLines k i d ind
	    in (l, j, c, x ^ sep ^ y)
	    end

	and printExcDescOne (A.ExcDescOne (id, label, _))        ind =
	    let
		val (x1, x2, x3, x4) = printIdent id ind
	    in
		if !(D.debugProgramLabelling)
		then (x1, x2, x3, D.printLabelledProgramString( x4 ^ "")^"^{" ^ L.printLab(label)^"}")
		else (x1, x2, x3, x4)
	    end
	  | printExcDescOne (A.ExcDescOf (id, ty, _, label, _))  ind =
	    let val (l, k, c, x) = printLabId id ind
		val (_, j, _, y) = printLabType ty ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ " \\operatorname{of} " ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ " of " ^ y)
	    end
	  | printExcDescOne (A.ExcDescOneDots pl)            ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printLongStrId (A.LongStrIdQual (a, b, _, label, _)) ind =
	    let val (l, k, c, x) = printStrId a ind
		val (_, j, _, y) = printLongStrId b ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ "\\operatorname{.}" ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ "." ^ y)
	    end
	  | printLongStrId (A.LongStrIdId id)                ind = printStrId id ind
	  | printLongStrId (A.LongStrIdDots pl)              ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printPart (A.PartExp   ex)                       ind = printExp       ex ind
	  | printPart (A.PartDec   de)                       ind = printDec       de ind
	  | printPart (A.PartType  ty)                       ind = printType      ty ind
	  | printPart (A.PartSeq   se)                       ind = printTypeRow   se ind
	  | printPart (A.PartPat   pa)                       ind = printPat       pa ind
	  | printPart (A.PartIdTy  id)                       ind = printIdentTy   id ind
	  | printPart (A.PartTyCon tc)                       ind = printLongTyCon tc ind
	  | printPart (A.PartSpec  sp)                       ind = printSpecOne   sp ind
	  | printPart (A.PartSige  si)                       ind = printSigExp    si ind
	  | printPart (A.PartStre  se)                       ind = printStrExp    se ind
	  (*| printPart (A.PartFund  fd)                       ind = printFunDec    fd ind*)
	  | printPart (A.PartSigd  sd)                       ind = printSigDec    sd ind
	  | printPart (A.PartStrd  sd)                       ind = printStrDecOne sd ind
	  | printPart (A.PartLgid  id)                       ind = printLongId    id ind
	  | printPart (A.PartLgsid id)                       ind = printLongStrId id ind
	  | printPart (A.PartSigid id)                       ind = printSigId     id ind
	  | printPart (A.PartTes   st)                       ind = printSmlTes    st ind
	  | printPart (A.PartClass cl)                       ind = printClass     cl ind

	and printPartDots []                                   _   = (NONE, NONE, NONE, dots)
	  | printPartDots (p :: pl)                            ind =
	    let val (l, k, c, x) = printPart p (dind ind)
		val (i, j, d, y) = printPartDots pl ind
		val sep = sepLines k i d ind
	    in (l, j, c, dots ^ x ^ sep ^ y)
	    end

	and printScon (A.SconInt    (s, _, r, label, _))         _   = (getLine [r], getLine [r], getCol [r], 
									if !(D.debugProgramLabelling)
									then D.printLabelledProgramString( s ^ "")^"^{" ^ L.printLab(label)^"}"
									else s)
	  | printScon (A.SconWord   (s, _, r, label, _))         _   = (getLine [r], getLine [r], getCol [r], 
									if !(D.debugProgramLabelling)
									then D.printLabelledProgramString( s ^ "")^"^{" ^ L.printLab(label)^"}"
									else s)
	  | printScon (A.SconReal   (s, _, r, label, _))         _   = (getLine [r], getLine [r], getCol [r], 
									if !(D.debugProgramLabelling)
									then D.printLabelledProgramString( s ^ "")^"^{" ^ L.printLab(label)^"}"
									else s)
	  | printScon (A.SconString (s, _, r, label, _))         _   = (getLine [r], getLine [r], getCol [r], 
									if !(D.debugProgramLabelling)
									then D.printLabelledProgramString( (String.translate transfun s) ^ "")^"^{" ^ L.printLab(label)^"}"
									else (String.translate transfun s))
	  | printScon (A.SconChar   (s, _, r, label, _))         _   = (getLine [r], getLine [r], getCol [r], 
									if !(D.debugProgramLabelling)
									then D.printLabelledProgramString( (String.translate transfun s) ^ "")^"^{" ^ L.printLab(label)^"}"
									else (String.translate transfun s))
	  | printScon A.SconDots                             _   = (NONE, NONE, NONE, ldots ^ dots ^ rdots)

	and printPcon (A.PconBool (s, _, r, label, _))           _   = (getLine [r], getLine [r], getCol [r], 
									if !(D.debugProgramLabelling)
									then D.printLabelledProgramString( s ^ "")^"^{" ^ L.printLab(label)^"}"
									else s)
	  | printPcon (A.PconRef  (s, _, r, label, _))           _   = (getLine [r], getLine [r], getCol [r], 
									if !(D.debugProgramLabelling)
									then D.printLabelledProgramString( s ^ "")^"^{" ^ L.printLab(label)^"}"
									else s)
	  | printPcon (A.PconNil  (s, _, r, label, _))           _   = (getLine [r], getLine [r], getCol [r], 
									if !(D.debugProgramLabelling)
									then D.printLabelledProgramString( s ^ "")^"^{" ^ L.printLab(label)^"}"
									else s)
	  | printPcon A.PconDots                             _   = (NONE, NONE, NONE, ldots ^ dots ^ rdots)

	and printLabId (A.LabId (id, _, label, _))           ind =
	    let
		val (x1, x2, x3, x4) = printIdent id ind
	    in
		if !(D.debugProgramLabelling)
		then (x1, x2, x3, D.printLabelledProgramString( x4 ^ "")^"^{" ^ L.printLab(label)^"}")
		else (x1, x2, x3, x4)
	    end
	  | printLabId (A.LabIdDots pl)                      ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printIdent (A.Ident (s, _, r, label, _))         _   =
	    if !(D.debugProgramLabelling)
	    then (getLine [r], getLine [r], getCol [r], D.printLabelledProgramString("\\operatorname{"^s^"}")^"^{"^L.printLab(label)^"}")
	    else (getLine [r], getLine [r], getCol [r], s)
	  | printIdent (A.IdentPcon pc)                      ind = printPcon pc ind
	  | printIdent A.IdentDots                           _   = (NONE, NONE, NONE, ldots ^ dots ^ rdots)

	and printLabClass (A.LabClass (cl, _, label, _))     ind =
	    let
		val (x1, x2, x3, x4) = printClass cl ind
	    in
		if !(D.debugProgramLabelling)
		then (x1, x2, x3, D.printLabelledProgramString( x4 ^ "")^"^{" ^ L.printLab(label)^"}")
		else (x1, x2, x3, x4)
	    end
	  | printLabClass (A.LabClassDots pl)                ind =
	    let val (l, k, c, x) = printPartDots pl ind
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printClass (A.Class (s, _, r, label, _))             _   = (getLine [r], getLine [r], getCol [r], 
									if !(D.debugProgramLabelling)
									then D.printLabelledProgramString( s ^ "")^"^{" ^ L.printLab(label)^"}"
									else s)
	  | printClass A.ClassDots                           _   = (NONE, NONE, NONE, ldots ^ dots ^ rdots)

	and printStrId (A.StrId (s, _, r, label, _))             _   = (getLine [r], getLine [r], getCol [r], 
									if !(D.debugProgramLabelling)
									then D.printLabelledProgramString( s ^ "")^"^{" ^ L.printLab(label)^"}"
									else s)
	  | printStrId A.StrIdDots                           _   = (NONE, NONE, NONE, ldots ^ dots ^ rdots)

	and printSigId (A.SigId (s, _, r, label, _))             _   = (getLine [r], getLine [r], getCol [r], 
									if !(D.debugProgramLabelling)
									then D.printLabelledProgramString( s ^ "")^"^{" ^ L.printLab(label)^"}"
									else s)
	  | printSigId A.SigIdDots                           _   = (NONE, NONE, NONE, ldots ^ dots ^ rdots)

	and printFunId (A.FunId (s, _, r, label, _))             _   = (getLine [r], getLine [r], getCol [r], 
									if !(D.debugProgramLabelling)
									then D.printLabelledProgramString( s ^ "")^"^{" ^ L.printLab(label)^"}"
									else s)
	  | printFunId A.FunIdDots                           _   = (NONE, NONE, NONE, ldots ^ dots ^ rdots)

	and printLongId (A.LongIdQual (sid, lid, _, label, _))   ind =
	    let val (l, k, c, x) = printStrId sid ind
		val (_, j, _, y) = printLongId lid ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ "\\operatorname{.}" ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ "." ^ y)
	    end
	  | printLongId (A.LongIdId id)                      ind = printIdent id ind
	  | printLongId (A.LongIdDots pl)                    ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printLongTyCon (A.LongTyConQual (s, t, _, label, _)) ind =
	    let val (l, k, c, x) = printStrId s ind
		val (_, j, _, y) =printLongTyCon t ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ "\\operatorname{.}" ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ "." ^ y)
	    end
	  | printLongTyCon (A.LongTyConId tc)                ind = printTyCon tc ind
	  | printLongTyCon (A.LongTyConDots pl)              ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printTyLab (A.TyLab (s, r, label, _))                _   = (getLine [r], getLine [r], getCol [r], 
									if !(D.debugProgramLabelling)
									then D.printLabelledProgramString( s ^ "")^"^{" ^ L.printLab(label)^"}"
									else s)
	  | printTyLab (A.TyLabDots)                         _   = (NONE, NONE, NONE, ldots ^ dots ^ rdots)

	and printTyCon (A.TyCon (s, _, r, label, _))             _   = (getLine [r], getLine [r], getCol [r], 
									if !(D.debugProgramLabelling)
									then D.printLabelledProgramString( s ^ "")^"^{" ^ L.printLab(label)^"}"
									else s)
	  | printTyCon A.TyConDots                           _   = (NONE, NONE, NONE, ldots ^ dots ^ rdots)

	and printTypeVar (A.TypeVar (s, _, r, label, _))         _   = 
	    if !(D.debugProgramLabelling)
	    then (getLine [r], getLine [r], getCol [r], D.printLabelledProgramString("\\operatorname{"^s^"}")^"^{"^L.printLab(label)^"}")
	    else (getLine [r], getLine [r], getCol [r], s)
	  | printTypeVar (A.EqualityTypeVar (s, _, r, label, _)) _   = 
	    if !(D.debugProgramLabelling)
	    then (getLine [r], getLine [r], getCol [r], D.printLabelledProgramString("\\operatorname{"^s^"}")^"^{"^L.printLab(label)^"}")
	    else (getLine [r], getLine [r], getCol [r], s)
	  | printTypeVar (A.TypeVarDots)                     _   = (NONE, NONE, NONE, ldots ^ dots ^ rdots)

	and printTypeVarList []                                _   = (NONE, NONE, NONE, "")
	  | printTypeVarList [ty]                              ind = printTypeVar ty ind
	  | printTypeVarList (ty :: tyl)                       ind =
	    let val (l, k, c, x) = printTypeVar ty ind
		val (_, j, _, y) = printTypeVarList tyl ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, x ^ "\\operatorname{,}" ^ y)
		else (l, j, c, x ^ "," ^ y)
	    end

	and printTypeVarDots []                                _   = (NONE, NONE, NONE, dots)
	  | printTypeVarDots (tv :: tvl)                       ind =
	    let val (l, k, c, x) = printTypeVar tv (dind ind)
		val (_, j, _, y) = printTypeVarDots tvl ind
	    in (l, j, c, dots ^ x ^ y)
	    end

	and printLabTypeVar (A.LabTypeVar (tv, _, label, _))         ind =
	    let
		val (x1, x2, x3, x4) = printTypeVar tv ind
	    in
		if !(D.debugProgramLabelling)
		then (x1, x2, x3, D.printLabelledProgramString( x4 ^ "")^"^{" ^ L.printLab(label)^"}")
		else (x1, x2, x3, x4)
	    end
	  | printLabTypeVar (A.LabTypeVarDots tvl)               ind =
	    let val (l, k, c, x) = printTypeVarDots tvl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printLabTypeVarList []                               _   = (NONE, NONE, NONE, "")
	  | printLabTypeVarList [x]                              ind = printLabTypeVar x ind
	  | printLabTypeVarList (x :: xs)                        ind =
	    let val (l, k, c, x) = printLabTypeVar x ind
		val (i, j, d, y) = printLabTypeVarList xs ind
		val sep = sepLines k i d ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, x ^ "\\operatorname{,}" ^ sep ^ y)
		else (l, j, c, x ^ "," ^ sep ^ y)
	    end

	and printTypeVarSeq (A.TypeVarSeqOne (tv, _, label, _))      ind =
	    let val (l, k, c, x) = printTypeVar tv (sind ind)
	    in 
		if !(D.debugProgramLabelling)
		then (l, k, c, D.printLabelledProgramString( splparen ^ x ^ sprparen ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, k, c, splparen ^ x ^ sprparen)
	    end
	  | printTypeVarSeq (A.TypeVarSeqEm (_, label, _))           _   = (NONE, NONE, NONE, 
									    if !(D.debugProgramLabelling)
									    then D.printLabelledProgramString( splparen ^ sprparen ^ "")^"^{" ^ L.printLab(label)^"}"
									    else splparen ^ sprparen) (*HACK: before it was empty string*)
	  | printTypeVarSeq (A.TypeVarSeqSeq (tvl, rs, label, _))    ind =
	    let val (_, _, _, x) = printLabTypeVarList tvl ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine rs, getLine (rev rs), getCol rs, D.printLabelledProgramString("\\operatorname{(}" ^ x ^ "\\operatorname{)}" ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine rs, getLine (rev rs), getCol rs, "(" ^ x ^ ")")
	    end
	  | printTypeVarSeq (A.TypeVarSeqDots tvl)               ind =
	    let val (l, k, c, x) = printTypeVarDots tvl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printTypeList []                                   _   = (NONE, NONE, NONE, "")
	  | printTypeList [ty]                                 ind = printType ty ind
	  | printTypeList (ty :: tyl)                          ind =
	    let val (l, k, c, x) = printType ty ind
		val (_, j, _, y) = printTypeList tyl ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, x ^ "\\operatorname{,}" ^ y)
		else (l, j, c, x ^ "," ^ y)
	    end

	and printTypeTuple []                                  _   = (NONE, NONE, NONE, "")
	  | printTypeTuple [ty]                                ind = printType ty ind
	  | printTypeTuple (ty :: tyl)                         ind =
	    let val (l, k, c, x) = printType ty ind
		val (_, j, _, y) = printTypeTuple tyl ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, x ^ "\\operatorname{*}" ^ y)
		else (l, j, c, x ^ "*" ^ y)
	    end

	and printLabTypeList [] _                              _   = (NONE, NONE, NONE, "")
	  | printLabTypeList [ty] []                           ind = printLabType ty ind
	  | printLabTypeList (ty :: tyl) (r :: rs)             ind =
	    let val (l, k, c, x) = printLabType ty ind
		val (i, j, d, y) = printLabTypeList tyl rs ind
		val j'  = case j of NONE => getLine [r] | _ => j
		val sep1 = sepLines k (getLine [r]) (getCol [r]) ind
		val sep2 = sepLines (getLine [r]) i d ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j', c, x ^ sep1 ^ "\\operatorname{,}" ^ sep2 ^ y)
		else (l, j', c, x ^ sep1 ^ "," ^ sep2 ^ y)
	    end
	  | printLabTypeList _ _                               _   = raise EH.DeadBranch "Missing region for a type row comma"

	and printLabTypeTuple []                               _   = (NONE, NONE, NONE, "")
	  | printLabTypeTuple [ty]                             ind = printLabType ty ind
	  | printLabTypeTuple (ty :: tyl)                      ind =
	    let val (l, k, c, x) = printLabType ty ind
		val (_, j, _, y) = printLabTypeTuple tyl ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, x ^ "\\operatorname{*}" ^ y)
		else (l, j, c, x ^ "*" ^ y)
	    end

	and printLabType (A.LabType (t, _, label, _))            ind =
	    let
		val (x1, x2, x3, x4) = printType t ind
	    in
		if !(D.debugProgramLabelling)
		then (x1, x2, x3, D.printLabelledProgramString( x4 ^ "")^"^{" ^ L.printLab(label)^"}")
		else (x1, x2, x3, x4)
	    end
	  | printLabType (A.LabTypeDots pl)                  ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printTyField (A.TyField (tl, lt, _, label, _))           ind =
	    let val (l, k, c, x) = printTyLab tl ind
		val (_, j, _, y) = printLabType lt ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ "\\operatorname{:}" ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ ":" ^ y)
	    end
	  | printTyField (A.TyFieldDots pl)                      ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printTyFieldList []                                  _   = (NONE, NONE, NONE, "")
	  | printTyFieldList [x]                                 ind = printTyField x ind
	  | printTyFieldList (x :: xs)                           ind =
	    let val (l, k, c, x) = printTyField x ind
		val (_, j, _, y) = printTyFieldList xs ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, x ^ "\\operatorname{,}" ^ y)
		else (l, j, c, x ^ "," ^ y)
	    end

	and printTyFieldSlList []                                _   = (NONE, NONE, NONE, dots)
	  | printTyFieldSlList (x :: xs)                         ind =
	    let val (l, k, c, x) = printTyField x (dind ind)
		val (i, j, d, y) = printTyFieldSlList xs ind
		val sep = sepLines k i d ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, dots ^ "\\operatorname{,}" ^ x ^ "\\operatorname{,}" ^ sep ^ y)
		else (l, j, c, dots ^ "," ^ x ^ "," ^ sep ^ y)
	    end

	and printType (A.TypeOneVar (tv))                    ind = printTypeVar tv ind
	  | printType (A.TypeArrow (t1, t2, _, label, _))        ind =
	    let val (l, k, c, x) = printLabType t1 ind
		val (_, j, _, y) = printLabType t2 ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ " \\operatorname{->} " ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ " -> " ^ y)
	    end
	  | printType (A.TypeTuple (tl, _, label, _))            ind =
	    let
		val (x1, x2, x3, x4) = printLabTypeTuple tl ind
	    in
		if !(D.debugProgramLabelling)
		then (x1, x2, x3, D.printLabelledProgramString( x4 ^ "")^"^{" ^ L.printLab(label)^"}")
		else (x1, x2, x3, x4)
	    end
	  | printType (A.TypeRecord (trl, rs, _, label, _))      ind =
	    let val (_, _, _, x) = printTyFieldList trl ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine rs, getLine (rev rs), getCol rs, D.printLabelledProgramString("\\operatorname{\\{}" ^ x ^ "\\operatorname{\\}}" ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine rs, getLine (rev rs), getCol rs, "{" ^ x ^ "}")
	    end
	  | printType (A.TypeSlRec (trl, rs, label, _))          ind =
	    let val (_, _, _, x) = printTyFieldSlList trl ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine rs, getLine (rev rs), getCol rs, D.printLabelledProgramString("\\operatorname{\\{}" ^ x ^ "\\operatorname{\\}}" ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine rs, getLine (rev rs), getCol rs, "{" ^ x ^ "}")
	    end
	  | printType (A.TypeTyCon (ts, ltc, _, label, _))       ind =
	    let val (l, k, c, x) = printTypeRow ts ind
		val (i, j, d, y) = printLongTyCon ltc ind
		val sep = sepLines k i d ind
	    in
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ sep ^ " " ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ sep ^ " " ^ y)
	    end
	  | printType (A.TypeParen (t, r1, r2, label, _))          ind =
	    let val (_, _, _, x) = printLabType t ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r1], getLine [r2], getCol [r1], D.printLabelledProgramString("\\operatorname{(}" ^ x ^ "\\operatorname{)}" ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine [r1], getLine [r2], getCol [r1], "(" ^ x ^ ")")
	    end
	  | printType (A.TypeDots spl)                       ind =
	    let val (l, k, c, x) = printPartDots spl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printTypeRow (A.TypeRowOne (t, _, label, _))         ind =
	    let val (l, k, c, x) = printType t (sind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, D.printLabelledProgramString( splparen ^ x ^ sprparen ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, k, c, splparen ^ x ^ sprparen)
	    end
	  | printTypeRow (A.TypeRowEm (r, label, _))             _   =
	    if !(D.debugProgramLabelling)
	    then (getLine [r], getLine [r], getCol [r], D.printLabelledProgramString( splparen ^ sprparen ^ "")^"^{" ^ L.printLab(label)^"}") (*HACK: before it was: ""*)
	    else (getLine [r], getLine [r], getCol [r], splparen ^ sprparen) (*HACK: before it was: ""*)
	  | printTypeRow (A.TypeRowSeq (tl, rs, label, _))       ind =
	    let val (r1, r2) = (List.hd rs, List.last rs)
			       handle Empty => raise EH.DeadBranch "Missing regions for type row parentheses"
		val rs' = (List.rev o List.tl o List.rev o List.tl) rs
		val (l, k, c, x) = printLabTypeList tl rs' ind
		val k'   = case k of NONE => getLine [r1] | _ => k
		val sep1 = sepLines (getLine [r1]) l c ind
		val sep2 = sepLines k' (getLine [r2]) (getCol [r2]) ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine rs, getLine (rev rs), getCol rs, D.printLabelledProgramString("\\operatorname{(}" ^ sep1 ^ x ^ sep2 ^ "\\operatorname{)}" ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine rs, getLine (rev rs), getCol rs, "(" ^ sep1 ^ x ^ sep2 ^ ")")
	    end
	  | printTypeRow (A.TypeRowDots spl)                 ind =
	    let val (l, k, c, x) = printPartDots spl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printTyClass (A.TyClassCl (cl, r, label, _))         ind =
	    let val (l, k, c, x) = printLabClass cl ind
		val k'  = case k of NONE => getLine [r] | _ => k
		val sep = sepLines (getLine [r]) l c ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r], k', getCol [r], D.printLabelledProgramString("\\operatorname{in} " ^ sep ^ x ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine [r], k', getCol [r], "in " ^ sep ^ x)
	    end
	  | printTyClass (A.TyClassTy (ty, label, _))            ind =
	    let
		val (x1, x2, x3, x4) = printType ty ind
	    in
		
		if !(D.debugProgramLabelling)
		then (x1, x2, x3, D.printLabelledProgramString( x4 ^ "")^"^{" ^ L.printLab(label)^"}")
		else (x1, x2, x3, x4)
	    end
	  | printTyClass (A.TyClassDots pl)                  ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printLabTyClass (A.LabTyClass (t, _, label, _))      ind =
	    let
		val (x1, x2, x3, x4) = printTyClass t ind
	    in
		if !(D.debugProgramLabelling)
		then (x1, x2, x3, D.printLabelledProgramString( x4 ^ "")^"^{" ^ L.printLab(label)^"}")
		else (x1, x2, x3, x4)
	    end
	  | printLabTyClass (A.LabTyClassDots pl)            ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printLabTyClassList [] _                           _   = (NONE, NONE, NONE, "")
	  | printLabTyClassList [ty] []                        ind = printLabTyClass ty ind
	  | printLabTyClassList (ty :: tyl) (r :: rs)          ind =
	    let val (l, k, c, x) = printLabTyClass ty ind
		val (i, j, d, y) = printLabTyClassList tyl rs ind
		val j'  = case j of NONE => getLine [r] | _ => j
		val sep1 = sepLines k (getLine [r]) (getCol [r]) ind
		val sep2 = sepLines (getLine [r]) i d ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j', c, x ^ sep1 ^ "\\operatorname{,}" ^ sep2 ^ y)
		else (l, j', c, x ^ sep1 ^ "," ^ sep2 ^ y)
	    end
	  | printLabTyClassList _ _                            _   = raise EH.DeadBranch "Missing region for a type row comma"

	and printTyClassSeq (A.TyClassSeqOne (t, _, label, _))   ind =
	    let val (l, k, c, x) = printTyClass t (sind ind)
	    in 
		if !(D.debugProgramLabelling)
		then (l, k, c, D.printLabelledProgramString( splparen ^ x ^ sprparen ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, k, c, splparen ^ x ^ sprparen)
	    end
	  | printTyClassSeq (A.TyClassSeqEm (r, label, _))       _   =
	    if !(D.debugProgramLabelling)
	    then (getLine [r], getLine [r], getCol [r], D.printLabelledProgramString( splparen ^ sprparen ^ "")^"^{" ^ L.printLab(label)^"}") (*HACK: before it was: ""*)
	    else (getLine [r], getLine [r], getCol [r], splparen ^ sprparen) (*HACK: before it was: ""*)
	  | printTyClassSeq (A.TyClassSeqSeq (tl, rs, label, _)) ind =
	    let val (r1, r2) = (List.hd rs, List.last rs)
			       handle Empty => raise EH.DeadBranch "Missing regions for tyclass row parentheses"
		val rs' = (List.rev o List.tl o List.rev o List.tl) rs
		val (l, k, c, x) = printLabTyClassList tl rs' ind
		val k'   = case k of NONE => getLine [r1] | _ => k
		val sep1 = sepLines (getLine [r1]) l c ind
		val sep2 = sepLines k' (getLine [r2]) (getCol [r2]) ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine rs, getLine (rev rs), getCol rs, D.printLabelledProgramString("\\operatorname{(}" ^ sep1 ^ x ^ sep2 ^ "\\operatorname{)}" ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine rs, getLine (rev rs), getCol rs, "(" ^ sep1 ^ x ^ sep2 ^ ")")
	    end
	  | printTyClassSeq (A.TyClassSeqDots spl)           ind =
	    let val (l, k, c, x) = printPartDots spl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printConBind (A.ConBind (id, _))                 ind = printIdent id ind
	  | printConBind (A.ConBindOf (id, t, r, label, _))      ind =
	    let val (l1, k1, c1, x) = printLabId id ind
		val (l2, k2, c2, y) = printLabType t ind
		val k2'  = case k2 of NONE => getLine [r] | _ => k2
		val sep1 = sepLines k1 (getLine [r]) (getCol [r]) ind
		val sep2 = sepLines (getLine [r]) l2 c2 ind
		val esep = case sep1 of "" => " " | _ => ""
	    in 
		if !(D.debugProgramLabelling)
		then (l1, k2', c1, D.printLabelledProgramString( x ^ sep1 ^ esep ^ "\\operatorname{of} " ^ sep2 ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l1, k2', c1, x ^ sep1 ^ esep ^ "of " ^ sep2 ^ y)
	    end
	  | printConBind (A.ConBindNoOf (id, _))             ind = printIdent id ind
	  | printConBind (A.ConBindDots spl)                 ind =
	    let val (l, k, c, x) = printPartDots spl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printConBindList []                                _   = (NONE, NONE, NONE, dots)
	  | printConBindList (x :: xs)                         ind =
	    let val (l, k, c, x) = printConBind x (dind ind)
		val (i, j, d, y) = printConBindList xs ind
		val sep = sepLines k i d ind
	    in (l, j, c, dots ^ x ^ sep ^ y)
	    end

	and printConBindSeq (A.ConBindSeq tcl)               ind =
	    let val (l, k, c, x) = printConBindList tcl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end
	  | printConBindSeq (A.ConBindSeqDots spl)           ind =
	    let val (l, k, c, x) = printPartDots spl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printValBindCore (A.ValBindCore (p, e, r, label, _)) ind =
	    let val (l1, k1, c1, x) = printLabPat p ind
		val (l2, k2, c2, y) = printLabExp e ind
		val k2'  = case k2 of NONE => getLine [r] | _ => k2
		val sep1 = sepLines k1 (getLine [r]) (getCol [r]) ind
		val sep2 = sepLines (getLine [r]) l2 c2 ind
		val esep = case sep1 of "" => " " | _ => ""
	    in 
		if !(D.debugProgramLabelling)
		then (l1, k2', c1, D.printLabelledProgramString( x ^ sep1 ^ esep ^ "\\operatorname{=} " ^ sep2 ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l1, k2', c1, x ^ sep1 ^ esep ^ "= " ^ sep2 ^ y)
	    end
	  | printValBindCore (A.ValBindCoreDots pl)          ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printValBindCoreList []                            _   = (NONE, NONE, NONE, dots)
	  | printValBindCoreList (x :: xs)                     ind =
	    let val (l, k, c, x) = printValBindCore x (dind ind)
		val (i, j, d, y) = printValBindCoreList xs ind
		val j'  = case j of NONE => k | _ => j
		val sep = sepLines k i d ind
	    in (l, j', c, dots ^ x ^ sep ^ y)
	    end

	and printValBindSeq (A.ValBindSeq (vbl, _, _))       ind =
	    let val (l, k, c, x) = printValBindCoreList vbl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end
	  | printValBindSeq (A.ValBindSeqDots pl)            ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printValBind (A.ValBindRec (vbs, r, label, _))       ind =
	    let val (l, k, c, x) = printValBindSeq vbs ind
		val k'  = case k of NONE => getLine [r] | _ => k
		val sep = sepLines (getLine [r]) l c ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r], k', getCol [r], D.printLabelledProgramString("\\operatorname{rec} " ^ sep ^ x ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine [r], k', getCol [r], "rec " ^ sep ^ x)
	    end
	  | printValBind (A.ValBind vbs)                     ind = printValBindSeq vbs ind
	  | printValBind (A.ValBindDots pl)                  ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printDatName (A.DatName (tvs, tn, _, _))         ind =
	    let val (l, k, c, x) = printTypeVarSeq tvs ind
		val (_, j, _, y) = printTyCon tn ind
	    in (l, j, c, x ^ " " ^ y)
	    end
	  | printDatName A.DatNameDots                       _   = (NONE, NONE, NONE, ldots ^ dots ^ rdots)

	and printLDatName (A.LDatName (tvs, tn, _, _))       ind =
	    let val (l, k, c, x) = printTypeVarSeq tvs ind
		val (_, j, _, y) = printLongTyCon tn ind
	    in (l, j, c, x ^ " " ^ y)
	    end
	  | printLDatName A.LDatNameDots                     _   = (NONE, NONE, NONE, ldots ^ dots ^ rdots)

	and printDatBind (A.DatBind (dn, tcs, r, label, _))      ind =
	    let val (l1, k1, c1, x) = printDatName dn ind
		val (l2, k2, c2, y) = printConBindSeq tcs ind
		val sep1 = sepLines k1 (getLine [r]) (getCol [r]) ind
		val sep2 = sepLines (getLine [r]) l2 c2 ind
		val esep = case sep1 of "" => " " | _ => ""
	    in 
		if !(D.debugProgramLabelling)
		then (l1, k2, c1, D.printLabelledProgramString( x ^ sep1 ^ esep ^ "\\operatorname{=} " ^ sep2 ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l1, k2, c1, x ^ sep1 ^ esep ^ "= " ^ sep2 ^ y)
	    end
	  | printDatBind (A.DatBindDots pl)                  ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printDatBindList []                                _   = (NONE, NONE, NONE, dots)
	  | printDatBindList (x :: xs)                         ind =
	    let val (l, k, c, x) = printDatBind x (dind ind)
		val (i, j, d, y) = printDatBindList xs ind
		val j'  = case j of NONE => k | _ => j
		val sep = sepLines l k c ind
	    in (l, j', c, dots ^ x ^ sep ^ y)
	    end

	(* do we want something special for the empty case? *)
	and printDatBindSeq (A.DatBindSeq (dbl, _, _))       ind =
	    let val (l, k, c, x) = printDatBindList dbl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end
	  | printDatBindSeq (A.DatBindSeqDots pl)            ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printLabAtPat (A.LabAtPat (ap, _, label, _))         ind =
	    let
		val (x1, x2, x3, x4) = printAtPat ap ind
	    in
		if !(D.debugProgramLabelling)
		then (x1, x2, x3, D.printLabelledProgramString( x4 ^ "")^"^{" ^ L.printLab(label)^"}")
		else (x1, x2, x3, x4)
	    end
	  | printLabAtPat (A.LabAtPatDots pl)                ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printFMatch (A.FMatchId (id, _, _))               ind = printIdent id ind
	  | printFMatch (A.FMatchApp (fm, a, _, _, label, _)) ind =
	    let val (l, k, c, x) = printFMatch fm ind
		val (_, j, _, y) = printLabAtPat a ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ " " ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ " " ^ y)
	    end
	  | printFMatch (A.FMatchSlApp (fm, lap, _))         ind =
	    let val (l, k, c, x) = printFMatch fm ind
		val (_, j, _, y) = printLabAtPat lap (dind ind)
	    in (l, j, c, x ^ dots ^ y)
	    end
	  | printFMatch (A.FMatchNoApp (fm, _))              ind =
	    let val (l, k, c, x) = printFMatch fm ind
	    in (l, k, c, x ^ dots)
	    end
	  | printFMatch A.FMatchDots                         _   = (NONE, NONE, NONE, dots)

	and printLabFMatch (A.LabFMatch (fm, _, label, _))       ind =
	    let val (l, k, c, x) = printFMatch fm ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, k, c, D.printLabelledProgramString( x ^ " " ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, k, c, x ^ " ")
	    end
	  | printLabFMatch (A.LabFMatchSl (fm, _))           ind =
	    let val (l, k, c, x) = printFMatch fm ind
	    in (l, k, c, x ^ dots)
	    end
	  | printLabFMatch A.LabFMatchDots                   _   = (NONE, NONE, NONE, dots)

	and printFMatchTy (A.FMatchT fm)                     ind = printLabFMatch fm ind
	  | printFMatchTy (A.FMatchTTy (fm, ty, _, label, _))    ind =
	    let val (l, k, c, x) = printLabFMatch fm ind
		val (_, j, _, y) = printLabType ty ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ " \\operatorname{:} " ^ y ^ " " ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ " : " ^ y ^ " ")
	    end
	  | printFMatchTy A.FMatchTDots                      _   = (NONE, NONE, NONE, dots)

	and printFVBCore (A.FValBindCore (x, e, r, label, _))    ind =
	    let val (l1, k1, c1, x) = printFMatchTy x ind
		val (l2, k2, c2, y) = printLabExp e ind
		val k2'  = case k2 of NONE => getLine [r] | _ => k2
		val sep1 = sepLines k1 (getLine [r]) (getCol [r]) ind
		val sep2 = sepLines (getLine [r]) l2 c2 ind
	    in (l1, k2', c1, 
		if !(D.debugProgramLabelling)
		then D.printLabelledProgramString( x ^ sep1 ^ "= " ^ sep2 ^ y ^ "")^"^{" ^ L.printLab(label)^"}"
		else x ^ sep1 ^ "= " ^ sep2 ^ y)
	    end
	  (*| printFVBCore (A.FVBCoreTy (x, t, e, _, _, _, _)) = printLabFMatch x ^ " : " ^ printLabType t ^ "= " ^ printLabExp e*)
	  | printFVBCore (A.FVBCoreDots pl)                  ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printFValBindCoreList []                           _   = (NONE, NONE, NONE, dots)
	  | printFValBindCoreList (x :: xs)                    ind =
	    let val (l, k, c, x) = printFVBCore x (dind ind)
		val (i, j, d, y) = printFValBindCoreList xs ind
		val j'  = case j of NONE => k | _ => j
		val i'  = case i of NONE => j' | _ => i
		(*val _ = Debug.printdebug2 (Int.toString (Option.getOpt (j, ~1)) ^ " " ^
					     Int.toString (Option.getOpt (j', ~1)))*)
		val sep = sepLines k i' d ind
	    in (l, j', c, dots ^ x ^ sep ^ y)
	    end

	(*
	 and printFValBindCoreList []                           = dots
	   | printFValBindCoreList [x]                          = dots ^ " | " ^ (printFVBCore x) ^ " | " ^ dots
	   | printFValBindCoreList (x :: xs)                    = dots ^ " | " ^ (printFVBCore x) ^ " | " ^ (printFValBindCoreList xs)
	*)

	and printFValBindOne (A.FValBindOne (x, _, label, _))    ind =
	    let val (l, k, c, x) = printFValBindCoreList x (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, D.printLabelledProgramString( ldotsLatex ^ x ^ rdotsLatex ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, k, c, ldots ^ x ^ rdots)
	    end
	  | printFValBindOne (A.FVBOneDots pl)               ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printFValBindOneList []                            _   = (NONE, NONE, NONE, dots)
	  | printFValBindOneList (x :: xs)                     ind =
	    let val (l, k, c, x) = printFValBindOne x (dind ind)
		val (i, j, d, y) = printFValBindOneList xs ind
		val j'  = case j of NONE => k | _ => j
		val sep = sepLines k i d ind
	    (*val _ = Debug.printdebug2 (Int.toString (Option.getOpt (j', ~1)))*)
	    in (l, j', c, dots ^ x ^ sep ^ y)
	    end

	and printFValBind (A.FValBind (fvbol, _, _))         ind =
	    let val (l, k, c, x) = printFValBindOneList fvbol (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end
	  | printFValBind (A.FValBindDots pl)                ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printTypBind (A.TypBind (dn, ty, r, label, _))       ind =
	    let val (l1, k1, c1, x) = printDatName dn ind
		val (l2, k2, c2, y) = printLabType ty ind
		val k2'  = case k2 of NONE => getLine [r] | _ => k2
		val sep1 = sepLines k1 (getLine [r]) (getCol [r]) ind
		val sep2 = sepLines (getLine [r]) l2 c2 ind
		val esep = case sep1 of "" => " " | _ => ""
	    in 
		if !(D.debugProgramLabelling)
		then (l1, k2', c1, D.printLabelledProgramString( x ^ sep1 ^ esep ^ "\\operatorname{=} " ^ sep2 ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l1, k2', c1, x ^ sep1 ^ esep ^ "= " ^ sep2 ^ y)
	    end
	  | printTypBind (A.TypBindDots pl)                  ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printTypBindList []                                _   = (NONE, NONE, NONE, dots)
	  | printTypBindList (x :: xs)                         ind =
	    let val (l, k, c, x) = printTypBind x (dind ind)
		val (i, j, d, y) = printTypBindList xs ind
		val j'  = case j of NONE => k | _ => j
		val sep = sepLines k i d ind
	    in (l, j', c, dots ^ x ^ sep ^ y)
	    end

	and printTypBindSeq (A.TypBindSeq (tbl, _, _))       ind =
	    let val (l, k, c, x) = printTypBindList tbl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end
	  | printTypBindSeq (A.TypBindSeqDots pl)            ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printExBind (A.ExBind (id, label, _))            ind =
	    let
		val (x1, x2, x3, x4) = printIdent id ind
	    in
		if !(D.debugProgramLabelling)
		then (x1, x2, x3, D.printLabelledProgramString( x4 ^ "")^"^{" ^ L.printLab(label)^"}")
		else (x1, x2, x3, x4)
	    end
	  | printExBind (A.ExBindOf (id, t, _, label, _))        ind =
	    let val (l, k, c, x) = printLabId id ind
		val (_, j, _, y) = printLabType t ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ " \\operatorname{of} " ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ " of " ^ y)
	    end
	  | printExBind (A.ExBindEq (id, sid, _, label, _))      ind =
	    let val (l, k, c, x) = printLabId id ind
		val (_, j, _, y) = printLongId sid ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ " \\operatorname{=} " ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ " = " ^ y)
	    end
	  | printExBind (A.ExBindNo (id, _))                 ind = printIdent id ind
	  | printExBind (A.ExBindDots pl)                    ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printExBindList []                                 _   = (NONE, NONE, NONE, dots)
	  | printExBindList (x :: xs)                          ind =
	    let val (l, k, c, x) =printExBind x (dind ind)
		val (i, j, d, y) = printExBindList xs ind
		val sep = sepLines k i d ind
	    in (l, j, c, dots ^ x ^ sep ^ y)
	    end

	and printExBindSeq (A.ExBindSeq (ebl, _, _))         ind =
	    let val (l, k, c, x) = printExBindList ebl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end
	  | printExBindSeq (A.ExBindSeqDots pl)              ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printSigIdSeqList []                              _   = (NONE, NONE, NONE, dots)
	  | printSigIdSeqList (x :: xs)                       ind =
	    let val (l, k, c, x) = printSigId x (dind ind)
		val (i, j, d, y) = printSigIdSeqList xs ind
		val sep = sepLines k i d ind
	    in (l, j, c, dots ^ x ^ sep ^ y)
	    end

	and printSigIdSeq (A.SigIdSeq (xs, _))              ind =
	    let val (l, k, c, x) = printSigIdSeqList xs (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end
	  | printSigIdSeq (A.SigIdSeqDots pl)               ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printLongStrIdList []                              _   = (NONE, NONE, NONE, dots)
	  | printLongStrIdList (x :: xs)                       ind =
	    let val (l, k, c, x) = printLongStrId x (dind ind)
		val (i, j, d, y) = printLongStrIdList xs ind
		val sep = sepLines k i d ind
	    in (l, j, c, dots ^ x ^ sep ^ y)
	    end

	and printLongStrSeq (A.LongStrSeq (idl, _))          ind =
	    let val (l, k, c, x) = printLongStrIdList idl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end
	  | printLongStrSeq (A.LongStrSeqDots pl)            ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printIdentList []                              _   = (NONE, NONE, NONE, dots)
	  | printIdentList (x :: xs)                       ind =
	    let val (l, k, c, x) = printIdent x (dind ind)
		val (i, j, d, y) = printIdentList xs ind
		val sep = sepLines k i d ind
	    in (l, j, c, dots ^ x ^ sep ^ y)
	    end

	and printIdentSeq (A.IdentSeq (idl, _))          ind =
	    let val (l, k, c, x) = printIdentList idl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end
	  | printIdentSeq (A.IdentSeqDots pl)            ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printDec (A.DecVal (tvs, vb, r, _))              ind =
	    let val (l1, k1, c1, x) = printTypeVarSeq tvs ind
		val (l2, k2, c2, y) = printValBind vb ind
		val k2' = case k2 of NONE => k1 | _ => k2
		val sep = sepLines (getLine [r]) l1 c1 ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r], k2', getCol [r], "\\operatorname{val} " ^ sep ^ x ^ " " ^ y)
		else (getLine [r], k2', getCol [r], "val " ^ sep ^ x ^ " " ^ y)
	    end
	  | printDec (A.DecFVal (tvs, fvb, r, _))            ind =
	    let val (l1, k1, c1, x) = printTypeVarSeq tvs ind
		val (l2, k2, c2, y) = printFValBind fvb ind
		val k2' = case k2 of NONE => k1 | _ => k2
		val sep = sepLines (getLine [r]) l1 c1 ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r], k2', getCol [r], "\\operatorname{fun} " ^ sep ^ x ^ " " ^ y)
		else (getLine [r], k2', getCol [r], "fun " ^ sep ^ x ^ " " ^ y)
	    end
	  | printDec (A.DecDatType (dbs, r, _))              ind =
	    let val (l, k, c, x) = printDatBindSeq dbs ind
		val k'  = case k of NONE => getLine [r] | _ => k
		val sep = sepLines (getLine [r]) l c ind
	    in
		if !(D.debugProgramLabelling)
		then (getLine [r], k', getCol [r], "\\operatorname{datatype} " ^ sep ^ x)
		else (getLine [r], k', getCol [r], "datatype " ^ sep ^ x)
	    end
	  | printDec (A.DecDatWith (db, tb, rs, label, _))       ind =
	    let val (_, _, _, x) = printDatBindSeq db ind
		val (_, j, _, y) = printTypBindSeq tb ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine rs, j, getCol rs, D.printLabelledProgramString("\\operatorname{datatype} " ^ x ^ " \\operatorname{withtype} " ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine rs, j, getCol rs, "datatype " ^ x ^ " withtype " ^ y)
	    end
	  | printDec (A.DecDatRep (tc, ltc, rs, label, _))       ind =
	    let val (_, _, _, x) = printTyCon tc ind
		val (_, j, _, y) = printLongTyCon ltc ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine rs, j, getCol rs, D.printLabelledProgramString("\\operatorname{datatype} " ^ x ^ " = \\opertorname{datatype} " ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine rs, j, getCol rs, "datatype " ^ x ^ " = datatype " ^ y)
	    end
	  | printDec (A.DecType (tbs, r, _))                 ind =
	    let val (_, k, _, x) = printTypBindSeq tbs ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r], k, getCol [r], "\\operatorname{type} " ^ x)
		else (getLine [r], k, getCol [r], "type " ^ x)
	    end
	  | printDec (A.DecEx (ebs, r, _))                   ind =
	    let val (_, k, _, x) = printExBindSeq ebs ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r], k, getCol [r], "\\operatorname{exception} " ^ x)
		else (getLine [r], k, getCol [r], "exception " ^ x)
	    end
	  | printDec (A.DecOpen (ids, r, label, _))              ind =
	    let val (l, k, c, x) = printLongStrSeq ids ind
		val k'  = case k of NONE => getLine [r] | _ => k
		val sep = sepLines (getLine [r]) l c ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r], k', getCol [r], D.printLabelledProgramString("\\operatorname{open} " ^ sep ^ x ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine [r], k', getCol [r], "open " ^ sep ^ x)
	    end
	  | printDec (A.DecLocal (d1, d2, rs, label, _))         ind =
	    let val (_, _, _, x) = printDecs d1 ind
		val (_, _, _, y) = printDecs d2 ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine rs, getLine (rev rs), getCol rs, D.printLabelledProgramString("\\operatorname{local} " ^ x ^ " \\operatorname{in} " ^ y ^ " \\operatorname{end}" ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine rs, getLine (rev rs), getCol rs, "local " ^ x ^ " in " ^ y ^ " end")
	    end
	  | printDec (A.DecAbsType (db, ds, rs, label, _))       ind =
	    let val (_, _, _, x) = printDatBindSeq db ind
		val (_, _, _, y) = printDecs ds ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine rs, getLine (rev rs), getCol rs, D.printLabelledProgramString("\\operatorname{abstype} " ^ x ^ " \\operatorname{with} " ^ y ^ " \\operatorname{end}" ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine rs, getLine (rev rs), getCol rs, "abstype " ^ x ^ " with " ^ y ^ " end")
	    end
	  | printDec (A.DecAbsWith (db, tb, ds, rs, label, _))   ind =
	    let val (_, _, _, x) = printDatBindSeq db ind
		val (_, _, _, y) = printTypBindSeq tb ind
		val (_, _, _, z) = printDecs ds ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine rs, getLine (rev rs), getCol rs, D.printLabelledProgramString("\\operatorname{abstype} " ^ x ^ " \\operatorname{withtype} " ^ y ^ " \\operatorname{with} " ^ z ^ " \\operatorname{end}" ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine rs, getLine (rev rs), getCol rs, "abstype " ^ x ^ " withtype " ^ y ^ " with " ^ z ^ " end")
	    end
	  | printDec (A.DecInfix (i, ids, r, label, _))          ind =
	    let val (_, k, _, x) = printIdentSeq ids ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r], k, getCol [r], D.printLabelledProgramString("\\operatorname{infix} " ^ x ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine [r], k, getCol [r], "infix " ^ x)
	    end
	  | printDec (A.DecInfixr (i, ids, r, label, _))         ind =
	    let val (_, k, _, x) = printIdentSeq ids ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r], k, getCol [r], D.printLabelledProgramString("\\operatorname{infixr} " ^ x ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine [r], k, getCol [r], "infixr " ^ x)
	    end
	  | printDec (A.DecNonfix (ids, r, label, _))            ind =
	    let val (_, k, _, x) = printIdentSeq ids ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r], k, getCol [r], D.printLabelledProgramString("\\operatorname{nonfix} " ^ x ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine [r], k, getCol [r], "nonfix " ^ x)
	    end
	  | printDec (A.DecOverload (i, t, v, s, rs, label, _))  ind =
	    let val (r1, r2, r3, r4) = (* r1: overload, r2: ":", r3: with, r4: in*)
		    case rs of
			[r1, r2, r3, r4] => (r1, r2, r3, r4)
		      | _ => raise EH.DeadBranch "not the correct number of regions"
		val (l1, k1, c1, x) = printLabId      i ind
		val (l2, k2, c2, y) = printLabType    t ind
		val (l3, k3, c3, z) = printLabTypeVar   v ind
		val (l4, k4, c4, w) = printTyClassSeq s ind
		val k1'  = case k1 of NONE => getLine [r1] | _ => k1
		val k2'  = case k2 of NONE => getLine [r2] | _ => k2
		val k3'  = case k3 of NONE => getLine [r3] | _ => k3
		val k4'  = case k4 of NONE => getLine [r4] | _ => k4
		val sep1 = sepLines (getLine [r1]) l1 c1 ind
		val sep2 = sepLines k1' (getLine [r2]) (getCol [r2]) ind
		val sep3 = sepLines (getLine [r2]) l2 c2 ind
		val sep4 = sepLines k2' (getLine [r3]) (getCol [r3]) ind
		val sep5 = sepLines (getLine [r3]) l3 c3 ind
		val sep6 = sepLines k3' (getLine [r4]) (getCol [r4]) ind
		val sep7 = sepLines (getLine [r4]) l4 c4 ind
		val asep = case sep2 of "" => " " | _ => ""
		val bsep = case sep4 of "" => " " | _ => ""
		val csep = case sep6 of "" => " " | _ => ""
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r1], k4', getCol [r1],
		      D.printLabelledProgramString("\\operatorname{overload} " ^ sep1 ^ x ^ sep2 ^ asep ^
		      "\\operatorname{:} "        ^ sep3 ^ y ^ sep4 ^ bsep ^
		      "\\operatorname{with} "     ^ sep5 ^ z ^ sep6 ^ csep ^
		      "\\operatorname{in} "       ^ sep7 ^ w ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine [r1], k4', getCol [r1],
		      "overload " ^ sep1 ^ x ^ sep2 ^ asep ^
		      ": "        ^ sep3 ^ y ^ sep4 ^ bsep ^
		      "with "     ^ sep5 ^ z ^ sep6 ^ csep ^
		      "in "       ^ sep7 ^ w)
	    end
	  | printDec (A.DecClass (i, s, r, label, _))            ind =
	    let val (l1, k1, c1, x) = printLabClass   i ind
		val (l2, k2, c2, y) = printTyClassSeq s ind
		val k1'  = case k1 of NONE => getLine [r] | _ => k1
		val k2'  = case k2 of NONE => k1' | _ => k2
		val sep1 = sepLines (getLine [r]) l1 c1 ind
		val sep2 = sepLines k1' l2 c2 ind
		val asep = case sep2 of "" => " " | _ => ""
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r], k2', getCol [r], D.printLabelledProgramString("\\operatorname{overload} " ^ sep1 ^ x ^ sep2 ^ asep ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine [r], k2', getCol [r], "overload " ^ sep1 ^ x ^ sep2 ^ asep ^ y)
	    end
	  | printDec (A.DecDots pl)                          ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printDecList []                                    _   = (NONE, NONE, NONE, dots)
	  | printDecList (x :: xs)                             ind =
	    let val (l, k, c, x) = printDec x (dind ind)
		val (i, j, d, y) = printDecList xs ind
		val j'  = case j of NONE => k | _ => j
		val sep = sepLines k i d ind
	    in (l, j', c, dots ^ x ^ sep ^ y)
	    end

	and printDecs (A.Decs (dl, _))                       ind =
	    let val (l, k, c, x) = printDecList dl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end
	  | printDecs (A.DecsDots pl)                        ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printExplist []                                    _   = (NONE, NONE, NONE, "")
	  | printExplist [e]                                   ind = printExp e ind
	  | printExplist (e :: el)                             ind =
	    let val (l, k, c, x) = printExp e ind
		val (i, j, d, y) = printExplist el ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, x ^ "\\operatorname{,}" ^ y)
		else (l, j, c, x ^ "," ^ y)
	    end

	and printLabExplist []                                 _   = (NONE, NONE, NONE, "")
	  | printLabExplist [e]                                ind = printLabExp e ind
	  | printLabExplist (e :: el)                          ind =
	    let val (l, k, c, x) = printLabExp e ind
		val (_, j, _, y) = printLabExplist el ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, x ^ "\\operatorname{,}" ^ y)
		else (l, j, c, x ^ "," ^ y)
	    end

	and printLabExpSeq []                                  _   = (NONE, NONE, NONE, "")
	  | printLabExpSeq [x]                                 ind = printLabExp x ind
	  | printLabExpSeq (x :: xs)                           ind =
	    let val (l, k, c, x) = printLabExp x ind
		val (_, j, _, y) = printLabExpSeq xs ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, x ^ "\\operatorname{;}" ^ y)
		else (l, j, c, x ^ ";" ^ y)
	    end

	and printLabExp (A.LabExp (e, _, _, label, _))           ind =
	    let
		val (x1, x2, x3, x4) = printExp e ind
	    in
		
		if !(D.debugProgramLabelling)
		then (x1, x2, x3, D.printLabelledProgramString( x4 ^ "")^"^{" ^ L.printLab(label)^"}")
		else (x1, x2, x3, x4)
	    end
	  (*  | printLabExp (A.LabExpDots [])                    = ldots ^ "exp" ^ rdots*)
	  | printLabExp (A.LabExpDots pl)                    ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printExpField (A.ExpField (tl, e, _, _, label, _))       ind =
	    let val (l, k, c, x) = printTyLab tl ind
		val (_, j, _, y) = printLabExp e ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ "\\operatorname{=}" ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ "=" ^ y)
	    end
	  | printExpField (A.ExpFieldDots pl)                    ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printExpFieldList []                                 _   = (NONE, NONE, NONE, "")
	  | printExpFieldList [x]                                ind = printExpField x ind
	  | printExpFieldList (x :: xs)                          ind =
	    let val (l, k, c, x) = printExpField x ind
		val (_, j, _, y) = printExpFieldList xs ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, x ^ "\\operatorname{,}" ^ y)
		else (l, j, c, x ^ "," ^ y)
	    end

	and printExpFieldSlList []                               _   = (NONE, NONE, NONE, dots)
	  | printExpFieldSlList (x :: xs)                        ind =
	    let val (l, k, c, x) = printExpField x (dind ind)
		val (i, j, d, y) = printExpFieldSlList xs ind
		val sep = sepLines k i d ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, dots ^ "\\operatorname{,}" ^ x ^ "\\operatorname{,}" ^ sep ^ y)
		else (l, j, c, dots ^ "," ^ x ^ "," ^ sep ^ y)
	    end

	and printSeqExp (A.SeqExp (el, e, _, _, label, _))       ind =
	    let
		val (x1, x2, x3, x4) = printLabExpSeq (el @ [e]) ind
	    in
		if !(D.debugProgramLabelling)
		then (x1, x2, x3, D.printLabelledProgramString( x4 ^ "")^"^{" ^ L.printLab(label)^"}")
		else (x1, x2, x3, x4)
	    end
	  | printSeqExp (A.SeqExpSl (pl, e, _, label, _))        ind =
	    let val (l, k, c, x) = printPartDots pl ind
		val (_, j, _, y) = printLabExp e ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ "\\operatorname{;}" ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ ";" ^ y)
	    end
	  | printSeqExp (A.SeqExpDots pl)                    ind = printPartDots pl ind

	and printAtExp (A.AtExpId id)                        ind = printLongId id ind
	  | printAtExp (A.AtExpScon sc)                      ind = printScon sc ind
	  | printAtExp (A.AtExpTuple (el, rs, label, _))         ind =
	    let val (_, _, _, x) = printLabExplist el ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine rs, getLine (rev rs), getCol rs, D.printLabelledProgramString("\\operatorname{(}" ^ x ^ "\\operatorname{)}" ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine rs, getLine (rev rs), getCol rs, "(" ^ x ^ ")")
	    end
	  | printAtExp (A.AtExpRecord (erl, rs, _, label, _))    ind =
	    let val (_, _, _, x) = printExpFieldList erl ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine rs, getLine (rev rs), getCol rs, D.printLabelledProgramString("\\operatorname{\\{}" ^ x ^ "\\operatorname{\\}}" ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine rs, getLine (rev rs), getCol rs, "{" ^ x ^ "}")
	    end
	  | printAtExp (A.AtExpSlRec (erl, rs, label, _))        ind =
	    let val (_, _, _, x) = printExpFieldSlList erl ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine rs, getLine (rev rs), getCol rs, D.printLabelledProgramString("\\operatorname{\\{}" ^ x ^ "\\operatorname{\\}}" ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine rs, getLine (rev rs), getCol rs, "{" ^ x ^ "}")
	    end
	  | printAtExp (A.AtExpLet (d, e, rs, label, _))         ind =
	    let val (r1, r2, r3) =
		    case rs of
			[r1, r2, r3] => (r1, r2, r3)
		      | _ => raise EH.DeadBranch "not the correct number of regions"
		val (l1, k1, c1, x) = printDecs d ind
		val (l2, k2, c2, y) = printLabExp e ind
		val k1'  = case k1 of NONE => getLine [r1] | _ => k1
		val k2'  = case k2 of NONE => getLine [r2] | _ => k2
		val sep1 = sepLines (getLine [r1]) l1 c1 ind
		val sep2 = sepLines k1' (getLine [r2]) (getCol [r2]) ind
		val sep3 = sepLines (getLine [r2]) l2 c2 ind
		val sep4 = sepLines k2' (getLine [r3]) (getCol [r3]) ind
		val isep = case sep2 of "" => " " | _ => ""
		val esep = case sep4 of "" => " " | _ => ""
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r1], getLine [r3], getCol [r1],
		      D.printLabelledProgramString("\\operatorname{let} " ^ sep1 ^ x ^ sep2 ^ isep ^
		      "\\operatorname{in} "  ^ sep3 ^ y ^ sep4 ^ esep ^
		      "\\operatorname{end}" ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine [r1], getLine [r3], getCol [r1],
		      "let " ^ sep1 ^ x ^ sep2 ^ isep ^
		      "in "  ^ sep3 ^ y ^ sep4 ^ esep ^
		      "end")
	    end
	  | printAtExp (A.AtExpDLet (d, s, rs, label, _))        ind =
	    let val (r1, r2, r3) =
		    case rs of
			[r1, r2, r3] => (r1, r2, r3)
		      | _ => raise EH.DeadBranch "not the correct number of regions"
		val (l1, k1, c1, x) = printDecs d ind
		val (l2, k2, c2, y) = printSeqExp s ind
		val k1'  = case k1 of NONE => getLine [r1] | _ => k1
		val k2'  = case k2 of NONE => getLine [r2] | _ => k2
		val sep1 = sepLines (getLine [r1]) l1 c1 ind
		val sep2 = sepLines k1' (getLine [r2]) (getCol [r2]) ind
		val sep3 = sepLines (getLine [r2]) l2 c2 ind
		val sep4 = sepLines k2' (getLine [r3]) (getCol [r3]) ind
		val isep = case sep2 of "" => " " | _ => ""
		val esep = case sep4 of "" => " " | _ => ""
	    in 
		if !(D.debugProgramLabelling)
		then (getLine rs,
		      getLine (rev rs),
		      getCol rs,
		      D.printLabelledProgramString("\\operatorname{let} " ^ sep1 ^ x ^ sep2 ^ isep ^
		      "\\operatorname{in} "  ^ sep3 ^ y ^ sep4 ^ esep ^
		      "\\operatorname{end}" ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine rs,
		      getLine (rev rs),
		      getCol rs,
		      "let " ^ sep1 ^ x ^ sep2 ^ isep ^
		      "in "  ^ sep3 ^ y ^ sep4 ^ esep ^
		      "end")
	    end
	  | printAtExp (A.AtExpParen (e, r1, r2, label, _))      ind =
	    let val (_, _, _, x) = printLabExp e ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r1], getLine [r2], getCol [r1], D.printLabelledProgramString("\\operatorname{(}" ^ x ^ "\\operatorname{)}" ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine [r1], getLine [r2], getCol [r1], "(" ^ x ^ ")")
	    end
	  | printAtExp (A.AtExpList (el, rs, label, _))          ind =
	    let val (_, _, _, x) = printLabExplist el ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine rs, getLine (rev rs), getCol rs, D.printLabelledProgramString("\\operatorname{[}" ^ x ^ "\\operatorname{]}" ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine rs, getLine (rev rs), getCol rs, "[" ^ x ^ "]")
	    end
	  | printAtExp (A.AtExpProj (tl, r, _, label, _))        ind =
	    let val (_, k, _, x) = printTyLab tl ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r], k, getCol [r], D.printLabelledProgramString("\\operatorname{#}" ^ x ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine [r], k, getCol [r], "#" ^ x)
	    end
	  | printAtExp (A.AtExpSeq (seq, rs, label, _))          ind =
	    let val (_, _, _, x) = printSeqExp seq ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine rs, getLine (rev rs), getCol rs, D.printLabelledProgramString("\\operatorname{(}" ^ x ^ "\\operatorname{)}" ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine rs, getLine (rev rs), getCol rs, "(" ^ x ^ ")")
	    end
	  | printAtExp (A.AtExpQuote (quotes, regs, label, _))   ind =
	    let val (_, _, _, x) = printQuotes quotes ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine regs, getLine (rev regs), getCol regs, D.printLabelledProgramString("\\operatorname{`}" ^ x ^ "\\operatorname{`}" ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine regs, getLine (rev regs), getCol regs, "\\operatorname{`}" ^ x ^ "\\operatorname{`}" )
	    end
	  | printAtExp (A.AtExpDots pl)                      ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printQuote (A.Quote (str, reg, label, _))            _   =
	    if !(D.debugProgramLabelling)
	    then (getLine [reg], getLine [reg], getCol [reg], D.printLabelledProgramString( str ^ "")^"^{" ^ L.printLab(label)^"}")
	    else (getLine [reg], getLine [reg], getCol [reg], str)
	  | printQuote (A.Antiquote (exp, [reg], label, _))      ind = (* means it's an identifier *)
	    let val (_, k, _, x) = printExp exp ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [reg], k, getCol [reg], D.printLabelledProgramString("\\operatorname{^}" ^ x ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine [reg], k, getCol [reg], "^" ^ x)
	    end
	  | printQuote (A.Antiquote (exp, regs, label, _))       ind = (* otherwise it's an expression in parentheses *)
	    let val (_, k, _, x) = printExp exp ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine regs, k, getCol regs, D.printLabelledProgramString("\\operatorname{^(}" ^ x ^ "\\operatorname{)}" ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine regs, k, getCol regs, "^(" ^ x ^ ")")
	    end
	  | printQuote (A.QuoteDots pl)                      ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printQuotes []                                   _   = (NONE, NONE, NONE, "")
	  | printQuotes [e]                                  ind = printQuote e ind
	  | printQuotes (e :: el)                            ind =
	    let val (l, k, c, x) = printQuote e ind
		val (_, j, _, y) = printQuotes el ind
	    in (l, j, c, x ^ y)
	    end

	and printExp (A.ExpAtExp ae)                         ind = printAtExp ae ind
	  | printExp (A.ExpFn (m, r, label, _))                  ind =
	    let val (_, k, _, x) = printMatch m ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r], k, getCol [r], D.printLabelledProgramString("\\operatorname{fn} " ^ x ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine [r], k, getCol [r], "fn " ^ x)
	    end
	  | printExp (A.ExpApp (e, ae, _, _, _, label, _))       ind =
	    let val (l, k, c, x) = printExp e ind
		val (_, j, _, y) = printAtExp ae ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ " " ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ " " ^ y)
	    end
	  | printExp (A.ExpCase (e, m, r1, r2, label, _))        ind =
	    let val (l1, k1, c1, x) = printLabExp e ind
		val (l2, k2, c2, y) = printMatch m ind
		val k1'  = case k1 of NONE => getLine [r1] | _ => k1
		val sep1 = sepLines (getLine [r1]) l1 c1 ind
		val sep2 = sepLines k1' (getLine [r2]) (getCol [r2]) ind
		val sep3 = sepLines (getLine [r2]) l2 c2 ind
		val osep = case sep2 of "" => " " | _ => ""
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r1],
		      k2,
		      getCol [r1],
		      D.printLabelledProgramString("\\operatorname{case} " ^ sep1 ^ x ^ sep2 ^ osep ^
		      "\\operatorname{of} "   ^ sep3 ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine [r1],
		      k2,
		      getCol [r1],
		      "case " ^ sep1 ^ x ^ sep2 ^ osep ^
		      "of "   ^ sep3 ^ y)
	    end
	  | printExp (A.ExpConsList (_, e1, e2, _, label, _))    ind =
	    let val (l, k, c, x) = printLabExp e1 ind
		val (_, j, _, y) = printLabExp e2 ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ " \\operatorname::} " ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ " :: " ^ y)
	    end
	  | printExp (A.ExpOp (s, _, e1, e2, _, label, _))       ind =
	    let val (l, k, c, x) = printLabExp e1 ind
		val (_, j, _, y) = printLabExp e2 ind
	    in (l, j, c, 
		if !(D.debugProgramLabelling)
		then D.printLabelledProgramString( x ^ " " ^ s ^ " " ^ y ^ "")^"^{" ^ L.printLab(label)^"}"
		else x ^ " " ^ s ^ " " ^ y)
	    end
	  | printExp (A.ExpOr (e1, e2, _, label, _))             ind =
	    let val (l, k, c, x) = printLabExp e1 ind
		val (_, j, _, y) = printLabExp e2 ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ " \\operatorname{orelse} " ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ " orelse " ^ y)
	    end
	  | printExp (A.ExpAnd (e1, e2, _, label, _))            ind =
	    let val (l, k, c, x) = printLabExp e1 ind
		val (_, j, _, y) = printLabExp e2 ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ " \\operatorname{andalso} " ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ " andalso " ^ y)
	    end
	  | printExp (A.ExpTyped (e, t, _, label, _))            ind =
	    let val (l, k, c, x) = printLabExp e ind
		val (_, j, _, y) = printLabType t ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ " \\operatorname{:} " ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ " : " ^ y)
	    end
	  | printExp (A.ExpIte (e, f, g, rs, label, _))          ind =
	    let val (r1, r2, r3) =
		    case rs of
			[r1, r2, r3] => (r1, r2, r3)
		      | _ => raise EH.DeadBranch "not the correct number of regions"
		val (l1, k1, c1, x) = printLabExp e ind
		val (l2, k2, c2, y) = printLabExp f ind
		val (l3, k3, c3, z) = printLabExp g ind
		val k1'  = case k1 of NONE => getLine [r1] | _ => k1
		val k2'  = case k2 of NONE => getLine [r2] | _ => k2
		val sep1 = sepLines (getLine [r1]) l1 c1 ind
		val sep2 = sepLines k1' (getLine [r2]) (getCol [r2]) ind
		val sep3 = sepLines (getLine [r2]) l2 c2 ind
		val sep4 = sepLines k2' (getLine [r3]) (getCol [r3]) ind
		val sep5 = sepLines (getLine [r3]) l3 c3 ind
		val tsep = case sep2 of "" => " " | _ => ""
		val esep = case sep4 of "" => " " | _ => ""
	    in 
		if !(D.debugProgramLabelling)
		then (getLine rs,
		      k3,
		      getCol rs,
		      D.printLabelledProgramString("\\operatorname{if} "   ^ sep1 ^ x ^ sep2 ^ tsep ^
		      "\\operatorname{then} " ^ sep3 ^ y ^ sep4 ^ esep ^
		      "\\operatorname{else} " ^ sep5 ^ z ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine rs,
		      k3,
		      getCol rs,
		      "if "   ^ sep1 ^ x ^ sep2 ^ tsep ^
		      "then " ^ sep3 ^ y ^ sep4 ^ esep ^
		      "else " ^ sep5 ^ z)
	    end
	  | printExp (A.ExpWhile (e1, e2, r1, r2, label, _))     ind =
	    let val (l1, k1, c1, x) = printLabExp e1 ind
		val (l2, k2, c2, y) = printLabExp e2 ind
		val k1'  = case k1 of NONE => getLine [r1] | _ => k1
		val sep1 = sepLines (getLine [r1]) l1 c1 ind
		val sep2 = sepLines k1' (getLine [r2]) (getCol [r2]) ind
		val sep3 = sepLines (getLine [r2]) l2 c2 ind
		val dsep = case sep2 of "" => " " | _ => ""
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r1],
		      k2,
		      getCol [r1],
		      D.printLabelledProgramString("\\operatorname{while} " ^ sep1 ^ x ^ sep2 ^ dsep ^
		      "\\operatorname{do} "    ^ sep3 ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine [r1],
		      k2,
		      getCol [r1],
		      "while " ^ sep1 ^ x ^ sep2 ^ dsep ^
		      "do "    ^ sep3 ^ y)
	    end
	  | printExp (A.ExpRaise (e, r, label, _))               ind =
	    let val (_, k, _, x) = printLabExp e ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r], k, getCol [r], D.printLabelledProgramString("\\operatorname{raise} " ^ x ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine [r], k, getCol [r], "raise " ^ x)
	    end
	  | printExp (A.ExpHandle (e, m, _, label, _))           ind =
	    let val (l1, k1, c1, x) = printLabExp e ind
		val (_,  k2, _,  y) = printMatch m ind
	    in 
		if !(D.debugProgramLabelling)
		then (l1, k2, c1, D.printLabelledProgramString( x ^ " \\operatorname{handle} " ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l1, k2, c1, x ^ " handle " ^ y)
	    end
	  | printExp (A.ExpDots pl)                          ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printMruleList []                                  _   = (NONE, NONE, NONE, dots)
	  | printMruleList (mr :: mrl)                         ind =
	    let val (l, k, c, x) = printMrule mr (dind ind)
		val (i, j, d, y) = printMruleList mrl ind
		val sep = sepLines k i d ind
	    in (l, j, c, dots ^ x ^ sep ^ y)
	    end

	and printMatch (A.Match (l, _, _))                   ind =
	    let val (l, k, c, x) = printMruleList l (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end
	  | printMatch (A.MatchDots pl)                      ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printMrule (A.Mrule (p, e, r, label, _))             ind =
	    let val (l1, k1, c1, x) = printLabPat p ind
		val (l2, k2, c2, y) = printLabExp e ind
		val sep1 = sepLines k1 (getLine [r]) (getCol [r]) ind
		val sep2 = sepLines (getLine [r]) l2 c2 ind
		val asep = case sep1 of "" => " " | _ => ""
	    in 
		if !(D.debugProgramLabelling)
		then (l1, k2, c1, D.printLabelledProgramString( x ^ sep1 ^ asep ^ "\\operatorname{=>} " ^ sep2 ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l1, k2, c1, x ^ sep1 ^ asep ^ "=> " ^ sep2 ^ y)
	    end
	  | printMrule (A.MruleDots pl)                      ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printPatList []                                    _   = (NONE, NONE, NONE, "")
	  | printPatList [p]                                   ind = printPat p ind
	  | printPatList (p :: pl)                             ind =
	    let val (l, k, c, x) = printPat p ind
		val (_, j, _, y) = printPatList pl ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, x ^ "\\operatorname{,}" ^ y)
		else (l, j, c, x ^ "," ^ y)
	    end

	and printLabPatList []                                 _   = (NONE, NONE, NONE, "")
	  | printLabPatList [p]                                ind = printLabPat p ind
	  | printLabPatList (p :: pl)                          ind =
	    let val (l, k, c, x) = printLabPat p ind
		val (_, j, _, y) = printLabPatList pl ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, x ^ "\\operatorname{,}" ^ y)
		else (l, j, c, x ^ "," ^ y)
	    end

	and printOrPatList []                                  _   = (NONE, NONE, NONE, "")
	  | printOrPatList [p]                                 ind = printLabPat p ind
	  | printOrPatList (p :: pl)                           ind =
	    let val (l, k, c, x) = printLabPat p ind
		val (_, j, _, y) = printLabPatList pl ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, x ^ "\\operatorname{|}" ^ y)
		else (l, j, c, x ^ "|" ^ y)
	    end

	and printPatDots []                                    _   = (NONE, NONE, NONE, dots)
	  | printPatDots (p :: pl)                             ind =
	    let val (l, k, c, x) = printPat p (dind ind)
		val (_, j, _, y) = printPatDots pl (dind ind)
	    in (l, j, c, dots ^ x ^ y)
	    end

	and printLabPat (A.LabPat (p, _, _, label, _))           ind =
	    let
		val (x1, x2, x3, x4) = printPat p ind
	    in
		if !(D.debugProgramLabelling)
		then (x1, x2, x3, D.printLabelledProgramString( x4 ^ "")^"^{" ^ L.printLab(label)^"}")
		else (x1, x2, x3, x4)
	    end
	  | printLabPat (A.LabPatDots pl)                    ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printPatField (A.PatField (tl, p, _, _, label, _))       ind =
	    let val (l, k, c, x) = printTyLab tl ind
		val (_, j, _, y) = printLabPat p ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ "\\operatorname{=}" ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ "=" ^ y)
	    end
	  | printPatField (A.PatFieldId (id, _))                 ind = printIdentTy id ind
	  | printPatField (A.PatFieldAs (id, p, _, label, _))        ind =
	    let val (l, k, c, x) = printLabIdTy id ind
		val (_, j, _, y) = printLabPat p ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ " \\operatorname{as} " ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ " as " ^ y)
	    end
	  | printPatField (A.PatFieldWild (r, label, _))             _   =
	    if !(D.debugProgramLabelling)
	    then (getLine [r], getLine [r], getCol [r], D.printLabelledProgramString("..." ^ "")^"^{" ^ L.printLab(label)^"}")
	    else (getLine [r], getLine [r], getCol [r], "...")
	  | printPatField (A.PatFieldDots pl)                    ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printPatFieldList []                                 _   = (NONE, NONE, NONE, "")
	  | printPatFieldList [x]                                ind = printPatField x ind
	  | printPatFieldList (x :: xs)                          ind =
	    let val (l, k, c, x) = printPatField x ind
		val (_, j, _, y) = printPatFieldList xs ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, x ^ "\\operatorname{,}" ^ y)
		else (l, j, c, x ^ "," ^ y)
	    end

	and printAtPat (A.AtPatWild (r, _))                  _   = (getLine [r], getLine [r], getCol [r], "_")
	  | printAtPat (A.AtPatId id)                        ind = printLongId id ind
	  | printAtPat (A.AtPatScon sc)                      ind = printScon sc ind
	  | printAtPat (A.AtPatTuple (pl, rs, label, _))         ind =
	    let val (_, _, _, x) = printLabPatList pl ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine rs, getLine (rev rs), getCol rs, D.printLabelledProgramString("\\operatorname{(}" ^ x ^ "\\operatorname{)}" ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine rs, getLine (rev rs), getCol rs, "(" ^ x ^ ")")
	    end
	  | printAtPat (A.AtPatRecord (prl, rs, _, label, _))    ind =
	    let val (_, _, _, x) = printPatFieldList prl ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine rs, getLine (rev rs), getCol rs, D.printLabelledProgramString("{" ^ x ^ "}" ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine rs, getLine (rev rs), getCol rs, "\\operatorname{\\{}" ^ x ^ "\\operatorname{\\}}")
	    end
	  | printAtPat (A.AtPatParen (p, r1, r2, label, _))      ind =
	    let val (_, _, _, x) = printLabPat p ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine [r1], getLine [r2], getCol [r1], D.printLabelledProgramString("\\operatorname{(}" ^  x ^ "\\operatorname{)}" ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine [r1], getLine [r2], getCol [r1], "(" ^  x ^ ")")
	    end
	  | printAtPat (A.AtPatList (pl, rs, label, _))          ind =
	    let val (_, _, _, x) = printLabPatList pl ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine rs, getLine (rev rs), getCol rs, D.printLabelledProgramString("\\operatorname{[}" ^ x ^ "\\operatorname{]}"^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine rs, getLine (rev rs), getCol rs, "[" ^ x ^ "]")
	    end
	  | printAtPat (A.AtPatOr (xs, rs, label, _))            ind =
	    let val (_, _, _, x) = printOrPatList xs ind
	    in 
		if !(D.debugProgramLabelling)
		then (getLine rs, getLine (rev rs), getCol rs, D.printLabelledProgramString("\\operatorname{(}" ^ x ^ "\\operatorname{)}" ^ "")^"^{" ^ L.printLab(label)^"}")
		else (getLine rs, getLine (rev rs), getCol rs, "(" ^ x ^ ")")
	    end
	  | printAtPat (A.AtPatDots pl)                      ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printIdentTy (A.IdentTyId id)                    ind = printIdent id ind
	  | printIdentTy (A.IdentTyTy (id, t, _, _, _))      ind =
	    let val (l, k, c, x) = printLabId id ind
		val (_, j, _, y) = printLabType t ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, x ^ " \\operatorname{:} " ^ y)
		else (l, j, c, x ^ " : " ^ y)
	    end
	  | printIdentTy (A.IdentTyDots pl)                  ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printLabIdTy (A.LabIdTy (id, _, label, _))           ind =
	    let
		val (x1, x2, x3, x4) = printIdentTy id ind
	    in
		if !(D.debugProgramLabelling)
		then (x1, x2, x3, D.printLabelledProgramString( x4 ^ "")^"^{" ^ L.printLab(label)^"}")
		else (x1, x2, x3, x4)
	    end
	  | printLabIdTy (A.LabIdTyDots pl)                  ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

	and printPat (A.PatAtPat ap)                         ind = printAtPat ap ind
	  | printPat (A.PatApp (id, ap, _, _, label, _))         ind =
	    let val (l, k, c, x) = printLongId id ind
		val (_, j, _, y) = printAtPat ap ind
	    in
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ " " ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ " " ^ y)
	    end
	  | printPat (A.PatConsList (_, p1, p2, _, label, _))    ind =
	    let val (l, k, c, x) = printLabPat p1 ind
		val (_, j, _, y) = printLabPat p2 ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ " \\operatorname{::} " ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ " :: " ^ y)
	    end
	  | printPat (A.PatOp (st, _, p1, p2, _, label, _))      ind =
	    let val (l, k, c, x) = printLabPat p1 ind
		val (_, j, _, y) = printLabPat p2 ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ " " ^ st ^ " " ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ " " ^ st ^ " " ^ y)
	    end
	  | printPat (A.PatTyped (p, t, _, label, _))            ind =
	    let val (l, k, c, x) = printLabPat p ind
		val (_, j, _, y) = printLabType t ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ " \\operatorname{:} " ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ " : " ^ y)
	    end
	  | printPat (A.PatAs (id, p, _, label, _))              ind =
	    let val (l, k, c, x) = printLabIdTy id ind
		val (_, j, _, y) = printLabPat p ind
	    in 
		if !(D.debugProgramLabelling)
		then (l, j, c, D.printLabelledProgramString( x ^ " \\operatorname{as} " ^ y ^ "")^"^{" ^ L.printLab(label)^"}")
		else (l, j, c, x ^ " as " ^ y)
	    end
	  | printPat (A.PatDots pl)                          ind =
	    let val (l, k, c, x) = printPartDots pl (pind ind)
	    in
		if !(D.debugProgramLabelling)
		then (l, k, c, ldotsLatex ^ x ^ rdotsLatex)
		else (l, k, c, ldots ^ x ^ rdots)
	    end

    (*val _ = Debug.printdebug2 ("PRINTING")*)

    in printProgs slprog indent
    end

fun printSlice slprog sep = printSlice' slprog "" sep

fun printOneSlice sl bslice sep = "\n" ^ printSlice' sl (sep ^ "  ") bslice ^ "\n" ^ sep

fun printOneSmlSlice sl = "slice = " ^ "\"" ^ printSlice sl true ^ "\""

fun printSlices []        = ""
  | printSlices [x]       = "SLICE:\n" ^ printSlice x true
  | printSlices (x :: xs) = "SLICE:\n" ^ printSlice x true ^ "\n" ^ printSlices xs



(* SLICER *)



fun flattenType [] = []
  | flattenType ((A.TypeDots sltp) :: xs) = sltp @ (flattenType xs)
  | flattenType (x :: xs) = (A.PartType x) :: (flattenType xs)

fun flattenTypeVar [] = []
  | flattenTypeVar (A.TypeVarDots :: xs) = flattenTypeVar xs
  | flattenTypeVar (x :: xs) = (A.PartType (A.TypeOneVar x)) :: (flattenTypeVar xs)

fun flattenLabTypeVar [] = []
  | flattenLabTypeVar ((A.LabTypeVarDots sltp) :: xs) = (flattenTypeVar sltp) @ (flattenLabTypeVar xs)
  | flattenLabTypeVar ((A.LabTypeVar (t, _, _, _)) :: xs) = (A.PartType (A.TypeOneVar t)) :: (flattenLabTypeVar xs)

fun flattenLabType [] = []
  | flattenLabType ((A.LabTypeDots sltp) :: xs) = sltp @ (flattenLabType xs)
  | flattenLabType ((A.LabType (t, _, _, _)) :: xs) = (A.PartType t) :: (flattenLabType xs)

fun flattenTypeRow [] = []
  | flattenTypeRow ((A.TypeRowDots sltp) :: xs) = sltp @ (flattenTypeRow xs)
  | flattenTypeRow (x :: xs) = (A.PartSeq x) :: (flattenTypeRow xs)

fun flattenSeqExp [] = []
  | flattenSeqExp ((A.SeqExpDots slp) :: xs) = slp @ (flattenSeqExp xs)
  | flattenSeqExp _ = raise EH.DeadBranch "flattening of a row of expressions failed" (* we might want to relax this condition *)

fun flattenAtExp [] = []
  | flattenAtExp ((A.AtExpDots slp) :: xs) = slp @ (flattenAtExp xs)
  | flattenAtExp (x :: xs) = (A.PartExp (A.ExpAtExp x)) :: (flattenAtExp xs)

fun flattenExp [] = []
  | flattenExp ((A.ExpDots slp) :: xs) = slp @ (flattenExp xs)
  | flattenExp (x :: xs) = (A.PartExp x) :: (flattenExp xs)

and flattenQuote [] = []
  | flattenQuote ((A.QuoteDots parts) :: xs) = parts @ (flattenQuote xs)
  | flattenQuote ((A.Quote _) :: xs) = flattenQuote xs
  | flattenQuote ((A.Antiquote (exp, _, _, _)) :: xs) = (A.PartExp exp) :: (flattenQuote xs)

fun flattenLabExp [] = []
  | flattenLabExp ((A.LabExpDots slp) :: xs) = slp @ (flattenLabExp xs)
  | flattenLabExp ((A.LabExp (e, _, _, _, _)) :: xs) = (A.PartExp e) :: (flattenLabExp xs)

fun flattenDec [] = []
  | flattenDec ((A.DecDots slp) :: xs) = slp @ (flattenDec xs)
  | flattenDec (x :: xs) = (A.PartDec x) :: (flattenDec xs)

fun flattenAtPat [] = []
  | flattenAtPat ((A.AtPatDots slp) :: xs) = slp @ (flattenAtPat xs)
  | flattenAtPat (x :: xs) = (A.PartPat (A.PatAtPat x)) :: (flattenAtPat xs)

fun flattenPat [] = []
  | flattenPat ((A.PatDots slp) :: xs) = slp @ (flattenPat xs)
  | flattenPat (x :: xs) = (A.PartPat x) :: (flattenPat xs)

fun flattenLabPat [] = []
  | flattenLabPat ((A.LabPatDots slp) :: xs) = slp @ (flattenLabPat xs)
  | flattenLabPat ((A.LabPat (p, _, _, _, _)) :: xs) = (A.PartPat p) :: (flattenLabPat xs)

fun flattenLabId [] = []
  | flattenLabId ((A.LabIdDots pl) :: xs) = pl @ (flattenLabId xs)
  | flattenLabId ((A.LabId (id, _, _, _)) :: xs) = (A.PartLgid (A.LongIdId id)) :: (flattenLabId xs)

fun flattenIdent [] = []
  | flattenIdent (A.IdentDots :: xs) = flattenIdent xs
  | flattenIdent (x :: xs) = (A.PartLgid (A.LongIdId x)) :: (flattenIdent xs)

fun flattenLabClass [] = []
  | flattenLabClass ((A.LabClassDots pl) :: xs) = pl @ (flattenLabClass xs)
  | flattenLabClass ((A.LabClass (id, _, _, _)) :: xs) = (A.PartClass id) :: (flattenLabClass xs)

fun flattenClass [] = []
  | flattenClass (A.ClassDots :: xs) = flattenClass xs
  | flattenClass (x :: xs) = (A.PartClass x) :: (flattenClass xs)

fun flattenTyClass [] = []
  | flattenTyClass ((A.TyClassDots pl) :: xs) = pl @ (flattenTyClass xs)
  | flattenTyClass ((A.TyClassCl (cl, _, _, _)) :: xs) = (flattenLabClass [cl]) @ (flattenTyClass xs)
  | flattenTyClass ((A.TyClassTy (ty, _, _)) :: xs) = (flattenType [ty]) @ (flattenTyClass xs)

fun flattenLabTyClass [] = []
  | flattenLabTyClass ((A.LabTyClassDots slp) :: xs) = slp @ (flattenLabTyClass xs)
  | flattenLabTyClass ((A.LabTyClass (x, _, _, _)) :: xs) = (flattenTyClass [x]) @ (flattenLabTyClass xs)

fun flattenTyClassSeq [] = []
  | flattenTyClassSeq ((A.TyClassSeqDots pl) :: xs) = pl @ (flattenTyClassSeq xs)
  | flattenTyClassSeq ((A.TyClassSeqOne (t, _, _, _)) :: xs) = (flattenTyClass [t]) @ (flattenTyClassSeq xs)
  | flattenTyClassSeq ((A.TyClassSeqEm _) :: xs) = (flattenTyClassSeq xs)
  | flattenTyClassSeq ((A.TyClassSeqSeq (tl, _, _, _)) :: xs) = (flattenLabTyClass tl) @ (flattenTyClassSeq xs)

fun flattenTyCon [] = []
  | flattenTyCon (A.TyConDots :: xs) = flattenTyCon xs
  | flattenTyCon (x :: xs) = (A.PartTyCon (A.LongTyConId x)) :: (flattenTyCon xs)

fun flattenSigId [] = []
  | flattenSigId (A.SigIdDots :: xs) = flattenSigId xs
  | flattenSigId (x :: xs) = (A.PartSigid x) :: (flattenSigId xs)

fun flattenLongId [] = []
  | flattenLongId ((A.LongIdDots pl) :: xs) = pl @ (flattenLongId xs)
  | flattenLongId (x :: xs) = (A.PartLgid x) :: (flattenLongId xs)

fun flattenLongTyCon [] = []
  | flattenLongTyCon ((A.LongTyConDots pl) :: xs) = pl @ (flattenLongTyCon xs)
  | flattenLongTyCon (x :: xs) = (A.PartTyCon x) :: (flattenLongTyCon xs)

fun flattenLongStrId [] = []
  | flattenLongStrId ((A.LongStrIdDots pl) :: xs) = pl @ flattenLongStrId xs
  | flattenLongStrId (x :: xs) = (A.PartLgsid x) :: (flattenLongStrId xs)

fun flattenLongStrId' [] = []
  | flattenLongStrId' ((A.LongStrIdDots x) :: (A.LongStrIdDots y):: xs) = flattenLongStrId' ((A.LongStrIdDots (x @ y)) :: xs)
  | flattenLongStrId' ((A.LongStrIdDots []) :: xs) = flattenLongStrId' xs
  | flattenLongStrId' (x :: xs) = x :: (flattenLongStrId' xs)

fun flattenSigId' [] = []
  | flattenSigId' (A.SigIdDots :: xs) = flattenSigId' xs
  | flattenSigId' (x :: xs) = x :: (flattenSigId' xs)

fun flattenIdent' [] = []
  | flattenIdent' (A.IdentDots :: xs) = flattenIdent' xs
  | flattenIdent' (x :: xs) = x :: (flattenIdent' xs)

fun flattenIdentTy [] = []
  | flattenIdentTy ((A.IdentTyDots pl) :: xs) = pl @ (flattenIdentTy xs)
  | flattenIdentTy (x :: xs) = (A.PartIdTy x) :: (flattenIdentTy xs)

fun flattenLongTyCon [] = []
  | flattenLongTyCon ((A.LongTyConDots pl) :: xs) = pl @ (flattenLongTyCon xs)
  | flattenLongTyCon (x :: xs) = (A.PartTyCon x) :: (flattenLongTyCon xs)

fun flattenLongTyConEq [] = []
  | flattenLongTyConEq ((A.LongTyConEqDots pl) :: xs) = pl @ (flattenLongTyConEq xs)
  | flattenLongTyConEq ((A.LongTyConEq (ys, _, _, _)) :: xs) = (flattenLongTyCon ys) @ (flattenLongTyConEq xs)

fun flattenLongStrIdEq [] = []
  | flattenLongStrIdEq ((A.LongStrIdEqDots pl) :: xs) = pl @ (flattenLongStrIdEq xs)
  | flattenLongStrIdEq ((A.LongStrIdEq (ys, _, _)) :: xs) = (flattenLongStrId ys) @ (flattenLongStrIdEq xs)

fun flattenStrId [] = []
  | flattenStrId (A.StrIdDots :: xs) = flattenStrId xs
  | flattenStrId (x :: xs) = (A.PartLgsid (A.LongStrIdId x)) :: (flattenStrId xs)

fun flattenConBind [] = []
  | flattenConBind ((A.ConBindDots x) :: (A.ConBindDots y) :: xs) = flattenConBind ((A.ConBindDots (x @ y)) ::  xs)
  | flattenConBind ((A.ConBindDots []) :: xs) = flattenConBind xs
  | flattenConBind (x :: xs) = x :: (flattenConBind xs)

fun flattenConDescOne [] = []
  | flattenConDescOne ((A.ConDescOneDots x) :: (A.ConDescOneDots y) :: xs) = flattenConDescOne ((A.ConDescOneDots (x @ y)) ::  xs)
  | flattenConDescOne ((A.ConDescOneDots []) :: xs) = flattenConDescOne xs
  | flattenConDescOne (x :: xs) = x :: (flattenConDescOne xs)

fun flattenDatBind [] = []
  | flattenDatBind ((A.DatBindDots x) :: (A.DatBindDots y) :: xs) = flattenDatBind ((A.DatBindDots (x @ y)) :: xs)
  | flattenDatBind ((A.DatBindDots []) :: xs) = flattenDatBind xs
  | flattenDatBind (x :: xs) = x :: (flattenDatBind xs)

fun flattenTypBind [] = []
  | flattenTypBind ((A.TypBindDots x) :: (A.TypBindDots y) :: xs) = flattenTypBind ((A.TypBindDots (x @ y)) :: xs)
  | flattenTypBind ((A.TypBindDots []) :: xs) = flattenTypBind xs
  | flattenTypBind (x :: xs) = x :: (flattenTypBind xs)

fun flattenExBind [] = []
  | flattenExBind ((A.ExBindDots x) :: (A.ExBindDots y) :: xs) = flattenExBind ((A.ExBindDots (x @ y)) :: xs)
  | flattenExBind ((A.ExBindDots []) :: xs) = flattenExBind xs
  | flattenExBind (x :: xs) = x :: (flattenExBind xs)

fun flattenValBindCore [] = []
  | flattenValBindCore ((A.ValBindCoreDots x) :: (A.ValBindCoreDots y) :: xs) = flattenValBindCore ((A.ValBindCoreDots (x @ y)) :: xs)
  | flattenValBindCore ((A.ValBindCoreDots []) :: xs) = flattenValBindCore xs
  | flattenValBindCore (x :: xs) = x :: (flattenValBindCore xs)

fun flattenLTReaDOne [] = []
  | flattenLTReaDOne ((A.LTReaDOneDots x) :: (A.LTReaDOneDots y) :: xs) = flattenLTReaDOne ((A.LTReaDOneDots (x @ y)) :: xs)
  | flattenLTReaDOne ((A.LTReaDOneDots []) :: xs) = flattenLTReaDOne xs
  | flattenLTReaDOne (x :: xs) = x :: (flattenLTReaDOne xs)

fun flattenMrule [] = []
  | flattenMrule ((A.MruleDots x) :: (A.MruleDots y) :: xs) = flattenMrule ((A.MruleDots (x @ y)) :: xs)
  | flattenMrule ((A.MruleDots []) :: xs) = flattenMrule xs
  | flattenMrule (x :: xs) = x :: (flattenMrule xs)

fun flattenTyField [] = []
  | flattenTyField ((A.TyFieldDots x) :: (A.TyFieldDots y) :: xs) = flattenTyField ((A.TyFieldDots (x @ y)) :: xs)
  | flattenTyField ((A.TyFieldDots []) :: xs) = flattenTyField xs
  | flattenTyField (x :: xs) = x :: (flattenTyField xs)

fun flattenExpField [] = []
  | flattenExpField ((A.ExpFieldDots x) :: (A.ExpFieldDots y) :: xs) = flattenExpField ((A.ExpFieldDots (x @ y)) :: xs)
  | flattenExpField ((A.ExpFieldDots []) :: xs) = flattenExpField xs
  | flattenExpField (x :: xs) = x :: (flattenExpField xs)

fun flattenPatField [] = []
  | flattenPatField ((A.PatFieldDots x) :: (A.PatFieldDots y) :: xs) = flattenPatField ((A.PatFieldDots (x @ y)) :: xs)
  | flattenPatField (x :: xs) = x :: (flattenPatField xs)

fun flattenFValBindOne [] = []
  | flattenFValBindOne ((A.FVBOneDots x) :: (A.FVBOneDots y) :: xs) = flattenFValBindOne ((A.FVBOneDots (x @ y)) :: xs)
  | flattenFValBindOne ((A.FVBOneDots []) :: xs) = flattenFValBindOne xs
  | flattenFValBindOne (x :: xs) = x :: (flattenFValBindOne xs)

fun flattenFValBindCore [] = []
  | flattenFValBindCore ((A.FVBCoreDots x) :: (A.FVBCoreDots y) :: xs) = flattenFValBindCore ((A.FVBCoreDots (x @ y)) :: xs)
  | flattenFValBindCore ((A.FVBCoreDots []) :: xs) = flattenFValBindCore xs
  | flattenFValBindCore (x :: xs) = x :: (flattenFValBindCore xs)

fun flattenDecs [] = []
  | flattenDecs ((A.DecsDots pl) :: xs) = pl @ (flattenDecs xs)
  | flattenDecs ((A.Decs (dl, _)) :: xs) = (flattenDec dl) @ (flattenDecs xs)

fun flattenSpecOne [] = []
  | flattenSpecOne ((A.SpecOneDots pl) :: xs) = pl @ (flattenSpecOne xs)
  | flattenSpecOne (x :: xs) = (A.PartSpec x) :: (flattenSpecOne xs)

fun flattenLabSigExp [] = []
  | flattenLabSigExp ((A.LabSigExpDots slp) :: xs) = slp @ (flattenLabSigExp xs)
  | flattenLabSigExp ((A.LabSigExp (e, _, _, _, _)) :: xs) = (A.PartSige e) :: (flattenLabSigExp xs)

fun flattenSigExp [] = []
  | flattenSigExp ((A.SigExpDots pl) :: xs) = pl @ (flattenSigExp xs)
  | flattenSigExp (x :: xs) = (A.PartSige x) :: (flattenSigExp xs)

fun flattenLabStrExp [] = []
  | flattenLabStrExp ((A.LabStrExpDots slp) :: xs) = slp @ (flattenLabStrExp xs)
  | flattenLabStrExp ((A.LabStrExp (e, _, _, _, _)) :: xs) = (A.PartStre e) :: (flattenLabStrExp xs)

fun flattenStrExp [] = []
  | flattenStrExp ((A.StrExpDots pl) :: xs) = pl @ (flattenStrExp xs)
  | flattenStrExp (x :: xs) = (A.PartStre x) :: (flattenStrExp xs)

and flattenSpec [] = []
  | flattenSpec ((A.Spec (spol, _)) :: xs) = (flattenSpecOne spol) @ (flattenSpec xs)
  | flattenSpec ((A.SpecDots pl) :: xs) = pl @ (flattenSpec xs)

and flattenStrDec [] = []
  | flattenStrDec ((A.StrDecDots pl) :: xs) = pl @ (flattenStrDec xs)
  | flattenStrDec ((A.StrDec (x, _, _)) :: xs) = (flattenStrDecOne x) @ (flattenStrDec xs)

(* TODO: TO CHANGE! *)
and flattenStrDecOne [] = []
  | flattenStrDecOne ((A.StrDecOneDots pl) :: xs) = pl @ (flattenStrDecOne xs)
  | flattenStrDecOne ((A.StrDecOneDec ds) :: xs) = (flattenDecs [ds]) @ (flattenStrDecOne xs)
  | flattenStrDecOne (x :: xs) = (A.PartStrd x) :: (flattenStrDecOne xs)

and flattenSigDec [] = []
  | flattenSigDec ((A.SigDecDots pl) :: xs) = pl @ (flattenSigDec xs)
  | flattenSigDec ((x as (A.SigDec _)) :: xs) = (A.PartSigd x) :: (flattenSigDec xs)

and flattenATopDec [] = []
  | flattenATopDec ((A.ATopDecDots pl) :: xs) = pl @ (flattenATopDec xs)
  | flattenATopDec ((A.ATopDecStr s) :: xs) = (flattenStrDec [s]) @ (flattenATopDec xs)
  | flattenATopDec ((A.ATopDecSig s) :: xs) = (flattenSigDec [s]) @ (flattenATopDec xs)

and flattenSmlTes [] = []
  | flattenSmlTes ((A.SmlTesDots pl) :: xs) = pl @ (flattenSmlTes xs)
  | flattenSmlTes (x :: xs) = (A.PartTes x) :: (flattenSmlTes xs)

(*and flattenFunDec [] = []
  | flattenFunDec ((A.FunDecDots pl) :: xs) = pl @ (flattenFunDec xs)
  | flattenFunDec ((x as (A.FunDec _)) :: xs) = (A.PartFund x) :: (flattenFunDec xs)*)

and flattenValDescOne [] = []
  | flattenValDescOne ((A.ValDescOneDots x) :: (A.ValDescOneDots y) :: xs) = flattenValDescOne ((A.ValDescOneDots (x @ y)) :: xs)
  | flattenValDescOne ((A.ValDescOneDots []) :: xs) = flattenValDescOne xs
  | flattenValDescOne (x :: xs) = x :: (flattenValDescOne xs)

and flattenTypDescOne [] = []
  | flattenTypDescOne ((A.TypDescOneDots x) :: (A.TypDescOneDots y) :: xs) = flattenTypDescOne ((A.TypDescOneDots (x @ y)) :: xs)
  | flattenTypDescOne ((A.TypDescOneDots []) :: xs) = flattenTypDescOne xs
  | flattenTypDescOne (x :: xs) = x :: (flattenTypDescOne xs)

and flattenExcDescOne [] = []
  | flattenExcDescOne ((A.ExcDescOneDots x) :: (A.ExcDescOneDots y) :: xs) = flattenExcDescOne ((A.ExcDescOneDots (x @ y)) :: xs)
  | flattenExcDescOne ((A.ExcDescOneDots []) :: xs) = flattenExcDescOne xs
  | flattenExcDescOne (x :: xs) = x :: (flattenExcDescOne xs)

and flattenTdrDescOne [] = []
  | flattenTdrDescOne ((A.TdrDescOneDots x) :: (A.TdrDescOneDots y) :: xs) = flattenTdrDescOne ((A.TdrDescOneDots (x @ y)) :: xs)
  | flattenTdrDescOne ((A.TdrDescOneDots []) :: xs) = flattenTdrDescOne xs
  | flattenTdrDescOne (x :: xs) = x :: (flattenTdrDescOne xs)

and flattenDatDescOne [] = []
  | flattenDatDescOne ((A.DatDescOneDots x) :: (A.DatDescOneDots y) :: xs) = flattenDatDescOne ((A.DatDescOneDots (x @ y)) :: xs)
  | flattenDatDescOne ((A.DatDescOneDots []) :: xs) = flattenDatDescOne xs
  | flattenDatDescOne (x :: xs) = x :: (flattenDatDescOne xs)

and flattenStrDescOne [] = []
  | flattenStrDescOne ((A.StrDescOneDots x) :: (A.StrDescOneDots y) :: xs) = flattenStrDescOne ((A.StrDescOneDots (x @ y)) :: xs)
  | flattenStrDescOne ((A.StrDescOneDots []) :: xs) = flattenStrDescOne xs
  | flattenStrDescOne (x :: xs) = x :: (flattenStrDescOne xs)

and flattenStrBindOne [] = []
  | flattenStrBindOne ((A.StrBindOneDots x) :: (A.StrBindOneDots y) :: xs) = flattenStrBindOne ((A.StrBindOneDots (x @ y)) :: xs)
  | flattenStrBindOne ((A.StrBindOneDots []) :: xs) = flattenStrBindOne xs
  | flattenStrBindOne (x :: xs) = x :: (flattenStrBindOne xs)

and flattenSigBindOne [] = []
  | flattenSigBindOne ((A.SigBindOneDots x) :: (A.SigBindOneDots y) :: xs) = flattenSigBindOne ((A.SigBindOneDots (x @ y)) :: xs)
  | flattenSigBindOne ((A.SigBindOneDots []) :: xs) = flattenSigBindOne xs
  | flattenSigBindOne (x :: xs) = x :: (flattenSigBindOne xs)

and flattenFunBindOne [] = []
  | flattenFunBindOne ((A.FunBindODots x) :: (A.FunBindODots y) :: xs) = flattenFunBindOne ((A.FunBindODots (x @ y)) :: xs)
  | flattenFunBindOne ((A.FunBindODots []) :: xs) = flattenFunBindOne xs
  | flattenFunBindOne (x :: xs) = x :: (flattenFunBindOne xs)

fun flattenTopDecOne [] = []
  | flattenTopDecOne ((A.TopDecOneDots x) :: (A.TopDecOneDots y) :: xs) = flattenTopDecOne ((A.TopDecOneDots (x @ y)) :: xs)
  | flattenTopDecOne ((A.TopDecOneDots []) :: xs) = flattenTopDecOne xs
  | flattenTopDecOne (x :: xs) = x :: (flattenTopDecOne xs)

fun flattenProgOne [] = []
  | flattenProgOne ((A.ProgOneDots x) :: (A.ProgOneDots y) :: xs) = flattenProgOne ((A.ProgOneDots (x @ y)) :: xs)
  | flattenProgOne ((A.ProgOneDots []) :: xs) = flattenProgOne xs
  | flattenProgOne (x :: xs) = x :: (flattenProgOne xs)


(* functions defined in case something wrong, uses the error hadler *)
fun msgEmpty labs =
    EH.msg ("slicing constraint non respected: " ^ L.toString labs ^ " should be empty")
fun msgOne lab labs =
    EH.msg ("slicing constraint non respected: " ^ L.printLab lab ^
	    " should be the only element in " ^ L.toString labs)

(* generates type error slices *)
fun slice prog labels =

    let

	val strictLab = true

	fun splitList nxt ll =
	    (if strictLab then L.split (Option.valOf nxt) ll else (ll, ll))
	    handle Option => raise EH.DeadBranch "undefined 'next' label"

	(*fun splitListTwo (SOME nxt1) (SOME nxt2) ll =
		  if nxt1 < nxt2
		  then L.splitList nxt1 ll
		  else (fn (x, y) => (y, x)) (L.splitList nxt2 ll)
		| splitListTwo (SOME nxt) NONE ll = L.splitList nxt ll
		| splitListTwo NONE _ _ = raise EH.DeadBranch "undefined 'next' label"*)

	fun splitListFirst (SOME nxt) (SOME fst) ll = L.split2 fst nxt ll
	  (*| splitListFirst (SOME nxt) NONE ll = L.splitList nxt ll*)
	  | splitListFirst _ _ ll = (L.empty, ll) (*raise EH.DeadBranch "undefined 'next' label"*)

	fun isinone l ll = if strictLab then L.isinone l ll else L.isin l ll
	fun isEmpty ll = L.isEmpty ll andalso strictLab
	(* Switching these two bool above leads to a compiler bug!?
	 * Apparently isEmpty needs to be applied to get the bug.
	 * Apparently strictLab needs to be false *)
	fun isEmptyL ll = L.isEmpty ll orelse not strictLab
	fun isin l ll = L.isin l ll

	(* This is for debugging only *)
	fun printFirst NONE     = "-"
	  | printFirst (SOME l) = L.printLab l

	fun sl_sl_part x ll =
	    if isEmpty ll
	    then []
	    else sl_part x ll

	and sl_part (A.PartExp   e) ll = flattenExp       [sl_exp       e ll]
	  | sl_part (A.PartDec   d) ll = flattenDec       [sl_dec       d ll]
	  | sl_part (A.PartType  t) ll = flattenType      [sl_type      t ll]
	  | sl_part (A.PartSeq   s) ll = flattenTypeRow   [sl_typeRow   s ll]
	  | sl_part (A.PartPat   p) ll = flattenPat       [sl_pat       p ll]
	  | sl_part (A.PartIdTy  i) ll = flattenIdentTy   [sl_identty   i ll]
	  | sl_part (A.PartTyCon t) ll = flattenLongTyCon [sl_longtycon t ll]
	  | sl_part (A.PartSpec  s) ll = flattenSpecOne   [sl_specone   s ll]
	  | sl_part (A.PartSige  s) ll = flattenSigExp    [sl_sigexp    s ll]
	  | sl_part (A.PartStre  s) ll = flattenStrExp    [sl_strexp    s ll]
	  (*| sl_part (A.PartFund  f) ll = flattenFunDec    [sl_fundec    f ll]*)
	  | sl_part (A.PartSigd  s) ll = flattenSigDec    [sl_sigdec    s ll]
	  | sl_part (A.PartStrd  s) ll = flattenStrDecOne [sl_strdecone s ll]
	  | sl_part (A.PartLgid  i) ll = flattenLongId    [sl_longid    i ll]
	  | sl_part (A.PartSigid i) ll = flattenSigId     [sl_sigid     i ll]
	  | sl_part (A.PartLgsid i) ll = flattenLongStrId [sl_longstrid i ll]
	  | sl_part (A.PartTes   t) ll = flattenSmlTes    [sl_smltes    t ll]
	  | sl_part (A.PartClass c) ll = flattenClass     [sl_class     c ll]

	and sl_sl_partlist x ll =
	    if isEmpty ll
	    then []
	    else sl_partlist x ll

	and sl_partlist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_partlist (x :: xs) ll =
	    let val nxt = A.getPartNext x
		val (lll, llr) = splitList nxt ll
		val p = sl_sl_part x lll
		val ps = sl_sl_partlist xs llr
	    in p @ ps
	    end

	and sl_sl_scon x ll =
	    if isEmpty ll
	    then A.SconDots
	    else sl_scon x ll

	and sl_scon (A.SconInt (st, v, r, l, n)) ll =
	    if isinone l ll
	    then A.SconInt (st, v, r, l, n)
	    else if strictLab
	    then raise EH.DeadBranch (msgOne l ll)
	    else A.SconDots
	  | sl_scon (A.SconWord (st, v, r, l, n)) ll =
	    if isinone l ll
	    then A.SconWord (st, v, r, l, n)
	    else if strictLab
	    then raise EH.DeadBranch (msgOne l ll)
	    else A.SconDots
	  | sl_scon (A.SconReal (st, v, r, l, n)) ll =
	    if isinone l ll
	    then A.SconReal (st, v, r, l, n)
	    else if strictLab
	    then raise EH.DeadBranch (msgOne l ll)
	    else A.SconDots
	  | sl_scon (A.SconString (st, v, r, l, n)) ll =
	    if isinone l ll
	    then A.SconString (st, v, r, l, n)
	    else if strictLab
	    then raise EH.DeadBranch (msgOne l ll)
	    else A.SconDots
	  | sl_scon (A.SconChar (st, v, r, l, n)) ll =
	    if isinone l ll
	    then A.SconChar (st, v, r, l, n)
	    else if strictLab
	    then raise EH.DeadBranch (msgOne l ll)
	    else A.SconDots
	  | sl_scon A.SconDots ll =
	    if isEmptyL ll
	    then A.SconDots
	    else raise EH.DeadBranch (msgEmpty ll)

	and sl_sl_pcon x ll =
	    if isEmpty ll
	    then A.PconDots
	    else sl_pcon x ll

	and sl_pcon (A.PconBool (st, v, r, l, n)) ll =
	    if isinone l ll
	    then A.PconBool (st, v, r, l, n)
	    else if strictLab
	    then raise EH.DeadBranch (msgOne l ll)
	    else A.PconDots
	  | sl_pcon (A.PconNil (st, v, r, l, n)) ll =
	    if isinone l ll
	    then A.PconNil (st, v, r, l, n)
	    else if strictLab
	    then raise EH.DeadBranch (msgOne l ll)
	    else A.PconDots
	  | sl_pcon (A.PconRef (st, v, r, l, n)) ll =
	    if isinone l ll
	    then A.PconRef (st, v, r, l, n)
	    else if strictLab
	    then raise EH.DeadBranch (msgOne l ll)
	    else A.PconDots
	  | sl_pcon A.PconDots ll =
	    if isEmptyL ll
	    then A.PconDots
	    else raise EH.DeadBranch (msgEmpty ll)

	and sl_sl_labid x ll =
	    if isEmpty ll
	    then A.LabIdDots []
	    else sl_labid x ll

	and sl_labid (A.LabId (id, rl, l, n)) ll =
	    if isin l ll
	    then A.LabId (sl_sl_ident id (L.delete l ll), rl, l, n)
	    else A.LabIdDots (flattenIdent [sl_ident id ll])
	  | sl_labid (A.LabIdDots pl) ll = A.LabIdDots (sl_partlist pl ll)

	and sl_sl_ident x ll =
	    if isEmpty ll
	    then A.IdentDots
	    else sl_ident x ll

	and sl_ident (A.Ident (s, v, r, l, n)) ll =
	    if isinone l ll
	    then A.Ident (s, v, r, l, n)
	    else if strictLab
	    then raise EH.DeadBranch (msgOne l ll)
	    else A.IdentDots
	  | sl_ident (A.IdentPcon pc) ll =
	    (case sl_pcon pc ll of
		 A.PconDots => A.IdentDots
	       | x            => A.IdentPcon x)
	  | sl_ident A.IdentDots ll =
	    if isEmptyL ll
	    then A.IdentDots
	    else raise EH.DeadBranch  (msgEmpty ll)

	and sl_sl_labclass x ll =
	    if isEmpty ll
	    then A.LabClassDots []
	    else sl_labclass x ll

	and sl_labclass (A.LabClass (id, rl, l, n)) ll =
	    if isin l ll
	    then A.LabClass (sl_sl_class id (L.delete l ll), rl, l, n)
	    else A.LabClassDots (flattenClass [sl_class id ll])
	  | sl_labclass (A.LabClassDots pl) ll = A.LabClassDots (sl_partlist pl ll)

	and sl_sl_class x ll =
	    if isEmpty ll
	    then A.ClassDots
	    else sl_class x ll

	and sl_class (A.Class (s, v, r, l, n)) ll =
	    if isinone l ll
	    then A.Class (s, v, r, l, n)
	    else if strictLab
	    then raise EH.DeadBranch (msgOne l ll)
	    else A.ClassDots
	  | sl_class A.ClassDots ll =
	    if isEmptyL ll
	    then A.ClassDots
	    else raise EH.DeadBranch  (msgEmpty ll)

	and sl_sl_strid x ll =
	    if isEmpty ll
	    then A.StrIdDots
	    else sl_strid x ll

	and sl_strid (A.StrId (s, v, r, l, n)) ll =
	    if isinone l ll
	    then A.StrId (s, v, r, l, n)
	    else if strictLab
	    then raise EH.DeadBranch (msgOne l ll)
	    else A.StrIdDots
	  | sl_strid A.StrIdDots ll =
	    if isEmptyL ll
	    then A.StrIdDots
	    else raise EH.DeadBranch (msgEmpty ll)

	and sl_sl_sigid x ll =
	    if isEmpty ll
	    then A.SigIdDots
	    else sl_sigid x ll

	and sl_sigid (A.SigId (s, v, r, l, n)) ll =
	    if isinone l ll
	    then A.SigId (s, v, r, l, n)
	    else if strictLab
	    then raise EH.DeadBranch (msgOne l ll)
	    else A.SigIdDots
	  | sl_sigid A.SigIdDots ll =
	    if isEmptyL ll
	    then A.SigIdDots
	    else raise EH.DeadBranch (msgEmpty ll)

	and sl_sl_funid x ll =
	    if isEmpty ll
	    then A.FunIdDots
	    else sl_funid x ll

	and sl_funid (A.FunId (s, v, r, l, n)) ll =
	    if isinone l ll
	    then A.FunId (s, v, r, l, n)
	    else if strictLab
	    then raise EH.DeadBranch (msgOne l ll)
	    else A.FunIdDots
	  | sl_funid A.FunIdDots ll =
	    if isEmptyL ll
	    then A.FunIdDots
	    else raise EH.DeadBranch (msgEmpty ll)

	and sl_sl_tycon x ll =
	    if isEmpty ll
	    then A.TyConDots
	    else sl_tycon x ll

	and sl_tycon (A.TyCon (st, v, r, l, n)) ll =
	    if isinone l ll
	    then A.TyCon (st, v, r, l, n)
	    else if strictLab
	    then raise EH.DeadBranch (msgOne l ll)
	    else A.TyConDots
	  | sl_tycon A.TyConDots ll =
	    if isEmptyL ll
	    then A.TyConDots
	    else raise EH.DeadBranch (msgEmpty ll)

	and sl_sl_longid x ll =
	    if isEmpty ll
	    then A.LongIdDots []
	    else sl_longid x ll

	and sl_longid (A.LongIdQual (sid, lid, r, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getStrIdNext sid
		     val (lll, llr) = splitList nxt (L.delete l ll)
		     val slsid = sl_sl_strid sid lll
		     val sllid = sl_sl_longid lid llr
		 in A.LongIdQual (slsid, sllid, r, l, n)
		 end
	    else let val nxt = A.getStrIdNext sid
		     val (lll, llr) = splitList nxt ll
		     val slsid = sl_sl_strid sid lll
		     val sllid = sl_sl_longid lid llr
		 in case (slsid, sllid) of
			(A.StrIdDots, _)     => A.LongIdDots (flattenLongId [sllid])
		      | (_, A.LongIdDots pl) => A.LongIdDots ((flattenStrId [slsid]) @ pl) (* NEW rule *)
		      | _                      => A.LongIdQual (slsid, sllid, r, l, n)
		 end (* if both of them are in the slice then the dot is as well *)
	  | sl_longid (A.LongIdId id) ll =
	    (case sl_ident id ll of
		 A.IdentDots => A.LongIdDots []
	       | x             => A.LongIdId x)
	  | sl_longid (A.LongIdDots pl) ll = A.LongIdDots (sl_partlist pl ll)

	and sl_sl_longtycon x ll =
	    if isEmpty ll
	    then A.LongTyConDots []
	    else sl_longtycon x ll

	and sl_longtycon (A.LongTyConQual (sid, ltc, r, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getStrIdNext sid
		     val (lll, llr) = splitList nxt (L.delete l ll)
		     val slsid = sl_sl_strid sid lll
		     val slltc = sl_sl_longtycon ltc llr
		 in A.LongTyConQual (slsid, slltc, r, l, n)
		 end
	    else let val nxt = A.getStrIdNext sid
		     val (lll, llr) = splitList nxt ll
		     val slsid = sl_sl_strid sid lll
		     val slltc = sl_sl_longtycon ltc llr
		 in case slsid of
			A.StrIdDots => A.LongTyConDots (flattenLongTyCon [slltc])
		      | _             => A.LongTyConQual (slsid, slltc, r, l, n)
		 end
	  | sl_longtycon (A.LongTyConId tc) ll =
	    (case (sl_tycon tc ll) of
		 A.TyConDots => A.LongTyConDots []
	       | x             => A.LongTyConId x)
	  | sl_longtycon (A.LongTyConDots pl) ll = A.LongTyConDots (sl_partlist pl ll)

	and sl_sl_tylab x ll =
	    if isEmpty ll
	    then A.TyLabDots
	    else sl_tylab x ll

	and sl_tylab (A.TyLab (s, r, l, n)) ll =
	    if isinone l ll
	    then A.TyLab (s, r, l, n)
	    else if strictLab
	    then raise EH.DeadBranch (msgOne l ll)
	    else A.TyLabDots
	  | sl_tylab A.TyLabDots ll =
	    if isEmptyL ll
	    then A.TyLabDots
	    else raise EH.DeadBranch (msgEmpty ll)

	and sl_sl_typevar x ll =
	    if isEmpty ll
	    then A.TypeVarDots
	    else sl_typevar x ll

	and sl_typevar (A.TypeVar (s, v, r, l, n)) ll =
	    if isinone l ll
	    then A.TypeVar (s, v, r, l, n)
	    else if strictLab
	    then raise EH.DeadBranch (msgOne l ll)
	    else A.TypeVarDots

	  | sl_typevar (A.EqualityTypeVar (s, v, r, l, n)) ll =
	    if isinone l ll
	    then A.TypeVar (s, v, r, l, n)
	    else if strictLab
	    then raise EH.DeadBranch (msgOne l ll)
	    else A.TypeVarDots

	  | sl_typevar A.TypeVarDots ll =
	    if isEmptyL ll
	    then A.TypeVarDots
	    else raise EH.DeadBranch (msgEmpty ll)

	and sl_sl_typevarlist xs ll = sl_typevarlist xs ll

	(* do something for the regions? - No *)
	and sl_typevarlist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_typevarlist (x :: xs) ll =
	    let val nxt = A.getTypeVarNext x
		val (lll, llr) = splitList nxt ll
		val y = sl_sl_typevar x lll
		val ys = sl_typevarlist xs llr
	    in y :: ys
	    end

	and sl_sl_typevardotlist x ll =
	    if isEmpty ll
	    then []
	    else sl_typevarlist x ll

	and sl_typevardotlist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_typevardotlist (x :: xs) ll =
	    let val nxt = A.getTypeVarNext x
		val (lll, llr) = splitList nxt ll
		val y = sl_sl_typevar x lll
		val ys = sl_sl_typevardotlist xs llr
	    in y :: ys
	    end

	and sl_sl_labtyvarlist xs ll = sl_labtyvarlist xs ll

	and sl_labtyvarlist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_labtyvarlist (x :: xs) ll =
	    let val nxt = A.getLabTypeVarNext x
		val (lll, llr) = splitList nxt ll
		val y = sl_sl_labtyvar x lll
		val ys = sl_labtyvarlist xs llr
	    in y :: ys
	    end

	and sl_sl_labtyvardotlist x ll =
	    if isEmpty ll
	    then []
	    else sl_labtyvardotlist x ll

	and sl_labtyvardotlist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_labtyvardotlist (x :: xs) ll =
	    let val nxt = A.getLabTypeVarNext x
		val (lll, llr) = splitList nxt ll
		val y = case sl_sl_labtyvar x lll of
			    A.LabTypeVar (tv, _, _, _) => [tv]
			  | A.LabTypeVarDots tvl       => tvl
		val ys = sl_sl_labtyvardotlist xs llr
	    in y @ ys
	    end

	and sl_sl_labtyvar x ll =
	    if isEmpty ll
	    then A.LabTypeVarDots []
	    else sl_labtyvar x ll

	and sl_labtyvar (A.LabTypeVar (tv, rl, l, n)) ll =
	    if isin l ll
	    then A.LabTypeVar (sl_sl_typevar tv (L.delete l ll), rl, l, n)
	    else A.LabTypeVarDots (sl_typevardotlist [tv] ll)
	  | sl_labtyvar (A.LabTypeVarDots tvl) ll =
	    A.LabTypeVarDots (sl_typevarlist tvl ll)

	and sl_sl_tyvarseq x ll =
	    if isEmpty ll
	    then A.TypeVarSeqDots []
	    else sl_tyvarseq x ll

	and sl_tyvarseq (A.TypeVarSeqOne (tv, r, l, n)) ll =
	    if isin l ll
	    then A.TypeVarSeqOne (sl_sl_typevar tv (L.delete l ll), r, l, n)
	    else let val sltv = sl_typevar tv ll
		 in case sltv of
			A.TypeVarDots => A.TypeVarSeqDots ([])
		      | x               => A.TypeVarSeqDots ([x])
		 end
	  | sl_tyvarseq (A.TypeVarSeqEm (r, l, n)) ll =
	    if isinone l ll
	    then A.TypeVarSeqEm (r, l, n)
	    else if strictLab
	    then raise EH.DeadBranch (msgOne l ll)
	    else A.TypeVarSeqDots []
	  | sl_tyvarseq (A.TypeVarSeqSeq (tvl, rl, l, n)) ll =
	    if isin l ll
	    then A.TypeVarSeqSeq (sl_sl_labtyvarlist tvl (L.delete l ll), rl, l, n)
	    else A.TypeVarSeqDots (sl_labtyvardotlist tvl ll)
	  | sl_tyvarseq (A.TypeVarSeqDots tvl) ll =
	    A.TypeVarSeqDots (sl_typevarlist tvl ll)
	(* Can we have an error inside the parentheses without having the parentheses in the error?
	         Do we want to use the same function sl_typevarlist in the else branch?
                    - In the else branch we don't have the row structure in the slice (l') so maybe
                      we don't care about the position of each variable.
                      This remark stands for all sort of row I guess *)
	(* I took care of this case - do the same for the other rows - the flattening already take care of that *)

	and sl_sl_labtypelist xs ll = sl_labtypelist xs ll

	and sl_labtypelist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_labtypelist (x :: xs) ll =
	    let val nxt = A.getLabTypeNext x
		val (lll, llr) = splitList nxt ll
		val slty = sl_sl_labtype x lll
		val sltl = sl_labtypelist xs llr
	    in slty :: sltl
	    end

	and sl_sl_labtype x ll =
	    if isEmpty ll
	    then A.LabTypeDots []
	    else sl_labtype x ll

	and sl_labtype (A.LabType (t, rl, l, n)) ll =
	    if isin l ll
	    then A.LabType (sl_sl_type t (L.delete l ll), rl, l, n)
	    else A.LabTypeDots (flattenType [sl_type t ll])
	  | sl_labtype (A.LabTypeDots pl) ll = A.LabTypeDots (sl_partlist pl ll)

	and sl_sl_tyfield x ll =
	    if isEmpty ll
	    then A.TyFieldDots []
	    else sl_tyfield x ll

	and sl_tyfield (A.TyField (tl, t, r, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getTyLabNext tl
		     val (lll, llr) = splitList nxt (L.delete l ll)
		     val sltl = sl_sl_tylab tl lll
		     val slt  = sl_sl_labtype t llr
		 in A.TyField (sltl, slt, r, l, n)
	  	 end
	    else let val nxt = A.getTyLabNext tl
		     val (lll, llr) = splitList nxt ll
		     val sltl = sl_sl_tylab tl lll
		     val slt  = sl_sl_labtype t llr
		 in case sltl of
			A.TyLabDots => A.TyFieldDots (flattenLabType [slt])
		      | _             => A.TyField (sltl, slt, r, l, n)
	  	 end
	  | sl_tyfield (A.TyFieldDots pl) ll = A.TyFieldDots (sl_partlist pl ll)

	and sl_sl_tyfieldlist xs ll = sl_tyfieldlist xs ll

	and sl_tyfieldlist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_tyfieldlist (x :: xs) ll =
	    let val nxt = A.getTyFieldNext x
		val (lll, llr) = splitList nxt ll
		val slx  = sl_sl_tyfield x lll
		val slxs = sl_tyfieldlist xs llr
	    in slx :: slxs
	    end

	and sl_sl_type x ll =
	    if isEmpty ll
	    then A.TypeDots []
	    else sl_type x ll

	and sl_type (A.TypeOneVar tv) ll =
	    let val sltv = sl_typevar tv ll
	    in case sltv of
		   A.TypeVarDots => A.TypeDots []
		 | _               => A.TypeOneVar sltv
	    end
	  | sl_type (A.TypeArrow (ty1, ty2, r, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getLabTypeNext ty1
		     val (lll, llr) = splitList nxt (L.delete l ll)
		     val slty1 = sl_sl_labtype ty1 lll
		     val slty2 = sl_sl_labtype ty2 llr
		 in A.TypeArrow (slty1, slty2, r, l, n)
		 end
	    else let val nxt = A.getLabTypeNext ty1
		     val (lll, llr) = splitList nxt ll
		     val slty1 = sl_sl_labtype ty1 lll
		     val slty2 = sl_sl_labtype ty2 llr
		 in A.TypeDots (flattenLabType [slty1, slty2])
		 end
	  | sl_type (A.TypeTuple (tl, rl, l, n)) ll =
	    if isin l ll
	    then A.TypeTuple (sl_sl_labtypelist tl (L.delete l ll), rl, l, n)
	    else A.TypeDots (flattenLabType (sl_labtypelist tl ll))
	  | sl_type (A.TypeRecord (rtl, rl1, rl2, l, n)) ll =
	    if isin l ll
	    then A.TypeRecord (sl_sl_tyfieldlist rtl (L.delete l ll), rl1, rl2, l, n)
	    else let val slrtl = sl_tyfieldlist rtl ll
		 in case flattenTyField slrtl of
			[] => A.TypeDots [] (* NEW - related to strictLab *)
		      | [A.TyFieldDots x] => A.TypeDots x
		      | x                 => A.TypeSlRec (x, rl1, l, n)
		 end
	  | sl_type (A.TypeSlRec (rtl, rl, l, n)) ll =
	    if isin l ll
	    then  A.TypeSlRec (sl_sl_tyfieldlist rtl (L.delete l ll), rl, l, n)
	    else let val slrtl = sl_tyfieldlist rtl ll
		 in case flattenTyField slrtl of
			[] => A.TypeDots [] (* NEW - related to strictLab *)
		      | [A.TyFieldDots x] => A.TypeDots x
		      | x                 => A.TypeSlRec (x, rl, l, n)
		 end
	  | sl_type (A.TypeTyCon (ts, ltc, rl, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getLongTyConNext ltc
		     val (lll, llr) = splitList nxt (L.delete l ll)
		     val sltc = sl_sl_longtycon ltc lll
		     val slts = sl_sl_typeRow ts llr
		 in A.TypeTyCon (slts, sltc, rl, l, n)
		 end
	    else let val nxt = A.getLongTyConNext ltc
		     val (lll, llr) = splitList nxt ll
		     val sltc = sl_sl_longtycon ltc lll
		     val slts = sl_sl_typeRow ts llr
		 in A.TypeDots ((flattenLongTyCon [sltc]) @ (flattenTypeRow [slts]))
		 (* case sltc of
			      A.LongTyConDots pl => A.TypeDots (pl @ (flattenTypeRow [slts]))
			    | _                    => A.TypeTyCon (slts, sltc, rl, l, n)*)
		 end
	  | sl_type (A.TypeParen (ty, r1, r2, l, n)) ll =
	    if isin l ll
	    then A.TypeParen (sl_sl_labtype ty (L.delete l ll), r1, r2, l, n)
	    else A.TypeDots (flattenLabType [sl_labtype ty ll])
	  | sl_type (A.TypeDots pl) ll = A.TypeDots (sl_partlist pl ll)

	and sl_sl_typeRow x ll =
	    if isEmpty ll
	    then A.TypeRowDots []
	    else sl_typeRow x ll

	and sl_typeRow (A.TypeRowOne (ty, r, l, n)) ll =
	    if isin l ll
	    then A.TypeRowOne (sl_sl_type ty (L.delete l ll), r, l, n)
	    else A.TypeRowDots (flattenType [sl_type ty ll])
	  | sl_typeRow (A.TypeRowEm (r, l, n)) ll =
	    if isinone l ll
	    then A.TypeRowEm (r, l, n)
	    else if strictLab
	    then raise EH.DeadBranch (msgOne l ll)
	    else A.TypeRowDots []
	  | sl_typeRow (A.TypeRowSeq (tl, rl, l, n)) ll =
	    if isin l ll
	    then A.TypeRowSeq (sl_sl_labtypelist tl (L.delete l ll), rl, l, n)
	    else A.TypeRowDots (flattenLabType (sl_labtypelist tl ll))
	  | sl_typeRow (A.TypeRowDots pl) ll = A.TypeRowDots (sl_partlist pl ll)

	and sl_sl_conbind x ll =
	    if isEmpty ll
	    then A.ConBindDots []
	    else sl_conbind x ll

	and sl_conbind (A.ConBind (id, n)) ll =
	    (case sl_ident id ll of
		 A.IdentDots => A.ConBindDots []
	       | slid          => A.ConBind (slid, n))
	  | sl_conbind (A.ConBindOf (id, ty, r, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getLabIdNext id
		     val (lll, llr) = splitList nxt (L.delete l ll)
		     val slid = sl_sl_labid id lll
		     val slty = sl_sl_labtype ty llr
		 in  A.ConBindOf (slid, slty, r, l, n)
		 end
	    (* we always keep the "of" if "id" is in - why? - No! *)
	    else let val nxt = A.getLabIdNext id
		     val (lll, llr) = splitList nxt ll
		     val slid = sl_sl_labid id lll
		     val slty = sl_sl_labtype ty llr
		 in case (slid, slty) of (*A.ConBindDots ((flattenLabId [slid]) @ (flattenLabType [slty]))*)
			(A.LabIdDots [(A.PartLgid (A.LongIdId id))],
			 A.LabTypeDots []) => A.ConBindNoOf (id, n)
		      | _ => A.ConBindDots ((flattenLabId [slid]) @ (flattenLabType [slty]))
		 end
	  | sl_conbind (A.ConBindNoOf (id, n)) ll =
	    (case sl_ident id ll of
		 A.IdentDots => A.ConBindDots []
	       | slid          => A.ConBindNoOf (slid, n))
	  | sl_conbind (A.ConBindDots pl) ll = A.ConBindDots (sl_partlist pl ll)

	and sl_sl_conbindlist x ll =
	    if isEmpty ll
	    then []
	    else sl_conbindlist x ll

	and sl_conbindlist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_conbindlist (tc :: tcl) ll =
	    let val nxt = A.getConBindNext tc
		val (lll, llr) = splitList nxt ll
		val sltc = sl_sl_conbind tc lll
		val sltcl = sl_sl_conbindlist tcl llr
	    in sltc :: sltcl
	    end

	and sl_sl_conbindseq x ll =
	    if isEmpty ll
	    then A.ConBindSeqDots []
	    else sl_conbindseq x ll

	and sl_conbindseq (A.ConBindSeq tcl) ll =
	    let val sltcl = flattenConBind (sl_conbindlist tcl ll)
	    in case sltcl of
		   [] => A.ConBindSeqDots [] (* NEW - related to strictLab *)
		 | [A.ConBindDots x] => A.ConBindSeqDots x
		 | _ => A.ConBindSeq sltcl
	    end
	  | sl_conbindseq (A.ConBindSeqDots pl) ll = A.ConBindSeqDots (sl_partlist pl ll)

	and sl_sl_ldatname x ll =
	    if isEmpty ll
	    then A.LDatNameDots
	    else sl_ldatname x ll

	and sl_ldatname (A.LDatName (tvs, tn, rl, n)) ll =
	    let val nxt = A.getLongTyConNext tn (* a type name is always first (left) *)
		val (lll, llr) = splitList nxt ll
		val sltn  = sl_sl_longtycon tn lll
		val sltvs = sl_sl_tyvarseq tvs llr
	    in case (sltn, sltvs) of
		   (A.LongTyConDots [], A.TypeVarSeqDots []) => A.LDatNameDots
		 | _ => A.LDatName (sltvs, sltn, rl, n)
	    end
	  | sl_ldatname A.LDatNameDots ll =
	    if isEmptyL ll
	    then A.LDatNameDots
	    else raise EH.DeadBranch (msgEmpty ll)

	and sl_sl_datname x ll =
	    if isEmpty ll
	    then A.DatNameDots
	    else sl_datname x ll

	and sl_datname (A.DatName (tvs, tn, rl, n)) ll =
	    let val nxt = A.getTyConNext tn (* a type name is always first (left) *)
		val (lll, llr) = splitList nxt ll
		val sltn = sl_sl_tycon tn lll
		val sltvs = sl_sl_tyvarseq tvs llr
	    in case (sltn, sltvs) of
		   (A.TyConDots, A.TypeVarSeqDots []) => A.DatNameDots
		 | _ => A.DatName (sltvs, sltn, rl, n)
	    end
	  | sl_datname A.DatNameDots ll =
	    if isEmptyL ll
	    then A.DatNameDots
	    else raise EH.DeadBranch (msgEmpty ll)

	and sl_sl_datbind x ll =
	    if isEmpty ll
	    then A.DatBindDots []
	    else sl_datbind x ll

	and sl_datbind (A.DatBind (dn, tcs, r, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getDatNameNext dn
		     val (lll, llr) = splitList nxt (L.delete l ll)
		     val sldn = sl_sl_datname dn lll
		     val sltcs = sl_sl_conbindseq tcs llr
		 in A.DatBind (sldn, sltcs, r, l, n)
		 end
	    else let val nxt = A.getDatNameNext dn
		     val (lll, llr) = splitList nxt ll
		     val sldn = sl_sl_datname dn lll
		     val sltcs = sl_sl_conbindseq tcs llr
		 in case (sldn, sltcs) of
			(A.DatNameDots, A.ConBindSeqDots x) => A.DatBindDots x (* HACK: before we were allowing [] to be anything! *)
		      | _                                       => A.DatBind (sldn, sltcs, r, l, n)
		 end
	  | sl_datbind (A.DatBindDots pl) ll = A.DatBindDots (sl_partlist pl ll)
	(* Do we really want to have the datatype if only the type name is in the slice?
                   - see test23.sml
                   -
                 sltvs <> A.TypeVarSeqDots [] implies isin l ll *)
	(* It might be better to have only: "A.ConBindSeqDots x" *)

	and sl_sl_datbindlist x ll =
	    if isEmpty ll
	    then []
	    else sl_datbindlist x ll

	and sl_datbindlist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_datbindlist (db :: dbl) ll =
	    let val nxt = A.getDatBindNext db
		val (lll, llr) = splitList nxt ll
		val sldb  = sl_sl_datbind db lll
		val sldbl = sl_sl_datbindlist dbl llr
	    in sldb :: sldbl
	    end

	and sl_sl_datbindseq x ll =
	    if isEmpty ll
	    then A.DatBindSeqDots []
	    else sl_datbindseq x ll

	(* what about the regions? *)
	and sl_datbindseq (A.DatBindSeq (dbl, rl, n)) ll =
	    let val sldbl = flattenDatBind (sl_datbindlist dbl ll)
	    in case sldbl of
		   []                => A.DatBindSeqDots [] (* NEW - related to strictLab *)
		 | [A.DatBindDots x] => A.DatBindSeqDots x
		 | _                 => A.DatBindSeq (sldbl, rl, n)
	    end
	  | sl_datbindseq (A.DatBindSeqDots pl) ll = A.DatBindSeqDots (sl_partlist pl ll)

	and sl_sl_valbindcore x ll =
	    if isEmpty ll
	    then A.ValBindCoreDots []
	    else sl_valbindcore x ll

	and sl_valbindcore (A.ValBindCore (p, e, r, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getLabPatNext p
		     val (lll, llr) = splitList nxt (L.delete l ll)
		     val slp = sl_sl_labpat p lll
		     val sle = sl_sl_labexp e llr
		 in A.ValBindCore (slp, sle, r, l, n)
		 end
	    else let val nxt = A.getLabPatNext p
		     val (lll, llr) = splitList nxt ll
		     val slp = sl_sl_labpat p lll
		     val sle = sl_sl_labexp e llr
		 in case slp of
			A.LabPatDots [] => A.ValBindCoreDots (flattenLabExp [sle])
		      | _ => A.ValBindCore (slp, sle, r, l, n)
		 (*A.ValBindCoreDots (flattenLabPat [slp] @ flattenLabExp [sle])*)
		 (*case (slp, sle) of
			     (A.LabPatDots [], _) => A.ValBindCoreDots (flattenLabExp [sle])
			   | (_, A.LabExpDots []) => A.ValBindCoreDots (flattenLabPat [slp])
			   | _ => A.ValBindCore (slp, sle, r, l, n)*)
		 (* Here are some possibilities for some parts of the pattern to be in the slice
		  * and nothing from the expresssion:
		  *  - syntactic error in the pattern
		  *  - an id that constrains the status or the monomorphism of its occurrences *)
		 end
	  | sl_valbindcore (A.ValBindCoreDots pl) ll = A.ValBindCoreDots (sl_partlist pl ll)

	and sl_sl_valbindcorelist x ll =
	    if isEmpty ll
	    then []
	    else sl_valbindcorelist x ll

	and sl_valbindcorelist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_valbindcorelist (vb :: vbl) ll =
	    let val nxt = A.getValBindCoreNext vb
		val (lll, llr) = splitList nxt ll
		val slvb = sl_sl_valbindcore vb lll
		val slvbl = sl_sl_valbindcorelist vbl llr
	    in slvb :: slvbl
	    end

	and sl_sl_valbindseq x ll =
	    if isEmpty ll
	    then A.ValBindSeqDots []
	    else sl_valbindseq x ll

	and sl_valbindseq (A.ValBindSeq (vbl, rl, n)) ll =
	    let val slvbl = flattenValBindCore (sl_valbindcorelist vbl ll)
	    in case slvbl of
		   [] => A.ValBindSeqDots [] (* NEW - related to strictLab *)
		 | [A.ValBindCoreDots x] => A.ValBindSeqDots x
		 | _                       => A.ValBindSeq (slvbl, rl, n)
	    end
	  | sl_valbindseq (A.ValBindSeqDots pl) ll = A.ValBindSeqDots (sl_partlist pl ll)

	and sl_sl_valbind x ll =
	    if isEmpty ll
	    then A.ValBindDots []
	    else sl_valbind x ll

	and sl_valbind (A.ValBindRec (vbs, r, l, n)) ll =
	    if isin l ll
	    then A.ValBindRec (sl_valbindseq vbs (L.delete l ll), r, l, n)
	    else (case sl_valbindseq vbs ll of
		      A.ValBindSeqDots x => A.ValBindDots x
		    | x => A.ValBindRec (x, r, l, n))
	  | sl_valbind (A.ValBind vbs) ll =
	    let val slvbs = sl_valbindseq vbs ll
	    in case slvbs of
		   A.ValBindSeqDots x => A.ValBindDots x
		 | _                    => A.ValBind slvbs
	    end
	  | sl_valbind (A.ValBindDots pl) ll = A.ValBindDots (sl_partlist pl ll)

	and sl_sl_labatpat x ll =
	    if isEmpty ll
	    then A.LabAtPatDots []
	    else sl_labatpat x ll

	and sl_labatpat (A.LabAtPat (ap, r, l, n)) ll =
	    if isin l ll
	    then A.LabAtPat (sl_sl_atpat ap (L.delete l ll), r, l, n)
	    else A.LabAtPatDots (flattenAtPat [sl_atpat ap ll])
	  | sl_labatpat (A.LabAtPatDots pl) ll = A.LabAtPatDots (sl_partlist pl ll)

	(*
	       and sl_sl_fmatchid x ll =
		   if isEmpty ll
		   then A.FMatchId A.IdentDots
		   else sl_fmatchid x ll
	*)

	(*
	       and sl_fmatchid fm ll =
		   (case sl_fmatch fm ll of
			A.FMatchDots                              => A.FMatchId A.IdentDots
		      | A.FMatchSlApp (x, A.LabAtPatDots [], _) => x
		      | slfm                                        => slfm)
	*)

	and sl_fmatchid fm ll =
	    (case sl_fmatch fm ll of
		 (*A.FMatchDots                                                   => A.FMatchId A.IdentDots*)
		 (*|*) A.FMatchSlApp (A.FMatchNoApp (x, n), A.LabAtPatDots [], _) => A.FMatchNoApp (x, n)
		     | A.FMatchSlApp (A.FMatchNoApp (x, _), lap, n)                 => A.FMatchSlApp (x, lap, n)
		     | A.FMatchSlApp (x, A.LabAtPatDots [], n)                      => A.FMatchNoApp (x, n)
		     | A.FMatchNoApp (A.FMatchNoApp (x, n), _)                      => A.FMatchNoApp (x, n)
		     | slfm                                                             => slfm)

	and sl_sl_fmatch x ll =
	    if isEmpty ll
	    then A.FMatchDots
	    else sl_fmatch x ll

	and sl_fmatch (A.FMatchId (id, fix, r)) ll =
	    (case sl_sl_ident id ll of
		 (*A.IdentDots => A.FMatchDots (* NEW - related to strictLab *)
		     |*) x => A.FMatchId ((**)x, fix, r))
	  | sl_fmatch (A.FMatchApp (fm, lap, rl, ra, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getFMatchNext fm
		     val (lll, llr) = splitList nxt (L.delete l ll)
		     val slfm = (*sl_*)sl_fmatchid fm lll
		     val slp  = sl_sl_labatpat lap llr
		 in A.FMatchApp (slfm, slp, rl, ra, l, n)
		 end
	    else let val nxt = A.getFMatchNext fm
		     val (lll, llr) = splitList nxt ll
		     val slfm = (*sl_*)sl_fmatchid fm lll
		     val slp  = sl_sl_labatpat lap llr
		 in A.FMatchSlApp (slfm, slp, n)
		 end
	  | sl_fmatch (A.FMatchSlApp (fm, lap, n)) ll =
	    let val nxt = A.getFMatchNext fm
		val (lll, llr) = splitList nxt ll
		val slfm = (*sl_*)sl_fmatchid fm lll
		val slp  = sl_sl_labatpat lap llr
	    in A.FMatchSlApp (slfm, slp, n)
	    end
	  | sl_fmatch (A.FMatchNoApp (fm, n)) ll =
	    A.FMatchNoApp (sl_fmatchid fm ll, n)
	  | sl_fmatch A.FMatchDots ll =
	    if isEmptyL ll
	    then A.FMatchDots
	    else raise EH.DeadBranch (msgEmpty ll)

	and sl_sl_labfmatch x ll =
	    if isEmpty ll
	    then A.LabFMatchDots
	    else sl_labfmatch x ll

	and sl_labfmatch (A.LabFMatch (fm, rl, l, n)) ll =
	    if isin l ll
	    then A.LabFMatch (sl_sl_fmatch fm (L.delete l ll), rl, l, n)
	    else (case sl_fmatchid fm ll of
		      A.FMatchDots                              => A.LabFMatchDots
		    (*| A.FMatchSlApp (A.FMatchDots, A.LabAtPatDots [], _) => A.LabFMatchDots (* NEW - related to strictLab *)*)
		    | A.FMatchSlApp (x, A.LabAtPatDots [], _) => A.LabFMatchSl (x, n)
		    (*| A.FMatchNoApp (A.FMatchDots, _)         => A.LabFMatchDots (* NEW - related to strictLab *)*)
		    (*| A.FMatchNoApp (A.FMatchId (A.IdentDots, _), _) => A.LabFMatchDots (* NEW - related to strictLab *)*)
		    | A.FMatchNoApp (x, _)                      => A.LabFMatchSl (x, n)
		    | x                                           => A.LabFMatchSl (x, n))
	  | sl_labfmatch (A.LabFMatchSl (fm, n)) ll =
	    (case sl_fmatchid fm ll of
		 A.FMatchDots                              => A.LabFMatchDots
 	       (*| A.FMatchSlApp (A.FMatchDots, A.LabAtPatDots [], _) => A.LabFMatchDots (* NEW - related to strictLab *)*)
	       | A.FMatchSlApp (x, A.LabAtPatDots [], _) => A.LabFMatchSl (x, n)
	       (*| A.FMatchNoApp (A.FMatchDots, _)         => A.LabFMatchDots (* NEW - related to strictLab *)*)
	       (*| A.FMatchNoApp (A.FMatchId (A.IdentDots, _), _) => A.LabFMatchDots (* NEW - related to strictLab *)*)
	       | A.FMatchNoApp (x, _)                      => A.LabFMatchSl (x, n)
	       | x                                           => A.LabFMatchSl (x, n))
	  | sl_labfmatch A.LabFMatchDots ll =
	    if isEmptyL ll
	    then A.LabFMatchDots
	    else raise EH.DeadBranch (msgEmpty ll)

	and sl_sl_fmatchty x ll =
	    if isEmpty ll
	    then A.FMatchTDots
	    else sl_fmatchty x ll

	and sl_fmatchty (A.FMatchT fm) ll =
	    (case sl_labfmatch fm ll of
		 A.LabFMatchDots => A.FMatchTDots
	       | x                 => A.FMatchT x)
	  | sl_fmatchty (A.FMatchTTy (fm, ty, r, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getLabFMatchNext fm
		     val (ll1, ll2) = splitList nxt (L.delete l ll)
		     val slfm = sl_sl_labfmatch fm ll1
		     val slty = sl_sl_labtype ty ll2
		 in A.FMatchTTy (slfm, slty, r, l, n)
		 end
	    else let val nxt = A.getLabFMatchNext fm
		     val (ll1, ll2) = splitList nxt ll
		     val slfm = sl_sl_labfmatch fm ll1
		     val slty = sl_sl_labtype ty ll2
		 in case (slfm, slty) of
			(A.LabFMatchDots, A.LabTypeDots []) => A.FMatchTDots
		      (*| (_, A.LabTypeDots []) => A.FMatchT slfm*)
		      | _ => A.FMatchTTy (slfm, slty, r, l, n)
		 end
	  | sl_fmatchty A.FMatchTDots ll =
	    if isEmptyL ll
	    then A.FMatchTDots
	    else raise EH.DeadBranch (msgEmpty ll)

	and sl_sl_fvalbindcore x ll =
	    if isEmpty ll
	    then A.FVBCoreDots []
	    else sl_fvalbindcore x ll

	and sl_fvalbindcore (A.FValBindCore (fm, le, r, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getFMatchTyNext fm
		     val (lll, llr) = splitList nxt (L.delete l ll)
		     val slfm = sl_sl_fmatchty fm lll
		     val slle = sl_sl_labexp le llr
		 in A.FValBindCore (slfm, slle, r, l, n)
		 end
	    else let val nxt = A.getFMatchTyNext fm
		     val (lll, llr) = splitList nxt ll
		     val slfm = sl_sl_fmatchty fm lll
		     val slle = sl_sl_labexp le llr
		 in case slfm of
			A.FMatchTDots => A.FVBCoreDots (flattenLabExp [slle])
		      | _               => A.FValBindCore (slfm, slle, r, l, n)
		 end
	  (*| sl_fvalbindcore (A.FVBCoreTy (lfm, ty, le, r1, r2, l, n)) ll =
		  if isin l ll
		  then
		      let
			  val nxt1 = A.getLabFMatchNext lfm
			  val nxt2 = A.getLabTypeNext ty
			  val (ll1, ll') = splitList nxt1 (L.delete l ll)
			  val (ll2, ll3) = splitList nxt2 ll'
			  val slfm = sl_sl_labfmatch lfm ll1
			  val slty = sl_sl_labtype ty ll2
			  val slle = sl_sl_labexp le ll3
		      in A.FVBCoreTy (slfm, slty, slle, r1, r2, l, n)
		      end
		  else
		      let
			  val nxt1 = A.getLabFMatchNext lfm
			  val nxt2 = A.getLabTypeNext ty
			  val (ll1, ll') = splitList nxt1 ll
			  val (ll2, ll3) = splitList nxt2 ll'
			  val slfm = sl_sl_labfmatch lfm ll1
			  val slty = sl_sl_labtype ty ll2
			  val slle = sl_sl_labexp le ll3
		      in case (slfm, slty) of
			     (A.LabFMatchDots, A.LabTypeDots []) => A.FVBCoreDots (flattenLabExp [slle])
			   | _ => A.FVBCoreTy (slfm, slty, slle, r1, r2, l, n)
		      end*)
	  | sl_fvalbindcore (A.FVBCoreDots pl) ll = A.FVBCoreDots (sl_partlist pl ll)

	and sl_sl_fvalbindcorelist x ll =
	    if isEmpty ll
	    then []
	    else sl_fvalbindcorelist x ll

	and sl_fvalbindcorelist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_fvalbindcorelist (x :: xs) ll =
	    let val nxt = A.getFVBCoreNext x
		val (lll, llr) = splitList nxt ll
		val slx = sl_sl_fvalbindcore x lll
		val slxs = sl_sl_fvalbindcorelist xs llr
	    in slx :: slxs
	    end

	and sl_sl_fvalbindone x ll =
	    if isEmpty ll
	    then A.FVBOneDots []
	    else sl_fvalbindone x ll

	and sl_fvalbindone (A.FValBindOne (slvbcl, rl, l, n)) ll =
	    if isin l ll
	    then A.FValBindOne (flattenFValBindCore (sl_sl_fvalbindcorelist slvbcl (L.delete l ll)), rl, l, n)
	    else (case flattenFValBindCore (sl_fvalbindcorelist slvbcl ll) of
		      [] => A.FVBOneDots [] (* NEW - related to strictLab *)
		    | [A.FVBCoreDots x] => A.FVBOneDots x
		    | x                   => A.FValBindOne (x, rl, l, n))
	  | sl_fvalbindone (A.FVBOneDots pl) ll = A.FVBOneDots (sl_partlist pl ll)

	and sl_sl_fvalbindonelist x ll =
	    if isEmpty ll
	    then []
	    else sl_fvalbindonelist x ll

	and sl_fvalbindonelist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_fvalbindonelist (x :: xs) ll =
	    let
		val nxt = A.getFVBOneNext x
		val (lll, llr) = splitList nxt ll
		val slx = sl_sl_fvalbindone x lll
		val slxs = sl_sl_fvalbindonelist xs llr
	    in slx :: slxs
	    end

	and sl_sl_fvalbind x ll =
	    if isEmpty ll
	    then A.FValBindDots []
	    else sl_fvalbind x ll

	and sl_fvalbind (A.FValBind (slvbol, rl, n)) ll =
	    (case flattenFValBindOne (sl_fvalbindonelist slvbol ll) of
		 [] => A.FValBindDots []  (* NEW - related to strictLab *)
	       | [A.FVBOneDots x] => A.FValBindDots x
	       | x                  => A.FValBind (x, rl, n))
	  | sl_fvalbind (A.FValBindDots pl) ll = A.FValBindDots (sl_partlist pl ll)

	and sl_sl_typbind x ll =
	    if isEmpty ll
	    then A.TypBindDots []
	    else sl_typbind x ll

	and sl_typbind (A.TypBind (dn, ty, r, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getDatNameNext dn
		     val (lll, llr) = splitList nxt (L.delete l ll)
		     val sldn = sl_sl_datname dn lll
		     val slty = sl_sl_labtype ty llr
		 in A.TypBind (sldn, slty, r, l, n)
		 end
	    else let val nxt = A.getDatNameNext dn
		     val (lll, llr) = splitList nxt ll
		     val sldn = sl_sl_datname dn lll
		     val slty = sl_sl_labtype ty llr
		 in case (sldn, slty) of
			(A.DatNameDots, A.LabTypeDots x) => A.TypBindDots x
		      | _  => A.TypBind (sldn, slty, r, l, n)
		 end
	  | sl_typbind (A.TypBindDots pl) ll = A.TypBindDots (sl_partlist pl ll)

	and sl_sl_typbindlist x ll =
	    if isEmpty ll
	    then []
	    else sl_typbindlist x ll

	and sl_typbindlist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_typbindlist (x :: xs) ll =
	    let val nxt = A.getTypBindNext x
		val (lll, llr) = splitList nxt ll
		val slx  = sl_sl_typbind x lll
		val slxs = sl_sl_typbindlist xs llr
	    in slx :: slxs
	    end

	and sl_sl_typbindseq x ll =
	    if isEmpty ll
	    then A.TypBindSeqDots []
	    else sl_typbindseq x ll

	and sl_typbindseq (A.TypBindSeq (tbl, rl, n)) ll =
	    let val sltbl = flattenTypBind (sl_typbindlist tbl ll)
	    in case sltbl of
		   [] => A.TypBindSeqDots [] (* NEW - related to strictLab *)
		 | [A.TypBindDots x] => A.TypBindSeqDots x
		 | _                   => A.TypBindSeq (sltbl, rl, n)
	    end
	  | sl_typbindseq (A.TypBindSeqDots pl) ll = A.TypBindSeqDots (sl_partlist pl ll)

	and sl_sl_exbind x ll =
	    if isEmpty ll
	    then A.ExBindDots []
	    else sl_exbind x ll

	and sl_exbind (A.ExBind (id, l, n)) ll =
	    if isin l ll
	    then A.ExBind (sl_sl_ident id (L.delete l ll), l, n)
	    else (case sl_ident id ll of
		      A.IdentDots => A.ExBindDots []
		    | x             => A.ExBind (x, l, n))
	  | sl_exbind (A.ExBindOf (id, t, r, l, n)) ll =
	    if isin l ll
	    then
		let
		    val nxt = A.getLabIdNext id
		    val (lll, llr) = splitList nxt (L.delete l ll)
		    val slid = sl_sl_labid id lll
		    val slty = sl_sl_labtype t llr
		in A.ExBindOf (slid, slty, r, l, n)
		end
	    else
		let
		    val nxt = A.getLabIdNext id
		    val (lll, llr) = splitList nxt ll
		    val slid = sl_sl_labid id lll
		    val slty = sl_sl_labtype t llr
		in case (slid, slty) of
		       (A.LabIdDots [(A.PartLgid (A.LongIdId id))],
			A.LabTypeDots []) => A.ExBindNo (id, n)
		     | _ => A.ExBindDots ((flattenLabId [slid]) @ (flattenLabType [slty]))
		end
	  | sl_exbind (A.ExBindEq (id, sid, r, l, n)) ll =
	    if isin l ll
	    then
		let
		    val nxt = A.getLabIdNext id
		    val (lll, llr) = splitList nxt (L.delete l ll)
		    val slid  = sl_sl_labid id lll
		    val slsid = sl_sl_longid sid llr
		in A.ExBindEq (slid, slsid, r, l, n)
		end
	    else
		let
		    val nxt = A.getLabIdNext id
		    val (lll, llr) = splitList nxt ll
		    val slid  = sl_sl_labid id lll
		    val slsid = sl_sl_longid sid llr
		in (*case (slid, slsid) of
			     (_, A.LongIdQual _) => A.ExBindEq (slid, slsid, r, l, n)
			   | (_, A.LongIdId _)   => A.ExBindEq (slid, slsid, r, l, n)
			   (* The two above are because if we have the id on the right only
			    * then it means that it is important that it is an exception.
			    * This is wrong because in:
			    *   structure S = struct end; exception e = S.e;
			    * we don't need the exception and =.
			    * Where is it we need the = but we don't have the label? *)
			   (*| (A.LabIdDots [(A.PartLgid (A.LongIdId id))],
			      A.LongIdDots []) => A.ExBindNo (id, n)*)
			   | _ =>*) A.ExBindDots ((flattenLabId [slid]) @ (flattenLongId [slsid]))
		end
	  | sl_exbind (A.ExBindNo (id, n)) ll =
	    (case sl_ident id ll of
		 A.IdentDots => A.ExBindDots []
	       | slid          => A.ExBindNo (slid, n))
	  | sl_exbind (A.ExBindDots pl) ll = A.ExBindDots (sl_partlist pl ll)

	and sl_sl_exbindlist x ll =
	    if isEmpty ll
	    then []
	    else sl_exbindlist x ll

	and sl_exbindlist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_exbindlist (x :: xs) ll =
	    let val nxt = A.getExBindNext x
		val (lll, llr) = splitList nxt ll
		val slx  = sl_sl_exbind x lll
		val slxs = sl_sl_exbindlist xs llr
	    in slx :: slxs
	    end

	and sl_sl_exbindseq x ll =
	    if isEmpty ll
	    then A.ExBindSeqDots []
	    else sl_exbindseq x ll

	and sl_exbindseq (A.ExBindSeq (ebl, rl, n)) ll =
	    let val slebl = flattenExBind (sl_exbindlist ebl ll)
	    in case slebl of
		   [] => A.ExBindSeqDots [] (* NEW - related to strictLab *)
		 | [A.ExBindDots x] => A.ExBindSeqDots x
		 | _                  => A.ExBindSeq (slebl, rl, n)
	    end
	  | sl_exbindseq (A.ExBindSeqDots pl) ll = A.ExBindSeqDots (sl_partlist pl ll)

	and sl_sl_longstrseq x ll =
	    if isEmpty ll
	    then A.LongStrSeqDots []
	    else sl_longstrseq x ll

	and sl_longstrseq (A.LongStrSeq (idl, n)) ll =
	    (case flattenLongStrId' (sl_longstridlist idl ll) of
		 [] => A.LongStrSeqDots [] (* NEW - related to strictLab *)
	       | [A.LongStrIdDots x] => A.LongStrSeqDots x
	       | x => A.LongStrSeq (x, n))
	  | sl_longstrseq (A.LongStrSeqDots pl) ll = A.LongStrSeqDots (sl_partlist pl ll)

	and sl_sl_longstridlist x ll =
	    if isEmpty ll
	    then []
	    else sl_longstridlist x ll

	and sl_longstridlist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_longstridlist (x :: xs) ll =
	    let val nxt = A.getLongStrIdNext x
		val (ll1, ll2) = splitList nxt ll
		val slx = sl_sl_longstrid x ll1
		val slxs = sl_sl_longstridlist xs ll2
	    in slx :: slxs
	    end

	and sl_sl_sigidseq x ll =
	    if isEmpty ll
	    then A.SigIdSeqDots []
	    else sl_sigidseq x ll

	and sl_sigidseq (A.SigIdSeq (xs, n)) ll =
	    (case flattenSigId' (sl_sigidlist xs ll) of
		 [] => A.SigIdSeqDots [] (* NEW - related to strictLab *)
	       | [A.SigIdDots] => A.SigIdSeqDots []
	       | x => A.SigIdSeq (x, n))
	  | sl_sigidseq (A.SigIdSeqDots pl) ll = A.SigIdSeqDots (sl_partlist pl ll)

	and sl_sl_sigidlist x ll =
	    if isEmpty ll
	    then []
	    else sl_sigidlist x ll

	and sl_sigidlist[] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_sigidlist (x :: xs) ll =
	    let
		val nxt = A.getSigIdNext x
		val (ll1, ll2) = splitList nxt ll
		val slx = sl_sl_sigid x ll1
		val slxs = sl_sl_sigidlist xs ll2
	    in slx :: slxs
	    end

	and sl_sl_identseq x ll =
	    if isEmpty ll
	    then A.IdentSeqDots []
	    else sl_identseq x ll

	and sl_identseq (A.IdentSeq (idl, n)) ll =
	    (case flattenIdent' (sl_identlist idl ll) of
		 [] => A.IdentSeqDots [] (* NEW - related to strictLab *)
	       | [A.IdentDots] => A.IdentSeqDots []
	       | x => A.IdentSeq (x, n))
	  | sl_identseq (A.IdentSeqDots pl) ll = A.IdentSeqDots (sl_partlist pl ll)

	and sl_sl_identlist x ll =
	    if isEmpty ll
	    then []
	    else sl_identlist x ll

	and sl_identlist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_identlist (x :: xs) ll =
	    let
		val nxt = A.getIdentNext x
		val (ll1, ll2) = splitList nxt ll
		val slx = sl_sl_ident x ll1
		val slxs = sl_sl_identlist xs ll2
	    in slx :: slxs
	    end

	and sl_sl_labtyclass x ll =
	    if isEmpty ll
	    then A.LabTyClassDots []
	    else sl_labtyclass x ll

	and sl_labtyclass (A.LabTyClass (t, rl, l, n)) ll =
	    if isin l ll
	    then A.LabTyClass (sl_sl_tyclass t (L.delete l ll), rl, l, n)
	    else A.LabTyClassDots (flattenTyClass [sl_tyclass t ll])
	  | sl_labtyclass (A.LabTyClassDots pl) ll = A.LabTyClassDots (sl_partlist pl ll)

	and sl_sl_tyclass x ll =
	    if isEmpty ll
	    then A.TyClassDots []
	    else sl_tyclass x ll

	and sl_tyclass (A.TyClassCl (cl, r, l, n)) ll =
	    if isin l ll
	    then A.TyClassCl (sl_sl_labclass cl (L.delete l ll), r, l, n)
	    else A.TyClassDots (flattenLabClass [sl_labclass cl ll])
	  | sl_tyclass (A.TyClassTy (ty, l, n)) ll =
	    if isin l ll
	    then A.TyClassTy (sl_sl_type ty (L.delete l ll), l, n)
	    else A.TyClassDots (flattenType [sl_type ty ll])
	  | sl_tyclass (A.TyClassDots pl) ll =
	    A.TyClassDots (sl_partlist pl ll)

	and sl_sl_labtyclasslist xs ll = sl_labtyclasslist xs ll

	and sl_labtyclasslist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_labtyclasslist (x :: xs) ll =
	    let
		val nxt = A.getLabTyClassNext x
		val (lll, llr) = splitList nxt ll
		val slty = sl_sl_labtyclass x lll
		val sltl = sl_labtyclasslist xs llr
	    in slty :: sltl
	    end

	and sl_sl_tyclassseq x ll =
	    if isEmpty ll
	    then A.TyClassSeqDots []
	    else sl_tyclassseq x ll

	and sl_tyclassseq (A.TyClassSeqOne (ty, r, l, n)) ll =
	    if isin l ll
	    then A.TyClassSeqOne (sl_sl_tyclass ty (L.delete l ll), r, l, n)
	    else A.TyClassSeqDots (flattenTyClass [sl_tyclass ty ll])
	  | sl_tyclassseq (A.TyClassSeqEm (r, l, n)) ll =
	    if isinone l ll
	    then A.TyClassSeqEm (r, l, n)
	    else
		if strictLab
		then raise EH.DeadBranch (msgOne l ll)
		else A.TyClassSeqDots []
	  | sl_tyclassseq (A.TyClassSeqSeq (tl, rl, l, n)) ll =
	    if isin l ll
	    then A.TyClassSeqSeq (sl_sl_labtyclasslist tl (L.delete l ll), rl, l, n)
	    else A.TyClassSeqDots (flattenLabTyClass (sl_labtyclasslist tl ll))
	  | sl_tyclassseq (A.TyClassSeqDots pl) ll = A.TyClassSeqDots (sl_partlist pl ll)

	and sl_sl_dec x ll =
	    if isEmpty ll
	    then A.DecDots []
	    else sl_dec x ll

	and sl_dec (A.DecVal (tvs, vb, r, n)) ll =
	    let val nxt = A.getTypeVarSeqNext tvs
		val (ll1, ll2) = splitList nxt ll
		val sltvs = sl_sl_tyvarseq tvs ll1
		val slvb  = sl_sl_valbind vb ll2
	    in case (slvb, sltvs) of
		   (A.ValBindDots x, A.TypeVarSeqDots []) => A.DecDots x
		 | _ => A.DecVal (sltvs, slvb, r, n)
	    end
	  | sl_dec (A.DecFVal (tvs, fvb, r, n)) ll =
	    let val nxt = A.getTypeVarSeqNext tvs
		val (ll1, ll2) = splitList nxt ll
		val sltvs = sl_sl_tyvarseq tvs ll1
		val slfvb = sl_sl_fvalbind fvb ll2
	    in case (slfvb, sltvs) of
		   (A.FValBindDots x, A.TypeVarSeqDots []) => A.DecDots x
		 | _ => A.DecFVal (sltvs, slfvb, r, n)
	    end
	  | sl_dec (A.DecDatType (dbs, r, n)) ll =
	    let val sldbs = sl_datbindseq dbs ll
	    in case sldbs of
		   A.DatBindSeqDots x => A.DecDots x
		 | _                    => A.DecDatType (sldbs, r, n)
	    end
	  | sl_dec (A.DecDatWith (db, tb, rs, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getDatBindSeqNext db
		     val (ll1, ll2) = splitList nxt (L.delete l ll)
		     val sldb = sl_sl_datbindseq db ll1
		     val sltb = sl_sl_typbindseq tb ll2
		 in A.DecDatWith (sldb, sltb, rs, l, n)
		 end
	    else let val nxt = A.getDatBindSeqNext db
		     val (ll1, ll2) = splitList nxt ll
		     val sldb = sl_sl_datbindseq db ll1
		     val sltb = sl_sl_typbindseq tb ll2
		 in case (sldb, sltb) of
			(A.DatBindSeqDots x, A.TypBindSeqDots y) => A.DecDots (x @ y)
		      | _ => A.DecDatWith (sldb, sltb, rs, l, n)
		 end
	  | sl_dec (A.DecDatRep (tc, ltc, rs, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getTyConNext tc
		     val (ll1, ll2) = splitList nxt (L.delete l ll)
		     val sltc  = sl_sl_tycon tc ll1
		     val slltc = sl_sl_longtycon ltc ll2
		 in A.DecDatRep (sltc, slltc, rs, l, n)
		 end
	    else let val nxt = A.getTyConNext tc
		     val (ll1, ll2) = splitList nxt ll
		     val sltc  = sl_sl_tycon tc ll1
		     val slltc = sl_sl_longtycon ltc ll2
		 in case sltc of
			A.TyConDots => A.DecDots (flattenLongTyCon [slltc])
		      | _ => A.DecDatRep (sltc, slltc, rs, l, n)
		 end
    	  | sl_dec (A.DecType (tbs, r, n)) ll =
	    let val sltbs = sl_typbindseq tbs ll
	    in case sltbs of
		   A.TypBindSeqDots x => A.DecDots x
		 | _                    => A.DecType (sltbs, r, n)
	    end
	  | sl_dec (A.DecEx (ebs, r, n)) ll =
	    let val slebs = sl_exbindseq ebs ll
	    in case slebs of
		   A.ExBindSeqDots x => A.DecDots x
		 | _                   => A.DecEx (slebs, r, n)
	    end
	  | sl_dec (A.DecOpen (ids, r, l, n)) ll =
	    if isin l ll
	    then A.DecOpen (sl_sl_longstrseq ids (L.delete l ll), r, l, n)
	    else (case sl_longstrseq ids ll of
		      A.LongStrSeqDots x => A.DecDots x
		    | x => A.DecOpen (x, r, l, n))
	  | sl_dec (A.DecLocal (d1, d2, rs, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getDecsNext d1
		     val (ll1, ll2) = splitList nxt (L.delete l ll)
		     val sld1 = sl_sl_decs d1 ll1
		     val sld2 = sl_sl_decs d2 ll2
		 in A.DecLocal (sld1, sld2, rs, l, n)
		 end
	    else let val nxt = A.getDecsNext d1
		     val (ll1, ll2) = splitList nxt ll
		     val sld1 = sl_sl_decs d1 ll1
		     val sld2 = sl_sl_decs d2 ll2
		 in A.DecDots (flattenDecs [sld1, sld2])
		 end
	  | sl_dec (A.DecAbsType (db, ds, rs, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getDatBindSeqNext db
		     val (ll1, ll2) = splitList nxt (L.delete l ll)
		     val sldb = sl_sl_datbindseq db ll1
		     val slds = sl_sl_decs ds ll2
		 in A.DecAbsType (sldb, slds, rs, l, n)
		 end
	    else let val nxt = A.getDatBindSeqNext db
		     val (ll1, ll2) = splitList nxt ll
		     val sldb = sl_sl_datbindseq db ll1
		     val slds = sl_sl_decs ds ll2
		 in case sldb of
			A.DatBindSeqDots x => A.DecDots (x @ (flattenDecs [slds]))
		      | _ => A.DecAbsType (sldb, slds, rs, l, n)
		 end
	  | sl_dec (A.DecAbsWith (db, tb, ds, rs, l, n)) ll =
	    if isin l ll
	    then let val nxt1 = A.getDatBindSeqNext db
		     val nxt2 = A.getTypBindSeqNext tb
		     val (ll1, ll') = splitList nxt1 (L.delete l ll)
		     val (ll2, ll3) = splitList nxt2 ll'
		     val sldb = sl_sl_datbindseq db ll1
		     val sltb = sl_sl_typbindseq tb ll2
		     val slds = sl_sl_decs ds ll3
		 in A.DecAbsWith (sldb, sltb, slds, rs, l, n)
		 end
	    else let val nxt1 = A.getDatBindSeqNext db
		     val nxt2 = A.getTypBindSeqNext tb
		     val (ll1, ll') = splitList nxt1 ll
		     val (ll2, ll3) = splitList nxt2 ll'
		     val sldb = sl_sl_datbindseq db ll1
		     val sltb = sl_sl_typbindseq tb ll2
		     val slds = sl_sl_decs ds ll3
		 in case (sldb, sltb) of
			(A.DatBindSeqDots x, A.TypBindSeqDots y) =>
			A.DecDots (x @ y @ (flattenDecs [slds]))
		      | _ => A.DecAbsWith (sldb, sltb, slds, rs, l, n)
		 end
	  | sl_dec (A.DecInfix (i, ids, r, l, n)) ll =
	    if isin l ll
	    then A.DecInfix (i, sl_identseq ids (L.delete l ll), r, l, n)
	    else (case sl_identseq ids ll of
		      A.IdentSeqDots x => A.DecDots x
		    | x => A.DecInfix (i, x, r, l, n))
	  | sl_dec (A.DecInfixr (i, ids, r, l, n)) ll =
	    if isin l ll
	    then A.DecInfixr (i, sl_identseq ids (L.delete l ll), r, l, n)
	    else (case sl_identseq ids ll of
		      A.IdentSeqDots x => A.DecDots x
		    | x => A.DecInfixr (i, x, r, l, n))
	  | sl_dec (A.DecNonfix (ids, r, l, n)) ll =
	    if isin l ll
	    then A.DecNonfix (sl_identseq ids (L.delete l ll), r, l, n)
	    else (case sl_identseq ids ll of
		      A.IdentSeqDots x => A.DecDots x
		    | x => A.DecNonfix (x, r, l, n))
	  | sl_dec (A.DecOverload (id, ty, tv, ts, rs, l, n)) ll =
	    if isin l ll
	    then let val nxt1 = A.getLabIdNext    id
		     val nxt2 = A.getLabTypeNext  ty
		     val nxt3 = A.getLabTypeVarNext tv
		     val (ll1, ll')  = splitList nxt1 (L.delete l ll)
		     val (ll2, ll'') = splitList nxt2 ll'
		     val (ll3, ll4)  = splitList nxt3 ll''
		     val slid = sl_sl_labid      id ll1
		     val slty = sl_sl_labtype    ty ll2
		     val sltv = sl_sl_labtyvar   tv ll3
		     val slts = sl_sl_tyclassseq ts ll4
		 in A.DecOverload (slid, slty, sltv, slts, rs, l, n)
		 end
	    else let val nxt1 = A.getLabIdNext    id
		     val nxt2 = A.getLabTypeNext  ty
		     val nxt3 = A.getLabTypeVarNext tv
		     val (ll1, ll')  = splitList nxt1 ll
		     val (ll2, ll'') = splitList nxt2 ll'
		     val (ll3, ll4)  = splitList nxt3 ll''
		     val slid = sl_sl_labid      id ll1
		     val slty = sl_sl_labtype    ty ll2
		     val sltv = sl_sl_labtyvar   tv ll3
		     val slts = sl_sl_tyclassseq ts ll4
		 in A.DecDots ((flattenLabId      [slid]) @
			       (flattenLabType    [slty]) @
			       (flattenLabTypeVar   [sltv]) @
			       (flattenTyClassSeq [slts]))
		 end
	  | sl_dec (A.DecClass (id, ts, r, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getLabClassNext id
		     val (ll1, ll2) = splitList nxt (L.delete l ll)
		     val slid = sl_sl_labclass   id ll1
		     val slts = sl_sl_tyclassseq ts ll2
		 in A.DecClass (slid, slts, r, l, n)
		 end
	    else let val nxt = A.getLabClassNext id
		     val (ll1, ll2) = splitList nxt ll
		     val slid = sl_sl_labclass   id ll1
		     val slts = sl_sl_tyclassseq ts ll2
		 in A.DecDots ((flattenLabClass [slid]) @ (flattenTyClassSeq [slts]))
		 end
	  | sl_dec (A.DecDots pl) ll = A.DecDots (sl_partlist pl ll)

	and sl_sl_declist x ll =
	    if isEmpty ll
	    then []
	    else sl_declist x ll

	and sl_declist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_declist (d :: dl) ll =
	    let val nxt = A.getDecNext d
		val (lll, llr) = splitList nxt ll
		val sld = sl_sl_dec d lll
		val sldl = sl_sl_declist dl llr
	    in sld :: sldl
	    end

	and sl_sl_decs x ll =
	    if isEmpty ll
	    then A.DecsDots []
	    else sl_decs x ll

	and sl_decs (A.Decs (ds, _)) ll = A.DecsDots (flattenDec (sl_declist ds ll))
	  | sl_decs (A.DecsDots pl)  ll = A.DecsDots (sl_partlist pl ll)

	and sl_explist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_explist (e :: el) ll =
	    let
		val nxt = A.getExpNext e
		val (lll, llr) = splitList nxt ll
		val sle = sl_sl_exp e lll
		val slel = sl_explist el llr
	    in sle :: slel
	    end

	and sl_sl_labexplist xs ll = sl_labexplist xs ll

	and sl_labexplist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_labexplist (e :: el) ll =
	    let
		val nxt = A.getLabExpNext e
		val (lll, llr) = splitList nxt ll
		val sle = sl_sl_labexp e lll
		val slel = sl_labexplist el llr
	    in sle :: slel
	    end

	and sl_sl_labexp x ll =
	    if isEmpty ll
	    then A.LabExpDots []
	    else sl_labexp x ll

	and sl_labexp (A.LabExp (e, rl, re, l, n)) ll =
	    if isin l ll
	    then A.LabExp (sl_sl_exp e (L.delete l ll), rl, re, l, n)
	    else A.LabExpDots (flattenExp [sl_exp e ll])
	  | sl_labexp (A.LabExpDots pl) ll = A.LabExpDots (sl_partlist pl ll)

	and sl_sl_expfield x ll =
	    if isEmpty ll
	    then A.ExpFieldDots []
	    else sl_expfield x ll

	and sl_expfield (A.ExpField (tl, e, r, rl, l, n)) ll =
	    if isin l ll
	    then
		let
		    val nxt = A.getTyLabNext tl
		    val (lll, llr) = splitList nxt (L.delete l ll)
		    val sltl = sl_sl_tylab tl lll
		    val sle  = sl_sl_labexp e llr
		in A.ExpField (sltl, sle, r, rl, l, n)
	  	end
	    else
		let
		    val nxt = A.getTyLabNext tl
		    val (lll, llr) = splitList nxt ll
		    val sltl = sl_sl_tylab tl lll
		    val sle  = sl_sl_labexp e llr
		in case sltl of
		       A.TyLabDots => A.ExpFieldDots (flattenLabExp [sle])
		     | _             => A.ExpField (sltl, sle, r, rl, l, n)
	  	end
	  | sl_expfield (A.ExpFieldDots pl) ll = A.ExpFieldDots (sl_partlist pl ll)

	and sl_sl_expfieldlist xs ll = sl_expfieldlist xs ll

	and sl_expfieldlist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_expfieldlist (x :: xs) ll =
	    let
		val nxt = A.getExpFieldNext x
		val (lll, llr) = splitList nxt ll
		val slx  = sl_sl_expfield x lll
		val slxs = sl_expfieldlist xs llr
	    in slx :: slxs
	    end

	and sl_sl_seqexp x ll =
	    if isEmpty ll
	    then A.SeqExpDots []
	    else sl_seqexp x ll

	and sl_seqexp (A.SeqExp (el, e, r, rs, l, n)) ll =
	    if isin l ll
	    then
		let
		    val slell = sl_sl_labexplist (el @ [e]) (L.delete l ll)
		    val sle   = List.hd (List.rev slell) handle Empty => raise EH.DeadBranch ""
		    val slel  = List.tl (List.rev slell) handle Empty => raise EH.DeadBranch ""
		in A.SeqExpSl (flattenLabExp slel, sle, r, l, n)
		end
	    else A.SeqExpDots (flattenLabExp (sl_labexplist (el @ [e]) ll))
	  | sl_seqexp (A.SeqExpSl (pl, e, r, l, n)) ll =
	    if isin l ll
	    then let
		val nxt = A.getPartListNext pl
		val (lll, llr) = splitList nxt (L.delete l ll)
		val slpl = sl_sl_partlist pl lll
		val sle  = sl_sl_labexp e llr
	    in A.SeqExpSl (slpl, sle, r, l, n)
	    end
	    else A.SeqExpDots (sl_partlist (pl @ (flattenLabExp [e])) ll)
	  | sl_seqexp (A.SeqExpDots pl) ll = A.SeqExpDots (sl_partlist pl ll)

	and sl_sl_atexp x ll =
	    if isEmpty ll
	    then A.AtExpDots []
	    else sl_atexp x ll

	and sl_atexp (A.AtExpId id) ll =
	    (case sl_longid id ll of
		 A.LongIdDots pl => A.AtExpDots pl
	       | x                 => A.AtExpId x)
	  | sl_atexp (A.AtExpScon sc) ll =
	    (case sl_scon sc ll of
		 A.SconDots => A.AtExpDots []
	       | x            => A.AtExpScon x)
	  | sl_atexp (A.AtExpTuple (el, rl, l, n)) ll =
	    if isin l ll
	    then A.AtExpTuple (sl_sl_labexplist el (L.delete l ll), rl, l, n)
	    else A.AtExpDots (flattenLabExp (sl_labexplist el ll))
	  | sl_atexp (A.AtExpRecord (erl, rl1, rl2, l, n)) ll =
	    if isin l ll
	    then A.AtExpRecord (sl_sl_expfieldlist erl (L.delete l ll), rl1, rl2, l, n)
	    else
		let
		    val sler = sl_expfieldlist erl ll
		in case flattenExpField sler of
		       [] => A.AtExpDots [] (* NEW - related to strictLab *)
		     | [A.ExpFieldDots x] => A.AtExpDots x
		     | x                  => A.AtExpSlRec (x, rl1, l, n) (* NEW: x instead of sler *)
		end
	  | sl_atexp (A.AtExpSlRec (erl, rl, l, n)) ll =
	    if isin l ll
	    then A.AtExpSlRec (sl_sl_expfieldlist erl (L.delete l ll), rl, l, n)
	    else
		let
		    val slerl = sl_expfieldlist erl ll
		in case flattenExpField slerl of
		       [] => A.AtExpDots [] (* NEW - related to strictLab *)
		     | [A.ExpFieldDots x] => A.AtExpDots x
		     | x                  => A.AtExpSlRec (x, rl, l, n)
		end
	  | sl_atexp (A.AtExpLet (ds, e, rl, l, n)) ll =
	    if isin l ll
	    then let
		val (lll, llr) = splitList (A.getDecsNext ds) (L.delete l ll)
		val slds = sl_sl_decs ds lll
		val sle = sl_sl_labexp e llr
	    in A.AtExpLet (slds, sle, rl, l, n)
	    end
	    else let
		val (lll, llr) = splitList (A.getDecsNext ds) ll
		val slds = sl_sl_decs ds lll
		val sle  = sl_sl_labexp e llr
	    in A.AtExpDots ((flattenDecs [slds]) @ (flattenLabExp [sle]))
	    end
	  | sl_atexp (A.AtExpDLet (ds, s, rl, l, n)) ll =
	    if isin l ll
	    then let
		val (lll, llr) = splitList (A.getDecsNext ds) (L.delete l ll)
		val slds = sl_sl_decs ds lll
		val sls  = sl_sl_seqexp s llr
	    in A.AtExpDLet (slds, sls, rl, l, n)
	    end
	    else let
		val (lll, llr) = splitList (A.getDecsNext ds) ll
		val slds = sl_sl_decs ds lll
		val sls  = sl_sl_seqexp s llr
	    in A.AtExpDots ((flattenDecs [slds]) @ (flattenSeqExp [sls]))
	    end
	  | sl_atexp (A.AtExpParen (e, r1, r2, l, n)) ll =
	    if isin l ll
	    then A.AtExpParen (sl_sl_labexp e (L.delete l ll), r1, r2, l, n)
	    else A.AtExpDots (flattenLabExp [sl_labexp e ll])
	  | sl_atexp (A.AtExpList (el, rl, l, n)) ll =
	    if isin l ll
	    then A.AtExpList (sl_sl_labexplist el (L.delete l ll), rl, l, n)
	    else A.AtExpDots (flattenLabExp (sl_labexplist el ll))
	  | sl_atexp (A.AtExpProj (tl, r, rt, l, n)) ll =
	    if isin l ll
	    then A.AtExpProj (sl_sl_tylab tl (L.delete l ll), r, rt, l, n)
	    else let val sltl = sl_tylab tl ll
		 in case sltl of
			A.TyLabDots => A.AtExpDots []
		      | _             => A.AtExpProj (sltl, r, rt, l, n)
		 end
	  | sl_atexp (A.AtExpSeq (seq, rs, l, n)) ll =
	    if isin l ll
	    then A.AtExpSeq (sl_sl_seqexp seq (L.delete l ll), rs, l, n)
	    else A.AtExpDots (flattenSeqExp [sl_seqexp seq ll])
	  | sl_atexp (A.AtExpQuote (quotes, regs, lab, next)) ll =
	    if isin lab ll
	    then A.AtExpQuote (sl_sl_quotes quotes (L.delete lab ll), regs, lab, next)
	    else A.AtExpDots (flattenQuote (sl_quotes quotes ll))
	  | sl_atexp (A.AtExpDots pl) ll = A.AtExpDots (sl_partlist pl ll)

	and sl_sl_quotes quotes ll = sl_quotes quotes ll

	and sl_quotes [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_quotes (quote :: quotes) ll =
	    let val nxt = A.getQuoteNext quote
		val (lll, llr) = splitList nxt ll
		val slq = sl_sl_quote quote lll
		val slqs = sl_quotes quotes llr
	    in slq :: slqs
	    end

	and sl_sl_quote quote ll =
	    if isEmpty ll
	    then A.QuoteDots []
	    else sl_quote quote ll

	and sl_quote (A.Quote (string, reg, lab, next)) ll =
	    if isinone lab ll
	    then A.Quote (string, reg, lab, next)
	    else if strictLab
	    then raise EH.DeadBranch (msgOne lab ll)
	    else A.QuoteDots []
	  | sl_quote (A.Antiquote (exp, regs, lab, next)) ll =
	    if isin lab ll
	    then A.Antiquote (sl_sl_exp exp (L.delete lab ll), regs, lab, next)
	    else A.QuoteDots (flattenExp [sl_exp exp ll])
	  | sl_quote (A.QuoteDots pl) ll = A.QuoteDots (sl_partlist pl ll)

	and sl_sl_exp x ll =
	    if isEmpty ll
	    then A.ExpDots []
	    else sl_exp x ll

	and sl_exp (A.ExpAtExp atexp) ll =
	    let val slatexp = sl_atexp atexp ll
	    in case slatexp of
		   A.AtExpDots x => A.ExpDots x
		 | x             => A.ExpAtExp x
	    end
	  | sl_exp (A.ExpFn (m, r, l, n)) ll =
	    if isin l ll
	    then A.ExpFn (sl_sl_match m (L.delete l ll), r, l, n)
	    else (case sl_match m ll of
		      A.MatchDots x => A.ExpDots x
		    | x               => A.ExpFn (x, r, l, n))
	  | sl_exp (A.ExpApp (e, a, rl, r1, r2, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getExpNext e
		     val (lll, llr) = splitList nxt (L.delete l ll)
		     val sle = sl_sl_exp e lll
		     val sla = sl_sl_atexp a llr
		 in A.ExpApp (sle, sla, rl, r1, r2, l, n)
		 end
	    else let val nxt = A.getExpNext e
		     val (lll, llr) = splitList nxt ll
		     val sle = sl_sl_exp e lll
		     val sla = sl_sl_atexp a llr
		 in A.ExpDots ((flattenExp [sle]) @ (flattenAtExp [sla]))
		 end
	  | sl_exp (A.ExpCase (e, m, r1, r2, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getLabExpNext e
		     val (lll, llr) = splitList nxt (L.delete l ll)
		     val sle = sl_sl_labexp e lll
		     val slm = sl_sl_match m llr
		 in A.ExpCase (sle, slm, r1, r2, l, n)
		 end
	    else let val nxt = A.getLabExpNext e
		     val (lll, llr) = splitList nxt ll
		     val sle = sl_sl_labexp e lll
		     val slm = sl_sl_match m llr
		 in case slm of
			A.MatchDots x => A.ExpDots ((flattenLabExp [sle]) @ x)
		      | _               => A.ExpCase (sle, slm, r1, r2, l, n)
		 end
	  | sl_exp (A.ExpConsList (v, e1, e2, r, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getLabExpNext e1
		     val (lll, llr) = splitList nxt (L.delete l ll)
		     val sle1 = sl_sl_labexp e1 lll
		     val sle2 = sl_sl_labexp e2 llr
		 in A.ExpConsList (v, sle1, sle2, r, l, n)
		 end
	    else let val nxt = A.getLabExpNext e1
		     val (lll, llr) = splitList nxt ll
		     val sle1 = sl_sl_labexp e1 lll
		     val sle2 = sl_sl_labexp e2 llr
		 in A.ExpDots (flattenLabExp [sle1, sle2])
		 end
	  | sl_exp (A.ExpOp (s, v, e1, e2, r, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getLabExpNext e1
		     val (lll, llr) = splitList nxt (L.delete l ll)
		     val sle1 = sl_sl_labexp e1 lll
		     val sle2 = sl_sl_labexp e2 llr
		 in A.ExpOp (s, v, sle1, sle2, r, l, n)
		 end
	    else let val nxt = A.getLabExpNext e1
		     val (lll, llr) = splitList nxt ll
		     val sle1 = sl_sl_labexp e1 lll
		     val sle2 = sl_sl_labexp e2 llr
		 in A.ExpDots (flattenLabExp [sle1, sle2])
		 end
	  | sl_exp (A.ExpOr (e1, e2, r, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getLabExpNext e1
		     val (lll, llr) = splitList nxt (L.delete l ll)
		     val sle1 = sl_sl_labexp e1 lll
		     val sle2 = sl_sl_labexp e2 llr
		 in A.ExpOr (sle1, sle2, r, l, n)
		 end
	    else let val nxt = A.getLabExpNext e1
		     val (lll, llr) = splitList nxt ll
		     val sle1 = sl_sl_labexp e1 lll
		     val sle2 = sl_sl_labexp e2 llr
		 in A.ExpDots (flattenLabExp [sle1, sle2])
		 end
	  | sl_exp (A.ExpAnd (e1, e2, r, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getLabExpNext e1
		     val (lll, llr) = splitList nxt (L.delete l ll)
		     val sle1 = sl_sl_labexp e1 lll
		     val sle2 = sl_sl_labexp e2 llr
		 in A.ExpAnd (sle1, sle2, r, l, n)
		 end
	    else let val nxt = A.getLabExpNext e1
		     val (lll, llr) = splitList nxt ll
		     val sle1 = sl_sl_labexp e1 lll
		     val sle2 = sl_sl_labexp e2 llr
		 in A.ExpDots (flattenLabExp [sle1, sle2])
		 end
	  | sl_exp (A.ExpTyped (e, t, r, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getLabExpNext e
		     val (lll, llr) = splitList nxt (L.delete l ll)
		     val sle = sl_sl_labexp e lll
		     val slt = sl_sl_labtype t llr
		 in A.ExpTyped (sle, slt, r, l, n)
		 end
	    else let val nxt = A.getLabExpNext e
		     val (lll, llr) = splitList nxt ll
		     val sle = sl_sl_labexp e lll
		     val slt = sl_sl_labtype t llr
		 in A.ExpDots ((flattenLabExp [sle]) @ (flattenLabType [slt]))
		 end
	  | sl_exp (A.ExpIte (e1, e2, e3, rl, l, n)) ll =
	    if isin l ll
	    then let val nxt1 = A.getLabExpNext e1
		     val nxt2 = A.getLabExpNext e2
		     val (ll1, llr) = splitList nxt1 (L.delete l ll)
		     val (ll2, ll3) = splitList nxt2 llr
		     val sle1 = sl_sl_labexp e1 ll1
		     val sle2 = sl_sl_labexp e2 ll2
		     val sle3 = sl_sl_labexp e3 ll3
		 in A.ExpIte (sle1, sle2, sle3, rl, l, n)
		 end
	    else let val nxt1 = A.getLabExpNext e1
		     val nxt2 = A.getLabExpNext e2
		     val (ll1, llr) = splitList nxt1 ll
		     val (ll2, ll3) = splitList nxt2 llr
		     val sle1 = sl_sl_labexp e1 ll1
		     val sle2 = sl_sl_labexp e2 ll2
		     val sle3 = sl_sl_labexp e3 ll3
		 in A.ExpDots (flattenLabExp [sle1, sle2, sle3])
		 end
	  | sl_exp (A.ExpWhile (e1, e2, r1, r2, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getLabExpNext e1
		     val (lll, llr) = splitList nxt (L.delete l ll)
		     val sle1 = sl_sl_labexp e1 lll
		     val sle2 = sl_sl_labexp e2 llr
		 in A.ExpWhile (sle1, sle2, r1, r2, l, n)
		 end
	    else let val nxt = A.getLabExpNext e1
		     val (lll, llr) = splitList nxt ll
		     val sle1 = sl_sl_labexp e1 lll
		     val sle2 = sl_sl_labexp e2 llr
		 in A.ExpDots (flattenLabExp [sle1, sle2])
		 end
	  | sl_exp (A.ExpRaise (e, r, l, n)) ll =
	    if isin l ll
	    then A.ExpRaise (sl_sl_labexp e (L.delete l ll), r, l, n)
	    else A.ExpDots (flattenLabExp [sl_labexp e ll])
	  | sl_exp (A.ExpHandle (e, m, r, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getLabExpNext e
		     val (lll, llr) = splitList nxt (L.delete l ll)
		     val sle = sl_sl_labexp e lll
		     val slm = sl_sl_match m llr
		 in A.ExpHandle (sle, slm, r, l, n)
		 end
	    else let val nxt = A.getLabExpNext e
		     val (lll, llr) = splitList nxt ll
		     val sle = sl_sl_labexp e lll
		     val slm = sl_sl_match m llr
		 in  case slm of
			 A.MatchDots x => A.ExpDots (flattenLabExp [sle] @ x)
		       | _               => A.ExpHandle (sle, slm, r, l, n)
		 end
	  | sl_exp (A.ExpDots pl) ll = A.ExpDots (sl_partlist pl ll)

	and sl_sl_match x ll =
	    if isEmpty ll
	    then A.MatchDots []
	    else sl_match x ll

	and sl_match (A.Match (ml, rl, n)) ll =
	    let
		val slml = flattenMrule (sl_mrulelist ml ll)
	    in case slml of
		   [] => A.MatchDots [] (* NEW - related to strictLab *)
		 | [A.MruleDots x] => A.MatchDots x
		 | _                 => A.Match (slml, rl, n)
	    end
	  | sl_match (A.MatchDots pl) ll = A.MatchDots (sl_partlist pl ll)

	and sl_sl_mrulelist x ll =
	    if isEmpty ll
	    then []
	    else sl_mrulelist x ll

	and sl_mrulelist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_mrulelist (m :: ml) ll =
	    let
		val nxt = A.getMruleNext m
		val (lll, llr) = splitList nxt ll
		val slm = sl_sl_mrule m lll
		val slml = sl_sl_mrulelist ml llr
	    in slm :: slml
	    end

	and sl_sl_mrule x ll =
	    if isEmpty ll
	    then A.MruleDots []
	    else sl_mrule x ll

	and sl_mrule (A.Mrule (p, e, r, l, n)) ll =
	    if isin l ll
	    then let
		val nxt = A.getLabPatNext p
		val (lll, llr) = splitList nxt (L.delete l ll)
		val slp = sl_sl_labpat p lll
		val sle = sl_sl_labexp e llr
	    in A.Mrule (slp, sle, r, l, n)
	    end
	    else let
		val nxt = A.getLabPatNext p
		val (lll, llr) = splitList nxt ll
		val slp = sl_sl_labpat p lll
		val sle = sl_sl_labexp e llr
	    in (*A.MruleDots ((flattenLabPat [slp]) @ (flattenLabExp [sle]))*)
		case slp of
		    A.LabPatDots [] => A.MruleDots (flattenLabExp [sle])
		  | _                 => A.Mrule (slp, sle, r, l, n)
	    end
	  | sl_mrule (A.MruleDots pl) ll = A.MruleDots (sl_partlist pl ll)

	and sl_sl_labpatlist xs ll = sl_labpatlist xs ll

	and sl_labpatlist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_labpatlist (p :: pl) ll =
	    let val nxt = A.getLabPatNext p
		val (lll, llr) = splitList nxt ll
		val slp = sl_sl_labpat p lll
		val slpl = sl_labpatlist pl llr
	    in slp :: slpl
	    end

	and sl_sl_labpat x ll =
	    if isEmpty ll
	    then A.LabPatDots []
	    else sl_labpat x ll

	and sl_labpat (A.LabPat (p, rl, rp, l, n)) ll =
	    if isin l ll
	    then A.LabPat (sl_sl_pat p (L.delete l ll), rl, rp, l, n)
	    else A.LabPatDots (flattenPat [sl_pat p ll])
	  | sl_labpat (A.LabPatDots pl) ll = A.LabPatDots (sl_partlist pl ll)

	and sl_sl_patfield x ll =
	    if isEmpty ll
	    then A.PatFieldDots []
	    else sl_patfield x ll

	and sl_patfield (A.PatField (tl, p, r, rl, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getTyLabNext tl
		     val (lll, llr) = splitList nxt (L.delete l ll)
		     val sltl = sl_sl_tylab tl lll
		     val slp  = sl_sl_labpat p llr
		 in A.PatField (sltl, slp, r, rl, l, n)
	  	 end
	    else let val nxt = A.getTyLabNext tl
		     val (lll, llr) = splitList nxt ll
		     val sltl = sl_sl_tylab tl lll
		     val slp  = sl_sl_labpat p llr
		 in case sltl of
			A.TyLabDots => A.PatFieldDots (flattenLabPat [slp])
		      | _             => A.PatField (sltl, slp, r, rl, l, n)
	  	 end
	  | sl_patfield (A.PatFieldId (id, n)) ll =
	    (case sl_identty id ll of
		 A.IdentTyDots pl => A.PatFieldDots pl
	       | x                  => A.PatFieldId (x, n))
	  | sl_patfield (A.PatFieldAs (id, p, r, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getLabIdTyNext id
		     val (lll, llr) = splitList nxt (L.delete l ll)
		     val slid = sl_sl_labidty id lll
		     val slp  = sl_sl_labpat p llr
		 in A.PatFieldAs (slid, slp, r, l, n)
	  	 end
	    else let val nxt = A.getLabIdTyNext id
		     val (lll, llr) = splitList nxt ll
		     val slid = sl_sl_labidty id lll
		     val slp  = sl_sl_labpat p llr
		 in case slid of
			A.LabIdTyDots pl => A.PatFieldDots (pl @ (flattenLabPat [slp]))
		      | _                  => A.PatFieldAs (slid, slp, r, l, n)
	  	 end
	  | sl_patfield (A.PatFieldWild (r, l, n)) ll =
	    if isinone l ll
	    then A.PatFieldWild (r, l, n)
	    else if strictLab
	    then raise EH.DeadBranch (msgOne l ll)
	    else A.PatFieldDots []
	  | sl_patfield (A.PatFieldDots pl) ll = A.PatFieldDots (sl_partlist pl ll)

	and sl_sl_patfieldlist xs ll = sl_patfieldlist xs ll

	and sl_patfieldlist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_patfieldlist (x :: xs) ll =
	    let val nxt = A.getPatFieldNext x
		val (lll, llr) = splitList nxt ll
		val slx  = sl_sl_patfield x lll
		val slxs = sl_patfieldlist xs llr
	    in slx :: slxs
	    end

	and sl_idfieldlist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_idfieldlist (x :: xs) ll =
	    let val nxt = A.getIdentNext x
		val (lll, llr) = splitList nxt ll
		val slx  = sl_sl_ident x lll
		val slxs = sl_idfieldlist xs llr
	    in slx :: slxs
	    end

	and sl_sl_atpat x ll =
	    if isEmpty ll
	    then A.AtPatDots []
	    else sl_atpat x ll

	and sl_atpat (A.AtPatWild _) ll =
	    if isEmptyL ll
	    then A.AtPatDots []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_atpat (A.AtPatId id) ll =
	    (case sl_longid id ll of
		 A.LongIdDots pl => A.AtPatDots pl
	       | x                 => A.AtPatId x)
	  | sl_atpat (A.AtPatScon sc) ll =
	    (case sl_scon sc ll of
		 A.SconDots => A.AtPatDots []
	       | x            => A.AtPatScon x)
	  | sl_atpat (A.AtPatTuple (pal, rl, l, n)) ll =
	    if isin l ll
	    then A.AtPatTuple (sl_sl_labpatlist pal (L.delete l ll), rl, l, n)
	    else A.AtPatDots (flattenLabPat (sl_labpatlist pal ll))
	  | sl_atpat (A.AtPatRecord (prl, rl1, rl2, l, n)) ll =
	    if isin l ll
	    then A.AtPatRecord (sl_sl_patfieldlist prl (L.delete l ll), rl1, rl2, l, n)
	    else let val slprl = sl_patfieldlist prl ll
		 in case flattenPatField slprl of
			[] => A.AtPatDots [] (* NEW - related to strictLab *)
		      | [A.PatFieldDots x] => A.AtPatDots x
		      | x                  => A.AtPatRecord (x, rl1, rl2, l, n)
		 end
	  | sl_atpat (A.AtPatParen (pa, r1, r2, l, n)) ll =
	    if isin l ll
	    then A.AtPatParen (sl_sl_labpat pa (L.delete l ll), r1, r2, l, n)
	    else A.AtPatDots (flattenLabPat [sl_labpat pa ll])
	  | sl_atpat (A.AtPatList (pal, rl, l, n)) ll =
	    if isin l ll
	    then A.AtPatList (sl_sl_labpatlist pal (L.delete l ll), rl, l, n)
	    else A.AtPatDots (flattenLabPat (sl_labpatlist pal ll))
	  | sl_atpat (A.AtPatOr (xs, rs, l, n)) ll =
	    if isin l ll
	    then A.AtPatOr (sl_sl_labpatlist xs (L.delete l ll), rs, l, n)
	    else A.AtPatDots (flattenLabPat (sl_labpatlist xs ll))
	  | sl_atpat (A.AtPatDots pl) ll = A.AtPatDots (sl_partlist pl ll)

	and sl_sl_identty x ll =
	    if isEmpty ll
	    then A.IdentTyDots []
	    else sl_identty x ll

	and sl_identty (A.IdentTyId id) ll =
	    (case sl_ident id ll of
		 A.IdentDots => A.IdentTyDots []
	       | x             => A.IdentTyId x)
	  | sl_identty (A.IdentTyTy (id, t, r, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getLabIdNext id
		     val (lll, llr) = splitList nxt (L.delete l ll)
		     val slid = sl_sl_labid id lll
		     val slt = sl_sl_labtype t llr
		 in A.IdentTyTy (slid, slt, r, l, n)
		 end
	    else let val nxt = A.getLabIdNext id
		     val (lll, llr) = splitList nxt ll
		     val slid = sl_sl_labid id lll
		     val slt = sl_sl_labtype t llr
		 in case slid of
			(A.LabIdDots pl) => A.IdentTyDots (pl @ (flattenLabType [slt]))
		      | _                  => A.IdentTyTy (slid, slt, r, l, n)
		 end
	  | sl_identty (A.IdentTyDots pl) ll = A.IdentTyDots (sl_partlist pl ll)

	and sl_sl_labidty x ll =
	    if isEmpty ll
	    then A.LabIdTyDots []
	    else sl_labidty x ll

	and sl_labidty (A.LabIdTy (id, rl, l, n)) ll =
	    if isin l ll
	    then A.LabIdTy (sl_sl_identty id (L.delete l ll), rl, l, n)
	    else A.LabIdTyDots (flattenIdentTy [sl_identty id ll])
	  | sl_labidty (A.LabIdTyDots pl) ll = A.LabIdTyDots (sl_partlist pl ll)

	and sl_sl_pat x ll =
	    if isEmpty ll
	    then A.PatDots []
	    else sl_pat x ll

	and sl_pat (A.PatAtPat atpat) ll =
	    (case sl_atpat atpat ll of
		 A.AtPatDots x => A.PatDots x
	       | x             => A.PatAtPat x)
	  | sl_pat (A.PatApp (id, ap, rl, r, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getLongIdNext id
		     val (lll, llr) = splitList nxt (L.delete l ll)
		     val slid = sl_sl_longid id lll
		     val slap = sl_sl_atpat ap llr
		 in A.PatApp (slid, slap, rl, r, l, n)
		 end
	    else let val nxt = A.getLongIdNext id
		     val (lll, llr) = splitList nxt ll
		     val slid = sl_sl_longid id lll
		     val slap = sl_sl_atpat ap llr
		 in A.PatDots ((flattenLongId [slid]) @ (flattenAtPat [slap]))
		 end
	  | sl_pat (A.PatConsList (v, p1, p2, r, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getLabPatNext p1
		     val (lll, llr) = splitList nxt (L.delete l ll)
		     val slp1 = sl_sl_labpat p1 lll
		     val slp2 = sl_sl_labpat p2 llr
		 in A.PatConsList (v, slp1, slp2, r, l, n)
		 end
	    else let val nxt = A.getLabPatNext p1
		     val (lll, llr) = splitList nxt ll
		     val slp1 = sl_sl_labpat p1 lll
		     val slp2 = sl_sl_labpat p2 llr
		 in A.PatDots (flattenLabPat [slp1, slp2])
		 end
	  | sl_pat (A.PatOp (st, v, p1, p2, r, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getLabPatNext p1
		     val (lll, llr) = splitList nxt (L.delete l ll)
		     val slp1 = sl_sl_labpat p1 lll
		     val slp2 = sl_sl_labpat p2 llr
		 in A.PatOp (st, v, slp1, slp2, r, l, n)
		 end
	    else let val nxt = A.getLabPatNext p1
		     val (lll, llr) = splitList nxt ll
		     val slp1 = sl_sl_labpat p1 lll
		     val slp2 = sl_sl_labpat p2 llr
		 in A.PatDots (flattenLabPat [slp1, slp2])
		 end
	  | sl_pat (A.PatTyped (p, t, r, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getLabPatNext p
		     val (lll, llr) = splitList nxt (L.delete l ll)
		     val slp = sl_sl_labpat p lll
		     val slt = sl_sl_labtype t llr
		 in A.PatTyped (slp, slt, r, l, n)
		 end
	    else let val nxt = A.getLabPatNext p
		     val (lll, llr) = splitList nxt ll
		     val slp = sl_sl_labpat p lll
		     val slt = sl_sl_labtype t llr
		 in A.PatDots ((flattenLabPat [slp]) @ (flattenLabType [slt]))
		 end
	  | sl_pat (A.PatAs (id, p, r, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getLabIdTyNext id
		     val (lll, llr) = splitList nxt (L.delete l ll)
		     val slid = sl_sl_labidty id lll
		     val slp  = sl_sl_labpat p llr
		 in A.PatAs (slid, slp, r, l, n)
		 end
	    else let val nxt = A.getLabIdTyNext id
		     val (lll, llr) = splitList nxt ll
		     val slid = sl_sl_labidty id lll
		     val slp  = sl_sl_labpat p llr
		 in case slid of
			(A.LabIdTyDots pl) => A.PatDots (pl @ (flattenLabPat [slp]))
		      | _                    => A.PatAs (slid, slp, r, l, n)
		 end
	  | sl_pat (A.PatDots pl) ll = A.PatDots (sl_partlist pl ll)

	and sl_sl_strdec x ll =
	    if isEmpty ll
	    then A.StrDecDots []
	    else sl_strdec x ll

	and sl_strdec (A.StrDec (xs, l, n)) ll =
	    A.StrDecDots (flattenStrDecOne (sl_strdeconelist xs (L.delete l ll)))
	  | sl_strdec (A.StrDecDots pl) ll = A.StrDecDots (sl_partlist pl ll)

	and sl_sl_strdeconelist x ll =
	    if isEmpty ll
	    then []
	    else sl_strdeconelist x ll

	and sl_strdeconelist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_strdeconelist (x :: xs) ll =
	    let val nxt = A.getStrDecOneNext x
		val (ll1, ll2) = splitList nxt ll
		val slx  = sl_sl_strdecone     x  ll1
		val slxs = sl_sl_strdeconelist xs ll2
	    in slx :: slxs
	    end

	and sl_sl_strdecone x ll =
	    if isEmpty ll
	    then A.StrDecOneDots []
	    else sl_strdecone x ll

	and sl_strdecone (A.StrDecOneDec d) ll =
	    (case sl_decs d ll of
		 A.DecsDots pl => A.StrDecOneDots pl
	       | _               => raise EH.DeadBranch "") (* a decs returns only dots *)
	  | sl_strdecone (A.StrDecOneStr (sb, r, n)) ll =
	    (case sl_strbind sb ll of
		 A.StrBindDots x => A.StrDecOneDots x
	       | x => A.StrDecOneStr (x, r, n))
	  | sl_strdecone (A.StrDecOneFun (sb, r, l, n)) ll =
	    if isin l ll
	    then A.StrDecOneFun (sl_sl_funbind sb (L.delete l ll), r, l, n)
	    else (case sl_funbind sb ll of (* TODO: CHANGE THAT!! *)
		      A.FunBindDots x => A.StrDecOneDots x
		    | x => A.StrDecOneFun (x, r, l, n))
	  | sl_strdecone (A.StrDecOneLoc (sd1, sd2, rs, l, n)) ll =
	    if isin l ll
	    then
		let
		    val nxt = A.getStrDecNext sd1
		    val (ll1, ll2) = splitList nxt (L.delete l ll)
		    val slsd1 = sl_sl_strdec sd1 ll1
		    val slsd2 = sl_sl_strdec sd2 ll2
		in A.StrDecOneLoc (slsd1, slsd2, rs, l, n)
		end
	    else
		let
		    val nxt = A.getStrDecNext sd1
		    val (ll1, ll2) = splitList nxt ll
		    val slsd1 = sl_sl_strdec sd1 ll1
		    val slsd2 = sl_sl_strdec sd2 ll2
		in A.StrDecOneDots (flattenStrDec [slsd1, slsd2])
		end
	  | sl_strdecone (A.StrDecOneDots pl) ll =
	    A.StrDecOneDots (sl_partlist pl ll)

	and sl_sl_strbind x ll =
	    if isEmpty ll
	    then A.StrBindDots []
	    else sl_strbind x ll

	and sl_strbind (A.StrBind (xs, rs, n)) ll =
	    (case flattenStrBindOne (sl_sl_strbindonelist xs ll) of
		 [] => A.StrBindDots [] (* NEW - related to strictLab *)
	       | [A.StrBindOneDots x] => A.StrBindDots x
	       | x => A.StrBind (x, rs, n))
	  | sl_strbind (A.StrBindDots pl) ll =
	    A.StrBindDots (sl_partlist pl ll)

	and sl_sl_strbindonelist x ll =
	    if isEmpty ll
	    then []
	    else sl_strbindonelist x ll

	and sl_strbindonelist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_strbindonelist (x :: xs) ll =
	    let val nxt = A.getStrBONext x
		val (lll, llr) = splitList nxt ll
		val slx = sl_sl_strbindone x lll
		val slxs = sl_sl_strbindonelist xs llr
	    in slx :: slxs
	    end

	and sl_sl_strbindone x ll =
	    if isEmpty ll
	    then A.StrBindOneDots []
	    else sl_strbindone x ll

	and sl_strbindone (A.StrBindOneOp (id, si, se, rl, l, n)) ll =
	    if isin l ll
	    then let val nxt1 = A.getStrIdNext id
		     val nxt2 = A.getLabSigExpNext si
		     val (ll1, ll') = splitList nxt1 (L.delete l ll)
		     val (ll2, ll3) = splitList nxt2 ll'
		     val slid = sl_sl_strid     id ll1
		     val slsi = sl_sl_labsigexp si ll2
		     val slse = sl_sl_labstrexp se ll3
		 in A.StrBindOneOp (slid, slsi, slse, rl, l, n)
		 end
	    else let val nxt1 = A.getStrIdNext id
		     val nxt2 = A.getLabSigExpNext si
		     val (ll1, ll') = splitList nxt1 ll
		     val (ll2, ll3) = splitList nxt2 ll'
		     val slid = sl_sl_strid     id ll1
		     val slsi = sl_sl_labsigexp si ll2
		     val slse = sl_sl_labstrexp se ll3
		 in case slid of
			A.StrIdDots =>
			A.StrBindOneDots ((flattenLabSigExp [slsi]) @ (flattenLabStrExp [slse]))
		      | _ => A.StrBindOneOp (slid, slsi, slse, rl, l, n)
		 end
	  | sl_strbindone (A.StrBindOneTr (id, si, se, rl, l, n)) ll =
	    if isin l ll
	    then let val nxt1 = A.getStrIdNext id
		     val nxt2 = A.getLabSigExpNext si
		     val (ll1, ll') = splitList nxt1 (L.delete l ll)
		     val (ll2, ll3) = splitList nxt2 ll'
		     val slid = sl_sl_strid     id ll1
		     val slsi = sl_sl_labsigexp si ll2
		     val slse = sl_sl_labstrexp se ll3
		 in A.StrBindOneTr (slid, slsi, slse, rl, l, n)
		 end
	    else let val nxt1 = A.getStrIdNext id
		     val nxt2 = A.getLabSigExpNext si
		     val (ll1, ll') = splitList nxt1 ll
		     val (ll2, ll3) = splitList nxt2 ll'
		     val slid = sl_sl_strid     id ll1
		     val slsi = sl_sl_labsigexp si ll2
		     val slse = sl_sl_labstrexp se ll3
		 in case slid of
			A.StrIdDots =>
			A.StrBindOneDots ((flattenLabSigExp [slsi]) @ (flattenLabStrExp [slse]))
		      | _ => A.StrBindOneTr (slid, slsi, slse, rl, l, n)
		 end
	  | sl_strbindone (A.StrBindOne (id, se, r, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getStrIdNext id
		     val (ll1, ll2) = splitList nxt (L.delete l ll)
		     val slid = sl_sl_strid  id ll1
		     val slse = sl_sl_labstrexp se ll2
		 in A.StrBindOne (slid, slse, r, l, n)
		 end
	    else let val nxt = A.getStrIdNext id
		     val (ll1, ll2) = splitList nxt ll
		     val slid = sl_sl_strid  id ll1
		     val slse = sl_sl_labstrexp se ll2
		 in case slid of
			A.StrIdDots => A.StrBindOneDots (flattenLabStrExp [slse])
		      | _ => A.StrBindOne (slid, slse, r, l, n)
		 end
	  | sl_strbindone (A.StrBindOneDots pl) ll = A.StrBindOneDots (sl_partlist pl ll)

	and sl_sl_ltreadescone x ll =
	    if isEmpty ll
	    then A.LTReaDOneDots []
	    else sl_ltreadescone x ll

	and sl_ltreadescone (A.LTReaDOne (dn, ty, rs, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getLDatNameNext dn
		     val (lll, llr) = splitList nxt (L.delete l ll)
		     val sldn = sl_sl_ldatname dn lll
		     val slty = sl_sl_labtype ty llr
		 in A.LTReaDOne (sldn, slty, rs, l, n)
		 end
	    else let val nxt = A.getLDatNameNext dn
		     val (lll, llr) = splitList nxt ll
		     val sldn = sl_sl_ldatname dn lll
		     val slty = sl_sl_labtype ty llr
		 in case (sldn, slty) of
			(A.LDatNameDots, A.LabTypeDots x) =>
			A.LTReaDOneDots x
		      | _ => A.LTReaDOne (sldn, slty, rs, l, n)
		 end
	  | sl_ltreadescone (A.LTReaDOneDots pl) ll =
	    A.LTReaDOneDots (sl_partlist pl ll)

	and sl_sl_ltreadesconelist x ll =
	    if isEmpty ll
	    then []
	    else sl_ltreadesconelist x ll

	and sl_ltreadesconelist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_ltreadesconelist (x :: xs) ll =
	    let val nxt = A.getLTReaDOneNext x
		val (ll1, ll2) = splitList nxt ll
		val slx = sl_sl_ltreadescone x ll1
		val slxs = sl_sl_ltreadesconelist xs ll2
	    in slx :: slxs
	    end

	and sl_sl_ltreadesc x ll =
	    if isEmpty ll
	    then A.LTReaDescDots []
	    else sl_ltreadesc x ll

	and sl_ltreadesc (A.LTReaDesc (xs, rs, l, n)) ll =
	    (case flattenLTReaDOne (sl_sl_ltreadesconelist xs (L.delete l ll)) of
		 [] => A.LTReaDescDots []
	       | [A.LTReaDOneDots x] => A.LTReaDescDots x
	       | x => A.LTReaDesc (x, rs, l, n))
	  | sl_ltreadesc (A.LTReaDescDots pl) ll =
	    A.LTReaDescDots (sl_partlist pl ll)

	(*and sl_sl_treadesc  x ll = A.TReaDescDots  []
	      and sl_treadesc     x ll = A.TReaDescDots  []*)

	and sl_sl_labsigexp x ll =
	    if isEmpty ll
	    then A.LabSigExpDots []
	    else sl_labsigexp x ll

	and sl_labsigexp (A.LabSigExp (e, rl, re, l, n)) ll =
	    if isin l ll
	    then A.LabSigExp (sl_sl_sigexp e (L.delete l ll), rl, re, l, n)
	    else A.LabSigExpDots (flattenSigExp [sl_sigexp e ll])
	  | sl_labsigexp (A.LabSigExpDots pl) ll = A.LabSigExpDots (sl_partlist pl ll)

	and sl_sl_sigexp x ll =
	    if isEmpty ll
	    then A.SigExpDots []
	    else sl_sigexp x ll

	and sl_sigexp (A.SigExpBasic (sp, rl, l, n)) ll =
	    ((*Debug.printdebug2 ("(1)" ^ L.toString ll ^ " " ^ L.printelt l);*)
	      if isin l ll
	      then ((*Debug.printdebug2 ("(B)");*)
		  A.SigExpBasic (sl_sl_spec sp (L.delete l ll), rl, l, n))
	      else ((*Debug.printdebug2 ("(NB)");*)
		  A.SigExpDots (flattenSpec [sl_spec sp ll])))
	  | sl_sigexp (A.SigExpId (id, l, n)) ll =
	    if isin l ll
	    then A.SigExpId (sl_sl_sigid id (L.delete l ll), l, n)
	    else A.SigExpDots (flattenSigId [sl_sigid id ll])
	  | sl_sigexp (A.SigExpRea (se, re, rs, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getLabSigExpNext se
		     val (ll1, ll2) = splitList nxt (L.delete l ll)
		     val slse = sl_sl_labsigexp se ll1
		     val slre = sl_sl_ltreadesc re ll2
		 in A.SigExpRea (slse, slre, rs, l, n)
		 end
	    else let val nxt = A.getLabSigExpNext se
		     val (ll1, ll2) = splitList nxt ll
		     val slse = sl_sl_labsigexp se ll1
		     val slre = sl_sl_ltreadesc re ll2
		 in case slre of
			A.LTReaDescDots pl =>
			A.SigExpDots ((flattenLabSigExp [slse]) @ pl)
		      | _ => A.SigExpRea (slse, slre, rs, l, n)
		 end
	  | sl_sigexp (A.SigExpDots pl) ll = A.SigExpDots (sl_partlist pl ll)

	and sl_sl_labstrexp x ll =
	    if isEmpty ll
	    then A.LabStrExpDots []
	    else sl_labstrexp x ll

	and sl_labstrexp (A.LabStrExp (e, rl, re, l, n)) ll =
	    if isin l ll
	    then A.LabStrExp (sl_sl_strexp e (L.delete l ll), rl, re, l, n)
	    else A.LabStrExpDots (flattenStrExp [sl_strexp e ll])
	  | sl_labstrexp (A.LabStrExpDots pl) ll = A.LabStrExpDots (sl_partlist pl ll)

	and sl_sl_strexp x ll =
	    if isEmpty ll
	    then A.StrExpDots []
	    else sl_strexp x ll

	and sl_strexp (A.StrExpBasic (sd, rl, l, n)) ll =
	    if isin l ll
	    then A.StrExpBasic (sl_sl_strdec sd (L.delete l ll), rl, l, n)
	    else A.StrExpDots (flattenStrDec [sl_strdec sd ll])
	  | sl_strexp (A.StrExpId (id, l, n)) ll =
	    if isin l ll
	    then A.StrExpId (sl_sl_longstrid id (L.delete l ll), l, n)
	    else A.StrExpDots (flattenLongStrId [sl_longstrid id ll])
	  | sl_strexp (A.StrExpOp (se, si, r, l, n)) ll =
	    if isin l ll
	    then
		let
		    val nxt = A.getLabStrExpNext se
		    val (ll1, ll2) = splitList nxt (L.delete l ll)
		    val slse = sl_sl_labstrexp se ll1
		    val slsi = sl_sl_labsigexp si ll2
		in A.StrExpOp (slse, slsi, r, l, n)
		end
	    else
		let
		    val nxt = A.getLabStrExpNext se
		    val (ll1, ll2) = splitList nxt ll
		    val slse = sl_sl_labstrexp se ll1
		    val slsi = sl_sl_labsigexp si ll2
		in A.StrExpDots ((flattenLabStrExp [slse]) @ (flattenLabSigExp [slsi]))
		end
	  | sl_strexp (A.StrExpTr (se, si, r, l, n)) ll =
	    if isin l ll
	    then
		let
		    val nxt = A.getLabStrExpNext se
		    val (ll1, ll2) = splitList nxt (L.delete l ll)
		    val slse = sl_sl_labstrexp se ll1
		    val slsi = sl_sl_labsigexp si ll2
		in A.StrExpTr (slse, slsi, r, l, n)
		end
	    else
		let
		    val nxt = A.getLabStrExpNext se
		    val (ll1, ll2) = splitList nxt ll
		    val slse = sl_sl_labstrexp se ll1
		    val slsi = sl_sl_labsigexp si ll2
		in A.StrExpDots ((flattenLabStrExp [slse]) @ (flattenLabSigExp [slsi]))
		end
	  | sl_strexp (A.StrExpFExp (id, se, rs, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getFunIdNext id
		     val (ll1, ll2) = splitList nxt (L.delete l ll)
		     val slid = sl_sl_funid     id ll1
		     val slse = sl_sl_labstrexp se ll2
		 in A.StrExpFExp (slid, slse, rs, l, n)
		 end
	    else let val nxt = A.getFunIdNext id
		     val (ll1, ll2) = splitList nxt ll
		     val slid = sl_sl_funid     id ll1
		     val slse = sl_sl_labstrexp se ll2
		 in case slid of
			A.FunIdDots => A.StrExpDots (flattenLabStrExp [slse])
		      | _ => A.StrExpFExp (slid, slse, rs, l, n)
		 end
	  | sl_strexp (A.StrExpFDec (id, sd, rs, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getFunIdNext id
		     val (ll1, ll2) = splitList nxt (L.delete l ll)
		     val slid = sl_sl_funid id ll1
		     val slsd = sl_sl_strdec sd ll2
		 in A.StrExpFDec (slid, slsd, rs, l, n)
		 end
	    else let val nxt = A.getFunIdNext id
		     val (ll1, ll2) = splitList nxt ll
		     val slid = sl_sl_funid id ll1
		     val slsd = sl_sl_strdec sd ll2
		 in case slid of
			A.FunIdDots => A.StrExpDots (flattenStrDec [slsd])
		      | _ => A.StrExpFDec (slid, slsd, rs, l, n)
		 end
	  | sl_strexp (A.StrExpLocal (sd, se, rs, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getStrDecNext sd
		     val (ll1, ll2) = splitList nxt (L.delete l ll)
		     val slsd = sl_sl_strdec    sd ll1
		     val slse = sl_sl_labstrexp se ll2
		 in A.StrExpLocal (slsd, slse, rs, l, n)
		 end
	    else let val nxt = A.getStrDecNext sd
		     val (ll1, ll2) = splitList nxt ll
		     val slsd = sl_sl_strdec    sd ll1
		     val slse = sl_sl_labstrexp se ll2
		 in A.StrExpDots ((flattenStrDec [slsd]) @ (flattenLabStrExp [slse]))
		 end
	  | sl_strexp (A.StrExpDots pl) ll = A.StrExpDots (sl_partlist pl ll)

	and sl_sl_longtyconlist x ll =
	    if isEmpty ll
	    then []
	    else sl_longtyconlist x ll

	and sl_longtyconlist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_longtyconlist (x :: xs) ll =
	    let val nxt = A.getLongTyConNext x
		val (ll1, ll2) = splitList nxt ll
		val slx = sl_sl_longtycon x ll1
		val slxs = sl_sl_longtyconlist xs ll2
	    in slx :: slxs
	    end

	and sl_sl_longtyconeq x ll =
	    if isEmpty ll
	    then A.LongTyConEqDots []
	    else sl_longtyconeq x ll

	and sl_longtyconeq (A.LongTyConEq (longtycons, regs, lab, n)) ll =
	    let val sllongtycons = sl_longtyconlist longtycons (L.delete lab ll)
	    in case sllongtycons of
		   []                      => A.LongTyConEqDots []
		 | [A.LongTyConDots parts] => A.LongTyConEqDots parts
		 | _                       => A.LongTyConEq (sllongtycons, regs, lab, n)
	    end
	  | sl_longtyconeq (A.LongTyConEqDots pl) ll = A.LongTyConEqDots (sl_partlist pl ll)

	and sl_sl_longstrideq x ll =
	    if isEmpty ll
	    then A.LongStrIdEqDots []
	    else sl_longstrideq x ll

	and sl_longstrideq (A.LongStrIdEq (longstrids, regs, n)) ll =
	    let val sllongstrids = sl_longstridlist longstrids ll
	    in case sllongstrids of
		   []                      => A.LongStrIdEqDots []
		 | [A.LongStrIdDots parts] => A.LongStrIdEqDots parts
		 | _                       => A.LongStrIdEq (sllongstrids, regs, n)
	    end
	  | sl_longstrideq (A.LongStrIdEqDots pl) ll = A.LongStrIdEqDots (sl_partlist pl ll)

	and sl_sl_specone x ll =
	    if isEmpty ll
	    then A.SpecOneDots []
	    else sl_specone x ll

	and sl_specone (A.SpecValue (vd, r, l, n)) ll =
	    if isin l ll
	    then A.SpecValue (sl_sl_valdesc vd (L.delete l ll), r, l, n)
	    else (case sl_valdesc vd ll of
		      A.ValDescDots pl => A.SpecOneDots pl
		    | svd => A.SpecValue (svd, r, l, n))
	  | sl_specone (A.SpecType (td, r, l, n)) ll =
	    if isin l ll
	    then A.SpecType (sl_sl_typdesc td (L.delete l ll), r, l, n)
	    else (case sl_typdesc td ll of
		      A.TypDescDots pl => A.SpecOneDots pl
		    | std => A.SpecType (std, r, l, n))
	  | sl_specone (A.SpecEqtype (td, r, l, n)) ll =
	    ((*Debug.printdebug2 ("(2)" ^ L.toString ll ^ " " ^ L.printelt l);*)
	      if isin l ll
	      then A.SpecEqtype (sl_sl_typdesc td (L.delete l ll), r, l, n)
	      else (case sl_typdesc td ll of
			A.TypDescDots pl => A.SpecOneDots pl
		      | std => A.SpecEqtype (std, r, l, n)))
	  | sl_specone (A.SpecException (ed, r, l, n)) ll =
	    if isin l ll
	    then A.SpecException (sl_sl_excdesc ed (L.delete l ll), r, l, n)
	    else (case sl_excdesc ed ll of
		      A.ExcDescDots pl => A.SpecOneDots pl
		    | sed => A.SpecException (sed, r, l, n))
	  | sl_specone (A.SpecTdr (td, r, l, n)) ll =
	    if isin l ll
	    then A.SpecTdr (sl_sl_tdrdesc td (L.delete l ll), r, l, n)
	    else (case sl_tdrdesc td ll of
		      A.TdrDescDots pl => A.SpecOneDots pl
		    | std => A.SpecTdr (std, r, l, n))
	  | sl_specone (A.SpecDat (dd, r, l, n)) ll =
	    if isin l ll
	    then A.SpecDat (sl_sl_datdesc dd (L.delete l ll), r, l, n)
	    else (case sl_datdesc dd ll of
		      A.DatDescDots pl => A.SpecOneDots pl
		    | sdd => A.SpecDat (sdd, r, l, n))
	  | sl_specone (A.SpecStr (sd, r, l, n)) ll =
	    if isin l ll
	    then A.SpecStr (sl_sl_strdesc sd (L.delete l ll), r, l, n)
	    else (case sl_strdesc sd ll of
		      A.StrDescDots pl => A.SpecOneDots pl
		    | slsd => A.SpecStr (slsd, r, l, n))
	  | sl_specone (A.SpecInc (si, r, l, n)) ll =
	    if isin l ll
	    then A.SpecInc (sl_sl_labsigexp si (L.delete l ll), r, l, n)
	    else (case sl_labsigexp si ll of
		      A.LabSigExpDots pl => A.SpecOneDots pl
		    | slsi => A.SpecInc (slsi, r, l, n))
	  | sl_specone (A.SpecIsi (si, r, l, n)) ll =
	    if isin l ll
	    then A.SpecIsi (sl_sl_sigidseq si (L.delete l ll), r, l, n)
	    else (case sl_sigidseq si ll of
		      A.SigIdSeqDots pl => A.SpecOneDots pl
		    | slsi => A.SpecIsi (slsi, r, l, n))
	  | sl_specone (A.SpecRep (tc, ltc, rs, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getTyConNext tc
		     val (ll1, ll2) = splitList nxt (L.delete l ll)
		     val sltc  = sl_sl_tycon tc ll1
		     val slltc = sl_sl_longtycon ltc ll2
		 in A.SpecRep (sltc, slltc, rs, l, n)
		 end
	    else let val nxt = A.getTyConNext tc
		     val (ll1, ll2) = splitList nxt ll
		     val sltc  = sl_sl_tycon tc ll1
		     val slltc = sl_sl_longtycon ltc ll2
		 in case sltc of
			A.TyConDots => A.SpecOneDots (flattenLongTyCon [slltc])
		      | _ => A.SpecRep (sltc, slltc, rs, l, n)
		 end
	  | sl_specone (A.SpecSha (sp, tce, rs, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getSpecNext sp
		     val (ll1, ll2) = splitList nxt (L.delete l ll)
		     val slsp  = sl_sl_spec sp ll1
		     val sltce = sl_sl_longtyconeq tce ll2
		 in A.SpecSha (slsp, sltce, rs, l, n)
		 end
	    else let val nxt = A.getSpecNext sp
		     val (ll1, ll2) = splitList nxt ll
		     val slsp  = sl_sl_spec sp ll1
		     val sltce = sl_sl_longtyconeq tce ll2
		 in A.SpecOneDots (flattenSpec [slsp] @
				   flattenLongTyConEq [sltce])
		 end
	  | sl_specone (A.SpecSsi (sp, tsi, rs, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getSpecNext sp
		     val (ll1, ll2) = splitList nxt (L.delete l ll)
		     val slsp  = sl_sl_spec sp ll1
		     val sltsi = sl_sl_longstrideq tsi ll2
		 in A.SpecSsi (slsp, sltsi, rs, l, n)
		 end
	    else let val nxt = A.getSpecNext sp
		     val (ll1, ll2) = splitList nxt ll
		     val slsp  = sl_sl_spec sp ll1
		     val sltsi = sl_sl_longstrideq tsi ll2
		 in A.SpecOneDots (flattenSpec [slsp] @
				   flattenLongStrIdEq [sltsi])
		 end
	  | sl_specone (A.SpecOneDots pl) ll =
	    A.SpecOneDots (sl_partlist pl ll)

	and sl_sl_speconelist x ll =
	    if isEmpty ll
	    then []
	    else sl_speconelist x ll

	and sl_speconelist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_speconelist (x :: xs) ll =
	    let val nxt = A.getSpecOneNext x
		val (ll1, ll2) = splitList nxt ll
		val slx = sl_sl_specone x ll1
		val slxs = sl_sl_speconelist xs ll2
	    in slx :: slxs
	    end

	and sl_sl_spec x ll =
	    if isEmpty ll
	    then A.SpecDots []
	    else sl_spec x ll

	and sl_spec (A.Spec (spol, n)) ll =
	    A.SpecDots (flattenSpecOne (sl_speconelist spol ll))
	  | sl_spec (A.SpecDots pl) ll =
	    A.SpecDots (sl_partlist pl ll)

	and sl_sl_strdesc x ll =
	    if isEmpty ll
	    then A.StrDescDots []
	    else sl_strdesc x ll

	and sl_strdesc (A.StrDesc (sdol, rl, n)) ll =
	    (case flattenStrDescOne (sl_strdesconelist sdol ll) of
		 [] => A.StrDescDots [] (* NEW - related to strictLab *)
	       | [A.StrDescOneDots pl] => A.StrDescDots pl
	       | x => A.StrDesc (x, rl, n))
	  | sl_strdesc (A.StrDescDots pl) ll =
	    A.StrDescDots (sl_partlist pl ll)

	and sl_sl_strdesconelist x ll =
	    if isEmpty ll
	    then []
	    else sl_strdesconelist x ll

	and sl_strdesconelist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_strdesconelist (x :: xs) ll =
	    let val nxt = A.getStrDescOneNext x
		val (ll1, ll2) = splitList nxt ll
		val slx = sl_sl_strdescone x ll1
		val slxs = sl_sl_strdesconelist xs ll2
	    in slx :: slxs
	    end

	and sl_sl_strdescone x ll =
	    if isEmpty ll
	    then A.StrDescOneDots []
	    else sl_strdescone x ll

	and sl_strdescone (A.StrDescOne (id, se, r, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getStrIdNext id
		     val (ll1, ll2) = splitList nxt (L.delete l ll)
		     val slid = sl_sl_strid     id ll1
		     val slse = sl_sl_labsigexp se ll2
		 in A.StrDescOne (slid, slse, r, l, n)
		 end
	    else let val nxt = A.getStrIdNext id
		     val (ll1, ll2) = splitList nxt ll
		     val slid = sl_sl_strid     id ll1
		     val slse = sl_sl_labsigexp se ll2
		 in case slid of
			A.StrIdDots => A.StrDescOneDots (flattenLabSigExp [slse])
		      | _ => A.StrDescOne (slid, slse, r, l, n)
		 end
	  | sl_strdescone (A.StrDescOneDots pl) ll =
	    A.StrDescOneDots (sl_partlist pl ll)

	and sl_sl_condescone x ll =
	    if isEmpty ll
	    then A.ConDescOneDots []
	    else sl_condescone x ll

	and sl_condescone (A.ConDescOneId (id, n)) ll =
	    (case sl_ident id ll of
		 A.IdentDots => A.ConDescOneDots []
	       | slid          => A.ConDescOneId (slid, n))
	  | sl_condescone (A.ConDescOneOf (id, ty, r, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getLabIdNext id
		     val (lll, llr) = splitList nxt (L.delete l ll)
		     val slid = sl_sl_labid id lll
		     val slty = sl_sl_labtype ty llr
		 in  A.ConDescOneOf (slid, slty, r, l, n)
		 end
	    else let val nxt = A.getLabIdNext id
		     val (lll, llr) = splitList nxt ll
		     val slid = sl_sl_labid id lll
		     val slty = sl_sl_labtype ty llr
		 in case (slid, slty) of
			(A.LabIdDots [(A.PartLgid (A.LongIdId id))],
			 A.LabTypeDots []) => A.ConDescOneNoOf (id, n)
		      | _ => A.ConDescOneDots ((flattenLabId [slid]) @ (flattenLabType [slty]))
		 end
	  | sl_condescone (A.ConDescOneNoOf (id, n)) ll =
	    (case sl_ident id ll of
		 A.IdentDots => A.ConDescOneDots []
	       | slid          => A.ConDescOneNoOf (slid, n))
	  | sl_condescone (A.ConDescOneDots pl) ll = A.ConDescOneDots (sl_partlist pl ll)

	and sl_sl_condesconelist x ll =
	    if isEmpty ll
	    then []
	    else sl_condesconelist x ll

	and sl_condesconelist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_condesconelist (x :: xs) ll =
	    let val nxt = A.getConDescOneNext x
		val (lll, llr) = splitList nxt ll
		val slx = sl_sl_condescone x lll
		val slxs = sl_sl_condesconelist xs llr
	    in slx :: slxs
	    end

	and sl_sl_condesc x ll =
	    if isEmpty ll
	    then A.ConDescDots []
	    else sl_condesc x ll

	and sl_condesc (A.ConDesc (xs, rl, n)) ll =
	    let val slxs = flattenConDescOne (sl_condesconelist xs ll)
	    in case slxs of
		   [] => A.ConDescDots [] (* NEW - related to strictLab *)
		 | [A.ConDescOneDots x] => A.ConDescDots x
		 | _ => A.ConDesc (slxs, rl, n)
	    end
	  | sl_condesc (A.ConDescDots pl) ll = A.ConDescDots (sl_partlist pl ll)

	and sl_sl_datdesc x ll =
	    if isEmpty ll
	    then A.DatDescDots []
	    else sl_datdesc x ll

	and sl_datdesc (A.DatDesc (ddol, rl, n)) ll =
	    (case flattenDatDescOne (sl_datdesconelist ddol ll) of
		 [] => A.DatDescDots [] (* NEW - related to strictLab *)
	       | [A.DatDescOneDots pl] => A.DatDescDots pl
	       | x => A.DatDesc (x, rl, n))
	  | sl_datdesc (A.DatDescDots pl) ll =
	    A.DatDescDots (sl_partlist pl ll)

	and sl_sl_datdesconelist x ll =
	    if isEmpty ll
	    then []
	    else sl_datdesconelist x ll

	and sl_datdesconelist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_datdesconelist (x :: xs) ll =
	    let
		val nxt = A.getDatDescOneNext x
		val (ll1, ll2) = splitList nxt ll
		val slx = sl_sl_datdescone x ll1
		val slxs = sl_sl_datdesconelist xs ll2
	    in slx :: slxs
	    end

	and sl_sl_datdescone x ll =
	    if isEmpty ll
	    then A.DatDescOneDots []
	    else sl_datdescone x ll

	and sl_datdescone (A.DatDescOne (dn, cd, r, l, n)) ll =
	    if isin l ll
	    then
		let
		    val nxt = A.getDatNameNext dn
		    val (ll1, ll2) = splitList nxt (L.delete l ll)
		    val sldn = sl_sl_datname dn ll1
		    val slcd = sl_sl_condesc cd ll2
		in A.DatDescOne (sldn, slcd, r, l, n)
		end
	    else
		let
		    val nxt = A.getDatNameNext dn
		    val (ll1, ll2) = splitList nxt ll
		    val sldn = sl_sl_datname dn ll1
		    val slcd = sl_sl_condesc cd ll2
		in case (sldn, slcd) of
		       (A.DatNameDots, A.ConDescDots x) => A.DatDescOneDots x
		     | _                                    => A.DatDescOne (sldn, slcd, r, l, n)
		end
	  | sl_datdescone (A.DatDescOneDots pl) ll =
	    A.DatDescOneDots (sl_partlist pl ll)

	and sl_sl_valdesc x ll =
	    if isEmpty ll
	    then A.ValDescDots []
	    else sl_valdesc x ll

	and sl_valdesc (A.ValDesc (vdol, rl, n)) ll =
	    (case flattenValDescOne (sl_valdesconelist vdol ll) of
		 [] => A.ValDescDots [] (* NEW - related to strictLab *)
	       | [A.ValDescOneDots pl] => A.ValDescDots pl
	       | x => A.ValDesc (x, rl, n))
	  | sl_valdesc (A.ValDescDots pl) ll =
	    A.ValDescDots (sl_partlist pl ll)

	and sl_sl_valdesconelist x ll =
	    if isEmpty ll
	    then []
	    else sl_valdesconelist x ll

	and sl_valdesconelist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_valdesconelist (x :: xs) ll =
	    let
		val nxt = A.getValDescOneNext x
		val (ll1, ll2) = splitList nxt ll
		val slx = sl_sl_valdescone x ll1
		val slxs = sl_sl_valdesconelist xs ll2
	    in slx :: slxs
	    end

	and sl_sl_valdescone x ll =
	    if isEmpty ll
	    then A.ValDescOneDots []
	    else sl_valdescone x ll

	and sl_valdescone (A.ValDescOne (id, ty, r, l, n)) ll =
	    if isin l ll
	    then
		let
		    val nxt = A.getLabIdNext id
		    val (ll1, ll2) = splitList nxt (L.delete l ll)
		    val slid = sl_sl_labid   id ll1
		    val slty = sl_sl_labtype ty ll2
		in A.ValDescOne (slid, slty, r, l, n)
		end
	    else
		let
		    val nxt = A.getLabIdNext id
		    val (ll1, ll2) = splitList nxt ll
		    val slid = sl_sl_labid   id ll1
		    val slty = sl_sl_labtype ty ll2
		in case slid of
		       A.LabIdDots [] => A.ValDescOneDots (flattenLabType [slty])
		     | _ => A.ValDescOne (slid, slty, r, l, n)
		end
	  | sl_valdescone (A.ValDescOneDots pl) ll =
	    A.ValDescOneDots (sl_partlist pl ll)

	and sl_sl_typdesc x ll =
	    if isEmpty ll
	    then A.TypDescDots []
	    else sl_typdesc x ll

	and sl_typdesc (A.TypDesc (tdol, rl, n)) ll =
	    (case flattenTypDescOne (sl_typdesconelist tdol ll) of
		 [] => A.TypDescDots [] (* NEW - related to strictLab *)
	       | [A.TypDescOneDots pl] => A.TypDescDots pl
	       | x => A.TypDesc (x, rl, n))
	  | sl_typdesc (A.TypDescDots pl) ll =
	    A.TypDescDots (sl_partlist pl ll)

	and sl_sl_typdesconelist x ll =
	    if isEmpty ll
	    then []
	    else sl_typdesconelist x ll

	and sl_typdesconelist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_typdesconelist (x :: xs) ll =
	    let
		val nxt = A.getTypDescOneNext x
		val (ll1, ll2) = splitList nxt ll
		val slx = sl_sl_typdescone x ll1
		val slxs = sl_sl_typdesconelist xs ll2
	    in slx :: slxs
	    end

	and sl_sl_typdescone x ll =
	    if isEmpty ll
	    then A.TypDescOneDots []
	    else sl_typdescone x ll

	and sl_typdescone (A.TypDescOne (dn, l, n)) ll =
	    if isin l ll
	    then A.TypDescOne (sl_sl_datname dn (L.delete l ll), l, n)
	    else (case sl_datname dn ll of
		      A.DatNameDots => A.TypDescOneDots []
		    | x => A.TypDescOne (x, l, n))
	  | sl_typdescone (A.TypDescOneDots pl) ll =
	    A.TypDescOneDots (sl_partlist pl ll)

	and sl_sl_excdesc x ll =
	    if isEmpty ll
	    then A.ExcDescDots []
	    else sl_excdesc x ll

	and sl_excdesc (A.ExcDesc (edol, rl, n)) ll =
	    (case flattenExcDescOne (sl_excdesconelist edol ll) of
		 [] => A.ExcDescDots [] (* NEW - related to strictLab *)
	       | [A.ExcDescOneDots pl] => A.ExcDescDots pl
	       | x => A.ExcDesc (x, rl, n))
	  | sl_excdesc (A.ExcDescDots pl) ll =
	    A.ExcDescDots (sl_partlist pl ll)

	and sl_sl_excdesconelist x ll =
	    if isEmpty ll
	    then []
	    else sl_excdesconelist x ll

	and sl_excdesconelist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_excdesconelist (x :: xs) ll =
	    let
		val nxt = A.getExcDescOneNext x
		val (ll1, ll2) = splitList nxt ll
		val slx = sl_sl_excdescone x ll1
		val slxs = sl_sl_excdesconelist xs ll2
	    in slx :: slxs
	    end

	and sl_sl_excdescone x ll =
	    if isEmpty ll
	    then A.ExcDescOneDots []
	    else sl_excdescone x ll

	and sl_excdescone (A.ExcDescOne (id, l, n)) ll =
	    if isin l ll
	    then A.ExcDescOne (sl_sl_ident id (L.delete l ll), l, n)
	    else A.ExcDescOneDots (flattenIdent [sl_ident id ll])
	  | sl_excdescone (A.ExcDescOf (id, ty, r, l, n)) ll =
	    if isin l ll
	    then
		let
		    val nxt = A.getLabIdNext id
		    val (ll1, ll2) = splitList nxt (L.delete l ll)
		    val slid = sl_sl_labid   id ll1
		    val slty = sl_sl_labtype ty ll2
		in A.ExcDescOf (slid, slty, r, l, n)
		end
	    else
		let
		    val nxt = A.getLabIdNext id
		    val (ll1, ll2) = splitList nxt ll
		    val slid = sl_sl_labid   id ll1
		    val slty = sl_sl_labtype ty ll2
		in A.ExcDescOneDots ((flattenLabId [slid]) @ (flattenLabType [slty]))
		end
	  | sl_excdescone (A.ExcDescOneDots pl) ll =
	    A.ExcDescOneDots (sl_partlist pl ll)

	and sl_sl_tdrdesc x ll =
	    if isEmpty ll
	    then A.TdrDescDots []
	    else sl_tdrdesc x ll

	and sl_tdrdesc (A.TdrDesc (tdol, rl, n)) ll =
	    (case flattenTdrDescOne (sl_tdrdesconelist tdol ll) of
		 [] => A.TdrDescDots [] (* NEW - related to strictLab *)
	       | [A.TdrDescOneDots pl] => A.TdrDescDots pl
	       | x => A.TdrDesc (x, rl, n))
	  | sl_tdrdesc (A.TdrDescDots pl) ll =
	    A.TdrDescDots (sl_partlist pl ll)

	and sl_sl_tdrdesconelist x ll =
	    if isEmpty ll
	    then []
	    else sl_tdrdesconelist x ll

	and sl_tdrdesconelist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_tdrdesconelist (x :: xs) ll =
	    let
		val nxt = A.getTdrDescOneNext x
		val (ll1, ll2) = splitList nxt ll
		val slx = sl_sl_tdrdescone x ll1
		val slxs = sl_sl_tdrdesconelist xs ll2
	    in slx :: slxs
	    end

	and sl_sl_tdrdescone x ll =
	    if isEmpty ll
	    then A.TdrDescOneDots []
	    else sl_tdrdescone x ll

	and sl_tdrdescone (A.TdrDescOne (dn, ty, r, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getDatNameNext dn
		     val (ll1, ll2) = splitList nxt (L.delete l ll)
		     val sldn = sl_sl_datname dn ll1
		     val slty = sl_sl_labtype ty ll2
		 in A.TdrDescOne (sldn, slty, r, l, n)
		 end
	    else let val nxt = A.getDatNameNext dn
		     val (ll1, ll2) = splitList nxt ll
		     val sldn = sl_sl_datname dn ll1
		     val slty = sl_sl_labtype ty ll2
		 in case (sldn, slty) of
			(A.DatNameDots, A.LabTypeDots dots) => A.TdrDescOneDots dots
		      | _ => A.TdrDescOne (sldn, slty, r, l, n)
		 end
	  | sl_tdrdescone (A.TdrDescOneDots pl) ll =
	    A.TdrDescOneDots (sl_partlist pl ll)

	and sl_sl_longstrid x ll =
	    if isEmpty ll
	    then A.LongStrIdDots []
	    else sl_longstrid x ll

	and sl_longstrid (A.LongStrIdQual (id, lid, r, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getStrIdNext id
		     val (ll1, ll2) = splitList nxt (L.delete l ll)
		     val slid  = sl_sl_strid     id  ll1
		     val sllid = sl_sl_longstrid lid ll2
		 in A.LongStrIdQual (slid, sllid, r, l, n)
		 end
	    else let val nxt = A.getStrIdNext id
		     val (ll1, ll2) = splitList nxt ll
		     val slid  = sl_sl_strid     id  ll1
		     val sllid = sl_sl_longstrid lid ll2
		 in case slid of
			A.StrIdDots => A.LongStrIdDots (flattenLongStrId [sllid])
		      | _ => A.LongStrIdQual (slid, sllid, r, l, n)
		 end
	  | sl_longstrid (A.LongStrIdId id) ll =
	    (case sl_strid id ll of
		 A.StrIdDots => A.LongStrIdDots []
	       | x => A.LongStrIdId x)
	  | sl_longstrid (A.LongStrIdDots pl) ll =
	    A.LongStrIdDots (sl_partlist pl ll)

	(* CHECK THE STUFF FOR FUNCTORS *)
	(*and sl_sl_fundec x ll =
		  if isEmpty ll
		  then A.FunDecDots []
		  else sl_fundec x ll

	      and sl_fundec (A.FunDec (sb, r, l, n)) ll =
		  (case sl_funbind sb (L.delete l ll) of (* TODO: CHANGE THAT!! *)
		       A.FunBindDots x => A.FunDecDots x
		     | x => A.FunDec (x, r, l, n))
		| sl_fundec (A.FunDecDots pl) ll = A.FunDecDots (sl_partlist pl ll)*)

	and sl_sl_funbind x ll =
	    if isEmpty ll
	    then A.FunBindDots []
	    else sl_funbind x ll

	and sl_funbind (A.FunBind (fbol, rl, n)) ll =
	    (case flattenFunBindOne (sl_funbindonelist fbol ll) of
		 [] => A.FunBindDots [] (* NEW - related to strictLab *)
	       | [A.FunBindODots pl] => A.FunBindDots pl
	       | x => A.FunBind (x, rl, n))
	  | sl_funbind (A.FunBindDots pl) ll = A.FunBindDots (sl_partlist pl ll)

	and sl_sl_funbindone x ll =
	    if isEmpty ll
	    then A.FunBindODots []
	    else sl_funbindone x ll

	and sl_funbindone (A.FunBindO (fid, sid, si, se, rs, l, n)) ll =
	    if isin l ll
	    then let val nxt1 = A.getFunIdNext     fid
		     val nxt2 = A.getStrIdNext     sid
		     val nxt3 = A.getLabSigExpNext si
		     val (ll1, ll')  = splitList nxt1 (L.delete l ll)
		     val (ll2, ll'') = splitList nxt2 ll'
		     val (ll3, ll4)  = splitList nxt3 ll''
		     val slfid = sl_sl_funid     fid ll1
		     val slsid = sl_sl_strid     sid ll2
		     val slsi  = sl_sl_labsigexp si  ll3
		     val slse  = sl_sl_labstrexp se  ll4
		 in A.FunBindO (slfid, slsid, slsi, slse, rs, l, n)
		 end
	    else let val nxt1 = A.getFunIdNext     fid
		     val nxt2 = A.getStrIdNext     sid
		     val nxt3 = A.getLabSigExpNext si
		     val (ll1, ll')  = splitList nxt1 ll
		     val (ll2, ll'') = splitList nxt2 ll'
		     val (ll3, ll4)  = splitList nxt3 ll''
		     val slfid = sl_sl_funid     fid ll1
		     val slsid = sl_sl_strid     sid ll2
		     val slsi  = sl_sl_labsigexp si  ll3
		     val slse  = sl_sl_labstrexp se  ll4
		 in case (slfid, slsid) of
			(A.FunIdDots, A.StrIdDots) =>
			A.FunBindODots ((flattenLabSigExp [slsi]) @ (flattenLabStrExp [slse]))
		      | _ => A.FunBindO (slfid, slsid, slsi, slse, rs, l, n)
		 end
	  | sl_funbindone (A.FunBindOO (fid, sid, si, si', se, rs, l, n)) ll =
	    if isin l ll
	    then let val nxt1 = A.getFunIdNext     fid
		     val nxt2 = A.getStrIdNext     sid
		     val nxt3 = A.getLabSigExpNext si
		     val nxt4 = A.getLabSigExpNext si'
		     val (ll1, ll')    = splitList nxt1 (L.delete l ll)
		     val (ll2, ll'')   = splitList nxt2 ll'
		     val (ll3, ll''')  = splitList nxt3 ll''
		     val (ll4, ll5)    = splitList nxt4 ll'''
		     val slfid = sl_sl_funid     fid ll1
		     val slsid = sl_sl_strid     sid ll2
		     val slsi  = sl_sl_labsigexp si  ll3
		     val slsi' = sl_sl_labsigexp si' ll4
		     val slse  = sl_sl_labstrexp se  ll5
		 in A.FunBindOO (slfid, slsid, slsi, slsi', slse, rs, l, n)
		 end
	    else let val nxt1 = A.getFunIdNext     fid
		     val nxt2 = A.getStrIdNext     sid
		     val nxt3 = A.getLabSigExpNext si
		     val nxt4 = A.getLabSigExpNext si'
		     val (ll1, ll')   = splitList nxt1 ll
		     val (ll2, ll'')  = splitList nxt2 ll'
		     val (ll3, ll''') = splitList nxt3 ll''
		     val (ll4, ll5)   = splitList nxt4 ll'''
		     val slfid = sl_sl_funid     fid ll1
		     val slsid = sl_sl_strid     sid ll2
		     val slsi  = sl_sl_labsigexp si  ll3
		     val slsi' = sl_sl_labsigexp si' ll4
		     val slse  = sl_sl_labstrexp se  ll5
		 in case (slfid, slsid) of
			(A.FunIdDots, A.StrIdDots) =>
			A.FunBindODots ((flattenLabSigExp [slsi, slsi']) @ (flattenLabStrExp [slse]))
		      | _ => A.FunBindOO (slfid, slsid, slsi, slsi', slse, rs, l, n)
		 end
	  | sl_funbindone (A.FunBindOT (fid, sid, si, si', se, rs, l, n)) ll =
	    if isin l ll
	    then let val nxt1 = A.getFunIdNext     fid
		     val nxt2 = A.getStrIdNext     sid
		     val nxt3 = A.getLabSigExpNext si
		     val nxt4 = A.getLabSigExpNext si'
		     val (ll1, ll')    = splitList nxt1 (L.delete l ll)
		     val (ll2, ll'')   = splitList nxt2 ll'
		     val (ll3, ll''')  = splitList nxt3 ll''
		     val (ll4, ll5)    = splitList nxt4 ll'''
		     val slfid = sl_sl_funid     fid ll1
		     val slsid = sl_sl_strid     sid ll2
		     val slsi  = sl_sl_labsigexp si  ll3
		     val slsi' = sl_sl_labsigexp si' ll4
		     val slse  = sl_sl_labstrexp se  ll5
		 in A.FunBindOT (slfid, slsid, slsi, slsi', slse, rs, l, n)
		 end
	    else let val nxt1 = A.getFunIdNext     fid
		     val nxt2 = A.getStrIdNext     sid
		     val nxt3 = A.getLabSigExpNext si
		     val nxt4 = A.getLabSigExpNext si'
		     val (ll1, ll')   = splitList nxt1 ll
		     val (ll2, ll'')  = splitList nxt2 ll'
		     val (ll3, ll''') = splitList nxt3 ll''
		     val (ll4, ll5)   = splitList nxt4 ll'''
		     val slfid = sl_sl_funid     fid ll1
		     val slsid = sl_sl_strid     sid ll2
		     val slsi  = sl_sl_labsigexp si  ll3
		     val slsi' = sl_sl_labsigexp si' ll4
		     val slse  = sl_sl_labstrexp se  ll5
		 in case (slfid, slsid) of
			(A.FunIdDots, A.StrIdDots) =>
			A.FunBindODots ((flattenLabSigExp [slsi, slsi']) @ (flattenLabStrExp [slse]))
		      | _ => A.FunBindOT (slfid, slsid, slsi, slsi', slse, rs, l, n)
		 end
	  | sl_funbindone (A.FunBindOS (fid, sp, se, rs, l, n)) ll =
	    if isin l ll
	    then let val nxt1 = A.getFunIdNext  fid
		     val nxt2 = A.getSpecNext   sp
		     val (ll1, ll') = splitList nxt1 (L.delete l ll)
		     val (ll2, ll3) = splitList nxt2 ll'
		     val slfid = sl_sl_funid     fid ll1
		     val slsp  = sl_sl_spec      sp  ll2
		     val slse  = sl_sl_labstrexp se  ll3
		 in A.FunBindOS (slfid, slsp, slse, rs, l, n)
		 end
	    else let val nxt1 = A.getFunIdNext  fid
		     val nxt2 = A.getSpecNext   sp
		     val (ll1, ll') = splitList nxt1 ll
		     val (ll2, ll3) = splitList nxt2 ll'
		     val slfid = sl_sl_funid     fid ll1
		     val slsp  = sl_sl_spec      sp  ll2
		     val slse  = sl_sl_labstrexp se  ll3
		 in case slfid of
			A.FunIdDots =>
			A.FunBindODots ((flattenSpec [slsp]) @ (flattenLabStrExp [slse]))
		      | _ => A.FunBindOS (slfid, slsp, slse, rs, l, n)
		 end
	  | sl_funbindone (A.FunBindOSO (fid, sp, si, se, rs, l, n)) ll =
	    if isin l ll
	    then let val nxt1 = A.getFunIdNext     fid
		     val nxt2 = A.getSpecNext      sp
		     val nxt3 = A.getLabSigExpNext si
		     val (ll1, ll')  = splitList nxt1 (L.delete l ll)
		     val (ll2, ll'') = splitList nxt2 ll'
		     val (ll3, ll4)  = splitList nxt3 ll''
		     val slfid = sl_sl_funid     fid ll1
		     val slsp  = sl_sl_spec      sp  ll2
		     val slsi  = sl_sl_labsigexp si  ll3
		     val slse  = sl_sl_labstrexp se  ll4
		 in A.FunBindOSO (slfid, slsp, slsi, slse, rs, l, n)
		 end
	    else let val nxt1 = A.getFunIdNext     fid
		     val nxt2 = A.getSpecNext      sp
		     val nxt3 = A.getLabSigExpNext si
		     val (ll1, ll')  = splitList nxt1 ll
		     val (ll2, ll'') = splitList nxt2 ll'
		     val (ll3, ll4)  = splitList nxt3 ll''
		     val slfid = sl_sl_funid     fid ll1
		     val slsp  = sl_sl_spec      sp  ll2
		     val slsi  = sl_sl_labsigexp si  ll3
		     val slse  = sl_sl_labstrexp se  ll4
		 in case slfid of
			A.FunIdDots =>
			A.FunBindODots ((flattenSpec [slsp]) @ (flattenLabSigExp [slsi]) @ (flattenLabStrExp [slse]))
		      | _ => A.FunBindOSO (slfid, slsp, slsi, slse, rs, l, n)
		 end
	  | sl_funbindone (A.FunBindOST (fid, sp, si, se, rs, l, n)) ll =
	    if isin l ll
	    then let val nxt1 = A.getFunIdNext     fid
		     val nxt2 = A.getSpecNext      sp
		     val nxt3 = A.getLabSigExpNext si
		     val (ll1, ll')  = splitList nxt1 (L.delete l ll)
		     val (ll2, ll'') = splitList nxt2 ll'
		     val (ll3, ll4)  = splitList nxt3 ll''
		     val slfid = sl_sl_funid     fid ll1
		     val slsp  = sl_sl_spec      sp  ll2
		     val slsi  = sl_sl_labsigexp si  ll3
		     val slse  = sl_sl_labstrexp se  ll4
		 in A.FunBindOST (slfid, slsp, slsi, slse, rs, l, n)
		 end
	    else let val nxt1 = A.getFunIdNext     fid
		     val nxt2 = A.getSpecNext      sp
		     val nxt3 = A.getLabSigExpNext si
		     val (ll1, ll')  = splitList nxt1 ll
		     val (ll2, ll'') = splitList nxt2 ll'
		     val (ll3, ll4)  = splitList nxt3 ll''
		     val slfid = sl_sl_funid     fid ll1
		     val slsp  = sl_sl_spec      sp  ll2
		     val slsi  = sl_sl_labsigexp si  ll3
		     val slse  = sl_sl_labstrexp se  ll4
		 in case slfid of
			A.FunIdDots =>
			A.FunBindODots ((flattenSpec [slsp]) @ (flattenLabSigExp [slsi]) @ (flattenLabStrExp [slse]))
		      | _ => A.FunBindOST (slfid, slsp, slsi, slse, rs, l, n)
		 end
	  | sl_funbindone (A.FunBindODots pl) ll = A.FunBindODots (sl_partlist pl ll)

	and sl_sl_funbindonelist x ll =
	    if isEmpty ll
	    then []
	    else sl_funbindonelist x ll

	and sl_funbindonelist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_funbindonelist (x :: xs) ll =
	    let val nxt = A.getFunBONext x
		val (lll, llr) = splitList nxt ll
		val slx  = sl_sl_funbindone x lll
		val slxs = sl_sl_funbindonelist xs llr
	    in slx :: slxs
	    end

	and sl_sl_sigdec x ll =
	    if isEmpty ll
	    then A.SigDecDots []
	    else sl_sigdec x ll

	and sl_sigdec (A.SigDec (sb, r, n)) ll =
	    (case sl_sigbind sb ll of
		 A.SigBindDots x => A.SigDecDots x
	       | x => A.SigDec (x, r, n))
	  | sl_sigdec (A.SigDecDots pl) ll = A.SigDecDots (sl_partlist pl ll)

	and sl_sl_sigbind x ll =
	    if isEmpty ll
	    then A.SigBindDots []
	    else sl_sigbind x ll

	and sl_sigbind (A.SigBind (sbol, rl, n)) ll =
	    (case flattenSigBindOne (sl_sigbindonelist sbol ll) of
		 [] => A.SigBindDots [] (* NEW - related to strictLab *)
	       | [A.SigBindOneDots pl] => A.SigBindDots pl
	       | x => A.SigBind (x, rl, n))
	  | sl_sigbind (A.SigBindDots pl) ll = A.SigBindDots (sl_partlist pl ll)

	and sl_sl_sigbindone x ll =
	    if isEmpty ll
	    then A.SigBindOneDots []
	    else sl_sigbindone x ll

	and sl_sigbindone (A.SigBindOne (id, se, r, l, n)) ll =
	    if isin l ll
	    then let val nxt = A.getSigIdNext id
		     val (ll1, ll2) = splitList nxt (L.delete l ll)
		     val slid = sl_sl_sigid     id ll1
		     val slse = sl_sl_labsigexp se ll2
		 in A.SigBindOne (slid, slse, r, l, n)
		 end
	    else let val nxt = A.getSigIdNext id
		     val (ll1, ll2) = splitList nxt ll
		     val slid = sl_sl_sigid     id ll1
		     val slse = sl_sl_labsigexp se ll2
		 in case slid of
			A.SigIdDots => A.SigBindOneDots (flattenLabSigExp [slse])
		      | _ => A.SigBindOne (slid, slse, r, l, n)
		 end
	  | sl_sigbindone (A.SigBindOneDots pl) ll = A.SigBindOneDots (sl_partlist pl ll)

	and sl_sl_sigbindonelist x ll =
	    if isEmpty ll
	    then []
	    else sl_sigbindonelist x ll

	and sl_sigbindonelist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_sigbindonelist (x :: xs) ll =
	    let
		val nxt = A.getSigBONext x
		val (lll, llr) = splitList nxt ll
		val slx = sl_sl_sigbindone x lll
		val slxs = sl_sl_sigbindonelist xs llr
	    in slx :: slxs
	    end

	and sl_sl_afile x ll =
	    if isEmpty ll
	    then A.AFileDots
	    else sl_afile x ll

	and sl_afile (A.AFile (f, r, l, n)) ll =
	    if isinone l ll
	    then A.AFile (f, r, l, n)
	    else if strictLab
	    then raise EH.DeadBranch (msgOne l ll)
	    else A.AFileDots
	  | sl_afile A.AFileDots ll =
	    if isEmptyL ll
	    then A.AFileDots
	    else raise EH.DeadBranch (msgEmpty ll)

	and sl_abool(A.ABool(b, r, l, n)) ll =
	    if isinone l ll
	    then A.ABool (b, r, l, n)
	    else if strictLab
	    then raise EH.DeadBranch (msgOne l ll)
	    else A.ABoolDots
	  | sl_abool A.ABoolDots ll =
	    if isEmptyL ll
	    then A.ABoolDots
	    else raise EH.DeadBranch (msgEmpty ll)

	and sl_sl_smltes x ll =
	    if isEmpty ll
	    then A.SmlTesDots []
	    else sl_smltes x ll

	and sl_smltes (A.SmlTesDec (s, r, n)) ll =
	    (case sl_atopdec s ll of
		 A.ATopDecDots [] => A.SmlTesDots []
	       | x => A.SmlTesDec (x, r, n))
	  | sl_smltes (A.SmlTesSpec (s, r, n)) ll =
	    (case sl_spec s ll of
		 A.SpecDots [] => A.SmlTesDots []
	       | x => A.SmlTesSpec (x, r, n))
	  | sl_smltes (A.SmlTesUse (af, r, n)) ll =
	    (case sl_afile af ll of
		 A.AFileDots => A.SmlTesDots []
	       | x => A.SmlTesUse (x, r, n))
	  | sl_smltes (A.SmlTesSBas (af, r, n)) ll =
	    (case sl_afile af ll of
		 A.AFileDots => A.SmlTesDots []
	       | x => A.SmlTesSBas (x, r, n))
	  | sl_smltes (A.SmlTesCBas (r, l, n)) ll =
	    if isinone l ll
	    then A.SmlTesCBas (r, l, n)
	    else if strictLab
	    then raise EH.DeadBranch (msgOne l ll)
	    else A.SmlTesDots []
	  | sl_smltes (A.SmlTesQuote _) ll = raise EH.TODO "no description, raised in the 'sl_smltes' function of Slice.sml"
	  | sl_smltes (A.SmlTesType _)  ll = A.SmlTesDots []
	  | sl_smltes (A.SmlTesDots pl) ll = A.SmlTesDots (sl_partlist pl ll)

	and sl_sl_atopdec x ll =
	    if isEmpty ll
	    then A.ATopDecDots []
	    else sl_atopdec x ll

	and sl_atopdec (A.ATopDecStr s) ll =
	    A.ATopDecDots (flattenStrDec [sl_strdec s ll])
	  | sl_atopdec (A.ATopDecSig s) ll =
	    A.ATopDecDots (flattenSigDec [sl_sigdec s ll])
	  (*| sl_atopdec (A.TopDecOneFun f) ll =
		  A.TopDecOneDots (flattenFunDec [sl_fundec f ll])*)
	  | sl_atopdec (A.ATopDecDots pl) ll =
	    A.ATopDecDots (sl_partlist pl ll)

	and sl_sl_topdecone x ll =
	    if isEmpty ll
	    then A.TopDecOneDots []
	    else sl_topdecone x ll

	and sl_topdecone (A.TopDecOneTes (t, n)) ll =
	    (case sl_smltes t ll of
		 A.SmlTesDots x => A.TopDecOneDots x
	       | x => (*A.TopDecOneTes (x, n)*) A.TopDecOneDots [A.PartTes x])
	  | sl_topdecone (A.TopDecOneDec (t, n)) ll =
	    (case sl_atopdec t ll of
		 A.ATopDecDots x => A.TopDecOneDots x
	       | x => A.TopDecOneDec (x, n))
	  | sl_topdecone (A.TopDecOneDots pl) ll =
	    A.TopDecOneDots (sl_partlist pl ll)

	and sl_sl_topdeconelist x ll =
	    if isEmpty ll
	    then []
	    else sl_topdeconelist x ll

	and sl_topdeconelist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_topdeconelist (x :: xs) ll =
	    let
		val nxt = A.getTopDecOneNext x
		val fst = A.getTopDecOneFirst x
		val (lll, llr) = splitListFirst nxt fst ll
		(*val _ = D.printdebug2 ("(topdeconelist) " ^ L.toString ll ^ ": " ^ L.toString lll ^ " " ^ L.toString llr)*)
		val slx  = sl_sl_topdecone x lll
		val slxs = sl_sl_topdeconelist xs llr
	    in slx :: slxs
	    end

	and sl_topdec (A.TopDec xs) ll =
	    (case flattenTopDecOne (sl_topdeconelist xs ll) of
		 [] => A.TopDecDots [] (* NEW - related to strictLab *)
	       | [A.TopDecOneDots x] => A.TopDecDots x
	       | x                     => A.TopDec x)
	  | sl_topdec (A.TopDecDots pl) ll =
	    A.TopDecDots (sl_partlist pl ll)

	(*and sl_sl_parse x ll =
		  if isEmpty ll
		  then []
		  else sl_parse x ll

	      and sl_parse [] ll =
		  if isEmpty ll
		  then []
		  else raise EH.DeadBranch
		| sl_parse ((x as (l, _, _)) :: xs) ll =
		  if isin l ll
		  then x :: (sl_sl_parse xs (L.delete l ll))
		  else sl_parse xs ll*)

	and sl_sl_progone x ll =
	    if isEmpty ll
	    then A.ProgOneDots []
	    else sl_progone x ll

	and sl_progone (A.ProgOneDec td) ll =
	    (case sl_topdec td ll of
		 A.TopDecDots x => A.ProgOneDots x
	       | x                => A.ProgOneDec x)
	  | sl_progone (A.ProgOneExp (e, v, r, l, n)) ll =
	    if isin l ll
	    then A.ProgOneExp (sl_sl_exp e (L.delete l ll), v, r, l, n)
	    else A.ProgOneDots (flattenExp [sl_exp e ll])
	  | sl_progone (A.ProgOneParse (x as (_, _, l, _))) ll =
	    if isinone l ll
	    then A.ProgOneParse x
	    else if strictLab
	    then raise EH.DeadBranch (msgOne l ll)
	    else A.ProgOneDots []
	  | sl_progone (A.ProgOneFile (af, n)) ll =
	    (case sl_afile af ll of
		 A.AFileDots => A.ProgOneDots []
	       | x => A.ProgOneFile (x, n))
	  | sl_progone (A.ProgOneDots pl) ll =
	    A.ProgOneDots (sl_partlist pl ll)

	and sl_sl_progonelist x ll =
	    if isEmpty ll
	    then []
	    else sl_progonelist x ll

	and sl_progonelist [] ll =
	    if isEmptyL ll
	    then []
	    else raise EH.DeadBranch (msgEmpty ll)
	  | sl_progonelist (x :: xs) ll =
	    let val nxt = A.getProgOneNext x
		val fst = A.getProgOneFirst x
		val (lll, llr) = splitListFirst nxt fst ll
		(*val _ = D.printdebug2 ("(progonelist) " ^ L.toString ll ^ ": " ^ L.toString lll ^ " " ^ L.toString llr)*)
		val slx  = sl_sl_progone x lll
		val slxs = sl_sl_progonelist xs llr
	    in slx :: slxs
	    end

	and sl_sl_prog x ll =
	    if isEmpty ll
	    then A.ProgDots []
	    else sl_prog x ll

	and sl_prog (A.Prog xs) ll =
	    (case flattenProgOne (sl_progonelist xs ll) of
		 [] => A.ProgDots [] (* NEW - related to strictLab *)
	       | [A.ProgOneDots x] => A.ProgDots x
	       | x                   => A.Prog x)
	  | sl_prog (A.ProgDots pl) ll = A.ProgDots (sl_partlist pl ll)

	and sl_proglist x ll =
	    if isEmpty ll
	    then []
	    else sl_sl_proglist x ll

	and sl_sl_proglist [] ll =
	    if isEmptyL ll
	    then []
	    else ((*Debug.printdebug2 ("empty");*) raise EH.DeadBranch (msgEmpty ll))
	  | sl_sl_proglist ((x, y, b, n) :: xs) ll =
	    let val nxt = SOME n
		val fst = A.getProgFirst x
		val (lll, llr) = splitListFirst nxt fst ll
		(*val _ = Debug.printdebug2 ("(1) " ^ L.toString ll ^ ": " ^ L.toString lll ^ " " ^ L.toString llr ^ " " ^ printFirst fst)*)
		val slx  = sl_sl_prog x lll
		(*val _ = Debug.printdebug2 ("done1")*)
		val slxs = sl_sl_proglist xs llr
	    (*val _ = Debug.printdebug2 ("done2")*)
	    in case slx of
		   A.ProgDots [] => slxs
		 | _ => (slx, y, b, n) :: slxs
	    end

	and sl_progs (A.Progs progs) ll = A.Progs (sl_proglist progs ll)

	val labels' = L.delete L.dummyLab labels

	val slice = sl_progs prog labels'

    in slice

    end


end
