(* Copyright 2009 2010 Heriot-Watt University
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
 *  o Date:        Decemeber 2009
 *  o File name:   PGOps.sml
 *  o Description: This file contains code to generate a dot file
 *      containing the dependencies of the sml-tes implementation's
 *      files.
 *        Evalute: SmlTesDeps.getDot "sources.cm"
 *
 *      It also contains code to generate a .tes file from a .cm
 *      file. (This can also be used to generate part of a .mlb
 *      file from a .cm file).
 *        Evalute: SmlTesDeps.getTes "sources.cm"
 *)

(* load the potable graph utility library *)
val _ = CM.autoload "$/pgraph.cm";
val _ = CM.autoload "$/pgraph-util.cm";
type ctxt = (string, string list, string, string, string, string) PGOps.context
val lib = ref (fn x : ctxt => fn _ : string list => "")

(* signature for SmlTesDeps *)
signature SMLTESDEPS = sig

    val getDot : string -> string -> unit (*string is a .cm file*)

    val cmToDot : (string * string list) -> OS.Process.status

    (*val printFiles : unit -> unit*)

end


structure SmlTesDeps :> SMLTESDEPS = struct

(* raised if there is any kind of problem with a file (incorrect type, non-existant, etc) *)
exception ErrorFile of string

(* This is a temporary file *)
val thelib = "/tmp/thelibrary.sml"

fun isin (x : string) xs = List.exists (fn y => y = x) xs

(* converts a list of something to a string *)
fun stringListToString xs = "[" ^ #1 (foldr (fn (t, (s, c)) => (t ^ c ^ s, ",")) ("", "") xs) ^ "]"

structure OrdFile : ORD_KEY =
struct
type ord_key = string(*file*) * string list(*dependencies*)
fun compare ((f1, d1), (f2, d2)) =
    if isin f1 d2
    then LESS
    else if isin f2 d1
    then GREATER
    else GREATER (*String.compare (f1, f2)*)
(* The last greater is a quick fix that uses that when adding an element
 * to a set, the first element of the pair when comparing 2 elements will
 * be the element that is added to the set.
 * Ideally, we would like something more robust. *)
end

structure M = BinarySetFn(OrdFile)

(* prints deps for a given file *)
fun printDeps file deps =
    print (file ^ ":" ^ stringListToString deps ^ "\n")

(* print everything in the binary set using stringListToString *)
fun printMFiles mfiles =
    (print "==========================================\n";
     M.app (fn (x, xs) => print (x ^ ":" ^ stringListToString xs ^ "\n")) mfiles;
     print "==========================================\n")

(* outputs a graph file from a CM input file *)
fun output file = (*file is a .cm file*)
    let val out   = TextIO.openOut thelib
	val graph = #graph (Option.valOf (CM.Graph.graph file))
	val _ = FormatPortable.output (graph, out)
	val _ = TextIO.output (out, "\nval _ = lib := thelibrary\n")
	val _ = TextIO.closeOut out
    in ()
    end

fun getLib file = (output file; use thelib) (*we get the thelibrary function*)

(* The quotes are for the dots in the file names. *)
fun setDotDeps xs x dot =
    dot := ("  \"" ^ x ^ "\";") ::
	   (map (fn y => "  \"" ^ y ^ "\" -> \"" ^ x ^ "\";") xs) @
	   (!dot)

fun setTesDeps deps file mfiles =
    let val deps1 = foldr (fn (file1, deps) =>
			      case M.find (fn (file2, _) => file1 = file2) (!mfiles) of
				  NONE => deps
				| SOME (_, deps') => deps' @ deps)
			  []
			  deps
	(* deps1 are the dependencies of the dependencies of file *)
	val deps2 = foldr (fn (file, deps) => if isin file deps then deps else file :: deps)
			  []
			  (deps1 @ deps)
	(*val _ = printDeps file deps2*)
	val _ = mfiles := M.add (!mfiles, (file, deps2))
	(*val _ = printMFiles (!mfiles)*)
    in ()
    end

fun context dot mfiles =
    { Ops = { Sgn  = fn misc => fn st => (misc, ""), (* forces sym to be string *)
	      Str  = fn misc => fn st => (misc, ""),
	      Fct  = fn misc => fn st => (misc, ""),
              Imp  = fn misc => fn (lib: string, syms) => (misc, []), (* forces env to be a 'a list and lib to be a string *)
	      Com  = fn misc => fn (st, env, syms, bool) =>
				   (setDotDeps env st dot;
				    setTesDeps env st mfiles;
				    (misc, (*env*) [st])), (* forces env to be a string list *)
	      Mer  = fn misc => fn envList => (misc, List.concat envList), (* forces env to be a 'a list *)
	      Fil  = fn misc => fn (env, syms) => (misc, env),
              Syms = fn misc => fn symList => (misc, ""), (* forces syms to be string *)
	      Exp  = fn misc => fn env => ""}, (* forces export to be string *)
      Misc = "" }; (* forces misc to be string *)

fun printOrderedFiles mfiles =
    M.app (fn (x, xs) => print (x ^ " " ^ stringListToString xs ^ "\n")) mfiles

(*fun thelibrary x _ = x*)

fun generateList () =
    let val stin = TextIO.openIn thelib
	val list =
	    case TextIO.inputLine stin of
		NONE => []
	      | SOME _ =>
		(case TextIO.inputLine stin of
		     NONE => []
		   | SOME line =>
		     let fun tok #"," = true
			   | tok _    = false
		     in map (fn _ => "") (String.tokens tok line)
		     end)
	val _ = TextIO.closeIn stin
    in list
    end

fun computeDependencies file =
    let val _     = getLib file
	val mfile = ref M.empty
	val dot   = ref []
	val list  = generateList ()
	(*val list  = ["", "", "", "", "", "", "", "", "", "", "", "", ""]*)
	(*val list  = []*)
	val _     = (!lib) (context dot mfile) list
    in (!dot, !mfile)
    end

(* verifies that a passed file that is supposed to be a cm file is actually a cm file *)
fun checkCMFile file =
    let val f = OS.FileSys.fullPath file
    in if OS.FileSys.isLink f orelse OS.FileSys.isDir f
       then raise ErrorFile "input file does not exist\n"
       else if String.isSuffix ".cm" f
       then String.substring (f, 0, (String.size f) - 3)
	    handle Subscript => raise ErrorFile ""
       else raise ErrorFile "input file is not a .cm file\n"
    end
    handle ErrorFile st => raise ErrorFile st
	 | SysErr => raise ErrorFile "input file does not exist\n"

fun checkFileOut file suffix =
    let fun getPref () =
	    String.substring (file, 0, (String.size file) - (String.size suffix))
	    handle Subscript => raise ErrorFile ""
    in if String.isSuffix suffix file
       then (if OS.FileSys.isDir file orelse OS.FileSys.isLink file
	     then raise ErrorFile "output file is not a file\n"
	     else getPref ())
	    handle SysErr => getPref ()
       else raise ErrorFile ("output file is not a " ^ suffix ^ " file\n")
    end

fun checkFiles filein fileout suffix =
    let val prefix = checkCMFile filein
	val out = prefix ^ suffix
    in case fileout of
	   "" => (print ("the output file will be " ^ out ^ "\n");
		  checkFileOut out suffix)
	 | _  => checkFileOut fileout suffix
    end

(* these come from Joe *)
val original_Control_Print_out = ! Control.Print.out
fun unsilence_compiler () =
    Control.Print.out := original_Control_Print_out
fun silence_compiler () =
    Control.Print.out := { flush = fn () => (), say = fn _ => () }

fun getDot filein fileout =
    let val suffix = ".dot"
	val prefix = checkFiles filein fileout suffix
	val fileout = prefix ^ suffix
	val _ = silence_compiler ()
	val dot = #1 (computeDependencies filein)
	    handle Error => (unsilence_compiler ();
			     raise ErrorFile "The input .cm file does not follow the correct syntax\n")
	val _ = unsilence_compiler ()
	val out = TextIO.openOut fileout
	val _ = TextIO.output (out, "digraph smlTesDeps {\n")
	val _ = app (fn x => TextIO.output (out, x ^ "\n")) dot
	val _ = TextIO.output (out, "}\n")
	val _ = TextIO.closeOut out
    in ()
    end
    handle ErrorFile st => print st
	 | _ => print "an error occured"

fun cmToDot (_, [filein, fileout]) =
    (getDot filein fileout; OS.Process.success)
  | cmToDot (_, [filein]) =
    (getDot filein ""; OS.Process.success)
  | cmToDot _ = OS.Process.failure (* we should give explanations here *)

fun printFiles () = printOrderedFiles (#2 (computeDependencies "sources.cm"))

(*val _ = genDot ()*)

end
