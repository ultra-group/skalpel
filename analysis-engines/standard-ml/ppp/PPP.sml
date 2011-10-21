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
 *  o Authors:     Joe Wells, Vincent Rahli
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        25 May 2010
 *  o File name:   PPP.sml
 *  o Description: Defines the structure Ppp which has signature PPP and
 *      which is used to use some of SML/NJ's features such as parsing
 *      and pretty printing.
 *)


structure Ppp : PPP = struct

val use = use

local val original_Control_Print_out = ! Control.Print.out;
in fun unsilence_compiler () =
     Control.Print.out := original_Control_Print_out;
   fun silence_compiler () =
     Control.Print.out := { flush = fn () => (), say = fn _ => () };
   (* Don't show declarations added to the top-level environment or
      further autoloading messages.  Disable next line if you want type
      error messages. *)
   (*val _ = silence_compiler ();*)
end;

fun evalString s =
  Backend.Interact.evalStream (TextIO.openString s, EnvRef.combined ());

fun unwind_protect body unwind_fn =
    (  (body ())
     before
       (* EEEK!  If this use of unwind_fn raises an exception, then unwind_fn will be run again from the exception handler. *)
       (unwind_fn ()))
  handle
    e => (unwind_fn (); raise e);

fun capturePrinting f =
  let
    val old_out = ! Control.Print.out
    val data = ref ""
  in
    Control.Print.out :=
      { flush = (fn () => ()),
        say = (fn s => data := !data ^ s) };
    unwind_protect
      (fn () => (f (); !data))
      (fn () => (Control.Print.out := old_out))
  end;

fun raisePrintingLimitsToMax f =
  let val old_print_depth = ! Control.Print.printDepth
      val old_print_length = ! Control.Print.printLength
      val old_string_depth = ! Control.Print.stringDepth
      val old_intinf_depth = ! Control.Print.intinfDepth
      (* *** FIX: what is the correct largest value to use here? *)
      (* BUG: SML/NJ doesn't let us have unlimited printing *)
      val _ = (Control.Print.printDepth := 9999) (* default: 5 *)
      val _ = (Control.Print.printLength := 9999) (* default: 12 *)
      val _ = (Control.Print.stringDepth := 9999) (* default: 70 *)
      val _ = (Control.Print.intinfDepth := 9999) (* default: 70 *)
      (* TODO: should probably also set Control.Print.linewidth
         (default: 79) to high value to avoid wasting space for pretty
         line breaks and indentation *)
      (* *** Do we care about Control.Print.printLoop? *)
  in
    unwind_protect
      (fn () => f ())
      (fn () => (Control.Print.printDepth := old_print_depth;
                 Control.Print.printLength := old_print_length))
  end;

(* Here is an example of providing definitions that can be used by
   configuration files which will be evalled. *)

(*print "making...\n";*)
(*CM.make "sources.cm";*)
(*print "made...\n";*)
open AstSML;

datatype data = AST of AstSML.progs;

val data_tmp : data option ref = ref NONE;

(* parses the string str, but reduce the output printed by the compiler *)
fun parse_data str =
  let val _ = (data_tmp := NONE)
      val e = "val _ = data_tmp := SOME (" ^ str ^ ");"
      val _ = print ("e: ["^e^"]\n")
      val msg = capturePrinting
                  (fn () =>   ignore (evalString (e))
                            handle _ => ())
      val _ = print ("msg: ["^msg^"]\n")
  in ! data_tmp
  end;

fun format_data data =
  let val _ = (data_tmp := SOME data)
      val e = "val x = valOf (! data_tmp);"
      val _ = print ("e: ["^e^"]\n")
      val s = raisePrintingLimitsToMax
                (fn () => capturePrinting
                            (fn () => evalString e))
  in (* the length of "val x = " is 8, the length of " : data\n" is 8 *)
    String.substring (s, 8, (String.size s) - 8 - 8)
  end;

fun test1 _ =
    let val v1 = AST (Progs [])
	val v2 = format_data v1
	val _  = print ("v2: [" ^ v2 ^ "]\n")
	val _  = print "done\n"
    in ()
    end

(* prints an abstract synntax tree given as a parameter *)
fun prettyPrintAst ast = format_data (AST ast)

fun readAst st = case Option.valOf (parse_data st) of AST ast => ast


end
