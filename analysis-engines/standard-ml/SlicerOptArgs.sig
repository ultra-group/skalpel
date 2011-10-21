(* Copyright 2010 Heriot-Watt University
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
 *  o Authors:     John Pirie
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        15 October 2010
 *  o File name:   SlicerOptArgs.sig
 *  o Description: Defines the signature SLICEROPTARGS which is the signature
 *      of the structure which provides functionality for optional arguments to
 *      the slicer
 *)


(*
 * there are a total of 14 arguments which can be passed as arguments here:
 *
 * 1 - BASOP is a number which states whether we use the full basis,
 * 2 - BCS is a number which states whether we print environment or not
 * (see Env.printEnv),
 * 3 - DEV is a boolean which if true will print debugging xml and
 * some other functions (see Tester.sml for the function which deals
 * with this)
 * 4 - FILEBAS is a file which we can specify optionally to be used as
 * the basis file
 * 5 - FILEHTML is an optional file where the type error slices can be
 * dumped to
 * 6 - FILEIN is the file which we use as input to the slicer (default
 * is "test-prog.sml"
 * 7 - FILELISP is a lisp file that we can export the type error
 * slices to
 * 8 - FILESML is a StandardML file where we dump the type error
 * slices to
 * 9 - FILEXML is an XML file where the type error slicer may be
 * placed
 * 10 - MIN is a flag which if set to true will show type error slices
 * which have not yet been minimised before they are in fact minimised.
 * 11 - SEARCHSPACE is an integer which can be used to switch between
 * 12 - SOL is an integer value which represents what solution we
 * use (we now only use solution 9)
 * 13 - TAB defines the tab size in user code regions
 * 14 - TIMELIM is an integer, allowing us to control the maximum time
 * that the slicer spends running on the user code
 *)

signature SLICEROPTARGS = sig
    datatype ('a,'b) opt_key_arg
      = BASOP       of int
      | BCS         of int
      | DEV         of bool
      | FILEBAS     of string
      | FILEHTML    of string
      | FILEIN      of string
      | FILELISP    of string
      | FILEPERL    of string
      | FILESML     of string
      | FILEXML     of string
      | MIN         of bool
      | SEARCHSPACE of int
      | SOL         of int
      | TAB         of int
      | TIMELIM     of int
    val optArg : ('a,'b) opt_key_arg list list
                 -> {basOp       : int,
		     bcs         : bool,
		     dev         : bool,
		     fileBas     : string,
                     fileHtml    : string,
		     fileIn      : string,
		     fileLisp    : string,
                     filePerl    : string,
		     fileSml     : string,
		     fileXml     : string,
                     min         : bool,
		     searchSpace : int,
		     sol         : int,
		     tab         : int,
		     timeLim     : int}
end

