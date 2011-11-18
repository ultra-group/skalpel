(* Copyright 2009 Heriot-Watt University
 * Copyright 2010 Heriot-Watt University
 * Copyright 2011 Heriot-Watt University
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
 *  o Date:        21 May 2010
 *  o File name:   RunSlicer.sig
 *  o Description: Defines the signature SLICER which is the signature
 *      of the functions that serve as interface with the slicer.
 *)


signature SLICER = sig

    (* As explained in Analyze.sig
     * the integer was before a Boolean and now it is:
     * 0 if we don't want any environemnt
     * 1 if we want the the builtin environment
     * 2 if we want to use basis.sml *)
    (*val slicer             : string      -> (* basis file       *)
			     string list -> (* files to slice   *)
			     string      -> (* html output file *)
			     int         -> (* basis option     *)
			     int         -> (* timelimit        *)
			     unit*)

    (* myslicerp:
     * The list is supposed to be a list of length 2.
     * The first one is a bool and the second one is an integer.
     * They are sent as arguments to myslicer. *)
    (*val myslicerp          : (string * string list) -> OS.Process.status*)

    (* genslicer:
     * - bool: true is we wanna print the constraints
     * - int:  0/1/2 as above
     * - int:  timelimit *)
    (*val genslicer          : bool -> int -> int -> unit*)

    (* The next function is used to store a new test in the database.
     * A test is always read from the "test-prog.sml" file *)
    val addtest            : int    -> (* test's identifier, it has to be greater or equal to 1 *)
			     bool   -> (* true if the result from the slices are currently correct *)
			     string -> (* test's name *)
			     int    -> (* basis switch: 0 (no basis), 1 (builtin basis), 2 (file) *)
			     unit
    (* This function differs from addtest by forcing a test to go into the database
     * even if the database contains a test with the specified identifier *)
    val replaceTest        : int  -> bool -> string -> int -> unit
    val deltest            : int  -> unit
    val mvtest             : int  -> int -> unit
    (* use empty list to run the slicer on all the tests
     * provide a list of two integers to run the slicer on an specific interval *)
    val checktests         : int list -> unit
    (* runs all the tests in the database using the given timelimit *)
    val runtests           : int list -> LargeInt.int option -> unit
    (* lists the tests in the database
     * if true list the typable ones otherwise all of them *)
    val listtests          : bool -> unit

    (*val getLastRegions     : unit -> ER.regs list
    val getLastSlices      : unit -> string list
    val getLastMin         : unit -> bool*)

    (* true if we want to be in the online demo mode:
     * no SML-TES-USE-FILE and no SML-TES-SET-BASIS *)
    val setWebDemo         : bool -> unit

    (*val commslicer         : string -> (* basis file       *)
			     string list -> (* input files *)
			     string -> (* HTML output file *)
			     string -> (* XML  output file *)
			     string -> (* SML  output file *)
			     string -> (* LISP output file *)
			     string -> (* PERL output file *)
			     int    -> (* 0/1/2, this is for the basis *)
			     int    -> (* desired running time         *)
			     unit*)

    val commslicerp        : string      -> (* basis file       *)
    			     string list -> (* input files      *)
    			     string      -> (* HTML output file *)
    			     string      -> (* XML  output file *)
    			     string      -> (* SML  output file *)
    			     string      -> (* LISP output file *)
    			     string      -> (* PERL output file *)
    			     int         -> (* 0/1/2, this is for the basis *)
    			     int         -> (* desired running time         *)
    			     int         -> (* basis overloading         *)
    			     unit

    (*function which turns optional arguments to full list of arguments for the slicer*)
    val smltes            : ({fileBas     : string,
			      tab         : int,
			      sol         : int,
			      min         : bool,
			      dev         : bool,
			      bcs         : bool,
			      searchSpace : int,
			      fileIn      : string,
			      fileHtml    : string,
			      fileXml     : string,
			      fileSml     : string,
			      fileJson    : string,
			      fileLisp    : string,
			      filePerl    : string,
			      basOp       : int,
			      timeLim     : int}) -> OS.Process.status

    val smlTesStrArgs : string -> OS.Process.status

    (*val smltes             : string -> OS.Process.status*)

    val smlteslight          : ('a,'b) SlicerOptArgs.opt_key_arg list -> OS.Process.status

    val smltesdev            : ('a,'b) SlicerOptArgs.opt_key_arg list -> OS.Process.status

    val smltesstr            : string -> OS.Process.status

    include SLICEROPTARGS

    (*val getFreeIdentifiers : string
			     -> bool
                             -> {freeConstructors       : string list,
				 freeTyNames            : string list,
				 freeUnknownIdentifiers : string list,
				 maybeFreeConstructors  : string list}*)


    (*val noslicer           : bool -> unit*) (* we can use genslicer instead *)
    (*val setstylecss        : int -> unit*)
    (*val checksometests     : int list -> unit*) (* now use checktests instead *)
    (*val temp              : int -> int option -> string option -> unit*)

end
