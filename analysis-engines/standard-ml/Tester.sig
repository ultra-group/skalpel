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
 *  o File name:   Tester.sig
 *  o Description: Defines the signature TESTER which is the signature
 *      of the interface with the running and testing of the slicer.
 *)


signature TESTER = sig

    (*(2010-03-04)An error should also contain the id of the error
     * We should also include the solution used when the error was recorded. *)
    type error = {errors       : {labels       : int * int list,
				  assumptions  : LongId.keyOut list,
				  kind         : ErrorKind.kind,
				  slice        : string,
				  time         : LargeInt.int,
				  identifier   : int,           (* unique identifier of the error, ~1 if can't do that.*)
				  regions      : ExtReg.regs} list,
		  time         : {analysis     : LargeInt.int,
				  enumeration  : LargeInt.int,
				  minimisation : LargeInt.int,
				  slicing      : LargeInt.int,
				  html         : LargeInt.int},
		  tyvar        : int * Id.assocOut,
		  ident        : Id.assocOut,
		  constraint   : {syntactic : int,
				  top       : int,
				  total     : int},
		  labels       : int,
		  minimisation : bool,
		  solution     : int,
		  basis        : int,
		  timelimit    : LargeInt.int,
		  labelling    : string,
		  final        : bool,
		  name         : string} option ref

    type 'a debug = Error.error list ->
		    AstSML.packs     -> (* the int the next label w.r.t. progs *)
		    bool        -> (* true if minimiser was called (this not used anymore, because the minimiser is always called now) *)
		    Error.times ->
		    Env.envcss  ->
		    Label.label -> (* the first label in progs                                *)
		    bool        -> (* true if the slices look good enough                     *)
		    string      -> (* the name of the test                                    *)
		    bool        -> (* true if we want the pretty printing of slices           *)
		    int         -> (* basis switch: 0 (no basis), 1 (builtin basis), 2 (file) *)
		    string      -> (* initial indentation                                     *)
		    'a

    type 'a temptype = Error.error list ->
		    AstSML.packs     -> (* the int the next label w.r.t. progs *)
		    bool        -> (* true if minimiser was called (this not used anymore, because the minimiser is always called now) *)
		    Error.times ->
		    Env.envcss  ->
		    Label.label -> (* the first label in progs                                *)
		    bool        -> (* true if the slices look good enough                     *)
		    string      -> (* the name of the test                                    *)
		    bool        -> (* true if we want the pretty printing of slices           *)
		    int         -> (* basis switch: 0 (no basis), 1 (builtin basis), 2 (file) *)
		    int         -> (* basis overloading errors *)
		    string      -> (* initial indentation                                     *)
		    'a

    (* TO RUN THE SLICER *)
    val slicing       : string       -> (* file containing the basis *)
			string list  -> (* input files               *)
			Error.export ->
			int          -> (* see Analyze.sig for this integer - related to the basis *)
			bool         -> (* true if running the slicer from the webdemo, so that SML-TES-USE-FILE and SML-TES-SET-BASIS are disabled *)
			bool         -> (* true if we want to report non-minimal errors *)
			bool         -> (* true if we want to output the all the errors in an html default file and the in the xml format in the standard output *)
			bool         -> (* true to print the constaints in the standard output *)
			int          -> (* 1 for the default searchspace, 2 for the searchspace' and 3 for the rbs one, the default is 1 *)
			int          -> (* basis overloading *)
			int (* int is the number of errors *)

    (* FUNCTIONS GENERATING FORMATED ERRORS *)
    val debuggingXML  : string temptype
    val debuggingSML  : string temptype
    val debuggingLISP : string temptype
    val debuggingPERL : string temptype
    val debuggingHTML : Error.error list ->
			AstSML.packs     ->
			bool        ->
			Error.times ->
			Env.envcss  ->
			Label.label -> (* the first label in progs                                *)
			bool        -> (* true if the slices look good enough                     *)
			string      -> (* the name of the test                                    *)
			bool        -> (* true if we want the pretty printing of slices           *)
			int         -> (* basis switch: 0 (no basis), 1 (builtin basis), 2 (file) *)
			int         -> (* overloading basis *)
			(string -> string)      -> (* overloading basis *)
			string      -> (* file out  *)
			string      -> (* file base *)
			bool        -> (* true if we want to print the head and body tags *)
			string      -> (* basis switch: 0 (no basis), 1 (builtin basis), 2 (file) *)
			unit

    val removeBasisSlice : string -> (* input slice *)
			   string    (* output slice *)

    (**)
    (*val buildError    : error debug*)
    (*val assignError   : error -> unit*)

    (**)
    (*val testXML       : (string -> string) -> string*)

    (* TO HANDLE THE DATABASE *)

    (* checktests will run the slicer on the code files available in the database directory *)
    val checktests    : int list -> unit
    val runtests      : int list -> LargeInt.int option -> unit
    val adderror      : string -> (* name of the file that contains the code to store in the database *)
			int    -> (* identifier of the test *)
			bool   -> (* true if we want to force the test to have this identifier, it will then erase any previous error with the same id *)
			bool   -> (* true if the slices for the error are currently OK *)
			string -> (* name of the test *)
			int    -> (* basis switch: 0 (no basis), 1 (built-in basis), 2 (file) *)
			unit
    val mverror       : int    -> int -> bool -> unit
    val delerror      : int    -> unit
    val listTests     : unit   -> unit
    val printTypables : unit   -> unit

    (* TO ACCESS INFORMATION ON THE LAST FOUND ERROR *)
    val getLastRegs   : unit   -> ExtReg.regs list
    val getLastSlices : unit   -> string list
    val getLastMin    : unit   -> bool

    (* TO HANDLE THE DIFFERENT BRANCHES *)
    val setsol        : int    -> unit
    val getsol        : unit   -> int
    val printsol      : unit   -> unit

    (* STYLE OF THE HTML PAGES GENERATED *)
    val setstylecss   : int    -> unit

    (**)
    val checkAstFile  : string -> bool * bool
    val checkAstDB    : unit   -> unit

    (* TO HANDLE THE TIMELIMIT OF THE SLICER *)
    val gettimelimit  : unit   -> LargeInt.int
    val timelimit     : LargeInt.int ref
    val mytimelimit   : LargeInt.int
    val notimelimit   : LargeInt.int
    val settimelimit  : LargeInt.int -> unit

    (* TO HANDLE TABULATION SIZE *)
    val getTabSize    : unit -> int
    val setTabSize    : int  -> unit

    (**)
    val error         : error
    val myfilebas     : string
    val myfilein      : string
    val myfilehtml    : string

    val vinnie        : int    -> unit

end
