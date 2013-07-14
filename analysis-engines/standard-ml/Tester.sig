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
 *  o Authors:     Vincent Rahli, John Pirie
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        25 May 2010
 *  o File name:   Tester.sig
 *  o Description: Defines the signature TESTER which is the signature
 *      of the interface with the running and testing of the slicer.
 *)


signature TESTER = sig

    type 'a debug = Error.error list ->
		    AstSML.packs     -> (* the int the next label w.r.t. progs *)
		    bool        -> (* true if minimiser was called (this not used anymore, because the minimiser is always called now) *)
		    Error.times ->
		    Env.envContextSensitiveSyntaxPair  ->
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
		    Env.envContextSensitiveSyntaxPair  ->
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
			bool         -> (* true if running the slicer from the webdemo, so that SKALPEL-USE-FILE and SKALPEL-SET-BASIS are disabled *)
			bool         -> (* true if we want to report non-minimal errors *)
			bool         -> (* true if we want to output the all the errors in an html default file and the in the xml format in the standard output *)
			bool         -> (* true to print the constaints in the standard output *)
			int          -> (* 1 for the default searchspace, 2 for the searchspace' and 3 for the rbs one, the default is 1 *)
			int          -> (* basis overloading *)
			int (* int is the number of errors *)

    (* FUNCTIONS GENERATING FORMATED ERRORS *)
    val debuggingXML  : string temptype
    val debuggingSML  : string temptype
    val debuggingJSON : string temptype
    val debuggingLISP : string temptype
    val debuggingPERL : string temptype
    val debuggingBASH : unit temptype
    val debuggingHTML : Error.error list ->
			AstSML.packs     ->
			bool        ->
			Error.times ->
			Env.envContextSensitiveSyntaxPair  ->
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


    (**)
    (*val buildError    : error debug*)
    (*val assignError   : error -> unit*)

    (**)
    (*val testXML       : (string -> string) -> string*)

    (* TO HANDLE THE DATABASE *)

    (* checktests will run the slicer on the code files available in the database directory *)
    val testFolder     : string ref
    val checktests     : int list -> unit

    val listTests     : unit   -> unit
    val printTypables : unit   -> unit

    (* TO ACCESS INFORMATION ON THE LAST FOUND ERROR *)
    val getLastRegs   : unit   -> ExtReg.regs list
    val getLastSlices : unit   -> string list
    val getLastMin    : unit   -> bool

    (* STYLE OF THE HTML PAGES GENERATED *)
    val setstylecss   : int    -> unit

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
    val error         : JsonParser.error
    val myfilebas     : string ref
    val myfilehtml    : string

    val vinnie        : int    -> unit

    (* remove this when test database is in json format *)
    val convertErrors : JsonParser.error -> string -> unit
    val generateTests  : int -> int -> unit

end
