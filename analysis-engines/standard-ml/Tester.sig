(* Copyright 2009 2010 2011 2012 2013 Heriot-Watt University
 * Copyright 2018 Christian Gregg
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
 *)

(** Defines the signature TESTER which is the signature of the interface with the running and testing of the slicer. *)
signature TESTER = sig

    type 'a debug = Error.error list ->
		    AstSML.packs     ->
		    bool        ->
		    Error.times ->
		    Env.envContextSensitiveSyntaxPair  ->
		    Label.label ->
		    bool        ->
		    string      ->
		    bool        ->
		    int         ->
		    string      ->
		    'a

    type 'a temptype = Error.error list ->
		    AstSML.packs->
		    bool        ->
		    Error.times ->
		    Env.envContextSensitiveSyntaxPair  ->
		    Label.label ->
		    bool        ->
		    string      ->
		    bool        ->
		    int         ->
		    int         ->
		    string      ->
		    'a

    val slicing       : string       ->
			string list  ->
			Error.export ->
			int          ->
			bool         ->
			bool         ->
			bool         ->
			bool         ->
			int          ->
			int          ->
			(int * Error.error list)

    val debuggingXML  : string temptype
    val debuggingSML  : string temptype
    val debuggingJSON : string temptype
    val debuggingLISP : string temptype
    val debuggingPERL : string temptype
    val debuggingVIZ  : string temptype
    val debuggingBASH : unit temptype
    val debuggingHTML : Error.error list ->
			AstSML.packs     ->
			bool        ->
			Error.times ->
			Env.envContextSensitiveSyntaxPair  ->
			Label.label ->
			bool        ->
			string      ->
			bool        ->
			int         ->
			int         ->
			(string -> string)      ->
			string      ->
			string      ->
			bool        ->
			string      ->
			unit

    val testFolder     : string ref
    val checktests     : int list -> unit

    val listTests     : unit   -> unit
    val printTypables : unit   -> unit

    val getLastRegs   : unit   -> ExtReg.regs list
    val getLastSlices : unit   -> string list
    val getLastMin    : unit   -> bool

    val setstylecss   : int    -> unit

    val gettimelimit  : unit   -> LargeInt.int
    val timelimit     : LargeInt.int ref
    val mytimelimit   : LargeInt.int
    val notimelimit   : LargeInt.int
    val settimelimit  : LargeInt.int -> unit

    val getTabSize    : unit -> int
    val setTabSize    : int  -> unit

    val error         : JsonParser.error
    val myfilebas     : string ref
    val myfilehtml    : string

    val convertErrors : JsonParser.error -> string -> unit

end
