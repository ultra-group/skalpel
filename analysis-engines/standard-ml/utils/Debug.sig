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
 *  o File name:   Debug.sig
 *  o Description: Defines the signature DEBUG for printing of debugging
 *      messages.
 *)


signature DEBUG = sig

    datatype debugFiles = JSON     (* JsonParser.sml *)
			| UNIF     (* Unification.sml *)
			| LABEL    (* Label.sml *)
			| TY       (* Ty.sml *)
			| MLGRM    (* ML.grm *)
			| AZE      (* Analyze.sml *)
			| TEST     (* Tester.sml *)
			| RUN      (* RunSlicer.sml *)
			| ENV      (* Env.sml *)
			| PARSER   (* Parser.sml *)

    val debugProgramLabelling : bool ref
    val oneRunOnly : bool ref
    val debug : bool ref
    datatype debugFeature = EQUALITY_TYPES
			  | CONSTRAINT_GENERATION
			  | CONSTRAINT_SOLVING
			  | TESTING
			  | PARSING
			  | STATE
			  | PROGRAM_LABELLING

    val sep1' : string
    val sep2' : string

    val colors : {black:string, red:string, green:string, yellow:string, blue:string, purple:string, cyan:string, white:string}
    val textReset : string

    val enableDebugFeature : debugFeature -> unit

    val printDebugFeature : debugFiles -> debugFeature -> (unit -> string) -> unit

    val printLabelledProgramString : string -> string

    val checkOneRunOnly  : unit -> unit

    val printdebug1 : string -> unit (* Does not print  *)
    val printdebug2 : string -> unit (* fancy printing  *)

end
