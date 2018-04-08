(* Copyright 2009 2010 2011 2012 Heriot-Watt University
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
 *  o File name:   Debug.sig
 *  o Description: Defines the signature DEBUG for printing of debugging
 *      messages.
 *)

(** A signature used by refstruct{Debug}, used to aid debugging of Skalpel. *)
signature DEBUG = sig

    datatype debugFiles = JSON
			| UNIF
			| LABEL
			| TY
			| MLGRM
			| AZE
			| TEST
			| RUN
			| ENV
			| PARSER
			| BLANK
			| LEXER
			| MIN

    val debugProgramLabelling : bool ref
    val oneRunOnly : bool ref
    val debug : bool ref
    val debugBasis : bool ref
    datatype debugFeature = EQUALITY_TYPES
			  | CONSTRAINT_PATH
			  | CONSTRAINT_GENERATION
			  | CONSTRAINT_SOLVING
			  | CONSTRAINT_SOLVE_PP
			  | TESTING
			  | PARSING
			  | STATE
			  | PROGRAM_LABELLING
			  | BASIS_LABELLING
			  | MINIMISATION
        | VISUALISATION
			  | TEMP                    (* temporary output, this should be cleaned up regularly! *)

    val sep1' : string
    val sep2' : string

    val colors : {black:string, red:string, green:string, yellow:string, blue:string, purple:string, cyan:string, white:string} ref
    val boldColors : {black:string, red:string, green:string, yellow:string, blue:string, purple:string, cyan:string, white:string} ref
    val underlineColors : {black:string, red:string, green:string, yellow:string, blue:string, purple:string, cyan:string, white:string} ref
    val backgroundColors : {black:string, red:string, green:string, yellow:string, blue:string, purple:string, cyan:string, white:string} ref
    val leafInBoxColors : {black:string, red:string, green:string, yellow:string, blue:string, purple:string, cyan:string, white:string} ref
    val textReset : string ref

    val enableDebugFeature : debugFeature -> unit

    val printReset : string -> unit

    val printDebug : debugFiles -> debugFeature -> (unit -> string) -> unit

    val printLabelledProgramString : string -> string

    val checkOneRunOnly  : unit -> unit

    val printdebug1 : string -> unit (* Does not print  *)
    val printdebug2 : string -> unit (* fancy printing  *)

end
