(* Copyright 2002 2009 2010 2012 Heriot-Watt University
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
 *  o Authors:     Vincent Rahli, Christian Haack
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        25 May 2010
 *  o File name:   Parser.sig
 *  o Description: Defines the signature PARSER to parse SML code and
 *      basis files.
 *)


signature PARSER = sig

    type messages = (string * string * Reg.region list) list

    (*exception ParsingError of int * messages (* the integer is a label *)*)

    val parse : string          ->
		TextIO.instream ->
		Label.label     ->
		Id.assoc        ->
		AstSML.prog * Label.label * Id.assoc
    (* 1st argument: file name, used in syntax error messages
     * 2nd argument: input stream, typically (TextIO.openIn file)
     * 3rd argument: next node label before execution
     * 4th argument: association lists for type variables and identifiers
     * 1st output component: abstract syntax tree
     * 2nd output component: next node label after execution
     *)

    (*(* The boolean is supposed to be true if the file (string) is a basis file *)
    val parseTes : TextIO.instream -> int -> (string * Reg.region * bool) list*)

    val consProgs : string list -> (* basis files *)
		    string list -> (* other files *)
		    Label.label -> (* next node label before execution *)
		    Id.assoc    -> (* association lists for type variables and identifiers *)
		    int         -> (* the option for the basis *)
		    bool        -> (* true if used by the webdemo, so that SKALPEL-USE-FILE and SKALPEL-SET-BASIS are disabled *)
		    AstSML.progs * Label.label * Id.assoc * int (* the updated option *)

    val convertToFull : string            -> (* file name *)
			Reg.region option -> (* region of the file name in another file*)
			string list       -> (* the list of file names already parsed *)
			string option * messages

end
