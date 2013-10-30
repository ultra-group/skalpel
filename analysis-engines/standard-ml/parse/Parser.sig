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
 *)

(** Defines the signature PARSER to parse SML code and basis files (used by refstruct{Parser}). *)
signature PARSER = sig

    type messages = (string * string * Reg.region list) list

    val parse : string          ->
		TextIO.instream ->
		Label.label     ->
		Id.assoc        ->
		AstSML.prog * Label.label * Id.assoc

    val consProgs : string list ->
		    string list ->
		    Label.label ->
		    Id.assoc    ->
		    int         ->
		    bool        ->
		    AstSML.progs * Label.label * Id.assoc * int

    val convertToFull : string            ->
			Reg.region option ->
			string list       ->
			string option * messages

end
