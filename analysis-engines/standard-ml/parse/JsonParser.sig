(* Copyright 2011 Heriot-Watt University
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
 *  o Authors:     John Pirie
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        05 November 2011
 *  o File name:   ParseJson.sml
 *)

(** A signature used by refstruct{JsonParser} to parse test answer JSON files. *)
signature JSONPARSER =
sig
    type oneerror

    type error = {errors       : {labels       : int * int list,
				  assumptions  : LongId.keyOut list,
				  kind         : ErrorKind.kind,
				  slice        : string,
				  time         : LargeInt.int,
				  identifier   : int,
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

    val parseTest : string -> error
    val parseTestControlFile : string -> string list
end
