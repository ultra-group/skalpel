(* Copyright 2010 Heriot-Watt University
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
 *  o Authors:     Vincent Rahli
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        19 June 2010
 *  o File name:   AstMLB.sml
 *  o Description: Abstract syntax for MLB files.
 *)

(** The abstract syntax tree for MLton .mlb files (no signature constraint). *)
structure AstMLB = struct

structure L = Label
structure I = Id
structure R = Reg

(** A string containing a filename. *)
type file = string

(** A datatype nonTerm containing basis declarations, functor/signature/struct identifiers etc.
 * The following constructors are defined:
 * \arg NT_BASDEC. For basis declarations.
 * \arg NT_BASODEC. One basis declaration.
 * \arg NT_BASEEXP. Basis expression.
 * \arg NT_BASBIND. Basis binder.
 * \arg NT_BASSTR. Basis structure.
 * \arg NT_BASSIG. Basis signature.
 * \arg NT_BASFUN. Basis functor.
 * \arg NT_BASID. Basis identifier.
 * \arg NT_STRID. Structure identifier.
 * \arg NT_SIGID. Signature identifier.
 * \arg NT_FUNID. Functor identifier.
 *)
datatype nonTerm   = NT_BASDEC
		   | NT_BASODEC
		   | NT_BASEXP
		   | NT_BASBIND
		   | NT_BASSTR
		   | NT_BASSIG
		   | NT_BASFUN
		   | NT_BASID
		   | NT_STRID
		   | NT_SIGID
		   | NT_FUNID

datatype caseRule  = CR_BASODECBAS  | CR_BASODECOPN  | CR_BASODECLOC
		   | CR_BASODECDSTR | CR_BASODECDSIG | CR_BASODECDFUN
		   | CR_BASODECSML  | CR_BASODECSIG  | CR_BASODECFUN | CR_BASODECMLB
		   | CR_BASDEC
		   | CR_BASBIND
		   | CR_BASSTR | CR_BASSTREQ
		   | CR_BASSIG | CR_BASSIGEQ
		   | CR_BASFUN | CR_BASFUNEQ
		   | CR_BASID
		   | CR_STRID
		   | CR_SIGID
		   | CR_FUNID
		   | CR_BASEXPBAS | CR_BASEXPID | CR_BASEXPLET

(** Node type, can be either a file, an identifier, an error, or a tuple of a #nonTerm and a #caseRule.
 * Has the following constructors:
 * \arg NORMAL. A typical value of a #nonTerm and a #caseRule.
 * \arg ERROR. An error, a string value.
 * \arg IDENT. An identifier, an #Id.id value.
 * \arg FILE. A #file value.
 *)
datatype nodeType  = NORMAL of nonTerm * caseRule
		   | ERROR  of string
		   | IDENT  of I.id
		   | FILE   of file

(** Holds portions of MLB files, has a single constructor NODE which is a quadruple of a #nodeType, #code, #Region.region and #Label.label. *)
datatype code      = NODE   of nodeType      *
			       code     list *
			       R.region list *
			       L.label

(** A type for storing files, a list of string * #code values. *)
type files         = (string * code) list

end
