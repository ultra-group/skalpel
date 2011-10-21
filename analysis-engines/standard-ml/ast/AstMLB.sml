(* Copyright 2010 Heriot-Watt University
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
 *  o Date:        19 June 2010
 *  o File name:   AstMLB.sml
 *  o Description: Abstract syntax for MLB files.
 *)


structure AstMLB = struct

structure L = Label
structure I = Id
structure R = Reg

type file = string
type bas  = bool

datatype nonTerm   = NT_BASDEC  (* basis declaration     *)
		   | NT_BASODEC (* one basis declaration *)
		   | NT_BASEXP  (* basis expression      *)
		   | NT_BASBIND (* basis binder          *)
		   | NT_BASSTR  (* basis structure       *)
		   | NT_BASSIG  (* basis signature       *)
		   | NT_BASFUN  (* basis functor         *)
		   | NT_BASID   (* basis identifier      *)
		   | NT_STRID   (* structure identifier  *)
		   | NT_SIGID   (* signature identifier  *)
		   | NT_FUNID   (* functor identifier    *)
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


datatype nodeType  = NORMAL of nonTerm * caseRule
		   | ERROR  of string
		   | IDENT  of I.id
		   | FILE   of file
datatype code      = NODE   of nodeType      *
			       code     list *
			       R.region list *
			       L.label
type files         = (string * code) list

end
