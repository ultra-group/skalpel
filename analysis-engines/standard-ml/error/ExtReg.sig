(* Copyright 2009 2010 2011 Heriot-Watt University
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
 *  o Authors:     Vincent Rahli
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        24 May 2010
 *  o File name:   ExtReg.sig
 *)

(** Defines the signature EXTREG which specifies the extended regions with colours, used by refstruct{ExtReg}. *)
signature EXTREG = sig

    datatype color   = Red | Blue | Purple | Green | Orange | Yellow

    type weight      = int
    type file        = string
    datatype treeReg = L of Reg.region * color * weight
		     | H of Reg.region * color * weight
		     | N of Reg.region * color * weight * treeReg list
    type fileReg     = file * treeReg list
    type regs        = fileReg list

    val getExtRegLine    : int -> treeReg list -> treeReg list (* used in Html.sml *)
    val delNegRegs       : regs -> regs
    val consWeightRegs   : regs -> regs -> regs * regs

    val checkSameRegs    : regs -> regs -> bool
    val checkSimRegs     : regs -> regs -> bool

    val simplify         : regs -> bool -> regs

    val getpos_progs     : AstSML.progs -> (ErrorKind.kind * Label.labels) -> regs

    val printOneRegs     : regs -> string
    val printSmlExtRegs  : regs -> string
    val printJsonExtRegs : regs -> string
    val printLispExtRegs : regs -> string
    val printPerlExtRegs : regs -> string
    val printBashExtRegs : regs -> unit

    val extRegsToJson : regs -> JSON.value
    val extRegListToJson : treeReg list -> JSON.value
    val getExplodedLinesJson : TextIO.instream -> JSON.value
end
