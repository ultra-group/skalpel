(* Copyright 2009 Heriot-Watt University
 * Copyright 2010 Heriot-Watt University
 * Copyright 2011 Heriot-Watt University
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
 *  o Date:        24 May 2010
 *  o File name:   ExtReg.sig
 *  o Description: Defines the signature EXTREG which specifies
 *      the extended regions with colours.
 *)


signature EXTREG = sig

    datatype color   = Red    (* for any error location              *)
		     | Blue   (* for one end point of a clash        *)
		     | Purple (* for one end point of a clash        *)
		     | Green  (* for shared fields in record clashes *)
		     | Orange (* for location forcing statuses       *)
		     | Yellow (* for parsing error locations         *)
    (* in treeReg we want weights as well *)
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
    (* checkSameExtRegs checks if regions are equal up to weights *)
    val checkSameRegs    : regs -> regs -> bool
    val checkSimRegs     : regs -> regs -> bool
    (* the Boolean has to be true if we want to merge consecutive similar regions *)
    val simplify         : regs -> bool -> regs
    (* What are the labels used for? *)
    val getpos_progs     : AstSML.progs -> (ErrorKind.kind * Label.labels) -> regs

    val printOneRegs     : regs -> string
    val printSmlExtRegs  : regs -> string
    val printJsonExtRegs : regs -> string
    val printLispExtRegs : regs -> string
    val printPerlExtRegs : regs -> string

    (*val getFormatPos     : (AstSML.progs * Error.error) list -> regs list*)
    (*val printExtRegSp    : treeReg list -> string*)
    (*val printExtRegPPSp  : treeReg list -> string*)
    (*val printColRegs     : treeReg list list -> string*)

end
