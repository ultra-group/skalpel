(* Copyright 2009 Heriot-Watt University
 * Copyright 2010 Heriot-Watt University
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
 *  o Date:        25 May 2010
 *  o File name:   Name.sml
 *  o Description: Defines the structure Name which has signature NAME,
 *      and is used to generate the builtin types of some type constructors.
 *)


structure Name :> NAME = struct

(* abbreviate the names of structures *)
structure T  = Ty
structure E  = Env
structure I  = Id
structure L  = Label
structure CL = ClassId
structure EH = ErrorHandler

fun genCstTyZero ftycons tf lab =
    E.genCstTfEm
	tf
	(T.TFC (T.SC ([], T.noflex (), lab), ftycons lab, lab))
	lab

fun genCstTyOne ftycons tf lab =
    let val tv = T.freshtyvar ()
    in E.genCstTfEm
	   tf
	   (T.TFC (T.SC (T.constuple [tv] lab, T.noflex (), lab), ftycons tv lab, lab))
	   lab
    end

(* construct a type with zero arguments *)
fun constyZ f    lab = f    lab T.BB

(* construct a type with one argument, passing in the type variable *)
fun constyO f tv lab = f tv lab T.BB

(* generate constraints for types with zero arguments (eg string) *)
fun genCstTyZero' ftycons = genCstTyZero (constyZ ftycons)

(* generate constraints for types with one argument (eg list) *)
fun genCstTyOne'  ftycons = genCstTyOne  (constyO ftycons)

(* generates a binding *)
fun genBind id tyf tnKind = E.consBindPoly id (tyf, tnKind, ref (E.emvar, false)) (CL.consTYCON ()) L.builtinLab

(* genBuiltZero is for types which do not require some kind of argument
 * such as string, int, or bool *)
fun genBuildZero f id tnKind =
    let val tyf  = T.newTFV ()
	val c    = genCstTyZero' f tyf L.builtinLab
	val bind = genBind id tyf tnKind
    in (bind, c)
    end

(* genBuildOne is for types which require some kind of argument
 * such as ref, list, or option *)
fun genBuildOne f id tnKind =
    let val tyf  = T.newTFV ()
	val c    = genCstTyOne' f tyf L.builtinLab
	val bind = genBind id tyf tnKind
    in (bind, c)
    end

(* Generates binding for top level type names *)
fun getTyName ascid =
    let val ltnid = List.mapPartial (fn x => case I.lookupSt x ascid of SOME y => SOME (y, x) | _ => NONE) T.tyNames
	fun bindTyName (id, "unit")      = genBuildZero T.constyunit'      id E.TYP
	  | bindTyName (id, "int")       = genBuildZero T.constyint'       id E.TYP
	  | bindTyName (id, "word")      = genBuildZero T.constyword'      id E.TYP
	  | bindTyName (id, "real")      = genBuildZero T.constyreal'      id E.TYP
	  | bindTyName (id, "char")      = genBuildZero T.constychar'      id E.TYP
	  | bindTyName (id, "string")    = genBuildZero T.constystring'    id E.TYP
	  | bindTyName (id, "substring") = genBuildZero T.constysubstring' id E.TYP
	  | bindTyName (id, "exn")       = genBuildZero T.constyexception' id E.TYP
	  | bindTyName (id, "array")     = genBuildOne  T.constyarray'     id E.TYP
	  | bindTyName (id, "vector")    = genBuildOne  T.constyvector'    id E.TYP
	  | bindTyName (id, "ref")       = genBuildOne  T.constyref'       id E.DAT
	  | bindTyName (id, "bool")      = genBuildZero T.constybool'      id E.DAT
	  | bindTyName (id, "option")    = genBuildOne  T.constyoption'    id E.DAT
	  | bindTyName (id, "order")     = genBuildZero T.constyorder'     id E.DAT
	  | bindTyName (id, "list")      = genBuildOne  T.constylist'      id E.DAT
	  | bindTyName (id, "frag")      = genBuildOne  T.constyfrag'      id E.DAT
	  | bindTyName _ = raise EH.DeadBranch "this type name is not a top-level type name"
	val (binds, cs) = ListPair.unzip (map bindTyName ltnid)
    in (E.bindToEnv binds, E.singcsts (L.builtinLab, cs))
    end

end