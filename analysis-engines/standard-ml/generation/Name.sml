(* Copyright 2009 2010 2012 Heriot-Watt University
 *
 * This file is part of the ULTRA SML Type Error Slicer (SMLTES) -
 * a Type Error Slicer for Standard ML written by the ULTRA Group of
 * Heriot-Watt University, Edinburgh.
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
    E.initFunctionTypeConstraint
	tf
	(T.TFC (T.ROW_C ([], T.noflex (), lab), ftycons lab, lab))
	lab

fun genCstTyOne ftycons tf lab =
    let val tv = T.freshTypeVar ()
    in E.initFunctionTypeConstraint
	   tf
	   (T.TFC (T.ROW_C (T.constuple [tv] lab, T.noflex (), lab), ftycons tv lab, lab))
	   lab
    end

(* construct a type with zero arguments *)
fun constyZ f    lab = f    lab T.BUILTIN_BASIS_CONS

(* construct a type with one argument, passing in the type variable *)
fun constyO f tv lab = f tv lab T.BUILTIN_BASIS_CONS

(* generate constraints for types with zero arguments (eg string) *)
fun genCstTyZero' ftycons = genCstTyZero (constyZ ftycons)

(* generate constraints for types with one argument (eg list) *)
fun genCstTyOne'  ftycons = genCstTyOne  (constyO ftycons)

(* generates a binding *)
fun genBind id tyf tnKind = E.consBindPoly {id=id,
					    typeOfId=(tyf, tnKind, ref (E.emvar, false)),
					    classOfId=(CL.consTYCON ()),
					    labelOfConstraint=L.builtinLab}

(* genBuiltZero is for types which do not require some kind of argument
 * such as string, int, or bool *)
fun genBuildZero f id tnKind =
    let val tyf  = T.newTYPE_FUNCTION_VAR ()
	val c    = genCstTyZero' f tyf L.builtinLab
	val bind = genBind id tyf tnKind
    in (bind, c)
    end

(* genBuildOne is for types which require some kind of argument
 * such as ref, list, or option *)
fun genBuildOne f id tnKind =
    let val tyf  = T.newTYPE_FUNCTION_VAR ()
	val c    = genCstTyOne' f tyf L.builtinLab
	val bind = genBind id tyf tnKind
    in (bind, c)
    end

(* Generates binding for top level type names *)
fun getTypename ascid =
    let val ltnid = List.mapPartial (fn x => case I.lookupSt x ascid of SOME y => SOME (y, x) | _ => NONE) T.typenames
	fun bindTyName (id, "unit")      = genBuildZero T.constyunit'      id E.TYPE
	  | bindTyName (id, "int")       = genBuildZero T.constyint'       id E.TYPE
	  | bindTyName (id, "word")      = genBuildZero T.constyword'      id E.TYPE
	  | bindTyName (id, "real")      = genBuildZero T.constyreal'      id E.TYPE
	  | bindTyName (id, "char")      = genBuildZero T.constychar'      id E.TYPE
	  | bindTyName (id, "string")    = genBuildZero T.constystring'    id E.TYPE
	  | bindTyName (id, "substring") = genBuildZero T.constysubstring' id E.TYPE
	  | bindTyName (id, "exn")       = genBuildZero T.constyexception' id E.TYPE
	  | bindTyName (id, "array")     = genBuildOne  T.constyarray'     id E.TYPE
	  | bindTyName (id, "vector")    = genBuildOne  T.constyvector'    id E.TYPE
	  | bindTyName (id, "ref")       = genBuildOne  T.constyref'       id E.DATATYPE
	  | bindTyName (id, "bool")      = genBuildZero T.constybool'      id E.DATATYPE
	  | bindTyName (id, "option")    = genBuildOne  T.constyoption'    id E.DATATYPE
	  | bindTyName (id, "order")     = genBuildZero T.constyorder'     id E.DATATYPE
	  | bindTyName (id, "list")      = genBuildOne  T.constylist'      id E.DATATYPE
	  | bindTyName (id, "frag")      = genBuildOne  T.constyfrag'      id E.DATATYPE
	  | bindTyName _ = raise EH.DeadBranch "this type name is not a top-level type name"
	val (binds, cs) = ListPair.unzip (map bindTyName ltnid)
    in (E.bindToEnv binds, E.singcsts (L.builtinLab, cs))
    end

end
