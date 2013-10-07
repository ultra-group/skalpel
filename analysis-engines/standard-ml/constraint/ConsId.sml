(* Copyright 2009 2010 2012 Heriot-Watt University
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
 *  o File name:   ConsId.sml
 *)

(** Defines the structure ConsId which is constrained opaquely by refstruct{CONSID} -  this structure is to deal with binders. *)
structure ConsId :> CONSID = struct

(* shorten names of structures *)
structure L  = Label
structure T  = Ty
structure P  = Poly
structure I  = Id
structure CL = ClassId
structure EH = ErrorHandler

(** A binder is used for program occurences of id that are being bound, there are six record fields.
 * Each field is described below:
 * \arg id Identifier for which we generate this constraint (of type #Id.id).
 * \arg bind Type of the identifier (of type 'a).
 * \arg equalityTypeVar An equality type variable for the binding itself (of type #Ty.equalityTypeVar).
 * \arg class Status/class of the identifier (of type #ClassId.class).
 * \arg lab Label of the constraint (of type #Label.label).
 * \arg poly Constraint on the poly/mono binding if binding (of type #Poly.poly). *)
type 'a bind = {id    : I.id,
		bind  : 'a,
		equalityTypeVar : Ty.equalityTypeVar,
		class : CL.class,
		lab   : L.label,
		poly  : P.poly}

(** Gets the ID field value from a binding (#bind). *)
fun getBindI (x : 'a bind) = #id    x
(** Gets the binding field value from a binding (#bind). *)
fun getBindT (x : 'a bind) = #bind  x
(** Gets the equality type variable field value from a binding (#bind). *)
fun getBindEqualityTypeVar (x : 'a bind) = #equalityTypeVar  x
(** Gets the status/class field value from a binding (#bind). *)
fun getBindC (x : 'a bind) = #class x
(** Gets the label of a binding (#bind). *)
fun getBindL (x : 'a bind) = #lab   x
(** Gets the poly status of a binding (#bind). *)
fun getBindP (x : 'a bind) = #poly  x


(** Constructs a #bind vaue, given an #Id.id, a bind (can be anything), a #Ty.equalityTypeVar, #ClassId.class, a #Label.label, and a #Poly.poly status. *)
fun consBind id bind equalityTypeVar class lab poly = {id = id, bind  = bind, equalityTypeVar = equalityTypeVar, class = class, lab   = lab, poly  = poly}
(** Constructs a #bind value using #consBind, setting the polymorphic status to POLY, indicating polymorphism. *)
fun consBindPoly id bind equalityTypeVar class lab = consBind id bind equalityTypeVar class lab P.POLY
(** Constructs a #bind value using #consBind, setting the polymorphic status to MONO, indicating monomorphism. *)
fun consBindMono id bind equalityTypeVar class lab = consBind id bind equalityTypeVar class lab (P.MONO [P.MONBIN (L.singleton lab)])

(** Takes a binding and turns it into a monomorphic binding. *)
fun toMonoBind {id, bind, equalityTypeVar, class, lab, poly} labs = consBind id bind equalityTypeVar class lab (P.polyToMono poly labs)
(** Takes a binding and turns it into a polymorphic binding. *)
fun toPolyBind {id, bind, equalityTypeVar, class, lab, poly}      = consBind id bind equalityTypeVar class lab (P.toPoly poly)
(** Sets the poly field of a #bind value to POLY. *)
fun resetPoly  {id, bind, equalityTypeVar, class, lab, poly}      = consBind id bind equalityTypeVar class lab P.POLY
(** Given a #bind value and a class value C, will set the class field inside the #bind to the value C. *)
fun updClass   {id, bind, equalityTypeVar, class, lab, poly} cl   = consBind id bind equalityTypeVar cl    lab poly
(** Given a #bind value and a poly value P, will set the poly field inside the #bind to the value P. *)
fun updPoly    {id, bind, equalityTypeVar, class, lab, poly} pol  = consBind id bind equalityTypeVar class lab pol

(** Gets the type variables inside the bind portion of a #bind value. *)
fun getTypeVar {id, bind = T.TYPE_VAR (tv, _, _, _), equalityTypeVar, class, lab, poly} = SOME tv
  | getTypeVar {id, bind, equalityTypeVar, class, lab, poly} =
    (*(2010-07-02) This is not totally safe, we should really return the
     * list of types in bind. *)
    if CL.classIsEXC class
    then NONE
    else (print (I.printId id ^ " " ^ T.printty bind ^ "\n"); raise EH.DeadBranch "")

(** Calls #getTypeVar to strip the type variables out of the bind portion of a #bind value. *)
fun getTypeVars {id, bind, equalityTypeVar, class, lab, poly} = T.getTypeVarsTy bind

(** Calls #ClassId.classIsVAL on the class of a #bind value. *)
fun isVAL {id, bind, equalityTypeVar, class, lab, poly} = CL.classIsVAL class
(** Calls #ClassId.classIsPAT on the class of a #bind value. *)
fun isPAT {id, bind, equalityTypeVar, class, lab, poly} = CL.classIsPAT class
(** Calls #ClassId.classIsDA0 on the class of a #bind value. *)
fun isDA0 {id, bind, equalityTypeVar, class, lab, poly} = CL.classIsDA0 class
(** Calls #ClassId.classIsREC on the class of a #bind value. *)
fun isREC {id, bind, equalityTypeVar, class, lab, poly} = CL.classIsREC class
(** Calls #ClassId.isEX0 on the class of a #bind value. *)
fun isEX0 {id, bind, equalityTypeVar, class, lab, poly} = CL.classIsEX0 class

(** Changes a #bind value to update the class field by using #ClassId.classToPAT on it. *)
fun toPAT {id, bind, equalityTypeVar, class, lab, poly} = consBind id bind equalityTypeVar (CL.classToPAT class) lab poly
(** Changes a #bind value to update the class field by using #ClassId.classToREC on it. *)
fun toREC {id, bind, equalityTypeVar, class, lab, poly} = consBind id bind equalityTypeVar (CL.classToREC class) lab poly
(** Changes a #bind value to update the class field by using #ClassId.classToVRC on it. *)
fun toVRC {id, bind, equalityTypeVar, class, lab, poly} = consBind id bind equalityTypeVar (CL.classToVRC class) lab poly
(** Changes a #bind value to update the class field by using #ClassId.classToDA0 on it. *)
fun toDA0 {id, bind, equalityTypeVar, class, lab, poly} = consBind id bind equalityTypeVar (CL.classToDA0 class) lab poly
(** Changes a #bind value to update the class field by using #ClassId.classToDA1 on it. *)
fun toDA1 {id, bind, equalityTypeVar, class, lab, poly} = consBind id bind equalityTypeVar (CL.classToDA1 class) lab poly
(** Changes a #bind value to update the class field by using #ClassId.classToDAT on it. *)
fun toDAT {id, bind, equalityTypeVar, class, lab, poly} = consBind id bind equalityTypeVar (CL.classToDAT class) lab poly
(** Changes a #bind value to update the class field by using #ClassId.classToEX0 on it. *)
fun toEX0 {id, bind, equalityTypeVar, class, lab, poly} = consBind id bind equalityTypeVar (CL.classToEX0 class) lab (P.polyToMono poly L.empty)
(** Changes a #bind value to update the class field by using #ClassId.classToEX1 on it. *)
fun toEX1 {id, bind, equalityTypeVar, class, lab, poly} = consBind id bind equalityTypeVar (CL.classToEX1 class) lab (P.polyToMono poly L.empty)

(** Given a #bind value and a class value, constructs a new binding with that class value, throwing away the old one. *)
fun toCLS {id, bind, class, equalityTypeVar, lab, poly} cls = consBind id bind equalityTypeVar cls lab poly

(** Take a #bind value and a function, and maps that function over the bind field of the binding. *)
fun mapBind {id, bind, equalityTypeVar, class, lab, poly} f = consBind id (f bind) equalityTypeVar class lab poly

(** Updates an identifier constraint with information on the expansiveness of the bound expression. *)
fun closeBind {id, bind, equalityTypeVar, class, lab, poly} clos =
    consBind id bind equalityTypeVar class lab (P.mergePoly (P.fromNonexpToPoly clos) poly)


(* resets the names of a recursive function if the constraint if for a recursive function, otherwise does nothing. *)
(*fun setREClabs (x as {id, bind, class, lab, poly}) labs =
    if CL.classIsREC class
    then consBind id bind (CL.setClassREClabs class labs) lab poly
    else x*)


(** Gather the labels in a set of extended type variables. *)
fun getlabExtChk xs =
    foldr (fn (x, y) => L.cons (getBindL x) y)
	  L.empty
	  xs


(** Gather the labels of EXCs in a list of extended type variables. *)
fun getlabExtChkExc xs =
    foldr (fn ({id, bind, class, lab, poly}, y) =>
	      if CL.classIsEX0 class
	      then (P.getLabsPoly poly) :: y
	      else raise EH.DeadBranch "")
	  []
	  xs


(** Given a label, turns it into a string value. *)
fun printlabel l = "l"  ^ L.printLab l

(** Prints out a #bind value. *)
fun printBind {id, bind, equalityTypeVar, poly, lab, class} fbind assoc =
    "(id = " ^ I.printId'  id assoc ^
    ", bind = " ^ fbind       bind     ^
    ", equalityTypeVar = " ^ T.printEqualityTypeVar equalityTypeVar     ^
    ", poly = " ^ P.toString  poly     ^
    ", lab = " ^ L.printLab  lab      ^
    ", class = " ^ CL.toString class    ^ ")"

(** Given a #bind value and a function to print the bind field of the #bind value, calls #printBind with the empty association list (#Id.emAssoc). *)
fun printBind' bind fbind = printBind bind fbind I.emAssoc

end
