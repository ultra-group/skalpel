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
 *  o Description: Defines the structure ConsId which has signature
 *      CONSID.  This structure is to deal with binders.
 *)


structure ConsId :> CONSID = struct

(* shorten names of structures *)
structure L  = Label
structure T  = Ty
structure P  = Poly
structure I  = Id
structure CL = ClassId
structure EH = ErrorHandler

(* a binder is used for program occurences of id that are being bound*)
type 'a bind = {id    : I.id,        (* Identifier for which we generate this constraint *)
		bind  : 'a,          (* Type of the identifier                           *)
		class : CL.class,    (* Status/class of the identifier                   *)
		lab   : L.label,     (* Label of the contraint                           *)
		poly  : P.poly}      (* Constraint on the poly/mono binding if binding   *)

(* grabs a different field from bind *)
fun getBindI (x : 'a bind) = #id    x
fun getBindT (x : 'a bind) = #bind  x
fun getBindC (x : 'a bind) = #class x
fun getBindL (x : 'a bind) = #lab   x
fun getBindP (x : 'a bind) = #poly  x

(*fun getREClabs {id, scope, bind, class, lab, poly} = (lab, CL.getClassREClabs class)*)

(* constructors of bind *)
fun consBind id bind class lab poly =
    {id    = id,
     bind  = bind,
     class = class,
     lab   = lab,
     poly  = poly}
fun consBindPoly id bind class lab = consBind id bind class lab P.POLY
fun consBindMono id bind class lab = consBind id bind class lab (P.MONO [P.MONBIN (L.singleton lab)])

fun toMonoBind {id, bind, class, lab, poly} labs = consBind id bind class lab (P.polyToMono poly labs)
fun toPolyBind {id, bind, class, lab, poly}      = consBind id bind class lab (P.toPoly poly)
fun resetPoly  {id, bind, class, lab, poly}      = consBind id bind class lab P.POLY
fun updClass   {id, bind, class, lab, poly} cl   = consBind id bind cl    lab poly
fun updPoly    {id, bind, class, lab, poly} pol  = consBind id bind class lab pol

(* get the type variables from the record *)
fun getTypeVar {id, bind = T.TYPE_VAR (tv, _, _, _), class, lab, poly} = SOME tv
  | getTypeVar {id, bind, class, lab, poly} =
    (*(2010-07-02) This is not totally safe, we should really return the
     * list of types in bind. *)
    if CL.classIsEXC class
    then NONE
    else (print (I.printId id ^ " " ^ T.printty bind ^ "\n"); raise EH.DeadBranch "")

fun getTypeVars {id, bind, class, lab, poly} = T.getTypeVarsTy bind

(* more checking to see what class the parameter is *)
fun isVAL {id, bind, class, lab, poly} = CL.classIsVAL class
fun isPAT {id, bind, class, lab, poly} = CL.classIsPAT class
fun isDA0 {id, bind, class, lab, poly} = CL.classIsDA0 class
fun isREC {id, bind, class, lab, poly} = CL.classIsREC class
(*fun isTYP {id, bind, class, lab, poly} = CL.classIsTYP class
fun isDAT {id, bind, class, lab, poly} = CL.classIsDAT class*)
fun isEX0 {id, bind, class, lab, poly} = CL.classIsEX0 class

(* SIMPLE TRANSFORMATIONS *)
fun toPAT {id, bind, class, lab, poly} = consBind id bind (CL.classToPAT class) lab poly
fun toREC {id, bind, class, lab, poly} = consBind id bind (CL.classToREC class) lab poly
fun toVRC {id, bind, class, lab, poly} = consBind id bind (CL.classToVRC class) lab poly
fun toDA0 {id, bind, class, lab, poly} = consBind id bind (CL.classToDA0 class) lab poly
fun toDA1 {id, bind, class, lab, poly} = consBind id bind (CL.classToDA1 class) lab poly
fun toDAT {id, bind, class, lab, poly} = consBind id bind (CL.classToDAT class) lab poly
fun toEX0 {id, bind, class, lab, poly} = consBind id bind (CL.classToEX0 class) lab (P.polyToMono poly L.empty)
fun toEX1 {id, bind, class, lab, poly} = consBind id bind (CL.classToEX1 class) lab (P.polyToMono poly L.empty)
(*fun toDAT {id, bind, class, lab, poly} = consBind id bind (CL.classToDATcons class CL.emCons) lab poly*)

fun toCLS {id, bind, class, lab, poly} cls = consBind id bind cls lab poly

fun mapBind {id, bind, class, lab, poly} f = consBind id (f bind) class lab poly

(* COMPLEX TRANSFORMATIONS *)

(*fun toCONlab  {id, bind, class, lab, poly} l    = consBind id bind (CL.classToCON class) lab poly
fun toEXClab  {id, bind, class, lab, poly} l    = consBind id bind (CL.classToEXC class) lab (P.polyToMono poly (L.ord [l, lab]))
(*fun toVALlabs {id, bind, class, lab, poly} labs = consBind id bind (CL.classToVAL class) lab poly (* that for the "as" *)*)
fun toPATlabs {id, bind, class, lab, poly} labs = consBind id bind (CL.classToPAT class) lab poly
fun toREClabs {id, bind, class, lab, poly} labs = consBind id bind (CL.classToREC class) lab poly
(*fun toDATcons {id, bind, class, lab, poly} cons = consBind id bind (CL.classToDATcons class cons)     lab poly*)
(*fun toSTRstr  {id, bind, class, lab, poly} str  = consBind id bind (CL.classToSTRstr  class str)      lab poly
fun toSIGstr  {id, bind, class, lab, poly} str  = consBind id bind (CL.classToSIGstr  class str)      lab poly*)*)


(* returns the labels associated to an as in a pattern that constrain
 * an identifier to be a value variable *)
(*fun getPATval x = CL.getClassPATval (getBindC x)*)


(* These functions returns the constructors of a datatype and raises
 * and exception if the constrained id is not for a datatype *)

(*fun getDATcons x = CL.getClassDATcons (getBindC x)*)


(* generation of monomorphic constraints with special care of REC *)

(*fun genMonREC x labs =
    let val (l, labs1) = getREClabs x
	val labs2 = if L.isEmpty labs1 then L.singleton l else labs1
    in L.foldr (fn (x, y) => (P.MONBIN (L.cons x labs)) :: y) [] labs2 end*)


(* update an identifier constraint with information on the expansiveness
 * of the bound expression *)
fun closeBind {id, bind, class, lab, poly} clos =
    consBind id bind class lab (P.mergePoly (P.fromNonexpToPoly clos) poly)


(* Resets the names of a recursive function if the constraint if for
 * a recursive function, otherwise does nothing. *)

(*fun setREClabs (x as {id, bind, class, lab, poly}) labs =
    if CL.classIsREC class
    then consBind id bind (CL.setClassREClabs class labs) lab poly
    else x*)


(* gather the labels in a set of extended type variables *)
fun getlabExtChk xs =
    foldr (fn (x, y) => L.cons (getBindL x) y)
	  L.empty
	  xs


(* gather the labels of EXCs in a list of extended type variables *)
fun getlabExtChkExc xs =
    foldr (fn ({id, bind, class, lab, poly}, y) =>
	      if CL.classIsEX0 class
	      then (P.getLabsPoly poly) :: y
	      else raise EH.DeadBranch "")
	  []
	  xs


(* gather the labels of RECs in a set of extended type variables *)
(*fun getlabExtChkRec xs =
    foldr
	(fn (x, y) =>
	    case CL.getClassREC (getBindC x) of
		SOME (ll, rf) => L.concat (Option.getOpt (rf, ll)) y
	      (*let
		   val st = A.getSetStatusNonexp nonexp
	       in if L.isEmpty st
		  then L.concat ll y
		  else L.concat st y
	       end*)
	      | NONE => y)
	L.empty
	xs*)


(* PRINTING SECTION *)
fun printlistgen xs f = "[" ^ #1 (foldr (fn (t, (s, c)) => (f t ^ c ^ s, ",")) ("", "") xs) ^ "]"

fun printlabel l = "l"  ^ L.printLab l

fun printBind {id, bind, poly, lab, class} fbind assoc =
    "(" ^ I.printId'  id assoc ^
    "," ^ fbind       bind     ^
    "," ^ P.toString  poly     ^
    "," ^ L.printLab  lab      ^
    "," ^ CL.toString class    ^ ")"

fun printBind' bind fbind = printBind bind fbind I.emAssoc

end
