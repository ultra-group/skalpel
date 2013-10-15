(* Copyright 2009 2010 2011 2012 Heriot-Watt University
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
 *  o File name:   Op.sml
 *)

(** Defines the structure Op which has signature OP and is used to generate builtin types of some operators (constrained opaqualy by refstruct{OP}). *)
structure Op :> OP = struct

(* shorten the names of some structures *)
structure I  = Id
structure T  = Ty
structure E  = Env
structure L  = Label
structure CL = ClassId
structure EH = ErrorHandler

(** Operators taking two arguments. *)
val binaryst = ["=", "<>", ">", "<", ">=", "<=",
		"+", "-", "*", "/", "div", "mod",
		"@", "^", ":=", "o", "::", "before"]

(** One argument applied to each operator only. *)
val unaryst  = ["~", "!"]

(** Return the item in the splay map mapped by x. *)
fun binaryid assoc = List.mapPartial
			 (fn x => case I.lookupSt x assoc of
				      NONE => NONE
				    | SOME y => SOME (y, x))
			 binaryst
fun unaryid assoc  = List.mapPartial
			 (fn x => case I.lookupSt x assoc of
				      NONE => NONE
				    | SOME y => SOME (y, x))
			 unaryst

fun getOpType ascid =
    let
	(** Returns three tuple of type on the left, type on the right, and type of the operator. *)
	fun getTyBinaryOp "=old" l =
	    let val tl = T.constyint'  l T.BUILTIN_BASIS_CONS
		val tr = T.constyint'  l T.BUILTIN_BASIS_CONS
		val to = T.constybool' l T.BUILTIN_BASIS_CONS
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp "<>" l =
	    let val tv = T.freshTypeVar ()
		val tl = T.consTYPE_VAR tv
		val tr = T.consTYPE_VAR tv
		val to = T.constybool' l T.BUILTIN_BASIS_CONS
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp ">" l =
	    let val tl = T.constyint'  l T.BUILTIN_BASIS_CONS
		val tr = T.constyint'  l T.BUILTIN_BASIS_CONS
		val to = T.constybool' l T.BUILTIN_BASIS_CONS
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp "<" l =
	    let val tl = T.constyint'  l T.BUILTIN_BASIS_CONS
		val tr = T.constyint'  l T.BUILTIN_BASIS_CONS
		val to = T.constybool' l T.BUILTIN_BASIS_CONS
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp ">=" l =
	    let val tl = T.constyint'  l T.BUILTIN_BASIS_CONS
		val tr = T.constyint'  l T.BUILTIN_BASIS_CONS
		val to = T.constybool' l T.BUILTIN_BASIS_CONS
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp "<=" l =
	    let val tl = T.constyint'  l T.BUILTIN_BASIS_CONS
		val tr = T.constyint'  l T.BUILTIN_BASIS_CONS
		val to = T.constybool' l T.BUILTIN_BASIS_CONS
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp "+" l =
	    let val tl = T.constyint' l T.BUILTIN_BASIS_CONS
		val tr = T.constyint' l T.BUILTIN_BASIS_CONS
		val to = T.constyint' l T.BUILTIN_BASIS_CONS
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp "-" l =
	    let val tl = T.constyint' l T.BUILTIN_BASIS_CONS
		val tr = T.constyint' l T.BUILTIN_BASIS_CONS
		val to = T.constyint' l T.BUILTIN_BASIS_CONS
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp "*" l =
	    let val tl = T.constyint' l T.BUILTIN_BASIS_CONS
		val tr = T.constyint' l T.BUILTIN_BASIS_CONS
		val to = T.constyint' l T.BUILTIN_BASIS_CONS
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp "/" l =
	    let val tl = T.constyreal' l T.BUILTIN_BASIS_CONS
		val tr = T.constyreal' l T.BUILTIN_BASIS_CONS
		val to = T.constyreal' l T.BUILTIN_BASIS_CONS
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp "div" l =
	    let val tl = T.constyint' l T.BUILTIN_BASIS_CONS
		val tr = T.constyint' l T.BUILTIN_BASIS_CONS
		val to = T.constyint' l T.BUILTIN_BASIS_CONS
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp "mod" l =
	    let val tl = T.constyint' l T.BUILTIN_BASIS_CONS
		val tr = T.constyint' l T.BUILTIN_BASIS_CONS
		val to = T.constyint' l T.BUILTIN_BASIS_CONS
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp "@" l =
	    let val tv = T.freshTypeVar ()
		val tl = T.constylist' tv l T.BUILTIN_BASIS_CONS
		val tr = T.constylist' tv l T.BUILTIN_BASIS_CONS
		val to = T.constylist' tv l T.BUILTIN_BASIS_CONS
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp "^" l =
	    let val tl  = T.constystring' l T.BUILTIN_BASIS_CONS
		val tr  = T.constystring' l T.BUILTIN_BASIS_CONS
		val to  = T.constystring' l T.BUILTIN_BASIS_CONS
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp ":=" l =
	    let val tv = T.freshTypeVar ()
		val tl = T.constyref' tv l T.BUILTIN_BASIS_CONS
		val tr = T.consTYPE_VAR tv
		val to = T.constytuple' [] l T.BUILTIN_BASIS_CONS
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp "o" l =
	    let val tv1 = T.freshTypeVar ()
		val tv2 = T.freshTypeVar ()
		val tv3 = T.freshTypeVar ()
		val tl = T.constyarrow' tv2 tv3 l T.BUILTIN_BASIS_CONS
		val tr = T.constyarrow' tv1 tv2 l T.BUILTIN_BASIS_CONS
		val to = T.constyarrow' tv1 tv3 l T.BUILTIN_BASIS_CONS
            in (tl, tr, to)
            end
	  | getTyBinaryOp "before" l =
	    let val tv = T.freshTypeVar ()
		val tl = T.consTYPE_VAR tv
		val tr = T.constyrecord' [] NONE l T.BUILTIN_BASIS_CONS
		val to = T.consTYPE_VAR tv
	    in (tl, tr, to)
            end
	  | getTyBinaryOp "::" l =
	    let val tv = T.freshTypeVar ()
		val tl = T.consTYPE_VAR tv
		val tr = T.constylist' tv l T.BUILTIN_BASIS_CONS
		val to = T.constylist' tv l T.BUILTIN_BASIS_CONS
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp __ = raise EH.DeadBranch "DeadBranch110"
	fun getTyUnaryOp "~" l =
	    let val ti = T.constyint' l T.BUILTIN_BASIS_CONS
		val to = T.constyint' l T.BUILTIN_BASIS_CONS
	    in (ti, to) (* type in and type out *)
	    end
	  | getTyUnaryOp "!" l =
	    let val tv = T.freshTypeVar ()
		val ti = T.constyref' tv l T.BUILTIN_BASIS_CONS
		val to = T.consTYPE_VAR tv
	    in (ti, to) (* type in and type out *)
	    end
	  | getTyUnaryOp _ _ = raise EH.DeadBranch ""
	fun scanBinary (id, str) =
	    let val lab = L.builtinLab
		val ((tl,tr,to), cst) = case str of
				     "=" =>
				     let val tv = T.freshTypeVar ()
					 val eqtv = T.freshEqualityTypeVar()
					 val equalityConstraint = E.initEqualityTypeConstraint (T.consEQUALITY_TYPE_VAR eqtv) (T.EQUALITY_TYPE_STATUS T.EQUALITY_TYPE) lab
					 val tl = T.consTYPE_VARwithEQ tv (T.consEQUALITY_TYPE_VAR eqtv)
					 val tr = T.consTYPE_VARwithEQ tv (T.consEQUALITY_TYPE_VAR eqtv)
					 val to = T.constybool' lab T.BUILTIN_BASIS_CONS
				     in ((tl, tr, to), [equalityConstraint])
				     end
				   | _ =>
				     (getTyBinaryOp str lab, [])
		val ty  = T.newTYPE_VAR ()
		val eqtv = T.freshEqualityTypeVar()
		val ti  = T.consTyTupleTy [tl, tr] lab T.BUILTIN_BASIS_CONS
		val c   = E.initTypeConstraint ty (T.consTyArrowTy ti to lab T.BUILTIN_BASIS_CONS) lab
		(* :: is the only constructor *)
		val cl  = if str = "::" then CL.consDA1 () else CL.consREC ()
		val bd  = E.consBindPoly {id=id, typeOfId=ty, equalityTypeVar = eqtv, classOfId=cl, labelOfConstraint=lab}
	    in (bd, [c]@cst)
	    end
	fun scanUnary (id, str) =
	    let val lab = L.builtinLab
		val (ti, to) = getTyUnaryOp str lab
		val ty  = T.newTYPE_VAR ()
		val c   = E.initTypeConstraint ty (T.consTyArrowTy ti to lab T.BUILTIN_BASIS_CONS) lab
		val bd  = E.consBindPoly {id=id,
					  typeOfId=ty,
					  equalityTypeVar=T.freshEqualityTypeVar(),
					  classOfId=(CL.consREC ()),
					  labelOfConstraint=lab}
	    in (bd, c)
	    end
	val (binds1, cst1) = foldr (fn (id, (binds, cs)) =>
				       let val (bd, c) = scanBinary id
				       in (bd :: binds, c @ cs) end)
				   ([], [])
				   (binaryid ascid)
	val (binds2, cst2) = foldr (fn (id, (binds, cs)) =>
				       let val (bd, c) = scanUnary id
				       in (bd :: binds, c :: cs) end)
				   ([], [])
				   (unaryid ascid)
    in (E.bindToEnv (binds1 @ binds2), E.singcsts (L.builtinLab, cst1 @ cst2))
    end

end
