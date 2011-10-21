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
 *  o File name:   Op.sml
 *  o Description: Defines the structure Op which has signature OP and
 *      is used to generate builtin types of some operators.
 *)


structure Op :> OP = struct

(* shorten the names of some structures *)
structure I  = Id
structure T  = Ty
structure E  = Env
structure L  = Label
structure CL = ClassId
structure EH = ErrorHandler

(* operators taking two arguments *)
val binaryst = ["=", "<>", ">", "<", ">=", "<=",
		"+", "-", "*", "/", "div", "mod",
		"@", "^", ":=", "o", "::", "before"]

(* one argument applied to each operator only *)
val unaryst  = ["~", "!"]

(* return the item in the slay map mapped by x *)
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
    (* returns three tuple of type on the left,
     * type on the right, and type of the operator *)
    let fun getTyBinaryOp "=" l =
	    let val tv = T.freshtyvar ()
		val tl = T.consV tv
		val tr = T.consV tv
		val to = T.constybool' l T.BB
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp "=old" l =
	    let val tl = T.constyint'  l T.BB
		val tr = T.constyint'  l T.BB
		val to = T.constybool' l T.BB
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp "<>" l =
	    let val tv = T.freshtyvar ()
		val tl = T.consV tv
		val tr = T.consV tv
		val to = T.constybool' l T.BB
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp ">" l =
	    let val tl = T.constyint'  l T.BB
		val tr = T.constyint'  l T.BB
		val to = T.constybool' l T.BB
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp "<" l =
	    let val tl = T.constyint'  l T.BB
		val tr = T.constyint'  l T.BB
		val to = T.constybool' l T.BB
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp ">=" l =
	    let val tl = T.constyint'  l T.BB
		val tr = T.constyint'  l T.BB
		val to = T.constybool' l T.BB
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp "<=" l =
	    let val tl = T.constyint'  l T.BB
		val tr = T.constyint'  l T.BB
		val to = T.constybool' l T.BB
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp "+" l =
	    let val tl = T.constyint' l T.BB
		val tr = T.constyint' l T.BB
		val to = T.constyint' l T.BB
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp "-" l =
	    let val tl = T.constyint' l T.BB
		val tr = T.constyint' l T.BB
		val to = T.constyint' l T.BB
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp "*" l =
	    let val tl = T.constyint' l T.BB
		val tr = T.constyint' l T.BB
		val to = T.constyint' l T.BB
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp "/" l =
	    let val tl = T.constyreal' l T.BB
		val tr = T.constyreal' l T.BB
		val to = T.constyreal' l T.BB
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp "div" l =
	    let val tl = T.constyint' l T.BB
		val tr = T.constyint' l T.BB
		val to = T.constyint' l T.BB
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp "mod" l =
	    let val tl = T.constyint' l T.BB
		val tr = T.constyint' l T.BB
		val to = T.constyint' l T.BB
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp "@" l =
	    let val tv = T.freshtyvar ()
		val tl = T.constylist' tv l T.BB
		val tr = T.constylist' tv l T.BB
		val to = T.constylist' tv l T.BB
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp "^" l =
	    let val tl  = T.constystring' l T.BB
		val tr  = T.constystring' l T.BB
		val to  = T.constystring' l T.BB
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp ":=" l =
	    let val tv = T.freshtyvar ()
		val tl = T.constyref' tv l T.BB
		val tr = T.consV tv
		val to = T.constytuple' [] l T.BB
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp "o" l =
	    let val tv1 = T.freshtyvar ()
		val tv2 = T.freshtyvar ()
		val tv3 = T.freshtyvar ()
		val tl = T.constyarrow' tv2 tv3 l T.BB
		val tr = T.constyarrow' tv1 tv2 l T.BB
		val to = T.constyarrow' tv1 tv3 l T.BB
            in (tl, tr, to)
            end
	  | getTyBinaryOp "before" l =
	    let val tv = T.freshtyvar ()
		val tl = T.consV tv
		val tr = T.constyrecord' [] NONE l T.BB
		val to = T.consV tv
	    in (tl, tr, to)
            end
	  | getTyBinaryOp "::" l =
	    let val tv = T.freshtyvar ()
		val tl = T.consV tv
		val tr = T.constylist' tv l T.BB
		val to = T.constylist' tv l T.BB
	    in (tl, tr, to)
	    end
	  | getTyBinaryOp __ = raise EH.DeadBranch ""
	fun getTyUnaryOp "~" l =
	    let val ti = T.constyint' l T.BB
		val to = T.constyint' l T.BB
	    in (ti, to) (* type in and type out *)
	    end
	  | getTyUnaryOp "!" l =
	    let val tv = T.freshtyvar ()
		val ti = T.constyref' tv l T.BB
		val to = T.consV tv
	    in (ti, to) (* type in and type out *)
	    end
	  | getTyUnaryOp _ _ = raise EH.DeadBranch ""
	fun scanBinary (id, str) =
	    let val lab = L.builtinLab
		val (tl, tr, to) = getTyBinaryOp str lab
		val ty  = T.newV ()
		val ti  = T.consTyTupleTy [tl, tr] lab T.BB
		val c   = E.genCstTyEm ty (T.consTyArrowTy ti to lab T.BB) lab
		(* :: is the only constructor *)
		val cl  = if str = "::" then CL.consDA1 () else CL.consREC ()
		val bd  = E.consBindPoly id ty cl lab
	    in (bd, c)
	    end
	fun scanUnary (id, str) =
	    let val lab = L.builtinLab
		val (ti, to) = getTyUnaryOp str lab
		val ty  = T.newV ()
		val c   = E.genCstTyEm ty (T.consTyArrowTy ti to lab T.BB) lab
		val bd  = E.consBindPoly id ty (CL.consREC ()) lab
	    in (bd, c)
	    end
	val (binds1, cst1) = foldr (fn (id, (binds, cs)) =>
				       let val (bd, c) = scanBinary id
				       in (bd :: binds, c :: cs) end)
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
