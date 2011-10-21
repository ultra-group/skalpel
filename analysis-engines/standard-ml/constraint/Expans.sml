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
 *  o File name:   Expans.sml
 *  o Description: Defines the structure Expans which has signature EXPANS.
 *      This structure is used to deal with expansive/non-expansive
 *      expressions.
 *)


structure Expans : EXPANS = struct

(* shorten the name of structures for use in the file *)
structure I = Id
structure L = Label

(* An expansive expression can either be:
 *   - A non-dependent expansive expression.
 *       o For example:  'let val decs in exp end' is expansive because
 *         it is a complicated expression that could potentially generate
 *         exceptions or extend the domain of the memory.
 *       o _____WHY 'Expexp of L.labels'?____
           For these we use Expexp.  It takes a list of labels which is the
 *         location set responsible for the expression to be expansive.
 *         For example, for the let-expresion above, this label set would
 *         just be the label of the let-expression.
 *       o We say that they are non-dependent because they do not depend on
 *         identifier status.
 *   - A dependent expansive expression.
 *       o For example: 'f g' is a dependent expansive expression, i.e.,
 *         it is expansive only if 'f' is a value variable.  The expansiveness
 *         of such an expression depends on the status of some identifier.
 *       o _____WHY 'Expdep of I.lid * L.labels'?____
 *         For these we use Expdep.  It takes an identifier and a list of labels.
 *         The identifier is the one the expansiveness of the expression depends
 *         on.  As before the label set is the location set justifying that the
 *         expression is dependently expansive.
 *         For example, for the application above, the id would be the one
 *         associated to 'f' and the label set would contain f's label as well
 *         as the label associated to the application of 'f' to 'g'.
 *)
datatype expans = Expexp of L.labels
		| Expdep of I.lid * L.labels

(* For non expansive expressions (constructor Nonexp) as well as expansive
 * expressions (constructor Expans).  An Expans takes a list because there
 * might be several explanations for an expression to be expansive (see
 * testcase 149).  The non expansive expression are discribed page 19 of the
 * Definition of SML.
 *)
datatype nonexp = Nonexp
		| Expans of expans list


(* PRINTING SECTION *)


fun printlistgen xs f = "[" ^ #1 (foldr (fn (t, (s, c)) => (f t ^ c ^ s, ",")) ("", "") xs) ^ "]"

fun printExpansSigs' xs =
    printlistgen xs (fn (id, lab) => "(" ^ I.printId id ^ "," ^ L.printLab lab ^ ")")

fun printEvOp NONE     = "-"
  | printEvOp (SOME v) = Int.toString v

fun printexpans (Expexp l)         = "Expexp(" ^ L.toString l   ^ ")"
  | printexpans (Expdep (lid, ll)) = "Expdep(" ^ I.printLid lid ^
				     ","       ^ L.toString ll  ^ ")"

fun printexpanslist xs = printlistgen xs printexpans

fun printnonexp Nonexp     = "Nonexp"
  | printnonexp (Expans l) = "Expans(" ^ printexpanslist l ^ ")"


(* OTHER STUFF *)


fun composeNonexp xs =
    foldr (fn (x,         Nonexp)    => x
	    | (Nonexp,    x)         => x
	    | (Expans l1, Expans l2) => Expans (l1 @ l2))
	  Nonexp
	  xs

val initLongExpans = ([], NONE)

fun addexpans (Expexp ll)      l = Expexp (L.cons l ll)
  | addexpans (Expdep (x, ll)) l = Expdep (x, L.cons l ll)

fun addnonexp (Expans ll) l = Expans (map (fn x => addexpans x l) ll)
  | addnonexp x _ = x

fun genOneExpdep n l = Expans [Expdep (I.ID (n, l), L.singleton l)]
fun genOneExpans l   = Expans [Expexp (L.singleton l)]
fun genMulExpans ll  = Expans [Expexp (L.ord ll)]

fun getLabsSigs (xs, _) = L.ord (map (fn (_, l) => l) xs)

fun getLabsExpans (Expexp labs) = (labs, NONE)
  | getLabsExpans (Expdep (lid, labs)) = (labs, SOME lid)
(* I.getLabs lid should be in labs anyway and for that
 * we should add l at the labs position in genOneExpdep. *)

(*fun getSetStatusExpans (Expexp _) = L.empty
  | getSetStatusExpans (Expdep (_, _, st)) = st*)

(*fun getSetStatusNonexp Nonexp = L.empty
  | getSetStatusNonexp (Expans xs) =
    foldr (fn (x, st) => L.concat (getSetStatusExpans x) st)
	  L.empty
	  xs*)

(*fun restrictExpans (x as (Expdep (I.ID (i, _), _, st))) set =
    if I.isin i set andalso L.isEmpty st
    then NONE
    else SOME x
  | restrictExpans (x as (Expdep _)) _ = SOME x
  | restrictExpans (x as (Expexp _)) _ = SOME x

fun restrictNonexp Nonexp _ = Nonexp
  | restrictNonexp (Expans xs) set =
    (case List.mapPartial (fn x => restrictExpans x set) xs of
	 [] => Nonexp
       | ys => Expans ys)*)

end
