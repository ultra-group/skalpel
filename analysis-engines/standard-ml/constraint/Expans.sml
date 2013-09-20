(* Copyright 2009 2010 Heriot-Watt University
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
 *  o File name:   Expans.sml
 *)

(** Used to deal with expansive/non-expansive expressions.
 * An expansive expression can either be:
 *   - A non-dependent expansive expression.
 *       - For example:  'let val decs in exp edn' is expansive because
 *         it is a complicated expression that could potentially generate
 *         exceptions or extend the domain of the memory.
 *       - \b Why 'Expexp of L.labels'?
 *         For these we use Expexp.  It takes a list of labels which is the
 *         location set responsible for the expression to be expansive.
 *         For example, for the let-expresion above, this label set would
 *         just be the label of the let-expression.
 *       - We say that they are non-dependent because they do not depend on
 *         identifier status.
 *   - A dependent expansive expression.
 *       - For example: 'f g' is a dependent expansive expression, i.e.,
 *         it is expansive only if 'f' is a value variable.  The expansiveness
 *         of such an expression depends on the status of some identifier.
 *       - \b Why 'Expdep of I.lid * L.labels'?
 *         For these we use Expdep.  It takes an identifier and a list of labels.
 *         The identifier is the one the expansiveness of the expression depends
 *         on.  As before the label set is the location set justifying that the
 *         expression is dependently expansive.
 *         For example, for the application above, the id would be the one
 *         associated to 'f' and the label set would contain f's label as well
 *         as the label associated to the application of 'f' to 'g'.
 *)
structure Expans : EXPANS = struct

(* shorten the name of structures for use in the file *)
structure I = Id
structure L = Label


(** Holds expansive and non-expansive expressions (see top of file/doc page)
 * The I.lid value in the Expdep constructor is the identifier that makes the expression expansive.
 *)
datatype expans = Expexp of L.labels
		| Expdep of I.lid * L.labels

(** For non expansive expressions (constructor #nonexp) as well as expansive
 * expressions (constructor #expans). An #expans takes a list because there
 * might be several explanations for an expression to be expansive (see
 * testcase 149).  The non expansive expression are discribed page 19 of the
 * Definition of SML.
 *)
datatype nonexp = Nonexp
		| Expans of expans list


(** Prints out a list. *)
fun printlistgen xs f = "[" ^ #1 (foldr (fn (t, (s, c)) => (f t ^ c ^ s, ",")) ("", "") xs) ^ "]"

(** Prints something of type #expans. *)
fun printexpans (Expexp l)         = "Expexp(" ^ L.toString l   ^ ")"
  | printexpans (Expdep (lid, ll)) = "Expdep(" ^ I.printLid lid ^
				     ","       ^ L.toString ll  ^ ")"

(** Prints a list of expansive expressions. *)
fun printexpanslist xs = printlistgen xs printexpans

(** Prints out contents of a #nonexp constructor value. *)
fun printnonexp Nonexp     = "Nonexp"
  | printnonexp (Expans l) = "Expans(" ^ printexpanslist l ^ ")"

(** Builds a non-expansive expression. *)
fun composeNonexp xs =
    foldr (fn (x,         Nonexp)    => x
	    | (Nonexp,    x)         => x
	    | (Expans l1, Expans l2) => Expans (l1 @ l2))
	  Nonexp
	  xs

(** Initialises an expansive expression. *)
val initLongExpans = ([], NONE)

(** Adds expansive expression labels. *)
fun addexpans (Expexp ll)      l = Expexp (L.cons l ll)
  | addexpans (Expdep (x, ll)) l = Expdep (x, L.cons l ll)

(** Adds non-expansive expression labels. *)
fun addnonexp (Expans ll) l = Expans (map (fn x => addexpans x l) ll)
  | addnonexp x _ = x

(** Generates a dependent form of an expansive expresison. *)
fun genOneExpdep n l = Expans [Expdep (I.ID (n, l), L.singleton l)]

(** Generates an expansive expresison. *)
fun genOneExpans l   = Expans [Expexp (L.singleton l)]
fun genMulExpans ll  = Expans [Expexp (L.ord ll)]

(** Gets expansive labels. *)
fun getLabsExpans (Expexp labs) = (labs, NONE)
  | getLabsExpans (Expdep (lid, labs)) = (labs, SOME lid)

end
