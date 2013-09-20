(* Copyright 2010 2011 2012 Heriot-Watt University
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
 *  o Authors:     Vincent Rahli, John Pirie
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        18 August 2010
 *  o File name:   ExtendedLabel.sml
 *)

(** Defines the ExtLab (ExtnededLabel) structure to deal with forms extended with dependencies (labels, value identeifiers). *)
structure ExtLab :> EXTLAB = struct

structure L  = Label
structure ContextDependancy = LongId

(** Defines a term annotated with labels.
 * Constructors are as follows:
 * \arg 'a A labelled term.
 * \arg #Label.labels labels labelling the term.
 * \arg #Label.labels distinguished labels : id term.
 * \arg #ContextDependency.set Context dependencies labelling the term. *)
type 'a extLab = 'a * L.labels * L.labels * ContextDependancy.set

(** Prints a dependent form. *)
fun printExtLab (term, labs, stats, cdeps) f ascid =
    "(" ^ f term                        ^
    "," ^ L.toString        labs        ^
    "," ^ L.toString        stats       ^
    ", contextDependancies=" ^ ContextDependancy.toStringListSt cdeps ascid ^ ")"

(** Prints a dependent form but doesn't explicity name the contextDependencies. *)
fun printExtLab' (x, labs, st, deps) f =
    "(" ^ f x              ^
    "," ^ L.toString  labs ^
    "," ^ L.toString  st   ^
    "," ^ ContextDependancy.toString deps ^ ")"

(** Gets the term of a dependent form. *)
fun getExtLabT (x, _, _, _) = x
(** Gets the labels of a dependent form. *)
fun getExtLabL (_, x, _, _) = x
(** Gets the extra labels of the dependent form. *)
fun getExtLabE (_, _, x, _) = x
(** Gets the context dependencies on a dependent form. *)
fun getExtLabD (_, _, _, x) = x

(** Constructs a depentent form given a term, two label sets and some context dependencies. *)
fun consExtLab x labs stats cdeps = (x, labs, stats, cdeps)

(** Initialises a depentent form given a term and a label. *)
fun initExtLab x lab = consExtLab x (L.singleton lab) L.empty ContextDependancy.empty

(** Replaces both sets of labels and context dependencies in a depentent form. *)
fun setExtLab x labs stats cdeps = consExtLab (getExtLabT x) labs stats cdeps

(** For mapping over dependent forms. *)
fun mapExtLab (x, labs, stats, cdeps) f = (f x, labs, stats, cdeps)

(** Strips an extLab from its annotations. *)
fun stripExtLab xs = map getExtLabT xs

(** Unions two extLab values. *)
fun unionExtLab (x1, labs1, stats1, cdeps1)
		(x2, labs2, stats2, cdeps2)
		funion =
    (funion (x1, x2),
     L.union  labs1  labs2,
     L.union  stats1 stats2,
     ContextDependancy.union cdeps1 cdeps2)

(** Updating of annotation of an extLab. *)
fun updExtLab elab labs2 stats2 cdeps2 =
    unionExtLab elab ((), labs2, stats2, cdeps2) (fn (x, _) => x)

(** Updates the first set of labels in a dependent form. *)
fun updExtLabL (x, labs, stts, deps) labs' = (x, L.union labs labs', stts, deps)

(** Updates the second set of labels in a dependent form. *)
fun updExtLabE (x, labs, stts, deps) stts' = (x, labs, L.union stts stts', deps)

(** Updates the context dependencies in a dependent form. *)
fun updExtLabD (x, labs, stts, deps) deps' = (x, labs, stts, ContextDependancy.union deps deps')

(** Creates a dependent form from a term with two empty label sets and no context dependencies. *)
fun resetExtLab x = (getExtLabT x, L.empty, L.empty, ContextDependancy.empty)

end
