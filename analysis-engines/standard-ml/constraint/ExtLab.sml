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
 *  o Description: Defines the ExtLab (ExtnededLabel) structure to deal with forms
 *    extended with dependencies (labels, value identeifiers).x
 *)

structure ExtLab :> EXTLAB = struct

structure L  = Label
structure ContextDependancy = LongId

type 'a extLab = 'a                    *  (* labelled term *)
		 L.labels              *  (* labels labelling the term *)
		 L.labels              *  (* distinguished labels : id term *)
		 ContextDependancy.set    (* context dependencies labelling the term *)

fun printExtLab (term, labs, stats, cdeps) f ascid =
    "(" ^ f term                        ^
    "," ^ L.toString        labs        ^
    "," ^ L.toString        stats       ^
    "," ^ ContextDependancy.toStringListSt cdeps ascid ^ ")"

fun printExtLab' (x, labs, st, deps) f =
    "(" ^ f x              ^
    "," ^ L.toString  labs ^
    "," ^ L.toString  st   ^
    "," ^ ContextDependancy.toString deps ^ ")"

(* Accessors to a extLab *)

fun getExtLabT (x, _, _, _) = x (* T for Term, whatever term is annotated *)
fun getExtLabL (_, x, _, _) = x (* L for labels                           *)
fun getExtLabE (_, _, x, _) = x (* E for extra labels                     *)
fun getExtLabD (_, _, _, x) = x (* D for context dependency               *)


(* extLab constructors *)

fun consExtLab x labs stats cdeps = (x, labs, stats, cdeps)
fun initExtLab x lab = consExtLab x (L.singleton lab) L.empty ContextDependancy.empty
fun setExtLab x labs stats cdeps = consExtLab (getExtLabT x) labs stats cdeps


(* Mapping over an extLab *)
fun mapExtLab (x, labs, stats, cdeps) f = (f x, labs, stats, cdeps)

(* Strips an extLab from its annotations *)
fun stripExtLab xs = map getExtLabT xs

(* Unions 2 extLabs *)
fun unionExtLab (x1, labs1, stats1, cdeps1)
		(x2, labs2, stats2, cdeps2)
		funion =
    (funion (x1, x2),
     L.union  labs1  labs2,
     L.union  stats1 stats2,
     ContextDependancy.union cdeps1 cdeps2)

(* Updating of annotation of an extLab *)
fun updExtLab elab labs2 stats2 cdeps2 =
    unionExtLab elab ((), labs2, stats2, cdeps2) (fn (x, _) => x)

fun updExtLabL (x, labs, stts, deps) labs' = (x, L.union labs labs', stts, deps)
fun updExtLabE (x, labs, stts, deps) stts' = (x, labs, L.union stts stts', deps)
fun updExtLabD (x, labs, stts, deps) deps' = (x, labs, stts, ContextDependancy.union deps deps')

fun resetExtLab x = (getExtLabT x, L.empty, L.empty, ContextDependancy.empty)

end
