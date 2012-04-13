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
 *  o File name:   Fresh.sml
 *  o Description: Defines the structures Fresh that uses maps as
 *      substitutions (SplayMapFn (OrdKey))
 *)


(* Implementation of FRESH using SplayMapFn *)
structure Fresh :> FRESH = struct

(* shorten the names of structures *)
structure T  = Ty
structure E  = Env
structure OM = SplayMapFn (OrdKey)

(* a new type which will hold the state of all the various
 * different variables from Ty *)
type state = {tv : T.typeVar     option OM.map ref,
	      tf : T.typeFunctionVar    option OM.map ref,
	      tn : T.typenameVar option OM.map ref,
	      sq : T.sequenceVar    option OM.map ref,
	      rt : T.rowVar    option OM.map ref,
	      lt : T.labelVar  option OM.map ref,
	      ev : E.envvar    option OM.map ref,
	      or : T.idor      option OM.map ref}

(* initialises the state *)
fun finitState () =
    let val atv = ref OM.empty (* empty map *)
        val atf = ref OM.empty
	val atn = ref OM.empty
	val asq = ref OM.empty
	val art = ref OM.empty
	val alt = ref OM.empty
	val aev = ref OM.empty
	val aor = ref OM.empty
    in {tv = atv, tf = atf, tn = atn, sq = asq, rt = art, lt = alt, ev = aev, or = aor}
    end

(* updates a state *)
fun fupdateOneState onestate x y = onestate := (OM.insert (!onestate, x, SOME y))

(* for the generation of fresh variables *)
fun freshvargen x onestate ffresh fint =
    (case Option.getOpt (OM.find (!onestate, fint x), NONE) of
	 NONE   => let val y = ffresh () in fupdateOneState onestate (fint x) y; y end
       | SOME y => y)

(* generates fresh variables from Ty *)
fun freshTypeVar         x (state : state) = freshvargen x (#tv state) T.freshTypeVar         T.typeVarToInt
fun freshTypeFunctionVar x (state : state) = freshvargen x (#tf state) T.freshTypeFunctionVar T.typeFunctionVarToInt
fun freshTypenameVar  x (state : state) = freshvargen x (#tn state) T.freshTypenameVar T.typenameVarToInt
fun freshSequenceVar x (state : state) = freshvargen x (#sq state) T.freshSequenceVar    T.sequenceVarToInt
fun freshRowVar    x (state : state) = freshvargen x (#rt state) T.freshRowVar   T.rowVarToInt
fun freshLabVar    x (state : state) = freshvargen x (#lt state) T.freshLabelVar T.labelVarToInt
fun freshEnvVar    x (state : state) = freshvargen x (#ev state) E.freshEnvVar   E.envVarToInt
fun freshIdOr      x (state : state) = freshvargen x (#or state) T.freshidor     T.idorToInt

end
