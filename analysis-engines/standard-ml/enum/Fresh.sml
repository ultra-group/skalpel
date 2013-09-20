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
 *)


(** Implementation using SplayMapFn of FRESH (constrained opaquely). *)
structure Fresh :> FRESH = struct

(* shorten the names of structures *)
structure T  = Ty
structure E  = Env
structure OM = SplayMapFn (OrdKey)

(** A type which will hold the state of all the various different variables from #Ty.
 * This new type state is a record made up of a:
 * \arg type variable #Ty.typeVar.
 * \arg type function variable #Ty.typeFunctionVar.
 * \arg typename variable #Ty.typenameVar.
 * \arg row variable #Ty.rowVar.
 * \arg field variable #Ty.fieldVar.
 * \arg label variable #Ty.labelVar.
 * \arg environment variable #Ty.envVar.
 * \arg overloaded identifier #Ty.idor.
 **)
type state = {tv : T.typeVar     option OM.map ref,
	      tf : T.typeFunctionVar    option OM.map ref,
	      tn : T.typenameVar option OM.map ref,
	      sq : T.rowVar    option OM.map ref,
	      rt : T.fieldVar    option OM.map ref,
	      lt : T.labelVar  option OM.map ref,
	      ev : E.envVar    option OM.map ref,
	      or : T.idor      option OM.map ref}

(** Initialises the state to the empty map. *)
fun finitState () =
    let
	(** For initialising the type variable part of the state. *)
	val atv = ref OM.empty
	(** For initialising the type function variable part of the state. *)
        val atf = ref OM.empty
	(** For initialising the type name variable part of the state. *)
	val atn = ref OM.empty
	(** For initialising the row variable variable part of the state. *)
	val asq = ref OM.empty
	(** For initialising the field variable part of the state. *)
	val art = ref OM.empty
	(** For initialising the label variable part of the state. *)
	val alt = ref OM.empty
	(** For initialising the environment variable part of the state. *)
	val aev = ref OM.empty
	(** For initialising the overloaded identifier variable part of the state. *)
	val aor = ref OM.empty
    in {tv = atv, tf = atf, tn = atn, sq = asq, rt = art, lt = alt, ev = aev, or = aor}
    end

(** Updates a state with a new value. *)
fun fupdateOneState onestate x y = onestate := (OM.insert (!onestate, x, SOME y))

(** For the generation of fresh variables. *)
fun freshvargen x onestate ffresh fint =
    (case Option.getOpt (OM.find (!onestate, fint x), NONE) of
	 NONE   => let (** Gets a new variable (kind dependent on argument ffresh). *) val y = ffresh () in fupdateOneState onestate (fint x) y; y end
       | SOME y => y)

(** Generates a fresh type variable. *)
fun freshTypeVar         x (state : state) = freshvargen x (#tv state) T.freshTypeVar         T.typeVarToInt
(** Generates a fresh type function variable. *)
fun freshTypeFunctionVar x (state : state) = freshvargen x (#tf state) T.freshTypeFunctionVar T.typeFunctionVarToInt
(** Generates a fresh typename variable. *)
fun freshTypenameVar  x (state : state) = freshvargen x (#tn state) T.freshTypenameVar T.typenameVarToInt
(** Generates a fresh row variable. *)
fun freshRowVar x (state : state) = freshvargen x (#sq state) T.freshRowVar    T.rowVarToInt
(** Generates a fresh field variable. *)
fun freshFieldVar    x (state : state) = freshvargen x (#rt state) T.freshFieldVar   T.fieldVarToInt
(** Generates a fresh label variable. *)
fun freshLabVar    x (state : state) = freshvargen x (#lt state) T.freshLabelVar T.labelVarToInt
(** Generates a fresh environment variable. *)
fun freshEnvVar    x (state : state) = freshvargen x (#ev state) E.freshEnvVar   E.envVarToInt
(** Generates a fresh overloaded identifier variable. *)
fun freshIdOr      x (state : state) = freshvargen x (#or state) T.freshidor     T.idorToInt

end
