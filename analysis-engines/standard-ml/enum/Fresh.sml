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
type state = {tv : T.tyvar     option OM.map ref,
	      tf : T.tyfvar    option OM.map ref,
	      tn : T.tynamevar option OM.map ref,
	      sq : T.seqvar    option OM.map ref,
	      rt : T.rowvar    option OM.map ref,
	      lt : T.labvar    option OM.map ref,
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
fun freshTyVar     x (state : state) = freshvargen x (#tv state) T.freshtyvar     T.tyvarToInt
fun freshTyfVar    x (state : state) = freshvargen x (#tf state) T.freshtyfvar    T.tyfvarToInt
fun freshTyNameVar x (state : state) = freshvargen x (#tn state) T.freshtynamevar T.tynamevarToInt
fun freshSeqVar    x (state : state) = freshvargen x (#sq state) T.freshseqvar    T.seqvarToInt
fun freshRowVar    x (state : state) = freshvargen x (#rt state) T.freshrowvar    T.rowvarToInt
fun freshLabVar    x (state : state) = freshvargen x (#lt state) T.freshlabvar    T.labvarToInt
fun freshEnvVar    x (state : state) = freshvargen x (#ev state) E.freshenvvar    E.envvarToInt
fun freshIdOr      x (state : state) = freshvargen x (#or state) T.freshidor      T.idorToInt

end
