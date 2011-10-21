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
 *  o File name:   Fresh.sig
 *  o Description: Defines the signature FRESH.
 *)


signature FRESH = sig

    type state

    (* initialises the state *)
    val finitState : unit -> state

    (* generates fresh variables *)
    val freshTyVar     : Ty.tyvar     -> state -> Ty.tyvar
    val freshTyfVar    : Ty.tyfvar    -> state -> Ty.tyfvar
    val freshTyNameVar : Ty.tynamevar -> state -> Ty.tynamevar
    val freshSeqVar    : Ty.seqvar    -> state -> Ty.seqvar
    val freshRowVar    : Ty.rowvar    -> state -> Ty.rowvar
    val freshLabVar    : Ty.labvar    -> state -> Ty.labvar
    val freshEnvVar    : Env.envvar   -> state -> Env.envvar
    val freshIdOr      : Ty.idor      -> state -> Ty.idor

end
