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
 *  o File name:   Fresh.sig
 *  o Description: Defines the signature FRESH.
 *)


signature FRESH = sig

    type state

    (* initialises the state *)
    val finitState : unit -> state

    (* generates fresh variables *)
    val freshTypeVar            : Ty.typeVar     -> state -> Ty.typeVar
    val freshTypeFunctionVar    : Ty.typeFunctionVar    -> state -> Ty.typeFunctionVar
    val freshTypenameVar        : Ty.typenameVar -> state -> Ty.typenameVar
    val freshSequenceVar        : Ty.sequenceVar    -> state -> Ty.sequenceVar
    val freshRowVar             : Ty.rowVar    -> state -> Ty.rowVar
    val freshLabVar             : Ty.labelVar    -> state -> Ty.labelVar
    val freshEnvVar             : Env.envVar   -> state -> Env.envVar
    val freshIdOr               : Ty.idor      -> state -> Ty.idor

end
