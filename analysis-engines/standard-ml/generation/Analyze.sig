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
 *  o Authors:     Vincent Rahli, John Pirie
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        20 May 2010
 *  o File name:   Analyze.sig
 *)

(** This file contains our the the signature ANALYZE of the structre that defines our constraint generator. *)
signature ANALYZE = sig

    val generateConstraints : AstSML.progs -> int -> Env.envContextSensitiveSyntaxPair
    val buildin : Env.envContextSensitiveSyntaxPair -> Id.assoc -> bool -> Env.envContextSensitiveSyntaxPair
    val fullConsGen :  AstSML.progs -> Id.assoc -> int -> Env.envContextSensitiveSyntaxPair

end
