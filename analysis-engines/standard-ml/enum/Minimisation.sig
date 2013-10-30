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
 *  o Date:        24 May 2010
 *  o File name:   Minimisation.sig
 *)

(** A signature used by refstruct{Min}, used to minimise errors. *)
signature MIN = sig

    val minimizeallkind  : Error.error list                   ->
			   Env.envContextSensitiveSyntaxPair  ->
			   AstSML.packs                       ->
			   VTimer.timer                       ->
			   (Error.export') option             ->
			   int                                ->
			   Error.error list

    val minimize         : Error.error                       ->
			   Env.envContextSensitiveSyntaxPair ->
			   AstSML.packs                      ->
			   VTimer.timer                      ->
			   (Error.export') option            ->
			   int                               ->
			   Error.error * int

end
