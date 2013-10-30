(* Copyright 2009 Heriot-Watt University
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
 *  o File name:   Html.sig
 *)

(** Defines the signature HTML, signature of the functor refstruct{Html} used to display slices in a webbrowser. *)
signature HTML = sig

    val transformErrSl : string           ->
			 string option    ->
			 Error.error list ->
			 Id.assoc         ->
			 AstSML.progs     ->
			 bool             ->
			 bool             ->
			 int              ->
			 (string -> string) ->
			 unit

    val setstylecss    : int -> unit

end
