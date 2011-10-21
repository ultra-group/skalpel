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
 *  o File name:   Html.sig
 *  o Description: Defines the signature HTML, signature of the function
 *      used to display slices in a webbrowser.
 *)


signature HTML = sig

    val transformErrSl : string           -> (* file out  *)
			 string option    -> (* file base *)
			 Error.error list ->
			 Id.assoc         ->
			 AstSML.progs     ->
			 bool             -> (* true to print the header of the html file *)
			 bool             -> (* true if we want to pring all those tags   *)
			 int              -> (* overloading basis *)
			 (string -> string)           -> (* overloading basis *)
			 unit

    val setstylecss    : int -> unit

end
