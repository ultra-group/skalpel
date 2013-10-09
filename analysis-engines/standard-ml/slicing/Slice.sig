(* Copyright 2009 2010 2011 2012 Heriot-Watt University
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
 *  o File name:   Slice.sig
 *  o Description: Defines the signature SLICING which is the signature
 *      of our slicing algorithm.
 *)


signature SLICING = sig

    val slice            : AstSML.progs -> (Label.label, bool) Label.labels -> AstSML.progs
    val printSlice       : AstSML.progs -> bool -> string
    val printOneSlice    : AstSML.progs -> bool -> string -> string
    (*val printOneSmlSlice : AstSML.progs -> string*)
    val printSlices      : AstSML.progs list -> string
    val getDots          : unit -> string * string * string * string * string
    val toString         : AstSML.progs -> string

end
