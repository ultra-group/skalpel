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
 *  o Description: This file contains our the the signature ANALYZE
 *      of the structre that defines our constraint generator.
 *)


signature ANALYZE = sig

    (* the int is:
     * 0 is we don't want any initial environment at all
     * 1 if we want the builtin environment
     * 2 if we want to use the basis.sml environment *)
    val generateConstraints : AstSML.progs -> int -> Env.envcss

    (* The first argument should be the output of analyze.  Program
     * identifiers are represented by integers in the output of
     * analyze, so the second argument is a mapping from the original
     * program identifiers to these integers.  This function has
     * builtin knowledge of types for all identifiers in the SML
     * initial basis as well as a number from the SML Basis Library.
     * The output is the input with additional constraints added to
     * enforce these types. *)
    val buildin : Env.envcss -> Id.assoc -> bool -> Env.envcss

    (* generates the warnings about the free identifiers *)
    (*val treatFreeIds : Env.env -> Env.env*)

    (* calls the three previous functions sequentially *)
    (* ths integer is as for generateConstraints *)
    val fullConsGen :  AstSML.progs -> Id.assoc -> int -> Env.envcss

end
