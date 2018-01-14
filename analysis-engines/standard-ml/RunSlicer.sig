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
 *  o Authors:     Vincent Rahli, John Pirie
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        21 May 2010
 *  o File name:   RunSlicer.sig
 *  o Description: Defines the signature SLICER which is the signature
 *      of the functions that serve as interface with the slicer.
 *)

(** The signature SLICER, used by the structure refstruct{Slicer}. *)
signature SLICER = sig

    val setWebDemo         : bool -> unit

    val smlnjEntryPoint          :  string * string list -> OS.Process.status
    val mltonEntryPoint          :  unit -> OS.Process.status
    val polymlEntryPoint         :  unit -> unit

    val commslicerp        : string      ->
    			     string list ->
    			     string      ->
    			     string      ->
    			     string      ->
    			     string      ->
    			     string      ->
    			     string      ->
    			     int         ->
    			     int         ->
    			     int         ->
    			     unit

    val smlTesStrArgs : string -> OS.Process.status

    val error : JsonParser.error

end
