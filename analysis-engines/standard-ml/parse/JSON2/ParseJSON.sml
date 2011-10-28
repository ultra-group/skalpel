(* Copyright 2011 Heriot-Watt University
 *
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
 *  o Date:        24 May 2010
 *  o File name:   RunSlicer.sml
 *  o Description: Contains the functions that serve as interface with
 *      the slicer.  The file defines the structure Slicer which has
 *      the signature Slicer.
 *)

(****************************************************************************
 * Note: File needs to find a proper home somewhere in parse/, this is just a
 * a test file for me at the moment.
 ****************************************************************************)

signature ParseJSON =
sig
    val getJSONObject : unit -> JSON.value
    val printMyVariable : JSON.value -> unit
end

structure ParseJSON : ParseJSON =
struct

fun getJSONObject () =
    JSONParser.parseFile "my-json-file"

fun printMyVariable (JSON.OBJECT objectlist) =
    let
	fun getname (name, (JSON.INT value)) =
	    print("name: "^name^" value: "^(IntInf.toString(value))^"\n")
    in
	getname (List.hd objectlist)
    end
end;

(* val () = ParseJSON.getJsonObject () *)

print ("mark-1\n");
ParseJSON.getJSONObject ();
print ("mark-2\n");
