(* Copyright 2011 Heriot-Watt University
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
 *  o Authors:     John Pirie
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        05 November 2011
 *  o File name:   RunSlicer.sml
 *  o Description: Parses the test answer JSON files
 *)

(****************************************************************************
 * Note: File needs to find a proper home somewhere in parse/, this is just a
 * a test file for me at the moment.
 ****************************************************************************)

signature ParseJSON =
sig
    val getJSONObject : unit -> JSON.value
    val printMyVariable : JSON.value -> unit
    val parseTest : unit -> unit
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

fun parseTest () =
    ()
end;

(* val () = ParseJSON.getJsonObject () *)

print ("mark-1\n");
ParseJSON.getJSONObject ();
print ("mark-2\n");
