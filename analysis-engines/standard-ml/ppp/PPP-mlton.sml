(* Copyright 2009 2010 Heriot-Watt University
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
 *  o Authors:     Joe Wells, Vincent Rahli
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        25 May 2010
 *  o File name:   PPP-mlton.sml
 *)

(** This is a dummy version of PPP.sml to use when compiling with MLton *)
structure Ppp : PPP = struct

open AstSML

(** Holds the abstract syntax tree of the program. *)
datatype data = AST of AstSML.progs

(** Dummy value that hold an empty abstract syntax tree option. *)
val data_tmp : data option ref = ref NONE

(** Dummy function, which returns unit for any argument. *)
fun use _ = ()

(** Dummy function, which takes unit and returns unit. *)
fun unsilence_compiler () = ()

(** Dummy function, which takes unit and returns unit. *)
fun silence_compiler () = ()

(** Dummy function, which takes some argument and returns an empty program tree. *)
fun readAst _ = AstSML.Progs []

(** Dummy function, which takes unit and returns the empty string. *)
fun prettyPrintAst _ = ""

end
