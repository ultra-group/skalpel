(* Copyright 2018 Christian Gregg
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
 * along with Skalpel.  If not, see <http://www.gnu.org/licenses/>. *)

structure Version : sig

    val BUILT_AT : string
    val BUILT_WITH : string
    val GIT_HASH : string
    val SKALPEL_VERSION : string

end = struct
  (* These variables are set automatically during compilation, no manual editting
   * should be required. No changes should be committed either. *)
  val BUILT_WITH = ""
  val BUILT_AT = ""
  val GIT_HASH = ""
  val SKALPEL_VERSION = "Built with " ^ BUILT_WITH ^ " on " ^ BUILT_AT ^ ". Skalpel version: " ^ GIT_HASH

end
