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
 *  o Authors:     Vincent Rahli
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        25 May 2010
 *  o File name:   Tag.sml
 *)

(** Defines the structure Tag for HTML tags, constrained opaquely by refstruct{TAG} *)
structure Tag :> TAG = struct

(* shorten the names of some structures *)
structure ER = ExtReg
structure EH = ErrorHandler

(** Used to get what class the font should be.
 * See also #getBTag. *)
fun getClash col weight kindReg =
    let val clW =
	    (* returns weight based on colour *)
	    case col of ER.Red => Int.toString
				      (if weight > 12
				       then 12
				       else weight)
		      | _      => ""
	(** Return clase string based on colour col. *)
	val clCol =
	    case col of ER.Red    => "clash1"
		      | ER.Blue   => "clash2"
		      | ER.Purple => "clash3"
		      | ER.Green  => "clash4"
		      | ER.Yellow => "clash5"
		      | ER.Orange => "clash6"
	(** Retruns kind of region. *)
	val clKindReg =
	    case kindReg of 0 => "a" (* ER.L *)
			  | 1 => "c" (* ER.H *)
			  | 2 => "b" (* ER.N *)
			  | _ => raise EH.DeadBranch "DeadBranch43"
    in clCol ^ clKindReg(* ^ clW*)
    end

(** Beginning tag (fonts). *)
fun getBTag col w n = "<fonts class=\"" ^ getClash col w n ^ "\">"

(** Ending tag (fonts). *)
fun getETag      () = "</fonts>"

(** Break tag. *)
fun getBreakTag  () = "\n<br/>\n"

(** Space tag. *)
fun getSpace     () = "&nbsp;"

(** Title tag (bold). *)
fun getTitle     st = "<b>" ^ st ^ ":</b>"

end


(** The structure Tag2 for tags that can be interpreted in some terminals, constrained opaqualy by redstruct{TAG}.*)
structure Tag2 :> TAG = struct

(* shorten the names of some structures *)
structure ER = ExtReg
structure EH = ErrorHandler

(*echo -en "\033[0;31;40mfoo bar\033[0m"*)

(** Beginning tag. *)
val tagB = "\\033[0"

(** Ending tag. *)
val tagE = "m"

(** Used to get what class the font should be.
 * See also #getBTag. *)
fun getClash col weight kindReg =
    let
	(** Return clase string based on colour col. *)
	val clCol =
	    case col of ER.Red    => ";30;41"
		      | ER.Blue   => ";30;44"
		      | ER.Purple => ";30;45"
		      | ER.Green  => ";30;42"
		      | ER.Yellow => ";30;43"
		      | ER.Orange => ";30;47" (* this is white unfortunately *)
    in case kindReg
	of 0 => clCol (* ER.L *)
	 | 1 => ""    (* ER.H *)
	 | 2 => ""    (* ER.N *)
	 | _ => raise EH.DeadBranch "DeadBranch44"
    end

(** Beginning tag. *)
fun getBTag col w n = tagB ^ getClash col w n ^ tagE

(** Ending tag. *)
fun getETag      () = tagB ^ tagE

(** Break tag. *)
fun getBreakTag  () = "\n"

(** Space tag. *)
fun getSpace     () = " "

(** Title tag. *)
fun getTitle     st = st ^ ":"

end
