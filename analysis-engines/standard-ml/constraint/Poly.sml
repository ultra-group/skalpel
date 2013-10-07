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
 *  o File name:   Poly.sml
 *  o Description: Defines the structure Poly which has signature POLY.
 *      This structure is to deal with polymorphism/monomorphism.
 *)

(** A structure used to distinguish polymorphic and monomorphic terms, constrained opaquely by refstruct{POLY}. *)
structure Poly :> POLY = struct

(* shorten names of structures *)
structure X  = Expans
structure L  = Label
structure CD = LongId
structure EH = ErrorHandler

(** A monomorphic term with two constructors for how the monomorphism is enforced.
 * Constructors are:
 * \arg EXPANS of #Expans.expans. For when an expansiveness constraint forcing the monomorphism.
 * \arg MONOBIN of #Label.labels. Labels forcing the monomorphism e.g. an exception. *)
datatype mono = EXPANS of X.expans
	      | MONBIN of L.labels

(** Represents whether an expression is monomorphic or polymorphic.
 * Constructors are as follows:
 * \arg POLY. Represents a polymorphic expression.
 * \arg MONO of #mono list. Represents a monomorphic expression, argument explains why expression is monomorphic.
 *)
datatype poly = POLY
	      | MONO of mono list

(** Returns the labels of a monomorphic term. *)
fun getLabsMono (EXPANS expans) =
    let val (labs, lidop) = X.getLabsExpans expans
    in case lidop of
	   NONE => (labs, labs, CD.empty)
	 | SOME lid => (labs, labs, CD.singleton (CD.buildKey lid))
    end
  | getLabsMono (MONBIN labs)   = (labs, L.empty, CD.empty)

(** Returns the labels of a polymorphic term. *)
fun getLabsPoly POLY         = (L.empty, L.empty, CD.empty)
  | getLabsPoly (MONO monos) =
    foldr (fn (mono, (labs, stts, deps)) =>
	      let val (labs', stts', deps') = getLabsMono mono
	      in (L.union  labs labs',
		  L.union  stts stts',
		  CD.union deps deps')
	      end)
	  (L.empty, L.empty, CD.empty)
	  monos

(** Turns a non expansive term into a polymorphic term. *)
fun fromNonexpToPoly X.Nonexp = POLY
  | fromNonexpToPoly (X.Expans xs) = MONO (map (fn x => EXPANS x) xs)

(** Turns a monomorphic expresison to a #Expans.expans value. *)
fun fromMonoToNonexp (EXPANS x) = SOME x
  | fromMonoToNonexp (MONBIN _) = NONE

(** Turns a polymorphic expresison to a #Expans.expans value. *)
fun fromPolyToNonexp POLY = X.Nonexp
  | fromPolyToNonexp (MONO xs) =
    (case List.mapPartial (fn x => fromMonoToNonexp x) xs of
	 [] => X.Nonexp
       | ys => X.Expans ys)

(** Merges two polymorphic expressions togethers. *)
fun mergePoly POLY x = x
  | mergePoly x POLY = x
  | mergePoly (MONO xs) (MONO ys) = MONO (xs @ ys)

(** Turns a polymorphic expresison into a monomorphic one, given some labels. *)
fun polyToMono POLY labs = MONO [MONBIN labs]
  | polyToMono _    _    = raise EH.DeadBranch ""

(** Gets all expression constructed with #MONOBIN. *)
fun allMonBin mons = List.all (fn (MONBIN _) => true | _ => false) mons

(** Turns a monomorphic expression into a polymorphic one. *)
fun toPoly (MONO xs) = if allMonBin xs then POLY else MONO xs
  | toPoly POLY = POLY

(** Returns true if expression is polymorphic, false otherwise. *)
fun isPoly POLY = true
  | isPoly _    = false

(** Returns true if the expresion is monomorphic, false otherwise. *)
fun isMono POLY = false
  | isMono _    = true

(** Prints a #mono expression. *)
fun printMono (EXPANS exp)  = "EXPANS(" ^ X.printexpans exp ^ ")"
  | printMono (MONBIN labs) = "MONBIN(" ^ L.toString labs   ^ ")"

(** Prints a list of values, given a print function and a list . *)
fun printlistgen xs f = "[" ^ #1 (foldr (fn (t, (s, c)) => (f t ^ c ^ s, ",")) ("", "") xs) ^ "]"

(** Prints a list of monomorphic expressions. *)
fun printMonoList xs = printlistgen xs printMono

(** Prints a polymorphic/monomorphic expression. *)
fun printPoly POLY      = "POLY"
  | printPoly (MONO xs) = "MONO(" ^ printMonoList xs ^ ")"

(** Prints a #poly value. *)
fun toString poly = printPoly poly

end
