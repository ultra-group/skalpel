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
 *  o File name:   Poly.sml
 *  o Description: Defines the structure Poly which has signature POLY.
 *      This structure is to deal with polymorphism/monomorphism.
 *)


structure Poly :> POLY = struct

(* shorten names of structures *)
structure X  = Expans
structure L  = Label
structure CD = LongId
structure EH = ErrorHandler

(* monomorphic and polymorphic terms *)
datatype mono = EXPANS of X.expans
	      | MONBIN of L.labels
datatype poly = POLY
	      | MONO of mono list

(* returns the labels of a mono term *)
fun getLabsMono (EXPANS expans) =
    let val (labs, lidop) = X.getLabsExpans expans
    in case lidop of
	   NONE => (labs, labs, CD.empty)
	 | SOME lid => (labs, labs, CD.singleton (CD.buildKey lid))
    end
  | getLabsMono (MONBIN labs)   = (labs, L.empty, CD.empty)

(* returns the labels of a polymorphic term *)
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

(* turns a non expansive term into a polymorphic term *)
fun fromNonexpToPoly X.Nonexp = POLY
  | fromNonexpToPoly (X.Expans xs) = MONO (map (fn x => EXPANS x) xs)

fun fromMonoToNonexp (EXPANS x) = SOME x
  | fromMonoToNonexp (MONBIN _) = NONE

fun fromPolyToNonexp POLY = X.Nonexp
  | fromPolyToNonexp (MONO xs) =
    (case List.mapPartial (fn x => fromMonoToNonexp x) xs of
	 [] => X.Nonexp
       | ys => X.Expans ys)

(* merges two polymorphic expressions togethers *)
fun mergePoly POLY x = x
  | mergePoly x POLY = x
  | mergePoly (MONO xs) (MONO ys) = MONO (xs @ ys)

fun polyToMono POLY labs = MONO [MONBIN labs]
  | polyToMono _    _    = raise EH.DeadBranch ""

fun allMonBin mons = List.all (fn (MONBIN _) => true | _ => false) mons

fun toPoly (MONO xs) = if allMonBin xs
		       then POLY
		       else MONO xs
  | toPoly POLY = POLY

(*fun genMonBin labs = MONO [MONBIN labs]*)


(*fun restrictMono (EXPANS expans) set =
    (SOME (EXPANS (Option.valOf (X.restrictExpans expans set)))
     handle Option => NONE)
  | restrictMono (x as (MONBIN _)) _ = SOME x

fun restrictPoly POLY      _   = POLY
  | restrictPoly (MONO xs) set =
    (case List.mapPartial (fn x => restrictMono x set) xs of
	 [] => POLY
       | ys => MONO ys)*)


(* true if poly is POLY *)

fun isPoly POLY = true
  | isPoly _    = false

fun isMono POLY = false
  | isMono _    = true


(* PRINTING SECTION *)


fun printMono (EXPANS exp)  = "EXPANS(" ^ X.printexpans exp ^ ")"
  | printMono (MONBIN labs) = "MONBIN(" ^ L.toString labs   ^ ")"

fun printlistgen xs f = "[" ^ #1 (foldr (fn (t, (s, c)) => (f t ^ c ^ s, ",")) ("", "") xs) ^ "]"

fun printMonoList xs = printlistgen xs printMono

fun printPoly POLY      = "POLY"
  | printPoly (MONO xs) = "MONO(" ^ printMonoList xs ^ ")"

fun toString poly = printPoly poly

end
