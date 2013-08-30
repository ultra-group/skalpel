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
 *  o Date:        25 May 2010
 *  o File name:   SymbSlice.sml
 *)

(** Contains the different symbols used by our slicer such as the extra symbols used in slices.
 * There are also symbols defined here which are used in the debug framework. *)
structure SymbSlice = struct

(** Dot characters - the string "..". *)
val dots     = ".."

(** Mathematical left angle bracket: \f$\langle\f$. *)
val ldots    = "\226\159\168"

(** Mathematical left angle bracket in latex: \f$\langle\f$. *)
val ldotsLatex = "\\langle"

(** Mathematical right angle bracket: \f$\rangle\f$. *)
val rdots    = "\226\159\169"

(** Mathematical right angle bracket in latex: \f$\rangle\f$. *)
val rdotsLatex = "\\rangle"

(** Left white square bracket.  *)
val splparen = "\227\128\154"

(** Right white square bracket. *)
val sprparen = "\227\128\155"

(** Bottom left box drawing character *)
val bottomLeftCurve = "\226\149\176"

(** Horiontal line box drawing character. *)
val straightLine = "\226\148\128"

(** Vertical fork box drawing character. *)
val verticalFork = "\226\148\156"

(** Miliseconds string (ms). *)
val unitT1 = "ms"

(** The mu symbol (\f$\mu\f$). *)
val unitT2 = "\194\181s"

end
