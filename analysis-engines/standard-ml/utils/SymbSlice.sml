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
 *  o Description: Defines the structure SymbSlice that contains the
 *      different symbols used by our slicer such as the extra symbols
 *      used in slices.
 *)


structure SymbSlice = struct

val dots     = ".."

(* mathematical left angle bracket- ⟨ *)
val ldots    = "\226\159\168"
val ldotsLatex = "\\langle"

(* mathematical right angle bracket- ⟩ *)
val rdots    = "\226\159\169"
val rdotsLatex = "\\rangle"

(* left white square bracket- 〚 *)
val splparen = "\227\128\154"

(* right white square bracket- 〛 *)
val sprparen = "\227\128\155"

(* box drawing characters *)

val bottomLeftCurve = "\226\149\176"
val straightLine = "\226\148\128"
val verticalFork = "\226\148\156"

val unitT1 = "ms"

(* mu - µ *)
val unitT2 = "\194\181s"

(* some other symbols, currently unused *)
(*val dots     = ".."
val ldots    = "⟨" (*"\226\159\168"*) (*"⧼"*) (*\226\167\188*) (*"〈"*) (*"〚"*) (*\227\128\154*) (*"〈"*) (*\226\140\169*) (*"⦑"*)
val rdots    = "⟩" (*"\226\159\169"*) (*"⧽"*) (*\226\167\189*) (*"〉"*) (*"〛"*) (*\227\128\155*) (*"〉"*) (*\226\140\170*) (*"⦒"*)
val splparen = "〚" (*"\227\128\154"*) (*"〘"*) (*\227\128\152*) (*"〈"*) (*"⦉"*)
val sprparen = "〛" (*"\227\128\155"*) (*"〙"*) (*\227\128\153*) (*"〉"*) (*"⦊"*)

val unitT1 = "ms"
val unitT2 = "µs"*)

end
