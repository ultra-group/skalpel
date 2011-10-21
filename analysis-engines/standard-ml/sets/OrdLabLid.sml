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
 *  o Date:        13 July 2010
 *  o File name:   OrdLabLid.sml
 *  o Description: Defines the structure OrdLabLid of ordered pairs:
 *      label and long identifier.
 *)


structure OrdLabLid : ORD_KEY = struct

(* shorten the names of some structures *)
structure O = OrdLid
structure L = Label
structure I = Id

(* ord_key is a label and an lid from ident *)
type ord_key = L.label * I.lid

(* comparison function which will compare two ord_key values *)
fun compare ((lab1, lid1), (lab2, lid2)) =
    case L.compareLab (lab1, lab2) of
	EQUAL => O.compare (lid1, lid2)
      | x => x

end
