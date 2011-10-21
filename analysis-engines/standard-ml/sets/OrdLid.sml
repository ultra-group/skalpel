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
 *  o File name:   OrdLid.sml
 *  o Description: Defines the structure OrdLid of ordered long
 *      identifiers.
 *)


structure OrdLid : ORD_KEY = struct

(* structure abbreviations *)
structure I = Id

type ord_key = I.lid

fun compare (I.LID _, I.ID _) = GREATER
  | compare (I.ID _, I.LID _) = LESS
  | compare (I.ID (id1, _), I.ID (id2, _)) =
    I.compare (id1, id2)
  | compare (I.LID ((id1, _), lid1, _),
	     I.LID ((id2, _), lid2, _)) =
    (case I.compare (id1, id2) of
	 EQUAL => compare (lid1, lid2)
       | x => x)

end
