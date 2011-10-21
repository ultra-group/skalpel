(* Copyright 2010 Heriot-Watt University
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
 *  o Date:        22 June 2010
 *  o File name:   Label.sig
 *  o Description: This file defines the signature LABEL to deal with
 *      labels.
 *)


signature LABEL = sig

    type label   (* a label (program point) *)
    type labels

    (* label (integer) constants *)
    val dummyLab   : label
    val builtinLab : label
    val firstLab   : label

    (* allows conversion to and from integers *)
    val toInt      : label -> int
    val fromInt    : int   -> label

    (* functions to control the next label *)
    val nextLabel  : label -> label
    val setNextLab : label -> unit
    val resetNext  : unit  -> unit

    val nextLabN   : label -> int -> label

    val eq         : label -> label -> bool
    val min        : label -> label -> label

    (* normal set functions *)
    val singleton  : label -> labels
    val cons       : label -> labels -> labels
    val delete     : label -> labels -> labels
    val isin       : label -> labels -> bool
    val isinone    : label -> labels -> bool

    val split      : label -> labels -> labels * labels
    val split2     : label -> label -> labels -> labels * labels
    val splitIn2   : labels -> labels * labels
    val splitIn2'  : labels -> labels * labels

    (* empty set *)
    val empty      : labels

    val isEmpty    : labels -> bool
    val isSingle   : labels -> bool
    val length     : labels -> int

    (* more normal set functions *)
    val union      : labels -> labels -> labels
    val diff       : labels -> labels -> labels
    val inter      : labels -> labels -> labels
    val disjoint   : labels -> labels -> bool
    val subset     : labels -> labels -> bool
    val subseteq   : labels -> labels -> bool

    val compareLab : label  * label  -> order
    val compare    : labels * labels -> order

    val ord        : label  list -> labels

    (* generalised union *)
    val unions     : labels list -> labels

    (* converts a set of labels into a set of integers *)
    val toList     : labels -> int list

    (* removes the first element in the set *)
    val remFirst   : labels -> label option * labels

    val foldr      : (label * 'a -> 'a) -> 'a -> labels -> 'a

    val exsubseteq : labels -> labels list -> bool

    val printLab   : label  -> string
    val toString   : labels -> string

end
