(* Copyright 2010 2012 Heriot-Watt University
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
 *  o Date:        22 June 2010
 *  o File name:   Label.sig
 *)

(** Defines the signature LABEL to represent program points. *)
signature LABEL = sig

    type label
    type ('a, 'b) labels

    val unionSizes : int list ref

    val dummyLab   : label
    val builtinLab : label
    val firstLab   : label

    val nextlab    : label ref

    val toInt      : label -> int
    val fromInt    : int   -> label

    val nextLabel  : label -> label
    val setNextLab : label -> unit
    val resetNext  : unit  -> unit

    val nextLabN   : label -> int -> label

    val eq         : label -> label -> bool
    val min        : label -> label -> label

    val singleton  : label -> (label, bool) labels
    val cons       : label -> (label, bool) labels -> (label, bool) labels
    val delete     : label -> (label, bool) labels -> (label, bool) labels
    val isin       : label -> (label, bool) labels -> bool
    val isinone    : label -> (label, bool) labels -> bool

    val split      : label -> (label, bool) labels -> (label, bool) labels * (label, bool) labels
    val split2     : label -> label -> (label, bool) labels -> (label, bool) labels * (label, bool) labels
    val splitIn2   : (label, bool) labels -> (label, bool) labels * (label, bool) labels

    val empty      : unit -> (label, bool) labels
    val bigEmpty   : unit -> (label, bool) labels

    val isEmpty    : (label, bool) labels -> bool
    val isSingle   : (label, bool) labels -> bool
    val length     : (label, bool) labels -> int

    val union      : (label, bool) labels -> (label, bool) labels -> (label, bool) labels
    val diff       : (label, bool) labels -> (label, bool) labels -> (label, bool) labels
    val inter      : (label, bool) labels -> (label, bool) labels -> (label, bool) labels
    val disjoint   : (label, bool) labels -> (label, bool) labels -> bool
    val subset     : (label, bool) labels -> (label, bool) labels -> bool
    val subseteq   : (label, bool) labels -> (label, bool) labels -> bool
    val compareLab : label  * label  -> order
    val compare    : (label, bool) labels * (label, bool) labels -> order
    val ord        : label  list -> (label, bool) labels
    val unions     : (label, bool) labels list -> (label, bool) labels
    val unionsCons : label list -> (label, bool) labels list -> (label, bool) labels
    val toList     : (label, bool) labels -> int list
    val remFirst   : (label, bool) labels -> label option * (label, bool) labels
    val foldr      : (label * 'a -> 'a) -> 'a -> (label, bool) labels -> 'a
    val exsubseteq : (label, bool) labels -> (label, bool) labels list -> bool

    val printLab   : label  -> string
    val toString   : (label, bool) labels -> string

end
