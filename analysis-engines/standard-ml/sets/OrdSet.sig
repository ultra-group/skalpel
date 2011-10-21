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
 *  o File name:   OrdSet.sig
 *  o Description: Defines the signature ORDSET, to deal with ordered
 *      sets of intergers.
 *)


signature ORDSET = sig

    (* operations on ordered lists of integers *)

    type elt     = int (* IntListSet.item *)
    type label   = elt     (* a label (program point) *)
    type ordset  (* = IntListSet.set *)
    type ordsets = ordset list
    type labels  = ordset  (* set of labels *)

    val dummylab     : elt
    val firstlab     : elt
    val getlab       : unit -> label
    val setnextlab   : int  -> unit
    (*val freshlab     : unit -> label*)
    val empty        : ordset
    val isEmpty      : ordset   -> bool
    val isSingle     : ordset   -> bool
    val length       : ordset   -> int
    val getfirst     : ordset   -> elt option
    val remfirst     : ordset   -> elt option * ordset
    val splitInTwo1  : ordset   -> ordset * ordset
    val splitInTwo2  : ordset   -> ordset * ordset
    val singleton    : elt      -> ordset
    val concats      : ordsets  -> ordset
    val ord          : elt list -> ordset
    val toList       : ordset   -> elt list
    val restrictLim  : ordset   -> elt     -> ordset
    val cons         : elt      -> ordset  -> ordset
    val delete       : elt      -> ordset  -> ordset
    val concat       : ordset   -> ordset  -> ordset
    val inter        : ordset   -> ordset  -> ordset
    (*val interList    : elt list -> ordset  -> elt list*)
    val difference   : ordset   -> ordset  -> ordset
    val getpairs     : ordset   -> ordset  -> ordset list
    val concatop     : ordset option -> ordset -> ordset
    val getpairss    : ordset list -> ordset list
    val equal        : ordset   -> ordset  -> bool
    val subseteq     : ordset   -> ordset  -> bool
    val subset       : ordset   -> ordset  -> bool
    val disjoint     : ordset   -> ordset  -> bool
    (* true if one element of the second argument is a subseteq of the first argument *)
    val exsubseteq   : ordset   -> ordsets -> bool
    val subseteqin   : ordset   -> ordsets -> bool
    val isin         : elt      -> ordset  -> bool
    val isinone      : elt      -> ordset  -> bool
    val splitList    : elt      -> ordset  -> (ordset * ordset)
    val splitInterv  : elt -> elt -> ordset -> ordset * ordset
    val foldr        : (elt * 'a -> 'a) -> 'a -> ordset -> 'a
    val foldl        : (elt * 'a -> 'a) -> 'a -> ordset -> 'a
    val printelt     : elt      -> string
    val toString     : ordset   -> string

end
