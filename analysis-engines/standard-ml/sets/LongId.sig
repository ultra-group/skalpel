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
 *  o Date:        24 May 2010
 *  o File name:   LongId.sig
 *  o Description: This file defines the signature LONGID to deal with
 *      long identifiers.
 *)


signature LONGID = sig

    type key
    type set
    type keyOut = int list * int (* We need keyOut to check the testcase database*)

    val buildKey       : Id.lid -> key

    val equal          : set -> set -> bool
    val subset         : set -> set -> bool
    val subseteq       : set -> set -> bool
    val union          : set -> set -> set
    val difference     : set -> set -> set
    val inter          : set -> set -> set
    val empty          : set
    val isEmpty        : set -> bool
    val isSingle       : set -> bool

    val isin           : Id.lid -> set -> bool

    val foldl          : ((Id.lid * 'b) -> 'b) -> 'b -> set -> 'b
    val foldr          : ((Id.lid * 'b) -> 'b) -> 'b -> set -> 'b

    val add            : key -> set -> set

    val unions         : set list -> set

    val singleton      : key -> set

    val toList         : set -> key list
    val ord            : key list -> set

    val toOutList      : set -> keyOut list
    (* inSet generates a set with dummy labels *)
    val inSet          : keyOut list -> set

    val sing           : Id.id -> Label.label -> set
    (* long sets are generated from the Id.set using dummy labels *)
    val diffShort      : Id.set -> set -> set
    val disjShort      : Id.set -> set -> bool

    val toString        : set -> string
    val toStringOut     : set -> string
    val toJsonStringOut : set -> string
    val toStringList    : set -> Id.assoc -> string list
    val toStringListSt  : set -> Id.assoc -> string

end
