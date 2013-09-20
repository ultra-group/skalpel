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
 *  o Date:        24 May 2010
 *  o File name:   LongId.sig
 *)

(** Used to deal with long identifiers, used by refstruct{LongId} *)
signature LONGID = sig

    type key
    type set
    type keyOut = int list * int

    val buildKey       : Id.lid -> key

    val equal          : set -> set -> bool
    val length         : set -> int
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

    val inSet          : keyOut list -> set

    val sing           : Id.id -> Label.label -> set

    val diffShort      : Id.set -> set -> set
    val disjShort      : Id.set -> set -> bool

    val toString        : set -> string
    val toStringOut     : set -> string
    val toJsonStringOut : set -> string
    val toStringList    : set -> Id.assoc -> string list
    val toStringListSt  : set -> Id.assoc -> string

end
