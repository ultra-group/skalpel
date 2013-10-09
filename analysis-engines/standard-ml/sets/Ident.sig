(* Copyright 2009 2010 2011 2012 Heriot-Watt University
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
 *  o File name:   Ident.sig
 *)

(** Deals with identifiers, used by refstruct{Id}. *)
signature ID = sig

    type id
    type labelledId     = id * Label.label

    datatype lid = ID  of labelledId
		 | LID of labelledId * lid * Label.label
    type set
    type assoc
    type assocOut = (int * string) list

    val consLidOp   : id -> Label.label -> Label.label -> lid option -> lid

    val compare     : id * id -> order
    val eqId        : id -> id -> bool

    val getLabs     : lid -> (Label.label, bool) Label.labels
    val getTopLab   : lid -> Label.label
    val getLabId    : lid -> Label.label
    val getTopIdl   : lid -> labelledId
    val getLidOut   : lid -> labelledId list * labelledId

    val getLeftId   : lid -> labelledId

    val getSubLid   : lid -> id -> Label.label -> lid option

    val idToLid     : id -> Label.label -> lid

    val isLong      : lid -> bool

    val toInt       : id  -> int
    val fromInt     : int -> id

    val foldr       : ((id * 'b) -> 'b) -> 'b -> set -> 'b
    val foldl       : ((id * 'b) -> 'b) -> 'b -> set -> 'b
    val app         : (id -> unit) -> set -> unit
    val add         : id -> set -> set
    val union       : set -> set -> set
    val empty       : set
    val isEmpty     : set -> bool
    val isin        : id  -> set -> bool
    val difference  : set -> set -> set
    val inter       : set -> set -> set
    val singleton   : id  -> set
    val toList      : set -> id list
    val ord         : id list -> set

    val dummyId     : id
    val freshId     : unit -> id
    val resetIds    : unit -> unit

    val emAssoc     : assoc
    val updateAssoc : string -> assoc -> id * assoc
    val lookupSt      : string -> assoc -> id option
    val lookupId      : id -> assoc -> string option
    val outAssoc    : assoc -> assocOut
    val inAssoc     : assocOut -> assoc

    val printId        : id      -> string
    val printId'       : id      -> assoc  -> string
    val printIdL       : labelledId     -> string
    val printIdL'      : labelledId     -> assoc  -> string
    val printLid       : lid     -> string
    val printLid'      : lid     -> assoc  -> string
    val printLidSt     : lid     -> assoc  -> string
    val printIdList    : id list -> string
    val printLidOut    : lid     -> string
    val printJsonLidOut    : lid     -> string
    val toString       : set     -> string
    val printAssoc     : assoc   -> string
    val printAssoc'    : assoc   -> string
    val printJsonAssoc : assoc   -> string

end
