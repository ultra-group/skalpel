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
 *  o File name:   Ident.sig
 *  o Description: This file defines the signature ID to deal with
 *                 identifiers.
 *)


signature ID = sig

    type id
    type idl     = id * Label.label (*idl stands for id + label*)
    (*lid stands for longid*)
    datatype lid = ID  of idl
		 | LID of idl * lid * Label.label
    type set
    type assoc
    type assocOut = (int * string) list
    (* We need assocOut to check the testcase database.
     * assocOut is the same as assoc. *)

    val consLidOp   : id -> Label.label -> Label.label -> lid option -> lid

    val compare     : id * id -> order
    val eqId        : id -> id -> bool

    val getLabs     : lid -> Label.labels
    val getTopLab   : lid -> Label.label
    val getLabId    : lid -> Label.label
    val getTopIdl   : lid -> idl

    (* Returns the left-most identifier in a long identifier *)
    val getLeftId   : lid -> idl

    (* Returns the sub tree of a lid starting at the given id/label *)
    val getSubLid   : lid -> id -> Label.label -> lid option

    val idToLid     : id -> Label.label -> lid

    (* Checks if an identifier is a long identifier or a short one *)
    val isLong      : lid -> bool

    (* CHANGE THAT. Both of them are due to error kinds.
     * They are used in ErrorKind.sml, Unification.sml, Analyze.sml, LongCds.sml. *)
    val toInt       : id  -> int
    val fromInt     : int -> id

    (* common set functions *)
    val foldr       : ((id * 'b) -> 'b) -> 'b -> set -> 'b
    val foldl       : ((id * 'b) -> 'b) -> 'b -> set -> 'b
    val app         : (id -> unit) -> set -> unit
    val add         : id -> set -> set
    val union       : set -> set -> set
    val empty       : set
    val isEmpty     : set -> bool
    val isin        : id  -> set -> bool
    val difference  : set -> set -> set (* second set \ first set *)
    val inter       : set -> set -> set
    val singleton   : id  -> set
    val toList      : set -> id list
    val ord         : id list -> set

    (* identifier information *)
    val dummyId     : id
    val freshId     : unit -> id
    val resetIds    : unit -> unit

    (* association manipulation and lookups *)
    val emAssoc     : assoc
    val updateAssoc : string -> assoc -> id * assoc
    val lookupSt      : string -> assoc -> id option
    val lookupId      : id -> assoc -> string option
    val outAssoc    : assoc -> assocOut
    val inAssoc     : assocOut -> assoc

    (* printing functions *)
    val printId     : id      -> string
    val printId'    : id      -> assoc  -> string
    val printIdL    : idl     -> string
    val printIdL'   : idl     -> assoc  -> string
    val printLid    : lid     -> string
    val printLid'   : lid     -> assoc  -> string
    val printLidSt  : lid     -> assoc  -> string
    val printIdList : id list -> string
    val printLidOut : lid     -> string
    val toString    : set     -> string
    val printAssoc  : assoc   -> string
    val printAssoc' : assoc   -> string

    (*
     val consAssoc    : id -> string -> assoc -> assoc
     val singAssoc    : id -> string -> assoc
     val printeltst   : elt -> assoc -> string
     val toStringSt   : ordset -> assoc -> string
     val toStringSt'  : ordset -> assoc -> string
     val toStringList : ordset -> assoc -> string list
     val ascidToStr   : assoc -> string
     *)

end
