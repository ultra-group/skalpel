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
 *  o Authors:     Vincent Rahli, John Pirie
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        24 May 2010
 *  o File name:   ErrorKind.sig
 *)

(** Defines the signature ERRORKIND which is the signature specifying the errors handled by our slicer. *)
signature ERRORKIND = sig

    type label    = int
    type id       = int
    type typename = int
    type laberr   = (label * string) list
    type recerr   = laberr * laberr * laberr * laberr
    type iderr    = label * id
    type idserr   = label * id * string
    type specerr  = label * id
    type arrerr   = label * int
    type tnerr    = label * typename
    type unmerr   = specerr * specerr list * label
    type synerr   = (int list * int) option
    (* 39 kinds of error *)
    (* Unmatched   : The label above is in case the list is empty.  We should really have an option. *)
    (* NotGenClash : is for a structure which is not as general as its signature. *)
    (* LabTyClash  : Record clashes are always merged now. *)
    (* DatTypClash : The first label is for a datatype in a signature and the second one for a type in a stucture. *)
    datatype kind = Circularity
		  | Overload       of iderr  * tnerr * tnerr list
		  | OverloadCst    of idserr * tnerr * tnerr list
		  | OverloadClash  of idserr * tnerr list * idserr * tnerr list
		  | OverloadIdCst  of iderr  * tnerr list * idserr * tnerr list
                  | ArityClash     of arrerr * arrerr
                  | TyConsClash    of tnerr  * tnerr
                  | EqTypeRequired of label  * label
		  | NotGenClash    of iderr  * tnerr
		  | TooGenSig      of iderr  * iderr * label list
		  | TyFunClash     of iderr  * tnerr
		  | LabTyClash     of recerr
		  | Unmatched      of unmerr
		  | UnbWhere       of unmerr
		  | DatTypClash    of id * label * label
		  | MissConsSig    of (label * id) * (label * id) list
		  | MissConsStr    of (label * id) * (label * id) list
		  | DuplicateId    of iderr * iderr
		  | SharingTypeNotInSig    of label (* or trying to rebind a cons *)
		  | ConsArgNApp    of label * label (* or trying to rebind a cons *)
		  | ConsNArgApp    of label * label (* not used yet *)
		  | MultiOcc       of synerr (* Do we need the argument here? *)
		  | ValVarApp      of synerr (* Do we need the argument here? *)
		  | ExcIsVar       of synerr (* Do we need the argument here? *)
		  | ExcIsDat       of synerr (* The argument is useless because it is a context independent error *)
		  | ConIsVar       of synerr (* Do we need the argument here? *)
		  | DatIsExc       of synerr (* Do we need the argument here?  They are never used *)
		  | TypeVarBind    of synerr
		  | Warning        of string
		  | Parsing        of string
		  | NonFlexWhere   of iderr * iderr
		  | IllFormedWhere of iderr * iderr
		  | RigidWhere
		  | Inclusion
		  | AppNotApp
		  | DiffFunName
		  | DiffNbArgFun
		  | FreeTypeVarTop (* not used anymore, but should be *)
		  | AsPatVar
		  | FnRecExp
		  | RealInPat
		  | FreeIdent
		  | FreeOpen
    (* in TyConsClash a we can have tynamevar too (and not only tyname) *)


    val issyn              : kind -> bool
    val issem              : kind -> bool
    val printSmlErrKind    : kind -> string
    val printJsonErrKind   : kind -> string
    val printErrKind       : kind -> Id.assoc -> (string * string)

end
