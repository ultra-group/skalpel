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
 *  o Date:        25 May 2010
 *  o File name:   ConsId.sig
 *  o Description: Defines the signature CONSID to deal with binders.
 *)


signature CONSID = sig

    type 'a bind = {id    : Id.id,
		    bind  : 'a,
		    class : ClassId.class,
		    lab   : Label.label,
		    poly  : Poly.poly}

    val getBindI          : 'a bind -> Id.id
    val getBindT          : 'a bind -> 'a
    val getBindC          : 'a bind -> ClassId.class
    val getBindL          : 'a bind -> Label.label
    val getBindP          : 'a bind -> Poly.poly

    (* Full constructor *)
    val consBind          : Id.id -> 'a -> ClassId.class -> Label.label -> Poly.poly -> 'a bind
    (* Constructor for a polymorphic binder *)
    val consBindPoly      : Id.id -> 'a -> ClassId.class -> Label.label -> 'a bind
    (* Constructor for a monomorphic binder *)
    val consBindMono      : Id.id -> 'a -> ClassId.class -> Label.label -> 'a bind

    (* Returns the label of the identifier and the labels associated
     * to the name of the function in the different branches of a fun dec
     * if the identifier is a function declared with a fun dec.
     * This is used in Unification.sml and Analize.sml. *)
    (*val getREClabs        : 'a bind -> Label.label * Label.labels*)

    (* generates monomorphic constraints using MONBIN and all based on
     * the label set, with a special care with REC (as many constraints
     * as a REC fun has branches if the id is a REC fun). *)
    (*val genMonREC         : 'a bind -> Label.labels -> Poly.mono list*)

    val resetPoly         : 'a bind -> 'a bind
    val toPolyBind        : 'a bind -> 'a bind
    val toMonoBind        : 'a bind -> Label.labels  -> 'a bind
    val updClass          : 'a bind -> ClassId.class -> 'a bind
    val updPoly           : 'a bind -> Poly.poly     -> 'a bind

    val getTyVar          : Ty.ty bind -> Ty.tyvar option
    val getTyVars         : Ty.ty bind -> Ty.exttyvar list

    val isVAL             : 'a bind -> bool
    val isPAT             : 'a bind -> bool
    val isDA0             : 'a bind -> bool
    val isREC             : 'a bind -> bool
    (*val isDAT             : 'a bind -> bool*)
    val isEX0             : 'a bind -> bool

    val toPAT             : 'a bind -> 'a bind
    val toREC             : 'a bind -> 'a bind
    val toVRC             : 'a bind -> 'a bind
    val toDA0             : 'a bind -> 'a bind
    val toDA1             : 'a bind -> 'a bind
    val toDAT             : 'a bind -> 'a bind
    val toEX0             : 'a bind -> 'a bind
    val toEX1             : 'a bind -> 'a bind
    (*val toDAT             : 'a bind -> 'a bind*)

    val toCLS             : 'a bind -> ClassId.class -> 'a bind

    val mapBind           : 'a bind -> ('a -> 'b) -> 'b bind

    (*val toCONlab          : 'a bind -> Label.label  -> 'a bind
    val toEXClab          : 'a bind -> Label.label  -> 'a bind (* this is to specify whic label constrains the monomorphism of an exception *)
    val toPATlabs         : 'a bind -> Label.labels -> 'a bind
    val toVALlabs         : 'a bind -> Label.labels -> 'a bind
    val toREClabs         : 'a bind -> Label.labels -> 'a bind*)
    (*val toSTRstr          : 'a bind -> ClassId.str   -> 'a bind
    val toSIGstr          : 'a bind -> ClassId.str   -> 'a bind*)
    (*val toDATcons         : 'a bind -> ClassId.cons  -> 'a bind*)

    (*val getPATval         : 'a bind -> Label.labels*)

    (* Returns the constructors of a datatype.  The 'a bind has to be for a datatype *)
    (*val getDATcons        : 'a bind -> ClassId.cons*)

    val closeBind         : 'a bind -> Expans.nonexp -> 'a bind

    (* Resets the names of a recursive function. *)
    (*val setREClabs        : 'a bind -> Label.labels -> 'a bind*)

    (*val getlabExtChkExc   : 'a bind list -> Label.labels list*)
    (*val getlabExtChkRec   : 'a bind list -> Label.labels*)
    (*val getlabExtChk      : 'a bind list -> Label.labels*)

    val printBind         : 'a bind -> ('a -> string) -> Id.assoc -> string
    val printBind'        : 'a bind -> ('a -> string) -> string
    (*val printExtTy        : extty         -> string
    val printExtTyList    : extty list    -> string
    val printExtTv        : exttv         -> string
    val printExtTvList    : exttv list    -> string
    val printExtSeqTy     : extseq        -> string
    val printExtSeqTyList : extseq list   -> string*)

end
