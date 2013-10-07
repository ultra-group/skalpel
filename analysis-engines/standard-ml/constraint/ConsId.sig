(* Copyright 2009 2010 2012 Heriot-Watt University
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
 *)

(** Defines the signature CONSID to deal with binders, used by refstruct{ConsId}. *)
signature CONSID = sig

    type 'a bind = {id    : Id.id,
		    bind  : 'a,
		    equalityTypeVar : Ty.equalityTypeVar,
		    class : ClassId.class,
		    lab   : Label.label,
		    poly  : Poly.poly}

    val getBindI          : 'a bind -> Id.id
    val getBindT          : 'a bind -> 'a
    val getBindEqualityTypeVar : 'a bind -> Ty.equalityTypeVar
    val getBindC          : 'a bind -> ClassId.class
    val getBindL          : 'a bind -> Label.label
    val getBindP          : 'a bind -> Poly.poly

    val consBind          : Id.id -> 'a -> Ty.equalityTypeVar -> ClassId.class -> Label.label -> Poly.poly -> 'a bind
    val consBindPoly      : Id.id -> 'a -> Ty.equalityTypeVar -> ClassId.class -> Label.label -> 'a bind
    val consBindMono      : Id.id -> 'a -> Ty.equalityTypeVar -> ClassId.class -> Label.label -> 'a bind

    val resetPoly         : 'a bind -> 'a bind
    val toPolyBind        : 'a bind -> 'a bind
    val toMonoBind        : 'a bind -> Label.labels  -> 'a bind
    val updClass          : 'a bind -> ClassId.class -> 'a bind
    val updPoly           : 'a bind -> Poly.poly     -> 'a bind

    val getTypeVar          : Ty.ty bind -> Ty.typeVar option
    val getTypeVars         : Ty.ty bind -> Ty.explicitTypeVar list

    val isVAL             : 'a bind -> bool
    val isPAT             : 'a bind -> bool
    val isDA0             : 'a bind -> bool
    val isREC             : 'a bind -> bool
    val isEX0             : 'a bind -> bool

    val toPAT             : 'a bind -> 'a bind
    val toREC             : 'a bind -> 'a bind
    val toVRC             : 'a bind -> 'a bind
    val toDA0             : 'a bind -> 'a bind
    val toDA1             : 'a bind -> 'a bind
    val toDAT             : 'a bind -> 'a bind
    val toEX0             : 'a bind -> 'a bind
    val toEX1             : 'a bind -> 'a bind

    val toCLS             : 'a bind -> ClassId.class -> 'a bind

    val mapBind           : 'a bind -> ('a -> 'b) -> 'b bind

    val closeBind         : 'a bind -> Expans.nonexp -> 'a bind

    val printBind         : 'a bind -> ('a -> string) -> Id.assoc -> string
    val printBind'        : 'a bind -> ('a -> string) -> string

end
