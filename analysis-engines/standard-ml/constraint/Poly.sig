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
 *  o File name:   Poly.sig
 *  o Description: Defines the signature POLY which is the signature to
 *      deal with monomorphism/polymorphism.
 *)


signature POLY = sig

    datatype mono = EXPANS of Expans.expans (* expansiveness constraint forcing the monomorphism. *)
		  | MONBIN of Label.labels  (* labels forcing the monomorphism such as for an exception. *)
    datatype poly = POLY
		  | MONO of mono list

    val getLabsMono       : mono -> Label.labels * Label.labels * LongId.set
    val getLabsPoly       : poly -> Label.labels * Label.labels * LongId.set

    val fromPolyToNonexp  : poly -> Expans.nonexp
    val fromNonexpToPoly  : Expans.nonexp -> poly

    (* This is similar to composeNonexp in Expans *)
    val mergePoly         : poly -> poly -> poly
    (* Transforms a polymorphic binding into a monomorphic one *)
    val polyToMono        : poly -> Label.labels -> poly
    (* Transforms a mono into a poly if all the mono info are monbins and not expans. *)
    val toPoly            : poly -> poly

    (*val genMonBin         : Label.labels -> poly (* generates a poly using MONBIN *)*)

    (*val restrictPoly      : poly -> Id.set -> poly*)

    val isPoly            : poly -> bool
    val isMono            : poly -> bool

    val toString          : poly -> string

end
