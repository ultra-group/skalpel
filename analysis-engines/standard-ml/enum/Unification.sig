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
 *  o Date:        21 May 2010
 *  o File name:   Unification.sig
 *  o Description: Contains the signature UNIF which is the signature of
 *      the functor Unif that contains our unification algorithm.
 *)


signature UNIF = sig

    structure S : STATE

    datatype error   = Success of S.state
                     | Error   of Error.error * S.state

    datatype user = MIN of Error.error (* called by the minimisation algorithm *)
		  | ENUM               (* called by the enumeration algorithm  *)
		  | DBENUM             (* called by the enumeration algorithm when dealing with database *)
    (* Free identifiers are only reported when the solver is called by the enumeration
     * algo.  When it is called by the enumeration algo we know that a free id, is one
     * which is not bound, but it is not the case at minimisation time because of the way
     * we report type constructor arity clashes. *)

    val unif       : Env.env        -> (* constraints                    *)
		     Filter.filters -> (* filters                        *)
		     user           -> (* caller of the unification algo *)
		     error

    (* When given ENUM, it means that we want to solve the builtin basis. *)

end
