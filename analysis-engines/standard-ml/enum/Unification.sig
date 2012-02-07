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

    val unif       : Env.environment -> (* constraints                    *)
		     Filter.filters  -> (* filters                        *)
		     user            -> (* caller of the unification algo *)
		     error

    (* When given ENUM, it means that we want to solve the builtin basis. *)

end
