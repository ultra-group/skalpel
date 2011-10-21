(* Copyright 2010 Heriot-Watt University
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
 *  o Date:        05 July 2010
 *  o File name:   SeachSpace.sig
 *  o Description: Contains the SEARCHSPACE signature for the
 *      searchspace of the enumeration algorithm.
 *)


signature SEARCHSPACE = sig

    type searchSpace

    (* Empty searchspace *)
    val emSearchSpace : searchSpace

    (* Transform a set of labels into a searchSpace where each filter is a singleton *)
    val flatLabs      : Label.labels -> searchSpace

    (* Returns one filter from the searchspace *)
    val getOneFilter  : searchSpace -> (Label.labels * searchSpace) option

    (* Adds new filters to the searchspace, build from the first argument (filter)
     * and the third one (error found).  The fourth argument is the set of
     * filters that led to successes.
     * The boolean indicates if the error has been newly found by the constraint
     * solver (true) or if it was already found before. *)
    val buildFilters  : Label.labels -> Label.labels -> searchSpace -> bool -> searchSpace

    (* Returns the filters for which the solver succeeded. *)
    val getSuccess    : searchSpace -> Label.labels list

    (* Adds a filter for which the solved succeeded to the searchspace *)
    val addSuccess    : Label.labels -> searchSpace -> searchSpace

end
