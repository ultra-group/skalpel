(* Copyright 2010 Heriot-Watt University
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
 * along with Skapel.  If not, see <http://www.gnu.org/licenses/>.
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

    val emSearchSpace : searchSpace
    val flatLabs      : (Label.label, bool) Label.labels -> searchSpace
    val getOneFilter  : searchSpace -> ((Label.label, bool) Label.labels * searchSpace) option
    val buildFilters  : (Label.label, bool) Label.labels -> (Label.label, bool) Label.labels -> searchSpace -> bool -> searchSpace
    val getSuccess    : searchSpace -> (Label.label, bool) Label.labels list
    val addSuccess    : (Label.label, bool) Label.labels -> searchSpace -> searchSpace

end
