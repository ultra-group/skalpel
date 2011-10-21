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
 *  o Date:        18 August 2010
 *  o File name:   ExtLab.sig
 *  o Description: Defines the EXTLAB signature which is the signature
 *      of forms extended with dependencies (labels, value identifiers).
 *)


signature EXTLAB = sig

    type 'a extLab = 'a           * (* labelled term                           *)
		     Label.labels * (* labels labelling the term               *)
		     Label.labels * (* distinguised labels : id status         *)
		     LongId.set     (* context dependencies labelling the term *)

    val getExtLabT   : 'a extLab -> 'a
    val getExtLabL   : 'a extLab -> Label.labels
    val getExtLabE   : 'a extLab -> Label.labels
    val getExtLabD   : 'a extLab -> LongId.set

    val initExtLab   : 'a -> Label.label -> 'a extLab
    val consExtLab   : 'a -> Label.labels -> Label.labels -> LongId.set -> 'a extLab

    val mapExtLab    : 'a extLab -> ('a -> 'b) -> 'b extLab
    val stripExtLab  : 'a extLab list -> 'a list
    val unionExtLab  : 'a extLab -> 'b extLab -> ('a * 'b -> 'c) -> 'c extLab
    val updExtLab    : 'a extLab -> Label.labels -> Label.labels -> LongId.set -> 'a extLab
    val updExtLabL   : 'a extLab -> Label.labels -> 'a extLab (* update the labs part *)
    val updExtLabE   : 'a extLab -> Label.labels -> 'a extLab (* update the stts part *)
    val updExtLabD   : 'a extLab -> LongId.set   -> 'a extLab (* update the deps part *)
    val resetExtLab  : 'a extLab -> 'a extLab

    val printExtLab' : 'a extLab      ->
		       ('a -> string) ->
		       string

    val printExtLab  : 'a extLab      ->
		       ('a -> string) ->
		       Id.assoc       ->
		       string

end
