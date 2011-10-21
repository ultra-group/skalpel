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
 *  o File name:   VTimer.sml
 *  o Description: Defines the structure VTimer which has signature
 *      VTIMER.
 *)


structure VTimer :> VTIMER = struct

structure SY = SymbSlice

type timer = {real : Timer.real_timer,
	      cpu  : Timer.cpu_timer}

(* starts both timers *)
fun startTimer () = {real = Timer.startRealTimer (),
		     cpu  = Timer.startCPUTimer ()}

(* returns the times taken *)
fun getTime1 (timer : timer) = Timer.checkRealTimer (#real timer)
fun getTime2 (timer : timer) =
    let val {usr, sys} = Timer.checkCPUTimer (#cpu timer)
    in Time.+ (usr, sys)
    end

fun getTime timer = getTime1 timer
(* We might want to use getTime2 instead. *)

(* gives the time in miliseconds - *10^(-3) *)
fun getMilliTime timer = Time.toMilliseconds (getTime timer)

(* gives the time in microseconds - *10^(-6) *)
fun getMicroTime timer = Time.toMicroseconds (getTime timer)

(* determines if the timers are still running *)
fun stillMilliTime timer limit = getMilliTime timer > limit
fun stillMicroTime timer limit = getMicroTime timer > limit

(* convercts times to strings *)
fun milliToString st timer = st ^ LargeInt.toString (getMilliTime timer) ^ SY.unitT1
fun microToString st timer = st ^ LargeInt.toString (getMicroTime timer) ^ SY.unitT2

end
