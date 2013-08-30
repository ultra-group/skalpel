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

(** Has the signature VTIMER, defines interactions with timers. *)
structure VTimer :> VTIMER = struct

structure SY = SymbSlice

(** Defines the timer type, containing two timers counting both CPU time, and real time taken. *)
type timer = {real : Timer.real_timer,
	      cpu  : Timer.cpu_timer}

(** Starts both the Real and CPU timers. *)
fun startTimer () = {real = Timer.startRealTimer (),
		     cpu  = Timer.startCPUTimer ()}

(** Returns the time the timer has been running in seconds. *)
fun getTime (timer : timer) = Timer.checkRealTimer (#real timer)

(** Returns the time the timer has been running in miliseconds. *)
fun getMilliTime timer = Time.toMilliseconds (getTime timer)

(** Returns the time the timer has been running in microseconds. *)
fun getMicroTime timer = Time.toMicroseconds (getTime timer)

(** Checks whether given a timer and a time limit in miliseconds, the timer in has ran over the limit. *)
fun stillMilliTime timer limit = getMilliTime timer > limit

(** Checks whether given a timer and a time limit in microseconds, the timer in has ran over the limit. *)
fun stillMicroTime timer limit = getMicroTime timer > limit

(** Converts milisecond time report to a strings. *)
fun milliToString st timer = st ^ LargeInt.toString (getMilliTime timer) ^ SY.unitT1

(** Converts microsecond time report to a strings. *)
fun microToString st timer = st ^ LargeInt.toString (getMicroTime timer) ^ SY.unitT2

end
