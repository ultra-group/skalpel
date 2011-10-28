(* Copyright 2010 John Pirie
 * Copyright 2011 Heriot-Watt University
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
 *  o Authors:     John Pirie
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        15 October 2010
 *  o File name:   SlicerOptArgs.sig
 *  o Description: Defines the signature SLICEROPTARGS which is the signature
 *      of the structure which provides functionality for optional arguments to
 *      the slicer
 *)

(* this is the structure that we use so that we can use optional arguments when
 * we want to use slicerFull *)

structure SlicerOptArgs : SLICEROPTARGS = struct
datatype ('a, 'b) opt_key_arg =
	 FILEBAS     of string
       | FILEIN      of string
       | FILEHTML    of string
       | FILEXML     of string
       | FILESML     of string
       | FILEJSON    of string
       | FILELISP    of string
       | FILEPERL    of string
       | BASOP       of int
       | TIMELIM     of int
       | TAB         of int
       | SOL         of int
       | MIN         of bool
       | DEV         of bool
       | BCS         of int
       | SEARCHSPACE of int

(* a series of refs are used here to control the optional arguments *)
fun optArg argListList =
    let val fileBasRef     = ref []
        val fileInRef      = ref []
        val fileHtmlRef    = ref []
        val fileXmlRef     = ref []
        val fileSmlRef     = ref []
        val fileJsonRef    = ref []
        val fileLispRef    = ref []
        val filePerlRef    = ref []
        val basOpRef       = ref []
        val timeLimRef     = ref []
        val tabRef         = ref []
        val solRef         = ref []
        val minRef         = ref []
        val devRef         = ref []
        val bcsRef         = ref []
        val searchSpaceRef = ref []
        fun handleArg (FILEBAS x)     = fileBasRef     := x :: !(fileBasRef)
          | handleArg (FILEIN x)      = fileInRef      := x :: !(fileInRef)
          | handleArg (FILEHTML x)    = fileHtmlRef    := x :: !(fileHtmlRef)
          | handleArg (FILEXML x)     = fileXmlRef     := x :: !(fileXmlRef)
          | handleArg (FILESML x)     = fileSmlRef     := x :: !(fileSmlRef)
          | handleArg (FILEJSON x)    = fileJsonRef    := x :: !(fileJsonRef)
          | handleArg (FILELISP x)    = fileLispRef    := x :: !(fileLispRef)
          | handleArg (FILEPERL x)    = filePerlRef    := x :: !(filePerlRef)
          | handleArg (BASOP x)       = basOpRef       := x :: !(basOpRef)
          | handleArg (TIMELIM x)     = timeLimRef     := x :: !(timeLimRef)
          | handleArg (TAB x)         = tabRef         := x :: !(tabRef)
          | handleArg (SOL x)         = solRef         := x :: !(solRef)
          | handleArg (MIN x)         = minRef         := x :: !(minRef)
          | handleArg (DEV x)         = devRef         := x :: !(devRef)
          | handleArg (BCS x)         = bcsRef         := x :: !(bcsRef)
          | handleArg (SEARCHSPACE x) = searchSpaceRef := x :: !(searchSpaceRef);
    in app (app handleArg) argListList;
       {fileBas     = (fn nil => ""    | x => hd x) (!fileBasRef),
        fileIn      = (fn nil => ""    | x => hd x) (!fileInRef),
        fileHtml    = (fn nil => ""    | x => hd x) (!fileHtmlRef),
        fileXml     = (fn nil => ""    | x => hd x) (!fileXmlRef),
        fileSml     = (fn nil => ""    | x => hd x) (!fileSmlRef),
        fileJson    = (fn nil => ""    | x => hd x) (!fileJsonRef),
        fileLisp    = (fn nil => ""    | x => hd x) (!fileLispRef),
        filePerl    = (fn nil => ""    | x => hd x) (!filePerlRef),
        basOp       = (fn nil => 2     | x => hd x) (!basOpRef),
        timeLim     = (fn nil => 5000  | x => hd x) (!timeLimRef),
        tab         = (fn nil => 8     | x => hd x) (!tabRef),
        sol         = (fn nil => 9     | x => hd x) (!solRef),
        min         = (fn nil => false | x => true) (!minRef),
        dev         = (fn nil => false | x => hd x) (!devRef),
        bcs         = (fn nil => false | x => true) (!bcsRef),
        searchSpace = (fn nil => 1     | x => hd x) (!searchSpaceRef)}
    end
end;
