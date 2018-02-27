(* Copyright 2009 2010 2011 2012 Heriot-Watt University
 * Copyright 2018 Christian Gregg
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
 *  o Date:        24 May 2010
 *  o File name:   Error.sig
 *)

(** Defines the signature ERROR for type errors. *)
signature ERROR = sig

    type id
    type error    = {id   : id,
		     labs : Label.labels,
		     deps : LongId.set,
		     ek   : ErrorKind.kind,
		     rf   : Label.labels,
		     bb   : bool,
		     rem  : id list,
		     time : LargeInt.int,
		     sl   : AstSML.progs,
		     regs : ExtReg.regs,
		     min  : bool}

    type times = LargeInt.int * LargeInt.int * LargeInt.int * LargeInt.int * LargeInt.int

    type export = error list   ->
		  AstSML.packs ->
		  bool         ->
		  times        ->
		  Env.envContextSensitiveSyntaxPair   ->
		  Label.label  ->
		  string       ->
		  string       ->
		  int          ->
		  int          ->
		  unit

    type export' = error list   ->
		   AstSML.packs ->
		   Env.envContextSensitiveSyntaxPair   ->
		   int          ->
		   int          ->
		   unit


    val removeBasisSlice : string -> string

    val resetError       : unit -> unit
    val getError         : unit -> id
    val freshError       : unit -> id

    val dummyId          : id

    val getI             : error -> id
    val getL             : error -> Label.labels
    val getD             : error -> LongId.set
    val getK             : error -> ErrorKind.kind
    val getE             : error -> id list
    val getF             : error -> Label.labels
    val getB             : error -> bool
    val getT             : error -> LargeInt.int
    val getS             : error -> AstSML.progs
    val getR             : error -> ExtReg.regs
    val getM             : error -> bool

    val setI             : error -> id             -> error
    val setL             : error -> Label.labels   -> error
    val setD             : error -> LongId.set     -> error
    val setK             : error -> ErrorKind.kind -> error
    val setE             : error -> id list        -> error
    val setF             : error -> Label.labels   -> error
    val setB             : error -> bool           -> error
    val setT             : error -> LargeInt.int   -> error
    val setS             : error -> AstSML.progs   -> error
    val setR             : error -> ExtReg.regs    -> error
    val setM             : error -> bool           -> error

    val consError        : id                ->
			   Label.labels      ->
			   LongId.set        ->
			   ErrorKind.kind    ->
			   Label.labels      ->
			   bool              ->
			   id list           ->
			   LargeInt.int      ->
			   AstSML.progs      ->
			   ExtReg.regs       ->
			   bool              ->
			   error

    val consPreError     : id                ->
			   Label.labels      ->
			   LongId.set        ->
			   ErrorKind.kind    ->
			   Label.labels      ->
			   error


    val consErrorNoR     : id                ->
			   Label.labels      ->
			   LongId.set        ->
			   ErrorKind.kind    ->
			   Label.labels      ->
			   error

    val consErrorNoRB    : id                ->
			   Label.labels      ->
			   LongId.set        ->
			   ErrorKind.kind    ->
			   error

    val stripDummy       : error -> error
    val stripDummys       : error -> error

    val orderErrors      : error list -> error list
    val recordSpeTreat   : error list -> error list
    val getMergedErrors  : error list -> error list
    val getMinErrors     : error list -> error list
    val getNewErrors     : error list -> error list -> error list
    val consWeight       : error list -> error list -> error list
    val sepsemsyn        : error list -> error list * error list
    val alreadyone       : error list -> error -> bool
    val mindone          : error list -> error -> error option * error list
    val mindone'         : error list -> error -> error option * error list
    val getErrorList     : error list -> int -> error option

    val labelError       : error -> Label.labels -> Label.labels -> LongId.set -> error

    val idToInt          : id -> int

    val printOneXmlErr   : error  -> string -> bool   -> string

    val printOneXmlErrTuple   : error  ->
				string ->
				bool   ->
				int    ->
				string * string * string * string * string *
				string * string * string * string * string * string


    val printOneSmlErr   : error -> bool  -> int ->
			   string *
			   string *
			   string *
			   string *
			   string *
			   string *
			   string
    val printOneJsonErr  : error -> bool  -> int ->
			   string *
			   string *
			   string *
			   string *
			   string *
			   string *
			   string
    val printOneLispErr  : error -> Id.assoc -> bool -> int ->
			   string *
			   string *
			   string *
			   string *
			   string *
			   string *
			   string *
			   string
    val printOnePerlErr  : error -> Id.assoc -> bool -> int ->
			   string *
			   string *
			   string *
			   string *
			   string *
			   string *
			   string

    val printOneBashErr  : error -> Id.assoc -> bool -> int -> unit

    val setSlice         : AstSML.progs -> error -> error
    val setSlices        : AstSML.progs -> error list -> error list

    val setReg           : error      -> bool -> error
    val setRegs          : error list -> bool -> error list

    val printErrorLabels : error -> string
    val printErrorListLabels : error list -> string

    val labelsFromError : error -> int * Label.labels
    val labelsFromList : error list -> (int * Label.labels) list
end
