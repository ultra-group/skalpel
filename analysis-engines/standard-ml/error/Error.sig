(* Copyright 2009 Heriot-Watt University
 * Copyright 2010 Heriot-Watt University
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
 *  o Authors:     Vincent Rahli
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        24 May 2010
 *  o File name:   Error.sig
 *  o Description: Defines the signature ERROR for type errors.
 *)


signature ERROR = sig

    type id
    (* id   : identifier of the error
     * labs : labels
     * deps : context dependencies
     * ek   : kind of the error
     * rf   : identifiers of functions constraining statuses in the error - rf stands for Recursive Function
     * bb   : true if the error involves the builtin basis
     * rem  : the errors invalidated by id
     * time : time to enumerate the error
     * min  : true if the error is minimal and false if it is in the process of being minimised *)
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
		  Env.envcss   ->
		  Label.label  ->
		  string       ->
		  string       ->
		  int          ->
		  int          ->
		  unit

    (* type of a function that export some dummy fields. *)
    type export' = error list   ->
		   AstSML.packs ->
		   Env.envcss   ->
		   int          ->
		   int          ->
		   unit


    val resetError       : unit -> unit
    val getError         : unit -> id
    val freshError       : unit -> id

    val dummyId          : id

    (* Accessors *)
    val getI             : error -> id                (* I for Identifier           *)
    val getL             : error -> Label.labels      (* L for Labels               *)
    val getD             : error -> LongId.set        (* D for context Dependencies *)
    val getK             : error -> ErrorKind.kind    (* K for Kind of the error    *)
    val getE             : error -> id list           (* E for errors to be Errase  *)
    val getF             : error -> Label.labels      (* F for bound Function       *)
    val getB             : error -> bool              (* B for Builtin basis        *)
    val getT             : error -> LargeInt.int      (* T for Time                 *)
    val getS             : error -> AstSML.progs      (* S for Slice                *)
    val getR             : error -> ExtReg.regs       (* R for Regions              *)
    val getM             : error -> bool              (* M for Minimal              *)

    (* Modifiers *)
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

    (* Constructors *)
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
			   Label.label       -> (* This label is not used *)
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

    (* Discard the dummy label from the labels of the error. *)
    val stripDummy       : error -> error
    (* Discard the dummy and builtin labels from the labels of the error. *)
    val stripDummys       : error -> error

    val orderErrors      : error list -> error list
    val recordSpeTreat   : error list -> error list
    val getMergedErrors  : error list -> error list
    val getMinErrors     : error list -> error list
    val getNewErrors     : error list -> error list -> error list
    val consWeight       : error list -> error list -> error list
    val sepsemsyn        : error list -> error list * error list
    val alreadyone       : error list -> error -> bool
    (* mindone: does not do mindone' checking because errors are supposed to be minimal anyway *)
    val mindone          : error list -> error -> error option * error list
    (* mindone': checks if there is already a minimal error or if the new error invalidate a already found error *)
    val mindone'         : error list -> error -> error option * error list
    val getErrorList     : error list -> int -> error option

    val labelError       : error -> Label.labels -> Label.labels -> LongId.set -> error

    val idToInt          : id -> int

    (* In all these printOne functions, the Boolean is used so that in the database,
     * we enter only inlined slices *)
    val printOneXmlErr   : error  ->
			   string -> (* indentation *)
			   bool   -> (* true if we want to pretty print the slices *)
			   string

    (* In all these printOne functions, the Boolean is used so that in the database,
     * we enter only inlined slices *)
    val printOneXmlErrTuple   : error  ->
				string -> (* indentation *)
				bool   -> (* true if we want to pretty print the slices *)
				int    -> (* overloading basis *)
				string * string * string * string * string *
				string * string * string * string * string * string


    val printOneSmlErr   : error -> bool  -> int ->
			   string * (* identifier           *)
			   string * (* labels               *)
			   string * (* context dependencies *)
			   string * (* error kind           *)
			   string * (* time information     *)
			   string * (* slice                *)
			   string   (* regions              *)
    val printOneJsonErr  : error -> bool  -> int ->
			   string * (* identifier           *)
			   string * (* labels               *)
			   string * (* context dependencies *)
			   string * (* error kind           *)
			   string * (* time information     *)
			   string * (* slice                *)
			   string   (* regions              *)
    val printOneLispErr  : error -> Id.assoc -> bool -> int ->
			   string * (* identifier                           *)
			   string * (* context dependencies                 *)
			   string * (* error kind                           *)
			   string * (* error invalidated by the printed one *)
			   string * (* slice                                *)
			   string * (* ast                                  *)
			   string * (* regions                              *)
			   string   (* minimal                              *)
    val printOnePerlErr  : error -> Id.assoc -> bool -> int ->
			   string * (* identifier                           *)
			   string * (* context dependencies                 *)
			   string * (* error kind                           *)
			   string * (* error invalidated by the printed one *)
			   string * (* slice                                *)
			   string * (* regions                              *)
			   string   (* minimal                              *)

    val setSlice         : AstSML.progs -> error -> error
    val setSlices        : AstSML.progs -> error list -> error list
    (* the Boolean has to be true if we want to merge consecutive similar regions
     * see ExtReg.sig *)
    val setReg           : error      -> bool -> error
    val setRegs          : error list -> bool -> error list

    (*val printError       : error list -> string*)
    (*val printErrorBis    : error list -> AstSML.prog -> string*)
    (*val printErrorBisBis : error list -> AstSML.prog -> string*)

end
