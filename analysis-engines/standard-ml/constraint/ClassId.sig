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
 *  o File name:   ClassId.sig
 *  o Description: Defines the signature CLASSID which is the signature
 *      for identifiers' classes.
 *)


signature CLASSID = sig

    type classvar

    datatype vid    = VAL (* Undefined status                   *)
		    | PAT (* Non applied identifier in pattern  *) (* Generated at unification when it turns out that a binder is an accessor *)
		    | REC (* Value variable                     *)
		    | VRC (* Constrained to be a value variable *)
		    | DA0 (* 0-ary constructor                  *)
		    | DA1 (* 1-ary constructor                  *)
		    | DAT (* Constructor                        *)
		    | CO0 (* 0-ary datatype constructor         *)
		    | CO1 (* 1-ary datatype constructor         *)
		    | CON (* Datatype constructor               *)
		    | EX0 (* 0-ary exception                    *)
		    | EX1 (* 1-ary exception                    *)
		    | EXC (* Exception                          *)

    datatype class  = VID of vid        (* values                  *)
		    | TYCON             (* type constructors       *)
		    | TYVAR             (* explicit type variables *)
		    | STR               (* structures              *)
		    | SIG               (* signatures              *)
		    | FUNC              (* functors                *)
		    | OC                (* overloading classes     *)
		    | CLVAR of classvar (* class variables         *)
		    | ANY               (* Unknown status          *) (* Generated at constraint filtering *)
    (* o DAT and TYP are for types
     *     - DAT is for datatypes (if the list of constructors is empty then it's a type function):
     *         -- the sequence variable is to check the arity of bound type constructors
     *         -- the list is the list of constructors of the datatype (id and label)
     *         -- the boolean is true if no id is sliced out during enumeration
     *     - TYP is for free type constructors
     *         -- the sequence variable is to check the arity of free type constructors
     * o PAT, CON, VAL, REC, EXC are for value variables (REC), datatype constructors (CON) and exceptions (EXC); (VAL and PAT are when the status is unknown)
     *     - PAT is for identifiers occurring at binding positions and with unknown status:
     *         -- the first set of labels is not empty if the value has to be a variable
     *            (it comes from VAL)
     *         -- the second set of labels is the set of labels constraining an identifier
     *            to be in a pattern (for example the equal in the mrule (only case?)).
     *     - CON is for value constructor:
     *         -- the set of labels is for the application in patterns or the labels of 'of'
     *            and '=' if in conbind (those are not used yet)
     *     - VAL is for free identifiers:
     *         ++ in extty the poly might contain some expansivness information on the expression
     *            associated to the value in a pattern,
     *         -- the set of labels is not empty if the value has to be a variable - left of an "as"
     *     - REC is for recursive binders:
     *         -- the labels are the labels associated to the name of the function
     *         ++ in extty, poly might contain some expansivness information coming from VAL
     *            when an id is confirmed to be a value variable
     *         -- the second set of label is for the labels of the ids
     *            which force the extty to be for a value variable
     *            Can we have: SOME O.empty?
     *     - OI is for overloaded identifier:
     *         -- The (ty * ty) option is for overloaded identifier.
     *            The first type is the type in the pattern function which is equal
     *            to the second type which is an Or type.
     *     - EXC is for exceptions:
     *         -- the labels that make an exception be an exception such as 'excepion' and '=' in
     *              exception .. = e
     *         ++ for exceptions in extty (see below) we should have the poly field
     *            marked as monomorphic with the labels as the labels that make an identifier
     *            be an exception such as exception binding in case the exception is a binding
     *            then the label is the label of the labid then used in compcstgen2
     * o ANY is for any status
     *     -- The boolean is false if it was previously a REC, true for a EXC/CON and NONE otherwise *)


    (*val tyToMap           : Id.set -> cons map
    val domMap            : 'a map -> Id.set
    val emMap             : 'a map
    val consMap           : Id.id -> 'a -> 'a map -> 'a map
    val getMap            : Id.id -> 'a map -> 'a option
    val foldliMap         : ((Id.id * 'a * 'b) -> 'b) -> 'b -> 'a map -> 'b
    val mapMap            : ('a -> 'b) -> 'a map -> 'b map
    val appi              : ((Id.id * 'a) -> unit) -> 'a map -> unit

    val genConsI          : Id.lid -> cons
    val genConsC          : ccons  -> cons
    val emCons            : cons*)

    val classvarToInt     : classvar -> int


    (* Class constructors *)
    val consVAL           : unit -> class
    val consPAT           : unit -> class
    val consREC           : unit -> class
    val consVRC           : unit -> class
    val consCO0           : unit -> class
    val consCO1           : unit -> class
    val consCON           : unit -> class
    val consDA0           : unit -> class
    val consDA1           : unit -> class
    val consDAT           : unit -> class
    val consEX0           : unit -> class
    val consEX1           : unit -> class
    val consEXC           : unit -> class
    val consANY           : unit -> class
    val consTYVAR         : unit -> class
    val consTYCON         : unit -> class
    val consSTR           : unit -> class
    val consSIG           : unit -> class
    val consFUNC          : unit -> class
    val consOC            : unit -> class

    val newClassVar             : unit -> class

    (*val consDATcons       : cons          -> class
    val consREClabs       : OrdSet.labels -> class
    val consTYPsv         : Ty.seqvar     -> class
    val consVID           : vid           -> class

    val consDATcons'      : cons -> unit -> class*)

    (* Classes checkings *)
    val classIsVAL        : class -> bool
    val classIsPAT        : class -> bool
    val classIsREC        : class -> bool
    val classIsVRC        : class -> bool
    val classIsDA0        : class -> bool
    val classIsEX0        : class -> bool
    val classIsDAT        : class -> bool
    val classIsEXC        : class -> bool
    (* classIsCON checks whether class is a DAT(0/1) a EXC(0/1) or a CON(0/1) *)
    val classIsCON        : class -> bool

    (*val classIsDAT        : class -> bool
    val classIsTYP        : class -> bool*)

    val classIsVID        : class -> bool
    val classIsTYCON      : class -> bool
    val classIsSTR        : class -> bool
    val classIsSIG        : class -> bool
    val classIsOC         : class -> bool
    val classIsVAR        : class -> bool
    val classIsANY        : class -> bool

    (*(* Checks if the first class can be a binder of the second class *)
    val compatible        : class -> class -> bool*)

    (* Transforms a VAL or a PAT into a PAT *)
    val classToPAT        : class -> class
    (* Transforms a VAL into a REC *)
    val classToREC        : class -> class
    (* Transforms a VAL into a VRC *)
    val classToVRC        : class -> class
    (* Transforms a VAL or a DAT into a DAT *)
    val classToDA0        : class -> class
    (* Transforms a VAL or a DAT into a DAT *)
    val classToDA1        : class -> class
    (* Transforms a VAL or a DAT into a DAT *)
    val classToDAT        : class -> class
    (* Transforms a VAL into a EXC *)
    val classToEX0        : class -> class
    (* Transforms a VAL into a EXC *)
    val classToEX1        : class -> class
    (* Transforms any vid into a ANY *)
    val classToANY        : class -> class

    (*(* Reset the labels of a REC *)
    val setClassREClabs   : class -> OrdSet.labels -> class

    (* Returns the labels constraining an identifier in a pattern to be
     * a value variable because of 'as' *)
    val getClassPATval    : class -> OrdSet.labels

    (* Returns the constructors of a datatype. *)
    val getClassDATcons   : class -> cons
    val getClassDATconsC  : class -> ccons  (* We wanna get rid of this one *)
    val getClassDATconsI  : class -> Id.set (* What is that used for?       *)

    (* Checks if it is a DAT (CONS []). *)
    val isClassDATem      : class -> bool
    val isConsDATem       : cons  -> bool

    (* Checks if the class is a TYCON (DAT (CONS _)). *)
    val isClassDATcons    : class -> bool

    (* Returns the labels of the constructors of a datatype. *)
    val getClassDATlabs   : class -> OrdSet.labels

    (* Returns the information contained inside a REC constraint *)
    val getClassREC       : class -> (OrdSet.labels * OrdSet.labels option) option

    (* Returns the labels associated the name in the different branches
     * of a function defined with 'fun'*)
    val getClassREClabs   : class -> OrdSet.labels

    (* Returns the labels responsible for the status of the id *)
    val getClassRECst     : class -> OrdSet.labels

    (* Returns the vid stored into a class *)
    val getClassVID       : class -> vid

    (* returns the information constained into TYP classes *)
    val getTYP            : class -> Ty.seqvar
    val classTYP          : class list -> Ty.seqvar list

    (* returns the sequence variable contained in any of the TYCONs *)
    val getTYCONseq       : class -> Ty.seqvar

    (* returns the labels contained in a CON *)
    val getClassCON       : class -> OrdSet.labels

    (* Returns the labels contained in a EXC *)
    val getClassEXC       : class -> OrdSet.labels

    (* Checks if the value status if an identifier has been confirmed *)
    val isForcedREC       : class -> bool

    (* Applies a function to the sequence stored into a TYCON *)
    val classTYCONmapSeq  : class -> (Ty.seqvar -> Ty.seqvar) -> class

    (* Applies a function to the type of the constructor of a type name *)
    val consDATmapCons    : cons  -> (Ty.ty -> Ty.ty) -> cons
    val classDATmapCons   : class -> (Ty.ty -> Ty.ty) -> class

    (* Sets the classvar option of a class if it's not already done *)
    val setClassVar       : class -> class
    (* Gets the classvar option from a class *)
    val getClassVar       : class -> classvar option
    (* Gets the pclass from a class *)
    val getPClass         : class -> pclass
    (* Checks if the classvar is set *)
    val isClassVar        : class -> bool*)

    val resetClVar     : unit -> unit

    (*(* Printing functions *)
    val printClassVar     : classvar -> string*)

    val toString          : class    -> string

end
