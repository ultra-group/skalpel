(* Copyright 2009 2010 Heriot-Watt University
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
 *  o Date:        25 May 2010
 *  o File name:   ClassId.sig
 *)

(** Defines the signature CLASSID which is the signature for identifiers' classes. *)
signature CLASSID = sig

    type classvar

    datatype vid = VAL | PAT | REC | VRC | DA0 | DA1 | DAT | CO0 | CO1 | CON | EX0 | EX1 | EXC
    datatype class  = VID of vid | TYCON | TYVAR | STR | SIG | FUNC | OC | CLVAR of classvar | ANY

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


    val classvarToInt     : classvar -> int


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

    val classIsVAL        : class -> bool
    val classIsPAT        : class -> bool
    val classIsREC        : class -> bool
    val classIsVRC        : class -> bool
    val classIsDA0        : class -> bool
    val classIsEX0        : class -> bool
    val classIsDAT        : class -> bool
    val classIsEXC        : class -> bool
    val classIsCON        : class -> bool

    val classIsVID        : class -> bool
    val classIsTYCON      : class -> bool
    val classIsSTR        : class -> bool
    val classIsSIG        : class -> bool
    val classIsOC         : class -> bool
    val classIsVAR        : class -> bool
    val classIsANY        : class -> bool

    val classToPAT        : class -> class
    val classToREC        : class -> class
    val classToVRC        : class -> class
    val classToDA0        : class -> class
    val classToDA1        : class -> class
    val classToDAT        : class -> class
    val classToEX0        : class -> class
    val classToEX1        : class -> class
    val classToANY        : class -> class

    val resetClVar     : unit -> unit

    val toString          : class    -> string

end
