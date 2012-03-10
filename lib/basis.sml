(* Copyright 2009 Heriot-Watt University
 * Copyright 2010 Heriot-Watt University
 *
 *
 * This file is part of the ULTRA SML Type Error Slicer (SKALPEL) -
 * a Type Error Slicer for Standard ML written by the ULTRA Group of
 * Heriot-Watt University, Edinburgh.
 *
 * SKALPEL is a free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * SKALPEL is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with SKALPEL.  If not, see <http://www.gnu.org/licenses/>.
 *
 *  o Authors:     Joe Wells, Vincent Rahli
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        19 March 2009
 *  o File name:   basis.sml
 *  o Description: Contains a part of the SML basis library and of
 *      the SML/NJ library.
 *)

(* Check opaque/translucent signatures for the SML/NJ structures. *)

(* We put these definitions in a structure and then open it because an
 * eqtype can only be declared as a spec, and for the other type
 * definitions it currently does not work to define them with dummy
 * definitions because they are currently only recognized as the real
 * built-in types if they are specs in a structure named Basis. *)

structure Basis :> sig

datatype bool = false | true
val not      : bool -> bool

eqtype int
val +    : int * int -> int
val -    : int * int -> int
val *    : int * int -> int
val div  : int * int -> int
val mod  : int * int -> int
val <    : int * int -> bool
val <=   : int * int -> bool
val >    : int * int -> bool
val >=   : int * int -> bool
val ~    : int -> int
val abs  : int -> int

type unit = {}
val before : 'a * unit -> 'a
val ignore : 'a -> unit

datatype 'a list = nil | :: of 'a * 'a list
val null   : 'a list -> bool
val hd     : 'a list -> 'a
val tl     : 'a list -> 'a list
val length : 'a list -> int
val rev    : 'a list -> 'a list
val @      : 'a list * 'a list -> 'a list
val app    : ('a -> unit) -> 'a list -> unit
val map    : ('a -> 'b) -> 'a list -> 'b list
val foldl  : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
val foldr  : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b

datatype 'a ref = ref of 'a
val !  : 'a ref -> 'a
val := : 'a ref * 'a -> unit

eqtype string
eqtype char
val size      : string -> int
val substring : string * int * int -> string
val ^         : string * string -> string
val concat    : string list -> string
val str       : char -> string
val implode   : char list -> string
val explode   : string -> char list
val print     : string -> unit
val chr       : int -> char
val ord       : char -> int

datatype 'a option = NONE | SOME of 'a
val getOpt : 'a option * 'a -> 'a
val isSome : 'a option -> bool
val valOf  : 'a option -> 'a

datatype order = EQUAL | GREATER | LESS

type real
val /     : real * real -> real
val ceil  : real -> int
val floor : real -> int
val real  : int -> real (* fromInt *)
val round : real -> int
val trunc : real -> int

type exn
val exnMessage : exn -> string
val exnName    : exn -> string

eqtype 'a vector
val vector : 'a list -> 'a vector

val o   : ('a -> 'b) * ('c -> 'a) -> 'c -> 'b
val use : string -> unit (* implementation dependent *)

eqtype word
type substring
eqtype 'a array

exception Bind
exception Chr
exception Div
exception Domain
exception Empty
exception Fail of string
exception Match
exception Option
exception Overflow
exception Size
exception Span
exception Subscript

end = _structBasis

open Basis

signature LIST =
  sig
    datatype 'a list = :: of 'a * 'a list | nil
    exception Empty
    val null : 'a list -> bool
    val hd : 'a list -> 'a
    val tl : 'a list -> 'a list
    val last : 'a list -> 'a
    val getItem : 'a list -> ('a * 'a list) option
    val nth : 'a list * int -> 'a
    val take : 'a list * int -> 'a list
    val drop : 'a list * int -> 'a list
    val length : 'a list -> int
    val rev : 'a list -> 'a list
    val @ : 'a list * 'a list -> 'a list
    val concat : 'a list list -> 'a list
    val revAppend : 'a list * 'a list -> 'a list
    val app : ('a -> unit) -> 'a list -> unit
    val map : ('a -> 'b) -> 'a list -> 'b list
    val mapPartial : ('a -> 'b option) -> 'a list -> 'b list
    val find : ('a -> bool) -> 'a list -> 'a option
    val filter : ('a -> bool) -> 'a list -> 'a list
    val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
    val exists : ('a -> bool) -> 'a list -> bool
    val all : ('a -> bool) -> 'a list -> bool
    val tabulate : int * (int -> 'a) -> 'a list
    val collate : ('a * 'a -> order) -> 'a list * 'a list -> order
  end

structure List :> LIST
    where type 'a list = 'a list = _structList

signature LIST_PAIR =
  sig
    exception UnequalLengths
    val zip : 'a list * 'b list -> ('a * 'b) list
    val zipEq : 'a list * 'b list -> ('a * 'b) list
    val unzip : ('a * 'b) list -> 'a list * 'b list
    val map : ('a * 'b -> 'c) -> 'a list * 'b list -> 'c list
    val mapEq : ('a * 'b -> 'c) -> 'a list * 'b list -> 'c list
    val app : ('a * 'b -> unit) -> 'a list * 'b list -> unit
    val appEq : ('a * 'b -> unit) -> 'a list * 'b list -> unit
    val foldl : ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c
    val foldr : ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c
    val foldlEq : ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c
    val foldrEq : ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c
    val all : ('a * 'b -> bool) -> 'a list * 'b list -> bool
    val allEq : ('a * 'b -> bool) -> 'a list * 'b list -> bool
    val exists : ('a * 'b -> bool) -> 'a list * 'b list -> bool
  end

structure ListPair :> LIST_PAIR = _structListPair

signature STRING_CVT =
  sig
    datatype radix = BIN | DEC | HEX | OCT
    datatype realfmt
      = EXACT | FIX of int option | GEN of int option | SCI of int option
    type ('a,'b) reader = 'b -> ('a * 'b) option
    val padLeft : char -> int -> string -> string
    val padRight : char -> int -> string -> string
    val splitl : (char -> bool) -> (char,'a) reader -> 'a -> string * 'a
    val takel : (char -> bool) -> (char,'a) reader -> 'a -> string
    val dropl : (char -> bool) -> (char,'a) reader -> 'a -> 'a
    val skipWS : (char,'a) reader -> 'a -> 'a
    type cs
    val scanString : ((char,cs) reader -> ('a,cs) reader)
                     -> string -> 'a option
  end

structure StringCvt :> STRING_CVT = _structStringCvt

signature CHAR =
  sig
    eqtype char
    eqtype string
    val chr : int -> char
    val ord : char -> int
    val minChar : char
    val maxChar : char
    val maxOrd : int
    val pred : char -> char
    val succ : char -> char
    val < : char * char -> bool
    val <= : char * char -> bool
    val > : char * char -> bool
    val >= : char * char -> bool
    val compare : char * char -> order
    val scan : (char,'a) StringCvt.reader -> (char,'a) StringCvt.reader
    val fromString : (*String.*)string -> char option
    val toString : char -> (*String.*)string
    val fromCString : (*String.*)string -> char option
    val toCString : char -> (*String.*)string
    val contains : string -> char -> bool
    val notContains : string -> char -> bool
    val isLower : char -> bool
    val isUpper : char -> bool
    val isDigit : char -> bool
    val isAlpha : char -> bool
    val isHexDigit : char -> bool
    val isAlphaNum : char -> bool
    val isPrint : char -> bool
    val isSpace : char -> bool
    val isPunct : char -> bool
    val isGraph : char -> bool
    val isCntrl : char -> bool
    val isAscii : char -> bool
    val toUpper : char -> char
    val toLower : char -> char
  end

structure Char :> CHAR
    where type char = char
    where type string = (*String.*)string = _structChar

signature STRING =
  sig
    eqtype char
    eqtype string
    val maxSize : int
    val size : string -> int
    val sub : string * int -> char
    val extract : string * int * int option -> string
    val substring : string * int * int -> string
    val ^ : string * string -> string
    val concat : string list -> string
    val concatWith : string -> string list -> string
    val str : char -> string
    val implode : char list -> string
    val explode : string -> char list
    val map : (char -> char) -> string -> string
    val translate : (char -> string) -> string -> string
    val tokens : (char -> bool) -> string -> string list
    val fields : (char -> bool) -> string -> string list
    val isPrefix : string -> string -> bool
    val isSubstring : string -> string -> bool
    val isSuffix : string -> string -> bool
    val compare : string * string -> order
    val collate : (char * char -> order) -> string * string -> order
    val < : string * string -> bool
    val <= : string * string -> bool
    val > : string * string -> bool
    val >= : string * string -> bool
    val fromString : (*String.*)string -> string option
    val toString : string -> (*String.*)string
    (*val fromCString : String.string -> string option
    val toCString : string -> String.string*)
  end

structure String :> STRING
    where type string = string
    (*where type string = CharVector.vector*)
    where type char = Char.char = _structString

signature MONO_VECTOR_SLICE =
  sig
    type elem
    type vector
    type slice

    val length : slice -> int
    val sub : slice * int -> elem
    val full : vector -> slice
    val slice : vector * int * int option -> slice
    val subslice : slice * int * int option -> slice
    val base : slice -> vector * int * int
    val vector : slice -> vector
    val concat : slice list -> vector
    val isEmpty : slice -> bool
    val getItem : slice -> (elem * slice) option
    val appi : (int * elem -> unit) -> slice -> unit
    val app  : (elem -> unit) -> slice -> unit
    val mapi : (int * elem -> elem) -> slice -> vector
    val map  : (elem -> elem) -> slice -> vector
    val foldli : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b
    val foldr  : (elem * 'b -> 'b) -> 'b -> slice -> 'b
    val foldl  : (elem * 'b -> 'b) -> 'b -> slice -> 'b
    val foldri : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b
    val findi : (int * elem -> bool)
		-> slice -> (int * elem) option
    val find  : (elem -> bool) -> slice -> elem option
    val exists : (elem -> bool) -> slice -> bool
    val all : (elem -> bool) -> slice -> bool
    val collate : (elem * elem -> order)
                  -> slice * slice -> order
  end

structure CharVectorSlice :> MONO_VECTOR_SLICE
    where type slice  = (*Substring.*)substring
    where type vector = String.string
    where type elem   = char = _structCharVectorSlice

signature SUBSTRING =
  sig
    type substring
    eqtype char
    eqtype string

    val sub : substring * int -> char
    val size : substring -> int
    val base : substring -> string * int * int
    val extract   : string * int * int option -> substring
    val substring : string * int * int -> substring
    val full : string -> substring
    val string : substring -> string
    val isEmpty : substring -> bool
    val getc : substring -> (char * substring) option
    val first : substring -> char option
    val triml : int -> substring -> substring
    val trimr : int -> substring -> substring
    val slice : substring * int * int option -> substring
    val concat : substring list -> string
    val concatWith : string -> substring list -> string
    val explode : substring -> char list
    val isPrefix    : string -> substring -> bool
    val isSubstring : string -> substring -> bool
    val isSuffix    : string -> substring -> bool
    val compare : substring * substring -> order
    val collate : (char * char -> order)
                    -> substring * substring -> order
    val splitl : (char -> bool)
                   -> substring -> substring * substring
    val splitr : (char -> bool)
                   -> substring -> substring * substring
    val splitAt : substring * int -> substring * substring
    val dropl : (char -> bool) -> substring -> substring
    val dropr : (char -> bool) -> substring -> substring
    val takel : (char -> bool) -> substring -> substring
    val taker : (char -> bool) -> substring -> substring
    val position : string -> substring -> substring * substring
    val span : substring * substring -> substring
    val translate : (char -> string) -> substring -> string
    val tokens : (char -> bool) -> substring -> substring list
    val fields : (char -> bool) -> substring -> substring list
    val app : (char -> unit) -> substring -> unit
    val foldl : (char * 'a -> 'a) -> 'a -> substring -> 'a
    val foldr : (char * 'a -> 'a) -> 'a -> substring -> 'a
end

structure Substring :> SUBSTRING
    where type substring = CharVectorSlice.slice
    where type string = String.string
    where type char = Char.char = _structSubtring
(*structure WideSubstring :> SUBSTRING
    where type substring = WideCharVectorSlice.slice
    where type string = WideString.string
    where type char = WideChar.char = _structWideSubtring*)

(* Dummy Int structure which is rebound below.
 * we define this structure to be able to use the
 * int type in the INT_INF signature. *)
structure Int :> sig type int = int end = _structInt

(* Befor the INT_INF signature and IntInf structure were
 * defined after INTEGER and Int, Int32, ... but we need
 * IntInf in INTEGER. *)
signature INT_INF =
  sig
    (* It includes INTEGER *)
    type int(* = int*)
    val precision : (*Int31*)Int.int option
    val minInt : int option
    val maxInt : int option
    val toLarge : int -> int
    val fromLarge : int -> int
    val toInt : int -> (*Int31*)Int.int
    val fromInt : (*Int31*)Int.int -> int
    val ~ : int -> int
    val + : int * int -> int
    val - : int * int -> int
    val * : int * int -> int
    val div : int * int -> int
    val mod : int * int -> int
    val quot : int * int -> int
    val rem : int * int -> int
    val min : int * int -> int
    val max : int * int -> int
    val abs : int -> int
    val sign : int -> (*Int31*)Int.int
    val sameSign : int * int -> bool
    val > : int * int -> bool
    val >= : int * int -> bool
    val < : int * int -> bool
    val <= : int * int -> bool
    val compare : int * int -> order
    val toString : int -> string
    val fromString : string -> int option
    val scan : StringCvt.radix
               -> (char,'a) StringCvt.reader -> (int,'a) StringCvt.reader
    val fmt : StringCvt.radix -> int -> string
    val divMod : int * int -> int * int
    val quotRem : int * int -> int * int
    val pow : int * (*Int31*)Int.int -> int
    val log2 : int -> (*Int31*)Int.int
    val orb : int * int -> int
    val xorb : int * int -> int
    val andb : int * int -> int
    val notb : int -> int
    val << : int * word -> int
    val ~>> : int * word -> int
  end

structure IntInf :> INT_INF
(*!*)(*where type int = int*) = _structIntInf

signature INTEGER =
  sig
    eqtype int
    val precision : (*Int31.*)Int.int option
    val minInt : int option
    val maxInt : int option
    val toLarge : int -> IntInf.int
    val fromLarge : IntInf.int -> int
    val toInt : int -> (*Int31*)Int.int
    val fromInt : (*Int31*)Int.int -> int
    val ~ : int -> int
    val + : int * int -> int
    val - : int * int -> int
    val * : int * int -> int
    val div : int * int -> int
    val mod : int * int -> int
    val quot : int * int -> int
    val rem : int * int -> int
    val min : int * int -> int
    val max : int * int -> int
    val abs : int -> int
    val sign : int -> (*Int31*)Int.int
    val sameSign : int * int -> bool
    val > : int * int -> bool
    val >= : int * int -> bool
    val < : int * int -> bool
    val <= : int * int -> bool
    val compare : int * int -> order
    val toString : int -> string
    val fromString : string -> int option
    val scan : StringCvt.radix
               -> (char,'a) StringCvt.reader -> (int,'a) StringCvt.reader
    val fmt : StringCvt.radix -> int -> string
  end

structure Int      :> INTEGER
    where type int = int      = _structInt
structure Int31    :> INTEGER
(*!*)(*where type int = int*) = _structInt31
structure Int32    :> INTEGER = _structInt32
structure Int64    :> INTEGER = _structInt64
structure Position :> INTEGER
(*!*)(*where type int = int*) = _structPosition
structure LargeInt :> INTEGER
    where type int = IntInf.int = _structLargeInt

(* This is a hack to get a structure LargeWord defining a type word *)
signature LARGEWORD_HACK = sig type word end
structure LargeWord :> LARGEWORD_HACK = _structLargeWord_Hack

signature WORD =
  sig
    eqtype word
    val wordSize : int
    val toLarge : word -> LargeWord.word
    val toLargeWord : word -> LargeWord.word
    val toLargeX : word -> LargeWord.word
    val toLargeWordX : word -> LargeWord.word
    val fromLarge : LargeWord.word -> word
    val fromLargeWord : LargeWord.word -> word
    val toLargeInt : word -> IntInf.int
    val toLargeIntX : word -> IntInf.int
    val fromLargeInt : IntInf.int -> word
    val toInt : word -> int
    val toIntX : word -> int
    val fromInt : int -> word
    val orb : word * word -> word
    val xorb : word * word -> word
    val andb : word * word -> word
    val notb : word -> word
    val << : word * (*Word31.*)word -> word
    val >> : word * (*Word31.*)word -> word
    val ~>> : word * (*Word31.*)word -> word
    val + : word * word -> word
    val - : word * word -> word
    val * : word * word -> word
    val div : word * word -> word
    val mod : word * word -> word
    val compare : word * word -> order
    val > : word * word -> bool
    val >= : word * word -> bool
    val < : word * word -> bool
    val <= : word * word -> bool
    val ~ : word -> word
    val min : word * word -> word
    val max : word * word -> word
    val scan : StringCvt.radix
               -> (char,'a) StringCvt.reader -> (word,'a) StringCvt.reader
    val fromString : string -> word option
    val fmt : StringCvt.radix -> word -> string
    val toString : word -> string
  end

structure Word      :> WORD
    where type word = word     = _structWord
structure Word8     :> WORD    = _structWord8
structure Word31    :> WORD
(*!*)(*where type word = word*)= _structWord31
structure Word32    :> WORD    = _structWord32
structure Word64    :> WORD    = _structWord64
structure LargeWord :> WORD
    where type word = LargeWord.word
                               = _structLargeWord
structure SysWord   :> WORD    = _structSysWord

signature MONO_VECTOR =
  sig
    type vector
    type elem
    val maxLen : int
    val fromList : elem list -> vector
    val tabulate : int * (int -> elem) -> vector
    val length : vector -> int
    val sub : vector * int -> elem
    val concat : vector list -> vector
    val update : vector * int * elem -> vector
    val appi : (int * elem -> unit) -> vector -> unit
    val app : (elem -> unit) -> vector -> unit
    val mapi : (int * elem -> elem) -> vector -> vector
    val map : (elem -> elem) -> vector -> vector
    val foldli : (int * elem * 'a -> 'a) -> 'a -> vector -> 'a
    val foldri : (int * elem * 'a -> 'a) -> 'a -> vector -> 'a
    val foldl : (elem * 'a -> 'a) -> 'a -> vector -> 'a
    val foldr : (elem * 'a -> 'a) -> 'a -> vector -> 'a
    val findi : (int * elem -> bool) -> vector -> (int * elem) option
    val find : (elem -> bool) -> vector -> elem option
    val exists : (elem -> bool) -> vector -> bool
    val all : (elem -> bool) -> vector -> bool
    val collate : (elem * elem -> order) -> vector * vector -> order
  end

structure Word8Vector :> MONO_VECTOR
    where type elem = Word8.word = _structWord8Vector
structure CharVector :> MONO_VECTOR
    where type vector = String.string
    where type elem   = char     = _structCharVector

signature MONO_ARRAY =
  sig
    eqtype array
    type elem
    type vector
    val maxLen : int
    val array : int * elem -> array
    val fromList : elem list -> array
    val tabulate : int * (int -> elem) -> array
    val length : array -> int
    val sub : array * int -> elem
    val update : array * int * elem -> unit
    val vector : array -> vector
    val copy : {di:int, dst:array, src:array} -> unit
    val copyVec : {di:int, dst:array, src:vector} -> unit
    val appi : (int * elem -> unit) -> array -> unit
    val app : (elem -> unit) -> array -> unit
    val modifyi : (int * elem -> elem) -> array -> unit
    val modify : (elem -> elem) -> array -> unit
    val foldli : (int * elem * 'a -> 'a) -> 'a -> array -> 'a
    val foldri : (int * elem * 'a -> 'a) -> 'a -> array -> 'a
    val foldl : (elem * 'a -> 'a) -> 'a -> array -> 'a
    val foldr : (elem * 'a -> 'a) -> 'a -> array -> 'a
    val findi : (int * elem -> bool) -> array -> (int * elem) option
    val find : (elem -> bool) -> array -> elem option
    val exists : (elem -> bool) -> array -> bool
    val all : (elem -> bool) -> array -> bool
    val collate : (elem * elem -> order) -> array * array -> order
  end

structure Word8Array :> MONO_ARRAY
    where type vector = Word8Vector.vector
    where type elem   = Word8.word = _structWord8Array
structure CharArray :> MONO_ARRAY
    where type vector = CharVector.vector
    where type elem   = char       = _structCharArray

signature MONO_ARRAY_SLICE = sig

    type elem
    type array
    type slice
    type vector
    type vector_slice

    val length : slice -> int
    val sub    : slice * int -> elem
    val update : slice * int * elem -> unit

    val full     : array -> slice
    val slice    : array * int * int option -> slice
    val subslice : slice * int * int option -> slice

    val base   : slice -> array * int * int
    val vector : slice -> vector

    val copy    : { src : slice, dst : array, di : int } -> unit
    val copyVec : { src: vector_slice, dst : array, di : int } -> unit

    val isEmpty : slice -> bool
    val getItem : slice -> (elem * slice) option

    val appi    : (int * elem -> unit) -> slice -> unit
    val app     : (elem -> unit) -> slice -> unit
    val modifyi : (int * elem -> elem) -> slice -> unit
    val modify  : (elem -> elem) -> slice -> unit

    val foldli : (int * elem * 'a -> 'a) -> 'a -> slice -> 'a
    val foldri : (int * elem * 'a -> 'a) -> 'a -> slice -> 'a
    val foldl  : (elem * 'a -> 'a) -> 'a -> slice -> 'a
    val foldr  : (elem * 'a -> 'a) -> 'a -> slice -> 'a

    val findi  : (int * elem -> bool) -> slice -> (int * elem) option
    val find   : (elem -> bool) -> slice -> elem option
    val exists : (elem -> bool) -> slice -> bool
    val all    : (elem -> bool) -> slice -> bool
    val collate: (elem * elem -> order) -> slice * slice -> order

end

structure CharArraySlice :> MONO_ARRAY_SLICE
				where type elem = char
				where type array = CharArray.array
				where type vector = CharVector.vector
				where type vector_slice = CharVectorSlice.slice
= _structCharArraySlice

signature BOOL =
  sig
    datatype bool = false | true
    val not : bool -> bool
    val toString : bool -> string
    val fromString : string -> bool option
    val scan : (char,'a) StringCvt.reader -> (bool,'a) StringCvt.reader
  end

structure Bool :> BOOL
    where type bool = bool = _structBool

signature IEEE_REAL =
  sig
    exception Unordered
    datatype real_order = EQUAL | GREATER | LESS | UNORDERED
    datatype nan_mode = QUIET | SIGNALLING
    datatype float_class = INF | NAN of nan_mode | NORMAL | SUBNORMAL | ZERO
    datatype rounding_mode = TO_NEAREST | TO_NEGINF | TO_POSINF | TO_ZERO
    val setRoundingMode : rounding_mode -> unit
    val getRoundingMode : unit -> rounding_mode
    type decimal_approx =
	 {digits:int list, exp:int, kind:float_class, sign:bool}
    val toString : decimal_approx -> string
    val fromString : string -> decimal_approx option
    val scan : (char,'a) StringCvt.reader
               -> (decimal_approx,'a) StringCvt.reader
  end

structure IEEEReal :> IEEE_REAL = _structIEEEReal

signature MATH =
sig
    type real
    val pi    : real
    val e     : real
    val sqrt  : real -> real
    val sin   : real -> real
    val cos   : real -> real
    val tan   : real -> real
    val asin  : real -> real
    val acos  : real -> real
    val atan  : real -> real
    val atan2 : real * real -> real
    val exp   : real -> real
    val pow   : real * real -> real
    val ln    : real -> real
    val log10 : real -> real
    val sinh  : real -> real
    val cosh  : real -> real
    val tanh  : real -> real
end

signature REAL =
  sig
    type real
    structure Math :(*>?*) MATH where type real = real
    val radix : int
    val precision : int
    val maxFinite : real
    val minPos : real
    val minNormalPos : real
    val posInf : real
    val negInf : real
    val + : real * real -> real
    val - : real * real -> real
    val * : real * real -> real
    val / : real * real -> real
    val *+ : real * real * real -> real
    val *- : real * real * real -> real
    val ~ : real -> real
    val abs : real -> real
    val min : real * real -> real
    val max : real * real -> real
    val sign : real -> int
    val signBit : real -> bool
    val sameSign : real * real -> bool
    val copySign : real * real -> real
    val compare : real * real -> order
    val compareReal : real * real -> IEEEReal.real_order
    val < : real * real -> bool
    val <= : real * real -> bool
    val > : real * real -> bool
    val >= : real * real -> bool
    val == : real * real -> bool
    val != : real * real -> bool
    val ?= : real * real -> bool
    val unordered : real * real -> bool
    val isFinite : real -> bool
    val isNan : real -> bool
    val isNormal : real -> bool
    val class : real -> IEEEReal.float_class
    val fmt : StringCvt.realfmt -> real -> string
    val toString : real -> string
    val fromString : string -> real option
    val scan : (char,'a) StringCvt.reader -> (real,'a) StringCvt.reader
    val toManExp : real -> {exp:int, man:real}
    val fromManExp : {exp:int, man:real} -> real
    val split : real -> {frac:real, whole:real}
    val realMod : real -> real
    val rem : real * real -> real
    val nextAfter : real * real -> real
    val checkFloat : real -> real
    val floor : real -> int
    val ceil : real -> int
    val trunc : real -> int
    val round : real -> int
    val realFloor : real -> real
    val realCeil : real -> real
    val realTrunc : real -> real
    val realRound : real -> real
    val toInt : IEEEReal.rounding_mode -> real -> int
    val toLargeInt : IEEEReal.rounding_mode -> real -> IntInf.int
    val fromInt : int -> real
    val fromLargeInt : IntInf.int -> real
    (*val toLarge : real -> Real64.real
    val fromLarge : IEEEReal.rounding_mode -> Real64.real -> real*)
    val toDecimal : real -> IEEEReal.decimal_approx
    val fromDecimal : IEEEReal.decimal_approx -> real
  end

structure Real      :> REAL
    where type real = real      = _structReal
structure Real64    :> REAL     = _structReal64
structure LargeReal :> REAL
(*!*)(*where type real = real*) = _structLargeReal
structure Math      :> MATH
    where type real = Real.real = _structMath

signature TIME =
  sig
    eqtype time
    exception Time
    val zeroTime : time
    val fromReal : real -> time
    val toReal : time -> real
    val toSeconds : time -> IntInf.int
    val fromSeconds : IntInf.int -> time
    val toMilliseconds : time -> IntInf.int
    val fromMilliseconds : IntInf.int -> time
    val toMicroseconds : time -> IntInf.int
    val fromMicroseconds : IntInf.int -> time
    val toNanoseconds : time -> IntInf.int
    val fromNanoseconds : IntInf.int -> time
    val + : time * time -> time
    val - : time * time -> time
    val compare : time * time -> order
    val < : time * time -> bool
    val <= : time * time -> bool
    val > : time * time -> bool
    val >= : time * time -> bool
    val now : unit -> time
    val toString : time -> string
    val fromString : string -> time option
    val fmt : int -> time -> string
    val scan : (char,'a) StringCvt.reader -> (time,'a) StringCvt.reader
  end

structure Time :> TIME = _structTime

signature TIMER =
  sig
    type cpu_timer
    type real_timer
    val startCPUTimer : unit -> cpu_timer
    val checkCPUTimes : cpu_timer
			-> {
                        nongc : {
                        usr : Time.time,
                        sys : Time.time
                        },
                        gc : {
                        usr : Time.time,
                        sys : Time.time
                        }
			}
    val checkCPUTimer : cpu_timer
			-> {usr : Time.time, sys : Time.time}
    val checkGCTime : cpu_timer -> Time.time
    val totalCPUTimer : unit -> cpu_timer
    val startRealTimer : unit -> real_timer
    val checkRealTimer : real_timer -> Time.time
    val totalRealTimer : unit -> real_timer
end

structure Timer :> TIMER = _structTimer

signature DATE =
  sig
    datatype weekday = Fri | Mon | Sat | Sun | Thu | Tue | Wed
    datatype month
      = Apr | Aug | Dec | Feb | Jan | Jul | Jun | Mar | May | Nov | Oct | Sep
    type date
    exception Date
    val year : date -> int
    val month : date -> month
    val day : date -> int
    val hour : date -> int
    val minute : date -> int
    val second : date -> int
    val weekDay : date -> weekday
    val yearDay : date -> int
    val isDst : date -> bool option
    val offset : date -> Time.time option
    val localOffset : unit -> Time.time
    val date : {day:int, hour:int, minute:int, month:month,
                offset:Time.time option, second:int, year:int}
               -> date
    val fromTimeLocal : Time.time -> date
    val fromTimeUniv : Time.time -> date
    val toTime : date -> Time.time
    val toString : date -> string
    val fmt : string -> date -> string
    val fromString : string -> date option
    val scan : (char,'a) StringCvt.reader -> (date,'a) StringCvt.reader
    val compare : date * date -> order
  end

structure Date :> DATE = _structDate

(* Not in the Standard ML basis library. *)
signature ATOM =
  sig
    type atom
    val atom : string -> atom
    val atom' : substring -> atom
    val toString : atom -> string
    val same : atom * atom -> bool
    val sameAtom : atom * atom -> bool
    val compare : atom * atom -> order
    val lexCompare : atom * atom -> order
    val hash : atom -> word
  end

(* Not in the Standard ML basis library. *)
structure Atom :> ATOM = _structAtom

signature OPTION =
  sig
    datatype 'a option = NONE | SOME of 'a
    exception Option
    val getOpt : 'a option * 'a -> 'a
    val isSome : 'a option -> bool
    val valOf : 'a option -> 'a
    val filter : ('a -> bool) -> 'a -> 'a option
    val join : 'a option option -> 'a option
    val app : ('a -> unit) -> 'a option -> unit
    val map : ('a -> 'b) -> 'a option -> 'b option
    val mapPartial : ('a -> 'b option) -> 'a option -> 'b option
    val compose : ('a -> 'c) * ('b -> 'a option) -> 'b -> 'c option
    val composePartial : ('a -> 'c option) * ('b -> 'a option)
                         -> 'b -> 'c option
  end

structure Option :> OPTION
    where type 'a option = 'a option = _structOption

(* Not in the Standard ML basis library. *)
signature FORMAT =
  sig
    datatype fmt_item
      = ATOM of Atom.atom
      | BOOL of bool
      | CHR of char
      | INT of int
      | LEFT of int * fmt_item
      | LINT of IntInf.int
      | LREAL of real
      | LWORD of LargeWord.word
      | REAL of real
      | RIGHT of int * fmt_item
      | STR of string
      | WORD of word
      | WORD8 of Word8.word
    exception BadFormat
    exception BadFmtList
    val format : string -> fmt_item list -> string
    val formatf : string -> (string -> unit) -> fmt_item list -> unit
  end

(* Not in the Standard ML basis library. *)
structure Format :> FORMAT = _structFormat

signature IO =
  sig
    exception Io of {cause:exn, function:string, name:string}
    exception BlockingNotSupported
    exception NonblockingNotSupported
    exception RandomAccessNotSupported
    exception TerminatedStream
    exception ClosedStream
    datatype buffer_mode = BLOCK_BUF | LINE_BUF | NO_BUF
  end

structure IO :> IO = _structIO

signature OS =
  sig
    eqtype syserror
    val errorName : syserror -> string
    val syserror : string -> syserror option
    val errorMsg : syserror -> string
    exception SysErr of string * syserror option
    structure FileSys :
      sig
        type dirstream
        val openDir : string -> dirstream
        val readDir : dirstream -> string option
        val rewindDir : dirstream -> unit
        val closeDir : dirstream -> unit
        val chDir : string -> unit
        val getDir : unit -> string
        val mkDir : string -> unit
        val rmDir : string -> unit
        val isDir : string -> bool
        val isLink : string -> bool
        val readLink : string -> string
        val fullPath : string -> string
        val realPath : string -> string
        val fileSize : string -> int
        val modTime : string -> Time.time
        val setTime : string * Time.time option -> unit
        val remove : string -> unit
        val rename : {new:string, old:string} -> unit
        datatype access_mode = A_EXEC | A_READ | A_WRITE
        val access : string * access_mode list -> bool
        val tmpName : unit -> string
        eqtype file_id
        val fileId : string -> file_id
        val hash : file_id -> word
        val compare : file_id * file_id -> order
      end
    structure Path :
      sig
        exception Path
	exception InvalidArc
        val parentArc : string
        val currentArc : string
        val validVolume : {isAbs:bool, vol:string} -> bool
        val fromString : string -> {arcs:string list, isAbs:bool, vol:string}
        val toString : {arcs:string list, isAbs:bool, vol:string} -> string
        val getVolume : string -> string
        val getParent : string -> string
        val splitDirFile : string -> {dir:string, file:string}
        val joinDirFile : {dir:string, file:string} -> string
        val dir : string -> string
        val file : string -> string
        val splitBaseExt : string -> {base:string, ext:string option}
        val joinBaseExt : {base:string, ext:string option} -> string
        val base : string -> string
        val ext : string -> string option
        val mkCanonical : string -> string
        val isCanonical : string -> bool
        val mkAbsolute : {path:string, relativeTo:string} -> string
        val mkRelative : {path:string, relativeTo:string} -> string
        val isAbsolute : string -> bool
        val isRelative : string -> bool
        val isRoot : string -> bool
        val concat : string * string -> string
        val fromUnixPath : string -> string
        val toUnixPath : string -> string
      end
    structure Process :
      sig
        eqtype status
        val success : status
        val failure : status
        val isSuccess : status -> bool
        val system : string -> status
        val atExit : (unit -> unit) -> unit
        val exit : status -> 'a
        val terminate : status -> 'a
        val getEnv : string -> string option
        val sleep : Time.time -> unit
      end
    structure IO :
      sig
        eqtype iodesc
        eqtype iodesc_kind
        val hash : iodesc -> word
        val compare : iodesc * iodesc -> order
        val kind : iodesc -> iodesc_kind
        structure Kind :
          sig
            val file : iodesc_kind
            val dir : iodesc_kind
            val symlink : iodesc_kind
            val tty : iodesc_kind
            val pipe : iodesc_kind
            val socket : iodesc_kind
            val device : iodesc_kind
          end
        type poll_desc
        type poll_info
        val pollDesc : iodesc -> poll_desc option
        val pollToIODesc : poll_desc -> iodesc
        exception Poll
        val pollIn : poll_desc -> poll_desc
        val pollOut : poll_desc -> poll_desc
        val pollPri : poll_desc -> poll_desc
        val poll : poll_desc list * Time.time option -> poll_info list
        val isIn : poll_info -> bool
        val isOut : poll_info -> bool
        val isPri : poll_info -> bool
        val infoToPollDesc : poll_info -> poll_desc
      end
  end

structure OS :> OS = _structOS

signature TEXT_IO =
  sig
    type vector = string
    type elem = char
    type instream
    type outstream
    val input : instream -> vector
    val input1 : instream -> elem option
    val inputN : instream * int -> vector
    val inputAll : instream -> vector
    val canInput : instream * int -> int option
    val lookahead : instream -> elem option
    val closeIn : instream -> unit
    val endOfStream : instream -> bool
    val output : outstream * vector -> unit
    val output1 : outstream * elem -> unit
    val flushOut : outstream -> unit
    val closeOut : outstream -> unit
    structure StreamIO :
      sig
        type vector = string
        type elem = char
        type reader
        type writer
        type instream
        type outstream
        type pos
        type out_pos
        val input : instream -> vector * instream
        val input1 : instream -> (elem * instream) option
        val inputN : instream * int -> vector * instream
        val inputAll : instream -> vector * instream
        val canInput : instream * int -> int option
        val closeIn : instream -> unit
        val endOfStream : instream -> bool
        val mkInstream : reader * vector -> instream
        val getReader : instream -> reader * vector
        val filePosIn : instream -> pos
        val output : outstream * vector -> unit
        val output1 : outstream * elem -> unit
        val flushOut : outstream -> unit
        val closeOut : outstream -> unit
        val setBufferMode : outstream * IO.buffer_mode -> unit
        val getBufferMode : outstream -> IO.buffer_mode
        val mkOutstream : writer * IO.buffer_mode -> outstream
        val getWriter : outstream -> writer * IO.buffer_mode
        val getPosOut : outstream -> out_pos
        val setPosOut : out_pos -> unit
        val filePosOut : out_pos -> pos
        val inputLine : instream -> (string * instream) option
        val outputSubstr : outstream * substring -> unit
      end
    val mkInstream : StreamIO.instream -> instream
    val getInstream : instream -> StreamIO.instream
    val setInstream : instream * StreamIO.instream -> unit
    val getPosOut : outstream -> StreamIO.out_pos
    val setPosOut : outstream * StreamIO.out_pos -> unit
    val mkOutstream : StreamIO.outstream -> outstream
    val getOutstream : outstream -> StreamIO.outstream
    val setOutstream : outstream * StreamIO.outstream -> unit
    val inputLine : instream -> string option
    val outputSubstr : outstream * substring -> unit
    val openIn : string -> instream
    val openString : string -> instream
    val openOut : string -> outstream
    val openAppend : string -> outstream
    val stdIn : instream
    val stdOut : outstream
    val stdErr : outstream
    val print : string -> unit
    val scanStream : ((elem,StreamIO.instream) StringCvt.reader
                      -> ('a,StreamIO.instream) StringCvt.reader)
                     -> instream -> 'a option
  end

signature PRIM_IO =
  sig
    type elem
    type vector
    type vector_slice
    type array
    type array_slice
    eqtype pos
    val compare : pos * pos -> order
    datatype reader
      = RD of {avail:unit -> int option, block:(unit -> unit) option,
               canInput:(unit -> bool) option, chunkSize:int,
               close:unit -> unit, endPos:(unit -> pos) option,
               getPos:(unit -> pos) option, ioDesc:OS.IO.iodesc option,
               name:string, readArr:(array_slice -> int) option,
               readArrNB:(array_slice -> int option) option,
               readVec:(int -> vector) option,
               readVecNB:(int -> vector option) option,
               setPos:(pos -> unit) option, verifyPos:(unit -> pos) option}
    datatype writer
      = WR of {block:(unit -> unit) option, canOutput:(unit -> bool) option,
               chunkSize:int, close:unit -> unit, endPos:(unit -> pos) option,
               getPos:(unit -> pos) option, ioDesc:OS.IO.iodesc option,
               name:string, setPos:(pos -> unit) option,
               verifyPos:(unit -> pos) option,
               writeArr:(array_slice -> int) option,
               writeArrNB:(array_slice -> int option) option,
               writeVec:(vector_slice -> int) option,
               writeVecNB:(vector_slice -> int option) option}
    val openVector : vector -> reader
    val nullRd : unit -> reader
    val nullWr : unit -> writer
    val augmentReader : reader -> reader
    val augmentWriter : writer -> writer
  end

structure BinPrimIO  :> PRIM_IO
    where type array  = Word8Array.array
    where type vector = Word8Vector.vector
    where type elem   = Word8.word
    where type pos    = Position.int = _structBinPrimIO
structure TextPrimIO :> PRIM_IO
    where type array  = CharArray.array
    where type vector = CharVector.vector
    where type elem   = Char.char    = _structTextPrimIO

structure TextIO :> TEXT_IO
    where type StreamIO.writer = TextPrimIO.writer
  = _structTextIO

(* *** The slicer is crashing on something in POSIX/Posix. *)

signature POSIX =
  sig
    structure Error :
      sig
        type syserror = OS.syserror (* SML Basis Library book erroneously says this is OS.Process.syserror *)
        val toWord : syserror -> SysWord.word
        val fromWord : SysWord.word -> syserror
        val errorMsg : syserror -> string
        val errorName : syserror -> string
        val syserror : string -> syserror option
        val toobig : syserror
        val acces : syserror
        val again : syserror
        val badf : syserror
        val badmsg : syserror
        val busy : syserror
        val canceled : syserror
        val child : syserror
        val deadlk : syserror
        val dom : syserror
        val exist : syserror
        val fault : syserror
        val fbig : syserror
        val inprogress : syserror
        val intr : syserror
        val inval : syserror
        val io : syserror
        val isdir : syserror
        val loop : syserror
        val mfile : syserror
        val mlink : syserror
        val msgsize : syserror
        val nametoolong : syserror
        val nfile : syserror
        val nodev : syserror
        val noent : syserror
        val noexec : syserror
        val nolck : syserror
        val nomem : syserror
        val nospc : syserror
        val nosys : syserror
        val notdir : syserror
        val notempty : syserror
        val notsup : syserror
        val notty : syserror
        val nxio : syserror
        val perm : syserror
        val pipe : syserror
        val range : syserror
        val rofs : syserror
        val spipe : syserror
        val srch : syserror
        val xdev : syserror
      end
    structure Signal :
      sig
        eqtype signal
        val toWord : signal -> SysWord.word
        val fromWord : SysWord.word -> signal
        val abrt : signal
        val alrm : signal
        val fpe : signal
        val hup : signal
        val ill : signal
        val int : signal
        val kill : signal
        val pipe : signal
        val quit : signal
        val segv : signal
        val term : signal
        val usr1 : signal
        val usr2 : signal
        val chld : signal
        val cont : signal
        val stop : signal
        val tstp : signal
        val ttin : signal
        val ttou : signal
        val bus : signal
      end
    structure Process :
      sig
        eqtype signal
        eqtype pid
        val wordToPid : SysWord.word -> pid
        val pidToWord : pid -> SysWord.word
        val fork : unit -> pid option
        val exec : string * string list -> 'a
        val exece : string * string list * string list -> 'a
        val execp : string * string list -> 'a
        datatype waitpid_arg
          = W_ANY_CHILD | W_CHILD of pid | W_GROUP of pid | W_SAME_GROUP
        datatype exit_status
          = W_EXITED
          | W_EXITSTATUS of Word8.word
          | W_SIGNALED of signal
          | W_STOPPED of signal
        val fromStatus : OS.Process.status -> exit_status
        structure W :
          sig
            eqtype flags
            val toWord : flags -> SysWord.word
            val fromWord : SysWord.word -> flags
            val all : flags
            val flags : flags list -> flags
            val intersect : flags list -> flags
            val clear : flags * flags -> flags
            val allSet : flags * flags -> bool
            val anySet : flags * flags -> bool
            val untraced : flags
          end
        val wait : unit -> pid * exit_status
        val waitpid : waitpid_arg * W.flags list -> pid * exit_status
        val waitpid_nh : waitpid_arg * W.flags list
                         -> (pid * exit_status) option
        val exit : Word8.word -> 'a
        datatype killpid_arg = K_GROUP of pid | K_PROC of pid | K_SAME_GROUP
        val kill : killpid_arg * signal -> unit
        val alarm : Time.time -> Time.time
        val pause : unit -> unit
        val sleep : Time.time -> Time.time
      end
    structure ProcEnv :
      sig
        eqtype pid
        eqtype file_desc
        eqtype uid
	eqtype gid
        val uidToWord : uid -> SysWord.word
        val wordToUid : SysWord.word -> uid
        val gidToWord : gid -> SysWord.word
        val wordToGid : SysWord.word -> gid
        val getpid : unit -> pid
        val getppid : unit -> pid
        val getuid : unit -> uid
        val geteuid : unit -> uid
        val getgid : unit -> gid
        val getegid : unit -> gid
        val setuid : uid -> unit
        val setgid : gid -> unit
        val getgroups : unit -> gid list
        val getlogin : unit -> string
        val getpgrp : unit -> pid
        val setsid : unit -> pid
        val setpgid : {pgid:pid option, pid:pid option} -> unit
        val uname : unit -> (string * string) list
	val time : unit -> Time.time
        val times : unit
                    -> {cstime:Time.time, cutime:Time.time, elapsed:Time.time,
                        stime:Time.time, utime:Time.time}
        val getenv : string -> string option
        val environ : unit -> string list
        val ctermid : unit -> string
        val ttyname : file_desc -> string
        val isatty : file_desc -> bool
        val sysconf : string -> SysWord.word
      end
    structure FileSys :
      sig
        eqtype uid
        eqtype gid
        eqtype file_desc
        val fdToWord : file_desc -> SysWord.word
        val wordToFD : SysWord.word -> file_desc
        val fdToIOD : file_desc -> OS.IO.iodesc
        val iodToFD : OS.IO.iodesc -> file_desc option
        type dirstream
        val opendir : string -> dirstream
        val readdir : dirstream -> string option
        val rewinddir : dirstream -> unit
        val closedir : dirstream -> unit
        val chdir : string -> unit
        val getcwd : unit -> string
        val stdin : file_desc
        val stdout : file_desc
        val stderr : file_desc
        structure S :
          sig
            type mode
            type flags = mode
            val toWord : flags -> SysWord.word
            val fromWord : SysWord.word -> flags
            val all : flags
            val flags : flags list -> flags
            val intersect : flags list -> flags
            val clear : flags * flags -> flags
            val allSet : flags * flags -> bool
            val anySet : flags * flags -> bool
            val irwxu : mode
            val irusr : mode
            val iwusr : mode
            val ixusr : mode
            val irwxg : mode
            val irgrp : mode
            val iwgrp : mode
            val ixgrp : mode
            val irwxo : mode
            val iroth : mode
            val iwoth : mode
            val ixoth : mode
            val isuid : mode
            val isgid : mode
          end
        datatype open_mode = O_RDONLY | O_RDWR | O_WRONLY
        structure O :
          sig
            eqtype flags
            val toWord : flags -> SysWord.word
            val fromWord : SysWord.word -> flags
            val all : flags
            val flags : flags list -> flags
            val intersect : flags list -> flags
            val clear : flags * flags -> flags
            val allSet : flags * flags -> bool
            val anySet : flags * flags -> bool
            val append : flags
            val dsync : flags
            val excl : flags
            val noctty : flags
            val nonblock : flags
            val rsync : flags
            val sync : flags
            val trunc : flags
          end
        val openf : string * open_mode * O.flags -> file_desc
        val createf : string * open_mode * O.flags * S.mode -> file_desc
        val creat : string * S.mode -> file_desc
        val umask : S.mode -> S.mode
        val link : {new:string, old:string} -> unit
        val mkdir : string * S.mode -> unit
        val mkfifo : string * S.mode -> unit
        val unlink : string -> unit
        val rmdir : string -> unit
        val rename : {new:string, old:string} -> unit
        val symlink : {new:string, old:string} -> unit
        val readlink : string -> string
        val ftruncate : file_desc * int -> unit
        eqtype dev
        (*val wordToDev : SysWord.word -> dev
        val devToWord : dev -> SysWord.word*)
        eqtype ino
        (*val wordToIno : SysWord.word -> ino
        val inoToWord : ino -> SysWord.word*)
        structure ST :
          sig
            type stat
            val isDir : stat -> bool
            val isChr : stat -> bool
            val isBlk : stat -> bool
            val isReg : stat -> bool
            val isFIFO : stat -> bool
            val isLink : stat -> bool
            val isSock : stat -> bool
            val mode : stat -> S.mode
            val ino : stat -> ino
            val dev : stat -> dev
            val nlink : stat -> int
            val uid : stat -> uid
            val gid : stat -> gid
            val size : stat -> int
            val atime : stat -> Time.time
            val mtime : stat -> Time.time
            val ctime : stat -> Time.time
          end
        val stat : string -> ST.stat
        val lstat : string -> ST.stat
        val fstat : file_desc -> ST.stat
        datatype access_mode = A_EXEC | A_READ | A_WRITE
        val access : string * access_mode list -> bool
        val chmod : string * S.mode -> unit
        (*val fchmod : file_desc * S.mode -> unit*)
        val chown : string * uid * gid -> unit
        val fchown : file_desc * uid * gid -> unit
        (*val utime : string * {actime:Time.time, modtime:Time.time} option
                    -> unit *)
        val pathconf : string * string -> SysWord.word option
        val fpathconf : file_desc * string -> SysWord.word option
      end
    structure IO :
      sig
        eqtype file_desc
        eqtype pid
        val pipe : unit -> {infd:file_desc, outfd:file_desc}
        val dup : file_desc -> file_desc
        val dup2 : {new:file_desc, old:file_desc} -> unit
        val close : file_desc -> unit
        (*val readVec : file_desc * int -> Word8Vector.vector*)
        (*val readArr : file_desc * Word8ArraySlice.slice -> int*)
        (*val writeVec : file_desc * Word8VectorSlice.slice -> int*)
        (*val writeArr : file_desc * Word8ArraySlice.slice -> int*)
        datatype whence = SEEK_CUR | SEEK_END | SEEK_SET
        structure FD :
          sig
            eqtype flags
            val toWord : flags -> SysWord.word
            val fromWord : SysWord.word -> flags
            val all : flags
            val flags : flags list -> flags
            val intersect : flags list -> flags
            val clear : flags * flags -> flags
            val allSet : flags * flags -> bool
            val anySet : flags * flags -> bool
            val cloexec : flags
          end
        structure O :
          sig
            eqtype flags
            val toWord : flags -> SysWord.word
            val fromWord : SysWord.word -> flags
            val all : flags
            val flags : flags list -> flags
            val intersect : flags list -> flags
            val clear : flags * flags -> flags
            val allSet : flags * flags -> bool
            val anySet : flags * flags -> bool
            val append : flags
            val nonblock : flags
            val sync : flags
            val rsync : flags
            val dsync : flags
          end
        datatype open_mode = O_RDONLY | O_RDWR | O_WRONLY
        val dupfd : {base:file_desc, old:file_desc} -> file_desc
        val getfd : file_desc -> FD.flags
        val setfd : file_desc * FD.flags -> unit
        val getfl : file_desc -> O.flags * open_mode
        val setfl : file_desc * O.flags -> unit
        val lseek : file_desc * int * whence -> int
        val fsync : file_desc -> unit
        datatype lock_type = F_RDLCK | F_UNLCK | F_WRLCK
        structure FLock :
          sig
            type flock
            val flock : {len:int, ltype:lock_type, pid:pid option, start:int,
                         whence:whence}
                        -> flock
            val ltype : flock -> lock_type
            val whence : flock -> whence
            val start : flock -> int
            val len : flock -> int
            val pid : flock -> pid option
          end
        val getlk : file_desc * FLock.flock -> FLock.flock
        val setlk : file_desc * FLock.flock -> FLock.flock
        val setlkw : file_desc * FLock.flock -> FLock.flock
	val mkBinReader : {fd:file_desc, initBlkMode:bool, name:string}
                          -> BinPrimIO.reader
        val mkTextReader : {fd:file_desc, initBlkMode:bool, name:string}
                           -> TextPrimIO.reader
        val mkBinWriter : {appendMode:bool, chunkSize:int, fd:file_desc,
                           initBlkMode:bool, name:string}
                          -> BinPrimIO.writer
        val mkTextWriter : {appendMode:bool, chunkSize:int, fd:file_desc,
                            initBlkMode:bool, name:string}
                           -> TextPrimIO.writer
      end
    structure SysDB :
      sig
        eqtype uid
        eqtype gid
        structure Passwd :
          sig
            type passwd
            val name : passwd -> string
            val uid : passwd -> uid
            val gid : passwd -> gid
            val home : passwd -> string
            val shell : passwd -> string
          end
        structure Group :
          sig
            type group
            val name : group -> string
            val gid : group -> gid
            val members : group -> string list
          end
        val getgrgid : gid -> Group.group
        val getgrnam : string -> Group.group
        val getpwuid : uid -> Passwd.passwd
        val getpwnam : string -> Passwd.passwd
      end
    structure TTY :
      sig
        eqtype pid
        eqtype file_desc
        structure I :
          sig
            eqtype flags
            val toWord : flags -> SysWord.word
            val fromWord : SysWord.word -> flags
            val all : flags
            val flags : flags list -> flags
            val intersect : flags list -> flags
            val clear : flags * flags -> flags
            val allSet : flags * flags -> bool
            val anySet : flags * flags -> bool
            val brkint : flags
            val icrnl : flags
            val ignbrk : flags
            val igncr : flags
            val ignpar : flags
            val inlcr : flags
            val inpck : flags
            val istrip : flags
            val ixoff : flags
            val ixon : flags
            val parmrk : flags
          end
        structure O :
          sig
            eqtype flags
            val toWord : flags -> SysWord.word
            val fromWord : SysWord.word -> flags
            val all : flags
            val flags : flags list -> flags
            val intersect : flags list -> flags
            val clear : flags * flags -> flags
            val allSet : flags * flags -> bool
            val anySet : flags * flags -> bool
            val opost : flags
          end
        structure C :
          sig
            eqtype flags
            val toWord : flags -> SysWord.word
            val fromWord : SysWord.word -> flags
            val all : flags
            val flags : flags list -> flags
            val intersect : flags list -> flags
            val clear : flags * flags -> flags
            val allSet : flags * flags -> bool
            val anySet : flags * flags -> bool
            val clocal : flags
            val cread : flags
            val csize : flags
            val cs5 : flags
            val cs6 : flags
            val cs7 : flags
            val cs8 : flags
            val cstopb : flags
            val hupcl : flags
            val parenb : flags
            val parodd : flags
          end
        structure L :
          sig
            eqtype flags
            val toWord : flags -> SysWord.word
            val fromWord : SysWord.word -> flags
            val all : flags
            val flags : flags list -> flags
            val intersect : flags list -> flags
            val clear : flags * flags -> flags
            val allSet : flags * flags -> bool
            val anySet : flags * flags -> bool
            val echo : flags
            val echoe : flags
            val echok : flags
            val echonl : flags
            val icanon : flags
            val iexten : flags
            val isig : flags
            val noflsh : flags
            val tostop : flags
          end
        structure V :
          sig
            val eof : int
            val eol : int
            val erase : int
            val intr : int
            val kill : int
            val min : int
            val quit : int
            val susp : int
            val time : int
            val start : int
            val stop : int
            val nccs : int
            type cc
            val cc : (int * char) list -> cc
            val update : cc * (int * char) list -> cc
            val sub : cc * int -> char
          end
        eqtype speed
        val compareSpeed : speed * speed -> order
        val speedToWord : speed -> SysWord.word
        val wordToSpeed : SysWord.word -> speed
        val b0 : speed
        val b50 : speed
        val b75 : speed
        val b110 : speed
        val b134 : speed
        val b150 : speed
        val b200 : speed
        val b300 : speed
        val b600 : speed
        val b1200 : speed
        val b1800 : speed
        val b2400 : speed
        val b4800 : speed
        val b9600 : speed
        val b19200 : speed
        val b38400 : speed
        type termios
        val termios : {cc:V.cc, cflag:C.flags, iflag:I.flags, ispeed:speed,
                       lflag:L.flags, oflag:O.flags, ospeed:speed}
                      -> termios
        val fieldsOf : termios
                       -> {cc:V.cc, cflag:C.flags, iflag:I.flags,
                           ispeed:speed, lflag:L.flags, oflag:O.flags,
                           ospeed:speed}
        val getiflag : termios -> I.flags
        val getoflag : termios -> O.flags
        val getcflag : termios -> C.flags
        val getlflag : termios -> L.flags
        val getcc : termios -> V.cc
        val getospeed : termios -> speed
        val setospeed : termios * speed -> termios
        val getispeed : termios -> speed
        val setispeed : termios * speed -> termios
        structure TC :
          sig
            eqtype set_action
            val sanow : set_action
            val sadrain : set_action
            val saflush : set_action
            eqtype flow_action
            val ooff : flow_action
            val oon : flow_action
            val ioff : flow_action
            val ion : flow_action
            eqtype queue_sel
            val iflush : queue_sel
            val oflush : queue_sel
            val ioflush : queue_sel
            (* For some reason in a much older SML/NJ version the
             * following items were in the parent structure. *)
            val getattr : file_desc -> termios
            val setattr : file_desc * set_action * termios -> unit
            val sendbreak : file_desc * int -> unit
            val drain : file_desc -> unit
            val flush : file_desc * queue_sel -> unit
            val flow : file_desc * flow_action -> unit
            val getpgrp : file_desc -> pid
            val setpgrp : file_desc * pid -> unit
          end
      end
    sharing type SysDB.gid = FileSys.gid = ProcEnv.gid
    sharing type SysDB.uid = FileSys.uid = ProcEnv.uid
    (*sharing type IO.open_mode = FileSys.open_mode*)
    sharing type TTY.file_desc = IO.file_desc = FileSys.file_desc = ProcEnv.file_desc
    sharing type Signal.signal = Process.signal
    sharing type TTY.pid = ProcEnv.pid = Process.pid
  end

structure Posix :> POSIX = _structPosix

signature VECTOR =
  sig
    (*eq*)type 'a vector = 'a vector
    val maxLen : int
    val fromList : 'a list -> 'a vector
    val tabulate : int * (int -> 'a) -> 'a vector
    val length : 'a vector -> int
    val sub : 'a vector * int -> 'a
    val update : 'a vector * int * 'a -> 'a vector
    val concat : 'a vector list -> 'a vector
    val appi : (int * 'a -> unit) -> 'a vector -> unit
    val app  : ('a -> unit) -> 'a vector -> unit
    val mapi : (int * 'a -> 'b) -> 'a vector -> 'b vector
    val map  : ('a -> 'b) -> 'a vector -> 'b vector
    val foldli : (int * 'a * 'b -> 'b) -> 'b -> 'a vector -> 'b
    val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a vector -> 'b
    val foldl  : ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b
    val foldr  : ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b
    val findi : (int * 'a -> bool)
		-> 'a vector -> (int * 'a) option
    val find  : ('a -> bool) -> 'a vector -> 'a option
    val exists : ('a -> bool) -> 'a vector -> bool
    val all : ('a -> bool) -> 'a vector -> bool
    val collate : ('a * 'a -> order)
                  -> 'a vector * 'a vector -> order
end

structure Vector :> VECTOR = _structVector

signature ARRAY =
  sig
    (*eq*)type 'a array = 'a array
    type 'a vector = 'a Vector.vector
    val maxLen : int
    val array : int * 'a -> 'a array
    val fromList : 'a list -> 'a array
    val tabulate : int * (int -> 'a) -> 'a array
    val length : 'a array -> int
    val sub : 'a array * int -> 'a
    val update : 'a array * int * 'a -> unit
    val vector : 'a array -> 'a vector
    val copy    : {src : 'a array, dst : 'a array, di : int}
                  -> unit
    val copyVec : {src : 'a vector, dst : 'a array, di : int}
                  -> unit
    val appi : (int * 'a -> unit) -> 'a array -> unit
    val app  : ('a -> unit) -> 'a array -> unit
    val modifyi : (int * 'a -> 'a) -> 'a array -> unit
    val modify  : ('a -> 'a) -> 'a array -> unit
    val foldli : (int * 'a * 'b -> 'b) -> 'b -> 'a array -> 'b
    val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a array -> 'b
    val foldl  : ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
    val foldr  : ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
    val findi : (int * 'a -> bool)
		-> 'a array -> (int * 'a) option
    val find  : ('a -> bool) -> 'a array -> 'a option
    val exists : ('a -> bool) -> 'a array -> bool
    val all : ('a -> bool) -> 'a array -> bool
    val collate : ('a * 'a -> order)
                  -> 'a array * 'a array -> order
end

structure Array :> ARRAY = _structArray


(* Check where to put that. *)

signature SML90 =
  sig

    type instream
    type outstream

    exception Sqrt
    exception Ln
    exception Ord
    exception Io of string
    exception Abs
    exception Quot
    exception Prod
    exception Neg
    exception Sum
    exception Diff
    exception Floor
    exception Exp
    exception Interrupt
    exception Mod

    val sqrt : real -> real
    val exp : real -> real
    val ln : real -> real
    val sin : real -> real
    val cos : real -> real
    val arctan : real -> real
    val ord : string -> int
    val chr : int -> string
    val explode : string -> string list
    val implode : string list -> string
    val std_in : instream
    val open_in : string -> instream
    val input : (instream * int) -> string
    val lookahead : instream -> string
    val close_in : instream -> unit
    val end_of_stream : instream -> bool
    val std_out : outstream
    val open_out : string -> outstream
    val output : (outstream * string) -> unit
    val close_out : outstream -> unit

  end

structure SML90 :> SML90 = _structSML90

signature UNIX = sig
    type ('a, 'b) proc
    type signal (* Posix.Signal.signal *)
    datatype exit_status = (* Posix.Process.exit_status *)
	     W_EXITED
	   | W_EXITSTATUS of Word8.word
	   | W_SIGNALED of signal
	   | W_STOPPED of signal
    val fromStatus : OS.Process.status -> exit_status
    val executeInEnv : string * string list * string list -> ('a, 'b) proc
    val execute : string * string list -> ('a, 'b) proc
    val textInstreamOf  : (TextIO.instream, 'a) proc -> TextIO.instream
    (* val binInstreamOf   : (BinIO.instream, 'a) proc -> BinIO.instream *)
    val textOutstreamOf : ('a, TextIO.outstream) proc -> TextIO.outstream
    (* val binOutstreamOf  : ('a, BinIO.outstream) proc -> BinIO.outstream *)
    val streamsOf : (TextIO.instream, TextIO.outstream) proc ->
		    TextIO.instream * TextIO.outstream
    val reap : ('a, 'b) proc -> OS.Process.status
    val kill : ('a, 'b) proc * signal -> unit
    val exit : Word8.word -> 'a
  end

structure Unix :> UNIX = _structUnix

(**********************************************************************)
(* It is all SML/NJ specific after this point.  This stuff should
   really go in its own file, but the slicer does not support multiple
   basis files yet. *)

(* Not in the Standard ML basis library. *)
signature SML_OF_NJ =
      sig
        structure Cont :
          sig
            type 'a cont (*= 'a cont*) (* Can't we have type t = u? *)
            val callcc : ('a cont -> 'a) -> 'a
            val throw : 'a cont -> 'a -> 'b
            val isolate : ('a -> unit) -> 'a cont
            type 'a control_cont (*= 'a control_cont*) (* Can't we have type t = u? *)
            val capture : ('a control_cont -> 'a) -> 'a
            val escape : 'a control_cont -> 'a -> 'b
          end
        structure IntervalTimer :
          sig
            val tick : unit -> Time.time
            val setIntTimer : Time.time option -> unit
          end
        structure Internals :
          sig
            structure CleanUp :
              sig
                datatype when
                  = AtExit | AtExportFn | AtExportML | AtInit | AtInitFn
                val atAll : when list
                val addCleaner : string * when list * (when -> unit)
                                 -> (when list * (when -> unit)) option
                val removeCleaner : string
                                    -> (when list * (when -> unit)) option
                val clean : when -> unit
              end
            (*structure ProfControl :
              sig
                val getTimeArray : unit -> int array
                val profMode : bool ref
                val current : int ref
                val profileOn : unit -> unit
                val profileOff : unit -> unit
                val getTimingMode : unit -> bool
                val getQuantum : unit -> int
                datatype compunit
                  = UNIT of {base:int, counts:int array, names:string,
                             size:int}
                val runTimeIndex : int
                val minorGCIndex : int
                val majorGCIndex : int
                val otherIndex : int
                val compileIndex : int
                val numPredefIndices : int
                val units : compunit list ref
                val reset : unit -> unit
                val spaceProfiling : bool ref
                val spaceProfRegister : (?.Unsafe.object * string
                                         -> ?.Unsafe.object) ref
              end*)
            structure GC :
              sig
                val doGC : int -> unit
                val messages : bool -> unit
              end
            val prHook : (string -> unit) ref
            val initSigTbl : unit -> unit
            val clearSigTbl : unit -> unit
            val resetSigTbl : unit -> unit
            val resetTimers : unit -> unit
            (*structure TDP :
              sig
                type plugin = ?.Core.tdp_plugin
                type monitor =
                  {monitor:bool * (unit -> unit) -> unit, name:string}
                val active_plugins : plugin list ref
                val active_monitors : monitor list ref
                val reserve : int -> int
                val reset : unit -> unit
                val idk_entry_point : int
                val idk_non_tail_call : int
                val idk_tail_call : int
                val mode : bool ref
                val with_monitors : bool -> (unit -> unit) -> unit
              end*)
          end
        structure SysInfo :
          sig
            exception UNKNOWN
            datatype os_kind = BEOS | MACOS | OS2 | UNIX | WIN32
            val getOSKind : unit -> os_kind
            val getOSName : unit -> string
            val getOSVersion : unit -> string
            val getHostArch : unit -> string
            val getTargetArch : unit -> string
            val hasSoftwarePolling : unit -> bool
            val hasMultiprocessing : unit -> bool
            val getHeapSuffix : unit -> string
          end
        structure Weak :
          sig
            type 'a weak
            val weak : 'a -> 'a weak
            val strong : 'a weak -> 'a option
            type weak'
            val weak' : 'a -> weak'
            val strong' : weak' -> bool
          end
        structure Susp :
          sig
            type 'a susp (*= 'a susp*)
            val delay : (unit -> 'a) -> 'a susp
            val force : 'a susp -> 'a
          end
        val exportML : string -> bool
        val exportFn : string * (string * string list -> OS.Process.status)
                       -> unit
        val getCmdName : unit -> string
        val getArgs : unit -> string list
        val getAllArgs : unit -> string list
        val shiftArgs : unit -> unit
        datatype 'a frag = ANTIQUOTE of 'a | QUOTE of string
        val exnHistory : exn -> string list
      end

(* Not in the Standard ML basis library. *)
structure SMLofNJ :> SML_OF_NJ = _structSMLofNJ

(* Not in the Standard ML basis library. *)
signature CONTROL =
  sig
    (*structure MC :
      sig
        val printArgs : bool ref
        val printRet : bool ref
        val bindNoVariableWarn : bool ref
        val bindNonExhaustiveWarn : bool ref
        val bindNonExhaustiveError : bool ref
        val matchNonExhaustiveWarn : bool ref
        val matchNonExhaustiveError : bool ref
        val matchRedundantWarn : bool ref
        val matchRedundantError : bool ref
      end*)
    (*structure CG :
      sig
        val tailrecur : bool ref
        val recordopt : bool ref
        val tail : bool ref
        val allocprof : bool ref
        val closureprint : bool ref
        val closureStrategy : int ref
        val lambdaopt : bool ref
        val cpsopt : string list ref
        val rounds : int ref
        val path : bool ref
        val betacontract : bool ref
        val eta : bool ref
        val selectopt : bool ref
        val dropargs : bool ref
        val deadvars : bool ref
        val flattenargs : bool ref
        val extraflatten : bool ref
        val switchopt : bool ref
        val handlerfold : bool ref
        val branchfold : bool ref
        val arithopt : bool ref
        val betaexpand : bool ref
        val unroll : bool ref
        val knownfiddle : bool ref
        val invariant : bool ref
        val targeting : int ref
        val lambdaprop : bool ref
        val newconreps : bool ref
        val boxedconstconreps : bool ref
        val sharepath : bool ref
        val staticprof : bool ref
        val unroll_recur : bool ref
        val hoistup : bool ref
        val hoistdown : bool ref
        val recordcopy : bool ref
        val recordpath : bool ref
        val debugcps : bool ref
        val misc4 : int ref
        val argrep : bool ref
        val bodysize : int ref
        val reducemore : int ref
        val alphac : bool ref
        val comment : bool ref
        val knownGen : int ref
        val knownClGen : int ref
        val escapeGen : int ref
        val calleeGen : int ref
        val spillGen : int ref
        val foldconst : bool ref
        val etasplit : bool ref
        val printit : bool ref
        val printsize : bool ref
        val scheduling : bool ref
        val cse : bool ref
        val optafterclosure : bool ref
        val uncurry : bool ref
        val ifidiom : bool ref
        val comparefold : bool ref
        val csehoist : bool ref
        val rangeopt : bool ref
        val icount : bool ref
        val debugRep : bool ref
        val checklty1 : bool ref
        val checklty2 : bool ref
        val checklty3 : bool ref
        val checkcps1 : bool ref
        val checkcps2 : bool ref
        val checkcps3 : bool ref
        val checkcps : bool ref
        val flatfblock : bool ref
        val deadup : bool ref
        val pollChecks : bool ref
        val pollRatioAtoI : real ref
        val printFlowgraphStream : TextIO.outstream ref
        val memDisambiguate : bool ref
        val controlDependence : bool ref
        val compdebugging : bool ref
        val mudebugging : bool ref
        val eedebugging : bool ref
        val insdebugging : bool ref
        val smdebugging : bool ref
        val emdebugging : bool ref
        val esdebugging : bool ref
        val etdebugging : bool ref
        val ecdebugging : bool ref
        val tmdebugging : bool ref
      end*)
    (*structure MLRISC :
      sig
        val registry : ?.ControlRegistry.registry
        val prefix : string
        val priority : ?.ControlReps.priority
        type cpu_time = {gc:Time.time, sys:Time.time, usr:Time.time}
        val mlrisc : bool ref
        val mlrisc_phases : string list ref
        val debug_stream : TextIO.outstream ref
        type 'a set = ('a,'a ref) ?.ControlSet.control_set
        val counters : int set
        val ints : int set
        val flags : bool set
        val reals : real set
        val strings : string set
        val stringLists : string list set
        val timings : cpu_time set
        val mkCounter : string * string -> int ref
        val mkInt : string * string -> int ref
        val mkFlag : string * string -> bool ref
        val mkReal : string * string -> real ref
        val mkString : string * string -> string ref
        val mkStringList : string * string -> string list ref
        val mkTiming : string * string -> cpu_time ref
        val counter : string -> int ref
        val int : string -> int ref
        val flag : string -> bool ref
        val real : string -> real ref
        val string : string -> string ref
        val stringList : string -> string list ref
        val timing : string -> cpu_time ref
        val getCounter : string -> int ref
        val getInt : string -> int ref
        val getFlag : string -> bool ref
        val getReal : string -> real ref
        val getString : string -> string ref
        val getStringList : string -> string list ref
        val getTiming : string -> cpu_time ref
      end*)
    structure Print :
      sig
        val printDepth : int ref
        val printLength : int ref
        val stringDepth : int ref
        val intinfDepth : int ref
        val printLoop : bool ref
        val signatures : int ref
        val printOpens : bool ref
        val out : {flush:unit -> unit, say:string -> unit} ref
        val linewidth : int ref
        val say : string -> unit
        val flush : unit -> unit
      end
    (*structure FLINT :
      sig
        val print : bool ref
        val printPhases : bool ref
        val printFctTypes : bool ref
        val phases : string list ref
        val inlineThreshold : int ref
        val unrollThreshold : int ref
        val maxargs : int ref
        val dropinvariant : bool ref
        val specialize : bool ref
        val sharewrap : bool ref
        val saytappinfo : bool ref
        val misc : int ref
        val check : bool ref
        val checkDatatypes : bool ref
        val checkKinds : bool ref
      end*)
    val debugging : bool ref
    val printAst : bool ref
    val printAbsyn : bool ref
    val printWarnings : bool ref
    (*val topregistry : ?.ControlRegistry.registry
    val nest : string * ?.ControlRegistry.registry * ?.ControlReps.priority
               -> unit*)
    val primaryPrompt : string ref (* This and other stuff cause problem to the slicer! *)
    val secondaryPrompt : string ref
    val lazysml : bool ref
    val overloadKW : bool ref
    val quotation : bool ref
    val saveLvarNames : bool ref
    val valueRestrictionLocalWarn : bool ref
    val valueRestrictionTopWarn : bool ref
    val multDefWarn : bool ref
    val shareDefError : bool ref
    val instantiateSigs : bool ref
    val internals : bool ref
    val interp : bool ref
    val saveLambda : bool ref
    val preserveLvarNames : bool ref
    val markabsyn : bool ref
    val trackExn : bool ref
    val polyEqWarn : bool ref
    val indexing : bool ref
    val instSigs : bool ref
    val saveit : bool ref
    val saveAbsyn : bool ref
    val saveConvert : bool ref
    val saveCPSopt : bool ref
    val saveClosure : bool ref
    (*structure LambdaSplitting :
      sig
        datatype globalsetting = Default of int option | Off
        type localsetting = int option option
        val UseDefault : localsetting
        val Suggest : int option -> localsetting
        val set : globalsetting -> unit
        val get : unit -> int option
        val get' : localsetting -> int option
        val parse : string -> globalsetting option
        val show : globalsetting -> string
      end*)
    val tdp_instrument : bool ref
  end

(* Not in the Standard ML basis library. *)
structure Control :> CONTROL = _structControl

(* Not in the Standard ML basis library. *)
structure Environment :
  sig
    type staticEnv
    type dynenv
    type symenv
    type environment
    type symbol
    val emptyEnv : environment
    val staticPart : environment -> staticEnv
    val dynamicPart : environment -> dynenv
    val symbolicPart : environment -> symenv
    val mkenv : {dynamic:dynenv, static:staticEnv, symbolic:symenv}
                -> environment
    val layerEnv : environment * environment -> environment
    val concatEnv : environment * environment -> environment
    val layerStatic : staticEnv * staticEnv -> staticEnv
    val layerSymbolic : symenv * symenv -> symenv
    (* val filterEnv : environment * Symbol.symbol list -> environment *)
    val consolidateEnv : environment -> environment
    val consolidateStatic : staticEnv -> staticEnv
    val consolidateSymbolic : symenv -> symenv
    val trimEnv : environment -> environment
    (* val describe : staticEnv -> Symbol.symbol -> unit *)
    val primEnv : staticEnv
  end = _structEnvironment

(* Not in the Standard ML basis library. *)
structure EnvRef :
  sig
    type environment = Environment.environment
    type envref = {get:unit -> environment, set:environment -> unit}
    (* type envstate = {base:envref, loc:envref, props:PropList.holder} *)
    (* val state : unit -> envstate *)
    val loc : unit -> envref
    val base : unit -> envref
    (* val props : unit -> PropList.holder *)
    val pervasive : envref
    val combined : unit -> environment
    (* val locally : envstate * (unit -> 'a) -> 'a *)
    (* val listBoundSymbols : unit -> Symbol.symbol list *)
  end = _structEnvRef

(* Not in the Standard ML basis library. *)
structure Backend :
  sig
    (* structure Profile : <sig> *)
    (* structure Compile : <sig> *)
    structure Interact :
      sig
        exception Interrupt
        val interact : unit -> unit
        val useFile : string -> unit
        val useStream : TextIO.instream -> unit
        val evalStream : TextIO.instream * Environment.environment
                         -> Environment.environment
        val withErrorHandling : bool
                                -> {cont:exn -> unit, flush:unit -> unit,
                                    thunk:unit -> unit}
                                   -> unit
        (* val installCompManagers : <ty> *)
        (* val redump_heap_cont : <ty> *)
      end
    (* structure Machine : <sig> *)
    val architecture : string
    val abi_variant : string option
  end = _structBackend

signature COMMAND_LINE =
  sig
      val name : unit -> string
      val arguments : unit -> string list
  end

structure CommandLine :> COMMAND_LINE = _structCommandLine

structure Compiler :
  sig
    val architecture : string
    val version : {date:string, system:string, version_id:int list}
  end = _structCompiler

(* Not in the Standard ML basis library. *)
signature CM =
  sig
    val autoload : string -> bool
    val make : string -> bool
    val recomp : string -> bool
    val stabilize : bool -> string -> bool
    type 'a controller = {get:unit -> 'a, set:'a -> unit}
    structure Anchor :
      sig
        val anchor : string -> string option controller
        val reset : unit -> unit
      end
    structure Control :
      sig
        val keep_going : bool controller
        val verbose : bool controller
        val parse_caching : int controller
        val warn_obsolete : bool controller
        val debug : bool controller
        val conserve_memory : bool controller
        val generate_index : bool controller
      end
    structure Library :
      sig
        type lib
        val known : unit -> lib list
        val descr : lib -> string
        val osstring : lib -> string
        val dismiss : lib -> unit
        val unshare : lib -> unit
      end
    structure State :
      sig
        val synchronize : unit -> unit
        val reset : unit -> unit
        val pending : unit -> string list
        val showBindings : unit -> unit
      end
    structure Server :
      sig
        type server
        val start : {cmd:string * string list, name:string,
                     pathtrans:(string -> string) option, pref:int}
                    -> server option
        val stop : server -> unit
        val kill : server -> unit
        val name : server -> string
      end
    val sources : {arch:string, os:string} option
                  -> string
                     -> {class:string, derived:bool, file:string} list option
    val symval : string -> int option controller
    val load_plugin : string -> bool
    val cm_dir_arc : string
    val mk_standalone : bool option
                        -> {project:string, setup:string option,
                            target:string, wrapper:string}
                           -> string list option
    (*structure Graph :
      sig
        val graph : string
                    -> {graph:?.PortableGraph.graph, imports:Library.lib list,
                        nativesrc:string -> string} option
      end*)
    val redump_heap : string -> unit
  end

(* Not in the Standard ML basis library. *)
structure CM :> CM = _structCM

(* Not in the Standard ML basis library. *)
signature SIGNALS =
  sig
    eqtype signal
    datatype sig_action
      = DEFAULT
      | HANDLER of signal * int * unit (*?*)SMLofNJ.Cont.cont -> unit (*?*)SMLofNJ.Cont.cont (* ?.Cont.cont is SMLofNJ.Cont.cont *)
      | IGNORE
    val listSignals : unit -> signal list
    val toString : signal -> string
    val fromString : string -> signal option
    val setHandler : signal * sig_action -> sig_action
    val overrideHandler : signal * sig_action -> sig_action
    val inqHandler : signal -> sig_action
    datatype sigmask = MASK of signal list | MASKALL
    val maskSignals : sigmask -> unit
    val unmaskSignals : sigmask -> unit
    val masked : unit -> sigmask
    val pause : unit -> unit
    val sigINT : signal
    val sigALRM : signal
    val sigTERM : signal
    val sigGC : signal
  end

(* Not in the Standard ML basis library. *)
structure Signals :> SIGNALS = _structSignals

(**********************************************************************)
(* The SML/NJ Library *)
(**********************************************************************)

(* In the SML/NJ library. *)
signature ORD_KEY = sig
    type ord_key
    val compare : (ord_key * ord_key) -> order
end

(* In the SML/NJ library. *)
signature ORD_SET = sig
    structure Key : ORD_KEY
    type item = Key.ord_key
    type set
    val empty : set
    val singleton : item -> set
    val fromList : item list -> set
    val add : set * item -> set
    val add' : item * set -> set
    val addList : set * item list -> set
    val delete : set * item -> set
    val member : set * item -> bool
    val isEmpty : set -> bool
    val equal : set * set -> bool
    val compare : set * set -> order
    val isSubset : set * set -> bool
    val numItems : set -> int
    val listItems : set -> item list
    val union : set * set -> set
    val intersection : set * set -> set
    val difference : set * set -> set
    val map : (item -> item) -> set -> set
    val app : (item -> unit) -> set -> unit
    val foldl : (item * 'a -> 'a) -> 'a -> set -> 'a
    val foldr : (item * 'a -> 'a) -> 'a -> set -> 'a
    val partition : (item -> bool) -> set -> set * set
    val filter : (item -> bool) -> set -> set
    val exists : (item -> bool) -> set -> bool
    val find : (item -> bool) -> set -> item option
end

(* In the SML/NJ library. *)
structure IntListSet :> ORD_SET where type Key.ord_key = Int.int = _structIntListSet
(* In the SML/NJ library. *)
(*functor SplaySetFn (ORD_KEY) : ORD_SET = _structSplaySetFn*)
functor SplaySetFn (K : ORD_KEY) :> ORD_SET where type Key.ord_key = K.ord_key = _functorbodySplaySetFn;
(* In the SML/NJ library library. *)
functor BinarySetFn (OK : ORD_KEY) :> ORD_SET where type Key.ord_key = OK.ord_key = _structBinarySetFn

(* In the SML/NJ library. *)
signature ORD_MAP = sig
    structure Key : ORD_KEY
    type 'a map
    val empty : 'a map
    val isEmpty : 'a map -> bool
    val singleton : (Key.ord_key * 'a) -> 'a map
    val insert : ('a map * Key.ord_key * 'a) -> 'a map
    val insert' : ((Key.ord_key * 'a) * 'a map) -> 'a map
    val find : ('a map * Key.ord_key) -> 'a option
    val lookup : 'a map * Key.ord_key -> 'a
    val inDomain : ('a map * Key.ord_key) -> bool
    val remove : ('a map * Key.ord_key) -> ('a map * 'a)
    val first : 'a map -> 'a option
    val firsti : 'a map -> (Key.ord_key * 'a) option
    val numItems : 'a map -> int
    val listItems : 'a map -> 'a list
    val listItemsi : 'a map -> (Key.ord_key * 'a) list
    val listKeys : 'a map -> Key.ord_key list
    val collate : (('a * 'a) -> order) -> ('a map * 'a map) -> order
    val unionWith : (('a * 'a) -> 'a) -> ('a map * 'a map) -> 'a map
    val unionWithi : ((Key.ord_key * 'a * 'a) -> 'a) -> ('a map * 'a map) -> 'a map
    val intersectWith : (('a * 'b) -> 'c) -> ('a map * 'b map) -> 'c map
    val intersectWithi : ((Key.ord_key * 'a * 'b) -> 'c) -> ('a map * 'b map) -> 'c map
    val mergeWith : ('a option * 'b option -> 'c option)
	  -> ('a map * 'b map) -> 'c map
    val mergeWithi : (Key.ord_key * 'a option * 'b option -> 'c option)
	  -> ('a map * 'b map) -> 'c map
    val app : ('a -> unit) -> 'a map -> unit
    val appi : ((Key.ord_key * 'a) -> unit) -> 'a map -> unit
    val map : ('a -> 'b) -> 'a map -> 'b map
    val mapi : ((Key.ord_key * 'a) -> 'b) -> 'a map -> 'b map
    val foldl : (('a * 'b) -> 'b) -> 'b -> 'a map -> 'b
    val foldli : ((Key.ord_key * 'a * 'b) -> 'b) -> 'b -> 'a map -> 'b
    val foldr : (('a * 'b) -> 'b) -> 'b -> 'a map -> 'b
    val foldri : ((Key.ord_key * 'a * 'b) -> 'b) -> 'b -> 'a map -> 'b
    val filter : ('a -> bool) -> 'a map -> 'a map
    val filteri : ((Key.ord_key * 'a) -> bool) -> 'a map -> 'a map
    val mapPartial : ('a -> 'b option) -> 'a map -> 'b map
    val mapPartiali : ((Key.ord_key * 'a) -> 'b option) -> 'a map -> 'b map
end

(* In the SML/NJ library. *)
structure IntBinaryMap :> ORD_MAP where type Key.ord_key = int = _structIntBinaryMap
(* In the SML/NJ library. *)
structure IntListMap :> ORD_MAP where type Key.ord_key = int = _structIntListMap
(* In the SML/NJ library. *)
(*functor SplayMapFn (ORD_KEY) : ORD_MAP = _structSplayMapnFn*)
functor SplayMapFn (K : ORD_KEY) :> ORD_MAP where type Key.ord_key = K.ord_key = _functorbodySplayMapFn;
(* In the SML/NJ library. *)
functor BinaryMapFn (OK : ORD_KEY) :> ORD_MAP where type Key.ord_key = OK.ord_key = _structBinaryMapFn

signature HASH_KEY =
  sig
    type hash_key
    val hashVal : hash_key -> word
    val sameKey : (hash_key * hash_key) -> bool
  end;

signature MONO_HASH_TABLE =
  sig
    structure Key : HASH_KEY
    type 'a hash_table
    val mkTable : (int * exn) -> 'a hash_table
    val clear : 'a hash_table -> unit
    val insert : 'a hash_table -> (Key.hash_key * 'a) -> unit
    val inDomain : 'a hash_table -> Key.hash_key -> bool
    val lookup : 'a hash_table -> Key.hash_key -> 'a
    val find : 'a hash_table -> Key.hash_key -> 'a option
    val remove : 'a hash_table -> Key.hash_key -> 'a
    val numItems : 'a hash_table ->  int
    val listItems  : 'a hash_table -> 'a list
    val listItemsi : 'a hash_table -> (Key.hash_key * 'a) list
    val app  : ('a -> unit) -> 'a hash_table -> unit
    val appi : ((Key.hash_key * 'a) -> unit) -> 'a hash_table -> unit
    val map  : ('a -> 'b) -> 'a hash_table -> 'b hash_table
    val mapi : ((Key.hash_key * 'a) -> 'b) -> 'a hash_table -> 'b hash_table
    val fold  : (('a * 'b) -> 'b) -> 'b -> 'a hash_table -> 'b
    val foldi : ((Key.hash_key * 'a * 'b) -> 'b) -> 'b -> 'a hash_table -> 'b
    val modify  : ('a -> 'a) -> 'a hash_table -> unit
    val modifyi : ((Key.hash_key * 'a) -> 'a) -> 'a hash_table -> unit
    val filter  : ('a -> bool) -> 'a hash_table -> unit
    val filteri : ((Key.hash_key * 'a) -> bool) -> 'a hash_table -> unit
    val copy : 'a hash_table -> 'a hash_table
    val bucketSizes : 'a hash_table -> int list
  end;

functor HashTableFn (K : HASH_KEY) :> MONO_HASH_TABLE where type Key.hash_key = K.hash_key = _functorbodyHashTableFn;

structure HashString :> sig
    val hashString  : string -> word
    val hashSubstring : substring -> word
  end = _structHashString;

(* Not in the Standard ML basis library. *)
signature MATCH_TREE = sig
    datatype 'a match_tree = Match of 'a * 'a match_tree list
    val root : 'a match_tree -> 'a
    val nth : 'a match_tree * int -> 'a
    val map : ('a -> 'b) -> 'a match_tree -> 'b match_tree
    val app : ('a -> unit) -> 'a match_tree -> unit
    val find : ('a -> bool) -> 'a match_tree -> 'a option
    val num : 'a match_tree -> int
end

(* Not in the Standard ML basis library. *)
structure MatchTree :> MATCH_TREE = _structMatchTree

signature REGEXP_SYNTAX =
  sig
      exception CannotParse
      exception CannotCompile
      structure CharSet : ORD_SET where type Key.ord_key = char
      datatype syntax
        = Group of syntax
	| Alt of syntax list
	| Concat of syntax list
	| Interval of (syntax * int * int option)
	| Option of syntax
	| Star of syntax
	| Plus of syntax
	| MatchSet of CharSet.set
	| NonmatchSet of CharSet.set
	| Char of char
	| Begin
	| End
      val addRange : CharSet.set * char * char -> CharSet.set
      val allChars : CharSet.set
  end;

structure RegExpSyntax :> REGEXP_SYNTAX = _structRegExpSyntax;

signature REGEXP_PARSER =
  sig
    val scan : (char, 'a) StringCvt.reader
	          -> (RegExpSyntax.syntax, 'a) StringCvt.reader
  end;

structure AwkSyntax :> REGEXP_PARSER = _structAwkSyntax

signature REGEXP_ENGINE =
  sig
    type regexp
    type 'a match = {pos : 'a, len : int} MatchTree.match_tree
    val compile : RegExpSyntax.syntax -> regexp
    val find : regexp -> (char,'a) StringCvt.reader -> ('a match, 'a) StringCvt.reader
    val prefix : regexp ->(char,'a) StringCvt.reader -> ('a match, 'a) StringCvt.reader
    val match : (RegExpSyntax.syntax * ('a match -> 'b)) list
		  -> (char,'a) StringCvt.reader -> ('b, 'a) StringCvt.reader
  end

structure BackTrackEngine :> REGEXP_ENGINE = _structBackTrackEngine;

signature REGEXP =
  sig
    type regexp
    type 'a match = {pos : 'a, len : int} MatchTree.match_tree
    val compile : (char,'a) StringCvt.reader -> (regexp, 'a) StringCvt.reader
    val compileString : string -> regexp
    val find : regexp -> (char,'a) StringCvt.reader -> ('a match, 'a) StringCvt.reader
    val prefix : regexp -> (char,'a) StringCvt.reader -> ('a match, 'a) StringCvt.reader
    val match : (string * ('a match -> 'b)) list
	  -> (char,'a) StringCvt.reader -> ('b, 'a) StringCvt.reader
  end;

functor RegExpFn (
    structure P : REGEXP_PARSER
    structure E : REGEXP_ENGINE
  ) :> REGEXP where type regexp = E.regexp = _functorbodyRegExpFn;

signature FIFO =
  sig
    type 'a fifo
    exception Dequeue
    val empty : 'a fifo
    val isEmpty : 'a fifo -> bool
    val enqueue : ('a fifo * 'a) -> 'a fifo
    val dequeue : 'a fifo -> ('a fifo * 'a)
    val delete : ('a fifo * ('a -> bool)) -> 'a fifo
    val head : 'a fifo -> 'a
    val peek : 'a fifo -> 'a option
    val length : 'a fifo -> int
    val contents : 'a fifo -> 'a list
    val app : ('a -> unit) -> 'a fifo -> unit
    val map : ('a -> 'b) -> 'a fifo -> 'b fifo
    val foldl : (('a * 'b) -> 'b) -> 'b -> 'a fifo -> 'b
    val foldr : (('a * 'b) -> 'b) -> 'b -> 'a fifo -> 'b
end

structure Fifo :> FIFO = _structFifo

signature LIB_BASE = sig
    exception Unimplemented of string
    exception Impossible of string
    exception NotFound
    val failure : {module : string, func : string, msg : string} -> 'a
    val version : {date : string, system : string, version_id : int list}
    val banner : string
end

structure LibBase :> LIB_BASE = _structLibBase

signature UTF8 =
  sig
    type wchar = word
    val maxCodePoint : wchar	(* = 0wx0010FFFF *)
    exception Incomplete
    val getu : (char, 'strm) StringCvt.reader -> (wchar, 'strm) StringCvt.reader
    val explode : string -> wchar list
    val fold : ((wchar * 'a) -> 'a) -> 'a -> string -> 'a
    val size : string -> int
    val encode : wchar -> string
    val isAscii : wchar -> bool
    val toAscii : wchar -> char
    val fromAscii : char -> wchar
    val toString : wchar -> string
  end

structure UTF8 :> UTF8 = _structUTF8

signature LIST_SORT =
  sig
     val sort : ('a * 'a -> bool) -> 'a list -> 'a list  
     val uniqueSort : ('a * 'a -> order) -> 'a list -> 'a list
     val sorted : ('a * 'a -> bool) -> 'a list -> bool  
  end

structure ListMergeSort :> LIST_SORT = _structListMergeSort

signature GET_OPT = 
  sig
      datatype 'a arg_order
        = RequireOrder
        | Permute
        | ReturnInOrder of string -> 'a
      datatype 'a arg_descr
        = NoArg of unit -> 'a
        | ReqArg of (string -> 'a) * string
        | OptArg of (string option -> 'a) * string
      type 'a opt_descr = {
          short : string,
          long : string list,
          desc : 'a arg_descr,
          help : string
        }
      val usageInfo : {
	      header : string,
	      options : 'a opt_descr list
	    } -> string
      val getOpt : {
	      argOrder : 'a arg_order,
	      options : 'a opt_descr list,
	      errFn : string -> unit
	    } -> string list -> ('a list * string list)
  end

structure GetOpt :> GET_OPT = _structGetOpt

(**********************************************************************)
(* Defined by mlyacc *)
(**********************************************************************)

signature STREAM = sig
    type 'xa stream
    val streamify : (unit -> '_a) -> '_a stream
    val cons : '_a * '_a stream -> '_a stream
    val get : '_a stream -> '_a * '_a stream
end

signature LR_TABLE = sig
    datatype ('a,'b) pairlist = EMPTY | PAIR of 'a * 'b * ('a,'b) pairlist
    datatype state = STATE of int
    datatype term = T of int
    datatype nonterm = NT of int
    datatype action = SHIFT of state
		    | REDUCE of int
		    | ACCEPT
		    | ERROR
    type table
    val numStates : table -> int
    val numRules : table -> int
    val describeActions : table -> state ->
			  (term,action) pairlist * action
    val describeGoto : table -> state -> (nonterm,state) pairlist
    val action : table -> state * term -> action
    val goto : table -> state * nonterm -> state
    val initialState : table -> state
    exception Goto of state * nonterm
    val mkLrTable : {actions : ((term,action) pairlist * action) array,
		     gotos : (nonterm,state) pairlist array,
		     numStates : int, numRules : int,
		     initialState : state} -> table
end

signature TOKEN = sig
    structure LrTable : LR_TABLE
    datatype ('a,'b) token = TOKEN of LrTable.term * ('a * 'b * 'b)
    val sameToken : ('a,'b) token * ('a,'b) token -> bool
end

signature LR_PARSER = sig
    structure Stream: STREAM
    structure LrTable : LR_TABLE
    structure Token : TOKEN
    sharing LrTable = Token.LrTable
    exception ParseError
    val parse : {table : LrTable.table,
		 lexer : ('_b,'_c) Token.token Stream.stream,
		 arg: 'arg,
		 saction : int *
			   '_c *
			   (LrTable.state * ('_b * '_c * '_c)) list *
			   'arg ->
			   LrTable.nonterm *
			   ('_b * '_c * '_c) *
			   ((LrTable.state *('_b * '_c * '_c)) list),
		 void : '_b,
		 ec : { is_keyword : LrTable.term -> bool,
			noShift : LrTable.term -> bool,
			preferred_change : (LrTable.term list * LrTable.term list) list,
			errtermvalue : LrTable.term -> '_b,
			showTerminal : LrTable.term -> string,
			terms: LrTable.term list,
			error : string * '_c * '_c -> unit
		      },
		 lookahead : int
		} -> '_b *
		     (('_b,'_c) Token.token Stream.stream)
end

structure LrParser :> LR_PARSER = _structLrParser

signature LEXER = sig
    structure UserDeclarations : sig
	type ('a,'b) token
	type pos
	type svalue
    end
    val makeLexer : (int -> string) -> unit ->
		    (UserDeclarations.svalue,UserDeclarations.pos) UserDeclarations.token
end

signature ARG_LEXER = sig
    structure UserDeclarations : sig
	type ('a,'b) token
	type pos
	type svalue
	type arg
    end
    val makeLexer : (int -> string) -> UserDeclarations.arg -> unit ->
		    (UserDeclarations.svalue,UserDeclarations.pos) UserDeclarations.token
end

signature PARSER_DATA = sig
    type pos
    type svalue
    type arg
    type result
    structure LrTable : LR_TABLE
    structure Token : TOKEN
    sharing Token.LrTable = LrTable
    structure Actions : sig
	val actions : int * pos *
		      (LrTable.state * (svalue * pos * pos)) list * arg->
		      LrTable.nonterm * (svalue * pos * pos) *
		      ((LrTable.state *(svalue * pos * pos)) list)
	val void : svalue
	val extract : svalue -> result
    end
    structure EC : sig
	val is_keyword : LrTable.term -> bool
	val noShift : LrTable.term -> bool
 	val preferred_change : (LrTable.term list * LrTable.term list) list
	val errtermvalue : LrTable.term -> svalue
	val showTerminal : LrTable.term -> string
	val terms: LrTable.term list
    end
    val table : LrTable.table
end

signature PARSER = sig
    structure Token : TOKEN
    structure Stream : STREAM
    exception ParseError
    type pos
    type result
    type arg
    type svalue
    val makeLexer : (int -> string) ->
		    (svalue,pos) Token.token Stream.stream
    val parse : int * ((svalue,pos) Token.token Stream.stream) *
		(string * pos * pos -> unit) * arg ->
		result * (svalue,pos) Token.token Stream.stream
    val sameToken : (svalue,pos) Token.token * (svalue,pos) Token.token ->
		    bool
end

signature ARG_PARSER = sig
    structure Token : TOKEN
    structure Stream : STREAM
    exception ParseError
    type arg
    type lexarg
    type pos
    type result
    type svalue
    val makeLexer : (int -> string) -> lexarg ->
		    (svalue,pos) Token.token Stream.stream
    val parse : int * ((svalue,pos) Token.token Stream.stream) *
		(string * pos * pos -> unit) * arg ->
		result * (svalue,pos) Token.token Stream.stream
    val sameToken : (svalue,pos) Token.token * (svalue,pos) Token.token ->
		    bool
end

functor JoinWithArg(structure Lex        : ARG_LEXER
                    structure ParserData : PARSER_DATA
		    structure LrParser   : LR_PARSER
		    sharing ParserData.LrTable = LrParser.LrTable
		    sharing ParserData.Token   = LrParser.Token
		    sharing type Lex.UserDeclarations.svalue = ParserData.svalue
		    sharing type Lex.UserDeclarations.pos    = ParserData.pos
		    sharing type Lex.UserDeclarations.token  = ParserData.Token.token)
	: ARG_PARSER  = _structJoinWithArg

(**********************************************************************)
(* NLFFI *)
(**********************************************************************)

structure MLRep = struct
    structure Signed = Int32
    structure LongLongSigned = Int64
    structure Unsigned = Word32
    structure LongLongUnsigned = Word64
    structure Real = Real64
    (*
    structure SignedBitops =
        IntBitOps (structure I = Signed structure W = Unsigned)
    *)
end

signature C = sig
    exception OutOfMemory
    type ('t, 'c) obj
    eqtype ('t, 'c) obj'
    type ro and rw
    type 'o ptr
    type ('t, 'n) arr
    eqtype 'o ptr'
    eqtype void
    type voidptr = void ptr'
    type 'f fptr
    eqtype 'f fptr'
    type ('s, 'r) vfptr
    type ('s, 'r) vfptr'
    type 'tag su
    eqtype 'tag enum
    eqtype schar     and uchar
    eqtype sint      and uint
    eqtype sshort    and ushort
    eqtype slong     and ulong
    eqtype slonglong and ulonglong
    type float       and double
    structure Cvt : sig
	val c_schar     : MLRep.Signed.int            -> schar
	val c_uchar     : MLRep.Unsigned.word         -> uchar
	val c_sint      : MLRep.Signed.int            -> sint
	val c_uint      : MLRep.Unsigned.word         -> uint
	val c_sshort    : MLRep.Signed.int            -> sshort
	val c_ushort    : MLRep.Unsigned.word         -> ushort
	val c_slong     : MLRep.Signed.int            -> slong
	val c_ulong     : MLRep.Unsigned.word         -> ulong
	val c_slonglong : MLRep.LongLongSigned.int    -> slonglong
	val c_ulonglong : MLRep.LongLongUnsigned.word -> ulonglong
	val c_float     : MLRep.Real.real             -> float
	val c_double    : MLRep.Real.real             -> double
	val i2c_enum    : MLRep.Signed.int            -> 'e enum
	val ml_schar     : schar     -> MLRep.Signed.int
	val ml_uchar     : uchar     -> MLRep.Unsigned.word
	val ml_sint      : sint      -> MLRep.Signed.int
	val ml_uint      : uint      -> MLRep.Unsigned.word
	val ml_sshort    : sshort    -> MLRep.Signed.int
	val ml_ushort    : ushort    -> MLRep.Unsigned.word
	val ml_slong     : slong     -> MLRep.Signed.int
	val ml_ulong     : ulong     -> MLRep.Unsigned.word
	val ml_slonglong : slonglong -> MLRep.LongLongSigned.int
	val ml_ulonglong : ulonglong -> MLRep.LongLongUnsigned.word
	val ml_float     : float     -> MLRep.Real.real
	val ml_double    : double    -> MLRep.Real.real
	val c2i_enum     : 'e enum   -> MLRep.Signed.int
    end
    type 'c schar_obj = (schar, 'c) obj
    type 'c uchar_obj = (uchar, 'c) obj
    type 'c sint_obj = (sint, 'c) obj
    type 'c uint_obj = (uint, 'c) obj
    type 'c sshort_obj = (sshort, 'c) obj
    type 'c ushort_obj = (ushort, 'c) obj
    type 'c slong_obj = (slong, 'c) obj
    type 'c ulong_obj = (ulong, 'c) obj
    type 'c slonglong_obj = (slonglong, 'c) obj
    type 'c ulonglong_obj = (ulonglong, 'c) obj
    type 'c float_obj = (float, 'c) obj
    type 'c double_obj = (double, 'c) obj
    type 'c voidptr_obj = (voidptr, 'c) obj
    type ('e, 'c) enum_obj = ('e enum, 'c) obj
    type ('f, 'c) fptr_obj = ('f fptr, 'c) obj
    type ('s, 'r, 'c) vfptr_obj = (('s, 'r) vfptr, 'c) obj
    type ('s, 'c) su_obj = ('s su, 'c) obj
    type 'c schar_obj' = (schar, 'c) obj'
    type 'c uchar_obj' = (uchar, 'c) obj'
    type 'c sint_obj' = (sint, 'c) obj'
    type 'c uint_obj' = (uint, 'c) obj'
    type 'c sshort_obj' = (sshort, 'c) obj'
    type 'c ushort_obj' = (ushort, 'c) obj'
    type 'c slong_obj' = (slong, 'c) obj'
    type 'c ulong_obj' = (ulong, 'c) obj'
    type 'c slonglong_obj' = (slonglong, 'c) obj'
    type 'c ulonglong_obj' = (ulonglong, 'c) obj'
    type 'c float_obj' = (float, 'c) obj'
    type 'c double_obj' = (double, 'c) obj'
    type 'c voidptr_obj' = (voidptr, 'c) obj'
    type ('e, 'c) enum_obj' = ('e enum, 'c) obj'
    type ('f, 'c) fptr_obj' = ('f fptr, 'c) obj'
    type ('s, 'r, 'c) vfptr_obj' = (('s, 'r) vfptr, 'c) obj'
    type ('s, 'c) su_obj' = ('s su, 'c) obj'
    eqtype 'c sbf and 'c ubf
    structure W : sig
	type ('from, 'to) witness
	val trivial : ('t, 't) witness
	val pointer : ('from, 'to) witness ->
		      ('from ptr, 'to ptr) witness
	val object  : ('st, 'tt) witness ->
		      (('st, 'c) obj, ('tt, 'c) obj) witness
	val arr     : ('st, 'tt) witness ->
		      (('st, 'n) arr, ('tt, 'n) arr) witness
	val ro      : ('st, 'tt) witness ->
		      (('st, 'c) obj, ('tt, ro) obj) witness
	val rw      : ('st, 'tt) witness ->
		      (('st, 'sc) obj, ('tt, 'tc) obj) witness
    end
    val convert : (('st, 'sc) obj, ('tt, 'tc) obj) W.witness ->
		  ('st, 'sc) obj -> ('tt, 'tc) obj
    val convert' : (('st, 'sc) obj, ('tt, 'tc) obj) W.witness ->
		   ('st, 'sc) obj' -> ('tt, 'tc) obj'
    structure Dim : sig
	type ('a, 'z) dim0
	val toInt : ('a, 'z) dim0 -> int
	type zero
	type nonzero
	type 'a dim = ('a, nonzero) dim0
	type dec
	type 'a dg0 and 'a dg1 and 'a dg2 and 'a dg3 and 'a dg4
	type 'a dg5 and 'a dg6 and 'a dg7 and 'a dg8 and 'a dg9
	val dec' :            (dec, zero) dim0
	val dg0' : 'a dim        -> 'a dg0 dim
	val dg1' : ('a, 'z) dim0 -> 'a dg1 dim
	val dg2' : ('a, 'z) dim0 -> 'a dg2 dim
	val dg3' : ('a, 'z) dim0 -> 'a dg3 dim
	val dg4' : ('a, 'z) dim0 -> 'a dg4 dim
	val dg5' : ('a, 'z) dim0 -> 'a dg5 dim
	val dg6' : ('a, 'z) dim0 -> 'a dg6 dim
	val dg7' : ('a, 'z) dim0 -> 'a dg7 dim
	val dg8' : ('a, 'z) dim0 -> 'a dg8 dim
	val dg9' : ('a, 'z) dim0 -> 'a dg9 dim
	val dec :            ((dec, zero) dim0 -> 'b) -> 'b
	val dg0 : 'a dim        -> ('a dg0 dim -> 'b) -> 'b
	val dg1 : ('a, 'z) dim0 -> ('a dg1 dim -> 'b) -> 'b
	val dg2 : ('a, 'z) dim0 -> ('a dg2 dim -> 'b) -> 'b
	val dg3 : ('a, 'z) dim0 -> ('a dg3 dim -> 'b) -> 'b
	val dg4 : ('a, 'z) dim0 -> ('a dg4 dim -> 'b) -> 'b
	val dg5 : ('a, 'z) dim0 -> ('a dg5 dim -> 'b) -> 'b
	val dg6 : ('a, 'z) dim0 -> ('a dg6 dim -> 'b) -> 'b
	val dg7 : ('a, 'z) dim0 -> ('a dg7 dim -> 'b) -> 'b
	val dg8 : ('a, 'z) dim0 -> ('a dg8 dim -> 'b) -> 'b
	val dg9 : ('a, 'z) dim0 -> ('a dg9 dim -> 'b) -> 'b
	val dim : ('a, 'z) dim0 -> ('a, 'z) dim0
    end
    structure S : sig
	type 't size
	val toWord : 't size -> word
	val schar     : schar size
	val uchar     : uchar size
	val sint      : sint size
	val uint      : uint size
	val sshort    : sshort size
	val ushort    : ushort size
	val slong     : slong size
	val ulong     : ulong size
	val slonglong : slonglong size
	val ulonglong : ulonglong size
	val float     : float size
	val double    : double size
	val voidptr : voidptr size
	val ptr : 'o ptr size
	val fptr : 'f fptr size
	val vfptr : ('s, 'r) vfptr size
	val enum : 'tag enum size
    end
    structure T : sig
	type 't typ
	val typeof : ('t, 'c) obj -> 't typ
	val pointer : 't typ -> ('t, rw) obj ptr typ
	val target  : ('t, 'c) obj ptr typ -> 't typ
	val arr     : 't typ * 'n Dim.dim -> ('t, 'n) arr typ
	val elem    : ('t, 'n) arr typ -> 't typ
	val ro      : ('t, 'c) obj ptr typ -> ('t, ro) obj ptr typ
	val sizeof : 't typ -> 't S.size
	val dim : ('t, 'n) arr typ -> 'n Dim.dim
	val schar     : schar typ
	val uchar     : uchar typ
	val sint      : sint typ
	val uint      : uint typ
	val sshort    : sshort typ
	val ushort    : ushort typ
	val slong     : slong typ
	val ulong     : ulong typ
	val slonglong : slonglong typ
	val ulonglong : ulonglong typ
	val float     : float typ
	val double    : double typ
	val voidptr : voidptr typ
	val enum : 'tag enum typ
    end
    structure Light : sig
	val obj : ('t, 'c) obj -> ('t, 'c) obj'
	val ptr : 'o ptr -> 'o ptr'
	val fptr : 'f fptr -> 'f fptr'
	val vfptr : ('s, 'r) vfptr -> ('s, 'r) vfptr'
    end
    structure Heavy : sig
	val obj : 't T.typ -> ('t, 'c) obj' -> ('t, 'c) obj
	val ptr : 'o ptr T.typ -> 'o ptr' -> 'o ptr
	val fptr : 'f fptr T.typ  -> 'f fptr' -> 'f fptr
	val vfptr : ('s, 'r) vfptr T.typ -> ('s, 'r) vfptr' -> ('s, 'r) vfptr
    end
    val sizeof : ('t, 'c) obj -> 't S.size
    structure Get : sig
	val schar     : 'c schar_obj -> MLRep.Signed.int
	val uchar     : 'c uchar_obj -> MLRep.Unsigned.word
	val sint      : 'c sint_obj -> MLRep.Signed.int
	val uint      : 'c uint_obj -> MLRep.Unsigned.word
	val sshort    : 'c sshort_obj -> MLRep.Signed.int
	val ushort    : 'c ushort_obj -> MLRep.Unsigned.word
	val slong     : 'c slong_obj -> MLRep.Signed.int
	val ulong     : 'c ulong_obj -> MLRep.Unsigned.word
	val slonglong : 'c slonglong_obj -> MLRep.LongLongSigned.int
	val ulonglong : 'c ulonglong_obj -> MLRep.LongLongUnsigned.word
	val float     : 'c float_obj -> MLRep.Real.real
	val double    : 'c double_obj -> MLRep.Real.real
	val enum      : ('e, 'c) enum_obj -> MLRep.Signed.int
	val schar'     : 'c schar_obj' -> MLRep.Signed.int
	val uchar'     : 'c uchar_obj' -> MLRep.Unsigned.word
	val sint'      : 'c sint_obj' -> MLRep.Signed.int
	val uint'      : 'c uint_obj' -> MLRep.Unsigned.word
	val sshort'    : 'c sshort_obj' -> MLRep.Signed.int
	val ushort'    : 'c ushort_obj' -> MLRep.Unsigned.word
	val slong'     : 'c slong_obj' -> MLRep.Signed.int
	val ulong'     : 'c ulong_obj' -> MLRep.Unsigned.word
	val slonglong' : 'c slonglong_obj' -> MLRep.LongLongSigned.int
	val ulonglong' : 'c ulonglong_obj' -> MLRep.LongLongUnsigned.word
	val float'     : 'c float_obj' -> MLRep.Real.real
	val double'    : 'c double_obj' -> MLRep.Real.real
	val enum'      : ('e, 'c) enum_obj' -> MLRep.Signed.int
	val ptr : ('o ptr, 'c) obj -> 'o ptr
	val fptr : ('f, 'c) fptr_obj -> 'f fptr
	val vfptr : ('s, 'r, 'c) vfptr_obj -> ('s, 'r) vfptr
	val voidptr : 'c voidptr_obj -> voidptr
	val ptr' : ('o ptr, 'c) obj' -> 'o ptr'
	val fptr' : ('f, 'c) fptr_obj' -> 'f fptr'
	val vfptr' : ('s, 'r, 'c) vfptr_obj' -> ('s, 'r) vfptr'
	val voidptr' : 'c voidptr_obj' -> voidptr
	val sbf : 'c sbf -> MLRep.Signed.int
	val ubf : 'c ubf -> MLRep.Unsigned.word
    end
    structure Set : sig
	val schar     : rw schar_obj * MLRep.Signed.int -> unit
	val uchar     : rw uchar_obj * MLRep.Unsigned.word -> unit
	val sint      : rw sint_obj * MLRep.Signed.int -> unit
	val uint      : rw uint_obj * MLRep.Unsigned.word -> unit
	val sshort    : rw sshort_obj * MLRep.Signed.int -> unit
	val ushort    : rw ushort_obj * MLRep.Unsigned.word -> unit
	val slong     : rw slong_obj * MLRep.Signed.int -> unit
	val ulong     : rw ulong_obj * MLRep.Unsigned.word -> unit
	val slonglong : rw slonglong_obj * MLRep.LongLongSigned.int -> unit
	val ulonglong : rw ulonglong_obj * MLRep.LongLongUnsigned.word -> unit
	val float     : rw float_obj * MLRep.Real.real -> unit
	val double    : rw double_obj * MLRep.Real.real -> unit
	val enum      : ('e, rw) enum_obj * MLRep.Signed.int -> unit
	val schar'     : rw schar_obj' * MLRep.Signed.int -> unit
	val uchar'     : rw uchar_obj' * MLRep.Unsigned.word -> unit
	val sint'      : rw sint_obj' * MLRep.Signed.int -> unit
	val uint'      : rw uint_obj' * MLRep.Unsigned.word -> unit
	val sshort'    : rw sshort_obj' * MLRep.Signed.int -> unit
	val ushort'    : rw ushort_obj' * MLRep.Unsigned.word -> unit
	val slong'     : rw slong_obj' * MLRep.Signed.int -> unit
	val ulong'     : rw ulong_obj' * MLRep.Unsigned.word -> unit
	val slonglong' : rw slonglong_obj' * MLRep.LongLongSigned.int -> unit
	val ulonglong' : rw ulonglong_obj' * MLRep.LongLongUnsigned.word -> unit
	val float'     : rw float_obj' * MLRep.Real.real -> unit
	val double'    : rw double_obj' * MLRep.Real.real -> unit
	val enum'      : ('e, rw) enum_obj' * MLRep.Signed.int -> unit
	val ptr : ('o ptr, rw) obj * 'o ptr -> unit
	val fptr : ('f, rw) fptr_obj * 'f fptr -> unit
	val vfptr : ('s, 'r, rw) vfptr_obj * ('s, 'r) vfptr -> unit
	val voidptr : rw voidptr_obj * voidptr -> unit
	val ptr' : ('o ptr, rw) obj' * 'o ptr' -> unit
	val fptr' : ('f, rw) fptr_obj' * 'f fptr' -> unit
	val vfptr' : ('s, 'r, rw) vfptr_obj' * ('s, 'r) vfptr' -> unit
	val voidptr' : rw voidptr_obj' * voidptr -> unit
	val ptr_voidptr : ('o ptr, rw) obj * voidptr -> unit
	val ptr_voidptr' : ('o ptr, rw) obj' * voidptr -> unit
	val sbf : rw sbf * MLRep.Signed.int -> unit
	val ubf : rw ubf * MLRep.Unsigned.word -> unit
    end
    val copy : { from: ('t, 'c) obj, to: ('t, rw) obj } -> unit
    val copy' : 't S.size -> { from: ('t, 'c) obj', to: ('t, rw) obj' } -> unit
    val ro : ('t, 'c) obj  -> ('t, ro) obj
    val rw : ('t, 'sc) obj -> ('t, 'tc) obj
    val ro' : ('t, 'c) obj'  -> ('t, ro) obj'
    val rw' : ('t, 'sc) obj' -> ('t, 'tc) obj'
    structure Ptr : sig
	val |&| : ('t, 'c) obj -> ('t, 'c) obj ptr
	val |*| : ('t, 'c) obj ptr -> ('t, 'c) obj
	val |&! : ('t, 'c) obj' -> ('t, 'c) obj ptr'
	val |*! : ('t, 'c) obj ptr' -> ('t, 'c) obj'
	val compare : 'o ptr * 'o ptr -> order
	val compare' : 'o ptr' * 'o ptr' -> order
	val inject : 'o ptr -> voidptr
	val inject' : 'o ptr' -> voidptr
	val cast : 'o ptr T.typ -> voidptr -> 'o ptr
	val cast' : voidptr -> 'o ptr'
	val vNull : voidptr
	val null : 'o ptr T.typ -> 'o ptr
	val null' : 'o ptr'
	val fnull : 'f fptr T.typ -> 'f fptr
	val fnull' : 'f fptr'
	val vfnull : ('s, 'r) vfptr T.typ -> ('s, 'r) vfptr
	val vfnull' : ('s, 'r) vfptr'
	val vIsNull : voidptr -> bool
	val isNull : 'o ptr -> bool
	val isNull' : 'o ptr' -> bool
	val isFNull : 'f fptr -> bool
	val isFNull' : 'f fptr' -> bool
	val isVFNull : ('s, 'r) vfptr -> bool
	val isVFNull' : ('s, 'r) vfptr' -> bool
	val |+| : ('t, 'c) obj ptr * int -> ('t, 'c) obj ptr
	val |-| : ('t, 'c) obj ptr * ('t, 'c) obj ptr -> int
	val |+! : 't S.size -> ('t, 'c) obj ptr' * int -> ('t, 'c) obj ptr'
	val |-! : 't S.size -> ('t, 'c) obj ptr' * ('t, 'c) obj ptr' -> int
	val sub : ('t, 'c) obj ptr * int -> ('t, 'c) obj
	val sub' : 't S.size -> ('t, 'c) obj ptr' * int -> ('t, 'c) obj'
	val convert : (('st, 'sc) obj ptr, ('tt, 'tc) obj ptr) W.witness ->
		      ('st, 'sc) obj ptr -> ('tt, 'tc) obj ptr
	val convert' : (('st, 'sc) obj ptr, ('tt, 'tc) obj ptr) W.witness ->
		       ('st, 'sc) obj ptr' -> ('tt, 'tc) obj ptr'
	val ro : ('t, 'c) obj ptr    -> ('t, ro) obj ptr
	val rw : ('t, 'sc) obj ptr   -> ('t, 'tc) obj ptr
	val ro' : ('t, 'c) obj ptr'  -> ('t, ro) obj ptr'
	val rw' : ('t, 'sc) obj ptr' -> ('t, 'tc) obj ptr'
    end
    structure Arr : sig
	val sub : (('t, 'n) arr, 'c) obj * int -> ('t, 'c) obj
	val sub' : 't S.size * 'n Dim.dim ->
		   (('t, 'n) arr, 'c) obj' * int -> ('t, 'c) obj'
	val decay : (('t, 'n) arr, 'c) obj -> ('t, 'c) obj ptr
	val decay' : (('t, 'n) arr, 'c) obj' -> ('t, 'c) obj ptr'
	val reconstruct :
	    ('t, 'c) obj ptr * 'n Dim.dim -> (('t, 'n) arr, 'c) obj
	val reconstruct':
	    ('t, 'c) obj ptr' * 'n Dim.dim -> (('t, 'n) arr, 'c) obj'
	val dim : (('t, 'n) arr, 'c) obj -> 'n Dim.dim
    end
    structure VA : sig
        type 'a vsig
	type ('a, 'b) vargs = 'b vsig -> 'a vsig
	type ('e, 'a) varg = ('e -> 'a, 'a) vargs
	val None   : ('a, 'a) vargs
	val Const  : ('e, 'a) varg -> 'e -> ('a, 'a) vargs
	val ptr    : ('o ptr, 'a) varg
	val fptr   : ('f fptr, 'a) varg
	val vfptr  : (('s, 'r) vfptr, 'a) varg
	val enum   : ('e enum, 'a) varg
	val schar  : (schar, 'a) varg
	val uchar  : (uchar, 'a) varg
	val sint   : (sint, 'a) varg
	val uint   : (uint, 'a) varg
	val sshort : (sshort, 'a) varg
	val ushort : (ushort, 'a) varg
	val slong  : (slong, 'a) varg
	val ulong  : (ulong, 'a) varg
	val slonglong : (slonglong, 'a) varg
	val ulonglong : (ulonglong, 'a) varg
	val float  : (float, 'a) varg
	val double : (double, 'a) varg
	val ptr'   : ('o ptr', 'a) varg
	val fptr'  : ('f fptr', 'a) varg
	val vfptr' : (('s, 'r) vfptr', 'a) varg
    end
    val new : 't T.typ -> ('t, 'c) obj
    val new' : 't S.size -> ('t, 'c) obj'
    val discard : ('t, 'c) obj -> unit
    val discard' : ('t, 'c) obj' -> unit
    val alloc : 't T.typ -> word -> ('t, 'c) obj ptr
    val alloc' : 't S.size -> word -> ('t, 'c) obj ptr'
    val free : 'o ptr -> unit
    val free' : 'o ptr' -> unit
    val call : ('a -> 'b) fptr * 'a -> 'b
    val call' : ('a -> 'b) fptr T.typ -> ('a -> 'b) fptr' * 'a -> 'b
    val vcall : ('s, 'r) vfptr -> ('a, 'r) VA.vargs -> 's -> 'a
    val vcall' : ('a, 'r) vfptr T.typ ->
		 ('a, 'r) vfptr' -> ('a, 'r) VA.vargs -> 's -> 'a
    structure U : sig
	val fcast : 'a fptr' -> 'b fptr'
	val p2i : 'o ptr' -> ulong
	val i2p : ulong -> 'o ptr'
    end
end

structure C :> C = _structC

signature ZSTRING = sig
    type 'c zstring = (C.uchar, 'c) C.obj C.ptr
    type 'c zstring' = (C.uchar, 'c) C.obj C.ptr'
    val length : 'c zstring -> int
    val length' : 'c zstring' -> int
    val toML : 'c zstring -> string
    val toML' : 'c zstring' -> string
    val cpML : { from: string, to: C.rw zstring } -> unit
    val cpML' : { from: string, to: C.rw zstring' } -> unit
    val dupML : string -> 'c zstring
    val dupML' : string -> 'c zstring'
end

structure ZString :> ZSTRING = _structZString

(***********************************************************************)
(* overloading *)
(***********************************************************************)


(* OVERLOADING CLASSES *)
overload Int    (int,    Int.int, Int31.int, Int32.int, Position.int, IntInf.int, LargeInt.int)
overload Word   (word,   Word.word, Word8.word, Word31.word, Word32.word, LargeWord.word, SysWord.word)
overload Real   (real,   Real.real, LargeReal.real)
overload Char   (char,   Char.char)
overload String (string, String.string)

(*overload Int  (int)
overload Word (word)
overload Real (real)
overload Char (char)
overload String (string)*)

(* OVERLOADING OF OPERATORS *)
overload <=  : 'a * 'a -> bool with 'a in (in Int, in Word, in Real, in Char, in String)
overload >=  : 'a * 'a -> bool with 'a in (in Int, in Word, in Real, in Char, in String)
overload <   : 'a * 'a -> bool with 'a in (in Int, in Word, in Real, in Char, in String)
overload >   : 'a * 'a -> bool with 'a in (in Int, in Word, in Real, in Char, in String)
overload -   : 'a * 'a -> 'a   with 'a in (in Int, in Word, in Real)
overload +   : 'a * 'a -> 'a   with 'a in (in Int, in Word, in Real)
overload *   : 'a * 'a -> 'a   with 'a in (in Int, in Word, in Real)
overload ~   : 'a -> 'a        with 'a in (in Int, in Word, in Real)
overload div : 'a * 'a -> 'a   with 'a in (in Int, in Word)
overload mod : 'a * 'a -> 'a   with 'a in (in Int, in Word)
overload abs : 'a -> 'a        with 'a in (in Int, in Real)
overload /   : 'a * 'a -> 'a   with 'a in (in Real)
