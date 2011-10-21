(* basis-light.sml
 *
 * Copyright 2009 2010 Joe Wells, Vincent Rahli, Heriot-Watt University.
 * Permission is granted only for personal non-commercial use.
 * (We are currently working on arranging something nicer).
 *
 * Authors: Joe Wells, Vincent Rahli
 * Affiliation: Heriot-Watt University, MACS
 * Date: 03 March 2010
 * Description: Contains a part of the SML basis library.
 *              Lighter version of basis.sml
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
val <>   : int * int -> bool
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
    (*val fromString : String.string -> char option
    val toString : char -> String.string
    val fromCString : String.string -> char option
    val toCString : char -> String.string*)
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

structure Char : CHAR
    where type char = char
    (*where type string = String.string*) = _structChar

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

signature INTEGER =
  sig
    eqtype int
    (*val precision : Int31.int option*)
    val minInt : int option
    val maxInt : int option
    (*val toLarge : int -> IntInf.int*)
    (*val fromLarge : IntInf.int -> int*)
    (*val toInt : int -> Int31.int*)
    (*val fromInt : Int31.int -> int*)
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
    (*val sign : int -> Int31.int*)
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
(*structure Int32    :> INTEGER = _structInt32*)
(*structure Position :> INTEGER
(*!*)(*where type int = int*) = _structPosition*)

signature INT_INF =
  sig
    (* It includes INTEGER *)
    type int = int
    val precision : Int31.int option
    val minInt : int option
    val maxInt : int option
    val toLarge : int -> int
    val fromLarge : int -> int
    val toInt : int -> Int31.int
    val fromInt : Int31.int -> int
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
    val sign : int -> Int31.int
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
    val pow : int * Int31.int -> int
    val log2 : int -> Int31.int
    val orb : int * int -> int
    val xorb : int * int -> int
    val andb : int * int -> int
    val notb : int -> int
    val << : int * word -> int
    val ~>> : int * word -> int
  end

structure IntInf :> INT_INF
(*!*)(*where type int = int*) = _structIntInf

signature WORD =
  sig
    eqtype word
    val wordSize : int
    (*val toLargeWord : word -> Word32.word*)
    (*val toLargeWordX : word -> Word32.word*)
    (*val fromLargeWord : Word32.word -> word*)
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
    (*val << : word * Word31.word -> word*)
    (*val >> : word * Word31.word -> word*)
    (*val ~>> : word * Word31.word -> word*)
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
(*structure Word31    :> WORD
(*!*)(*where type word = word*)= _structWord31*)
(*structure Word32    :> WORD    = _structWord32*)
(*structure LargeWord :> WORD    = _structLargeWord*)

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

signature REAL =
  sig
    type real
    (*structure Math :
      sig
        type real
        val pi : real
        val e : real
        val sqrt : real -> real
        val sin : real -> real
        val cos : real -> real
        val tan : real -> real
        val asin : real -> real
        val acos : real -> real
        val atan : real -> real
        val atan2 : real * real -> real
        val exp : real -> real
        val pow : real * real -> real
        val ln : real -> real
        val log10 : real -> real
        val sinh : real -> real
        val cosh : real -> real
        val tanh : real -> real
      end*)
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
    (*sharing type Math.real = real*)
  end

structure Real      :> REAL
    where type real = real      = _structReal
(*structure LargeReal :> REAL
(*!*)(*where type real = real*) = _structLargeReal*)

(*signature MATH = sig
    type real
    val pi : real
    val e : real
    val sqrt : real -> real
    val sin : real -> real
    val cos : real -> real
    val tan : real -> real
    val asin : real -> real
    val acos : real -> real
    val atan : real -> real
    val atan2 : real * real -> real
    val exp : real -> real
    val pow : real * real -> real
    val ln    : real -> real
    val log10 : real -> real
    val sinh : real -> real
    val cosh : real -> real
    val tanh : real -> real
end

structure Math :> MATH where type real = real = _structMath*)

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

(* OVERLOADING CLASSES *)
overload Int  (int(*,  Int.int, Int31.int, Int32.int, Position.int*))
overload Word (word(*, Word.word, Word8.word, Word31.word, Word32.word, LargeWord.word*))
overload Real (real(*, Real.real, LargeReal.real*))
overload Char (char(*, Char.char*))
overload String (string(*, String.string*))

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
