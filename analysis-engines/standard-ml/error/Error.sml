(* Copyright 2009 2010 2011 2012 2013 Heriot-Watt University
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
 *  o Authors:     Vincent Rahli, John Pirie
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        24 May 2010
 *  o File name:   Error.sml
 *)

(** Contains the definition of a type error, This file defines the structure Error which has the signature ERROR. *)
structure Error :> ERROR = struct

(* abbreviate the names of structures *)
structure I  = Id
structure E  = Env
structure A  = AstSML
structure R  = Reg
structure L  = Label
structure S  = Slicing
structure O  = OrdSet
structure D  = Debug
structure TO = Tools
structure EK = ErrorKind
structure ER = ExtReg
structure EH = ErrorHandler
structure CD = LongId

(** An identifier for an error, set to be an integer. *)
type id      = int

(** A type used to represent an error. Has the following fields:
 * \arg id. The id used to represent the error.
 * \arg labs. The labels of the error.
 * \arg deps. The dependencies of the error.
 * \arg ek. The error kind.
 * \arg rf
 * \arg bb. Whether the built-in basis was used.
 * \arg rem.
 * \arg time. Time taken to find the error.
 * \arg sl. Slice of the error.
 * \arg regs. Regions for the error.
 * \arg min. Indicates whether the slice is minimal or not. *)
type error   = {id   : id,
		labs : L.labels,
		deps : CD.set,
		ek   : EK.kind,
		rf   : L.labels,
		bb   : bool,
		rem  : id list,
		time : LargeInt.int,
		sl   : A.progs,
		regs : ER.regs,
		min  : bool}

(** A quintuple of LargeInt.times. *)
type times = LargeInt.int * LargeInt.int * LargeInt.int * LargeInt.int * LargeInt.int

(** An exported error, a function. *)
type export = error list ->
	      A.packs    ->
	      bool       ->
	      times      ->
	      E.envContextSensitiveSyntaxPair   ->
	      L.label    ->
	      string     ->
	      string     ->
	      int        ->
	      int        ->
	      unit

(** Type of a function that exports some dummy fields. *)
type export' = error list ->
	       A.packs    ->
	       E.envContextSensitiveSyntaxPair   ->
	       int        ->
	       int        ->
	       unit


(*********************************************************************)
(* printing                                                          *)

(** Prints an identifier. *)
fun printId id = Int.toString id

(** Function used to print a list. *)
fun printlistgen xs f = "[" ^ #1 (List.foldr (fn (t, (s, c)) => (f t ^ c ^ s, ",")) ("", "") xs) ^ "]"

(** Prinst a list of identifiers using #printid. *)
fun printRemoves xs = printlistgen xs printId

(** Turns the character into a string. *)
fun transfun1 #"\""    = "\\\""
  | transfun1 #"\227"  = "\227" (* sequence ldots and rdots *)
  | transfun1 #"\128"  = "\128"
  | transfun1 #"\152"  = "\152"
  | transfun1 #"\153"  = "\153"
  | transfun1 #"\154"  = "\154"
  | transfun1 #"\155"  = "\155"
  | transfun1 #"\226"  = "\226" (* old ldots and rdots *)
  | transfun1 #"\167"  = "\167"
  | transfun1 #"\188"  = "\188"
  | transfun1 #"\189"  = "\189"
  | transfun1 #"\159"  = "\159" (* new ldots and rdots *)
  | transfun1 #"\168"  = "\168"
  | transfun1 #"\169"  = "\169"
  | transfun1 x        = Char.toString x

(** Translates a string using String.translate and #transfun1 operated on the string argument. *)
fun transfun2 st = String.translate transfun1 st

(** Turns a charecter into a string as understood by Lisp. *)
fun transLisp' #"'"  = "\\'"
  | transLisp' #"#"  = "\\#"
  | transLisp' #"\\" = "\\\\"
  | transLisp' #"?"  = "\\?"
  | transLisp' #"`"  = "\\`"
  | transLisp' x     = Char.toString x

(** Translates a string using String.translate and #transLisp' operated on the string argument. *)
fun transLisp st = String.translate transLisp' st

(** Prints error information in XML format. *)
fun printOneXmlErr {id, labs, deps, ek, rf, bb, rem, time, sl, regs, min} sep bslice =
    sep ^ "<id>"           ^ Int.toString id                   ^ "</id>\n"           ^
    sep ^ "<labels nb=\""  ^ Int.toString (L.length labs)      ^ "\">" ^ L.toString labs ^ "</labels>\n" ^
    sep ^ "<statusLabels>" ^ L.toString rf                     ^ "</statusLabels>\n" ^
    sep ^ "<builtinBasis>" ^ Bool.toString bb                  ^ "</builtinBasis>\n" ^
    sep ^ "<assumptions>"  ^ CD.toString deps                  ^ "</assumptions>\n"  ^
    sep ^ "<kind>"         ^ #2 (EK.printErrKind ek I.emAssoc) ^ "</kind>\n"         ^
    sep ^ "<remove>"       ^ printRemoves rem                  ^ "</remove>\n"       ^
    sep ^ "<time>"         ^ LargeInt.toString time            ^ "</time>\n"         ^
    sep ^ "<slice>"        ^ S.printOneSlice sl bslice sep     ^ "</slice>\n"        ^
    sep ^ "<regions>"      ^ ER.printOneRegs regs              ^ "</regions>\n"      ^
    sep ^ "<minimal>"      ^ Bool.toString min                 ^ "</minimal>\n"
    (*^ sep ^ "<slast>"       ^ S.toString sl                   ^ "</slast>\n"*)

(** Prints an error in XML format. *)
fun printOneXmlErrTuple {id, labs, deps, ek, rf, bb, rem, time, sl, regs, min} sep bslice basisoverloading =
    (Int.toString id,
     Int.toString (L.length labs) ^ "\">" ^ L.toString labs,
     L.toString rf,
     Bool.toString bb,
     CD.toString deps,
     #2 (EK.printErrKind ek I.emAssoc),
     printRemoves rem,
     LargeInt.toString time,
     S.printOneSlice sl bslice sep,
     (if basisoverloading = 0
      then ER.printOneRegs (List.filter (fn (name, regs) => (if (String.isSubstring "basis.sml" name) then false else true)) regs)
      else ER.printOneRegs regs),
     Bool.toString min)

(** Used for our database, that's why we don't print anything for this new "rem" field. *)
fun printOneSmlErr {id, labs, deps, ek, rf, bb, rem, time, sl, regs, min} bslice basisoverloading =
    let val ll = "labels      = " ^ "(" ^ Int.toString (L.length labs) ^ "," ^ L.toString labs ^ ")"
	val cd = "assumptions = " ^ CD.toStringOut deps
	val ek = "kind        = " ^ EK.printSmlErrKind ek
	val tm = "time        = " ^ LargeInt.toString time
	val id = "identifier  = " ^ Int.toString id
	val sl = "slice       = " ^ "\"" ^ transfun2 (S.printSlice sl bslice) ^ "\""
	val re = "regions     = " ^ "[" ^ ER.printSmlExtRegs (List.filter (fn (name, regs) => (if (String.isSubstring "basis.sml" name) then false else true)) regs) ^ "]"
    in (id, ll, cd, ek, tm, sl,
       (if basisoverloading = 0
       then "regions     = " ^ "[" ^ ER.printSmlExtRegs (List.filter (fn (name, regs) => (if (String.isSubstring "basis.sml" name) then false else true)) regs) ^ "]"
       else "regions     = " ^ "[" ^ ER.printSmlExtRegs regs ^ "]"))
    end

(** Prints a JSON error to string. *)
fun printOneJsonErr {id, labs, deps, ek, rf, bb, rem, time, sl, regs, min} bslice basisoverloading =
    let
	(** The labels used in the erorr. *)
	val ll = "\"labels\"      : " ^ "{\"count\": " ^ Int.toString (L.length labs) ^ ", " ^ "\"labelNumbers\": " ^ L.toString labs ^ "}"
	(** The assumptions used in the error. *)
	val cd = "\"assumptions\" : " ^ CD.toJsonStringOut deps
	(** The kind of the error. *)
	val ek = "\"kind\"        : " ^ EK.printJsonErrKind ek
	(** The time taken to locate the error. *)
	val tm = "\"time\"        : " ^ LargeInt.toString time
	(** The identifier of the error. *)
	val id = "\"identifier\"  : " ^ Int.toString id
	(** The slice of the error. *)
	val sl = "\"slice\"       : " ^ "\"" ^ transfun2 (S.printSlice sl bslice) ^ "\""
    in (id, ll, cd, ek, tm, sl,
       (if basisoverloading = 0
       then "\"regions\"     : " ^ "[" ^ ER.printJsonExtRegs (List.filter (fn (name, regs) => (if (String.isSubstring "basis.sml" name andalso String.isSubstring "Overload" ek andalso basisoverloading = 0) then false else true)) regs ) ^ "]"
       else "\"regions\"     : " ^ "[" ^ ER.printJsonExtRegs regs ^ "]"))
    end


(** Takes the argument xs, applies the argument p to ever element of xs and concatenates the result with the 'sep' argument.
 * \param xs A list to be turned into a string.
 * \param sep An element seperator.
 * \param p A function applied to each element of xs. *)
fun toListSep xs sep p =
    #1 (foldr (fn (x, (y, z)) => (p ^ x ^ p ^ z ^ y, sep)) ("", "") xs)

(** Returns a give slice as a string *)
fun SlToString sl = S.toString sl

(** Prints all ids using #printId over a given list, rems. *)
fun removesToList rems = map (fn x => printId x) rems

(** Prints one error in lisp format. *)
fun printOneLispErr {id, labs, deps, ek, rf, bb, rem, time, sl, regs, min} ascid bslice basisoverloading =
    let val id = "(id . " ^ Int.toString id ^ ")"
	val cd = "(assumptions . (" ^ transLisp (toListSep (CD.toStringList deps ascid) " " "") ^ "))"
	val (idk, errk) = EK.printErrKind ek ascid
	val ek = "(kind . (" ^ idk ^ " \"" ^ transfun2 errk ^ "\"))"
	val rm = "(remove . (" ^ toListSep (removesToList rem) " " "" ^ "))"
	val ss = "(slice . \"" ^ transfun2 (S.printSlice sl bslice) ^ "\")"
	val at = "(ast . \"" ^ ""(*SlToString sl*) ^ "\")"
	val re = "(regions . (" ^ (String.translate (fn #"\\" => "\\\\" | x=>str x) (ER.printLispExtRegs (List.filter (fn (name, regs) => (if (String.isSubstring "basis.sml" name andalso String.isSubstring "OV" ek andalso basisoverloading = 0) then false else true)) regs))) ^ "))"
	val mn = "(minimal . " ^ Bool.toString min ^ ")"
    in (id, cd, ek, rm, ss, at,
	(if basisoverloading = 0
	 then "(regions . (" ^ (String.translate (fn #"\\" => "\\\\" | x=>str x) (ER.printLispExtRegs (List.filter (fn (name, regs) => (if (String.isSubstring "basis.sml" name andalso String.isSubstring "OV" ek andalso basisoverloading = 0) then false else true)) regs))) ^ "))"
	 else "(regions . (" ^ (String.translate (fn #"\\" => "\\\\" | x=>str x) (ER.printLispExtRegs regs)) ^ "))")
      , mn)
    end

(** Prints one error in perl format. *)
fun printOnePerlErr {id, labs, deps, ek, rf, bb, rem, time, sl, regs, min} ascid bslice basisoverloading =
    let val id = "id => " ^ Int.toString id
	val cd = "assumptions => [" ^ toListSep (CD.toStringList deps ascid) ", " "\"" ^ "]"
	val (idk, errk) = EK.printErrKind ek ascid
	val errk = String.translate (fn #"\"" => "\\\"" | x=>(Char.toString x)) errk
	val ek = "kind        => {id  => \"" ^ idk ^ "\", msg => \"" ^ errk ^ "\"}"
	val rm = "remove      => " ^ printRemoves rem
	val sl = "slice       => \"" ^ transfun2 (S.printSlice sl bslice) ^ "\""
	val mn = "minimal     => " ^ Bool.toString min
    in (id, cd, ek, rm, sl,
	(if basisoverloading = 0
	 then "regions     => [" ^ ER.printPerlExtRegs (List.filter (fn (name, regs) => (if (String.isSubstring "basis.sml" name andalso String.isSubstring "OV" ek andalso basisoverloading = 0) then false else true)) regs) ^ "]"
	 else "regions     => [" ^ ER.printPerlExtRegs regs ^ "]")
	, mn)
    end

(** Quick hack for the next release to reduce overloading information from the basis. *)
fun removeBasisSlice sl =
let
    (** Helper function for #removeBasisSlice. *)
    fun stripBasisSlice sl =
	let
	    (** We start at two because when we look for the start of the basis slice, we take
             * two brackets into account *)
	    val bracketBalancer = ref 2;

	    (** Locates the start of the slice for the basis *)
	    fun findBasisSlice [] = []
	      | findBasisSlice (h::t) =
		if h = #"\168" (* opening slice bracket *)
		then (bracketBalancer := !bracketBalancer + 1; findBasisSlice t)
		else
		    if h = #"\169" (* closing slice bracket *)
		    then (bracketBalancer := !bracketBalancer - 1;
			  if !bracketBalancer = 0
			  then t
			  else findBasisSlice t)
		    else findBasisSlice t
	in
	    String.implode (findBasisSlice sl)
	end

    (** Finds the start of the basis slice. This should ideally be done in the AST. *)
    fun findStartBasisSlice sl =
	if (String.extract (sl, 0, SOME(11))) = "..structure"
	   andalso
	   (String.extract (sl, 15, SOME(7))) = "..Basis"
	then stripBasisSlice (String.explode (String.extract (sl, 20, NONE)))
	else findStartBasisSlice (String.extract (sl, 1, NONE))
in
    findStartBasisSlice sl
end

(** Prints an error to stdout in bash format. *)
fun printOneBashErr {id, labs, deps, ek, rf, bb, rem, time, sl, regs, min} ascid bslice basisoverloading =
    let
	val cdStringList = (CD.toStringList deps ascid)
	val cd = toListSep cdStringList ", " "\""
	val (idk, errk) = EK.printErrKind ek ascid
	val ek = "kind        => {id  => \"" ^ idk ^ "\", msg => \"" ^ errk ^ "\"}"
	val errk = String.translate (fn #"\"" => "\\\"" | x=>(Char.toString x)) errk
	(** Prints the string argument followed by #Debug.textReset. *)
	fun printReset str = print (str^(!D.textReset))
    in
	(printReset( (#yellow (!D.underlineColors)) ^ errk ^ "\n\n");
	 printReset( (#yellow (!D.underlineColors)) ^ "Slice in context:\n\n");
	 ER.printBashExtRegs (List.filter (fn (name, regs) => (if (String.isSubstring "basis.sml" name) andalso String.isSubstring "OV" ek andalso basisoverloading = 0 then false else true)) regs);
	 printReset( (#yellow (!D.underlineColors)) ^ "\nSlice on its own:\n");
	 printReset( (#white (!D.boldColors)) ^ (if String.isSubstring "OV" ek andalso basisoverloading = 0
						 then removeBasisSlice (S.printSlice sl bslice)
						 else(S.printSlice sl bslice))  ^ "\n");

	 if List.length cdStringList = 0
	 then ()
	 else if List.length cdStringList = 1
	 then printReset( (#yellow (!D.underlineColors)) ^ "\nContext Dependency:" ^ (!D.textReset) ^ " " ^ cd ^ " is neither a type nor an exception constructor.\n")
	 else printReset( (#yellow (!D.underlineColors)) ^ "\nContext Dependencies:" ^ (!D.textReset) ^ " " ^ cd ^ " are neither datatype nor exception constructors.\n"))
    end


val idError = ref 0

(** Sets the #idError to the value in the argument. *)
fun setError   n = idError := n
(** Gets the #idError value. *)
fun getError   _ = !idError
(** Increments and then gets a fresh error. *)
fun freshError _ = let val x = !idError in (idError := !idError + 1; x) end
(** Resets the error counter. *)
fun resetError _ = setError 0

(** A dummy identifier, set to 0. *)
val dummyId = 0

(** The initial value for the "rem" field of an error *)
val initRem   = []
(** The initial value for the "rf" field of an error *)
val initRf    = L.empty
(** The dummy time value *)
val initTime  = Int.toLarge ~1
(** The dummy slice *)
val initSlice = A.Progs []
(** The dummy Regsins *)
val initRegs  = []
(** The dummy builtinBasis *)
val initBB    = true
(** The dummy minimality marker *)
val initMin   = false

(** Changes and id of type id to an integer. *)
fun idToInt id = id

(** Gets the identifier of an error. *)
fun getI (x : error) = #id   x
(** Gets the labels of an error. *)
fun getL (x : error) = #labs x
(** Gets the dependencies of an error. *)
fun getD (x : error) = #deps x
(** Gets the error kind of an error. *)
fun getK (x : error) = #ek   x
(** Gets the bound functions of an error. *)
fun getF (x : error) = #rf   x
(** Gets the builtin basis specification for an error. *)
fun getB (x : error) = #bb   x
(** Gets the errors to be erased from an error. (?) *)
fun getE (x : error) = #rem  x
(** Get time taken to generate the error. *)
fun getT (x : error) = #time x
(** Gets the slice of an error. *)
fun getS (x : error) = #sl   x
(** Gets the regions of the error. *)
fun getR (x : error) = #regs x
(** Gets the minimisation value of an error. *)
fun getM (x : error) = #min  x

(** Constructs an error, given the record fields as parameters. *)
fun consError id labs deps ek rf bb rem time sl regs min =
    {id   = id,
     labs = labs,
     deps = deps,
     ek   = ek,
     rf   = rf,
     bb   = bb,
     rem  = rem,
     time = time,
     sl   = sl,
     regs = regs,
     min  = min}

(** Construct an error given just an identifier, labels, dependencies and an error kind. *)
fun consErrorNoRB id labs deps ek        =
    consError id labs deps ek initRf initBB initRem initTime initSlice initRegs initMin
(** Construct an error given just an identifier, labels, dependencies, an error kind and an 'rf' (?). *)
fun consErrorNoR  id labs deps ek rf     =
    consError id labs deps ek rf     initBB initRem initTime initSlice initRegs initMin
(** Same as #consErrorNoR? *)
fun consPreError  id labs deps ek rf =
    consError id labs deps ek rf     initBB initRem initTime initSlice initRegs initMin

(** Delete the dummy label. *)
fun stripDummy {id, labs, deps, ek, rf, bb, rem, time, sl, regs, min} =
    consError id (L.delete L.dummyLab labs) deps ek rf bb rem time sl regs min

(** Delete the dummy and builtin labels. *)
fun stripDummys {id, labs, deps, ek, rf, bb, rem, time, sl, regs, min} =
    consError id (L.delete L.builtinLab (L.delete L.dummyLab labs)) deps ek rf bb rem time sl regs min

(** Sets the identifier of an error. *)
fun setI {id = _, labs, deps, ek, rf, bb, rem, time, sl, regs, min} id   = consError id labs deps ek rf bb rem time sl regs min
(** Sets the labels of an error. *)
fun setL {id, labs = _, deps, ek, rf, bb, rem, time, sl, regs, min} labs = consError id labs deps ek rf bb rem time sl regs min
(** Sets the dependencies of an error. *)
fun setD {id, labs, deps = _, ek, rf, bb, rem, time, sl, regs, min} deps = consError id labs deps ek rf bb rem time sl regs min
(** Sets the error kind of an error. *)
fun setK {id, labs, deps, ek = _, rf, bb, rem, time, sl, regs, min} ek   = consError id labs deps ek rf bb rem time sl regs min
(** Sets the bound functions of an error. *)
fun setF {id, labs, deps, ek, rf = _, bb, rem, time, sl, regs, min} rf   = consError id labs deps ek rf bb rem time sl regs min
(** Sets the builtin basis value of an error. *)
fun setB {id, labs, deps, ek, rf, bb = _, rem, time, sl, regs, min} bb   = consError id labs deps ek rf bb rem time sl regs min
(** Sets the errors to be erased  from an error (?). *)
fun setE {id, labs, deps, ek, rf, bb, rem = _, time, sl, regs, min} rem  = consError id labs deps ek rf bb rem time sl regs min
(** Sets the time taken to locate an error. *)
fun setT {id, labs, deps, ek, rf, bb, rem, time = _, sl, regs, min} time = consError id labs deps ek rf bb rem time sl regs min
(** Sets the slice of an error. *)
fun setS {id, labs, deps, ek, rf, bb, rem, time, sl = _, regs, min} sl   = consError id labs deps ek rf bb rem time sl regs min
(** Sets the regions of an error. *)
fun setR {id, labs, deps, ek, rf, bb, rem, time, sl, regs = _, min} regs = consError id labs deps ek rf bb rem time sl regs min
(** Sets the minimisation field of an error. *)
fun setM {id, labs, deps, ek, rf, bb, rem, time, sl, regs, min = _} min  = consError id labs deps ek rf bb rem time sl regs min

(** Adds dependencies to an error. *)
fun labelError {id, labs, deps, ek, rf, bb, rem, time, sl, regs, min} labs' stts' deps' =
    consError id (L.union labs labs') (CD.union deps deps') ek (L.union rf stts') bb rem time sl regs min

(** Separates semantic errors from syntactic ones. *)
fun sepsemsyn [] = ([], [])
  | sepsemsyn (x :: xs) =
    let val (sem, syn) = sepsemsyn xs
    in if EK.issem (getK x)
       then (x :: sem, syn)
       else (sem, x :: syn)
    end

(** Ordering of errors depending on their sizes. *)
fun orderErrors errs =
    let
	(** Helper function for #orderErrors. *)
	fun add x [] = [x]
	  | add x (y :: ys) = if L.length (getL x) <= L.length (getL y)
			      then x :: y :: ys
			      else y :: (add x ys)
	(** Calls #add on each of the elemnts of the list 'xs' while folding right over it. *)
	fun order xs = foldr (fn (x, y) => add x y) [] xs
    in (fn (sem, syn) => (order syn) @ (order sem)) (sepsemsyn errs)
    end

(** Gets the identifiers in a list of errors, and returns them all as a list. *)
fun getIdsErrors [] = []
  | getIdsErrors (x :: xs) = (getI x) :: (getIdsErrors xs)

(** Returns the errors in errs2 that are not in errs1 using the id's. *)
fun getNewErrors errs1 errs2 =
    let val ids = O.ord (getIdsErrors errs1)
    in List.filter (fn x => not (O.isin (getI x) ids)) errs2
    end

(** Given an identifier and a list of errors, finds the error with that identifier number. *)
fun getErrorList []        _          = NONE
  | getErrorList (x :: xs) (id : int) = if getI x = id then SOME x else getErrorList xs id

(** Returns the identifiers of all the errors that can be removed. *)
fun getToRemove errs = foldr (fn (x, y) => (getE x) @ y) initRem errs

(** Filters the errors: keep only the one that are not unvalidated by others. *)
fun getMergedErrors errs =
    let val rems = getToRemove errs
    in List.filter (fn x => not (TO.isin (getI x) rems)) errs
    end

(** Filters the errors: keep all the minimal ones (that don't specify any removal). *)
fun getMinErrors errs =
    List.filter (fn x => null (getE x)) errs

fun sortlab ll1 ll2 ll3 ll4 ll1' ll2' ll3' ll4' =
    let val lla  = TO.union ll1 ll1'
	val llb  = TO.union ll2 ll2'
	val llc' = TO.union ll3 ll3'
	val lld' = TO.union ll4 ll4'
	val (lle, llf) =
	    List.partition
		(fn (_, x) => not (Tools.isin x (#2 (ListPair.unzip (Tools.union llb lld')))))
		lla
	val (llg, llh) =
	    List.partition
		(fn (_, x) => not (Tools.isin x (#2 (ListPair.unzip (Tools.union lla llc')))))
		llb
	val llc = Tools.union llc' llf
	val lld = Tools.union lld' llh
    in (lle, llg, llc, lld)
    end

(** Combines two errors to make a new error, returning the result. *)
fun fusionerr {id = id1, labs = labs1, deps = deps1,
	       ek = EK.LabTyClash (ll1,  ll2,  ll3,  ll4),
	       rf = rf1, bb = bb1, rem = rem1, time = time1,
	       sl = sl1, regs = regs1, min = true}
	      {id = id2, labs = labs2, deps = deps2,
	       ek = EK.LabTyClash (ll1', ll2', ll3', ll4'),
	       rf = rf2, bb = bb2, rem = rem2, time = time2,
	       sl = sl2, regs = regs2, min = true} =
    {id   = freshError (),
     labs = L.union labs1 labs2,
     deps = CD.union deps1 deps2,
     ek   = EK.LabTyClash (sortlab ll1 ll2 ll3 ll4 ll1' ll2' ll3' ll4'),
     rf   = L.union rf1 rf2,
     bb   = bb1 orelse bb2,
     rem  = id1 :: id2 :: rem1 @ rem2,
     time = LargeInt.max (time1, time2),
     sl   = initSlice,
     regs = initRegs,
     min  = true}
  | fusionerr _ _ = raise EH.DeadBranch "DeadBranch69"

(** Multi minimal record errors at once. *)
(* returns: (keep, remove)             *)
fun recordSpeTreat [] = []
  | recordSpeTreat ((x as {id, labs, deps, ek = EK.LabTyClash (ll1, ll2, ll3, ll4), rf, bb, rem, time, sl, regs, min}) :: xs) =
    let val errs = recordSpeTreat xs
	val err1 =
	    List.filter
		(fn {id, labs, deps, ek = EK.LabTyClash (ll1', ll2', ll3', ll4'), rf, bb, rem, time, sl, regs, min} =>
		    not (Tools.disjoint (ll1 @ ll3) (ll1' @ ll3'))
		    andalso
		    not (Tools.disjoint (ll2 @ ll4) (ll2' @ ll4'))
		  | _ => false) errs
	val err1' = getMergedErrors err1
    in case err1' of
	   [] => x :: errs
	 | _  => (foldr (fn (err, x) => fusionerr err x) x err1') :: x :: errs
    end
  | recordSpeTreat (x :: xs) = x :: recordSpeTreat xs

fun consWeight' new [] = [new]
  | consWeight' new (fnd :: found) =
    let val regs1 = getR new
	val regs2 = getR fnd
	val (regs1', regs2') = ER.consWeightRegs regs1 regs2
	val new' = setR new regs1'
	val fnd' = setR fnd regs2'
    in fnd' :: consWeight' new' found
    end

fun consWeight [] found = found
  | consWeight (new :: news) found =
    let val found' = consWeight' new found
    in consWeight news found'
    end

(** Checks if err1 is smaller or equal to err2. *)
fun alreadyoneone err1 err2 =
    case getK err1 of
	EK.FreeIdent => false
      | EK.Warning _ => false
      | EK.Parsing _ => false
      | _ => L.subseteq (getL err1) (getL err2)
	     andalso
	     CD.subseteq (getD err1) (getD err2)

(** Checks if there exists an error smaller or equal to err in the list. *)
fun alreadyone [] _ = false
  | alreadyone (x :: xs) err =
    alreadyoneone x err orelse alreadyone xs err

(** Checks if err is smaller or equal to one of the errors in the list of errors. *)
fun alreadyone' _ [] = false
  | alreadyone' err (x :: xs) =
    alreadyoneone err x orelse alreadyone' err xs

(** Checks if the first error is strictly smaller than the second one. *)
fun orderdone {id = _, labs = labs1, deps = deps1, ek = ek1, rf = _,
	       bb = _, rem = _, time = _, sl = _, regs = _, min = _}
	      {id = _, labs = labs2, deps = deps2, ek = ek2, rf = _,
	       bb = _, rem = _, time = _, sl = _, regs = _, min = _} =
    case ek1 of
	EK.FreeIdent => false
      | EK.Warning _ => false
      | EK.Parsing _ => false
      | _  => (L.subset labs1 labs2 andalso CD.subseteq deps1 deps2)
	      orelse
	      (L.subseteq labs1 labs2 andalso CD.subset deps1 deps2)

(** All the minimal errors on which depend an errors (merged or not). *)
fun removedErrors errs err =
    if null (getE err)
    then [err]
    else foldr (fn (id, errs') =>
		   case getErrorList errs id of
		       NONE => errs'
		     | SOME err' => (removedErrors errs err') @ errs')
	       []
	       (getE err)

fun mindone1 errs err =
    let
	(** Helper function to mindone1. *)
	fun g [] false = let val newerr = setI err (freshError ())
			 in (SOME newerr, [newerr]) end
	  | g [] true  = (NONE, [])
	  | g (x :: xs) b =
	    if orderdone x err
	    then (fn (u, v) => (u, x :: v)) (g xs true)
	    else if alreadyone' err (removedErrors errs x)
	    then g xs b
	    else (fn (u, v) => (u, x :: v)) (g xs b)
    in g errs false
    end

fun mindone2 errs err = (SOME err, errs @ [err])
fun mindone3 errs err = (SOME err, err :: errs)

(** Does not do mindone' checking because errors are supposed to be minimal anyway *)
fun mindone  errs err = mindone2 errs err
(** Checks if there is already a minimmal error or if the new error invalidates an already found error. *)
fun mindone' errs err = mindone1 errs err
(** Sets the slice associated with an error given an abstract snytax tree, which #Slice.slice is called on. *)
fun setSlice ast err = setS err (S.slice ast (getL err))
(** Maps #setSlice over a list of errors. *)
fun setSlices ast xs = map (setSlice ast) xs

(** The boolean has to be true if we want to merge consecutive similar regions (see ExtReg.sig). *)
fun setReg err bmerge =
    let val info = (getK err, getF err)
	val extr = ER.getpos_progs (getS err) info
	val regs = ER.delNegRegs (ER.simplify extr bmerge)
    in setR err regs
    end

(** Sets the regions for an error by calling #setReg. *)
fun setRegs errors bmerge = map (fn err => setReg err bmerge) errors


end
