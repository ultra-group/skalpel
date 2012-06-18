(* Copyright 2009 2010 2011 2012 Heriot-Watt University
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
 *  o File name:   Error.sml
 *  o Description: Contains the definition of a type error.  The file
 *      defines the structure Error which has the signature ERROR.
 *)


structure Error :> ERROR = struct

(* abbreviate the names of structures *)
structure I  = Id
structure E  = Env
structure A  = AstSML
structure R  = Reg
structure L  = Label
structure S  = Slicing
structure O  = OrdSet
structure TO = Tools
structure EK = ErrorKind
structure ER = ExtReg
structure EH = ErrorHandler
structure CD = LongId

(* declare some new types *)
type id      = int
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

type times = LargeInt.int * LargeInt.int * LargeInt.int * LargeInt.int * LargeInt.int

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

type export' = error list ->
	       A.packs    ->
	       E.envContextSensitiveSyntaxPair   ->
	       int        ->
	       int        ->
	       unit


(*********************************************************************)
(* printing                                                          *)

fun printId id = Int.toString id

fun printlistgen xs f = "[" ^ #1 (List.foldr (fn (t, (s, c)) => (f t ^ c ^ s, ",")) ("", "") xs) ^ "]"

fun printRemoves xs = printlistgen xs printId

(* should be just for debugging *)
fun printlabss xs = printlistgen xs L.toString ^ "\n\n"

fun printLabReg xs =
    printlistgen
	xs
	(fn (l, rl) => "(" ^ L.printLab l ^ "," ^ R.printRegList rl ^ ")")


(*
fun printErrBisBis [] _ = ""
  | printErrBisBis ((x as {id, labs, deps, ek, rem}) :: xs) ast =
    let
	val regs = A.getpos_prog ast labs
    in
        "<ERROR"
        ^ " labels="      ^ printLabReg regs
        ^ " regions="     ^ "[" ^ R.printRegList (foldl (fn ((_, x), y) => x @ y) [] regs) ^ "]"
        ^ " assumptions=" ^ L.toString deps
        ^ " typeerror="   ^ #2 (EK.printErrKind ek)
        ^ ">"
        ^ "\n"
        ^ (printErrBisBis xs ast)
    end
*)

(*
fun printErrBis [] _ = ""
  | printErrBis ((x as {id, labs, deps, ek, rem}) :: xs) ast =
    let
	val regs = A.getpos_prog ast labs
    in
        "<ERROR"
        ^ " labels="      ^ printLabReg regs
        ^ " assumptions=" ^ L.toString deps
        ^ " typeerror="   ^ #2 (EK.printErrKind ek)
        ^ ">"
        ^ "\n"
        ^ (printErrBis xs ast)
    end
*)

(* turns the character into a string *)
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

fun transfun2 st = String.translate transfun1 st

fun transLisp' #"'"  = "\\'"
  | transLisp' #"#"  = "\\#"
  | transLisp' #"\\" = "\\\\"
  | transLisp' #"?"  = "\\?"
  | transLisp' #"`"  = "\\`"
  | transLisp' x     = Char.toString x

fun transLisp st = String.translate transLisp' st

(* prints error information in XML format *)
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

(* this is used for our database, that's why we don't print anything for this new "rem" field *)
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

fun printOneJsonErr {id, labs, deps, ek, rf, bb, rem, time, sl, regs, min} bslice basisoverloading =
    let val ll = "\"labels\"      : " ^ "{\"count\": " ^ Int.toString (L.length labs) ^ ", " ^ "\"labelNumbers\": " ^ L.toString labs ^ "}"
	val cd = "\"assumptions\" : " ^ CD.toJsonStringOut deps
	val ek = "\"kind\"        : " ^ EK.printJsonErrKind ek
	val tm = "\"time\"        : " ^ LargeInt.toString time
	val id = "\"identifier\"  : " ^ Int.toString id
	val sl = "\"slice\"       : " ^ "\"" ^ transfun2 (S.printSlice sl bslice) ^ "\""
    in (id, ll, cd, ek, tm, sl,
       (if basisoverloading = 0
       then "\"regions\"     : " ^ "[" ^ ER.printJsonExtRegs (List.filter (fn (name, regs) => (if (String.isSubstring "basis.sml" name) then false else true)) regs ) ^ "]"
       else "\"regions\"     : " ^ "[" ^ ER.printJsonExtRegs regs ^ "]"))
    end


fun toListSep xs sep p =
    #1 (foldr (fn (x, (y, z)) => (p ^ x ^ p ^ z ^ y, sep)) ("", "") xs)

(* returns a give slice as a string *)
fun SlToString sl = S.toString sl

fun removesToList rems = map (fn x => printId x) rems

(* prints one error in lisp format *)
fun printOneLispErr {id, labs, deps, ek, rf, bb, rem, time, sl, regs, min} ascid bslice basisoverloading =
    let val id = "(id . " ^ Int.toString id ^ ")"
	val cd = "(assumptions . (" ^ transLisp (toListSep (CD.toStringList deps ascid) " " "") ^ "))"
	val (idk, errk) = EK.printErrKind ek ascid
	val ek = "(kind . (" ^ idk ^ " \"" ^ transfun2 errk ^ "\"))"
	val rm = "(remove . (" ^ toListSep (removesToList rem) " " "" ^ "))"
	val ss = "(slice . \"" ^ transfun2 (S.printSlice sl bslice) ^ "\")"
	val at = "(ast . \"" ^ ""(*SlToString sl*) ^ "\")"
	val re = "(regions . (" ^ (String.translate (fn #"\\" => "\\\\" | x=>str x) (ER.printLispExtRegs (List.filter (fn (name, regs) => (if (String.isSubstring "basis.sml" name) then false else true)) regs))) ^ "))"
	val mn = "(minimal . " ^ Bool.toString min ^ ")"
    in (id, cd, ek, rm, ss, at,
	(if basisoverloading = 0
	 then "(regions . (" ^ (String.translate (fn #"\\" => "\\\\" | x=>str x) (ER.printLispExtRegs (List.filter (fn (name, regs) => (if (String.isSubstring "basis.sml" name) then false else true)) regs))) ^ "))"
	 else "(regions . (" ^ (String.translate (fn #"\\" => "\\\\" | x=>str x) (ER.printLispExtRegs regs)) ^ "))")
      , mn)
    end

(* prints one error in perl format *)
fun printOnePerlErr {id, labs, deps, ek, rf, bb, rem, time, sl, regs, min} ascid bslice basisoverloading =
    let val id = "id => " ^ Int.toString id
	val cd = "assumptions => [" ^ toListSep (CD.toStringList deps ascid) ", " "\"" ^ "]"
	val (idk, errk) = EK.printErrKind ek ascid
	val ek = "kind        => {id  => \"" ^ idk ^ "\", msg => \"" ^ errk ^ "\"}"
	val rm = "remove      => " ^ printRemoves rem
	val sl = "slice       => \"" ^ transfun2 (S.printSlice sl bslice) ^ "\""
	val mn = "minimal     => " ^ Bool.toString min
    in (id, cd, ek, rm, sl,
	(if basisoverloading = 0
	 then "regions     => [" ^ ER.printPerlExtRegs (List.filter (fn (name, regs) => (if (String.isSubstring "basis.sml" name) then false else true)) regs) ^ "]"
	 else "regions     => [" ^ ER.printPerlExtRegs regs ^ "]")
	, mn)
    end

(*
fun printErr [] = ""
  | printErr ((x as {id, labs, deps, ek}) :: xs) =
    "<ERROR"
    ^ " labels="      ^ L.toString labs
    ^ " assumptions=" ^ L.toString deps
    ^ " typeerror="   ^ #2 (EK.printErrKind ek)
    ^ ">"
    ^ "\n"
    ^ (printErr xs)
*)

(*
fun printErrorBisBis [] _ = "<NOERROR>"
  | printErrorBisBis err ast = printErrBisBis err ast
*)

(*
fun printErrorBis [] _ = "<NOERROR>"
  | printErrorBis err ast = printErrBis err ast
*)

(*
fun printError []  = "<NOERROR>"
  | printError err = printErr err
*)


(*********************************************************************)


val idError = ref 0

(* functions to set, get and reset idError.
 * A fresh function is also implemented which will increment idError but return the current value
 *)
fun setError   n = idError := n
fun getError   _ = !idError
fun freshError _ = let val x = !idError in (idError := !idError + 1; x) end
fun resetError _ = setError 0

val dummyId = 0

(* The initial value for the "rem" field of an error *)
val initRem   = []
(* The initial value for the "rf" field of an error *)
val initRf    = L.empty
(* The dummy time value *)
val initTime  = Int.toLarge ~1
(* The dummy slice *)
val initSlice = A.Progs []
(* The dummy Regsins *)
val initRegs  = []
(* The dummy builtinBasis *)
val initBB    = true
(* The dummy minimality marker *)
val initMin   = false

(* changes and id of type id to an integer *)
fun idToInt id = id

(* functions to get each of the record fields *)
fun getI (x : error) = #id   x
fun getL (x : error) = #labs x
fun getD (x : error) = #deps x
fun getK (x : error) = #ek   x
fun getF (x : error) = #rf   x
fun getB (x : error) = #bb   x
fun getE (x : error) = #rem  x
fun getT (x : error) = #time x
fun getS (x : error) = #sl   x
fun getR (x : error) = #regs x
fun getM (x : error) = #min  x

(* constructs an error, given the record fields as parameters *)
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
fun consErrorNoRB id labs deps ek        =
    consError id labs deps ek initRf initBB initRem initTime initSlice initRegs initMin
fun consErrorNoR  id labs deps ek rf     =
    consError id labs deps ek rf     initBB initRem initTime initSlice initRegs initMin
fun consPreError  id labs deps ek rf =
    consError id labs deps ek rf     initBB initRem initTime initSlice initRegs initMin

(* Delete the dummy label *)
fun stripDummy {id, labs, deps, ek, rf, bb, rem, time, sl, regs, min} =
    consError id (L.delete L.dummyLab labs) deps ek rf bb rem time sl regs min

(* Delete the dummy and builtin labels *)
fun stripDummys {id, labs, deps, ek, rf, bb, rem, time, sl, regs, min} =
    consError id (L.delete L.builtinLab (L.delete L.dummyLab labs)) deps ek rf bb rem time sl regs min

(* functions which will set each of the record fields *)
fun setI {id = _, labs, deps, ek, rf, bb, rem, time, sl, regs, min} id   = consError id labs deps ek rf bb rem time sl regs min
fun setL {id, labs = _, deps, ek, rf, bb, rem, time, sl, regs, min} labs = consError id labs deps ek rf bb rem time sl regs min
fun setD {id, labs, deps = _, ek, rf, bb, rem, time, sl, regs, min} deps = consError id labs deps ek rf bb rem time sl regs min
fun setK {id, labs, deps, ek = _, rf, bb, rem, time, sl, regs, min} ek   = consError id labs deps ek rf bb rem time sl regs min
fun setF {id, labs, deps, ek, rf = _, bb, rem, time, sl, regs, min} rf   = consError id labs deps ek rf bb rem time sl regs min
fun setB {id, labs, deps, ek, rf, bb = _, rem, time, sl, regs, min} bb   = consError id labs deps ek rf bb rem time sl regs min
fun setE {id, labs, deps, ek, rf, bb, rem = _, time, sl, regs, min} rem  = consError id labs deps ek rf bb rem time sl regs min
fun setT {id, labs, deps, ek, rf, bb, rem, time = _, sl, regs, min} time = consError id labs deps ek rf bb rem time sl regs min
fun setS {id, labs, deps, ek, rf, bb, rem, time, sl = _, regs, min} sl   = consError id labs deps ek rf bb rem time sl regs min
fun setR {id, labs, deps, ek, rf, bb, rem, time, sl, regs = _, min} regs = consError id labs deps ek rf bb rem time sl regs min
fun setM {id, labs, deps, ek, rf, bb, rem, time, sl, regs, min = _} min  = consError id labs deps ek rf bb rem time sl regs min

(* Adds dependencies to an error *)
fun labelError {id, labs, deps, ek, rf, bb, rem, time, sl, regs, min} labs' stts' deps' =
    consError id (L.union labs labs') (CD.union deps deps') ek (L.union rf stts') bb rem time sl regs min

fun sepsemsyn [] = ([], [])
  | sepsemsyn (x :: xs) =
    let val (sem, syn) = sepsemsyn xs
    in if EK.issem (getK x)
       then (x :: sem, syn)
       else (sem, x :: syn)
    end

(* ordering of errors depending on their sizes *)
fun orderErrors errs =
    let fun add x [] = [x]
	  | add x (y :: ys) = if L.length (getL x) <= L.length (getL y)
			      then x :: y :: ys
			      else y :: (add x ys)
	fun order xs = foldr (fn (x, y) => add x y) [] xs
    in (fn (sem, syn) => (order syn) @ (order sem)) (sepsemsyn errs)
    end

fun getIdsErrors [] = []
  | getIdsErrors (x :: xs) = (getI x) :: (getIdsErrors xs)

(* returns the errors in errs2 that are not in errs1 using the id's *)
fun getNewErrors errs1 errs2 =
    let val ids = O.ord (getIdsErrors errs1)
    in List.filter (fn x => not (O.isin (getI x) ids)) errs2
    end

fun getErrorList []        _          = NONE
  | getErrorList (x :: xs) (id : int) = if getI x = id then SOME x else getErrorList xs id

(* returns the identifiers of all the errors that can be removed *)
fun getToRemove errs = foldr (fn (x, y) => (getE x) @ y) initRem errs

(* filters the errors: keep only the one that are not unvalidated by others *)
fun getMergedErrors errs =
    let val rems = getToRemove errs
    in List.filter (fn x => not (TO.isin (getI x) rems)) errs
    end

(* filters the errors: keep all the minimal ones (that don't specify any removal) *)
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
  | fusionerr _ _ = raise EH.DeadBranch ""

(* multi minimal record errors at once *)
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

(* checks if err1 is smaller or equal to err2 *)
fun alreadyoneone err1 err2 =
    case getK err1 of
	EK.FreeIdent => false
      | EK.Warning _ => false
      | EK.Parsing _ => false
      | _ => L.subseteq (getL err1) (getL err2)
	     andalso
	     CD.subseteq (getD err1) (getD err2)

(* checks if there exists an error smaller or equal to err in the list *)
fun alreadyone [] _ = false
  | alreadyone (x :: xs) err =
    alreadyoneone x err orelse alreadyone xs err

(* checks if err is smaller or equal to one of the errors in the list of errors *)
fun alreadyone' _ [] = false
  | alreadyone' err (x :: xs) =
    alreadyoneone err x orelse alreadyone' err xs

(* checks if the first error is strictly smaller than the second one *)
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

(* all the minimal errors on which depend an errors (merged or not) *)
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
    let fun g [] false = let val newerr = setI err (freshError ())
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

(* before both of them were mindone1 *)
fun mindone  errs err = mindone2 errs err
fun mindone' errs err = mindone1 errs err


fun setSlice ast err = setS err (S.slice ast (getL err))

fun setSlices ast xs = map (setSlice ast) xs

fun setReg err bmerge =
    let val info = (getK err, getF err)
	val extr = ER.getpos_progs (getS err) info
	(*val _ = Debug.printdebug2 (printOneXmlErr err "" false
				     ^ "\n" ^
				     ER.printOneRegs extr)*)
	val regs = ER.delNegRegs (ER.simplify extr bmerge)
    in setR err regs
    end

fun setRegs errors bmerge = map (fn err => setReg err bmerge) errors


end
