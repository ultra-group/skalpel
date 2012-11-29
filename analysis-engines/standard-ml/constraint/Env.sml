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
 *  o Authors:     Vincent Rahli, John Pirie
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        21 May 2010
 *  o File name:   Env.sml
 *  o Description: Contains our constraint system.  The file defines the
 *      structure Env with signature ENV.
 *)


structure Env :> ENV = struct

(* ====== STRUCTURES ====== *)
structure T   = Ty
structure I   = Id
structure X   = Expans
structure L   = Label
structure P   = Poly
structure C   = ConsId
structure D   = Debug
structure CL  = ClassId
structure CD  = LongId
structure EL  = ExtLab  (* WTF is the "Ext" for ?*)
structure EH  = ErrorHandler
structure envOrdMap = SplayMapFn (OrdId)  (* map for environments *)
structure OMC = SplayMapFn (OrdKey) (* map for Constraints, should be OrdLab *)
structure OMO = Fifo (*SplayMapFn (OrdLid) (* map for Open environments *)*)

(* ====== TYPES/DATATYPES ====== *)

(* ------ MAPPINGS ------ *)
type 'a constraintMap   = 'a OMC.map        (* constraint map *)
type 'a envMap          = 'a envOrdMap.map  (* environment map *)
type 'a openEnvMap      = 'a OMO.fifo       (* open environment map *)

(* ------ VARIABLES ------ *)
type envVar         = int

(* this is for what kind of bind indentifier we are binding. We have the:
 * - binding (the actual thing being bound, C.bind)
 * - class part of C.bind (the class which we're binding it in)
 * - label part of C.bind (everything is labelled)
 * - poly  part of C.bind (some polymorphic information?) *)
type 'a bind  = 'a C.bind EL.extLab

(* genericEnvironment
 * stores binders of structures and signatures and such
 * it is suspected that this was created as a speed optimisation *)
(*(2010-04-08)We should use genericEnvironment for all our similar environments*)
type 'a genericEnv  = 'a bind list envMap

(* ------ OPENENV ------ *)
datatype openKind    = OPENED_STRUCT        (* opened structure *)
		     | DATATYPE_REPLICATION (* datatype replication *)
		     | INCLUDED_SIG         (* included signature *)

type openSem         = I.lid   *  (* id of struct to open *)
		       L.label *  (* label of struct to open *)
		       openKind    (* kind of the opening *)

type openEnv         = openSem openEnvMap

(* ------ VARENV ------ *)
(*(2010-06-10)The Boolean in Ty.ty bind to indicate if the varEnv is complete or not.*)
type extVar         = T.ty bind
type varEnv         = T.ty genericEnv (* Tyty (ConsId.bind ExtLab.extLab list) envOrdMap.mp *)

(* ------ KIND OF A TYPE DECLARATION *)
datatype typeNameKind = DATATYPE | TYPE

(* ------ TYPEENV ------ *)

 (* extType maybe stands for explicit type variables? *)
type extType         = (T.typeFunction * typeNameKind * (varEnv * bool) ref) bind
type typeEnv         = (T.typeFunction * typeNameKind * (varEnv * bool) ref) genericEnv

(* ------ OVERLOADINGENV ------ *)
type extovc                = T.rowType bind
type overloadingClassesEnv = T.rowType genericEnv

(* ------ TYPEVARENV ------ *)
type explicitTypeVar = (T.typeVar * bool) bind (* bool is true for an explicit type variable and false for an implicit one.*)
type typeVarEnv      = (T.typeVar * bool) genericEnv

(* ------ INFORMATION ON ENV ------ *)
type typeName    = {id : I.id, lab : L.label, kind : typeNameKind, name : T.typename}
type typeNameMap = typeName list (* this should be: tname extLab list *)
datatype names   = TYPENAME of typeName EL.extLab (* definitely a typename *)
		 | DUMTYPENAME of typeName       (* maybe a type name defining tname but not enough info to be sure *)
		 | MAYTYPENAME                   (* maybe a type name but not enough info to know *)
		 | NOTTYPENAME of I.id EL.extLab (* definitely not a type name *)

type infoEnv     = {lab : L.label,   (* should be a list or a set. See uenvEnv in Analyze.sml *)
		    complete : bool, (* true if env is complete (true when initialised and can be false during unification *)
		    infoTypeNames : typeNameMap, (* typeNames introduced in the structure, these are here to ease the
					      * collection of type names when pushing an env onto a unification context *)
		    argOfFunctor : bool}      (* true if the env is the argument of a function *)

(* ------ ENRICHEMENT AND INSTANTIATION ------ *)
(* 1st envVar: instantiated signature
 * 2nd envVar: signature matching
 * 3rd envVar: enriched structure
 * 4th envVar: resulting structure
 * the bool if false if we want to impose some restrictions based on the
 * structure env (where clauses). *)
(* WARNING: it is suspected the above comment may not be relevant to the below. Read with caution! *)
type evsbind        = envVar        *
		      envVar option *
		      envVar        *
		      envVar option *
		      L.label

(* ------ FUNCTOR INSTANTIATION ------ *)
type evfbind        = envVar * envVar * envVar * envVar * L.label

(* ------ SHARING ------ *)
type shabind        = envVar * envVar * envVar * L.label

(* represents a type of accessor - see datatype constructors ending in "_ACCESSOR" *)
type 'a accessorId       = {lid : I.lid, equalityTypeVar : T.equalityTypeVar, sem : 'a, class : CL.class, lab : L.label}

(* ------ LONG TYPE CONSTRUCTOR BINDER ------ *)
type longTypeConsBinder        = T.typeFunction accessorId EL.extLab

type class = CL.class

(* for denoting opaque or translucent signatures *)
datatype matchKind  = OPAQUE | TRANSLUCENT

(* the env datatype *)
datatype env = ENV_CONS of {valueIds : varEnv,                          (* value identifiers *)
			    typeNames : typeEnv,                        (* type names *)
			    explicitTypeVars : typeVarEnv,              (* explicit type variables (not used any more?) *)
			    structs : env genericEnv,                   (* structures (this is strenv *)
			    sigs : env genericEnv,                      (* signatures (this is sigenv *)
			    functors : (env * env) genericEnv,          (* functors (this is funenv *)
			    overloadingClasses : overloadingClassesEnv, (* overloading classes *)
			    info : infoEnv}
	     | ENV_VAR of envVar * L.label

	     (* ROW_ENV represents row envs (formerly called sequence envs)
	      * e := ... | e2; e1
	      * we use rows as a sort of logical AND *)
	     | ROW_ENV of env * env

	     (* LOCAL_ENV represents local envs
	      * e := ... | loc e1 in e2
	      * This builds an env e2 which depends on e1 and only exports e2's binders,
	      * so only e2's binders can be accessed from outside the local env. This is
	      * different from the envs of the form e1;e2 as this builds a new env
	      * from e1 and e2 and exports both e1's binders not shadowed by e2 and e2's binders *)
	     | LOCAL_ENV of env * env

	     | ENVWHR of env * longTypeConsBinder

	     (*(2010-07-07)We want something like: Ty.typeFunction accessorId extLab list, instead of the second env.*)
	     | ENVSHA of env * env

	     (* SIGNATURE_ENV represents signature envs
	      * in the paper this would be e := ... | e1;e2| ens(e) | subty *)
	     | SIGNATURE_ENV of env * env * matchKind


	     | ENVPOL of typeVarEnv * env (* the first env is the explicit type variables and the second the value bindings *)
	     | DATATYPE_CONSTRUCTOR_ENV of I.labelledId  * env (* (2010-06-10)This is to associate constructors to a datatype *)
	     | ENVOPN of openEnv (* we want to move from ENVOPN to ENVDEP *)
	     | ENVDEP of env EL.extLab
	     | FUNCTOR_ENV of constraints (*(2010-06-22)This env is only solved when dealing with CSTFUN.*)

	     (* CONSTRAINT_ENV is probably for constraint/env
	      * think this is c ::= μ1=μ2 | e1=e2 | τ1=τ2
	      * these act as both a constraint and an env *)
	     | CONSTRAINT_ENV of constraints

	     | ENVPTY of string (* This is generated by a sml-tes special comment SML-TES-TYPE and used to print the type of an identifier *)
	     | ENVFIL of string * env * (unit -> env) (* Like a SEQ but the first env is from a file which has the name given by the string and the second env is a stream *)
	     | TOP_LEVEL_ENV (* to mark that we reached the top level *)

     and accessor        = VALUEID_ACCESSOR of T.ty accessorId EL.extLab                       (* value identifiers *)
			 | EXPLICIT_TYPEVAR_ACCESSOR of T.ty accessorId EL.extLab              (* explicit type variables *)
			 | EQUALITY_TYPE_ACCESSOR of T.equalityType accessorId EL.extLab       (* explicit type variables *)
			 | TYPE_CONSTRUCTOR_ACCESSOR of T.typeFunction accessorId EL.extLab    (* type constructors *)
			 | OVERLOADING_CLASSES_ACCESSOR of T.rowType accessorId EL.extLab      (* overloading classes *)
			 | STRUCTURE_ACCESSOR of env accessorId EL.extLab                      (* structures *)
			 | SIGNATURE_ACCESSOR of env accessorId EL.extLab                      (* signatures *)
			 | FUNCTOR_ACCESSOR of (env * env) accessorId EL.extLab                (* functors *)

     and oneConstraint    = TYPE_CONSTRAINT     of (T.ty     * T.ty)     EL.extLab
			  | TYPENAME_CONSTRAINT of (T.typenameType   * T.typenameType)   EL.extLab
			  | ROW_CONSTRAINT of (T.rowType  * T.rowType)  EL.extLab
			  | FIELD_CONSTRAINT of (T.fieldType  * T.fieldType)  EL.extLab
			  | LABEL_CONSTRAINT of (T.labelType  * T.labelType)  EL.extLab
			  | ENV_CONSTRAINT of (env      * env)      EL.extLab
			  | IDENTIFIER_CLASS_CONSTRAINT of (CL.class * CL.class) EL.extLab
			  | FUNCTION_TYPE_CONSTRAINT of (T.typeFunction  * T.typeFunction)  EL.extLab (* type function constrint*)
			  | ACCESSOR_CONSTRAINT of accessor
			  | LET_CONSTRAINT of env
			  | SIGNATURE_CONSTRAINT of evsbind (* Transform that into an env with a switch for opaque and translucent *)
			  | FUNCTOR_CONSTRAINT of evfbind   (* Transform that into an env *)
			  | SHARING_CONSTRAINT of shabind   (* Transform that into an env *)
			  | EQUALITY_TYPE_CONSTRAINT of (T.equalityType * T.equalityType) EL.extLab  (* a constraint for equality type checking *)


     and constraints      = CONSTRAINTS of oneConstraint list constraintMap
(* constraints maps integers (program point labels) to lists (conceptually sets) of oneConstraint's *)
(*(2010-04-14)Conceptually, (INJI extty) seems to be an env of the form:
 * ENVS (ENVV ev, ENVC x), where extty is declared in x and ev is fresh.*)
(*(2010-03-02)not withtype because it is not SML valid and even though SML/NJ
 * does not complain, MLton does.*)

fun getConstraintItems (CONSTRAINTS(map)) = OMC.listItems map

type extstr = env bind
type strenv = env genericEnv

type extsig = env bind
type sigenv = env genericEnv

(* functor has type env1 -> env2 *)
type funsem = env * env
type extfun = funsem bind
type funenv = funsem genericEnv

(* oneContextSensitiveSyntaxError
 * these used to be in the env but they got moved out
 * they are now separate because the usualyl get passed through *)
datatype oneContextSensitiveSyntaxError = CSSMULT of Label.labels (* for a multi occurrence - the 'id list' is then always empty *)
					| CSSCVAR of Label.labels (* for applied identifiers in patterns that should be constructors but are variables *)
					| CSSEVAR of Label.labels (* for identifiers in exception bindings that should be exceptions but are variables *)
					| CSSECON of Label.labels (* for identifiers in exception bindings that should be exceptions but are datatype constructors *)
					| CSSINCL of Label.labels (* for non inclusion of type variable in dataptype         *)
					| CSSAPPL of Label.labels (* for applied and not applied value in pattern            *)
					| CSSFNAM of Label.labels (* for different function names                            *)
					| CSSFARG of Label.labels (* a function with different number of arguments           *)
					| CSSTYVA of Label.labels (* free type variable at top-level                         *)
					| CSSLEFT of Label.labels (* ident to left of as in pattern must be a variable       *)
					| CSSFREC of Label.labels (* expressions within rec value bindings must be functions *)
					| CSSREAL of Label.labels (* reals cannot occur within patterns                      *)
					| CSSFREE of Label.labels (* free identifier                                         *)
					| CSSWARN of Label.labels * string (* for a warning *)
					| CSSPARS of Label.labels * string (* for a parsing problem *)
type contextSensitiveSyntaxError       = oneContextSensitiveSyntaxError list

type envContextSensitiveSyntaxPair = env * contextSensitiveSyntaxError


(* ====== FUNCTIONS ====== *)

(* ------ printing section ------ *)

val tab = "           "

fun printlistgen xs f = "[" ^ #1 (foldr (fn (t, (s, c)) => (f t ^ c ^ s, ",")) ("", "") xs) ^ "]"

fun printBind bind f assoc = EL.printExtLab bind (fn x => C.printBind x f assoc) assoc

fun printBind' bind f = printBind bind f I.emAssoc

fun printEnvVar v = "e" ^ Int.toString v

fun printEnvVarList xs = printlistgen xs printEnvVar

fun printOp NONE _ = "-"
  | printOp (SOME x) f = f x

fun printEvOp       x = printOp x printEnvVar

fun printTnKind DATATYPE = "DATATYPE"
  | printTnKind TYPE = "TYPE"

fun printGenEnv xs ind f =
    let val ind' = ind ^ "    "
    in #1 (envOrdMap.foldli
	       (fn (k, x, (y, z)) =>
		   (y ^ z ^ I.printId k ^ ":" ^ f x, "\n" ^ ind'))
	       ("", "")
	       xs)
    end

fun printExtVar extvar = printBind' extvar T.printty
and printExtTyv exttyv = printBind' exttyv (fn (tv, b) => "(" ^ T.printTypeVar tv ^ "," ^ Bool.toString b ^ ")")
and printExtTyp exttyp = printBind' exttyp (fn (tyf, tnkind, cons) => "(tyf=" ^ T.printtyf tyf ^ ",tnkind=" ^ printTnKind tnkind ^ ",cons=" ^ printExtCon (!cons) ^ ")")
and printExtSeq extovc = printBind' extovc T.printseqty
and printExtCon (cons, b) = "(" ^ printVarEnv cons "" ^ "," ^ Bool.toString b ^ ")"

and printExtVarList xs = printlistgen xs printExtVar
and printExtTyvList xs = printlistgen xs printExtTyv
and printExtTypList xs = printlistgen xs printExtTyp
and printExtSeqList xs = printlistgen xs printExtSeq

and printTypeVarEnv xs ind = printGenEnv xs ind printExtTyvList
and printVarEnv xs ind = printGenEnv xs ind printExtVarList
and printTypeEnv xs ind = printGenEnv xs ind printExtTypList
and printOvcEnv xs ind = printGenEnv xs ind printExtSeqList

fun printOpenKind OPENED_STRUCT = "OPENED_STRUCT"
  | printOpenKind DATATYPE_REPLICATION = "DATATYPE_REPLICATION"
  | printOpenKind INCLUDED_SIG = "INCLUDED_SIG"

fun printOpenEnv xs ind =
    #1 (OMO.foldl
	    (fn ((i, l, k), (y, z)) =>
		(y ^ z ^
		 I.printLid      i ^ ":(" ^
		 L.printLab      l ^ ","  ^
		 printOpenKind    k ^ ")",
		 "\n" ^ ind ^ "    "))
	    ("", "")
	    xs)

fun printTypeNameMap xs =
    printlistgen xs (fn {id, lab, kind, name} =>
			"(" ^ I.printId     id   ^
			"," ^ L.printLab    lab  ^
			"," ^ printTnKind   kind ^
			"," ^ T.printTypename name ^ ")")

fun printNfoEnv {lab, complete, infoTypeNames, argOfFunctor} =
    "(" ^ L.printLab    lab ^
    "," ^ Bool.toString complete ^
    "," ^ printTypeNameMap    infoTypeNames ^
    "," ^ Bool.toString argOfFunctor ^ ")"

fun printEvsBind (ev1, ev2, ev3, ev4, lab) =
    "(" ^ printEnvVar ev1 ^
    "," ^ printEvOp   ev2 ^
    "," ^ printEnvVar ev3 ^
    "," ^ printEvOp   ev4 ^
    "," ^ L.printLab  lab ^ ")"

fun printEvfBind (ev1, ev2, ev3, ev4, lab) =
    "(" ^ printEnvVar ev1 ^
    "," ^ printEnvVar ev2 ^
    "," ^ printEnvVar ev3 ^
    "," ^ printEnvVar ev4 ^
    "," ^ L.printLab  lab ^ ")"

fun printShaBind (ev1, ev2, ev3, lab) =
    "(" ^ printEnvVar ev1 ^
    "," ^ printEnvVar ev2 ^
    "," ^ printEnvVar ev3 ^
    "," ^ L.printLab  lab ^ ")"

fun printPair (x, y) f = "(" ^ f x ^ "," ^ f y ^ ")"

fun printAccessorId {lid, equalityTypeVar, sem, class, lab} f ind ascid =
    "{" ^ I.printLid' lid ascid ^
    "," ^ T.printEqualityTypeVar equalityTypeVar ^
    "," ^ f sem                 ^
    "," ^ CL.toString class     ^
    "," ^ L.printLab lab        ^ "}"

fun printMatchKind OPAQUE = "OPAQUE"
  | printMatchKind TRANSLUCENT = "TRANSLUCENT"

fun printLongTypeConsBinder longTypeConsBinder ind =
    "LONGTYPECONSBINDER(" ^ EL.printExtLab' longTypeConsBinder (fn x => printAccessorId x T.printtyf' ind I.emAssoc) ^ ")"

fun printExtEnv     x      = printBind' x (fn x => printEnv x "")
and printExtEnvList xs     = printlistgen xs printExtEnv
and printFunSemList xs     = printlistgen xs (fn (x, y) => "(" ^ printExtEnv x ^ "," ^ printExtEnv y ^ ")")
and printSigEnv     xs ind = printGenEnv xs (ind ^ "  ") printExtEnvList
and printStrEnv     xs ind = printGenEnv xs (ind ^ "  ") printExtEnvList
and printFunEnv     xs ind = printGenEnv xs ind printFunSemList
and printEnv (ENV_CONS {valueIds, typeNames, explicitTypeVars, structs, sigs, functors, overloadingClasses, info}) ind =
    "\n" ^ ind ^ "{valueIds:" ^ printVarEnv valueIds ind ^
    "\n" ^ ind ^ " typeNames:" ^ printTypeEnv typeNames ind ^
    "\n" ^ ind ^ " explicitTypeVars:" ^ printTypeVarEnv explicitTypeVars ind ^
    "\n" ^ ind ^ " structs:" ^ printStrEnv structs ind ^
    "\n" ^ ind ^ " sigs:" ^ printSigEnv sigs ind ^
    "\n" ^ ind ^ " overloadingClasses:" ^ printOvcEnv overloadingClasses ind ^
    "\n" ^ ind ^ " info:" ^ printNfoEnv info ^ "}"
  | printEnv (ENV_VAR (ev, lab))  _   = "ENV_VAR(" ^ printEnvVar ev ^ "," ^ L.printLab lab ^ ")"
  | printEnv (ENVOPN openEnv)     ind = "ENVOPN(" ^ printOpenEnv openEnv ind ^ ")"
  | printEnv (ENVDEP extenv)     ind = "ENVDEP(" ^ EL.printExtLab' extenv (fn env => printEnv env ind) ^ ")"
  | printEnv (FUNCTOR_ENV cst) ind = "CONSTRAINT_ENV(" ^ printcst' cst ind I.emAssoc ^ ")"
  | printEnv (CONSTRAINT_ENV cst) ind = "CONSTRAINT_ENV(" ^ printcst' cst ind I.emAssoc ^ ")"
  | printEnv (ROW_ENV (env1, env2)) ind =
    "ROW_ENV(" ^ printEnv env1 ind ^ ",\n" ^ ind ^ printEnv env2 ind ^ ")"
  | printEnv (LOCAL_ENV (env1, env2)) ind =
    "LOCAL_ENV(" ^ printEnv env1 ind ^ ",\n" ^ ind ^ printEnv env2 ind ^ ")"
  | printEnv (ENVWHR (env, longTypeConsBinder)) ind =
    "ENVWHR(" ^ printEnv env ind ^ ",\n" ^ ind ^ printLongTypeConsBinder longTypeConsBinder ind ^ ")"
  | printEnv (ENVSHA (env1, env2)) ind =
    "ENVSHA(" ^ printEnv env1 ind ^ ",\n" ^ ind ^ printEnv env2 ind ^ ")"
  | printEnv (SIGNATURE_ENV (env1, env2, kind)) ind =
    "SIGNATURE_ENV(" ^ printEnv env1 ind ^ ",\n" ^ ind ^ printEnv env2 ind ^ "," ^ printMatchKind kind ^ ")"
  | printEnv (ENVPOL (typeVarEnv, env)) ind =
    "ENVPOL(" ^ printTypeVarEnv typeVarEnv ind ^ ",\n" ^ ind ^ printEnv env ind ^ ")"
  | printEnv (DATATYPE_CONSTRUCTOR_ENV (labelledId, env)) ind =
    "DATATYPE_CONSTRUCTOR_ENV(" ^ I.printIdL labelledId ^ "," ^ printEnv env ind ^ ")"
  | printEnv (ENVPTY st) ind = "ENVPTY(" ^ st ^ ")"
  | printEnv (ENVFIL (st, env, stream)) ind =
    "ENVFIL(" ^ st ^ "," ^ printEnv env ind ^ ",\n" ^ printEnv (stream ()) ind ^ ")"
  | printEnv ENVTOP ind = "ENVTOP"
and printAcc (VALUEID_ACCESSOR x) ind ascid =
    "VALUEID_ACCESSOR(" ^ EL.printExtLab x (fn x => printAccessorId x T.printty' ind ascid) ascid ^ ")"
  | printAcc (EXPLICIT_TYPEVAR_ACCESSOR x) ind ascid =
    "EXPLICIT_TYPEVAR_ACCESSOR(" ^ EL.printExtLab x (fn x => printAccessorId x T.printty ind ascid) ascid ^ ")"
  | printAcc (EQUALITY_TYPE_ACCESSOR x) ind ascid =
    "EQUALITY_TYPE_ACCESSOR(" ^ EL.printExtLab x (fn x => printAccessorId x T.printEqualityType ind ascid) ascid ^ ")"
  | printAcc (TYPE_CONSTRUCTOR_ACCESSOR x) ind ascid =
    "TYPE_CONSTRUCTOR_ACCESSOR(" ^ EL.printExtLab x (fn x => printAccessorId x T.printtyf' ind ascid) ascid ^ ")"
  | printAcc (OVERLOADING_CLASSES_ACCESSOR x) ind ascid =
    "OVERLOADING_CLASSES_ACCESSOR(" ^ EL.printExtLab x (fn x => printAccessorId x T.printseqty' ind ascid) ascid ^ ")"
  | printAcc (STRUCTURE_ACCESSOR x) ind ascid =
    "STRUCTURE_ACCESSOR(" ^ EL.printExtLab x (fn x => printAccessorId x (fn e => printEnv e (ind ^ tab)) ind ascid) ascid ^ ")"
  | printAcc (SIGNATURE_ACCESSOR x) ind ascid =
    "SIGNATURE_ACCESSOR(" ^ EL.printExtLab x (fn x => printAccessorId x (fn e => printEnv e (ind ^ tab)) ind ascid) ascid ^ ")"
  | printAcc (FUNCTOR_ACCESSOR x) ind ascid =
    "FUNCTOR_ACCESSOR(" ^ EL.printExtLab x (fn x => printAccessorId x (fn (e1, e2) => "(" ^ printEnv e1 (ind ^ tab) ^ ",\n" ^ printEnv e1 (ind ^ tab) ^ ")") ind ascid) ascid ^ ")"
and printocst (TYPE_CONSTRAINT x) _ ascid =
    "  TYPE_CONSTRAINT(" ^ EL.printExtLab x (fn x => printPair x T.printty') ascid ^ ")"
  | printocst (FUNCTION_TYPE_CONSTRAINT x) _ ascid =
    "  TYPE_FUNCTION_CONSTRAINT(" ^ EL.printExtLab x (fn x => printPair x T.printtyf') ascid ^ ")"
  | printocst (TYPENAME_CONSTRAINT x) _ ascid =
    "  TYPENAME_CONSTRAINT(" ^ EL.printExtLab x (fn x => printPair x T.printtnty') ascid ^ ")"
  | printocst (ROW_CONSTRAINT x) _ ascid =
    "  ROW_CONSTAINT(" ^ EL.printExtLab x (fn x => printPair x T.printseqty') ascid ^ ")"
  | printocst (EQUALITY_TYPE_CONSTRAINT x) _ ascid =
    "  EQUALITY_TYPE_CONSTAINT(" ^ EL.printExtLab x (fn x => printPair x T.printEqualityType) ascid ^ ")"
  | printocst (FIELD_CONSTRAINT x) _ ascid =
    "  FIELD_CONSTRAINT(" ^ EL.printExtLab x (fn x => printPair x T.printfieldty') ascid ^ ")"
  | printocst (LABEL_CONSTRAINT x) _ ascid =
    "  LABEL_CONSTAINT(" ^ EL.printExtLab x (fn x => printPair x T.printlabty) ascid ^ ")"
  | printocst (ENV_CONSTRAINT x) ind ascid =
    "  ENV_CONSTRAINT(" ^ EL.printExtLab x (fn x => printPair x (fn x => printEnv x (ind ^ tab))) ascid ^ ")"
  | printocst (IDENTIFIER_CLASS_CONSTRAINT x) ind ascid =
    "  IDENTTIFIER_CLASS_CONSTRAINT(" ^ EL.printExtLab x (fn x => printPair x CL.toString) ascid  ^ ")"
  | printocst (SIGNATURE_CONSTRAINT evsbind) ind ascid = "  SIG(" ^ printEvsBind evsbind ^ ")"
  | printocst (FUNCTOR_CONSTRAINT evfbind) ind ascid = "  FUN(" ^ printEvfBind evfbind ^ ")"
  | printocst (SHARING_CONSTRAINT shabind) ind ascid = "  SHA(" ^ printShaBind shabind ^ ")"
  | printocst (LET_CONSTRAINT env)     ind ascid = "  LET(" ^ printEnv env (ind ^ "      ") ^ ")"
  | printocst (ACCESSOR_CONSTRAINT acc)     ind ascid = "  ACCESSOR_CONSTRAINT(" ^ printAcc acc ind ascid ^ ")"
and printocstlist []        ind1 ind2 ind3 ascid = ""
  | printocstlist [x]       ind1 ind2 ind3 ascid = ind2 ^ printocst x ind1 ascid
  | printocstlist (x :: xs) ind1 ind2 ind3 ascid = ind2 ^ printocst x ind1 ascid ^
						   "\n" ^
						   printocstlist xs ind1 (ind1 ^ ind3) ind3 ascid
and printcst' (CONSTRAINTS cst) ind ascid =
    OMC.foldri
	(fn (n, oneConstraint, y) =>
	    ind ^ Int.toString n
	    ^ ":"
	    ^ printocstlist oneConstraint ind "" (" " ^ (String.translate (fn _ => " ") (Int.toString n))) ascid
	    ^ "\n"
	    ^ y) "" cst
and printcst cst ascid = printcst' cst "" ascid

fun printConstraints cst =
    printcst cst I.emAssoc (* printocst(List.hd (List.hd(OMC.listItems(cst)))) *)

fun printOneConstraint cst = printocst cst "" I.emAssoc
fun printOneAccessor   cst = printAcc cst "" I.emAssoc

(* Bindings constructors *)

fun consBind     id bind equalityTypeVar class lab poly = EL.initExtLab (C.consBind     id bind equalityTypeVar class lab poly) lab
fun consBindPoly {id=id, typeOfId=bind, equalityTypeVar=eqtv, classOfId=class, labelOfConstraint=lab} =
    EL.initExtLab (C.consBindPoly id bind eqtv class lab)      lab
fun consBindMono id bind equalityTypeVar class lab      = EL.initExtLab (C.consBindMono id bind equalityTypeVar class lab)      lab

fun consAccessorId lid eqtv sem class lab = {lid = lid, equalityTypeVar = eqtv, sem = sem, class = class, lab = lab}

(* Accessors to a extenv *)

fun getBindI x = C.getBindI (EL.getExtLabT x)
fun getBindT x = C.getBindT (EL.getExtLabT x)
fun getBindEqualityTypeVar x = C.getBindEqualityTypeVar (EL.getExtLabT x) (* get equality type information from a binder *)
fun getBindC x = C.getBindC (EL.getExtLabT x)
fun getBindL x = C.getBindL (EL.getExtLabT x)
fun getBindP x = C.getBindP (EL.getExtLabT x)

val nextEnvVar     = ref 0
fun setNextEnvVar     n  = nextEnvVar := n
fun getEnvVar   () = !nextEnvVar
fun freshEnvVar () = let val x = !nextEnvVar in nextEnvVar := x + 1; x end
fun resetEnvVar () = setNextEnvVar 0

fun envVarToInt envVar = envVar

fun eqEnvVar ev1 ev2 = (ev1 = (ev2 : envVar))

fun consENV_VAR ev lab = ENV_VAR (ev, lab)
fun newEnvVar  lab    = consENV_VAR (freshEnvVar ()) lab

fun envToEnvVar (ENV_VAR ev) = ev
  | envToEnvVar _ = raise EH.DeadBranch "the env should be a variable"

fun consEnvConstructor valueIds typeNames explicitTypeVars structs sigs functors overloadingClasses info =
    ENV_CONS {valueIds = valueIds,
	    typeNames = typeNames,
	    explicitTypeVars = explicitTypeVars,
	    structs = structs,
	    sigs = sigs,
	    functors = functors,
	    overloadingClasses = overloadingClasses,
	    info = info}

fun consInfo lab complete infoTypeNames argOfFunctor = {lab = lab, complete = complete, infoTypeNames = infoTypeNames, argOfFunctor = argOfFunctor}

val emptyMap = envOrdMap.empty
val emvar = emptyMap
val emtyp = emptyMap
val emtv  = emptyMap
val emstr = emptyMap
val emsig = emptyMap
val emfun = emptyMap
val emopn = OMO.empty
val emoc  = emptyMap
val emful = true        (* The env is initially complete                       *)
val emtn  = [] : typeNameMap  (* The env does not initially define any type name     *)
val emfct = false       (* The env is initially not the parameter of a functor *)
val emnfo = consInfo L.dummyLab emful emtn emfct
val emptyEnv = consEnvConstructor emvar emtyp emtv emstr emsig emfun emoc emnfo


(* Accessors to envs *)

fun getValueIds (ENV_CONS x) = #valueIds x
  | getValueIds _          = raise EH.DeadBranch ""
fun getTypeNameEnv (ENV_CONS x) = #typeNames x
  | getTypeNameEnv _          = raise EH.DeadBranch ""
fun getExplicitTypeVars (ENV_CONS x) = #explicitTypeVars x
  | getExplicitTypeVars _          = raise EH.DeadBranch ""
fun getStructs (ENV_CONS x) = #structs x
  | getStructs _          = raise EH.DeadBranch ""
fun getSigs (ENV_CONS x) = #sigs x
  | getSigs _          = raise EH.DeadBranch ""
fun getFunctors (ENV_CONS x) = #functors x
  | getFunctors _          = raise EH.DeadBranch ""
fun getOverloadingClasses (ENV_CONS x) = #overloadingClasses x
  | getOverloadingClasses _          = raise EH.DeadBranch ""
fun getInfo (ENV_CONS x) = #info x
  | getInfo _          = raise EH.DeadBranch ""
fun getILab (ENV_CONS x) = #lab (#info x)
  | getILab _          = raise EH.DeadBranch ""
fun getIComplete (ENV_CONS x) = #complete (#info x)
  | getIComplete _          = raise EH.DeadBranch ""
fun getIArgOfFunctor (ENV_CONS x) = #argOfFunctor (#info x)
  | getIArgOfFunctor _          = raise EH.DeadBranch ""
fun getITypeNames (ENV_CONS x)        = #infoTypeNames (#info x)
  | getITypeNames (ROW_ENV (e1, e2)) = (getITypeNames e1) @ (getITypeNames e2)
  | getITypeNames (ENV_VAR _)        = []
  | getITypeNames env               = (print (printEnv env ""); raise EH.DeadBranch "")

(* update fields in a record of type infoEnv *)
fun updateInfoLab lab {lab = _, complete, infoTypeNames, argOfFunctor} = consInfo lab complete infoTypeNames argOfFunctor
fun updateInfoComplete complete {lab, complete = _, infoTypeNames, argOfFunctor} = consInfo lab complete infoTypeNames argOfFunctor
fun updateInfoTypeNames infoTypeNames {lab, complete, infoTypeNames = _, argOfFunctor} = consInfo lab complete infoTypeNames argOfFunctor
fun updateInfoArgOfFunctor argOfFunctor {lab, complete, infoTypeNames, argOfFunctor = _} = consInfo lab complete infoTypeNames argOfFunctor

(* modifier methods to update values of constructors of the datatype env *)
fun updateValueIds valueIds (ENV_CONS {valueIds = _, typeNames, explicitTypeVars, structs, sigs, functors, overloadingClasses, info}) =
    consEnvConstructor valueIds typeNames explicitTypeVars structs sigs functors overloadingClasses info
  | updateValueIds _ _ = raise EH.DeadBranch ""
fun updateTypeNames typeNames (ENV_CONS {valueIds, typeNames = _, explicitTypeVars, structs, sigs, functors, overloadingClasses, info}) =
    consEnvConstructor valueIds typeNames explicitTypeVars structs sigs functors overloadingClasses info
  | updateTypeNames _ _ = raise EH.DeadBranch ""
fun updateExplicitTypeVars explicitTypeVars (ENV_CONS {valueIds, typeNames, explicitTypeVars = _, structs, sigs, functors, overloadingClasses, info}) =
    consEnvConstructor valueIds typeNames explicitTypeVars structs sigs functors overloadingClasses info
  | updateExplicitTypeVars _ _ = raise EH.DeadBranch ""
fun updateStructs structs (ENV_CONS {valueIds, typeNames, explicitTypeVars, structs = _, sigs, functors, overloadingClasses, info}) =
    consEnvConstructor valueIds typeNames explicitTypeVars structs sigs functors overloadingClasses info
  | updateStructs _ _ = raise EH.DeadBranch ""
fun updateSigs sigs (ENV_CONS {valueIds, typeNames, explicitTypeVars, structs, sigs = _, functors, overloadingClasses, info}) =
    consEnvConstructor valueIds typeNames explicitTypeVars structs sigs functors overloadingClasses info
  | updateSigs _ _ = raise EH.DeadBranch ""
fun updateFunctors functors (ENV_CONS {valueIds, typeNames, explicitTypeVars, structs, sigs, functors = _, overloadingClasses, info}) =
    consEnvConstructor valueIds typeNames explicitTypeVars structs sigs functors overloadingClasses info
  | updateFunctors _ _ = raise EH.DeadBranch ""
fun updateOverloadingClasses overloadingClasses (ENV_CONS {valueIds, typeNames, explicitTypeVars, structs, sigs, functors, overloadingClasses = _, info}) =
    consEnvConstructor valueIds typeNames explicitTypeVars structs sigs functors overloadingClasses info
  | updateOverloadingClasses _ _ = raise EH.DeadBranch ""
fun updateILab lab (ENV_CONS {valueIds, typeNames, explicitTypeVars, structs, sigs, functors, overloadingClasses, info}) =
    consEnvConstructor valueIds typeNames explicitTypeVars structs sigs functors overloadingClasses (updateInfoLab lab info)
  | updateILab lab (ROW_ENV (env1, env2)) = ROW_ENV (updateILab lab env1, updateILab lab env2)
  | updateILab lab (LOCAL_ENV (env1, env2)) = LOCAL_ENV (env1, updateILab lab env2)
  | updateILab _ env = env
fun updateIComplete complete (ENV_CONS {valueIds, typeNames, explicitTypeVars, structs, sigs, functors, overloadingClasses, info}) =
    consEnvConstructor valueIds typeNames explicitTypeVars structs sigs functors overloadingClasses (updateInfoComplete complete info)
  | updateIComplete _ _ = raise EH.DeadBranch ""
fun updateInfoTypeNames infoTypeNames (ENV_CONS {valueIds, typeNames, explicitTypeVars, structs, sigs, functors, overloadingClasses, info as {lab, complete, infoTypeNames = _, argOfFunctor}}) =
    consEnvConstructor valueIds typeNames explicitTypeVars structs sigs functors overloadingClasses (consInfo lab complete infoTypeNames argOfFunctor)
  | updateInfoTypeNames _ _ = raise EH.DeadBranch ""
fun updateIArgOfFunctor argOfFunctor (ENV_CONS {valueIds, typeNames, explicitTypeVars, structs, sigs, functors, overloadingClasses, info}) =
    consEnvConstructor valueIds typeNames explicitTypeVars structs sigs functors overloadingClasses (updateInfoArgOfFunctor argOfFunctor info)
  | updateIArgOfFunctor argOfFunctor (ROW_ENV (env1, env2)) = ROW_ENV (updateIArgOfFunctor argOfFunctor env1, updateIArgOfFunctor argOfFunctor env2)
  | updateIArgOfFunctor argOfFunctor (LOCAL_ENV (env1, env2)) = LOCAL_ENV (env1, updateIArgOfFunctor argOfFunctor env2)
  | updateIArgOfFunctor _ env = env

fun projValueIds idG = updateValueIds idG emptyEnv

(* constructs an empty env with typeNames *)
fun consEnvTypeNames idT = updateTypeNames idT emptyEnv

fun projExplicitTypeVars idV = updateExplicitTypeVars idV emptyEnv
fun projStructs idS = updateStructs idS emptyEnv
fun projSigs idS = updateSigs idS emptyEnv
fun projFunctors idF = updateFunctors idF emptyEnv
fun projOverloadingClasses idD = updateOverloadingClasses idD emptyEnv

fun projOpns idO = ENVOPN idO


fun isEmptyIdEnv idenv = envOrdMap.numItems idenv = 0

fun isEmptyVarEnv varenv = isEmptyIdEnv varenv
fun isEmptyTypeVarEnv typeVarEnv = isEmptyIdEnv typeVarEnv
fun isEmptyStrEnv strenv = isEmptyIdEnv strenv
fun isEmptyOpenEnv openEnv = OMO.isEmpty openEnv

(* tests whether an env is empty *)
fun isEmptyEnv (env as ENV_CONS _) =
    isEmptyIdEnv (getValueIds env) andalso
    isEmptyIdEnv (getTypeNameEnv env) andalso
    isEmptyIdEnv (getExplicitTypeVars env) andalso
    isEmptyIdEnv (getStructs env) andalso
    isEmptyIdEnv (getSigs env) andalso
    isEmptyIdEnv (getFunctors env) andalso
    isEmptyIdEnv (getOverloadingClasses env)
  | isEmptyEnv (ENVDEP extenv) = isEmptyEnv (EL.getExtLabT extenv)
  | isEmptyEnv (ROW_ENV (env1, env2)) = isEmptyEnv env1 andalso isEmptyEnv env2
  | isEmptyEnv env = false

fun addenv  (v, semty) env = envOrdMap.insert (env,       v, semty)

(* a function which will take an key and a value and construct a new map for envs, with a mapping of that key to that value inside it *)
fun consSingleEnv (v, semty)     = envOrdMap.insert (envOrdMap.empty, v, semty)

fun findenv id env = envOrdMap.find (env, id)

fun remenv id env = #1 (envOrdMap.remove (env, id)) handle LibBase.NotFound => env

fun mapenv fmap env = envOrdMap.map fmap env

(* unionEnvList is a function which unions a list of environments *)
fun unionEnvList envList =
    foldr (fn (env1, env2) => envOrdMap.unionWith     (* unionWith returns a map that is the union of two maps *)
				 (fn (x, y) => x @ y) (* used to define the map on elements that are in the domain of both maps *)
				 (env1, env2))        (* the two maps that are to be unioned *)
	  emptyMap (* the empty map (from the ORD_MAP signature defined in the smlnj library *)
	  envList     (* a list of maps to union, which foldr operates on *)

(*fun outenv env dom = I.foldr (fn (x, env) => remenv x env) env dom
fun inenv  env dom = I.foldr (fn (x, cenv) =>
				 case envOrdMap.find (env, x) of
				     NONE => cenv
				   | SOME y => envOrdMap.insert (cenv, x, y)) emptyMap dom*)
fun plusenv env1 env2 = envOrdMap.unionWith (fn (_, y) => y) (env1, env2)

fun foldrenv  ffold init env = envOrdMap.foldr  ffold init env
fun foldlenv  ffold init env = envOrdMap.foldl  ffold init env
fun foldrienv ffold init env = envOrdMap.foldri ffold init env
fun foldlienv ffold init env = envOrdMap.foldli ffold init env

fun appenv  fmap env = envOrdMap.app  fmap env
fun appienv fmap env = envOrdMap.appi fmap env

fun dom  env  = envOrdMap.foldri (fn (l, _, set) => I.add l set) I.empty env
fun doms envl = foldr (fn (x, y) => I.union (dom x) y) I.empty envl

fun plusproj env id = case findenv id env of NONE => [] | SOME x => x

fun bindToEnv binds =
    List.foldr (fn (x, genenv) => unionEnvList [consSingleEnv (C.getBindI (EL.getExtLabT x), [x]), genenv]) emptyMap binds

fun envToBind env = foldrenv (fn ([x], binds) => x :: binds
			       | _ => raise EH.DeadBranch "")
			     []
			     env

fun envsToSeq [] = emptyEnv
  | envsToSeq [env] = env
  | envsToSeq (env :: envs) =
    if isEmptyEnv env
    then envsToSeq envs
    else ROW_ENV (env, envsToSeq envs)

val foldlOEnv = OMO.foldl
fun appOEnv fapp env = OMO.app fapp env
fun addOEnv x env = OMO.enqueue (env, x)
fun singOEnv x = addOEnv x emopn
fun uOEnv envl =
    foldl (fn (env1, env2) => OMO.foldl (fn (x, env) => addOEnv x env) env2 env1)
	  emopn
	  envl

fun uenvEnvInfo {lab = lab1, complete = complete1, infoTypeNames = infoTypeNames1, argOfFunctor = argOfFunctor1}
		{lab = lab2, complete = complete2, infoTypeNames = infoTypeNames2, argOfFunctor = argOfFunctor2} =
    consInfo lab2 (*(2010-04-06)Why do we keep the second one only?*)
	     (complete1 andalso complete2) (* is complete if both are complete *)
	     (infoTypeNames1 @ infoTypeNames2)
	     (argOfFunctor1 orelse argOfFunctor2) (* is the argument of a functor if at least one is *)

fun uenvEnvC (ENV_CONS {valueIds = valueIds1, typeNames = typeNameEnv1, explicitTypeVars = explicitTypeVars1, structs = structs1,
		      sigs = sigs1, functors = functors1, overloadingClasses = overloadingClasses1, info = info1})
	     (ENV_CONS {valueIds = valueIds2, typeNames = typeNameEnv2, explicitTypeVars = explicitTypeVars2, structs = structs2,
		      sigs = sigs2, functors = functors2, overloadingClasses = overloadingClasses2, info = info2}) =
	 consEnvConstructor (unionEnvList [valueIds1, valueIds2])
	     (unionEnvList [typeNameEnv1, typeNameEnv2])
	     (unionEnvList [explicitTypeVars1, explicitTypeVars2])
	     (unionEnvList [structs1, structs2])
	     (unionEnvList [sigs1, sigs2])
	     (unionEnvList [functors1, functors2])
	     (unionEnvList [overloadingClasses1, overloadingClasses2])
	     (uenvEnvInfo info1 info2)
  | uenvEnvC x y =
    if isEmptyEnv x
    then y
    else if isEmptyEnv y
    then x
    else ROW_ENV (x, y)

(* union of an env *)
fun unionEnv [] = emptyEnv
  | unionEnv ((ROW_ENV (env1, env2)) :: xs) = unionEnv (env1 :: env2 :: xs)
  | unionEnv [x] = x
  | unionEnv (x :: xs) = uenvEnvC x (unionEnv xs)

fun closeValueIds valueIds clos =
    mapenv (fn sem => map (fn x => EL.mapExtLab x (fn x => C.closeBind x clos)) sem)
	   valueIds

fun plusEnv (env1 as ENV_CONS _) (env2 as ENV_CONS _) =
    let val valueIds = plusenv (getValueIds env1) (getValueIds env2)
	val typeNames = plusenv (getTypeNameEnv env1) (getTypeNameEnv env2)
	val explicitTypeVars = plusenv (getExplicitTypeVars env1) (getExplicitTypeVars env2)
	val structs = plusenv (getStructs env1) (getStructs env2)
	val sigs = plusenv (getSigs env1) (getSigs env2)
	val functors = plusenv (getFunctors env1) (getFunctors env2)
	val overloadingClasses = plusenv (getOverloadingClasses env1) (getOverloadingClasses env2)
	val info = uenvEnvInfo (getInfo env1) (getInfo env2)
	val env = consEnvConstructor valueIds typeNames explicitTypeVars structs sigs functors overloadingClasses info
    in env
    end
  | plusEnv (ROW_ENV (env1, env2)) env3 = ROW_ENV (env1, plusEnv env2 env3)
  | plusEnv env1 (ROW_ENV (env2, env3)) = ROW_ENV (plusEnv env1 env2, env3)
  | plusEnv env1 env2 =
    if isEmptyEnv env1
    then env2
    else if isEmptyEnv env2
    then env1
    else ROW_ENV (env1, env2)

fun pushExtIdEnv idenv labs stts deps(* f*) =
    mapenv (fn sem => map (fn bind => EL.updExtLab bind(* (EL.mapExtLab bind (fn bind => C.mapBind bind (fn x => f x labs stts deps)))*)
						labs
						stts
						deps)
			  sem)
	   idenv

fun pushExtEnv (env as ENV_CONS _) labs stts deps =
    if isEmptyEnv env
    then ENVDEP (env, labs, stts, deps)
    else let val valueIds = pushExtIdEnv (getValueIds env) labs stts deps(* dumPush*)
	     val typeNames = pushExtIdEnv (getTypeNameEnv env) labs stts deps(* dumPush*)
	     val explicitTypeVars = pushExtIdEnv (getExplicitTypeVars env) labs stts deps(* dumPush*)
	     val structs = pushExtIdEnv (getStructs env) labs stts deps(* pushExtEnv*)
	     val sigs = pushExtIdEnv (getSigs env) labs stts deps(* pushExtEnv*)
	     val functors = pushExtIdEnv (getFunctors env) labs stts deps(* pushExtFunEnv*)
	     val overloadingClasses = pushExtIdEnv (getOverloadingClasses env) labs stts deps(* dumPush*)
	 in consEnvConstructor valueIds typeNames explicitTypeVars structs sigs functors overloadingClasses (getInfo env)
	 end
  | pushExtEnv (ROW_ENV (env1, env2)) labs stts deps =
    ROW_ENV (pushExtEnv env1 labs stts deps, pushExtEnv env2 labs stts deps)
  (*(2010-06-09)NOTE: we shouldn't need to push onto env1 because this env
   * should be useless. *)
  | pushExtEnv (ENVDEP (env, labs0, stts0, deps0)) labs stts deps =
    pushExtEnv env (L.union labs0 labs) (L.union stts0 stts) (CD.union deps0 deps)
  | pushExtEnv env labs stts deps = ENVDEP (env, labs, stts, deps)

val emptyContextSensitiveSyntaxError = []
val emptyConstraint = CONSTRAINTS OMC.empty

fun getcstSemi cst i = case OMC.find (cst, i) of NONE => [] | SOME x => x

fun consConstraint (v, c) (CONSTRAINTS cst) = CONSTRAINTS (OMC.insert (cst, L.toInt v, c :: (getcstSemi cst (L.toInt v))))
fun conscsss cl css = cl @ css

(* this adds a list of constraints (cs) to cst using the label as the key
 * if constraints have already been added for a label, these new constraints are added to the original constraints *)
fun conscsts (v, cs) (CONSTRAINTS cst) = CONSTRAINTS (OMC.insert (cst, L.toInt v, cs @ (getcstSemi cst (L.toInt v))))

fun singcss  c  = [c]
fun singcsss cs = cs
fun singleConstraint (v, c) = consConstraint  (v, c)  emptyConstraint
fun singcsts (v, cs) = conscsts (v, cs) emptyConstraint

(* unions a list of context sensitive syntax errors *)
fun unionContextSensitiveSyntaxErrors xs = foldr (fn (x, y) => x@y) emptyContextSensitiveSyntaxError xs

(* unionConstraints will union two CONSTRAINTS values (will union two lists of constraints)*)
fun unionConstraints (CONSTRAINTS cst1) (CONSTRAINTS cst2) = CONSTRAINTS (OMC.unionWith (fn (x, y) => x @ y) (cst1, cst2))

(* unionConstraintsList will take a list of CONSTRAINTS values, and use unionConstraints to union the values *)
fun unionConstraintsList xs = foldr (fn (x, y) => unionConstraints x y) emptyConstraint xs

fun foldlicst ffold init (CONSTRAINTS cst) = OMC.foldli ffold init cst
fun foldricst ffold init (CONSTRAINTS cst) = OMC.foldri ffold init cst
fun mapicst   ffold      (CONSTRAINTS cst) = CONSTRAINTS (OMC.mapi ffold cst)

fun getnbcst' cst =
    foldlicst (fn (_, cs, nb) =>
		  foldl (fn (TYPE_CONSTRAINT _, nb) => 1 + nb
			  | (TYPENAME_CONSTRAINT _, nb) => 1 + nb
			  | (ROW_CONSTRAINT _, nb) => 1 + nb
			  | (EQUALITY_TYPE_CONSTRAINT _, nb) => 1 + nb
			  | (FIELD_CONSTRAINT _, nb) => 1 + nb
			  | (LABEL_CONSTRAINT _, nb) => 1 + nb
			  | (IDENTIFIER_CLASS_CONSTRAINT _, nb) => 1 + nb
			  | (FUNCTION_TYPE_CONSTRAINT _, nb) => 1 + nb
			  | (ACCESSOR_CONSTRAINT _, nb) => 1 + nb
			  | (SIGNATURE_CONSTRAINT _, nb) => 1 + nb
			  | (FUNCTOR_CONSTRAINT _, nb) => 1 + nb
			  | (SHARING_CONSTRAINT _, nb) => 1 + nb
			  | (ENV_CONSTRAINT ((env1, env2), _, _, _), nb) => (getnbcstenv env1) + (getnbcstenv env2) + nb
			  | (LET_CONSTRAINT env, nb) => (getnbcstenv env) + nb)
			nb
			cs)
	      0
	      cst

and getnbcstenv (ENV_CONS _) = 0
  | getnbcstenv (ENV_VAR _) = 0
  | getnbcstenv (ROW_ENV (env1, env2)) = (getnbcstenv env1) + (getnbcstenv env2)
  | getnbcstenv (LOCAL_ENV (env1, env2)) = (getnbcstenv env1) + (getnbcstenv env2)
  | getnbcstenv (ENVSHA (env1, env2)) = (getnbcstenv env1) + (getnbcstenv env2)
  | getnbcstenv (SIGNATURE_ENV (env1, env2, _)) = (getnbcstenv env1) + (getnbcstenv env2)
  | getnbcstenv (ENVWHR (env, _)) = getnbcstenv env
  | getnbcstenv (ENVPOL (_, env)) = getnbcstenv env
  | getnbcstenv (DATATYPE_CONSTRUCTOR_ENV (_, env)) = getnbcstenv env
  | getnbcstenv (ENVOPN _) = 0
  | getnbcstenv (ENVDEP eenv) = getnbcstenv (EL.getExtLabT eenv)
  | getnbcstenv (FUNCTOR_ENV cst)  = getnbcst' cst
  | getnbcstenv (CONSTRAINT_ENV cst)  = getnbcst' cst
  | getnbcstenv (ENVPTY _) = 0
  | getnbcstenv (ENVFIL (file, env, strm)) = (getnbcstenv env) + (getnbcstenv (strm ()))
  | getnbcstenv ENVTOP = 0

fun getnbcst (env, css) = getnbcstenv env

fun getnbcsttop (env, css) = getnbcstenv env (* NOTE: this not useful anymore *)

fun getnbcss' css = List.length css

fun getnbcss (envc, css) = getnbcss' css

fun getnbcs (env, css) = (getnbcstenv env) + (getnbcss' css)

(* we get the labels at the csbindings *)

fun getlabsidenv idenv =
    envOrdMap.foldr
	(fn (sem, labs) => L.union (L.ord (map (fn (x, _, _, _) => C.getBindL x) sem)) labs)
	L.empty
	idenv

fun getlabopenEnv openEnv =
    OMO.foldl (fn ((_, lab, _), set) => L.cons lab set) L.empty openEnv

fun combineLabsEnv (outlabs1, inlabs1) (outlabs2, inlabs2) =
    (outlabs1 @ outlabs2, L.union inlabs1 inlabs2)

fun getlabsenv (env as ENV_CONS _) =
    let val labsValueIds = getlabsidenv (getValueIds env)
	val labsTyps = getlabsidenv (getTypeNameEnv env)
	val labsExplicitTypeVars = getlabsidenv (getExplicitTypeVars env)
	val labsStructs = getlabsidenv (getStructs env)
	val labsSigs = getlabsidenv (getSigs env)
	val labsFunctors = getlabsidenv (getFunctors env)
	val labsOverloadingClasses = getlabsidenv (getOverloadingClasses env)
	(*(2010-04-06)Functors and type variables ?*)
    in ([L.unions [labsValueIds, labsTyps, labsExplicitTypeVars, labsStructs, labsSigs, labsFunctors, labsOverloadingClasses]], L.empty)
    end
  | getlabsenv (ROW_ENV (env1, env2))    = combineLabsEnv (getlabsenv env1) (getlabsenv env2)
  | getlabsenv (LOCAL_ENV (env1, env2))    = combineLabsEnv (getlabsenv env1) (getlabsenv env2)
  | getlabsenv (ENVSHA (env1, env2))    = combineLabsEnv (getlabsenv env1) (getlabsenv env2)
  | getlabsenv (SIGNATURE_ENV (env1, env2, _)) = combineLabsEnv (getlabsenv env1) (getlabsenv env2)
  | getlabsenv (ENVWHR (env, _))        = getlabsenv env (*Don't we want to get the label of the longTypeConsBinder?*)
  | getlabsenv (ENVPOL (typeVarEnv, env))   = combineLabsEnv ([getlabsidenv typeVarEnv], L.empty) (getlabsenv env)
  | getlabsenv (DATATYPE_CONSTRUCTOR_ENV (labelledId, env))      = getlabsenv env
  | getlabsenv (ENVOPN openEnv)          = ([getlabopenEnv openEnv], L.empty)
  | getlabsenv (ENVDEP eenv)            = getlabsenv (EL.getExtLabT eenv)
  | getlabsenv (FUNCTOR_ENV cst)             = getlabscst cst
  | getlabsenv (CONSTRAINT_ENV cst)             = getlabscst cst
  | getlabsenv (ENV_VAR _)               = ([], L.empty)
  | getlabsenv (ENVPTY _)               = ([], L.empty)
  | getlabsenv (ENVFIL (f, env, strm))  = combineLabsEnv (getlabsenv env) (getlabsenv (strm ()))
  | getlabsenv ENVTOP                   = ([], L.empty)
and getlabcsbindocst (TYPE_CONSTRAINT _)   = ([], L.empty)
  | getlabcsbindocst (FUNCTION_TYPE_CONSTRAINT _)   = ([], L.empty)
  | getlabcsbindocst (TYPENAME_CONSTRAINT _)   = ([], L.empty)
  | getlabcsbindocst (ROW_CONSTRAINT _)   = ([], L.empty)
  | getlabcsbindocst (EQUALITY_TYPE_CONSTRAINT _)   = ([], L.empty)
  | getlabcsbindocst (FIELD_CONSTRAINT _)   = ([], L.empty)
  | getlabcsbindocst (LABEL_CONSTRAINT _)   = ([], L.empty)
  | getlabcsbindocst (ENV_CONSTRAINT _)   = ([], L.empty)
  | getlabcsbindocst (IDENTIFIER_CLASS_CONSTRAINT _)   = ([], L.empty)
  | getlabcsbindocst (ACCESSOR_CONSTRAINT _)   = ([], L.empty)
  | getlabcsbindocst (LET_CONSTRAINT env) = getlabsenv env
  | getlabcsbindocst (SIGNATURE_CONSTRAINT _)   = ([], L.empty)
  | getlabcsbindocst (FUNCTOR_CONSTRAINT _)   = ([], L.empty)
  | getlabcsbindocst (SHARING_CONSTRAINT _)   = ([], L.empty)
and getlabscst cst =
    foldricst
	(fn (_, ocstl, labsenv) =>
	    foldr (fn (oneConstraint, labsenv) => combineLabsEnv (getlabcsbindocst oneConstraint) labsenv)
		  labsenv
		  ocstl)
	([], L.empty)
	cst
and getbindings env = getlabsenv env

(* ====== Generation of type constraints ====== *)

fun genCstAllGen x1 x2 labs sts cds = EL.consExtLab (x1, x2) labs sts cds
fun genCstTyAll x1 x2 labs sts cds = TYPE_CONSTRAINT (genCstAllGen x1 x2 labs sts cds)
fun genCstTfAll x1 x2 labs sts cds = FUNCTION_TYPE_CONSTRAINT (genCstAllGen x1 x2 labs sts cds)
fun genCstEqAll x1 x2 labs sts cds = EQUALITY_TYPE_CONSTRAINT (genCstAllGen x1 x2 labs sts cds)
fun genCstTnAll x1 x2 labs sts cds = TYPENAME_CONSTRAINT (genCstAllGen x1 x2 labs sts cds)
fun genCstSqAll x1 x2 labs sts cds = ROW_CONSTRAINT (genCstAllGen x1 x2 labs sts cds)
fun genCstRtAll x1 x2 labs sts cds = FIELD_CONSTRAINT (genCstAllGen x1 x2 labs sts cds)
fun genCstLtAll x1 x2 labs sts cds = LABEL_CONSTRAINT (genCstAllGen x1 x2 labs sts cds)
fun genCstEvAll x1 x2 labs sts cds = ENV_CONSTRAINT (genCstAllGen x1 x2 labs sts cds)
fun genCstClAll x1 x2 labs sts cds = IDENTIFIER_CLASS_CONSTRAINT (genCstAllGen x1 x2 labs sts cds)

fun genValueIDAccessor x labs sts cds = VALUEID_ACCESSOR (EL.consExtLab x labs sts cds)

(* initTypeConstraint - return type oneConstraint
 * x1 and x2 here are of type T.ty, lab is of type Label.labels
 * x1 appears to be the type variable that you are constraing
 * x2 appears to be what you are constraining x1 to be
 *
*)
fun initTypeConstraint x1 x2 lab = (TYPE_CONSTRAINT (EL.initExtLab (x1, x2) lab))
fun initFunctionTypeConstraint x1 x2 lab = FUNCTION_TYPE_CONSTRAINT (EL.initExtLab (x1, x2) lab)

(* I don't see anywhere where this is used. Why does this exist? *)
fun initTypenameConstraint x1 x2 lab = TYPENAME_CONSTRAINT (EL.initExtLab (x1, x2) lab)
fun initRowConstraint x1 x2 lab = ROW_CONSTRAINT (EL.initExtLab (x1, x2) lab)
fun initEqualityTypeConstraint x1 x2 lab = EQUALITY_TYPE_CONSTRAINT (EL.initExtLab (x1, x2) lab)
fun initFieldConstraint x1 x2 lab = FIELD_CONSTRAINT (EL.initExtLab (x1, x2) lab)
fun initLabelConstraint x1 x2 lab = LABEL_CONSTRAINT (EL.initExtLab (x1, x2) lab)
fun initEnvConstraint x1 x2 lab = ENV_CONSTRAINT (EL.initExtLab (x1, x2) lab)
fun initClassConstraint x1 x2 lab = IDENTIFIER_CLASS_CONSTRAINT (EL.initExtLab (x1, x2) lab)

fun initValueIDAccessor x lab = VALUEID_ACCESSOR (EL.initExtLab x lab)
fun genAccIeEm x lab = EXPLICIT_TYPEVAR_ACCESSOR (EL.initExtLab x lab)
fun initEqualityTypeAccessor x lab = EQUALITY_TYPE_ACCESSOR (EL.initExtLab x lab)
fun genAccItEm x lab = TYPE_CONSTRUCTOR_ACCESSOR (EL.initExtLab x lab)
fun genAccIoEm x lab = OVERLOADING_CLASSES_ACCESSOR (EL.initExtLab x lab)
fun genAccIsEm x lab = STRUCTURE_ACCESSOR (EL.initExtLab x lab)
fun genAccIiEm x lab = SIGNATURE_ACCESSOR (EL.initExtLab x lab)
fun genAccIfEm x lab = FUNCTOR_ACCESSOR (EL.initExtLab x lab)

fun isMonoBind bind = P.isMono (getBindP bind)

fun toMonoValueIds valueIds labs =
    mapenv (fn sems => map (fn extlab => EL.mapExtLab extlab (fn x => C.toMonoBind x (L.cons (getBindL extlab) labs))) sems)
	   valueIds

fun toPolyValueIds valueIds =
    mapenv (fn sems => map (fn extlab => EL.mapExtLab extlab (fn x => C.toPolyBind x)) sems)
	   valueIds

fun toRECValueIds valueIds labs =
    mapenv (fn sems => map (fn x => EL.updExtLab (EL.mapExtLab x C.toREC) labs L.empty CD.empty) sems)
	   valueIds

fun toPATValueIds valueIds labs =
    mapenv (fn sems => map (fn x => EL.updExtLab (EL.mapExtLab x C.toPAT) labs L.empty CD.empty) sems)
	   valueIds

fun toEX0ValueIds valueIds labs =
    mapenv (fn sems => map (fn x => EL.updExtLab (EL.mapExtLab x C.toEX0) labs L.empty CD.empty) sems)
	   valueIds

fun toEX1ValueIds valueIds labs =
    mapenv (fn sems => map (fn x => EL.updExtLab (EL.mapExtLab x C.toEX1) labs L.empty CD.empty) sems)
	   valueIds

fun toDA0ValueIds valueIds labs =
    mapenv (fn sems => map (fn x => EL.updExtLab (EL.mapExtLab x C.toDA0) labs L.empty CD.empty) sems)
	   valueIds

fun toDA1ValueIds valueIds labs =
    mapenv (fn sems => map (fn x => EL.updExtLab (EL.mapExtLab x C.toDA1) labs L.empty CD.empty) sems)
	   valueIds

fun toDATValueIds valueIds labs =
    mapenv (fn sems => map (fn x => EL.updExtLab (EL.mapExtLab x C.toDAT) labs L.empty CD.empty) sems)
	   valueIds

fun toCLSValueIds valueIds cls labs =
    mapenv (fn sems => map (fn x => EL.updExtLab (EL.mapExtLab x (fn x => C.toCLS x cls)) labs L.empty CD.empty) sems)
	   valueIds

(* We have DAT here because this is only used for datatypes and datatype descriptions. *)
(* WARNING: typeNames here may not be typeNames! It's this env that's been going around! *)
fun toTYCONTypeNameEnv typeNames cons b labs =
    let fun mapbind x = C.mapBind x (fn (tyf, _, _) => (tyf, DATATYPE, ref (cons, b)))
    in mapenv (fn sems => map (fn x => EL.updExtLab (EL.mapExtLab x mapbind) labs L.empty CD.empty) sems)
	      typeNames
    end

fun allEqualValueIds valueIds =
    let val ty = T.newTYPE_VAR ()
    in foldrenv (fn (sems, cst) =>
		    foldr (fn (bind, cst) =>
			      let val ty' = getBindT bind
				  val lab = getBindL bind
				  val c   = initTypeConstraint ty ty' lab
			      in consConstraint (lab, c) cst
			      end)
			  cst
			  sems)
		emptyConstraint
		valueIds
    end

(* Generates an env from a long identifier and a type function *)
fun genLongEnv (I.ID (id, lab)) tyfun =
    let val tfv  = T.freshTypeFunctionVar ()
	val eqtv  = T.freshEqualityTypeVar ()
	val c    = initFunctionTypeConstraint (T.consTYPE_FUNCTION_VAR tfv) tyfun lab
	val typeNames = consSingleEnv (id, [consBindPoly
						{id=id,
						 typeOfId=(T.consTYPE_FUNCTION_VAR tfv, TYPE, ref (emvar, false)),
						 equalityTypeVar = eqtv,
						 classOfId=(CL.consTYCON ()),
						 labelOfConstraint=lab}])
    in (singleConstraint (lab, c), consEnvTypeNames typeNames)
    end
  | genLongEnv (I.LID ((id, lab1), lid, lab2)) tyfun =
    let val (cst, env1) = genLongEnv lid tyfun
	val ev1  = freshEnvVar ()
	val ev2  = freshEnvVar ()
	val c1   = initEnvConstraint (consENV_VAR ev1 lab1) env1 lab1
	val c2   = initEnvConstraint (consENV_VAR ev2 lab2) (consENV_VAR ev1 lab2) lab2
	val structs = consSingleEnv (id, [consBindPoly
					      {id=id,
					       typeOfId=(consENV_VAR ev2 lab1),
					       equalityTypeVar = T.freshEqualityTypeVar(),
					       classOfId=(CL.consSTR ()),
					       labelOfConstraint=lab1}])
    in (consConstraint (lab2, c2) (consConstraint (lab1, c1) cst), projStructs structs)
    end


(* Checks whether the env is composed by at least an env variable. *)
fun hasEnvVar (ENV_VAR _)            = true
  | hasEnvVar (ROW_ENV (env1, env2)) = hasEnvVar env2 orelse hasEnvVar env1
  | hasEnvVar (ENVDEP extenv)       = hasEnvVar (EL.getExtLabT extenv)
  | hasEnvVar _                     = false


(* Checks the second element in infoEnv (complete env). *)
(* Should we check the label as well? *)
(* true if a the env is complete: checks the second element in infoEnv
 * of an ENVC (false if not a ENVC). *)
fun completeEnv (env as ENV_CONS _)     = getIComplete env
  | completeEnv (ROW_ENV (env1, env2)) = completeEnv env2 andalso completeEnv env1
  | completeEnv (ENVDEP extenv)       = completeEnv (EL.getExtLabT extenv)
  | completeEnv  _                    = false

(*(* Marks an env as being incomplete. *)
fun toIncompleteEnv (env as ENV_CONS _) = updateIComplete false env
  | toIncompleteEnv x = x*)


fun isENV_VAR (ENV_VAR _) = true
  | isENV_VAR _ = false

(* Checks if the env is a ENV_CONS *)
fun isENV_CONS (ENV_CONS _) = true
  | isENV_CONS _ = false


(* Returns the labels of envs *)
(* n is just some debugging info *)
fun getLabsIdsGenEnv idenv =
    foldrienv (fn (id, semty, (lids, labs)) =>
		  foldr (fn (bind, (lids, labs)) =>
			    let val lab   = getBindL bind
				val lid   = (L.toInt lab, I.toInt id)
				val labs' = EL.getExtLabL bind
			    in (lid :: lids, L.union labs' labs)
			    end)
			(lids, labs)
			semty)
	      ([], L.empty)
	      idenv

(* Returns the ids (as integers) of an env along with the associated labels.
 * The int is a debugging info.
 * This function is only used by our unification algorithm (in Unification.sml). *)
fun getLabsIdsEnv (env as ENV_CONS _) n =
    let val (idlabsValueIds, labsValueIds) = getLabsIdsGenEnv (getValueIds env)
	val (idlabsTyps, labsTyps) = getLabsIdsGenEnv (getTypeNameEnv env)
	val (idlabsExplicitTypeVars, labsExplicitTypeVars) = getLabsIdsGenEnv (getExplicitTypeVars env)
	val (idlabsStructs, labsStructs) = getLabsIdsGenEnv (getStructs env)
	val (idlabsSigs, labsSigs) = getLabsIdsGenEnv (getSigs env)
	val (idlabsFunctors, labsFunctors) = getLabsIdsGenEnv (getFunctors env)
	val (idlabsOverloadingClasses, labsOverloadingClasses) = getLabsIdsGenEnv (getOverloadingClasses env)
    in (idlabsValueIds @ idlabsTyps @ idlabsExplicitTypeVars @ idlabsStructs @ idlabsSigs @ idlabsFunctors @ idlabsOverloadingClasses,
	L.unions [labsValueIds, labsTyps, labsExplicitTypeVars, labsStructs, labsSigs, labsFunctors, labsOverloadingClasses])
    end
  | getLabsIdsEnv _ _ = raise EH.DeadBranch "" (*([], L.empty)*)


(* Returns the label associated to a env (dummylab if not an ENV_CONS).
 * This function is only used for complete envs which are ENV_CONSs.
 * It is only used in Unification.sml.*)
fun getLabEnv (env as ENV_CONS _) = getILab env
  | getLabEnv _                 = raise EH.DeadBranch "" (*L.dummyLab*)




(* Extract the type names defined in a type constructor env *)
fun getTypeNames typeNames =
    foldrienv (fn (id, sem, typeNameMap) =>
		  List.foldr (fn (bind, typeNameMap) =>
				 let val (tf, typeNameKind, _) = getBindT bind
				     val lab = getBindL bind
				     val (names, labs, stts, deps) = T.isTypename tf
				 in case names of
					T.TYPENAME name =>
					let val labs  = L.union  labs (EL.getExtLabL bind)
					    val stts  = L.union  stts (EL.getExtLabE bind)
					    val deps  = CD.union deps (EL.getExtLabD bind)
					    val tname = {id = id, lab = lab, kind = typeNameKind, name = name}
					    val names = TYPENAME (tname, labs, stts, deps)
					in names :: typeNameMap
					end
				      | T.DUMTYPENAME name =>
					let val tname = {id = id, lab = lab, kind = typeNameKind, name = name}
					    val names = DUMTYPENAME tname
					in names :: typeNameMap
					end
				      | T.MAYTYPENAME => typeNameMap
				      | T.NOTTYPENAME =>
					let val labs  = L.union  labs (EL.getExtLabL bind)
					    val stts  = L.union  stts (EL.getExtLabE bind)
					    val deps  = CD.union deps (EL.getExtLabD bind)
					    val names = NOTTYPENAME (id, labs, stts, deps)
					in names :: typeNameMap
					end
				 end)
			     typeNameMap
			     sem)
	      []
	      typeNames

fun getEqualityTypeVars (CONSTRAINTS(constraints)) =
    let
	(* the list of constraints associated with each key *)
	val allConstraintValues = OMC.listItems constraints

	(* join all the lists of lists so that it's easier to search through them *)
	val singleConstraintList = List.foldl (op @) [] allConstraintValues

	fun findEqualityTypeVars [] = []
	  | findEqualityTypeVars (TYPE_CONSTRAINT((Ty.TYPE_VAR(tyv, _, _, _), Ty.TYPE_POLY _),_,_,_)::t) =
	    tyv::(findEqualityTypeVars t)
	  | findEqualityTypeVars (h::t) = findEqualityTypeVars t
    in
	findEqualityTypeVars singleConstraintList
    end

(* (2012-07-09) jpirie: I don't think that this is needed any more, removing this and the calls to it
 * in the ExpOp case of the constraint generation process *)
(* fun createEqualityTypeConstraints (CONSTRAINTS(constraints)) lab equalityValue = *)
(*     let *)
(* 	(* we shouldn't need allConstraintValues, singleConstraintList or makeTypeVarsEquality *)
(* 	 * keeping them during testing (2012-05-16) *) *)
(* 	(* the list of constraints associated with each key *) *)
(* 	val allConstraintValues = OMC.listItems constraints *)

(* 	(* join all the lists of lists so that it's easier to search through them *) *)
(* 	val singleConstraintList = List.foldl (op @) [] allConstraintValues *)

(* 	(* makes a TYPE_VAR an equality type but replacing the equality type status field with EQUALITY_TYPE *) *)
(* 	fun changeTypeVarsEquality (Ty.TYPE_VAR (tv,extv,poly,_)) equalityValue = Ty.TYPE_VAR(tv,extv,poly,equalityValue) *)
(* 	  | changeTypeVarsEquality x equalityValue  = x *)

(* 	(* finds equality type variables nested a 'ty' type *) *)
(* 	fun findEqualityTypeVars [] _ = [] *)
(* 	  | findEqualityTypeVars (TYPE_CONSTRAINT((Ty.TYPE_VAR(a, b, c, x), Ty.TYPE_POLY (d,e,f,g,h,_)),l1,l2,deps)::t) lab = *)
(* 	    (D.printDebugFeature D.ENV D.EQUALITY_TYPES (fn _ => "Creating equality constraint for type variable number "^(Int.toString (T.typeVarToInt a))); *)
(* 	     TYPE_CONSTRAINT((Ty.TYPE_VAR(a,b,c,x), Ty.TYPE_POLY(d,e,f,g,h,equalityValue)),(L.cons lab l1),l2,deps)::(findEqualityTypeVars t lab)) *)

(* 	  | findEqualityTypeVars (TYPE_CONSTRAINT((Ty.TYPE_VAR(a, b, c, _), Ty.TYPE_VAR (a2, b2, c2, _)),l1,l2,deps)::t) lab = *)
(* 	    (D.printDebugFeature D.ENV D.EQUALITY_TYPES (fn _ => "Creating equality constraint for type variable number "^(Int.toString (T.typeVarToInt a))); *)
(* 	     TYPE_CONSTRAINT((Ty.TYPE_VAR(a,b,c,equalityValue), Ty.TYPE_VAR(a2,b2,c2,equalityValue)),(L.cons lab l1),l2,deps)::(findEqualityTypeVars t lab)) *)

(* 	  | findEqualityTypeVars (ACCESSOR_CONSTRAINT (VALUEID_ACCESSOR({lid=lid,sem=sem,class=class,lab=label}, l1, l2, cd))::t) lab = *)
(* 	    (D.printDebugFeature D.ENV D.EQUALITY_TYPES (fn _ => "Creating equality constraint for an accessor"); *)
(* 	     ACCESSOR_CONSTRAINT(VALUEID_ACCESSOR({lid=lid,sem=(changeTypeVarsEquality sem equalityValue),class=class,lab=label},(L.cons lab l1),l2,cd))::(findEqualityTypeVars t lab)) *)
(* 	  | findEqualityTypeVars (h::t) lab = *)
(* 	    (D.printDebugFeature D.ENV D.EQUALITY_TYPES (fn _ => "WARNING: findEqualityTypeVars got something of a form that is not yet supported"); *)
(* 	     findEqualityTypeVars t lab) *)
(*     in *)
(* 	CONSTRAINTS (OMC.map (fn cs => (findEqualityTypeVars cs lab)) constraints) *)
(*     end *)


(* a function which will create NOT_EQUALITY_TYPE constaints for types used
 * in an opaque signature. Called at constraint generation time when we see
 * that we are in fact dealing with an opaque signature *)
fun createOpaqueEqualityConstraints env lab =
    let
	val equalityTypeVarsToFreshen = ref []

	fun freshenEqualityTypeVar eqtv =
	    let
		val newEqtv = T.freshEqualityTypeVar()
		val _ = equalityTypeVarsToFreshen := (eqtv,newEqtv)::(!(equalityTypeVarsToFreshen))
	    in
		newEqtv
	    end

	fun checkForFreshening eqtv =
	    let
		fun checkForFreshening' eqtv [] = eqtv
		  | checkForFreshening' eqtv ((old,new)::t) =
		    if (T.equalityTypeVarToInt eqtv = T.equalityTypeVarToInt old)
		    then new
		    else checkForFreshening' eqtv t
	    in
		checkForFreshening' eqtv (!(equalityTypeVarsToFreshen))
	    end

	fun createOpaqueEqualityConstraintsInEnv (FUNCTOR_ENV cst)      lab = FUNCTOR_ENV (topLevel cst lab)
	  | createOpaqueEqualityConstraintsInEnv (CONSTRAINT_ENV cst)   lab = CONSTRAINT_ENV(topLevel cst lab)
	  | createOpaqueEqualityConstraintsInEnv (ROW_ENV (env1, env2)) lab = ROW_ENV (createOpaqueEqualityConstraintsInEnv env1 lab, createOpaqueEqualityConstraintsInEnv env2 lab)
	  | createOpaqueEqualityConstraintsInEnv x                      lab = x

	and createOpaqueEqualityOneConstraint (ENV_CONSTRAINT ((env1, env2),x,y,z)) lab = (ENV_CONSTRAINT ((createOpaqueEqualityConstraintsInEnv env1 lab, createOpaqueEqualityConstraintsInEnv env2 lab), x,y,z))
	  | createOpaqueEqualityOneConstraint (currentConstraint as EQUALITY_TYPE_CONSTRAINT ((T.EQUALITY_TYPE_VAR eqtv1, T.EQUALITY_TYPE_VAR eqtv2), x, deps ,z)) lab =
					       if (T.equalityTypeVarToInt eqtv1) = (T.equalityTypeVarToInt eqtv2) andalso L.length deps > 0
					       then (D.printDebugFeature D.ENV D.EQUALITY_TYPES (fn _ => "Found eqtv used in opaque scope, freshening and constraining: "^(T.printEqualityTypeVar eqtv1));
						     EQUALITY_TYPE_CONSTRAINT((T.EQUALITY_TYPE_VAR (freshenEqualityTypeVar eqtv1), T.EQUALITY_TYPE_STATUS (T.NOT_EQUALITY_TYPE)), L.cons lab L.empty, L.empty, CD.empty))
					       else EQUALITY_TYPE_CONSTRAINT((T.EQUALITY_TYPE_VAR (checkForFreshening eqtv1), T.EQUALITY_TYPE_VAR (checkForFreshening eqtv2)), x, deps ,z)
	  | createOpaqueEqualityOneConstraint (currentConstraint as TYPE_CONSTRAINT((tv,T.TYPE_CONSTRUCTOR(v,w,x,T.EQUALITY_TYPE_VAR(eqtv))), y, deps ,z)) lab =
	    TYPE_CONSTRAINT((tv,T.TYPE_CONSTRUCTOR(v,w,x,T.EQUALITY_TYPE_VAR(checkForFreshening eqtv))), y, deps ,z)
	  | createOpaqueEqualityOneConstraint x lab = x

	and createOpaqueEqualityConstraintsList []     lab = []
	  | createOpaqueEqualityConstraintsList (h::t) lab = ((createOpaqueEqualityOneConstraint h lab)::(createOpaqueEqualityConstraintsList t lab))

	and topLevel (CONSTRAINTS cst) lab =
	    CONSTRAINTS(OMC.map (fn oneConstraint => createOpaqueEqualityConstraintsList oneConstraint lab) cst)

	fun getBinds term =
	    let
		val binds = List.map getBindT term
		val _ = List.map (fn x => print x) binds
	    in
		binds
	    end

	fun parseTypeNames typeNames = mapenv (fn x => getBinds x) typeNames

	fun parseEnv (test as ENV_CONS {valueIds,typeNames,explicitTypeVars,structs,sigs,functors,overloadingClasses,info}) =
	    let
		val binds = parseTypeNames typeNames
	    in
		ENV_CONS {valueIds=valueIds,typeNames=typeNames,explicitTypeVars=explicitTypeVars,structs=structs,sigs=sigs,functors=functors,overloadingClasses=overloadingClasses,info=info}
	    end
	  | parseEnv _ = raise EH.DeadBranch "Attempeted to create opaque equality type constraints with something that isn't an environment"
    in
	parseEnv env
    end

(* The set of labels is the set of labels that we want to keep *)

(* Same as in Filter *)
fun testlab lab labs = L.eq lab L.dummyLab
		       orelse L.eq lab L.builtinLab
		       orelse L.isin lab labs

fun testlabs labs1 labs2 = L.subseteq labs1 (L.cons L.dummyLab (L.cons L.builtinLab labs2))

fun filterIdEnv idenv labs =
    envOrdMap.foldri (fn (id, sem, (idenv, complete)) =>
		   let val (sem', complete') =
			   foldr (fn (bind, (sem, complete)) =>
				     if testlabs (EL.getExtLabL bind) labs
				     then (bind :: sem, complete)
				     else (sem, complete(*false*)))
				 ([], complete)
				 sem
		   in if List.null sem'
		      then (idenv, false)
		      else (addenv (id, sem') idenv, complete')
		   end)
	       (emptyMap, true)
	       idenv

fun filterOpenEnv openEnv labs =
    let val openEnv' =
	    OMO.foldr (fn (openSem as (lid, lab, kind), openEnv) =>
			  if testlab lab labs
			  then addOEnv openSem openEnv
			  else openEnv)
		      emopn
		      openEnv
    in if OMO.isEmpty openEnv'
       then NONE
       else SOME openEnv'
    end

fun filterLongTypeConsBinder (longTypeConsBinder as ({lid, equalityTypeVar, sem, class, lab}, _, _, _)) labs =
    if testlab lab labs
    then SOME longTypeConsBinder
    else NONE

fun toIncomplete (env as ENV_CONS _)               = updateIComplete false env
  | toIncomplete (env as ROW_ENV (env1, env2))    = ROW_ENV (toIncomplete env1, toIncomplete env2)
  | toIncomplete (env as ENV_VAR _)               = env
  | toIncomplete (env as LOCAL_ENV (env1, env2))    = ROW_ENV (env1, toIncomplete env2)
  | toIncomplete (env as ENVWHR (env1, longTypeConsBinder)) = ENVWHR (toIncomplete env1, longTypeConsBinder)
  | toIncomplete (env as ENVSHA (env1, env2))    = ENVSHA (toIncomplete env1, env2)
  | toIncomplete (env as SIGNATURE_ENV (env1, env2, l)) = SIGNATURE_ENV (toIncomplete env1, env2, l)
  | toIncomplete (env as ENVPOL (typeVarEnv, env1))  = ENVPOL (typeVarEnv, toIncomplete env1)
  | toIncomplete (env as DATATYPE_CONSTRUCTOR_ENV (labelledId, env1))     = DATATYPE_CONSTRUCTOR_ENV (labelledId, toIncomplete env1)
  | toIncomplete (env as ENVOPN _)               = env
  | toIncomplete (env as ENVDEP eenv)            = ENVDEP (EL.mapExtLab eenv toIncomplete)
  | toIncomplete (env as FUNCTOR_ENV _)               = env
  | toIncomplete (env as CONSTRAINT_ENV _)               = env
  | toIncomplete (env as ENVPTY _)               = env
  | toIncomplete (env as ENVFIL (f, e, strm))    = ENVFIL (f, toIncomplete e, fn () => toIncomplete (strm ()))
  | toIncomplete (env as ENVTOP)                 = env

fun filterOcst (oneConstraint as TYPE_CONSTRAINT _) labs = SOME oneConstraint
  | filterOcst (oneConstraint as TYPENAME_CONSTRAINT _) labs = SOME oneConstraint
  | filterOcst (oneConstraint as ROW_CONSTRAINT _) labs = SOME oneConstraint
  | filterOcst (oneConstraint as EQUALITY_TYPE_CONSTRAINT _) labs = SOME oneConstraint
  | filterOcst (oneConstraint as FIELD_CONSTRAINT _) labs = SOME oneConstraint
  | filterOcst (oneConstraint as LABEL_CONSTRAINT _) labs = SOME oneConstraint
  | filterOcst (oneConstraint as ENV_CONSTRAINT ((env1, env2), labs0, stts0, deps0)) labs =
    (case (filterEnv env1 labs, filterEnv env2 labs) of
	 (SOME env1', SOME env2') => SOME (ENV_CONSTRAINT ((env1', env2'), labs0, stts0, deps0))
       | (SOME env1', NONE)       => SOME (ENV_CONSTRAINT ((env1', emptyEnv), labs0, stts0, deps0))
       | (NONE,       SOME env2') => SOME (ENV_CONSTRAINT ((emptyEnv, env2'), labs0, stts0, deps0))
       | (NONE,       NONE)       => NONE)
  | filterOcst (oneConstraint as IDENTIFIER_CLASS_CONSTRAINT _) labs = SOME oneConstraint
  | filterOcst (oneConstraint as FUNCTION_TYPE_CONSTRAINT _) labs = SOME oneConstraint
  | filterOcst (oneConstraint as ACCESSOR_CONSTRAINT _) labs = SOME oneConstraint
  | filterOcst (oneConstraint as LET_CONSTRAINT env) labs =
    (case filterEnv env labs of
	 SOME env' => SOME (LET_CONSTRAINT env')
       | NONE      => NONE)
  | filterOcst (oneConstraint as SIGNATURE_CONSTRAINT _) labs = SOME oneConstraint
  | filterOcst (oneConstraint as FUNCTOR_CONSTRAINT _) labs = SOME oneConstraint
  | filterOcst (oneConstraint as SHARING_CONSTRAINT _) labs = SOME oneConstraint

and filterCst (CONSTRAINTS oneConstraint) labs =
    CONSTRAINTS (OMC.mapPartiali (fn (key, cs) =>
			      if testlab (L.fromInt key) labs
			      then case List.mapPartial (fn oneConstraint => filterOcst oneConstraint labs) cs of
				       []  => NONE
				     | cs' => SOME cs'
			      else NONE)
			  oneConstraint)

and filterEnv (env as ENV_CONS _) labs =
    let val (valueIds, completeValueIds) = filterIdEnv (getValueIds env) labs
	val (typeNames, completeTyps) = filterIdEnv (getTypeNameEnv env) labs
	val (explicitTypeVars, completeExplicitTypeVars) = filterIdEnv (getExplicitTypeVars env) labs
	val (structs, completeStructs) = filterIdEnv (getStructs env) labs
	val (sigs, completeSigs) = filterIdEnv (getSigs env) labs
	val (functors, completeFunctors) = filterIdEnv (getFunctors env) labs
	val (overloadingClasses, completeOverloadingClasses) = filterIdEnv (getOverloadingClasses env) labs
	val complete  = completeValueIds andalso completeTyps andalso completeExplicitTypeVars andalso
		   completeStructs andalso completeSigs andalso completeFunctors andalso
		   completeOverloadingClasses andalso getIComplete env
	val info = consInfo (getILab env) complete (getITypeNames env) (getIArgOfFunctor env)
	val env' = consEnvConstructor valueIds typeNames explicitTypeVars structs sigs functors overloadingClasses info
    in SOME env'
    end
  | filterEnv (env as ENV_VAR _) labs = SOME env
  | filterEnv (env as ROW_ENV (env1, env2)) labs =
    (case (filterEnv env1 labs, filterEnv env2 labs) of
	 (SOME env1', SOME env2') => SOME (ROW_ENV (env1', env2'))
       | (SOME env1', NONE)       => SOME env1'
       | (NONE,       SOME env2') => SOME env2'
       | (NONE,       NONE)       => NONE)
  | filterEnv (env as LOCAL_ENV (env1, env2)) labs =
    (case (filterEnv env1 labs , filterEnv env2 labs) of
	 (SOME env1', SOME env2') => SOME (LOCAL_ENV (env1', env2'))
       | (SOME env1', NONE)       => SOME (LOCAL_ENV (env1', updateIComplete false emptyEnv))
       | (NONE,       SOME env2') => SOME env2'
       | (NONE,       NONE)       => NONE)
  | filterEnv (env as ENVWHR (env1, longTypeConsBinder)) labs =
    (case (filterEnv env1 labs, filterLongTypeConsBinder longTypeConsBinder labs) of
	 (SOME env1', SOME longTypeConsBinder') => SOME (ENVWHR (env1', longTypeConsBinder'))
       | (SOME env1', NONE)          => SOME env1'
       | (NONE,       SOME longTypeConsBinder') => SOME (ENVWHR (updateIComplete false emptyEnv, longTypeConsBinder'))
       | (NONE,       NONE)          => NONE)
  | filterEnv (env as ENVSHA (env1, env2)) labs =
    (case (filterEnv env1 labs , filterEnv env2 labs) of
	 (SOME env1', SOME env2') => SOME (ENVSHA (env1', env2'))
       | (SOME env1', NONE)       => SOME env1'
       | (NONE,       SOME env2') => SOME (ENVSHA (updateIComplete false emptyEnv, env2'))
       | (NONE,       NONE)       => NONE)
  | filterEnv (env as SIGNATURE_ENV (env1, env2, kind)) labs =
    (case (filterEnv env1 labs , filterEnv env2 labs) of
	 (SOME env1', SOME env2') => SOME (SIGNATURE_ENV (env1', env2', kind))
       | (SOME env1', NONE)       => SOME (SIGNATURE_ENV (env1', updateIComplete false emptyEnv, kind))
       | (NONE,       SOME env2') => SOME (SIGNATURE_ENV (updateIComplete false emptyEnv, env2', kind))
       | (NONE,       NONE)       => NONE)
  | filterEnv (env as ENVPOL (typeVarEnv, env1)) labs =
    (case (filterIdEnv typeVarEnv labs , filterEnv env1 labs) of
	 ((typeVarEnv', _), SOME env1') => SOME (ENVPOL (typeVarEnv', env1'))
       | ((typeVarEnv', _), NONE)       =>
	 if isEmptyIdEnv typeVarEnv'
	 then NONE
	 else SOME (ENVPOL (typeVarEnv', updateIComplete false emptyEnv)))
  | filterEnv (env as DATATYPE_CONSTRUCTOR_ENV (labelledId as (id, lab), env1)) labs =
    (case filterEnv env1 labs of
	 SOME env1' => SOME (DATATYPE_CONSTRUCTOR_ENV (labelledId, env1'))
       | NONE       => if testlab lab labs
		       then SOME (DATATYPE_CONSTRUCTOR_ENV (labelledId, updateIComplete false emptyEnv))
		       else NONE)
  | filterEnv (env as ENVOPN openEnv) labs =
    (case filterOpenEnv openEnv labs of
	 SOME openEnv' => SOME (ENVOPN openEnv')
       | NONE         => NONE)
  | filterEnv (env as ENVDEP (env1, labs1, stts1, deps1)) labs =
    if testlabs labs1 labs
    then case filterEnv env1 labs of
	     SOME env1' => SOME (ENVDEP (env1', labs1, stts1, deps1))
	   | NONE       => NONE
    else NONE
  | filterEnv (env as FUNCTOR_ENV cst) labs =
    (case filterCst cst labs of
	 CONSTRAINTS ocsts => if OMC.numItems ocsts = 0
		       then NONE
		       else SOME (FUNCTOR_ENV (CONSTRAINTS ocsts)))
  | filterEnv (env as CONSTRAINT_ENV cst) labs =
    (case filterCst cst labs of
	 CONSTRAINTS ocsts => if OMC.numItems ocsts = 0
		       then NONE
		       else SOME (CONSTRAINT_ENV (CONSTRAINTS ocsts)))
  | filterEnv (env as ENVPTY _) _ = SOME env
  | filterEnv (env as ENVFIL (file, e, strm)) labs =
    (case (filterEnv e labs, filterEnv (strm ()) labs) of
	 (SOME env1, SOME env2) => SOME (ENVFIL (file, env1, fn () => env2))
       | (SOME env1, NONE)      => SOME (ENVFIL (file, env1, fn () => emptyEnv))
       | (NONE,      SOME env2) => SOME (ENVFIL (file, emptyEnv, fn () => env2))
       | (NONE,      NONE)      => NONE)
  | filterEnv (env as ENVTOP) _ = SOME env

(* Filters the constraintss that are not in the label set *)
val filterEnv = fn env => fn labs => case filterEnv env labs of NONE => updateIComplete false emptyEnv | SOME env' => env'

end
