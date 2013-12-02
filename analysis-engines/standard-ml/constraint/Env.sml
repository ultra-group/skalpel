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
 *)

(** Contains our constraint system. The file defines the structure Env with signature ENV. *)
structure Env :> ENV = struct

structure T   = Ty
structure I   = Id
structure X   = Expans
structure L   = Label
structure P   = Poly
structure C   = ConsId
structure D   = Debug
structure CL  = ClassId
structure CD  = LongId
structure EL  = ExtLab
structure EH  = ErrorHandler
structure envOrdMap = SplayMapFn (OrdId)
structure OMC = SplayMapFn (OrdKey)
structure OMO = Fifo

(** A constraint map. *)
type 'a constraintMap   = 'a OMC.map
(** An environment map. *)
type 'a envMap          = 'a envOrdMap.map
(** Open environment map. *)
type 'a openEnvMap      = 'a OMO.fifo

(** An environment variable, set to be an integer. *)
type envVar         = int

(** This is for what kind of bind indentifier we are binding. We have the:
 * - binding (the actual thing being bound, C.bind)
 * - class part of C.bind (the class which we're binding it in)
 * - label part of C.bind (everything is labelled)
 * - poly  part of C.bind (some polymorphic information?) *)
type 'a bind  = 'a C.bind EL.extLab

(** genericEnvironment
 * stores binders of structures and signatures and such
 * it is suspected that this was created as a speed optimisation
 * (2010-04-08)We should use genericEnvironment for all our similar environments*)
type 'a genericEnv  = 'a bind list envMap

(** An open environment datatype. Constructors are:
 * \arg OPENED_STRUCT For an opened structure.
 * \arg DATATYPE_REPLICATION For datatype replication.
 * \arg INCLUDED_SIG For included signatures. *)
datatype openKind    = OPENED_STRUCT | DATATYPE_REPLICATION | INCLUDED_SIG

(** A triple of the id of structure to open, a label of the structure to open, and the kind of an opening. *)
type openSem         = I.lid   * L.label * openKind

(** An open environment. *)
type openEnv         = openSem openEnvMap

(**(2010-06-10)The Boolean in Ty.ty bind to indicate if the varEnv is complete or not.*)
type extVar         = T.ty bind
type varEnv         = T.ty genericEnv

(** Kind of datatype replication. Can be DATATYPE or TYPE. *)
datatype typeNameKind = DATATYPE | TYPE

type extType         = (T.typeFunction * typeNameKind * (varEnv * bool) ref) bind
type typeEnv         = (T.typeFunction * typeNameKind * (varEnv * bool) ref) genericEnv

(** Onverloading environment. *)
type extovc                = T.rowType bind
(** Onverloading classes environment. *)
type overloadingClassesEnv = T.rowType genericEnv

(** Bool is true for an explicit type variable and false for an implicit one.*)
type explicitTypeVar = (T.typeVar * bool) bind
(** A type variable environment. *)
type typeVarEnv      = (T.typeVar * bool) genericEnv


type typeName    = {id : I.id, lab : L.label, kind : typeNameKind, name : T.typename}
(** This should be: tname extLab list *)
type typeNameMap = typeName list

(** Different sorts of typename, 4 constructors.
 * \arg TYPENAME. Definitely a typename.
 * \arg DUMTYPENAME. Maybe a typename defining a typename but not enough information to be sure.
 * \arg MAYTYPENAME. Maybe a typename defining but not enough information to be sure.
 * \arg NOTTYPENAME. Not a typename. *)
datatype names   = TYPENAME of typeName EL.extLab | DUMTYPENAME of typeName | MAYTYPENAME | NOTTYPENAME of I.id EL.extLab

(** An information environment, a record with 4 fields.
 * \arg lab. Should be a list or a set, see uenvEnv in Analyze.sml.
 * \arg complete. True if env is complete (true when initialised and can be false during unification).
 * \arg infoTypeNames. Type names introduced in the structure, these are here to ease the collection of type names when pushing an env onto a unification context.
 * \arg argOfFunctor. True if the environment is an argument of a functor. *)
type infoEnv     = {lab : L.label, complete : bool, infoTypeNames : typeNameMap, argOfFunctor : bool}

(** 1st envVar: instantiated signature
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

(** Functor instantiation. *)
type evfbind        = envVar * envVar * envVar * envVar * L.label

(** Sharing. *)
type shabind        = envVar * envVar * envVar * L.label

(** Represents a type of accessor - see datatype constructors ending in "_ACCESSOR" *)
type 'a accessorId       = {lid : I.lid, equalityTypeVar : T.equalityTypeVar, sem : 'a, class : CL.class, lab : L.label}

(** Long type constructor binder. *)
type longTypeConsBinder        = T.typeFunction accessorId EL.extLab

(** Same as #ClassId.class. *)
type class = CL.class

(** For denoting opaque or translucent signatures *)
datatype matchKind  = OPAQUE | TRANSLUCENT

(** A datatype holding all the different kinds of environment that we represent.
 * \arg ENV_CONS. A record of value identifiers, type names, explicit type variables, structures, signatures, functors, overloading classes, and additional information.
 * \arg ENV_VAR. An environment variable.
 * \arg ROW_ENV. A sequence environment.
 * \arg LOCAL_ENV. A local environment (used for the local sml feature).
 * \arg ENVWHR. A 'where' environment. (2010-07-07) We want something like: Ty.typeFunction accessorId extLab list, instead of the second env.
 * \arg SIGNATURE_ENV. Signature environments.
 * \arg ENVPOL The first environment is the explicit type variables and the second the value.
 * \arg ENVSHA. A sharing environment, used for type sharing.
 * \arg SIGNATURE_ENV. Used to represent signature environments.
 * \arg ENVPOL. The first env is the explicit type variables and the second the value bindings.
 * \arg DATATYPE_CONSTRUCTOR_ENV. This is to associate constructors to a datatype.
 * \arg ENVOPN. Used to represent the 'open' SML feature.
 * \arg ENVDEP. Represents dependent environments.
 * \arg FUNCTOR_ENV. Used to represent functors. This is only solved when dealing with CSTFUN.
 * \arg CONSTRAINT_ENV.
 * \arg ENVPTY. This is generated by a sml-tes special comment SML-TES-TYPE and used to print the type of an identifier.
 * \arg ENVFIL. Like a SEQ but the first env is from a file which has the name given by the string and the second env is a stream.
 * \arg TOP_LEVEL_ENV. To make that we have reached the top level of an environment.
 * \arg NO_DUPLICATE_ID. Used to indicate that we need to check inside envVar for duplicate identifier specifications.
 *)


 datatype env = ENV_CONS of {valueIds : varEnv,
			     typeNames : typeEnv,
			     explicitTypeVars : typeVarEnv,
			     structs : env genericEnv,
			     sigs : env genericEnv,
			     functors : (env * env) genericEnv,
			     overloadingClasses : overloadingClassesEnv,
			     info : infoEnv}
	      | ENV_VAR of envVar * L.label
	      | ROW_ENV of env * env
	      | LOCAL_ENV of env * env
	      | ENVWHR of env * longTypeConsBinder
	      | ENVSHA of env * env
	      | SIGNATURE_ENV of env * env * matchKind
	      | ENVPOL of typeVarEnv * env
	      | DATATYPE_CONSTRUCTOR_ENV of I.labelledId  * env
	      | ENVOPN of openEnv
	      | ENVDEP of env EL.extLab
	      | FUNCTOR_ENV of constraints
	      | CONSTRAINT_ENV of constraints
	      | ENVPTY of string
	      | ENVFIL of string * env * (unit -> env)
	      | TOP_LEVEL_ENV
	      | NO_DUPLICATE_ID

      (** Holds the different kinds of accessor we use.
       * \arg VALUEID_ACCESSOR. For accessors of value identifiers.
       * \arg EXPLICIT_TYPEVAR_ACCESSOR. Explicit type variable acessors.
       * \arg TYPE_CONSTRUCTOR_ACCESSOR. Accessors to type constructors.
       * \arg OVERLOADING_CLASSES_ACCESSORS. Accessors for overloading classes.
       * \arg STRUCTURE_ACESSOR. Accessors for structure identifiers.
       * \arg SIGNATURE_ACCESSOR. Accessors for signatures.
       * \arg FUNCTOR_ACCESSOR. Functor accessors. *)
     and accessor        = VALUEID_ACCESSOR of T.ty accessorId EL.extLab
			 | EXPLICIT_TYPEVAR_ACCESSOR of T.ty accessorId EL.extLab
			 | EQUALITY_TYPE_ACCESSOR of T.equalityType accessorId EL.extLab
			 | TYPE_CONSTRUCTOR_ACCESSOR of (T.typeFunction * bool) accessorId EL.extLab
			 | OVERLOADING_CLASSES_ACCESSOR of T.rowType accessorId EL.extLab
			 | STRUCTURE_ACCESSOR of env accessorId EL.extLab
			 | SIGNATURE_ACCESSOR of env accessorId EL.extLab
			 | FUNCTOR_ACCESSOR of (env * env) accessorId EL.extLab

     (** Holds different kinds of constraints.
      * \arg TYPE_CONSRTAINT. Constraint between two #Ty.ty values
      * \arg TYPENAME_CONSTRAINT. Constraint between #Ty.typenameType constraint.
      * \arg ROW_CONSTRAINT. Constraint between two #Ty.rowType values.
      * \arg FIELD_CONSTRAINT. Constraint between two #Ty.fieldType values.
      * \arg LABEL_CONSTRAINT. Constraint between two #Ty.labelType values.
      * \arg ENV_CONSTRAINT. Constraint between two environments.
      * \arg IDENTIFIER_CLASS_CONSTRAINT. Constraint between two #ClassId.class values.
      * \arg FUNCTION_TYPE_CONSTRAINT. Constraint between two #Ty.typeFunction values.
      * \arg ACCESSOR_CONSTRAINT. Constraint on an accessor.
      * \arg LET_CONSTRAINT. A constraint on a let environment.
      * \arg SIGNATURE_CONSTRAINT. A constraint on signatures.
      * \arg FUNCTOR_CONSTRAINT. A constraint on functors.
      * \arg SHARING_CONSTRAINT. A constraint for the type sharing feature of ML.
      * \arg EQUALITY_TYPE_CONSTRAINT. A constraint between two #Ty.equalityType values.
      *)
     and oneConstraint    = TYPE_CONSTRAINT     of (T.ty     * T.ty)     EL.extLab
			  | TYPENAME_CONSTRAINT of (T.typenameType   * T.typenameType)   EL.extLab
			  | ROW_CONSTRAINT of (T.rowType  * T.rowType)  EL.extLab
			  | FIELD_CONSTRAINT of (T.fieldType  * T.fieldType)  EL.extLab
			  | LABEL_CONSTRAINT of (T.labelType  * T.labelType)  EL.extLab
			  | ENV_CONSTRAINT of (env      * env)      EL.extLab
			  | IDENTIFIER_CLASS_CONSTRAINT of (CL.class * CL.class) EL.extLab
			  | FUNCTION_TYPE_CONSTRAINT of (T.typeFunction  * T.typeFunction)  EL.extLab
			  | ACCESSOR_CONSTRAINT of accessor
			  | LET_CONSTRAINT of env
			  | SIGNATURE_CONSTRAINT of evsbind
			  | FUNCTOR_CONSTRAINT of evfbind
			  | SHARING_CONSTRAINT of shabind
			  | EQUALITY_TYPE_CONSTRAINT of (T.equalityType * T.equalityType) EL.extLab

     (** Only one constructor - CONSTRAINTS, a constraintMap of oneConstraint lists. *)
     and constraints      = CONSTRAINTS of oneConstraint list constraintMap

(* constraints maps integers (program point labels) to lists (conceptually sets) of oneConstraint's *)
(*(2010-04-14)Conceptually, (INJI extty) seems to be an env of the form:
 * ENVS (ENVV ev, ENVC x), where extty is declared in x and ev is fresh.*)
(*(2010-03-02)not withtype because it is not SML valid and even though SML/NJ
 * does not complain, MLton does.*)

(** Lists the items in the map of CONSTRAINTS items. *)
fun getConstraintItems (CONSTRAINTS(map)) = OMC.listItems map

(** An #env #bind. *)
type extstr = env bind
(** A #env #genericBind. *)
type strenv = env genericEnv

(** An #env #bind. *)
type extsig = env bind
(** A #end #genericEnv. *)
type sigenv = env genericEnv

(** Functor has type env1 -> env2 *)
type funsem = env * env
type extfun = funsem bind
type funenv = funsem genericEnv

(** Represensts a context sensitive syntax error.
 * These used to be in the env but they got moved out, * they are now separate because the usualyl get passed through.
 * \arg CSSMULT. For multi occurrence - the 'id list' is then always empty.
*\ \arg CSSCVAR. For applied identifiers in patterns that should be constructors but are variables.
*\ \arg CSSEVAR. For identifiers in exception bindings that should be exceptions but are variables.
*\ \arg CSSECON. For identifiers in exception bindings that should be exceptions but are datatype constructors.
*\ \arg CSSINCL. For non-inclusion of type variables in a datatype.
*\ \arg CSSAPPL. For applied and not applied value in pattern.
*\ \arg CSSFNAM. For different function names.
*\ \arg CSSFARG. A fgunction with different number of arguments.
*\ \arg CSSTYVA. Free type variable at top-level.
*\ \arg CSSLEFT. Ident to left of as in pattern must be a variable.
*\ \arg CSSFREC. Expressions within rec value bindings must be functions.
*\ \arg CSSREAL. Reals cannot occur within patterns.
*\ \arg CSSFREE. Free identifiers
*\ \arg CSSWARM. For a warning.
*\ \arg CSSPARS. For a parsing problem.
*)
datatype oneContextSensitiveSyntaxError = CSSMULT of Label.labels
					| CSSCVAR of Label.labels
					| CSSEVAR of Label.labels
					| CSSECON of Label.labels
					| CSSINCL of Label.labels
					| CSSAPPL of Label.labels
					| CSSFNAM of Label.labels
					| CSSFARG of Label.labels
					| CSSTYVA of Label.labels
					| CSSLEFT of Label.labels
					| CSSFREC of Label.labels
					| CSSREAL of Label.labels
					| CSSFREE of Label.labels
					| CSSWARN of Label.labels * string
					| CSSPARS of Label.labels * string

(** A list of context sensitive syntax errors. *)
type contextSensitiveSyntaxError       = oneContextSensitiveSyntaxError list

(** A pair of an environment and a context sensitive syntax error. *)
type envContextSensitiveSyntaxPair = env * contextSensitiveSyntaxError


(** A tab separation space, set to 11 spaces. *)
val tab = "           "

(** Prints a list. *)
fun printlistgen xs f = "[" ^ #1 (foldr (fn (t, (s, c)) => (f t ^ c ^ s, ",")) ("", "") xs) ^ "]"

(** Prints a binding using #ExtLab.printExtLab. *)
fun printBind bind f assoc = EL.printExtLab bind (fn x => C.printBind x f assoc) assoc

(** Prints a binding using #printBind. *)
fun printBind' bind f = printBind bind f I.emAssoc

(** Function to print an environent variable. *)
fun printEnvVar v = "e" ^ Int.toString v

(** Prints a list of environment variables. *)
fun printEnvVarList xs = printlistgen xs printEnvVar

(** Prints an option value. *)
fun printOp NONE _ = "-"
  | printOp (SOME x) f = f x

(** Calls #printOp on the argument with the #printEnvVar function. *)
fun printEvOp       x = printOp x printEnvVar

(** Prints a typename kind to a string. *)
fun printTnKind DATATYPE = "DATATYPE"
  | printTnKind TYPE = "TYPE"

(** Prints a generic environment. *)
fun printGenEnv xs ind f =
    let val ind' = ind ^ "    "
    in #1 (envOrdMap.foldli
	       (fn (k, x, (y, z)) =>
		   (y ^ z ^ I.printId k ^ ":" ^ f x, "\n" ^ ind'))
	       ("", "")
	       xs)
    end

(** Prints a binding using #Ty.printtty. *)
fun printExtVar extvar = printBind' extvar T.printty
(** Prints a binding using a function to print type variables and a boolean they are paired with. *)
and printExtTyv exttyv = printBind' exttyv (fn (tv, b) => "(" ^ T.printTypeVar tv ^ "," ^ Bool.toString b ^ ")")
(** Prints a binding using a function to print type function variables, their kind, and their constructor sort. *)
and printExtTyp exttyp = printBind' exttyp (fn (tyf, tnkind, cons) => "(tyf=" ^ T.printtyf tyf ^ ",tnkind=" ^ printTnKind tnkind ^ ",cons=" ^ printExtCon (!cons) ^ ")")
(** Prints a binding using #Ty.printseqty. *)
and printExtSeq extovc = printBind' extovc T.printseqty
(** Prints an external constructor. *)
and printExtCon (cons, b) = "(" ^ printVarEnv cons "" ^ "," ^ Bool.toString b ^ ")"

(** Prins a list of extended variables using #printExtVar. *)
and printExtVarList xs = printlistgen xs printExtVar
(** Prins a list of extended type variables using #printExtTyv. *)
and printExtTyvList xs = printlistgen xs printExtTyv
(** Prins a list of extended typenames using #printExtTyp. *)
and printExtTypList xs = printlistgen xs printExtTyp
(** Prins a list of extended sequence types using #printExtSeq. *)
and printExtSeqList xs = printlistgen xs printExtSeq

(** Prints a type variable environment. *)
and printTypeVarEnv xs ind = printGenEnv xs ind printExtTyvList
(** Prints a variable environment. *)
and printVarEnv xs ind = printGenEnv xs ind printExtVarList
(** Prints a type environment. *)
and printTypeEnv xs ind = printGenEnv xs ind printExtTypList
(** Prints an overloading constructor environmetn. *)
and printOvcEnv xs ind = printGenEnv xs ind printExtSeqList

(** Prints a kind of opened expression, either an opened structure, datatype replication, or included signature. *)
fun printOpenKind OPENED_STRUCT = "OPENED_STRUCT"
  | printOpenKind DATATYPE_REPLICATION = "DATATYPE_REPLICATION"
  | printOpenKind INCLUDED_SIG = "INCLUDED_SIG"

(** Prints an OPEN environment. *)
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

(** Prints a typename mapping. *)
fun printTypeNameMap xs =
    printlistgen xs (fn {id, lab, kind, name} =>
			"(" ^ I.printId     id   ^
			"," ^ L.printLab    lab  ^
			"," ^ printTnKind   kind ^
			"," ^ T.printTypename name ^ ")")

(** Prints an info environment. *)
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

(** Prints a sharing binder. *)
fun printShaBind (ev1, ev2, ev3, lab) =
    "(" ^ printEnvVar ev1 ^
    "," ^ printEnvVar ev2 ^
    "," ^ printEnvVar ev3 ^
    "," ^ L.printLab  lab ^ ")"

(** Print a pair (x,y) appling the function f to both the left and right hand side. *)
fun printPair (x, y) f = "(" ^ f x ^ "," ^ f y ^ ")"

(** Prints out an accessor as a string. *)
fun printAccessorId {lid, equalityTypeVar, sem, class, lab} f ind ascid =
    "{" ^ I.printLid' lid ascid ^
    "," ^ T.printEqualityTypeVar equalityTypeVar ^
    "," ^ f sem                 ^
    "," ^ CL.toString class     ^
    "," ^ L.printLab lab        ^ "}"

(** Prints a match kind, either OPAQUE or TRANSLUCENT. *)
fun printMatchKind OPAQUE = "OPAQUE"
  | printMatchKind TRANSLUCENT = "TRANSLUCENT"

(** Prints a long type constructor binder. *)
fun printLongTypeConsBinder longTypeConsBinder ind =
    "LONGTYPECONSBINDER(" ^ EL.printExtLab' longTypeConsBinder (fn x => printAccessorId x T.printtyf' ind I.emAssoc) ^ ")"

(** Prints a binder using #printEnv. *)
fun printExtEnv     x      = printBind' x (fn x => printEnv x "")
(** Prints a list of binders using #printExtEnv. *)
and printExtEnvList xs     = printlistgen xs printExtEnv
and printFunSemList xs     = printlistgen xs (fn (x, y) => "(" ^ printExtEnv x ^ "," ^ printExtEnv y ^ ")")

(** Prints a signature environment. *)
and printSigEnv     xs ind = printGenEnv xs (ind ^ "  ") printExtEnvList
(** Prints a structure environment. *)
and printStrEnv     xs ind = printGenEnv xs (ind ^ "  ") printExtEnvList
(** Prints a functor environment. *)
and printFunEnv     xs ind = printGenEnv xs ind printFunSemList

(** Produces a string representation of an environment.
 * \param An environment
 * \param An indentation level. *)
and printEnv (ENV_CONS {valueIds, typeNames, explicitTypeVars, structs, sigs, functors, overloadingClasses, info}) ind =
    "\n" ^ ind ^ "ENV_CONS{valueIds:" ^ printVarEnv valueIds ind ^
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
  | printEnv NO_DUPLICATE_ID ind = "NO_DUPLICATE_ID"
  | printEnv (ENVFIL (st, env, stream)) ind =
    "ENVFIL(" ^ st ^ "," ^ printEnv env ind ^ ",\n" ^ printEnv (stream ()) ind ^ ")"
  | printEnv ENVTOP ind = "ENVTOP"

(** Prints an accessor. *)
and printAcc (VALUEID_ACCESSOR x) ind ascid =
    "VALUEID_ACCESSOR(" ^ EL.printExtLab x (fn x => printAccessorId x T.printty' ind ascid) ascid ^ ")"
  | printAcc (EXPLICIT_TYPEVAR_ACCESSOR x) ind ascid =
    "EXPLICIT_TYPEVAR_ACCESSOR(" ^ EL.printExtLab x (fn x => printAccessorId x T.printty ind ascid) ascid ^ ")"
  | printAcc (EQUALITY_TYPE_ACCESSOR x) ind ascid =
    "EQUALITY_TYPE_ACCESSOR(" ^ EL.printExtLab x (fn x => printAccessorId x T.printEqualityType ind ascid) ascid ^ ")"
  | printAcc (TYPE_CONSTRUCTOR_ACCESSOR x) ind ascid =
    "TYPE_CONSTRUCTOR_ACCESSOR(" ^ EL.printExtLab x (fn x => printAccessorId x (fn (x,y) => "(" ^ (T.printtyf' x) ^ "," ^ (Bool.toString y) ^ ")") ind ascid) ascid ^ ")"
  | printAcc (OVERLOADING_CLASSES_ACCESSOR x) ind ascid =
    "OVERLOADING_CLASSES_ACCESSOR(" ^ EL.printExtLab x (fn x => printAccessorId x T.printseqty' ind ascid) ascid ^ ")"
  | printAcc (STRUCTURE_ACCESSOR x) ind ascid =
    "STRUCTURE_ACCESSOR(" ^ EL.printExtLab x (fn x => printAccessorId x (fn e => printEnv e (ind ^ tab)) ind ascid) ascid ^ ")"
  | printAcc (SIGNATURE_ACCESSOR x) ind ascid =
    "SIGNATURE_ACCESSOR(" ^ EL.printExtLab x (fn x => printAccessorId x (fn e => printEnv e (ind ^ tab)) ind ascid) ascid ^ ")"
  | printAcc (FUNCTOR_ACCESSOR x) ind ascid =
    "FUNCTOR_ACCESSOR(" ^ EL.printExtLab x (fn x => printAccessorId x (fn (e1, e2) => "(" ^ printEnv e1 (ind ^ tab) ^ ",\n" ^ printEnv e1 (ind ^ tab) ^ ")") ind ascid) ascid ^ ")"

(** Prints a single constraint. *)
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

(** Prints a list of single constraints. *)
and printocstlist []        ind1 ind2 ind3 ascid = ""
  | printocstlist [x]       ind1 ind2 ind3 ascid = ind2 ^ printocst x ind1 ascid
  | printocstlist (x :: xs) ind1 ind2 ind3 ascid = ind2 ^ printocst x ind1 ascid ^
						   "\n" ^
						   printocstlist xs ind1 (ind1 ^ ind3) ind3 ascid

(** Folds right over a CONSTRAINTS map, and prints the constraints within. *)
and printcst' (CONSTRAINTS cst) ind ascid =
    OMC.foldri
	(fn (n, oneConstraint, y) =>
	    ind ^ Int.toString n
	    ^ ":"
	    ^ printocstlist oneConstraint ind "" (" " ^ (String.translate (fn _ => " ") (Int.toString n))) ascid
	    ^ "\n"
	    ^ y) "" cst

(** Calls #printcst'. *)
and printcst cst ascid = printcst' cst "" ascid

(** Prints a CONSTRAINTS value. *)
fun printConstraints cst = printcst cst I.emAssoc

(** Prints one constraint using #printocst. *)
fun printOneConstraint cst = printocst cst "" I.emAssoc

(** Prints a list of single constraints using #printOneConstraint. *)
fun printOneConstraintList [] = ""
  | printOneConstraintList (h::h2::t) = printOneConstraint h ^ ", " ^ (printOneConstraintList t)
  | printOneConstraintList (h::t) = printOneConstraint h

(** Prints an accessor using #printAcc. *)
fun printOneAccessor   cst = printAcc cst "" I.emAssoc

(* Bindings constructors *)

(** Constructs a binder. *)
fun consBind     id bind equalityTypeVar class lab poly = EL.initExtLab (C.consBind     id bind equalityTypeVar class lab poly) lab
(** Constructs a polymorphic binding. *)
fun consBindPoly {id=id, typeOfId=bind, equalityTypeVar=eqtv, classOfId=class, labelOfConstraint=lab} =
    EL.initExtLab (C.consBindPoly id bind eqtv class lab)      lab
(** Constructs a monomorphic binding. *)
fun consBindMono id bind equalityTypeVar class lab = EL.initExtLab (C.consBindMono id bind equalityTypeVar class lab) lab

(** Constructs a new accessorId value.. *)
fun consAccessorId lid eqtv sem class lab = {lid = lid, equalityTypeVar = eqtv, sem = sem, class = class, lab = lab}

(* Accessors to a extenv *)

(** Gets the identifier of a binding. *)
fun getBindI x = C.getBindI (EL.getExtLabT x)
(** Gets the binding term. *)
fun getBindT x = C.getBindT (EL.getExtLabT x)
(** Gets equality type information from a binding. *)
fun getBindEqualityTypeVar x = C.getBindEqualityTypeVar (EL.getExtLabT x)
(** Gets the class from a binding. *)
fun getBindC x = C.getBindC (EL.getExtLabT x)
(** Gets the labels of a binding. *)
fun getBindL x = C.getBindL (EL.getExtLabT x)
(** Gets the polymorphism status of a binding. *)
fun getBindP x = C.getBindP (EL.getExtLabT x)

(** The next environment variable to assign, a ref initially set to 0. *)
val nextEnvVar     = ref 0
(** Sets the #nextEnvVar to the value specified in the argument. *)
fun setNextEnvVar     n  = nextEnvVar := n
(** Gets the next environment variable to use. *)
fun getEnvVar   () = !nextEnvVar
(** Generates a fresh environment variable, incrementing the #nextEnvVar counter. *)
fun freshEnvVar () = let val x = !nextEnvVar in nextEnvVar := x + 1; x end
(** Resets the environment variable counter. *)
fun resetEnvVar () = setNextEnvVar 0

(** Converts an environment variable to an integer. *)
fun envVarToInt envVar = envVar

(** Tests equality of two environment variables. *)
fun eqEnvVar ev1 ev2 = (ev1 = (ev2 : envVar))

(** Constructs an environment variable. *)
fun consENV_VAR ev lab = ENV_VAR (ev, lab)
(** Builds a new, fresh, environment variable, taking a label of the new environment variable as an argument. *)
fun newEnvVar  lab    = consENV_VAR (freshEnvVar ()) lab

(** Unwraps an ENV_VAR, throws an exception if receives an environment that isn't an ENV_VAR. *)
fun envToEnvVar (ENV_VAR ev) = ev
  | envToEnvVar _ = raise EH.DeadBranch "the env should be a variable"

(** Construct an environment constructor (ENV_CONS). *)
fun consEnvConstructor valueIds typeNames explicitTypeVars structs sigs functors overloadingClasses info =
    ENV_CONS {valueIds = valueIds,
	    typeNames = typeNames,
	    explicitTypeVars = explicitTypeVars,
	    structs = structs,
	    sigs = sigs,
	    functors = functors,
	    overloadingClasses = overloadingClasses,
	    info = info}

(** Constructs an info field of an environment constructor. *)
fun consInfo lab complete infoTypeNames argOfFunctor = {lab = lab, complete = complete, infoTypeNames = infoTypeNames, argOfFunctor = argOfFunctor}

(** The empty map. *)
val emptyMap = envOrdMap.empty
(** The empty variable map. *)
val emvar = emptyMap
(** The empty typename map. *)
val emtyp = emptyMap
(** The empty type variable map. *)
val emtv  = emptyMap
(** The empty structure map. *)
val emstr = emptyMap
(** The empty signature map. *)
val emsig = emptyMap
(** The empty functor map. *)
val emfun = emptyMap
(** The empty open map. *)
val emopn = OMO.empty
(** The empty overloading constructor map. *)
val emoc  = emptyMap
(** The env is initially complete. *)
val emful = true
(** The env does not initially define any type name. *)
val emtn  = [] : typeNameMap
(* The env is initially not the parameter of a functor. *)
val emfct = false
(** The empty info field. *)
val emnfo = consInfo L.dummyLab emful emtn emfct
(** An empty environment. *)
val emptyEnv = consEnvConstructor emvar emtyp emtv emstr emsig emfun emoc emnfo

(** Gets the value identifiers from an environment constructor. *)
fun getValueIds (ENV_CONS x) = #valueIds x
  | getValueIds _          = raise EH.DeadBranch ""
(** Gets the type names from an environment constructor. *)
fun getTypeNameEnv (ENV_CONS x) = #typeNames x
  | getTypeNameEnv _          = raise EH.DeadBranch ""
(** Gets the explicit type variables from an environment constructor. *)
fun getExplicitTypeVars (ENV_CONS x) = #explicitTypeVars x
  | getExplicitTypeVars _          = raise EH.DeadBranch ""
(** Gets the structures from an environment constructor. *)
fun getStructs (ENV_CONS x) = #structs x
  | getStructs _          = raise EH.DeadBranch ""
(** Gets the signatures from an environment constructor. *)
fun getSigs (ENV_CONS x) = #sigs x
  | getSigs _          = raise EH.DeadBranch ""
(** Gets the functors from an environment constructor. *)
fun getFunctors (ENV_CONS x) = #functors x
  | getFunctors _          = raise EH.DeadBranch ""
(** Gets the overloading classes from an environment constructor. *)
fun getOverloadingClasses (ENV_CONS x) = #overloadingClasses x
  | getOverloadingClasses _          = raise EH.DeadBranch ""
(** Gets the info field from an environment constructor. *)
fun getInfo (ENV_CONS x) = #info x
  | getInfo _          = raise EH.DeadBranch ""
(** Gets the labels from an environment constructor. *)
fun getILab (ENV_CONS x) = #lab (#info x)
  | getILab _          = raise EH.DeadBranch ""
(** Gets the 'complete' field from an environment constructor. *)
fun getIComplete (ENV_CONS x) = #complete (#info x)
  | getIComplete _          = raise EH.DeadBranch ""
(** Gets the argument of a functor field from an environment constructor. *)
fun getIArgOfFunctor (ENV_CONS x) = #argOfFunctor (#info x)
  | getIArgOfFunctor _          = raise EH.DeadBranch ""
(** Gets the info type names field of an environment constructor. *)
fun getITypeNames (ENV_CONS x)        = #infoTypeNames (#info x)
  | getITypeNames (ROW_ENV (e1, e2)) = (getITypeNames e1) @ (getITypeNames e2)
  | getITypeNames (ENV_VAR _)        = []
  | getITypeNames env               = (print (printEnv env ""); raise EH.DeadBranch "")

(** Update fields in a record of type infoEnv *)
fun updateInfoLab lab {lab = _, complete, infoTypeNames, argOfFunctor} = consInfo lab complete infoTypeNames argOfFunctor
(** Updatates the 'complete' field. *)
fun updateInfoComplete complete {lab, complete = _, infoTypeNames, argOfFunctor} = consInfo lab complete infoTypeNames argOfFunctor
(** Updates the typenames field. *)
fun updateInfoTypeNames infoTypeNames {lab, complete, infoTypeNames = _, argOfFunctor} = consInfo lab complete infoTypeNames argOfFunctor
(** Updates the argOfFunctor field. *)
fun updateInfoArgOfFunctor argOfFunctor {lab, complete, infoTypeNames, argOfFunctor = _} = consInfo lab complete infoTypeNames argOfFunctor

(** Updates the value identifiers in an environment constructor. *)
fun updateValueIds valueIds (ENV_CONS {valueIds = _, typeNames, explicitTypeVars, structs, sigs, functors, overloadingClasses, info}) =
    consEnvConstructor valueIds typeNames explicitTypeVars structs sigs functors overloadingClasses info
  | updateValueIds _ _ = raise EH.DeadBranch ""
(** Updates the type names in an environment constructor. *)
fun updateTypeNames typeNames (ENV_CONS {valueIds, typeNames = typeNamesOld, explicitTypeVars, structs, sigs, functors, overloadingClasses, info}) =
    consEnvConstructor valueIds typeNames explicitTypeVars structs sigs functors overloadingClasses info
  | updateTypeNames _ _ = raise EH.DeadBranch ""
(** Updates the explicit type variables in an environment constructor. *)
fun updateExplicitTypeVars explicitTypeVars (ENV_CONS {valueIds, typeNames, explicitTypeVars = _, structs, sigs, functors, overloadingClasses, info}) =
    consEnvConstructor valueIds typeNames explicitTypeVars structs sigs functors overloadingClasses info
  | updateExplicitTypeVars _ _ = raise EH.DeadBranch ""
(** Updates the structures in an environment constructor. *)
fun updateStructs structs (ENV_CONS {valueIds, typeNames, explicitTypeVars, structs = _, sigs, functors, overloadingClasses, info}) =
    consEnvConstructor valueIds typeNames explicitTypeVars structs sigs functors overloadingClasses info
  | updateStructs _ _ = raise EH.DeadBranch ""
(** Updates the signatures in an environment constructor. *)
fun updateSigs sigs (ENV_CONS {valueIds, typeNames, explicitTypeVars, structs, sigs = _, functors, overloadingClasses, info}) =
    consEnvConstructor valueIds typeNames explicitTypeVars structs sigs functors overloadingClasses info
  | updateSigs _ _ = raise EH.DeadBranch ""
(** Updates the functorrs in an environment constructor. *)
fun updateFunctors functors (ENV_CONS {valueIds, typeNames, explicitTypeVars, structs, sigs, functors = _, overloadingClasses, info}) =
    consEnvConstructor valueIds typeNames explicitTypeVars structs sigs functors overloadingClasses info
  | updateFunctors _ _ = raise EH.DeadBranch ""
(** Updates the overloading classes in an environment constructor. *)
fun updateOverloadingClasses overloadingClasses (ENV_CONS {valueIds, typeNames, explicitTypeVars, structs, sigs, functors, overloadingClasses = _, info}) =
    consEnvConstructor valueIds typeNames explicitTypeVars structs sigs functors overloadingClasses info
  | updateOverloadingClasses _ _ = raise EH.DeadBranch ""
(** Updates the info lab field of an environment constructor. *)
fun updateILab lab (ENV_CONS {valueIds, typeNames, explicitTypeVars, structs, sigs, functors, overloadingClasses, info}) =
    consEnvConstructor valueIds typeNames explicitTypeVars structs sigs functors overloadingClasses (updateInfoLab lab info)
  | updateILab lab (ROW_ENV (env1, env2)) = ROW_ENV (updateILab lab env1, updateILab lab env2)
  | updateILab lab (LOCAL_ENV (env1, env2)) = LOCAL_ENV (env1, updateILab lab env2)
  | updateILab _ env = env
(** Updates the complete field of the info field. *)
fun updateIComplete complete (ENV_CONS {valueIds, typeNames, explicitTypeVars, structs, sigs, functors, overloadingClasses, info}) =
    consEnvConstructor valueIds typeNames explicitTypeVars structs sigs functors overloadingClasses (updateInfoComplete complete info)
  | updateIComplete _ _ = raise EH.DeadBranch ""
(** Updates the typename filed of the info field. *)
fun updateInfoTypeNames infoTypeNames (ENV_CONS {valueIds, typeNames, explicitTypeVars, structs, sigs, functors, overloadingClasses, info as {lab, complete, infoTypeNames = _, argOfFunctor}}) =
    consEnvConstructor valueIds typeNames explicitTypeVars structs sigs functors overloadingClasses (consInfo lab complete infoTypeNames argOfFunctor)
  | updateInfoTypeNames _ _ = raise EH.DeadBranch ""
(** Updates the arg of functor field of the info field. *)
fun updateIArgOfFunctor argOfFunctor (ENV_CONS {valueIds, typeNames, explicitTypeVars, structs, sigs, functors, overloadingClasses, info}) =
    consEnvConstructor valueIds typeNames explicitTypeVars structs sigs functors overloadingClasses (updateInfoArgOfFunctor argOfFunctor info)
  | updateIArgOfFunctor argOfFunctor (ROW_ENV (env1, env2)) = ROW_ENV (updateIArgOfFunctor argOfFunctor env1, updateIArgOfFunctor argOfFunctor env2)
  | updateIArgOfFunctor argOfFunctor (LOCAL_ENV (env1, env2)) = LOCAL_ENV (env1, updateIArgOfFunctor argOfFunctor env2)
  | updateIArgOfFunctor _ env = env

(** Calls #updateValueIds with the argument and the empty environment. *)
fun projValueIds idG = updateValueIds idG emptyEnv
(** Constructs an environment containing the typenames in idT using the empty environment.  *)
fun consEnvTypeNames idT = updateTypeNames idT emptyEnv
(** Calls #updateExplicitTypeVars with the argument and the empty environment. *)
fun projExplicitTypeVars idV = updateExplicitTypeVars idV emptyEnv
(** Calls #updateStructs with the argument and the empty environment. *)
fun projStructs idS = updateStructs idS emptyEnv
(** Calls #updateSigs with the argument and the empty environment. *)
fun projSigs idS = updateSigs idS emptyEnv
(** Calls #updateFunctors with the argument and the empty environment. *)
fun projFunctors idF = updateFunctors idF emptyEnv
(** Calls #updateOverloadingClasses with the argument and the empty environment. *)
fun projOverloadingClasses idD = updateOverloadingClasses idD emptyEnv

(** Wraps the argument in an ENVOPN constructor. *)
fun projOpns idO = ENVOPN idO

(** Tests that the number of items in the argument map is equal to 0. *)
fun isEmptyIdEnv idenv = envOrdMap.numItems idenv = 0

(** Tests whether the variable environment is empty. *)
fun isEmptyVarEnv varenv = isEmptyIdEnv varenv
(** Tests whether the type variable environment is empty. *)
fun isEmptyTypeVarEnv typeVarEnv = isEmptyIdEnv typeVarEnv
(** Tests whether the structure environment is empty. *)
fun isEmptyStrEnv strenv = isEmptyIdEnv strenv
(** Tests whether the open environment is empty. *)
fun isEmptyOpenEnv openEnv = OMO.isEmpty openEnv

(** Tests whether an env is empty *)
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

(** Inserts a value into an envirnment. *)
fun addenv  (v, semty) env = envOrdMap.insert (env, v, semty)

(** Takes an key and a value and construct a new map for envs, with a mapping of that key to that value inside it. *)
fun consSingleEnv (v, semty)     = envOrdMap.insert (envOrdMap.empty, v, semty)

(** Finds an identifier in an environment .*)
fun findenv id env = envOrdMap.find (env, id)

(** Removes an identifier from an environment. *)
fun remenv id env = #1 (envOrdMap.remove (env, id)) handle LibBase.NotFound => env

(** Maps a function over an environment. *)
fun mapenv fmap env = envOrdMap.map fmap env

(** A function which unions a list of environments. *)
fun unionEnvList envList =
    foldr (fn (env1, env2) => envOrdMap.unionWith     (* unionWith returns a map that is the union of two maps *)
				 (fn (x, y) => x @ y) (* used to define the map on elements that are in the domain of both maps *)
				 (env1, env2))        (* the two maps that are to be unioned *)
	  emptyMap (* the empty map (from the ORD_MAP signature defined in the smlnj library *)
	  envList     (* a list of maps to union, which foldr operates on *)

fun plusenv env1 env2 = envOrdMap.unionWith (fn (_, y) => y) (env1, env2)

(** Folds right over an environment. *)
fun foldrenv  ffold init env = envOrdMap.foldr  ffold init env
(** Folds left over an environment. *)
fun foldlenv  ffold init env = envOrdMap.foldl  ffold init env
(** Folds right over an environment (including keys, not just values). *)
fun foldrienv ffold init env = envOrdMap.foldri ffold init env
(** Folds left over an environment (including keys, not just values). *)
fun foldlienv ffold init env = envOrdMap.foldli ffold init env

(** Applies a function to an environment. *)
fun appenv  fmap env = envOrdMap.app  fmap env
(** Applies a function to an environment (to keys and values, not just values). *)
fun appienv fmap env = envOrdMap.appi fmap env

(** Gets the domain of an environment. *)
fun dom  env  = envOrdMap.foldri (fn (l, _, set) => I.add l set) I.empty env
(** Gets the domains of a list of environments. *)
fun doms envl = foldr (fn (x, y) => I.union (dom x) y) I.empty envl

(** Finds an id in an environment, returning the found item if found or the empty list otherwise. *)
fun plusproj env id = case findenv id env of NONE => [] | SOME x => x

(** Translates binders into environments by folding right over the binder list and constructing single environments. *)
fun bindToEnv binds =
    List.foldr (fn (x, genenv) => unionEnvList [consSingleEnv (C.getBindI (EL.getExtLabT x), [x]), genenv]) emptyMap binds

fun envToBind env = foldrenv (fn ([x], binds) => x :: binds
			       | _ => raise EH.DeadBranch "")
			     []
			     env

(** Turns a list of environments into row environments. *)
fun envsToSeq [] = emptyEnv
  | envsToSeq [env] = env
  | envsToSeq (env :: envs) =
    if isEmptyEnv env
    then envsToSeq envs
    else ROW_ENV (env, envsToSeq envs)

(** Fifo.foldl. *)
val foldlOEnv = OMO.foldl
(** Fifo.opp. *)
fun appOEnv fapp env = OMO.app fapp env
(** Fifo.enqueue. *)
fun addOEnv x env = OMO.enqueue (env, x)
(** Creates a single open environment using #addOEnv and #emopn. *)
fun singOEnv x = addOEnv x emopn
(** Unions open environments. *)
fun uOEnv envl =
    foldl (fn (env1, env2) => OMO.foldl (fn (x, env) => addOEnv x env) env2 env1)
	  emopn
	  envl

(** Union the environment info of two environments. *)
fun uenvEnvInfo {lab = lab1, complete = complete1, infoTypeNames = infoTypeNames1, argOfFunctor = argOfFunctor1}
		{lab = lab2, complete = complete2, infoTypeNames = infoTypeNames2, argOfFunctor = argOfFunctor2} =
    consInfo lab2 (*(2010-04-06)Why do we keep the second one only?*)
	     (complete1 andalso complete2) (* is complete if both are complete *)
	     (infoTypeNames1 @ infoTypeNames2)
	     (argOfFunctor1 orelse argOfFunctor2) (* is the argument of a functor if at least one is *)

(** Union two environment constructors. *)
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

(** Union of an environment. *)
fun unionEnv [] = emptyEnv
  | unionEnv ((ROW_ENV (env1, env2)) :: xs) = unionEnv (env1 :: env2 :: xs)
  | unionEnv [x] = x
  | unionEnv (x :: xs) = uenvEnvC x (unionEnv xs)

fun closeValueIds valueIds clos =
    mapenv (fn sem => map (fn x => EL.mapExtLab x (fn x => C.closeBind x clos)) sem)
	   valueIds

(** Merges two environments together. *)
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

fun pushExtIdEnv idenv labs stts deps = mapenv (fn sem => map (fn bind => EL.updExtLab bind labs stts deps) sem) idenv

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

(** An empty context sensitive syntax error. *)
val emptyContextSensitiveSyntaxError = []
(** An empty CONSTRAINTS value. *)
val emptyConstraint = CONSTRAINTS OMC.empty

fun getcstSemi cst i = case OMC.find (cst, i) of NONE => [] | SOME x => x

(** Adds a consrtaints into a CONSTRAINTS map. *)
fun consConstraint (v, c) (CONSTRAINTS cst) = CONSTRAINTS (OMC.insert (cst, L.toInt v, c :: (getcstSemi cst (L.toInt v))))
fun conscsss cl css = cl @ css

(** This adds a list of constraints (cs) to cst using the label as the key
 * if constraints have already been added for a label, these new constraints are added to the original constraints *)
fun conscsts (v, cs) (CONSTRAINTS cst) = CONSTRAINTS (OMC.insert (cst, L.toInt v, cs @ (getcstSemi cst (L.toInt v))))

(** A single context sensitive syntax value. *)
fun singcss  c  = [c]
(** Takes one context sensitive syntax error list and returns a one context sensitive syntax from it. *)
fun singcsss cs = cs
(** Adds a single constraint into a CONSTRAINTS map. *)
fun singleConstraint (v, c) = consConstraint  (v, c)  emptyConstraint
(** Calls #conscsts on the argument and the #emptyConstraint value. *)
fun singcsts (v, cs) = conscsts (v, cs) emptyConstraint

(** Unions a list of context sensitive syntax errors *)
fun unionContextSensitiveSyntaxErrors xs = foldr (fn (x, y) => x@y) emptyContextSensitiveSyntaxError xs

(** Unions two CONSTRAINTS values (will union two lists of constraints). *)
fun unionConstraints (CONSTRAINTS cst1) (CONSTRAINTS cst2) = CONSTRAINTS (OMC.unionWith (fn (x, y) => x @ y) (cst1, cst2))

(** Takes a list of CONSTRAINTS values, and use unionConstraints to union the values *)
fun unionConstraintsList xs = foldr (fn (x, y) => unionConstraints x y) emptyConstraint xs

(** Folds left (keys and values) over a CONSRTAINTS map. *)
fun foldlicst ffold init (CONSTRAINTS cst) = OMC.foldli ffold init cst
(** Folds right (keys and values) over a CONSRTAINTS map. *)
fun foldricst ffold init (CONSTRAINTS cst) = OMC.foldri ffold init cst
(** Maps a function over a CONSTRAINTS map. *)
fun mapicst   ffold      (CONSTRAINTS cst) = CONSTRAINTS (OMC.mapi ffold cst)

(** Calculates the number of constraints that exist in the constraints agrument. *)
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

(** Calculates the number of constraints that exist in the environment argument. *)
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

(** Calculates the number of constraints present in the environment, context sensitive syntax error tuple argument (only uses the env). *)
fun getnbcst (env, css) = getnbcstenv env

(** Deprecated, old comment indicates this is not useful any longer.
 * \deprecated Seems to do the same now as #getnbcst. *)
fun getnbcsttop (env, css) = getnbcstenv env

(** Gets the length of the context sensitive syntax error list. *)
fun getnbcss' css = List.length css

(** Gets the length of the context sensitive syntax error list in an (env,css) pair (ignores env). *)
fun getnbcss (envc, css) = getnbcss' css

(** Adds the result of #getnbcstenv and #getnbcss' together from the respective functions called on the argument. *)
fun getnbcs (env, css) = (getnbcstenv env) + (getnbcss' css)

(** We get the labels at the csbindings *)
fun getlabsidenv idenv =
    envOrdMap.foldr
	(fn (sem, labs) => L.union (L.ord (map (fn (x, _, _, _) => C.getBindL x) sem)) labs)
	L.empty
	idenv

fun getlabopenEnv openEnv = OMO.foldl (fn ((_, lab, _), set) => L.cons lab set) L.empty openEnv

(** Combines labels in environments. *)
fun combineLabsEnv (outlabs1, inlabs1) (outlabs2, inlabs2) = (outlabs1 @ outlabs2, L.union inlabs1 inlabs2)

(** Gets the labels that exist in an environment. *)
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

(** Gets the labels in a single constraint. *)
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

(** Gets the labels in a CONSTRAINTS map. *)
and getlabscst cst =
    foldricst
	(fn (_, ocstl, labsenv) =>
	    foldr (fn (oneConstraint, labsenv) => combineLabsEnv (getlabcsbindocst oneConstraint) labsenv)
		  labsenv
		  ocstl)
	([], L.empty)
	cst

(** Gets the bindings in an environment. *)
and getbindings env = getlabsenv env

(** Generate a extended label using #ExtLab.consExtLab. *)
fun genCstAllGen x1 x2 labs sts cds = EL.consExtLab (x1, x2) labs sts cds
(** Generates a type constraint. *)
fun genCstTyAll x1 x2 labs sts cds = TYPE_CONSTRAINT (genCstAllGen x1 x2 labs sts cds)
(** Generates a function type constraint. *)
fun genCstTfAll x1 x2 labs sts cds = FUNCTION_TYPE_CONSTRAINT (genCstAllGen x1 x2 labs sts cds)
(** Generates an equality type constraint. *)
fun genCstEqAll x1 x2 labs sts cds = EQUALITY_TYPE_CONSTRAINT (genCstAllGen x1 x2 labs sts cds)
(** Generates an equality type constraint. *)
fun genCstTnAll x1 x2 labs sts cds = TYPENAME_CONSTRAINT (genCstAllGen x1 x2 labs sts cds)
(** Generates an row constraint. *)
fun genCstSqAll x1 x2 labs sts cds = ROW_CONSTRAINT (genCstAllGen x1 x2 labs sts cds)
(** Generates an fild constraint. *)
fun genCstRtAll x1 x2 labs sts cds = FIELD_CONSTRAINT (genCstAllGen x1 x2 labs sts cds)
(** Generates an label constraint. *)
fun genCstLtAll x1 x2 labs sts cds = LABEL_CONSTRAINT (genCstAllGen x1 x2 labs sts cds)
(** Generates an environment constraint. *)
fun genCstEvAll x1 x2 labs sts cds = ENV_CONSTRAINT (genCstAllGen x1 x2 labs sts cds)
(** Generates an identifier class constraint. *)
fun genCstClAll x1 x2 labs sts cds = IDENTIFIER_CLASS_CONSTRAINT (genCstAllGen x1 x2 labs sts cds)

(** Creates a ValueID accessor. *)
fun genValueIDAccessor x labs sts cds = VALUEID_ACCESSOR (EL.consExtLab x labs sts cds)

(** initTypeConstraint - return type oneConstraint
 * x1 and x2 here are of type T.ty, lab is of type Label.labels
 * x1 appears to be the type variable that you are constraing
 * x2 appears to be what you are constraining x1 to be
 *
*)
fun initTypeConstraint x1 x2 lab = (TYPE_CONSTRAINT (EL.initExtLab (x1, x2) lab))
(** Creates a function type constraint. *)
fun initFunctionTypeConstraint x1 x2 lab = FUNCTION_TYPE_CONSTRAINT (EL.initExtLab (x1, x2) lab)
(** Creates a typename constraint. *)
fun initTypenameConstraint x1 x2 lab = TYPENAME_CONSTRAINT (EL.initExtLab (x1, x2) lab)
(** Creates a row constraint. *)
fun initRowConstraint x1 x2 lab = ROW_CONSTRAINT (EL.initExtLab (x1, x2) lab)
(** Creates an equality type constraint. *)
fun initEqualityTypeConstraint x1 x2 lab = EQUALITY_TYPE_CONSTRAINT (EL.initExtLab (x1, x2) lab)
(** Creates a field constraint. *)
fun initFieldConstraint x1 x2 lab = FIELD_CONSTRAINT (EL.initExtLab (x1, x2) lab)
(** Cretes a label constraint. *)
fun initLabelConstraint x1 x2 lab = LABEL_CONSTRAINT (EL.initExtLab (x1, x2) lab)
(** Creates an environment constraint. *)
fun initEnvConstraint x1 x2 lab = ENV_CONSTRAINT (EL.initExtLab (x1, x2) lab)
(** Creates a class constraint. *)
fun initClassConstraint x1 x2 lab = IDENTIFIER_CLASS_CONSTRAINT (EL.initExtLab (x1, x2) lab)

(** A value identifier accessor. *)
fun initValueIDAccessor x lab = VALUEID_ACCESSOR (EL.initExtLab x lab)
(** Builds an explicit type variable accessor. *)
fun genAccIeEm x lab = EXPLICIT_TYPEVAR_ACCESSOR (EL.initExtLab x lab)
(** Builds an equality type accessor. *)
fun initEqualityTypeAccessor x lab = EQUALITY_TYPE_ACCESSOR (EL.initExtLab x lab)
(** Bulids a type constructor accessor. *)
fun genAccItEm x lab = TYPE_CONSTRUCTOR_ACCESSOR (EL.initExtLab x lab)
(** An overloading classes accessor. *)
fun genAccIoEm x lab = OVERLOADING_CLASSES_ACCESSOR (EL.initExtLab x lab)
(** Bulids a structure accessor. *)
fun genAccIsEm x lab = STRUCTURE_ACCESSOR (EL.initExtLab x lab)
(** Builds a signature accessor. *)
fun genAccIiEm x lab = SIGNATURE_ACCESSOR (EL.initExtLab x lab)
(** Builds a functor accessor. *)
fun genAccIfEm x lab = FUNCTOR_ACCESSOR (EL.initExtLab x lab)

(** Tests whether a binding is monomorphic in nature. *)
fun isMonoBind bind = P.isMono (getBindP bind)

(** Changes the value id bindings in an environment to be monomorphic. *)
fun toMonoValueIds valueIds labs =
    mapenv (fn sems => map (fn extlab => EL.mapExtLab extlab (fn x => C.toMonoBind x (L.cons (getBindL extlab) labs))) sems)
	   valueIds

(** Changes the value id bindings in an environment to be polymorphic. *)
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

(** We have DAT here because this is only used for datatypes and datatype descriptions.
 * WARNING: typeNames here may not be typeNames! It's this env that's been going around! *)
fun toTYCONTypeNameEnv typeNames cons b labs =
    let
	(** Calls #ConsId.mapBind given a binding and a function to be applied. *)
	fun mapbind x = C.mapBind x (fn (tyf, _, _) => (tyf, DATATYPE, ref (cons, b)))
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

(** Generates an env from a long identifier and a type function *)
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


(** Checks whether the env is composed by at least an env variable. *)
fun hasEnvVar (ENV_VAR _)            = true
  | hasEnvVar (ROW_ENV (env1, env2)) = hasEnvVar env2 orelse hasEnvVar env1
  | hasEnvVar (ENVDEP extenv)       = hasEnvVar (EL.getExtLabT extenv)
  | hasEnvVar _                     = false


(** Checks the second element in infoEnv (complete env).
 * Should we check the label as well?
 * true if a the env is complete: checks the second element in infoEnv
 * of an ENVC (false if not a ENVC). *)
fun completeEnv (env as ENV_CONS _)     = getIComplete env
  | completeEnv (ROW_ENV (env1, env2)) = completeEnv env2 andalso completeEnv env1
  | completeEnv (ENVDEP extenv)       = completeEnv (EL.getExtLabT extenv)
  | completeEnv  _                    = false

(** Marks an environment as being incomplete. *)
fun toIncompleteEnv (env as ENV_CONS _) = updateIComplete false env
  | toIncompleteEnv x = x

(** Tests whether the environment in the argument is an environment variable. *)
fun isENV_VAR (ENV_VAR _) = true
  | isENV_VAR _ = false

(** Checks if the env is a ENV_CONS *)
fun isENV_CONS (ENV_CONS _) = true
  | isENV_CONS _ = false


(** Returns the labels of envs
 * n is just some debugging info *)
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

(** Returns the ids (as integers) of an env along with the associated labels.
 * The int is a debugging info.
 * This function is only used by our unification algorithm (in Unification.sml). *)
fun getLabsIdsEnv (env as ENV_CONS _) =
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
  | getLabsIdsEnv _ = raise EH.DeadBranch "" (*([], L.empty)*)


(** Returns the label associated to a env (dummylab if not an ENV_CONS).
 * This function is only used for complete envs which are ENV_CONSs.
 * It is only used in Unification.sml.*)
fun getLabEnv (env as ENV_CONS _) = getILab env
  | getLabEnv _                 = raise EH.DeadBranch "" (*L.dummyLab*)




(** Extract the type names defined in a type constructor env *)
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

(** Creates NOT_EQUALITY_TYPE constaints for types used
 * in an opaque signature. Called at constraint generation time when we see
 * that we are in fact dealing with an opaque signature *)
fun createOpaqueEqualityConstraints env lab =
    let
	(** Holds a list of the equality type variables that we have freshened. *)
 	val equalityTypeVarsToFreshen = ref []

	(** Freshens an equality type variable. *)
	fun freshenEqualityTypeVar eqtv =
	    let
		val newEqtv = T.freshEqualityTypeVar()
		val _ = equalityTypeVarsToFreshen := (eqtv,newEqtv)::(!(equalityTypeVarsToFreshen))
	    in
		newEqtv
	    end

	(** Check if an equality type variable has already been freshened, returning the new freshened one if so. *)
	fun checkForFreshening eqtv =
	    let
		(** Helper function for #checkForFreshening. *)
		fun checkForFreshening' eqtv [] = freshenEqualityTypeVar eqtv
		  | checkForFreshening' eqtv ((old,new)::t) =
		    if (T.equalityTypeVarToInt eqtv = T.equalityTypeVarToInt old)
		    then new
		    else checkForFreshening' eqtv t
	    in
		checkForFreshening' eqtv (!(equalityTypeVarsToFreshen))
	    end

	(** Gets the bindings in the list argument. Aids in the use of creating equality constraints for signatures. *)
	fun getBinds [] = []
	  | getBinds (singleBind::t) =
	    let
		val (extLabTerm, extLabLabel, extLabExtraLabels, extLabCds) = (EL.getExtLabT singleBind, EL.getExtLabL singleBind, EL.getExtLabE singleBind, EL.getExtLabD singleBind)
		val (bindId, bindBind, bindEqtv, bindClass, bindLab, bindPoly) = (C.getBindI extLabTerm, C.getBindT extLabTerm, C.getBindEqualityTypeVar extLabTerm, C.getBindC extLabTerm, C.getBindL extLabTerm, C.getBindP extLabTerm)
		val (s1, s2, s3) = bindBind

		(* if we see a type dependancy on a type constructor on the right hand side that is for a user type, freshen its type variable *)
		val s1 = case s1 of
			     T.TYPE_FUNCTION_DEPENDANCY(T.TFC(a, T.TYPE_DEPENDANCY(T.TYPE_CONSTRUCTOR(T.NC(r,p,q),w,x,T.EQUALITY_TYPE_VAR(eqtv)),b,c,d),e),f,g,h) =>
			     if T.printTypename' r = "a user type"
			     then T.TYPE_FUNCTION_DEPENDANCY(T.TFC(a, T.TYPE_DEPENDANCY(T.TYPE_CONSTRUCTOR(T.NC(r,p,q),w,x,T.EQUALITY_TYPE_VAR(checkForFreshening eqtv)),b,c,d),e),f,g,h)
			     else s1
			   | _ => s1

		val consBindBuilt = C.consBind bindId (s1,s2,s3) bindEqtv bindClass bindLab bindPoly
		val extLabBuilt   = EL.consExtLab consBindBuilt extLabLabel extLabExtraLabels extLabCds
	    in
		extLabBuilt::(getBinds t)
	    end

	(** Parses typenames for freshing of equality variables when parsing a signature. *)
	fun parseTypeNames typeNames = mapenv (fn x => getBinds x) typeNames

	(** Make sure the fresh equality type variables are mapped to NOT_EQUALITY_TYPE (this is an opaque signature, we can't check for equality *)
	fun generateEqtvConstraints [] lab = []
	  | generateEqtvConstraints ((_,new)::t) lab =
	    (initEqualityTypeConstraint (T.consEQUALITY_TYPE_VAR new) (T.EQUALITY_TYPE_STATUS (T.NOT_EQUALITY_TYPE)) lab)::(generateEqtvConstraints t lab)

	(** Parses an environment to aid in replacement of equality type variables after a signature has been defined. *)
	fun parseEnv (test as ENV_CONS {valueIds,typeNames,explicitTypeVars,structs,sigs,functors,overloadingClasses,info}) =
	    let
		val binds = parseTypeNames typeNames
	    in
		(ENV_CONS {valueIds=valueIds,typeNames=parseTypeNames typeNames,explicitTypeVars=explicitTypeVars,structs=structs,sigs=sigs,functors=functors,overloadingClasses=overloadingClasses,info=info})
	    end

	  (* we don't care about other cases like environment variable dependancies... *)
	  | parseEnv env = env
    in
	(parseEnv env, generateEqtvConstraints (!(equalityTypeVarsToFreshen)) lab)
    end

(** Same as in Filter. *)
fun testlab lab labs = L.eq lab L.dummyLab
		       orelse L.eq lab L.builtinLab
		       orelse L.isin lab labs

(** Tests whether labs1 is a subset of labs2. *)
fun testlabs labs1 labs2 = L.subseteq labs1 (L.cons L.dummyLab (L.cons L.builtinLab labs2))

(** Filters an environment given an idenv. *)
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

(** Filter an open environment by labels specified as a parameter. *)
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

(** Filter a long type constructor binder given a binder and some labels. *)
fun filterLongTypeConsBinder (longTypeConsBinder as ({lid, equalityTypeVar, sem, class, lab}, _, _, _)) labs =
    if testlab lab labs
    then SOME longTypeConsBinder
    else NONE

(** Creates an incomplete environment. *)
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

(** Filter a single constraint by the labels given in the parameter. *)
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

(** filters somehting of type CONSTRAINTS by some labels given in the function argument. *)
and filterCst (CONSTRAINTS oneConstraint) labs =
    CONSTRAINTS (OMC.mapPartiali (fn (key, cs) =>
			      if testlab (L.fromInt key) labs
			      then case List.mapPartial (fn oneConstraint => filterOcst oneConstraint labs) cs of
				       []  => NONE
				     | cs' => SOME cs'
			      else NONE)
			  oneConstraint)

(** A function which will filter envirnomnts by the labels given in the second parameter to this function. *)
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

(** Filters the constraintss that are not in the label set. *)
val filterEnv = fn env => fn labs => case filterEnv env labs of NONE => updateIComplete false emptyEnv | SOME env' => env'

end
