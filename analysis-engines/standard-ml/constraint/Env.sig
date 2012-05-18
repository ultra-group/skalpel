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
 *  o Date:        24 May 2010
 *  o File name:   Env.sig
 *  o Description: Defines the ENV signature which is the signature of
 *      our constraint system.
 *)


signature ENV = sig

    (* ====== TYPES AND DATATYPES ====== *)

    (* ------ MAPPINGS ------ *)
    type 'a constraintMap
    type 'a envMap
    type 'a openEnvMap

    (* ------ VARIABLES ------ *)
    type envVar         = int

    (* ------ BINDERS ------ *)
    type 'a bind        = 'a ConsId.bind ExtLab.extLab

    (* ------ GENERIC ENVIRONMENT ------ *)
    type 'a genericEnv        = 'a bind list envMap

    (* ------ OPENENV ------ *)

    datatype openKind    = OPENED_STRUCT
			 | DATATYPE_REPLICATION
			 | INCLUDED_SIG

    type openSem         = Id.lid * Label.label * openKind

    type openEnv         = openSem openEnvMap

    (* ------ VARENV ------ *)
    type extVar         = Ty.ty bind
    type varEnv         = Ty.ty genericEnv

    (* ------ KIND OF A TYPE DECLARATION *)
    datatype typeNameKind     = DATATYPE | TYPE

    (* ------ TYPENV ------ *)
    type extType         = (Ty.typeFunction * typeNameKind * (varEnv * bool) ref) bind
    type typeEnv         = (Ty.typeFunction * typeNameKind * (varEnv * bool) ref) genericEnv

    (* ------ OVERLADINGENV ------ *)
    type extovc         = Ty.rowType bind
    type overloadingClassesEnv = Ty.rowType genericEnv

    (* ------ TYPEVARENV ------ *)
    type explicitTypeVar = (Ty.typeVar * bool) bind
    type typeVarEnv         = (Ty.typeVar * bool) genericEnv

    (* ------ INFORMATION ON ENVS ------ *)
    type typeName          = {id : Id.id, lab : Label.label, kind : typeNameKind, name : Ty.typename}
    type typeNameMap       = typeName list
    datatype names      = TYPENAME    of typeName ExtLab.extLab
			| DUMTYPENAME of typeName
			| MAYTYPENAME
			| NOTTYPENAME of Id.id ExtLab.extLab

    type infoEnv        = {lab : Label.label,
			   complete : bool,
			   infoTypeNames : typeNameMap,
			   argOfFunctor : bool}

    type class = ClassId.class

    (* ------ ACCESSORS ------ *)
    type 'a accid       = {lid : Id.lid, sem : 'a, class : ClassId.class, lab : Label.label}

    (* ------ LONG TYPE CONSTRUCTOR BINDER ------ *)
    type longTypeConsBinder        = Ty.typeFunction accid ExtLab.extLab

    (* ------ ENRICHEMENT AND INSTANTIATION ------ *)
    type evsbind        = envVar * envVar option * envVar * envVar option * Label.label

    (* ------ FUNCTOR INSTANTIATION ------ *)
    type evfbind        = envVar * envVar * envVar * envVar * Label.label

    (* ------ SHARING ------ *)
    type shabind        = envVar * envVar * envVar * Label.label

    (* ------ SIGNATURE MATCHING ------ *)
    datatype matchKind  = OPAQUE
			| TRANSLUCENT

    (* ------ ENVS ------ *)
    datatype env        = ENV_CONS of {valueIds : varEnv,
				       typeNames : typeEnv,
				       explicitTypeVars : typeVarEnv,
				       structs : env genericEnv,
				       sigs : env genericEnv,
				       functors : (env * env) genericEnv,
				       overloadingClasses : overloadingClassesEnv,
				       info : infoEnv}
			| ENV_VAR of envVar * Label.label
			| ROW_ENV of env * env
			| LOCAL_ENV of env * env
			| ENVSHA of env * env
			| SIGNATURE_ENV of env * env * matchKind
			| ENVWHR of env * longTypeConsBinder
			| ENVPOL of typeVarEnv * env
			| DATATYPE_CONSTRUCTOR_ENV of Id.labelledId * env
			| ENVOPN of openEnv
			| ENVDEP of env ExtLab.extLab
			| FUNCTOR_ENV of constraints
			| CONSTRAINT_ENV of constraints
			| ENVPTY of string
			| ENVFIL of string * env * (unit -> env)
			| TOP_LEVEL_ENV

	and accessor        = VALUEID_ACCESSOR of Ty.ty       accid ExtLab.extLab
			    | EXPLICIT_TYPEVAR_ACCESSOR of Ty.typeVar    accid ExtLab.extLab
			    | TYPE_CONSTRUCTOR_ACCESSOR of Ty.typeFunction    accid ExtLab.extLab
			    | OVERLOADING_CLASSES_ACCESSOR of Ty.rowType    accid ExtLab.extLab
			    | STRUCTURE_ACCESSOR of env         accid ExtLab.extLab
			    | SIGNATURE_ACCESSOR of env         accid ExtLab.extLab
			    | FUNCTOR_ACCESSOR of (env * env) accid ExtLab.extLab

	 and oneConstraint       = TYPE_CONSTRAINT of (Ty.ty    * Ty.ty)    ExtLab.extLab
				 | TYPENAME_CONSTRAINT of (Ty.typenameType  * Ty.typenameType)  ExtLab.extLab
				 | ROW_CONSTRAINT of (Ty.rowType * Ty.rowType) ExtLab.extLab
				 | FIELD_CONSTRAINT of (Ty.fieldType * Ty.fieldType) ExtLab.extLab
				 | LABEL_CONSTRAINT of (Ty.labelType * Ty.labelType) ExtLab.extLab
				 | ENV_CONSTRAINT of (env      * env)      ExtLab.extLab
				 | IDENTIFIER_CLASS_CONSTRAINT of (class    * class)    ExtLab.extLab
				 | FUNCTION_TYPE_CONSTRAINT of (Ty.typeFunction * Ty.typeFunction) ExtLab.extLab
				 | ACCESSOR_CONSTRAINT of accessor
				 | LET_CONSTRAINT of env
				 | SIGNATURE_CONSTRAINT of evsbind
				 | FUNCTOR_CONSTRAINT of evfbind
				 | SHARING_CONSTRAINT of shabind
				 | EQUALITY_TYPE_CONSTRAINT of (Ty.ty * Ty.ty) ExtLab.extLab

	 and constraints        = CONSTRAINTS of oneConstraint list constraintMap

    type extstr = env bind
    type strenv = env genericEnv

    type extsig = env bind
    type sigenv = env genericEnv

    type funsem = env * env
    type extfun = funsem bind
    type funenv = funsem genericEnv

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
    type contextSensitiveSyntaxError = oneContextSensitiveSyntaxError list

    type envContextSensitiveSyntaxPair = env * contextSensitiveSyntaxError

    (* ====== FUNCTIONS ====== *)

    val resetEnvVar  : unit  -> unit

    val freshEnvVar  : unit -> envVar
    val getEnvVar    : unit -> envVar

    val envVarToInt  : envVar -> int

    val eqEnvVar     : envVar -> envVar -> bool

    val newEnvVar    : Label.label -> env
    val consENV_VAR   : envVar -> Label.label -> env

    val consEnvConstructor     : varEnv  ->
		       typeEnv  ->
		       typeVarEnv  ->
		       strenv  ->
		       sigenv  ->
		       funenv  ->
		       overloadingClassesEnv  ->
		       infoEnv ->
		       env
    val consInfo     : Label.label ->
		       bool        ->
		       typeNameMap       ->
		       bool        ->
		       infoEnv

    val emptyEnv : env
    val emvar        : varEnv
    val emtyp        : typeEnv
    val emstr        : strenv
    val emsig        : sigenv
    val emfun        : funenv
    val emopn        : openEnv
    val emtv         : typeVarEnv
    val emoc         : overloadingClassesEnv
    val emptyMap     : 'a envMap
    val emnfo        : infoEnv

    val consBind     : Id.id -> 'a -> ClassId.class -> Label.label -> Poly.poly -> 'a bind
    val consBindPoly : {id : Id.id, typeOfId : 'a, classOfId : ClassId.class, labelOfConstraint : Label.label} -> 'a bind
    val consBindMono : Id.id -> 'a -> ClassId.class -> Label.label              -> 'a bind

    val consAccId    : Id.lid -> 'a -> ClassId.class -> Label.label -> 'a accid

    val getBindI     : 'a bind -> Id.id
    val getBindT     : 'a bind -> 'a
    val getBindC     : 'a bind -> ClassId.class
    val getBindL     : 'a bind -> Label.label
    val getBindP     : 'a bind -> Poly.poly

    val getValueIds           : env -> varEnv
    val getTypeNameEnv        : env -> typeEnv
    val getExplicitTypeVars   : env -> typeVarEnv
    val getStructs            : env -> strenv
    val getSigs               : env -> sigenv
    val getFunctors           : env -> funenv
    val getOverloadingClasses      : env -> overloadingClassesEnv
    val getInfo               : env -> infoEnv
    val getILab               : env -> Label.label
    val getIComplete          : env -> bool
    val getITypeNames         : env -> typeNameMap
    val getIArgOfFunctor      : env -> bool

    val projValueIds     : varEnv -> env
    val consEnvTypeNames : typeEnv -> env
    val projExplicitTypeVars     : typeVarEnv -> env
    val projStructs     : strenv -> env
    val projSigs     : sigenv -> env
    val projFunctors     : funenv -> env
    val projOpns     : openEnv -> env
    val projOverloadingClasses     : overloadingClassesEnv  -> env

    val updateValueIds   : varEnv -> env -> env
    val updateTypeNames : typeEnv -> env -> env
    val updateExplicitTypeVars   : typeVarEnv -> env -> env
    val updateStructs   : strenv -> env -> env
    val updateOverloadingClasses   : overloadingClassesEnv  -> env -> env
    val updateILab   : Label.label -> env -> env
    val updateIComplete   : bool   -> env -> env
    val updateInfoTypeNames : typeNameMap  -> env -> env
    val updateIArgOfFunctor   : bool   -> env -> env

    val getTypeNames   : typeEnv -> names list
    val getEqualityTypeVars : constraints -> Ty.typeVar list

    val plusEnv            : env -> env -> env

    val pushExtEnv         : env -> Label.labels -> Label.labels -> LongId.set -> env

    val isEmptyIdEnv       : 'a envMap  -> bool
    val isEmptyEnv         : env      -> bool

    val genLongEnv    : Id.lid -> Ty.typeFunction -> constraints * env

    val hasEnvVar     : env -> bool

    val completeEnv     : env -> bool

    val isENV_VAR        : env -> bool

    val isENV_CONS          : env -> bool

    val getLabsIdsEnv : env -> int -> (int * int) list * Label.labels

    val getLabEnv     : env -> Label.label

    val filterEnv     : env -> Label.labels -> env

    val addenv        : (Id.id * 'a) -> 'a envMap -> 'a envMap
    val consSingleEnv : (Id.id * 'a) -> 'a envMap
    val plusproj     : 'a genericEnv -> Id.id -> 'a bind list
    val mapenv       : ('a -> 'a) -> 'a envMap -> 'a envMap
    val unionEnvList         : 'a genericEnv list -> 'a genericEnv
    val plusenv      : 'a genericEnv -> 'a genericEnv -> 'a genericEnv
    val foldrienv    : ((Id.id * 'a * 'b) -> 'b) -> 'b -> 'a envMap -> 'b
    val dom          : 'a envMap -> Id.set

    val isMonoBind   : 'a bind -> bool

    val toMonoValueIds   : varEnv -> Label.labels -> varEnv
    val toPolyValueIds   : varEnv -> varEnv
    val toRECValueIds    : varEnv -> Label.labels -> varEnv
    val toPATValueIds    : varEnv -> Label.labels -> varEnv
    val toEX0ValueIds    : varEnv -> Label.labels -> varEnv
    val toEX1ValueIds    : varEnv -> Label.labels -> varEnv
    val toDA0ValueIds    : varEnv -> Label.labels -> varEnv
    val toDA1ValueIds    : varEnv -> Label.labels -> varEnv
    val toDATValueIds    : varEnv -> Label.labels -> varEnv
    val toCLSValueIds    : varEnv -> ClassId.class -> Label.labels -> varEnv
    val toTYCONTypeNameEnv  : typeEnv -> varEnv -> bool -> Label.labels -> typeEnv

    val closeValueIds    : varEnv -> Expans.nonexp -> varEnv

    val allEqualValueIds : varEnv -> constraints

    val bindToEnv    : 'a bind list -> 'a genericEnv
    val envToBind    : 'a genericEnv      -> 'a bind list

    val unionEnv      : env list -> env

    val envsToSeq    : env list -> env

    val appOEnv      : (openSem -> unit) -> openEnv -> unit
    val addOEnv      : openSem -> openEnv -> openEnv
    val singOEnv     : openSem -> openEnv
    val uOEnv        : openEnv list -> openEnv
    val foldlOEnv    : ((openSem * 'b) -> 'b) -> 'b -> openEnv -> 'b

    (* ------ FUNCTIONS FOR CONSTRAINTS ------*)
    val emptyContextSensitiveSyntaxError            : contextSensitiveSyntaxError
    val emptyConstraint  : constraints
    val consConstraint   : (Label.label * oneConstraint) -> constraints -> constraints
    val conscsts         : (Label.label * oneConstraint list) -> constraints -> constraints
    val singleConstraint : (Label.label * oneConstraint) -> constraints
    val singcss          : oneContextSensitiveSyntaxError -> contextSensitiveSyntaxError
    val singcsss         : oneContextSensitiveSyntaxError list -> contextSensitiveSyntaxError
    val singcsts         : (Label.label * oneConstraint list) -> constraints
    val unionContextSensitiveSyntaxErrors: contextSensitiveSyntaxError list -> contextSensitiveSyntaxError
    val unionConstraintsList : constraints list -> constraints
    val getnbcs          : envContextSensitiveSyntaxPair -> int
    val getnbcss         : envContextSensitiveSyntaxPair -> int
    val getnbcst         : envContextSensitiveSyntaxPair -> int
    val getnbcsttop      : envContextSensitiveSyntaxPair -> int
    val foldlicst        : ((int * oneConstraint list * 'b) -> 'b) -> 'b -> constraints -> 'b
    val getbindings      : env -> Label.labels list * Label.labels

    val genCstTyAll  : Ty.ty     -> Ty.ty    -> Label.labels -> Label.labels -> LongId.set -> oneConstraint
    val genCstTfAll  : Ty.typeFunction  -> Ty.typeFunction -> Label.labels -> Label.labels -> LongId.set -> oneConstraint
    val genCstTnAll  : Ty.typenameType   -> Ty.typenameType  -> Label.labels -> Label.labels -> LongId.set -> oneConstraint
    val genCstSqAll  : Ty.rowType  -> Ty.rowType -> Label.labels -> Label.labels -> LongId.set -> oneConstraint
    val genCstRtAll  : Ty.fieldType  -> Ty.fieldType -> Label.labels -> Label.labels -> LongId.set -> oneConstraint
    val genCstLtAll  : Ty.labelType  -> Ty.labelType -> Label.labels -> Label.labels -> LongId.set -> oneConstraint
    val genCstEvAll  : env       -> env      -> Label.labels -> Label.labels -> LongId.set -> oneConstraint
    val genCstClAll  : class     -> class    -> Label.labels -> Label.labels -> LongId.set -> oneConstraint

    val genValueIDAccessor  : Ty.ty    accid -> Label.labels -> Label.labels -> LongId.set -> accessor

    val initTypeConstraint         : Ty.ty    -> Ty.ty    -> Label.label -> oneConstraint
    val initFunctionTypeConstraint : Ty.typeFunction -> Ty.typeFunction -> Label.label -> oneConstraint
    val initTypenameConstraint     : Ty.typenameType  -> Ty.typenameType  -> Label.label -> oneConstraint
    val initRowConstraint          : Ty.rowType -> Ty.rowType -> Label.label -> oneConstraint
    val initEqualityTypeConstraint : Ty.ty -> Ty.ty -> Label.label -> oneConstraint
    val initFieldConstraint        : Ty.fieldType -> Ty.fieldType -> Label.label -> oneConstraint
    val initLabelConstraint        : Ty.labelType -> Ty.labelType -> Label.label -> oneConstraint
    val initEnvConstraint          : env      -> env      -> Label.label -> oneConstraint
    val initClassConstraint        : class    -> class    -> Label.label -> oneConstraint

    val initValueIDAccessor   : Ty.ty    accid -> Label.label -> accessor
    val genAccIeEm   : Ty.typeVar accid -> Label.label -> accessor
    val genAccItEm   : Ty.typeFunction accid -> Label.label -> accessor
    val genAccIoEm   : Ty.rowType accid -> Label.label -> accessor
    val genAccIsEm   : env      accid -> Label.label -> accessor
    val genAccIiEm   : env      accid -> Label.label -> accessor
    val genAccIfEm   : funsem   accid -> Label.label -> accessor

    val printEnv     : env    -> string -> string
    val printConstraints : constraints -> string
    val printOneConstraint : oneConstraint -> string
    val printOneAccessor : accessor -> string
    val createEqualityTypeConstraints : constraints -> constraints
end
