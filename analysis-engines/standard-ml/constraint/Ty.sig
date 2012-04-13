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
 * along with Skalpel. If not, see <http://www.gnu.org/licenses/>.
 *
 *  o Authors:     Vincent Rahli, John Pirie
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        24 May 2010
 *  o File name:   Ty.sig
 *  o Description: This file defines the signature TY specifying the
 *                 internal types used by our slicer.
 *)


signature TY = sig

    type typeVar
    type sequenceVar
    type typenameVar
    type labelVar
    type rowVar
    type typeFunctionVar

    type typename
    type idor
    type labcons    = string
    type flex       = Label.label option
    type extv       = Id.labelledId option

    type explicitTypeVar   = typeVar ExtLab.extLab

    val eqTypeTyVars : typeVar list ref

    datatype poly   = POLY
		    | MONO

    datatype constructorKind  = DECLARATION_CONS of Id.id
			      | PATTERN_CONS
			      | OTHER_CONS
			      | BUILTIN_BASIS_CONS

    datatype orKind = VALUE of Id.labelledId
		    | CONSTANT of string * Id.id * Label.label

    datatype labelType = LABEL_VAR  of labelVar
		       | LC  of labcons * Label.label
		       | LABEL_DEPENDANCY  of labelType ExtLab.extLab

    datatype typenameType = TYPENAME_VAR  of typenameVar
			  | NC  of typename * constructorKind * Label.label
			  | TYPENAME_DEPENDANCY  of typenameType ExtLab.extLab

    datatype rowType = ROW_VAR  of rowVar
		     | RC  of labelType * ty * Label.label
		     | ROW_DEPENDANCY  of rowType ExtLab.extLab
		     | ROW_NO_OVERLOAD

	 and sequenceType = SEQUENCE_VAR  of sequenceVar
			  | SC  of rowType list * flex  * Label.label
			  | SEQUENCE_DEPENDANCY  of sequenceType ExtLab.extLab

	 and typeFunction = TYPE_FUNCTION_VAR of typeFunctionVar
			  | TFC of sequenceType * ty * Label.label
			  | TYPE_FUNCTION_DEPENDANCY of typeFunction ExtLab.extLab

	 and ty = TYPE_VAR          of typeVar  * extv  * poly
                | EXPLICIT_TYPE_VAR of Id.id  * typeVar * Label.label
		| TYPE_CONSTRUCTOR       of typenameType   * sequenceType * Label.label
		| APPLICATION            of typeFunction  * sequenceType * Label.label
		| TYPE_POLY              of sequenceType  * idor  * poly * orKind * Label.label
		| GEN                    of ty list ref
		| TYPE_DEPENDANCY        of ty ExtLab.extLab


    datatype names = TYPENAME of typename | DUMTYPENAME of typename | MAYTYPENAME | NOTTYPENAME

    val typenames           : string list

    val consTypenameVar     : Label.label -> ty

    val consTYPE_VAR             : typeVar  -> ty
    val consSEQUENCE_VAR         : sequenceVar -> sequenceType
    val consTYPE_FUNCTION_VAR    : typeFunctionVar -> typeFunction

    val newTYPE_VAR           : unit -> ty
    val newROW_VAR            : unit -> rowType
    val newSEQUENCE_VAR       : unit -> sequenceType
    val newTYPE_FUNCTION_VAR  : unit -> typeFunction

    val typeVarToInt        : typeVar     -> int
    val typeFunctionVarToInt       : typeFunctionVar    -> int
    val typenameVarToInt    : typenameVar -> int
    val typenameToInt       : typename    -> int
    val labelVarToInt       : labelVar    -> int
    val rowVarToInt       : rowVar    -> int
    val sequenceVarToInt       : sequenceVar    -> int
    val idorToInt         : idor      -> int

    val typenameFromInt     : int -> typename

    val eqTypeVar           : typeVar     -> typeVar     -> bool
    val eqSequenceVar          : sequenceVar    -> sequenceVar    -> bool
    val eqLabelVar          : labelVar    -> labelVar    -> bool
    val eqRowVar          : rowVar    -> rowVar    -> bool
    val eqTypename          : typename    -> typename    -> bool
    val eqTypenameVar       : typenameVar -> typenameVar -> bool
    val eqIdor            : idor      -> idor      -> bool

    val isTypename          : typeFunction -> names ExtLab.extLab

    val getTypename     : typeFunction -> typename option

    val noflex            : unit -> flex
    val consflex          : Label.label -> flex
    val isflex            : flex -> bool
    val getflex           : flex -> Label.label
    val printflex         : flex -> string

    val DUMMYTYPENAME       : typename
    val CONSREAL          : typename

    val constuple         : typeVar list -> Label.label -> rowType list

    val constyint         : Label.label -> ty
    val constyword        : Label.label -> ty
    val constyreal        : Label.label -> ty
    val constybool        : Label.label -> ty
    val constystring      : Label.label -> ty
    val constychar        : Label.label -> ty
    val constyexception   : Label.label -> ty
    val constyunit        : Label.label -> ty
    val constynewcons     : Label.label -> ty
    val constysubstring   : Label.label -> ty
    val constyorder       : Label.label -> ty
    val constylist        : typeVar -> Label.label -> ty
    val constyref         : typeVar -> Label.label -> ty
    val constyarray       : typeVar -> Label.label -> ty
    val constyvector      : typeVar -> Label.label -> ty
    val constyoption      : typeVar -> Label.label -> ty
    val constyfrag        : typeVar -> Label.label -> ty
    val constyarrow       : typeVar -> typeVar -> Label.label -> ty
    val constytuple       : typeVar list -> Label.label -> ty
    val constyrecord      : rowVar list -> Label.label option -> Label.label -> ty

    val constyint'        : Label.label -> constructorKind -> ty
    val constyword'       : Label.label -> constructorKind -> ty
    val constyreal'       : Label.label -> constructorKind -> ty
    val constybool'       : Label.label -> constructorKind -> ty
    val constystring'     : Label.label -> constructorKind -> ty
    val constychar'       : Label.label -> constructorKind -> ty
    val constyexception'  : Label.label -> constructorKind -> ty
    val constyunit'       : Label.label -> constructorKind -> ty
    val constynewcons'    : Label.label -> constructorKind -> ty
    val constysubstring'  : Label.label -> constructorKind -> ty
    val constyorder'      : Label.label -> constructorKind -> ty
    val constylist'       : typeVar -> Label.label -> constructorKind -> ty
    val constyref'        : typeVar -> Label.label -> constructorKind -> ty
    val constyarray'      : typeVar -> Label.label -> constructorKind -> ty
    val constyvector'     : typeVar -> Label.label -> constructorKind -> ty
    val constyoption'     : typeVar -> Label.label -> constructorKind -> ty
    val constyfrag'       : typeVar -> Label.label -> constructorKind -> ty
    val constyarrow'      : typeVar -> typeVar -> Label.label -> constructorKind -> ty
    val constytuple'      : typeVar list -> Label.label -> constructorKind -> ty
    val constyrecord'     : rowVar list -> Label.label option -> Label.label -> constructorKind -> ty

    val consTyArrowTy     : ty -> ty -> Label.label -> constructorKind -> ty
    val consTyTupleTy     : ty list  -> Label.label -> constructorKind -> ty

    val labelBuiltinTy    : ty    -> Label.label -> ty
    val labelBuiltinTyf   : typeFunction -> Label.label -> typeFunction

    val isTyC             : ty -> bool
    val isTyV             : ty -> bool
    val getTyLab          : ty -> Label.label option
    val getTypenameType   : ty -> typenameType option

    val isBase            : typename -> bool
    val isBase'           : typename -> bool
    val isBaseTy          : typenameType   -> bool
    val isArrowTy         : typenameType   -> bool
    val isExcTy           : typenameType   -> bool
    val isDecTy           : typenameType   -> bool
    val isPatTy           : typenameType   -> bool
    val isVarTypename       : typenameType   -> bool
    val isShallowSeq      : sequenceType  -> bool

    val freshTypeVar         : unit -> typeVar
    val freshTypeFunctionVar : unit -> typeFunctionVar
    val freshSequenceVar     : unit -> sequenceVar
    val freshTypenameVar    : unit -> typenameVar
    val freshLabelVar       : unit -> labelVar
    val freshRowVar       : unit -> rowVar
    val freshTypename       : unit -> typename
    val freshidor         : unit -> idor

    val resetnexts        : unit -> unit

    val getTypeVar        : unit -> typeVar
    val getsequenceVar    : unit -> sequenceVar
    val getRowVar         : unit -> rowVar
    val getLabelVar       : unit -> labelVar
    val getTypenameVar      : unit -> sequenceVar
    val getidor           : unit -> idor

    val getTypenameString   : string -> typename

    val getTypeVarsTy     : ty -> explicitTypeVar list

    val stripDepsSq       : sequenceType -> sequenceType ExtLab.extLab

    val tntyToTyCon       : typenameType -> typename

    val printTypenameAssoc  : typename -> Id.assoc -> string

    val isPoly            : poly -> bool

    val printTypeVar        : typeVar       -> string
    val printTypeVarList    : typeVar list  -> string
    val printSequenceVar       : sequenceVar      -> string
    val printSequenceVarList   : sequenceVar list -> string
    val printTypenameVar    : typenameVar   -> string
    val printTypename       : typename      -> string
    val printTypename'      : typename      -> string
    val printsmltn        : typename      -> string
    val printseqty        : sequenceType       -> string
    val printseqty'       : sequenceType       -> string
    val printtnty         : typenameType        -> string
    val printtnty'        : typenameType        -> string
    val printtyf         : typeFunction       -> string
    val printtyf'         : typeFunction       -> string
    val printty           : ty          -> string
    val printty'          : ty          -> string
    val printtylist       : ty list     -> string
    val printlabcons      : labcons     -> string
    val printsmllc        : labcons     -> string
    val printRowType        : rowType       -> string
    val printrowty'       : rowType       -> string
    val printrowtylist    : rowType list  -> string
    val printRowVar       : rowVar      -> string
    val printlabty        : labelType       -> string

end
