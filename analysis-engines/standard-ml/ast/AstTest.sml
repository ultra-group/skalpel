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
 *  o File name:   AstTest.sml
 *  o Description: Defines the structure AstTest which has signature
 *      ASTTEST, used to generate SML code.
 *)


structure AstTest :> ASTTEST = struct

(* abbreviate structure names *)
structure VT = VTimer
structure EH = ErrorHandler

fun genNewProgs _ =
    let

	(* timer function (start, get, check not over a limit)*)
	val timer = VT.startTimer ()

	fun getTime timer = Int.fromLarge (VT.getMicroTime timer)

	fun checkTime timer nb =
	    let
		val time = getTime timer
		(*val _ = Debug.printdebug2 (Int.toString time)*)
		val ret = if time > 100000 then 0 else time mod nb
	    in ret
	    end

	(* the function called at the 'in' statement, callls genProgList for now *)
	 fun genProgs _ = genProgList ()

	 and genProgList _ =
	     case checkTime timer 2 of
		 0 => ""
	       | 1 => genProg () ^ "\n" ^ genProgList ()
	       | _ => raise EH.DeadBranch ""

	 and genProg _ = genProgOneList ()

	 (* if timer expired, then return empty string, otherwise go ahead and generate *)
	and genProgOneList _ =
	    case checkTime timer 2 of
		0 => ""
	      | 1 => genProgOne () ^ ";\n" ^ genProgOneList ()
	      | _ => raise EH.DeadBranch ""

	(* if timer expired, then generate expressions, otherwise just generate the top decs *)
	and genProgOne _ =
	    case checkTime timer 2 of
		0 => genExp ()
	      | 1 => genTopDec ()
	      | _ => raise EH.DeadBranch ""

	and genTopDec _ = genTopDecOneList ()

	and genTopDecOneList _ =
	    case checkTime timer 2 of
		0 => ""
	      | 1 => genTopDecOne () ^ "\n" ^ genTopDecOneList ()
	      | _ => raise EH.DeadBranch ""

	and genTopDecOne _ =
	    case checkTime timer 3 of
		0 => genSigDec ()
	      | 1 => genStrDec ()
	      | 2 => genFunDec ()
	      | _ => raise EH.DeadBranch ""

	(* functor and its binding *)
	and genFunDec _ = "functor " ^ genFunBind ()

	and genFunBind _ = genFunBindOneList ()

	and genFunBindOneList _ =
	    case checkTime timer 2 of
		0 => genFBO ()
	      | 1 => genFBO () ^ "\n" ^ genFunBindOneList ()
	      | _ => raise EH.DeadBranch ""

	and genFBO _ =
	    case checkTime timer 6 of
		0 => genFunId () ^ " (" ^ genStrId () ^ " : " ^ genSigExp () ^ ") = " ^ genStrExp ()
	      | 1 => genFunId () ^ " (" ^ genStrId () ^ " : " ^ genSigExp () ^ ") :> " ^ genSigExp () ^ " = " ^ genStrExp ()
	      | 2 => genFunId () ^ " (" ^ genStrId () ^ " : " ^ genSigExp () ^ ") : " ^ genSigExp () ^ " = " ^ genStrExp ()
	      | 3 => genFunId () ^ " (" ^ genSpec () ^ ") = " ^ genStrExp ()
	      | 4 => genFunId () ^ " (" ^ genSpec () ^ ") :> " ^ genSigExp () ^ " = " ^ genStrExp ()
	      | 5 => genFunId () ^ " (" ^ genSpec () ^ ") : " ^ genSigExp () ^ " = " ^ genStrExp ()
	      | _ => raise EH.DeadBranch ""


	(* signature and binding *)
	and genSigDec _ = "signature " ^ genSigBind ()

	and genSigBind _ = genSigBindOneList ()

	(* we need something for empty *)
	and genSigBindOneList _ =
	    case checkTime timer 2 of
		0 => genSigBindOne ()
	      | 1 => genSigBindOne () ^ " and " ^ genSigBindOneList ()
	      | _ => raise EH.DeadBranch ""

	and genSigBindOne _ = genSigId () ^ " = " ^ genSigExp ()

	and genStrDec _ = genStrDecOneList ()

	and genStrDecOneList _ =
	    case checkTime timer 2 of
		0 => ""
	      | 1 => genStrDecOne () ^ "\n" ^ genStrDecOneList () (* we can have a semicolon here *)
	      | _ => raise EH.DeadBranch ""

	and genStrDecOne _ =
	    case checkTime timer 3 of
		0 => genDecs () ^ ";"
	      | 1 => "structure " ^ genStrBind ()
	      | 2 => "local " ^ genStrDec () ^ " in " ^ genStrDec () ^ " end"
	      | _ => raise EH.DeadBranch ""

	(* structure and binding *)
	and genStrBind _ = genStrBindOneList ()

	and genStrBindOneList _ =
	    case checkTime timer 2 of
		0 => genStrBO ()
	      | 1 => genStrBO () ^ " and " ^ genStrBindOneList ()
	      | _ => raise EH.DeadBranch ""

	and genStrBO _ =
	    case checkTime timer 3 of
		0 => genStrId () ^ " :> " ^ genSigExp () ^ " = " ^ genStrExp ()
	      | 1 => genStrId () ^ " : " ^ genSigExp () ^ " = " ^ genStrExp ()
	      | 2 => genStrId () ^ " = " ^ genStrExp ()
	      | _ => raise EH.DeadBranch ""

	and genLTReaDO _ = genLDatName () ^ " = " ^ genLabType ()

	and genLTReaDOneList _ =
	    case checkTime timer 2 of
		0 => genLTReaDO ()
	      | 1 => genLTReaDO () ^ " and " ^ genLTReaDOneList ()
	      | _ => raise EH.DeadBranch ""

	and genLTReaDesc _ = genLTReaDOneList ()

	and genTReaDO _ = genDatName () ^ " = " ^ genLabType ()

	and genTReaDOneList _ =
	    case checkTime timer 2 of
		0 => genTReaDO ()
	      | 1 => genTReaDO () ^ " and " ^ genTReaDOneList ()
	      | _ => raise EH.DeadBranch ""

	and genTReaDesc _ = genTReaDOneList ()

	(* generate signatue expressions *)
	and genSigExp _ =
	    case checkTime timer 3 of
		0 => "sig " ^ genSpec () ^ " end"
	      | 1 => genSigId ()
	      | 2 => genSigExp () ^ " where type " ^ genLTReaDesc ()
	      | _ => raise EH.DeadBranch ""

	(* take care of the transparant/opaque signatures, we want these too *)
	and genStrExp _ =
	    case checkTime timer 6 of
		0 => "struct " ^ genStrDec () ^ " end"
	      | 1 => genStrExp () ^ " :> " ^ genSigExp ()
	      | 2 => genStrExp () ^ " : " ^ genSigExp ()
	      | 3 => genFunId () ^ " (" ^ genStrExp () ^ ")"
	      | 4 => genFunId () ^ " (" ^ genStrDec () ^ ")"
	      | 5 => "let " ^ genStrDec () ^ " in " ^ genStrExp () ^ " end"
	      | _ => raise EH.DeadBranch ""

	and genSpec _ = genSpecOneList ()

	and genSpecOneList _ =
	    case checkTime timer 2 of
		0 => ""
	      | 1 => genSpecOne () ^ "\n" ^ genSpecOneList () (* we can have a semicolon *)
	      | _ => raise EH.DeadBranch ""

	and genSpecOne _ =
	    case checkTime timer 9 of
		0 => "val "       ^ genValDesc ()
	      | 1 => "type "      ^ genTypDesc ()
	      | 2 => "eqtype"     ^ genTypDesc ()
	      | 3 => "exception " ^ genExcDesc ()
	      | 4 => "type "      ^ genTdrDesc ()
	      | 5 => "datatype "  ^ genDatDesc ()
	      | 6 => "structure " ^ genStrDesc ()
	      | 7 => "include "   ^ genSigExp ()
	      | 8 => "datatype "  ^ genTyCon () ^ " = datatype " ^ genLongTyCon ()
	      | _ => raise EH.DeadBranch ""

	and genStrDesc _ = genStrDescOneList ()

	and genStrDescOneList _ =
	    case checkTime timer 2 of
		0 => genStrDescOne ()
	      | 1 => genStrDescOne () ^ " and " ^ genStrDescOneList ()
	      | _ => raise EH.DeadBranch ""

	(* generate the signature expression to go along with the structure *)
	and genStrDescOne _ = genStrId () ^ " : " ^ genSigExp ()

	and genDatDesc _ = genDatDescOneList ()

	and genDatDescOneList _ =
	    case checkTime timer 2 of
		0 => genDatDescOne ()
	      | 1 => genDatDescOne () ^ " and " ^ genDatDescOneList ()
	      | _ => raise EH.DeadBranch ""

	and genDatDescOne _ = genDatName () ^ " = " ^ genConDesc ()

	and genConDesc _ = genConDescOneList ()

	and genConDescOneList _ =
	    case checkTime timer 2 of
		0 => genConDescOne ()
	      | 1 => genConDescOne () ^ " and " ^ genConDescOneList ()
	      | _ => raise EH.DeadBranch ""

	and genConDescOne _ =
	    case checkTime timer 2 of
		0 => genIdent ()
	      | 1 => genLabId () ^ " of " ^ genLabType ()
	      | _ => raise EH.DeadBranch ""

	and genValDesc _ = genValDescOneList ()

	and genValDescOneList _ =
	    case checkTime timer 2 of
		0 => genValDescOne ()
	      | 1 => genValDescOne () ^ " and " ^ genValDescOneList ()
	      | _ => raise EH.DeadBranch ""

	and genValDescOne _ = genLabId () ^ " : " ^ genLabType ()

	and genTypDesc _  = genTypDescOneList ()

	and genTypDescOneList _ =
	    case checkTime timer 2 of
		0 => genTypDescOne ()
	      | 1 => genTypDescOne () ^ " and " ^ genTypDescOneList ()
	      | _ => raise EH.DeadBranch ""

	and genTypDescOne _ = genDatName ()

	and genTdrDesc _ = genTdrDescOneList ()

	and genTdrDescOneList _ =
	    case checkTime timer 2 of
		0 => genTdrDescOne ()
	      | 1 => genTdrDescOne () ^ " and " ^ genTdrDescOneList ()
	      | _ => raise EH.DeadBranch ""

	and genTdrDescOne _ = genDatName () ^ " = " ^ genLabType ()

	and genExcDesc _ = genExcDescOneList ()

	and genExcDescOneList _ =
	    case checkTime timer 2 of
		0 => genExcDescOne ()
	      | 1 => genExcDescOne () ^ " and " ^ genExcDescOneList ()
	      | _ => raise EH.DeadBranch ""

	and genExcDescOne _ =
	    case checkTime timer 2 of
		0 => genIdent ()
	      | 1 => genIdent () ^ " of " ^ genLabType ()
	      | _ => raise EH.DeadBranch ""

	and genLongStrId _ =
	    case checkTime timer 2 of
		0 => genStrId ()
	      | 1 => genStrId () ^ "." ^ genLongStrId ()
	      | _ => raise EH.DeadBranch ""

	(* We need somthing else for these ones *)
	and genInt    _ = "1"
	and genWord   _ = "0w0"
	and genReal   _ = "1.1"
	and genString _ = "\"foo\""
	and genChar   _ = "#\"a\""
	and genBool   _ = "true"
	and genRef    _ = "ref"
	and genNil    _ = "nil"
	and genId     _ = "x"
	and genSId    _ = "S"
	and gensId    _ = "s"
	and genFId    _ = "F"
	and genTId    _ = "T"
	and genCId    _ = "C"
	and genVId    _ = "'a"
	and genLab    _ = "l"
	and genOp     _ = "+"

	(* all of the below generation functions using the above *)
	and genScon _ =
	    case checkTime timer 5 of
		0 => genInt    ()
	      | 1 => genWord   ()
	      | 2 => genReal   ()
	      | 3 => genString ()
	      | 4 => genChar   ()
	      | _ => raise EH.DeadBranch ""

	and genPcon _ =
	    case checkTime timer 3 of
		0 => genBool ()
	      | 1 => genRef  ()
	      | 2 => genNil  ()
	      | _ => raise EH.DeadBranch ""

	and genLabId _ = genIdent ()

	and genIdent _ =
	    case checkTime timer 2 of
		0 => genId   ()
	      | 1 => genPcon ()
	      | _ => raise EH.DeadBranch ""

	and genStrId _ = genSId ()

	and genSigId _ = gensId ()

	and genFunId _ = genFId ()

	and genLongId _ =
	    case checkTime timer 2 of
		0 => genIdent ()
	      | 1 => genStrId () ^ "." ^ genLongId ()
	      | _ => raise EH.DeadBranch ""

	and genLongTyCon _ =
	    case checkTime timer 2 of
		0 => genTyCon ()
	      | 1 => genStrId () ^ "." ^ genLongTyCon ()
	      | _ => raise EH.DeadBranch ""

	and genTyLab _ = genLab ()

	and genTyCon _ = genTId ()

	and genTypeVar _ = genVId ()

	and genLabTyVar _ = genTypeVar ()

	and genLabTyVarList _ =
	    case checkTime timer 2 of
		0 => genLabTyVar ()
	      | 1 => genLabTyVar () ^ "," ^ genLabTyVarList ()
	      | _ => raise EH.DeadBranch ""

	and genTyVarSeq _ =
	    case checkTime timer 3 of
		0 => ""
	      | 1 => genTypeVar ()
	      | 2 => "(" ^ genLabTyVarList () ^ ")"
	      | _ => raise EH.DeadBranch ""

	and genTypeTuple _ =
	    case checkTime timer 3 of
		0 => ""
	      | 1 => genType ()
	      | 2 => genType () ^ " * " ^ genTypeTuple ()
	      | _ => raise EH.DeadBranch ""

	and genLabTypeList _ =
	    case checkTime timer 2 of
		0 => genLabType ()
	      | 1 => genLabType () ^ "," ^ genLabTypeList ()
	      | _ => raise EH.DeadBranch ""

	and genLabTypeTuple _ =
	    case checkTime timer 2 of
		0 => genLabType ()
	      | 1 => genLabType () ^ " * " ^ genLabTypeTuple ()
	      | _ => raise EH.DeadBranch ""

	and genLabType _ = genType ()

	and genTyRow _ = genTyLab () ^ " : " ^ genLabType ()

	and genTyRowList _ =
	    case checkTime timer 2 of
		0 => genTyRow ()
	      | 1 => genTyRow () ^ "," ^ genTyRowList ()
	      | _ => raise EH.DeadBranch ""

	and genType _ =
	    case checkTime timer 7 of
		0 => genTypeVar ()
	      | 1 => genLabType () ^ " -> " ^ genLabType ()
	      | 2 => genLabTypeTuple ()
	      | 3 => "{}"
	      | 4 => "{" ^ genTyRowList () ^ "}"
	      | 5 => genTypeSeq () ^ " " ^ genLongTyCon ()
	      | 6 => "(" ^ genLabType () ^ ")"
	      | _ => raise EH.DeadBranch ""

	and genTypeSeq _ =
	    case checkTime timer 4 of
		0 => genType ()
	      | 1 => ""
	      | 2 => "()"
	      | 3 => "(" ^ genLabTypeList () ^ ")"
	      | _ => raise EH.DeadBranch ""

	and genConBind _ =
	    case checkTime timer 2 of
		0 => genIdent ()
	      | 1 => genLabId () ^ " of " ^ genLabType ()
	      | _ => raise EH.DeadBranch ""

	and genConBindList _ =
	    case checkTime timer 2 of
		0 => genConBind ()
	      | 1 => genConBind () ^ " | " ^ genConBindList ()
	      | _ => raise EH.DeadBranch ""

	and genConBindSeq _ = genConBindList ()

	and genValBindCore _ = genLabPat () ^ " = " ^ genLabExp ()

	and genValBindCoreList _ =
	    case checkTime timer 2 of
		0 => genValBindCore ()
	      | 1 => genValBindCore () ^ " and " ^ genValBindCoreList ()
	      | _ => raise EH.DeadBranch ""

	and genValBindSeq _ = genValBindCoreList ()

	and genValBind _ =
	    case checkTime timer 3 of
		0 => "rec " ^ genValBindSeq ()
	      | 1 => genValBindSeq ()
	      | _ => raise EH.DeadBranch ""

	and genDatName _ = genTyVarSeq () ^ " " ^ genTyCon ()

	and genLDatName _ = genTyVarSeq () ^ " " ^ genLongTyCon ()

	and genDatBind _ = genDatName () ^ " = " ^ genConBindSeq ()

	and genDatBindList _ =
	    case checkTime timer 2 of
		0 => genDatBind ()
	      | 1 => genDatBind () ^ " and " ^ genDatBindList ()
	      | _ => raise EH.DeadBranch ""

	and genDatBindSeq _ = genDatBindList ()

	and genLabAtPat _ = genAtPat ()

	and genFMatch _ =
	    case checkTime timer 2 of
		0 => genIdent ()
	      | 1 => genFMatch () ^ " " ^ genLabAtPat ()
	      | _ => raise EH.DeadBranch ""

	and genLabFMatch _ = genFMatch () ^ " "

	and genFMatchTy _ =
	    case checkTime timer 2 of
		0 => genLabFMatch ()
	      | 1 => genLabFMatch () ^ " : " ^ genLabType ()
	      | _ => raise EH.DeadBranch ""

	and genFVBCore _ = genFMatchTy () ^ " = " ^ genLabExp ()

	and genFValBindCoreList _ =
	    case checkTime timer 2 of
		0 => genFVBCore ()
	      | 1 => genFVBCore () ^ " | " ^ genFValBindCoreList ()
	      | _ => raise EH.DeadBranch ""

	and genFValBindOne _ = genFValBindCoreList ()

	and genFValBindOneList _ =
	    case checkTime timer 2 of
		0 => genFValBindOne ()
	      | 1 => genFValBindOne () ^ " and " ^ genFValBindOneList ()
	      | _ => raise EH.DeadBranch ""

	and genFValBind _ = genFValBindOneList ()

	and genTypBind _ = genDatName () ^ " = " ^ genLabType ()

	and genTypBindList _ =
	    case checkTime timer 2 of
		0 => genTypBind ()
	      | 1 => genTypBind () ^ " and " ^ genTypBindList ()
	      | _ => raise EH.DeadBranch ""

	and genTypBindSeq _ = genTypBindList ()

	and genExBind _ =
	    case checkTime timer 3 of
		0 => genIdent ()
	      | 1 => genLabId () ^ " of " ^ genLabType ()
	      | 2 => genLabId () ^ " = " ^ genLongId ()
	      | _ => raise EH.DeadBranch ""

	and genExBindList _ =
	    case checkTime timer 2 of
		0 => genExBind ()
	      | 1 => genExBind () ^ " and " ^ genExBindList ()
	      | _ => raise EH.DeadBranch ""

	and genExBindSeq _ = genExBindList ()

	and genLongStrIdList _ =
	    case checkTime timer 2 of
		0 => genLongStrId ()
	      | 1 => genLongStrId () ^ " " ^ genLongStrIdList ()
	      | _ => raise EH.DeadBranch ""

	and genLongSidSeq _ = genLongStrIdList ()

	and genIdentList _ =
	    case checkTime timer 2 of
		0 => genIdent ()
	      | 1 => genIdent () ^ " " ^ genIdentList ()
	      | _ => raise EH.DeadBranch ""

	and genIdentSeq _ = genIdentList ()

	and genDec _ =
	    case checkTime timer 14 of
		0  => "val "       ^ genTyVarSeq () ^ " " ^ genValBind ()
	      | 1  => "fun "       ^ genTyVarSeq () ^ " " ^ genFValBind ()
	      | 2  => "datatype "  ^ genDatBindSeq ()
	      | 3  => "datatype "  ^ genDatBindSeq () ^ " withtype " ^ genTypBindSeq ()
	      | 4  => "datatype "  ^ genTyCon () ^ " = datatype " ^ genLongTyCon ()
	      | 5  => "type "      ^ genTypBindSeq ()
	      | 6  => "exception " ^ genExBindSeq ()
	      | 7  => "open "      ^ genLongSidSeq ()
	      | 8  => "local "     ^ genDecs () ^ " in " ^ genDecs () ^ " end"
	      | 9  => "abstype "   ^ genDatBindSeq () ^ " with " ^ genDecs () ^ " end"
	      | 10 => "abstype "   ^ genDatBindSeq () ^ " withtype " ^ genTypBindSeq () ^ " with " ^ genDecs () ^ " end"
	      | 11 => "infix "     ^ genIdentSeq ()
	      | 12 => "infixr "    ^ genIdentSeq ()
	      | 13 => "nonfix "    ^ genIdentSeq ()
	      | _  => raise EH.DeadBranch ""

	and genDecList _ =
	    case checkTime timer 2 of
		0 => ""
	      | 1 => genDec () ^ "\n" ^ genDecList ()
	      | _ => raise EH.DeadBranch ""

	and genDecs _ = genDecList ()

	and genExpList _ =
	    case checkTime timer 2 of
		0 => genExp ()
	      | 1 => genExp () ^ "," ^ genExpList ()
	      | _ => raise EH.DeadBranch ""

	and genLabExpList _ =
	    case checkTime timer 2 of
		0 => genLabExp ()
	      | 1 => genLabExp () ^ "," ^ genLabExpList ()
	      | _ => raise EH.DeadBranch ""

	and genLabExpSeq _ =
	    case checkTime timer 2 of
		0 => genLabExp ()
	      | 1 => genLabExp () ^ ";" ^ genLabExpSeq ()
	      | _ => raise EH.DeadBranch ""

	and genLabExp _ = genExp ()

	and genExpRow _ = genTyLab () ^ " = " ^ genLabExp ()

	and genExpRowList _ =
	    case checkTime timer 2 of
		0 => genExpRow ()
	      | 1 => genExpRow () ^ "," ^ genExpRowList ()
	      | _ => raise EH.DeadBranch ""

	and genSeqExp _ = genLabExpSeq ()

	and genAtExp _ =
	    case checkTime timer 9 of
		0 => genLongId ()
	      | 1 => genScon ()
	      | 2 => "(" ^ genLabExpList () ^ ")"
	      | 3 => "{" ^ genExpRowList () ^ "}"
	      | 4 => "let " ^ genDecs () ^ " in " ^ genSeqExp () ^ " end"
	      | 5 => "(" ^ genLabExp () ^ ")"
	      | 6 => "[" ^ genLabExpList () ^ "]"
	      | 7 => "#" ^ genTyLab ()
	      | 8 => "(" ^ genSeqExp () ^ ")"
	      | _ => raise EH.DeadBranch ""

	and genExp _ =
	    case checkTime timer 2 of
		0  => genAtExp ()
	      | 1  => "fn " ^ genMatch ()
	      | 2  => genExp () ^ " " ^ genAtExp ()
	      | 3  => "case " ^ genLabExp () ^ " of " ^ genMatch ()
	      | 4  => genLabExp () ^ " :: " ^ genLabExp ()
	      | 5  => genLabExp () ^ " " ^ genOp () ^ " " ^ genLabExp ()
	      | 6  => genLabExp () ^ " : " ^ genLabType ()
	      | 7  => "if " ^ genLabExp () ^ " then " ^ genLabExp () ^ " else " ^ genLabExp ()
	      | 8  => "while " ^ genLabExp () ^ " do " ^ genLabExp ()
	      | 9  => "raise " ^ genLabExp ()
	      | 10 => genLabExp () ^ " handle " ^ genMatch ()
	      | _  => raise EH.DeadBranch ""

	and genMruleList _ =
	    case checkTime timer 2 of
		0 => genMrule ()
	      | 1 => genMrule () ^ " | " ^ genMruleList ()
	      | _ => raise EH.DeadBranch ""

	and genMatch _ = genMruleList ()

	and genMrule _ = genLabPat () ^ " => " ^ genLabExp ()

	and genPatList _ =
	    case checkTime timer 2 of
		0 => genPat ()
	      | 1 => genPat () ^ "," ^ genPatList ()
	      | _ => raise EH.DeadBranch ""

	and genLabPatList _ =
	    case checkTime timer 2 of
		0 => genLabPat ()
	      | 1 => genLabPat () ^ "," ^ genLabPatList ()
	      | _ => raise EH.DeadBranch ""

	and genLabPat _ = genPat ()

	and genPatRow _ =
	    case checkTime timer 4 of
		0 => genTyLab () ^ " = " ^ genLabPat ()
	      | 1 => genIdentTy ()
	      | 2 => genLabIdTy () ^ " as " ^ genLabPat ()
	      | 3 => "..."
	      | _ => raise EH.DeadBranch ""

	and genPatRowList _ =
	    case checkTime timer 2 of
		0 => genPatRow ()
	      | 1 => genPatRow () ^ "," ^ genPatRowList ()
	      | _ => raise EH.DeadBranch ""

	and genAtPat _ =
	    case checkTime timer 7 of
		0 => "_"
	      | 1 => genLongId ()
	      | 2 => genScon ()
	      | 3 => "(" ^ genLabPatList () ^ ")"
	      | 4 => "{" ^ genPatRowList () ^ "}"
	      | 5 => "(" ^ genLabPat () ^ ")"
	      | 6 => "[" ^ genLabPatList () ^ "]"
	      | _ => raise EH.DeadBranch ""

	and genIdentTy _ =
	    case checkTime timer 2 of
		0 => genIdent ()
	      | 1 => genLabId () ^ " : " ^ genLabType ()
	      | _ => raise EH.DeadBranch ""

	and genLabIdTy _ = genIdentTy ()

	and genPat _ =
	    case checkTime timer 5 of
		0 => genAtPat ()
	      | 1 => genLongId () ^ " " ^ genAtPat ()
	      | 2 => genLabPat () ^ " :: " ^ genLabPat ()
	      | 3 => genLabPat () ^ " : " ^ genLabType ()
	      | 4 => genLabIdTy () ^ " as " ^ genLabPat ()
	      | _ => raise EH.DeadBranch ""

    in genProgs ()
    end



end
