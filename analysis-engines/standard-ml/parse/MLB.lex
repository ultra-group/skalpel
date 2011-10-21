(* Copyright 2010 Heriot-Watt University
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
 *  o Date:        19 June 2010
 *  o File name:   MLB.lex
 *  o Description: Defines a lexer for MLB files.
 *)


structure L = LexDefs
structure C = Comment
structure T = Tokens
structure R = Reg

type pos           = int * int
type svalue        = T.svalue
type ('a,'b) token = ('a,'b) T.token
type lexresult     = (svalue, pos) token
type arg           = string * pos ref

fun consCommentRegions pos = [R.consReg pos (R.upPos pos)]

fun eof (file, posref) =
    if not (Comment.isClosed())
    then raise (L.UnclosedComment (file, consCommentRegions (Option.valOf (Comment.getTop ())) handle Option => []))
    else T.EOF (!posref, !posref)

fun error ((file, posref), msg) =
    TextIO.print (file ^ ":" ^ R.printPos (!posref) ^ ": " ^  msg ^ "\n")

fun token c s (_, posref) =
    let val (from as (row, col)) = !posref
	val next = col + String.size s
	val to = next - 1
    in (posref := (row, next); c (from, (row, to))) end

fun tokenWithString c s (_, posref) =
    let val (from as (row, col)) = !posref
	val next = col + String.size s
	val to = next - 1
    in (posref := (row, next); c (s, from, (row, to))) end

fun skip s (_, posref) = posref := R.addString (!posref) s

fun newline (_, posref) =
    let val (row, col) = !posref
    in posref := (row + 1, 1)
    end

fun closedComment (file, posref) =
    let val regs = consCommentRegions (!posref)
    in raise L.ClosedComment (file, regs)
    end



%%
%header (functor MLBLexFun(structure Tokens : MLB_TOKENS));
%arg (arg);
%s COMMENT;


escape     = [\t\ ]+;
digitDec   = [0-9];
letter     = [a-zA-Z];
pathShort  = ({digitDec} | {letter})+;
pathPart   = {pathShort}"/";
path       = {pathPart}*{pathShort};
smlfile    = {path}".sml";
sigfile    = {path}".sig";
funfile    = {path}".fun";
mlbfile    = {path}".mlb";
symbol     = [! % & \$ # \+ \/ : \< \= \> \? @ \\ ~ ` \^ \| \* -];
symbols    = {symbol}+;
prime      = "'";
alphadigit = {letter} | {digitDec} | _ | {prime};
ident      = {letter} {alphadigit}* | {symbols};


%%


"\n"                   => (newline arg;
			   continue());
{escape}               => (skip yytext arg;
			   continue());

<INITIAL> "(*"         => (C.ope (!(#2 arg));
			   skip yytext arg;
			   YYBEGIN COMMENT;
			   continue());
<INITIAL> "*)"         => (closedComment arg);

<INITIAL> "="          => (token T.EQUALOP    yytext arg);
<INITIAL> ";"          => (token T.SEMICOLON  yytext arg);
<INITIAL> "open"       => (token T.OPEN       yytext arg);
<INITIAL> "basis"      => (token T.BASIS      yytext arg);
<INITIAL> "bas"        => (token T.BAS        yytext arg);
<INITIAL> "and"        => (token T.AND        yytext arg);
<INITIAL> "local"      => (token T.LOCAL      yytext arg);
<INITIAL> "let"        => (token T.LET        yytext arg);
<INITIAL> "in"         => (token T.IN         yytext arg);
<INITIAL> "end"        => (token T.END        yytext arg);
<INITIAL> "structure"  => (token T.STRUCTURE  yytext arg);
<INITIAL> "signature"  => (token T.SIGNATURE  yytext arg);
<INITIAL> "functor"    => (token T.FUNCTOR    yytext arg);
<INITIAL> {ident}      => (tokenWithString T.ID      yytext arg);
<INITIAL> {smlfile}    => (tokenWithString T.SMLFILE yytext arg);
<INITIAL> {sigfile}    => (tokenWithString T.SIGFILE yytext arg);
<INITIAL> {funfile}    => (tokenWithString T.FUNFILE yytext arg);
<INITIAL> {mlbfile}    => (tokenWithString T.MLBFILE yytext arg);
<INITIAL> .            => (error (arg, "ignoring bad character");
			   continue());

<COMMENT> "(*"         => (C.ope (!(#2 arg));
			   skip yytext arg;
			   continue());
<COMMENT> "*)"         => (skip yytext arg;
			   C.close();
			   if C.isClosed () then YYBEGIN INITIAL else ();
			   continue());
<COMMENT> .            => (skip yytext arg;
			   continue());
