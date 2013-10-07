(* Copyright 2002 2009 2010 2012 Heriot-Watt University
 *
 * Skalpel is a free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Skalpel is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Skalpel.  If not, see <http://www.gnu.org/licenses/>.
 *
 *  o Authors:     Vincent Rahli, Christian Haack, John Pirie
 *  o Affiliation: Heriot-Watt University, MACS
 *  o Date:        25 May 2010
 *  o File name:   ML.lex
 *  o Description: Defines a lexer for SML.
 *)


structure L = LexDefs
structure C = Comment
structure T = Tokens
structure R = Reg
structure D = Debug

type pos           = int * int
type svalue        = T.svalue
type ('a,'b) token = ('a,'b) T.token
type lexresult     = (svalue, pos) token
type arg           = string * pos ref

(* Used to store what is in a quasiquote *)
val charlist : string list ref = ref []

(* Position:    position of the begining of the string being read
 * First bool:  true if reading a string
 * Second bool: true for a string and false for a char
 * String:      string being read *)
val initQuotedString : (pos * bool * bool * string) = ((0, 0), false, true, "")

val quotedString = ref initQuotedString

fun consCommentRegions pos =
    [R.consReg pos (R.upPos pos)]

fun consRegions pos = [R.consReg pos pos]

fun reset () =
    let val _ = charlist := []
        val _ = quotedString := initQuotedString
    in ()
    end

  fun raiseError error = (reset (); raise error)

fun eof (file, posref) =
    if not (Comment.isClosed())
    then raiseError (L.UnclosedComment (file, consCommentRegions (Option.valOf (Comment.getTop ())) handle Option => []))
    else let val (pos, read, b, str) = !quotedString
         in if read
            then raiseError (L.UnclosedString (file, consRegions pos))
            else T.EOF (!posref, !posref)
	 end

fun error ((file, posref), msg) =
    TextIO.print (file ^ ":" ^ R.printPos (!posref) ^ ": " ^  msg ^ "\n")

fun badchar (file, posref) =
    let val msg = "ignoring bad character"
	(*val _   = TextIO.print (file ^ ":" ^ R.printPos (!posref) ^ ": " ^  msg ^ "\n")*)
        val _   = raise L.BadCharacter (file, consRegions (!posref))
    in ()
    end

fun unclosedString (file, posref) =
    let val (pos, read, b, str) = !quotedString
    in if read
       then raise (L.UnclosedString (file, consRegions pos))
       else badchar (file, posref) (* Should never happen *)
    end

fun token c s (_, posref) =
    let val (from as (row, col)) = !posref
	val next = col + String.size s
	val to = next - 1
    in (posref := (row, next); c (from, (row, to)))
    end

fun tokenWithString c s (_, posref) =
    let val (from as (row, col)) = !posref
	val next = col + String.size s
	val to = next - 1
    in (posref := (row, next); c (s, from, (row, to)))
    end

fun tokenWithStringAQ token yytextaq quoteString (_, posref) =
    let val (from as (row, col)) = !posref
	val next = col + String.size yytextaq
	val to = next - 1
    in (posref := (row, next); token (quoteString, from, (row, to)))
    end

fun skip s (_, posref) = posref := R.addString (!posref) s

fun newline (_, posref) =
    let val (row, col) = !posref
    in posref := (row + 1, 1)
    end

fun closedComment (file, posref) =
    raise L.ClosedComment (file, consCommentRegions (!posref))

fun addString (charlist, s:string, (_, posref)) =
    let val (row, col) = !posref
        val next = col + String.size s
        val _ = posref := (row, next)
        val _ = charlist := s :: (!charlist)
    in ()
    end

fun addChar (charlist, c:char, arg) = addString (charlist, String.str c, arg)

fun makeString charlist = (concat(rev(!charlist)) before charlist := nil)

fun startQuotedString (_, posref) b = quotedString := (!posref, true, b, "")

fun addQuotedString (strR, yytext, (_, posref)) =
    let val (row, col) = !posref
        val next = col + String.size yytext
	val (pos, read, b, strL) = !quotedString
        val _ = posref := (row, next)
	val _ = quotedString := (pos, read, b, strL ^ strR)
    in ()
    end

fun tokenWithStringSTRING (_, posref) =
    let val (row, col) = !posref
	val (pos, read, b, str) = !quotedString
        val str = if b then "\"" ^ str ^ "\"" else "#\"" ^ str ^ "\""
	val tok = if b then T.STRING else T.CHAR
	val _   = quotedString := initQuotedString
    in (posref := (row, col + 1); tok (str, pos, (row, col)))
    end

fun stringToChar str =
    let val sub = String.substring (str, 1, 3)
        val num = Option.valOf (Int.fromString sub)
    in if num > 255
       then "" (* raise an error here *)
       else Char.toString (Char.chr num)
    end

fun containQuote str =
    let val chars = String.explode str
    in List.exists (fn char => char = #"`") chars
    end


%%
%header (functor MLLexFun(structure Tokens : ML_TOKENS));
%arg (arg);
%s COMMENT STDEC STUSE QUOTE AQUOTE BACKQUOTE ANTIQUOTE STRI STRD ESCSTRI ESCSTRD;
%reject


digitDec     = [0-9];
boolean      = "true" | "false";
numeral      = [1-9] {digitDec}*;
digitDecSeq  = {digitDec}+;
digitHexLow  = [a-f];
digitHexUp   = [A-F];
digitHex     = {digitHexLow} | {digitHexUp};
hex          = {digitHex} | {digitDec};
digitHexSeq  = {hex}+;
integerDec   = ~ {digitDecSeq} | {digitDecSeq};
integerHex   = ~ 0 x {digitHexSeq} | 0 x {digitHexSeq};
wordDec      = 0 w {digitDecSeq};
wordHex      = 0 w x {digitHexSeq};
realE        = {integerDec} | {integerDec} "." {digitDecSeq};
real         = {realE} | {realE} [eE] {integerDec};
esc          = [\t\ ]+;
letter       = [a-zA-Z];
quote        = "`";
colon        = ":";
star         = "*";
presymb      = [! % & \$ # \+ \/ \< \= \> \? @ \\ ~ \^ \| -];
ppresymb     = {colon} | {star} | {presymb};
symbol       = {quote} | {ppresymb};
symboltc     = {quote} | {colon} | {presymb};
inIdSymbol   = {star} | {presymb};
symbolstc    = {symboltc} | {symbol} {symbol}+;
prime        = "'";
alphadigit   = {letter} | {digitDec} | _ | {prime};
tycon        = {letter} {alphadigit}* | {symbolstc};
ident        = {letter} {alphadigit}* | {symbol}+;
inident      = {letter} {alphadigit}* | {inIdSymbol}+;
inid         = "_"*{inident};
module       = {ident}".";
longid       = {module}+ {ident};
eqtyvar      = {prime} {prime} {alphadigit}*;
tyvar        = {prime} {alphadigit}*;
lab          = {ident} | {numeral};
smlTesEsc    = #![^\n]*\n;
skalpelTag    = "(**SKALPEL" | "(**SML-TES";
skalpelUse    = {skalpelTag} "-USE-FILE";
skalpelDec    = {skalpelTag} "-DEC";
skalpelSpec   = {skalpelTag} "-SPEC";
skalpelCbas   = {skalpelTag} "-CLEAR-BASIS";
skalpelSbas   = {skalpelTag} "-SET-BASIS";
skalpelQuote  = {skalpelTag} "-QUASIQUOTES";
skalpelType   = {skalpelTag} "-TYPE";
smlTesTag    = "(**SML-TES";
smlTesUse    = {smlTesTag} "-USE-FILE";
smlTesDec    = {smlTesTag} "-DEC";
smlTesSpec   = {smlTesTag} "-SPEC";
smlTesCbas   = {smlTesTag} "-CLEAR-BASIS";
smlTesSbas   = {smlTesTag} "-SET-BASIS";
smlTesQuote  = {smlTesTag} "-QUASIQUOTES";
smlTesType   = {smlTesTag} "-TYPE";
stUseFile    = ([^\*\ \n\t]*(\*[^\)])?)*;


%%


<INITIAL,COMMENT> "\n"       => (newline arg;
				 continue());
<STDEC,STUSE> "\n"           => (newline arg;
				 continue());
<QUOTE,AQUOTE> "\n"          => (newline arg;
				 continue());
<BACKQUOTE,ANTIQUOTE> "\n"   => (newline arg;
				 continue());

<INITIAL,COMMENT> {esc}      => (skip yytext arg;
				 continue());
<STDEC,STUSE> {esc}          => (skip yytext arg;
				 continue());
<QUOTE,AQUOTE> {esc}         => (skip yytext arg;
				 continue());
<BACKQUOTE,ANTIQUOTE> {esc}  => (skip yytext arg;
				 continue());

^{smlTesEsc}                 => (skip yytext arg;
				 newline arg;
				 continue());

<INITIAL> {skalpelDec}        => (C.ope (!(#2 arg));
				 YYBEGIN STDEC;
				 token T.STDEC yytext arg);
<INITIAL> {skalpelSpec}       => (C.ope (!(#2 arg));
				 YYBEGIN STDEC;
				 token T.STSPEC yytext arg);
<INITIAL> {skalpelUse}        => (C.ope (!(#2 arg));
				 YYBEGIN STUSE;
				 token T.STUSE yytext arg);
<INITIAL> {skalpelCbas}       => (C.ope (!(#2 arg));
				 YYBEGIN STUSE;
				 token T.STCBAS yytext arg);
<INITIAL> {skalpelSbas}       => (C.ope (!(#2 arg));
				 YYBEGIN STUSE;
				 token T.STSBAS yytext arg);
<INITIAL> {skalpelQuote}      => (C.ope (!(#2 arg));
				 YYBEGIN STUSE;
				 token T.STQUOTE yytext arg);
<INITIAL> {skalpelType}       => (C.ope (!(#2 arg));
				 YYBEGIN STUSE;
				 token T.STTYPE yytext arg);

<INITIAL> {smlTesDec}        => (C.ope (!(#2 arg));
				 YYBEGIN STDEC;
				 token T.STDEC yytext arg);
<INITIAL> {smlTesSpec}       => (C.ope (!(#2 arg));
				 YYBEGIN STDEC;
				 token T.STSPEC yytext arg);
<INITIAL> {smlTesUse}        => (C.ope (!(#2 arg));
				 YYBEGIN STUSE;
				 token T.STUSE yytext arg);
<INITIAL> {smlTesCbas}       => (C.ope (!(#2 arg));
				 YYBEGIN STUSE;
				 token T.STCBAS yytext arg);
<INITIAL> {smlTesSbas}       => (C.ope (!(#2 arg));
				 YYBEGIN STUSE;
				 token T.STSBAS yytext arg);
<INITIAL> {smlTesQuote}      => (C.ope (!(#2 arg));
				 YYBEGIN STUSE;
				 token T.STQUOTE yytext arg);
<INITIAL> {smlTesType}       => (C.ope (!(#2 arg));
				 YYBEGIN STUSE;
				 token T.STTYPE yytext arg);

<INITIAL> "(*"               => (C.ope (!(#2 arg));
				 skip yytext arg;
				 YYBEGIN COMMENT;
				 continue());

<INITIAL> "*)"               => (closedComment arg;
				 continue ());

<STDEC> "*)"                 => (C.close ();
				 if C.isClosed() then YYBEGIN INITIAL else ();
				 token T.STEND yytext arg);

<INITIAL> "#\""              => (YYBEGIN STRI;
				 startQuotedString arg false;
				 skip yytext arg;
				 continue ());
<STDEC> "#\""                => (YYBEGIN STRD;
				 startQuotedString arg false;
				 skip yytext arg;
				 continue ());

<INITIAL> "\""               => (YYBEGIN STRI;
				 startQuotedString arg true;
				 skip yytext arg;
				 continue ());
<STDEC> "\""                 => (YYBEGIN STRD;
				 startQuotedString arg true;
				 skip yytext arg;
				 continue ());

<INITIAL,STDEC> "["          => (token T.LLIST      yytext arg);
<INITIAL,STDEC> "]"          => (token T.RLIST      yytext arg);
<INITIAL,STDEC> ","          => (token T.COMMA      yytext arg);
<INITIAL,STDEC> "("          => (D.printDebug D.LEXER D.PARSING (fn _ => "Detected opening bracket in <INITIAL,STDEC>.");
				 if not (C.isClosedQ())
                                 then C.opeQ (!(#2 arg))
                                 else ();
				 token T.LPAREN yytext arg);
<INITIAL,STDEC> ")"          => (if not (C.isClosedQ())
				 then (C.closeQ ();
				       if C.isClosedQ ()
				       then YYBEGIN BACKQUOTE
				       else ())
				 else ();
				 token T.RPAREN yytext arg);
<INITIAL,STDEC> "{"          => (token T.LBRACE     yytext arg);
<INITIAL,STDEC> "}"          => (token T.RBRACE     yytext arg);
<INITIAL,STDEC> "->"         => (token T.TYPEARROW  yytext arg);
<INITIAL,STDEC> {star}       => (token T.STAR       yytext arg);
<INITIAL,STDEC> "of"         => (token T.OF         yytext arg);
<INITIAL,STDEC> "|"          => (token T.PIPE       yytext arg);
<INITIAL,STDEC> "=>"         => (token T.DARROW     yytext arg);
<INITIAL,STDEC> "="          => (token T.EQUALOP    yytext arg);
<INITIAL,STDEC> "rec"        => (token T.REC        yytext arg);
<INITIAL,STDEC> "and"        => (token T.AND        yytext arg);
<INITIAL,STDEC> "val"        => (token T.VAL        yytext arg);
<INITIAL,STDEC> "datatype"   => (token T.DATATYPE   yytext arg);
<INITIAL,STDEC> "eqdatatype" => (token T.EQDATATYPE   yytext arg);
<INITIAL,STDEC> "let"        => (token T.LET        yytext arg);
<INITIAL,STDEC> "in"         => (token T.IN         yytext arg);
<INITIAL,STDEC> "end"        => (token T.END        yytext arg);
<INITIAL,STDEC> "fn"         => (token T.FN         yytext arg);
<INITIAL,STDEC> "case"       => (token T.CASE       yytext arg);
<INITIAL,STDEC> "while"      => (token T.WHILE      yytext arg);
<INITIAL,STDEC> "fun"        => (token T.FUN        yytext arg);
<INITIAL,STDEC> "do"         => (token T.DO         yytext arg);
<INITIAL,STDEC> "if"         => (token T.IF         yytext arg);
<INITIAL,STDEC> "then"       => (token T.THEN       yytext arg);
<INITIAL,STDEC> "else"       => (token T.ELSE       yytext arg);
<INITIAL,STDEC> "_"          => (token T.WILD       yytext arg);
<INITIAL,STDEC> {colon}      => (token T.COLON      yytext arg);
<INITIAL,STDEC> "..."        => (token T.WILDCARD   yytext arg);
<INITIAL,STDEC> "#"          => (token T.SHARP      yytext arg);
<INITIAL,STDEC> ";"          => (token T.SEMICOLON  yytext arg);
<INITIAL,STDEC> "type"       => (token T.TYPE       yytext arg);
<INITIAL,STDEC> "raise"      => (token T.RAISE      yytext arg);
<INITIAL,STDEC> "handle"     => (token T.HANDLE     yytext arg);
<INITIAL,STDEC> "exception"  => (token T.EXCEPTION  yytext arg);
<INITIAL,STDEC> "structure"  => (token T.STRUCTURE  yytext arg);
<INITIAL,STDEC> "struct"     => (token T.STRUCT     yytext arg);
<INITIAL,STDEC> "op"         => (token T.OP         yytext arg);
<INITIAL,STDEC> "open"       => (token T.OPEN       yytext arg);
<INITIAL,STDEC> "infix"      => (token T.INFIX      yytext arg);
<INITIAL,STDEC> "infixr"     => (token T.INFIXR     yytext arg);
<INITIAL,STDEC> "nonfix"     => (token T.NONFIX     yytext arg);
<INITIAL,STDEC> "overload"   => (token T.OVERLOAD   yytext arg);
<INITIAL,STDEC> "local"      => (token T.LOCAL      yytext arg);
<INITIAL,STDEC> "with"       => (token T.WITH       yytext arg);
<INITIAL,STDEC> "where"      => (token T.WHERE      yytext arg);
<INITIAL,STDEC> "withtype"   => (token T.WITHTYPE   yytext arg);
<INITIAL,STDEC> "abstype"    => (token T.ABSTYPE    yytext arg);
<INITIAL,STDEC> "sharing"    => (token T.SHARING    yytext arg);
<INITIAL,STDEC> "eqtype"     => (token T.EQTYPE     yytext arg);
<INITIAL,STDEC> "include"    => (token T.INCLUDE    yytext arg);
<INITIAL,STDEC> ":>"         => (token T.SEAL       yytext arg);
<INITIAL,STDEC> "signature"  => (token T.SIGNATURE  yytext arg);
<INITIAL,STDEC> "functor"    => (token T.FUNCTOR    yytext arg);
<INITIAL,STDEC> "sig"        => (token T.SIG        yytext arg);
<INITIAL,STDEC> "as"         => (token T.AS         yytext arg);
<INITIAL,STDEC> "orelse"     => (token T.ORELSE     yytext arg);
<INITIAL,STDEC> "andalso"    => (token T.ANDALSO    yytext arg);
<INITIAL,STDEC> {numeral}    => (tokenWithString T.NUM       yytext arg);
<INITIAL,STDEC> {integerDec} => (tokenWithString T.INT       yytext arg);
<INITIAL,STDEC> {integerHex} => (tokenWithString T.INT       yytext arg);
<INITIAL,STDEC> {wordDec}    => (tokenWithString T.WORD      yytext arg);
<INITIAL,STDEC> {wordHex}    => (tokenWithString T.WORD      yytext arg);
<INITIAL,STDEC> {real}       => (tokenWithString T.REAL      yytext arg);
<INITIAL,STDEC> {eqtyvar}    => (tokenWithString T.EQTYPEVAR yytext arg);
<INITIAL,STDEC> {tyvar}      => (tokenWithString T.TYPEVAR   yytext arg);
<INITIAL,STDEC> {ident}      => (if L.getQuotation ()
				 then if containQuote yytext
  				      then REJECT ()
				      else tokenWithString T.ID yytext arg
				 else tokenWithString T.ID yytext arg);
<INITIAL,STDEC> {ppresymb}+  => (tokenWithString T.ID      yytext arg);
<INITIAL,STDEC> {quote}      => (if L.getQuotation ()
				 then (YYBEGIN BACKQUOTE;
				       token T.BQUOTE yytext arg)
				 else (badchar arg;
				       continue ()));
<INITIAL,STDEC> {inid}       => (tokenWithString T.INID    yytext arg);
<INITIAL,STDEC> {longid}     => (tokenWithString T.LONGID  yytext arg);
<INITIAL,STDEC> .            => (badchar arg;
				 continue ());

<COMMENT> "(*"               => (C.ope (!(#2 arg));
				 skip yytext arg;
				 continue());
<COMMENT> "*)"               => (skip yytext arg;
				 C.close();
				 if C.isClosed () then YYBEGIN INITIAL else ();
				 continue());
<COMMENT> .                  => (skip yytext arg;
				 continue());

<BACKQUOTE> {quote}          => (YYBEGIN INITIAL;
				 tokenWithStringAQ T.EQUOTE yytext (makeString charlist) arg);

<BACKQUOTE> "^"              => (YYBEGIN ANTIQUOTE;
				 tokenWithStringAQ T.AQUOTE yytext (makeString charlist) arg);

<BACKQUOTE> "\n"             => (addString (charlist, "\n", arg); (*newline arg?*)
				 continue());

<BACKQUOTE> .                => (addString (charlist, yytext, arg);
				 continue());

<ANTIQUOTE> {ident}          => (YYBEGIN BACKQUOTE;
				 tokenWithString T.ID yytext arg);

<ANTIQUOTE> "("              => (C.opeQ (!(#2 arg));
				 YYBEGIN INITIAL;
				 token T.LPAREN yytext arg);

<STUSE> "*)"                 => (C.close();
				 if C.isClosed () then YYBEGIN INITIAL else ();
				 token T.STEND yytext arg);

<STUSE> {boolean}            => (L.setQuotation (Option.valOf (Bool.fromString yytext) handle Option => false);
				 tokenWithString T.QQVALUE yytext arg);
<STUSE> {stUseFile}          => (tokenWithString T.FILE yytext arg);
<STUSE> {ident}              => (tokenWithString T.ID yytext arg);

<STRI> "\\"                  => (skip yytext arg;
				 YYBEGIN ESCSTRI;
				 continue ());
<STRD> "\\"                  => (skip yytext arg;
				 YYBEGIN ESCSTRD;
				 continue ());
<STRI,STRD> "\\a"            => (addQuotedString ("\a", yytext, arg);
				 continue ());
<STRI,STRD> "\\b"            => (addQuotedString ("\b", yytext, arg);
				 continue ());
<STRI,STRD> "\\t"            => (addQuotedString ("\t", yytext, arg);
				 continue ());
<STRI,STRD> "\\n"            => (addQuotedString ("\n", yytext, arg);
				 continue ());
<STRI,STRD> "\\v"            => (addQuotedString ("\v", yytext, arg);
				 continue ());
<STRI,STRD> "\\f"            => (addQuotedString ("\f", yytext, arg);
				 continue ());
<STRI,STRD> "\\r"            => (addQuotedString ("\r", yytext, arg);
				 continue ());
<STRI,STRD> "\\^"            => (addQuotedString ("^", yytext, arg);
				 continue ());
<STRI,STRD> "\\\\"           => (addQuotedString ("\\", yytext, arg);
				 continue ());
<STRI,STRD> "\\\""           => (addQuotedString ("\"", yytext, arg);
				 continue ());
<STRI,STRD> "\\"[0-9]{3}     => (addQuotedString (stringToChar yytext, yytext, arg);
				 continue ());
<STRI,STRD> "\\u"{hex}{4}    => (addQuotedString (yytext, yytext, arg);
				 continue ());
<STRI> "\""                  => (YYBEGIN INITIAL;
				 tokenWithStringSTRING arg);
<STRD> "\""                  => (YYBEGIN STDEC;
				 tokenWithStringSTRING arg);
<STRI,STRD> "\n"             => (unclosedString arg;
				 continue ());
<STRI,STRD> [^\"\n]          => (addQuotedString (yytext, yytext, arg);
				 continue());
<ESCSTRI,ESCSTRD> "\n"       => (newline arg;
				 continue());
<ESCSTRI,ESCSTRD> {esc}      => (skip yytext arg;
				 continue());
<ESCSTRI> "\\"               => (skip yytext arg;
				 YYBEGIN STRI;
				 continue());
<ESCSTRD> "\\"               => (skip yytext arg;
				 YYBEGIN STRD;
				 continue());
<ESCSTRI,ESCSTRD> .          => (badchar arg;
				 continue ());
