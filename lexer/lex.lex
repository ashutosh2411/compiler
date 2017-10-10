type pos = int
type svalue = Tokens.svalue

type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue, pos) token

val lineNum = ErrorMsg.lineNum 
val linePos = ErrorMsg.linePos 

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

%%

alpha =[a-zA-Z] ;
digit =[0-9] ;
whitespace=[\t\ ]+;

%%
\n 							=> (lineNum := !lineNum + 1; linePos := yypos::(!linePos); continue());
{whitespace}  				=> (continue());

{digit}+                 	=>		(Tokens.INT(valOf(Int.fromString yytext),yypos,yypos+size yytext));

"+"							=>		(Tokens.PLUS(yypos,yypos+1));
"-"							=>		(Tokens.MINUS(yypos,yypos+1));
"/"							=>		(Tokens.DIVIDE(yypos,yypos+1));
"*"							=>		(Tokens.TIMES(yypos,yypos+1));

.       					=> (ErrorMsg.error yypos ("illegal character:" ^ "|" ^ yytext ^ "|"); continue());
