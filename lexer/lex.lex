type pos = int
type svalue = Tokens.svalue

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

%%

alpha =[a-zA-Z] ;
digit =[0-9] ;
whitespace=[\t\ ]+;

%%

"+"							=>		(Tokens.PLUS(yypos,yypos+1));
"-"							=>		(Tokens.MINUS(yypos,yypos+1));
"/"							=>		(Tokens.DIVIDE(yypos,yypos+1));
"*"							=>		(Tokens.TIMES(yypos,yypos+1));

.       					=> (ErrorMsg.error yypos ("illegal character:" ^ "|" ^ yytext ^ "|"); continue());
