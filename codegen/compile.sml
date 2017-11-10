CM.make("../parser/sources.cm");
Control.Print.printDepth:=20;
Control.Print.printLength:=20;
Control.Print.stringDepth:=20;

val ast = Parser.parse "../parser/test3.c"

(*

datatype checktype = Plus
 
fun op Plus ="+"
	|op Minus ="-"
	|op Times ="*"
	|op Divide ="/"
	|op Mod ="%" *)



fun expToString (Ast.id1_(x) )= x
 	|expToString (Ast.int1_(x)) = Int.toString(x)
 	|expToString (Ast.exp_(Plus , exp1 ,exp2)) = expToString (exp1 )^" + " ^expToString (exp2 )
 	
fun boolToString (Ast.boolexp_(EQ,exp1,exp2)) = expToString(exp1)^" == "^ expToString(exp1)


fun assToString (Ast.assign_(Ast.id_(x) ,exp)) = x^" = "^expToString(exp)^";"

fun stlistToString [] = ""
	|stlistToString (Ast.stmt(x)::xs) = (assToString x )^stlistToString (xs)
	|stlistToString (Ast.floop(Ast.floop_(st1,bexp , st2 ,st3)) ) = "while ("


fun 

val x=stlistToString ast ;