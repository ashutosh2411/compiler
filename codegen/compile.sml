CM.make("../parser/sources.cm");
Control.Print.printDepth:=20;
Control.Print.printLength:=20;
Control.Print.stringDepth:=20;

val ast = Parser.parse "../parser/test3.av"

fun op1 (Ast.Plus) = "+"
	|op1 (Ast.Minus) = "-"
	|op1 (Ast.Times) = "*"
	|op1 (Ast.Divide) = "/"
	|op1 (Ast.Mod) = "%" 



fun expToString (Ast.id1_(x) )= x
 	|expToString (Ast.int1_(x)) = Int.toString(x)
 	|expToString (Ast.exp_(x , exp1 ,exp2)) = expToString (exp1 )^ op1(x) ^expToString (exp2 )
 	 


fun boolToString (Ast.boolexp_(EEQ,exp1,exp2)) = expToString(exp1)^" == "^ expToString(exp1)


fun assToString (Ast.assign_(Ast.id_(x) ,exp)) = x^" = "^expToString(exp)^";"

fun stlistToString [] = ""
	|stlistToString (Ast.stmt(x)::xs) = (assToString x )^ stlistToString (xs)
	


  

val x=stlistToString ast ;