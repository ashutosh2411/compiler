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

fun boolop (Ast.EEQ) = " == "
	 |boolop (Ast.NEQ) = " != " 
	 |boolop (Ast.GE) = ">="
	 |boolop (Ast.GT) = ">"
	 |boolop (Ast.LT) = "<" 
	 |boolop (Ast.LE) = "<=" 


fun expToString (Ast.id1_(x) )= x
 	|expToString (Ast.int1_(x)) = Int.toString(x)
 	|expToString (Ast.exp_(x , exp1 ,exp2)) = expToString (exp1 )^ op1(x) ^expToString (exp2 )
 	 


fun boolToString (Ast.boolexp_(x,exp1,exp2)) = expToString(exp1)^ boolop(x) ^ expToString(exp1)


fun assToString (Ast.assign_(Ast.id_(x) ,exp)) = x^" = "^expToString(exp)^";"

fun stlistToString ([]) 				= " "
	|stlistToString ((Ast.stmt(x))::xs) = (assToString x )^stlistToString (xs)

	|stlistToString ([Ast.floop(x)]) = let 
		val Ast.forloop_(st1 ,bexp ,st2 ,st3) = x 
		in 
			assToString(st1)^" ; while ( " ^ boolToString(bexp) ^" ){ " ^stlistToString(st3) ^ assToString(st2) ^ "} "

		end ;
	


  

val x=stlistToString (ast) ;