CM.make("../parser/sources.cm");
Control.Print.printDepth:=20;
Control.Print.printLength:=20;
Control.Print.stringDepth:=20;

val ast = Parser.parse "../parser/test3.c"

(*

datatype checktype = Plus
(*
fun op Plus ="+"
	|op Minus ="-"
	|op Times ="*"
	|op Divide ="/"
	|op Mod ="%" *)

fun compile [] = ""
   | compile (x::xs) = let

   				val Ast.stmt(st) =x ;
   				fun rulematch (st , output)=
	   				case  st of
	   					 Ast.assign_( Ast.id_(ID) , exp ) => ID ^ "=" ^ "0" ^ "\n" 

	   					(*
	   					(Ast.id1_(ID)) => ID
	   					| _+ => =""
*)
	   					(*Ast.exp (int1_(INT)) => INT
	   					Ast.exp_ of BinOp * exp1 * exp2 =>output^(rulematch exp1)^ (op BinOp )^ (rulematch exp2 )*)
				in

				rulematch(x,"")

				end ;


*)