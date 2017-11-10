Control.Print.printDepth:=20;
Control.Print.printLength:=20;
Control.Print.stringDepth:=20;

val ast = Parser.parse "test.c"


fun compile [] = []
   | compile x::xs = let
   				val Ast.stmt(st) =x ;
   				case  st of
   					Astassign_ 