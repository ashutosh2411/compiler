
structure Ast =
struct

	datatype INT = int_ of int   
	datatype ID  = id_ of string  

	datatype BinOp = Plus | Minus | TIMES | DIVIDE  

	and exp  =    INT 
				| ID
				| exp_ of BinOp * exp * exp

	and assign = assign_ of ID  * exp 

	and statements  = statements_ of dum   * statements


end
