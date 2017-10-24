structure Ast =
struct

	datatype BinOp = Plus | Minus | TIMES | DIVIDE | MOD 

	and exp  =    INT 
				| ID
				| exp_ of BinOp * exp * exp

end
