
structure Ast =
struct

	datatype INT = int_ of int   
	datatype ID  = id_ of string  
	datatype bool = true | false

	datatype boolc = EQ | NEQ | LT | LE | GT | GE | EEQ  
	datatype BinOp = Plus | Minus | TIMES | DIVIDE | MOD 

	and exp  =    INT 
				| ID
				| exp_ of BinOp * exp * exp

	and dum = exp | ifonly | forloop  | boolexp 

	and assign = assign_ of ID  * exp 

	and statements  = statements_ of dum   * statements

	and boolexp = boolexp_ of boolc * exp * exp 

	and ifonly = ifonly_ of boolexp * statements

	and forloop = forloop_ of assign * boolexp * assign*statements 

	and bools = bool_ of bool

end
