structure Ast =
struct

	datatype BinOp = Plus | Minus | Mul;

	datatype Expr  = Const of int
		       | Op    of Expr * BinOp * Expr;


	fun plus  a b = Op (a, Plus, b)
	fun minus a b = Op (a, Minus, b)
	fun mul   a b = Op (a, Mul, b)

	fun binOpDenote Plus  x y = x + y
	  | binOpDenote Minus x y = x - y
	  | binOpDenote Mul   x y = x * y;

	fun exprDenote (Const x)       = x
	  | exprDenote (Op (x,oper,y)) = binOpDenote oper (exprDenote x) (exprDenote y);


	fun binOpToString Plus  = "+"
	  | binOpToString Minus = "-"
	  | binOpToString Mul   = "*"

end