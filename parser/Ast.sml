structure Ast =
struct


datatype INT = int_ of int   
datatype ID  = id_ of string  
datatype bool = TRUE | FALSE

datatype boolc = EQ | NEQ | LT | LE | GT | GE | EEQ  
datatype BinOp = Plus | Minus | Times | Divide | Mod 

and exp  =    int1_ of int 
			| id1_ of string
			| exp_ of BinOp * exp * exp

and statement =   ifonlystmt of ifonly | ifelsestmt of ifelse | floop of forloop  | bstmt of boolexp | stmt of assign  

and assign = assign_ of ID  * exp 

and boolexp = boolexp_ of boolc * exp * exp 
 



and ifonly = ifonly_ of boolexp * statement list

and ifelse = ifelse_ of boolexp * statement list *statement list

and forloop = forloop_ of statement * boolexp * statement *statement list

 
type li = statement list


datatype statements = stlist of li 	




end


