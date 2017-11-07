functor CLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : C_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct



end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\010\000\000\000\
\\001\000\002\000\010\000\022\000\009\000\025\000\008\000\000\000\
\\001\000\002\000\021\000\003\000\020\000\000\000\
\\001\000\005\000\011\000\000\000\
\\001\000\005\000\023\000\000\000\
\\001\000\005\000\049\000\000\000\
\\001\000\006\000\013\000\000\000\
\\001\000\006\000\014\000\000\000\
\\001\000\007\000\024\000\000\000\
\\001\000\007\000\053\000\000\000\
\\001\000\008\000\037\000\000\000\
\\001\000\008\000\055\000\000\000\
\\001\000\008\000\056\000\000\000\
\\001\000\009\000\052\000\000\000\
\\001\000\009\000\059\000\000\000\
\\001\000\009\000\060\000\000\000\
\\001\000\010\000\035\000\011\000\034\000\012\000\033\000\013\000\032\000\
\\014\000\031\000\015\000\030\000\016\000\029\000\017\000\028\000\
\\018\000\027\000\019\000\026\000\020\000\025\000\000\000\
\\001\000\021\000\015\000\000\000\
\\062\000\000\000\
\\063\000\005\000\012\000\000\000\
\\064\000\000\000\
\\065\000\000\000\
\\066\000\000\000\
\\067\000\000\000\
\\068\000\010\000\035\000\011\000\034\000\012\000\033\000\013\000\032\000\
\\014\000\031\000\000\000\
\\069\000\000\000\
\\070\000\000\000\
\\071\000\012\000\033\000\013\000\032\000\014\000\031\000\000\000\
\\072\000\012\000\033\000\013\000\032\000\014\000\031\000\000\000\
\\073\000\000\000\
\\074\000\000\000\
\\075\000\000\000\
\\076\000\023\000\054\000\000\000\
\\077\000\000\000\
\\078\000\000\000\
\\079\000\010\000\035\000\011\000\034\000\012\000\033\000\013\000\032\000\
\\014\000\031\000\000\000\
\\080\000\010\000\035\000\011\000\034\000\012\000\033\000\013\000\032\000\
\\014\000\031\000\000\000\
\\081\000\010\000\035\000\011\000\034\000\012\000\033\000\013\000\032\000\
\\014\000\031\000\000\000\
\\082\000\010\000\035\000\011\000\034\000\012\000\033\000\013\000\032\000\
\\014\000\031\000\000\000\
\\083\000\010\000\035\000\011\000\034\000\012\000\033\000\013\000\032\000\
\\014\000\031\000\000\000\
\\084\000\010\000\035\000\011\000\034\000\012\000\033\000\013\000\032\000\
\\014\000\031\000\000\000\
\"
val actionRowNumbers =
"\002\000\024\000\023\000\004\000\
\\020\000\019\000\007\000\008\000\
\\018\000\022\000\002\000\001\000\
\\003\000\003\000\021\000\005\000\
\\009\000\017\000\027\000\026\000\
\\025\000\003\000\011\000\003\000\
\\003\000\003\000\003\000\003\000\
\\003\000\003\000\003\000\003\000\
\\003\000\003\000\006\000\002\000\
\\036\000\038\000\039\000\041\000\
\\040\000\037\000\032\000\031\000\
\\030\000\029\000\028\000\001\000\
\\014\000\010\000\033\000\012\000\
\\013\000\002\000\002\000\015\000\
\\016\000\035\000\034\000\000\000"
val gotoT =
"\
\\001\000\059\000\002\000\005\000\003\000\004\000\004\000\003\000\
\\006\000\002\000\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\014\000\003\000\004\000\004\000\003\000\006\000\002\000\
\\007\000\001\000\000\000\
\\004\000\015\000\000\000\
\\005\000\017\000\008\000\016\000\000\000\
\\005\000\020\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\017\000\008\000\034\000\000\000\
\\000\000\
\\005\000\036\000\000\000\
\\005\000\037\000\000\000\
\\005\000\038\000\000\000\
\\005\000\039\000\000\000\
\\005\000\040\000\000\000\
\\005\000\041\000\000\000\
\\005\000\042\000\000\000\
\\005\000\043\000\000\000\
\\005\000\044\000\000\000\
\\005\000\045\000\000\000\
\\005\000\046\000\000\000\
\\000\000\
\\002\000\048\000\003\000\004\000\004\000\003\000\006\000\002\000\
\\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\049\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\055\000\003\000\004\000\004\000\003\000\006\000\002\000\
\\007\000\001\000\000\000\
\\002\000\056\000\003\000\004\000\004\000\003\000\006\000\002\000\
\\007\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 60
val numrules = 23
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | INT of unit ->  (int) | ID of unit ->  (string)
 | bool_expression of unit ->  (Ast.boolexp)
 | iteration_statement of unit ->  (Ast.statement)
 | selection_statement of unit ->  (Ast.statement)
 | exp of unit ->  (Ast.exp)
 | assignment_statement of unit ->  (Ast.statement)
 | statement of unit ->  (Ast.statement)
 | statements of unit ->  (Ast.statement list)
 | start of unit ->  (Ast.li)
end
type svalue = MlyValue.svalue
type result = Ast.li
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "ID"
  | (T 2) => "INT"
  | (T 3) => "COMMA"
  | (T 4) => "SEMICOLON"
  | (T 5) => "LPAREN"
  | (T 6) => "RPAREN"
  | (T 7) => "LBRACE"
  | (T 8) => "RBRACE"
  | (T 9) => "PLUS"
  | (T 10) => "MINUS"
  | (T 11) => "TIMES"
  | (T 12) => "DIVIDE"
  | (T 13) => "MOD"
  | (T 14) => "NEQ"
  | (T 15) => "LT"
  | (T 16) => "LE"
  | (T 17) => "GT"
  | (T 18) => "GE"
  | (T 19) => "EEQ"
  | (T 20) => "EQ"
  | (T 21) => "IF"
  | (T 22) => "ELSE"
  | (T 23) => "WHILE"
  | (T 24) => "FOR"
  | (T 25) => "BREAK"
  | (T 26) => "TRUE"
  | (T 27) => "FALSE"
  | (T 28) => "PRINTF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22)
 $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15)
 $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8)
 $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.statements statements1, statements1left, 
statements1right)) :: rest671)) => let val  result = MlyValue.start
 (fn _ => let val  (statements as statements1) = statements1 ()
 in ( statements )
end)
 in ( LrTable.NT 0, ( result, statements1left, statements1right), 
rest671)
end
|  ( 1, ( ( _, ( MlyValue.statement statement1, statement1left, 
statement1right)) :: rest671)) => let val  result = 
MlyValue.statements (fn _ => let val  (statement as statement1) = 
statement1 ()
 in ( [statement] )
end)
 in ( LrTable.NT 1, ( result, statement1left, statement1right), 
rest671)
end
|  ( 2, ( ( _, ( MlyValue.statements statements1, _, statements1right)
) :: _ :: ( _, ( MlyValue.statement statement1, statement1left, _)) ::
 rest671)) => let val  result = MlyValue.statements (fn _ => let val 
 (statement as statement1) = statement1 ()
 val  (statements as statements1) = statements1 ()
 in (  statement :: statements  )
end)
 in ( LrTable.NT 1, ( result, statement1left, statements1right), 
rest671)
end
|  ( 3, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( 
MlyValue.assignment_statement assignment_statement1, 
assignment_statement1left, _)) :: rest671)) => let val  result = 
MlyValue.statement (fn _ => let val  (assignment_statement as 
assignment_statement1) = assignment_statement1 ()
 in (assignment_statement)
end)
 in ( LrTable.NT 2, ( result, assignment_statement1left, 
SEMICOLON1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.selection_statement selection_statement1, 
selection_statement1left, selection_statement1right)) :: rest671)) =>
 let val  result = MlyValue.statement (fn _ => let val  (
selection_statement as selection_statement1) = selection_statement1 ()
 in (selection_statement)
end)
 in ( LrTable.NT 2, ( result, selection_statement1left, 
selection_statement1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.iteration_statement iteration_statement1, 
iteration_statement1left, iteration_statement1right)) :: rest671)) =>
 let val  result = MlyValue.statement (fn _ => let val  (
iteration_statement as iteration_statement1) = iteration_statement1 ()
 in (iteration_statement)
end)
 in ( LrTable.NT 2, ( result, iteration_statement1left, 
iteration_statement1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.assignment_statement (fn _ => let val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in (Ast.stmt(Ast.assign_ (Ast.id_(ID) , exp )) )
end)
 in ( LrTable.NT 3, ( result, ID1left, exp1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  (ID as ID1) = ID1 ()
 in ( Ast.id1_(ID)  )
end)
 in ( LrTable.NT 4, ( result, ID1left, ID1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671))
 => let val  result = MlyValue.exp (fn _ => let val  (INT as INT1) = 
INT1 ()
 in ( Ast.int1_(INT) )
end)
 in ( LrTable.NT 4, ( result, INT1left, INT1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Ast.exp_(Ast.Plus , exp1 ,exp2))
end)
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Ast.exp_(Ast.Minus , exp1 ,exp2))
end)
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Ast.exp_(Ast.Times , exp1 ,exp2))
end)
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Ast.exp_(Ast.Divide , exp1 ,exp2))
end)
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Ast.exp_(Ast.Mod , exp1 ,exp2))
end)
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 14, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.statements 
statements1, _, _)) :: _ :: _ :: ( _, ( MlyValue.bool_expression 
bool_expression1, _, _)) :: _ :: ( _, ( _, IF1left, _)) :: rest671))
 => let val  result = MlyValue.selection_statement (fn _ => let val  (
bool_expression as bool_expression1) = bool_expression1 ()
 val  (statements as statements1) = statements1 ()
 in (Ast.ifonlystmt(Ast.ifonly_(bool_expression , statements)))
end)
 in ( LrTable.NT 5, ( result, IF1left, RBRACE1right), rest671)
end
|  ( 15, ( ( _, ( _, _, RBRACE2right)) :: ( _, ( MlyValue.statements 
statements2, _, _)) :: _ :: _ :: _ :: ( _, ( MlyValue.statements 
statements1, _, _)) :: _ :: _ :: ( _, ( MlyValue.bool_expression 
bool_expression1, _, _)) :: _ :: ( _, ( _, IF1left, _)) :: rest671))
 => let val  result = MlyValue.selection_statement (fn _ => let val  (
bool_expression as bool_expression1) = bool_expression1 ()
 val  statements1 = statements1 ()
 val  statements2 = statements2 ()
 in (
Ast.ifelsestmt(Ast.ifelse_(bool_expression , statements1 , statements2))
)
end)
 in ( LrTable.NT 5, ( result, IF1left, RBRACE2right), rest671)
end
|  ( 16, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.statements 
statements1, _, _)) :: _ :: _ :: ( _, ( MlyValue.assignment_statement 
assignment_statement2, _, _)) :: _ :: ( _, ( MlyValue.bool_expression 
bool_expression1, _, _)) :: _ :: ( _, ( MlyValue.assignment_statement 
assignment_statement1, _, _)) :: _ :: ( _, ( _, FOR1left, _)) :: 
rest671)) => let val  result = MlyValue.iteration_statement (fn _ =>
 let val  assignment_statement1 = assignment_statement1 ()
 val  (bool_expression as bool_expression1) = bool_expression1 ()
 val  assignment_statement2 = assignment_statement2 ()
 val  (statements as statements1) = statements1 ()
 in (
Ast.floop(Ast.forloop_(assignment_statement1 , bool_expression ,assignment_statement2 , statements))
)
end)
 in ( LrTable.NT 6, ( result, FOR1left, RBRACE1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.bool_expression (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Ast.boolexp_( Ast.EEQ ,exp1 ,exp2))
end)
 in ( LrTable.NT 7, ( result, exp1left, exp2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.bool_expression (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Ast.boolexp_( Ast.NEQ ,exp1 ,exp2))
end)
 in ( LrTable.NT 7, ( result, exp1left, exp2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.bool_expression (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Ast.boolexp_( Ast.GE ,exp1 ,exp2))
end)
 in ( LrTable.NT 7, ( result, exp1left, exp2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.bool_expression (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Ast.boolexp_( Ast.GT ,exp1 ,exp2))
end)
 in ( LrTable.NT 7, ( result, exp1left, exp2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.bool_expression (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Ast.boolexp_( Ast.LT ,exp1 ,exp2))
end)
 in ( LrTable.NT 7, ( result, exp1left, exp2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.bool_expression (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Ast.boolexp_( Ast.LE ,exp1 ,exp2))
end)
 in ( LrTable.NT 7, ( result, exp1left, exp2right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : C_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun EEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINTF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
end
end
