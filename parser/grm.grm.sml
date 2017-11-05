functor LrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : _TOKENS
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
\\001\000\002\000\015\000\003\000\014\000\027\000\013\000\031\000\012\000\
\\033\000\011\000\034\000\010\000\000\000\
\\001\000\002\000\033\000\003\000\014\000\000\000\
\\001\000\002\000\033\000\003\000\014\000\033\000\011\000\034\000\010\000\000\000\
\\001\000\002\000\046\000\000\000\
\\001\000\006\000\027\000\000\000\
\\001\000\006\000\049\000\000\000\
\\001\000\006\000\053\000\000\000\
\\001\000\007\000\029\000\000\000\
\\001\000\007\000\030\000\000\000\
\\001\000\008\000\050\000\000\000\
\\001\000\008\000\057\000\000\000\
\\001\000\010\000\052\000\000\000\
\\001\000\010\000\059\000\000\000\
\\001\000\010\000\060\000\000\000\
\\001\000\011\000\056\000\000\000\
\\001\000\011\000\063\000\000\000\
\\001\000\011\000\064\000\000\000\
\\001\000\012\000\026\000\013\000\025\000\014\000\024\000\015\000\023\000\
\\016\000\022\000\018\000\021\000\019\000\020\000\020\000\019\000\
\\021\000\018\000\022\000\017\000\023\000\016\000\000\000\
\\001\000\017\000\031\000\000\000\
\\066\000\000\000\
\\067\000\009\000\028\000\000\000\
\\068\000\000\000\
\\069\000\000\000\
\\070\000\000\000\
\\071\000\000\000\
\\072\000\000\000\
\\073\000\012\000\026\000\013\000\025\000\014\000\024\000\015\000\023\000\
\\016\000\022\000\000\000\
\\074\000\000\000\
\\074\000\017\000\031\000\000\000\
\\075\000\000\000\
\\076\000\014\000\024\000\015\000\023\000\016\000\022\000\000\000\
\\077\000\014\000\024\000\015\000\023\000\016\000\022\000\000\000\
\\078\000\000\000\
\\079\000\000\000\
\\080\000\000\000\
\\081\000\029\000\058\000\000\000\
\\082\000\000\000\
\\083\000\000\000\
\\084\000\012\000\026\000\013\000\025\000\014\000\024\000\015\000\023\000\
\\016\000\022\000\000\000\
\\085\000\012\000\026\000\013\000\025\000\014\000\024\000\015\000\023\000\
\\016\000\022\000\000\000\
\\086\000\012\000\026\000\013\000\025\000\014\000\024\000\015\000\023\000\
\\016\000\022\000\000\000\
\\087\000\012\000\026\000\013\000\025\000\014\000\024\000\015\000\023\000\
\\016\000\022\000\000\000\
\\088\000\012\000\026\000\013\000\025\000\014\000\024\000\015\000\023\000\
\\016\000\022\000\000\000\
\\089\000\012\000\026\000\013\000\025\000\014\000\024\000\015\000\023\000\
\\016\000\022\000\000\000\
\\090\000\000\000\
\\091\000\000\000\
\"
val actionRowNumbers =
"\001\000\026\000\025\000\024\000\
\\018\000\005\000\021\000\020\000\
\\046\000\045\000\008\000\009\000\
\\030\000\029\000\002\000\002\000\
\\002\000\002\000\002\000\002\000\
\\002\000\002\000\002\000\002\000\
\\002\000\023\000\001\000\004\000\
\\003\000\002\000\039\000\028\000\
\\041\000\042\000\044\000\043\000\
\\040\000\035\000\034\000\033\000\
\\032\000\031\000\022\000\006\000\
\\019\000\010\000\027\000\003\000\
\\012\000\007\000\001\000\004\000\
\\015\000\011\000\036\000\013\000\
\\014\000\001\000\001\000\016\000\
\\017\000\038\000\037\000\000\000"
val gotoT =
"\
\\001\000\063\000\002\000\007\000\003\000\006\000\004\000\005\000\
\\005\000\004\000\006\000\003\000\007\000\002\000\008\000\001\000\000\000\
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
\\000\000\
\\000\000\
\\005\000\030\000\000\000\
\\005\000\032\000\000\000\
\\005\000\033\000\000\000\
\\005\000\034\000\000\000\
\\005\000\035\000\000\000\
\\005\000\036\000\000\000\
\\005\000\037\000\000\000\
\\005\000\038\000\000\000\
\\005\000\039\000\000\000\
\\005\000\040\000\000\000\
\\005\000\041\000\000\000\
\\000\000\
\\002\000\042\000\003\000\006\000\004\000\005\000\005\000\004\000\
\\006\000\003\000\007\000\002\000\008\000\001\000\000\000\
\\004\000\043\000\000\000\
\\005\000\004\000\008\000\045\000\000\000\
\\005\000\046\000\000\000\
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
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\004\000\008\000\049\000\000\000\
\\000\000\
\\000\000\
\\002\000\052\000\003\000\006\000\004\000\005\000\005\000\004\000\
\\006\000\003\000\007\000\002\000\008\000\001\000\000\000\
\\004\000\053\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\059\000\003\000\006\000\004\000\005\000\005\000\004\000\
\\006\000\003\000\007\000\002\000\008\000\001\000\000\000\
\\002\000\060\000\003\000\006\000\004\000\005\000\005\000\004\000\
\\006\000\003\000\007\000\002\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 64
val numrules = 26
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
end
type svalue = MlyValue.svalue
type result = unit
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
  | (T 4) => "COLON"
  | (T 5) => "SEMICOLON"
  | (T 6) => "LPAREN"
  | (T 7) => "RPAREN"
  | (T 8) => "NEWLINE"
  | (T 9) => "LBRACE"
  | (T 10) => "RBRACE"
  | (T 11) => "PLUS"
  | (T 12) => "MINUS"
  | (T 13) => "TIMES"
  | (T 14) => "DIVIDE"
  | (T 15) => "MOD"
  | (T 16) => "EQ"
  | (T 17) => "NEQ"
  | (T 18) => "LT"
  | (T 19) => "LE"
  | (T 20) => "GT"
  | (T 21) => "GE"
  | (T 22) => "EEQ"
  | (T 23) => "AND"
  | (T 24) => "OR"
  | (T 25) => "ASSIGN"
  | (T 26) => "IF"
  | (T 27) => "THEN"
  | (T 28) => "ELSE"
  | (T 29) => "WHILE"
  | (T 30) => "FOR"
  | (T 31) => "BREAK"
  | (T 32) => "true"
  | (T 33) => "false"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27)
 $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20)
 $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13)
 $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ 
(T 5) $$ (T 4) $$ (T 3) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.ntVOID program1, program1left, 
program1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  (program as program1) = program1 ()
 in (program)
end; ()))
 in ( LrTable.NT 0, ( result, program1left, program1right), rest671)

end
|  ( 1, ( ( _, ( MlyValue.ntVOID statement1, statement1left, 
statement1right)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  (statement as statement1) = statement1 ()
 in ( statement )
end; ()))
 in ( LrTable.NT 1, ( result, statement1left, statement1right), 
rest671)
end
|  ( 2, ( ( _, ( MlyValue.ntVOID program1, _, program1right)) :: _ :: 
( _, ( MlyValue.ntVOID statement1, statement1left, _)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  (statement as 
statement1) = statement1 ()
 val  (program as program1) = program1 ()
 in ( Ast.statements_(statement , program ))
end; ()))
 in ( LrTable.NT 1, ( result, statement1left, program1right), rest671)

end
|  ( 3, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.ntVOID 
assignment_exp1, assignment_exp1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  assignment_exp1 = 
assignment_exp1 ()
 in (exp)
end; ()))
 in ( LrTable.NT 2, ( result, assignment_exp1left, SEMICOLON1right), 
rest671)
end
|  ( 4, ( ( _, ( MlyValue.ntVOID selection_statement1, 
selection_statement1left, selection_statement1right)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  (
selection_statement as selection_statement1) = selection_statement1 ()
 in (selection_statement)
end; ()))
 in ( LrTable.NT 2, ( result, selection_statement1left, 
selection_statement1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.ntVOID iteration_statement1, 
iteration_statement1left, iteration_statement1right)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  (
iteration_statement as iteration_statement1) = iteration_statement1 ()
 in (iteration_statement)
end; ()))
 in ( LrTable.NT 2, ( result, iteration_statement1left, 
iteration_statement1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.ntVOID bool_expression1, 
bool_expression1left, bool_expression1right)) :: rest671)) => let val 
 result = MlyValue.ntVOID (fn _ => ( let val  (bool_expression as 
bool_expression1) = bool_expression1 ()
 in (bool_expression)
end; ()))
 in ( LrTable.NT 2, ( result, bool_expression1left, 
bool_expression1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in (Ast.assign_ (ID , exp ))
end; ()))
 in ( LrTable.NT 3, ( result, ID1left, exp1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  (ID as ID1) = 
ID1 ()
 in (Ast.id_ ID)
end; ()))
 in ( LrTable.NT 4, ( result, ID1left, ID1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671))
 => let val  result = MlyValue.ntVOID (fn _ => ( let val  (INT as INT1
) = INT1 ()
 in (Ast.int_ INT)
end; ()))
 in ( LrTable.NT 4, ( result, INT1left, INT1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Ast.exp_(PLUS , exp1 ,exp2))
end; ()))
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Ast.exp_(MINUS , exp1 ,exp2))
end; ()))
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Ast.exp_(TIMES , exp1 ,exp2))
end; ()))
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Ast.exp_(DIVIDE , exp1 ,exp2))
end; ()))
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Ast.exp_(MOD , exp1 ,exp2))
end; ()))
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 15, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.ntVOID 
program1, _, _)) :: _ :: _ :: ( _, ( MlyValue.ntVOID bool_expression1,
 _, _)) :: _ :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result
 = MlyValue.ntVOID (fn _ => ( let val  (bool_expression as 
bool_expression1) = bool_expression1 ()
 val  (program as program1) = program1 ()
 in (Ast.ifonly_(bool_expression , program))
end; ()))
 in ( LrTable.NT 5, ( result, IF1left, RBRACE1right), rest671)
end
|  ( 16, ( ( _, ( _, _, RBRACE2right)) :: ( _, ( MlyValue.ntVOID 
program2, _, _)) :: _ :: _ :: _ :: ( _, ( MlyValue.ntVOID program1, _,
 _)) :: _ :: _ :: ( _, ( MlyValue.ntVOID bool_expression1, _, _)) :: _
 :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  (bool_expression as 
bool_expression1) = bool_expression1 ()
 val  program1 = program1 ()
 val  program2 = program2 ()
 in (Ast.ifelse_(bool_expression , program1 , program2))
end; ()))
 in ( LrTable.NT 5, ( result, IF1left, RBRACE2right), rest671)
end
|  ( 17, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.ntVOID 
program1, _, _)) :: _ :: _ :: ( _, ( MlyValue.ntVOID assignment_exp2,
 _, _)) :: _ :: ( _, ( MlyValue.ntVOID bool_expression1, _, _)) :: _
 :: ( _, ( MlyValue.ntVOID assignment_exp1, _, _)) :: _ :: ( _, ( _, 
FOR1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  (assignment_exp as assignment_exp1) = assignment_exp1
 ()
 val  (bool_expression as bool_expression1) = bool_expression1 ()
 val  assignment_exp2 = assignment_exp2 ()
 val  (program as program1) = program1 ()
 in (
Ast.forloop_(assignment_exp , bool_expression ,assignment_exp , program)
)
end; ()))
 in ( LrTable.NT 6, ( result, FOR1left, RBRACE1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Ast.boolexp_( EEQ ,exp1 ,exp2))
end; ()))
 in ( LrTable.NT 7, ( result, exp1left, exp2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Ast.boolexp_( NEQ ,exp1 ,exp2))
end; ()))
 in ( LrTable.NT 7, ( result, exp1left, exp2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Ast.boolexp_( GE ,exp1 ,exp2))
end; ()))
 in ( LrTable.NT 7, ( result, exp1left, exp2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Ast.boolexp_( GT ,exp1 ,exp2))
end; ()))
 in ( LrTable.NT 7, ( result, exp1left, exp2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Ast.boolexp_( LT ,exp1 ,exp2))
end; ()))
 in ( LrTable.NT 7, ( result, exp1left, exp2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (Ast.boolexp_( LE ,exp1 ,exp2))
end; ()))
 in ( LrTable.NT 7, ( result, exp1left, exp2right), rest671)
end
|  ( 24, ( ( _, ( _, true1left, true1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => (Ast.bool_(true)))
 in ( LrTable.NT 7, ( result, true1left, true1right), rest671)
end
|  ( 25, ( ( _, ( _, false1left, false1right)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => (Ast.bool_(false)))
 in ( LrTable.NT 7, ( result, false1left, false1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.ntVOID x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : _TOKENS =
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
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun NEWLINE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun EEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun true (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun false (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
end
end
