{	
	open Printf
	open YaccFile2
}

let integers = ('-')?('0'|['1' - '9']['0' - '9']*)												(*Regex for integers*)
let floats = ('-')?('0'|['1' - '9']['0' - '9']*)'.'('0'|['0' - '9']*['1' - '9']) 				(*Regex for floats*)
let variables = ['A' - 'Z' '_']['A' - 'Z' 'a' - 'z' '0' - '9' '_']* 							(*Regex for Prolog variables*)
let strings = '\''['A' - 'Z' '_' 'a' - 'z' '0' - '9' ' ']*'\''
let atom = (['a' - 'z']['A' - 'Z' 'a' - 'z' '0' - '9' '_']*)|(strings) 							(*Regex for atoms, include strings as well*)

rule token = parse
| [' ' '\t' '\n']									{token lexbuf}								(*Ignoring all the whitespaces*)		
| integers as lxm									{INTEGER(lxm)}		
| floats as lxm										{FLOAT(lxm)}	
| '('												{LPAREN}		
| ')'											 	{RPAREN}
|','												{COMMA}
|variables as lxm									{VARIABLE(lxm)}
|atom as lxm										{ATOM(lxm)}
|'.'												{DOT}
| eof												{EOF}			
| _ 												{token lexbuf}								(*Ignoring any other garbage Characters*)