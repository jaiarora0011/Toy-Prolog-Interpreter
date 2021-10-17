%{
open Printf
open List
open Module									(*opens the module containing all the functions which are to be called on a sheet*)

%}

%token <string> INTEGER
%token <string> FLOAT
%token DOT LPAREN RPAREN COMMA EOF
%token <string> VARIABLE ATOM


%start query							/*The starting non terminal symbol*/
%type <Module.query option> query  		/*Query is an OCaml data type, which is a term list*/	

%%
query: foo																{$1}
	| error 															{None}	/*Return None if the query is invalid*/
	;	
foo: atomic_formula_list DOT											{Some($1)}
	| error DOT															{None}
	;

atomic_formula_list: atomic_formula 									{[$1]}
					| atomic_formula COMMA atomic_formula_list 			{$1 :: $3}
					;	

atomic_formula:  ATOM 													{Node($1,[])}
				|ATOM LPAREN term_list RPAREN							{Node($1,$3)}
				;
term_list :  term 														{[$1]}
			|term COMMA term_list										{$1 :: $3}
			;
term:  VARIABLE 														{V($1)} 		/*The variables are kept as they are in prolog*/ 
	  |INTEGER															{Node($1,[])} 	/*so as to distinguish them from the variables in the database*/
	  |FLOAT 															{Node($1,[])}
	  |atomic_formula 													{$1}
	  ;
%%