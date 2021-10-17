%{
open Printf
open List
open Module									
let i = ref 0;; 								(*Keeps track of the clause number in the programs*)
%}

%token <string> INTEGER
%token <string> FLOAT
%token DOT LPAREN RPAREN COMMA SEPARATOR EOF
%token <string> VARIABLE ATOM


%start program							/*The starting non terminal symbol, myProg is an OCaml data type which is a clause list*/
%type <Module.myProg> program 	

%%
program: clause_list EOF													{$1}
		 ;

clause_list: 																{[]}
			| clause DOT clause_list										{$1 :: $3}
			;

clause:  fact																{i := !i + 1; $1} 	/*Incrementing the counter on each clause */
		|rule																{i := !i + 1; $1}
		;

fact: atomic_formula														{($1,[])}
	  ;

rule: atomic_formula SEPARATOR atomic_formula_list							{($1,$3)}
	  ;

atomic_formula_list:  atomic_formula										{[$1]}
					| atomic_formula COMMA atomic_formula_list				{$1 :: $3}				
					;

atomic_formula:  ATOM 														{Node($1,[])}
				|ATOM LPAREN term_list RPAREN								{Node($1,$3)}
				;
term_list :  term 															{[$1]}
			|term COMMA term_list											{$1 :: $3}
			;
term:  VARIABLE 															{V((string_of_int (!i))^($1))}  /*The variables in the database are augmented with the clause number they belong to*/
	  |INTEGER																{Node($1,[])}
	  |FLOAT 																{Node($1,[])}
	  |atomic_formula 														{$1}
	  ;
%%