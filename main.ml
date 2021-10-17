open Printf
open Module
let rec resolve (myProgram:myProg) = let () = printf "?- " in
	let line = read_line () in 										(*input query prompt. The query has to be entered in a single line.*)
    	if not(line = "halt.") then begin     					
    		let lexbuf = Lexing.from_string line in
    		let myQuery = YaccFile2.query LexFile2.token lexbuf in
    		match myQuery with
    		Some(goalList) ->  resolveQuery myProgram goalList; resolve myProgram   (*Valid Query. Resolve it with the database and loop again*)
    		|None -> printf "Invalid query\n"; resolve myProgram 					(*Invalid Query*)
    	end
    	else ();;    			(*Exit prompt for the interpreter*)

let main () = begin
		let filename = Sys.argv.(1) in 							(*Name of input file taken as argument to main*)
		let filePtr = open_in filename in 						(*Input file channel*)
		let lexbuf = Lexing.from_channel filePtr in
		let myProgram = YaccFile1.program LexFile1.token lexbuf in   (*myProgram is of type myProg, which is a clause list, or the database for the interpreter*)
			resolve myProgram
	end;;
main ();;