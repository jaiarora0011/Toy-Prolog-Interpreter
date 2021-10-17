open List
open Printf
type variable = string 									
type symbol = string										(*Variables and symbols are just strings*)
type term = V of variable | Node of symbol * (term list)
type arity = int
type signature = (symbol * arity) list  					(*Signature contains symbol-arity pairs*)
type substitution = (variable * term) list 					(*substitution is a list of variable-term pairs*)

type clause = term * (term list)
type myProg = clause list
type query = term list

exception InvalidSignature
exception InvalidSymbol
exception NOT_UNIFIABLE
exception InvalidTerm
exception FatalFailure

(*
let mySignature : signature = [("add",2);("subt",2);("uMINUS",1);("mult",2);("div",2);("mod",2);("0",0);("prec",1);("succ",1);("1",0)]
let testsig1 :signature = [("hello there",0);("hey",1);("lol",-1);("lmao",3);("lmao",3);("1",0)]
let t1 = V("x");;
let t2 = Node("0",[]);;
let t3 = Node("add",[V("x");V("y")]);;
let t4 = Node("mult",[Node("0",[]);V("z")]);;
let t5 = Node("prec",[Node("mult",[Node("div",[V("x");Node("prec",[V("t")])]);Node("succ",[V("a")])])]);;
let t6 = Node("div",[t4;t5]);;
let testcases = [t1;t2;t3;t4;t5;t6]
*)

(*
  Takes a list of type 'a and a function from 'a to 'b and returns a list of type 'b
  Time Complexity: O(n), where n is the number of elements in the list.
*)
let rec map f l = match l with
				[] -> []
				|x :: xs -> (f x) :: (map f xs)

(*
  Time complexity: O(n), where n is the number of elements in the list
*)
let rec foldl f e l = match l with
					[] -> e
					| x :: xs -> foldl f (f e x) xs

let max_int x y =  if(x < y) then y else x 				
let int_addition y x = x + y
let and_op b1 b2 = b1 && b2 				(*Takes 2 bool values and calculates their 'AND'*)


(*
  Takes a list of pairs of any arbitrary type, and splits the original list into a pair of lists, where first list contains all the first projections of the tuples and so on
  Time Complexity: O(n), where n is the number of pairs in the original list
*)
let rec split_list l = match l with
							[] -> ([],[])
							| (x,y) ::  xs -> begin 
												let (l1,l2) = (split_list xs) in
												(x :: l1,y :: l2)
											end

(*
  Tells whether a given element is present in the list or not.
  Time Complexity: O(n) worst case, where n is the number of elements in the list
*)
let rec search l e = match l with
					[] -> false
					|x :: xs -> if x = e then true else search xs e

(*
  Takes 2 lists as input and returns a list whose elements are present either in l1 or l2.
  Time Complexity: O(n1 * n2), where n1, n2 are the respective lengths of the two lists.
*)
let rec list_union l1 l2 = match l1 with
						[] -> l2
						|x :: xs -> if(search l2 x) then list_union xs l2 else x :: (list_union xs l2)

(*
  Given a signature and a symbol, this function searchefor the symbol in the pair list, and returns it's arity if it finds it
  Time Complexity: O(n), where n is the number of elements in the list
*)
let rec get_arity (s:signature) (sym:symbol) = match s with
										[] -> raise InvalidSymbol
										| (x,ar) :: xs -> if(sym = x) then ar else get_arity xs sym

(*
  Given a signature and a symbol, this function tells if the given symbol is present in the list or not. It does so by splitting the pair list first and calls the search function on the first list.
  Time Complexity: O(n), where n is the number of elements in the list
*)
let rec isPresentinSig (s:signature) (sym:symbol) = begin 
											let (l1,l2) = split_list s in
											search l1 sym
										end

(*
  Checks if a given signature is a valid one or not. For this traverse over the list and check if the arity is non negative or not, or if the signature is present once or more than once in the signature.
  Time Complexity: O(n*n) worst case, where n is the number of pairs in the signature.	
*)
let rec check_sig (s:signature) = match s with
								[] -> true    						(*Empty signature is a valid one*)
								| (sym,ar) :: xs -> if(ar < 0 || (isPresentinSig xs sym)) then false else check_sig xs 	
		(*If arity is negative or the symbol is present in the remaining list, then signature is invalid, else I recursively call the function on the remaining list*)

(*
  Tells is a given term is wellformed wrt a given signature or not. For this, pattern matching is done on the term.
  If the term is just a variable node, then it is well formed. Else if the term is a Node of symbol * term list, then is the arity of the symbol is not same as the length of the list, then it is an invalid term,
  else I call wfterm on the term list, so this term will be well formed if all the terms in the list are well formed. I map the term list to the boolean values, and use foldl to check if all values are true or not.
  
  Time Complexity: O(n^h), where n is the number of pairs in the signature and h is the height of the term. T(h) = O(n)*T(h-1) + O(n)
*)
let rec wfterm (s:signature) (t:term) = if not(check_sig s) then raise InvalidSignature 
									else begin
										match t with
										V(var) -> true
										|Node(sym,l) -> if not(length l = get_arity s sym) then false else foldl and_op true (map (wfterm s) l)
									end

(*
  Returns the size of a given term. If the term is a variable node, then I return 1, else if it a node, then I calculate the size of all the terms in the term list,
  and add then using foldl, and return 1 plus that quantity
*)
let rec size (t:term) = match t with
						V(var) -> 1
						| Node(sym,l) -> 1 + (foldl int_addition 0 (map size l))

(*
  Returns the height of a given term. If the term is a variable node or a Node with a 0-ary symbol, then the height is 0, else it 1 + (the max of the heights of it's subterms)
*)
let rec ht (t:term) = match t with
						V(var) -> 0
						| Node(_,[]) -> 0
						| Node(sym,l) -> 1 + (foldl max_int 0 (map ht l))

(*
  Returns a list of variables present in a term. Pattern matching is done on the term, and it is just a variable node, then I return a singeton list with that variable,
  else if it is a node, then I map each term in term list to it's own list of variables, and then calculate their union using foldl.
*)
let rec vars (t:term) = begin 
						match t with
						V(var) -> [var]
						|Node(sym,l) -> foldl list_union [] (map vars l)
					end;;

(*
check_sig mySignature;;
check_sig testsig1;;
map (wfterm mySignature) testcases;;
map size testcases;;
map ht testcases;;
map vars testcases;;

let subst1 : substitution = [("x",V("abc"));("y",Node("0",[]));("z",Node("mult",[Node("add",[V("x");V("z")]);V("a")]))];;
let subst2 :substitution = [("abc",Node("prec",[Node("0",[])]));("x",Node("div",[V("z");V("x")]))];;
let s : substitution = [("x",V("y"));("y",V("x"))];;
*)

(*
  A substitution which is empty, i.e., [] will be the identity substitution.
*)

(*
  This function takes a substitution and a variable, and searches for the variable in it, if found, then the corresponding term is returned, else the variable remians untouched.
  So (substitute s) is a function from variable to term.

  Time Complexity: O(n) worst case, where n is the number of pairs in the substitution.
*)
let rec substitute (s:substitution) (var:variable) = match s with
												[] -> V(var)
												| (v,t) :: xs -> if(var = v) then t else substitute xs var

(*
  This function takes a substitution and term. Patterm matching is done on the term, if it us just a variable node, then the corresponding substitution of that variables is replaced.
  Else if it is a Node, then the term list is mapped to their corresponsing substitution using subst recursively.

  Time Complexity: O(n^h) worst case, where n is the average arity of each symbol, and h is the height of the term. T(h) = O(n) * T(h-1)
*)
let rec subst (s:substitution) (t:term) = match t with
										V(var) -> substitute s var
									   |Node(sym,l) -> Node(sym,(map (subst s) l))

(*
  This function takes substitutions and returns another substitution whose elements have those variables which have mappings in s1 but not in s2.

  Time Complexity: O(n1 * n2), where n1, n2 are the lengths of the 2 substitutions.
*)
let rec difference (s1:substitution) (s2:substitution) :substitution = match s1 with
														[] -> []
														| (v,t) :: xs -> begin
															let (l1,l2) = split_list s2 in
															if(search l1 v) then difference xs s2
														else (v,t) :: (difference xs s2)
														end

(*
  Maps each term in variable term pair in s1 to it's substitution wrt s2.
*)
let rec compose_helper (s1:substitution) (s2:substitution) :substitution = match s1 with
														[] -> []
														| (v,t) :: xs -> (v,(subst s2 t)) :: (compose_helper xs s2) 
(*
  This function takes a substitution and removes all the mappings of the type identity.

  Time Complexity: O(n) worst case, where n is the number of pairs in the substitution.
*)
let rec reduce (s:substitution) : substitution = match s with
								[] -> []
								|(var,V(v)) :: xs -> if(var = v) then (reduce xs) else (var,V(v)) :: (reduce xs)
								| x :: xs -> x :: (reduce xs)
(*
  Compose s2 s1 returns the substitution s1.s2, i.e., s1 was applied first, and then s2 was applied.
*)

let rec compose (s2:substitution) (s1:substitution) :substitution = reduce ((compose_helper s1 s2) @ (difference s2 s1)) 

let rec mgu_helper (l1:term list) (l2:term list) (mgu_acc:substitution) = match l1, l2 with
									[], [] -> mgu_acc
									| x :: xs, y :: ys -> let s = mgu x y in
														mgu_helper (map (subst s) xs) (map (subst s) ys) (compose s mgu_acc)
									| _, _ -> raise FatalFailure
(*
  Returns the unifier of 2 terms. Patterm matching is done on both the terms.
*)
and mgu (t1:term) (t2:term) :substitution = match t1, t2 with
											V(var1),V(var2) -> begin           
														if(var1 = var2) then [] 
													else [(var1,V(var2))] 
												end
											| V(var),Node(sym,l) -> begin 
														if (search (vars (Node(sym,l))) var) then raise NOT_UNIFIABLE  (*OCCURS check*)
													else [(var,Node(sym,l))]
												end
											| Node(sym,l), V(var) -> begin 
														if (search (vars (Node(sym,l))) var) then raise NOT_UNIFIABLE  (*OCCURS check*)
													else [(var,Node(sym,l))]
												end
											| Node(s1,l1),Node(s2,l2) -> begin 
														if not(s1 = s2) then raise NOT_UNIFIABLE 
													else mgu_helper l1 l2 []
												end;;
(*
let replaceChar (i:int) (c:char) = if not(i = 0) then c else match c with
									'_' -> '~'
									| _ -> Char.lowercase_ascii c 

let revertChar (i:int) (c:char) = if not(i = 0) then c else match c with
									'~' -> '_'
									| _ -> Char.uppercase_ascii c 
 
let rec convertVar (s:string) = String.mapi replaceChar s

let rec revertVar (s:string) = String.mapi revertChar s
*)

(*
let rec resolveGoalHelper1 (l:term list) (origProg:myProg) (s:substitution)  = match l with
															 [] -> (s,true)
															|x :: xs -> let (res,b) = resolveGoalHelper origProg x origProg in
																	if b then
																		resolveGoalHelper1 (map (subst res) xs) origProg (compose res s) 
																	else ([],false)

and resolveGoalHelper (clauselist:myProg) (goal:term) (origProg:myProg) = match clauselist with
														 [] -> ([],false)
														|x :: xs -> try
															let (head,body) = x in
															let altSolution = resolveGoalHelper xs goal origProg in
																match body with
																 [] -> (mgu goal head,true)
																|_ -> resolveGoalHelper1 (map (subst (mgu head goal)) body) origProg []
														with
														| NOT_UNIFIABLE -> resolveGoalHelper xs goal origProg
*)

let rec printTermList (l:term list) = match l with
									[] -> ()
									|x :: [] -> printTerm x
									|x :: xs -> let () = printTerm x in
												let () = printf "," in
												printTermList xs

and printTerm (t:term) = match t with
							V(var) -> printf "%s" var
							|Node(sym,l) -> let () = printf "%s" sym in
											begin
												match l with
												[] -> ()
												| _ -> let () = printf "(" in
														let () = printTermList l in
														let () = printf ")" in
														()
											end

let rec printSubstitution (s:substitution) = match s with
											[] -> printf "Yes.\n"
											|x :: [] -> let (var,t) = x in
														let () = printf "%s = " var in
														let () = printTerm t in
														()
											|x :: xs -> let (var,t) = x in
														let () = printf "%s = " var in
														let () = printTerm t in
														let () = print_newline () in
														printSubstitution xs

let rec promptHelper () = let () = printf " ?\n" in      	(*Asks for a prompt from the user, which decides whether to search for more solutions or not*)
							 let prompt = read_line () in
							if prompt = ";" then true    	(*Backtrack and Search for next solution*)
						else if prompt = "." then false  	(*OR halt, and find no more solutions*)
					else promptHelper () 					(*If an invalid prompt is entered, then ask for a prompt again*)


(*
  This function takes a variable list and a substitution, and removes all the mappings from the substitution from those variables which are not present in the variable list
*)
let rec removeVariables (s:substitution) (varList:variable list) = match s with 
																  [] -> []
																| (v,t) :: xs -> if(search varList v) then (v,t) :: (removeVariables xs varList)
																			else removeVariables xs varList

(*
  myQuery: It is of the form g1,g2,g3,.. which are all the goals which are to be resolved.
  prog contains all the clauses which have not been checked for unification with the head  of the current query/term list.
  origProg: Contains the original database/clause list
  varList: Contains all the variables present in the query

  This function returns a pair of two boolean values: (b1,b2), where b1 tells if at least one solution was found or not,
  																and b2 denotes if I need to backtrack and find the next solution or not
*)
let rec resolveQueryHelper (prog:myProg) (myQuery:query) (s:substitution list) (origProg:myProg) (varList:variable list) = 
							match myQuery with
							[] -> let () = printSubstitution (removeVariables (hd s) varList) in   
								(true,promptHelper ())
							|g :: gs -> begin
								match prog with
								[] -> (false,true)
								|(t,l) :: ys -> begin
										try
									let theta_ = mgu t g in
									let b1,b2 = resolveQueryHelper origProg (map (subst theta_) (l@gs)) (map (compose theta_) s) origProg varList in
										if b2 then begin
											let b3,b4 = resolveQueryHelper ys myQuery s origProg varList in
											(b1||b3,b4)
										end
									else (b1,b2)
								with
								| NOT_UNIFIABLE ->  resolveQueryHelper ys myQuery s origProg varList
							end
						end

let rec resolveQuery (prog:myProg) (myQuery:query) = let varList = foldl list_union [] (map vars myQuery) in   		(*All the variables present in the query*)
													let (b1,b2) = resolveQueryHelper prog myQuery [[]] prog varList in
														if b1 then () else printf "No.\n"  (*If no solution was found, then just "No" is printed*)


