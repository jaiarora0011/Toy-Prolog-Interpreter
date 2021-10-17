# Toy Prolog Interpreter (COL226 - Assignment 6)
Designed a simplified Prolog Interpreter in OCaml which does resolution of query goals using unification and backtracking, with REPL support

## Usage

- Use `make` to build the project which will generate an executable `PrologInterpreter`
- Call `./PrologInterpreter <input_database>` or `make execute INPUT_FILE=<input_database>` to load the database and start the REPL
- Now you can enter queries in Prolog Syntax. Enter `halt.` to exit the REPL
- The interpreter is capable to finding more than one answers to the given query. Enter `;` to get the next answer (if there is one), or enter `.` to stop the backtracking search and return to the REPL
