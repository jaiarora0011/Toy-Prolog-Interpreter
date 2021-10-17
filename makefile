LEX = ocamllex
YACC = ocamlyacc
OCAML_COMPILER = ocamlc
FINAL_EXEC = assignment6
LEX_FILE_1 = lexFile1
LEX_FILE_2 = lexFile2
YACC_FILE_1 = yaccFile1
YACC_FILE_2 = yaccFile2
INPUT_FILE = input.txt
MODULE = module
MAIN_FILE = main

.PHONY: all
all: parser scanner linking

.PHONY: scanner
scanner: $(LEX_FILE_1).mll $(LEX_FILE_2).mll
	$(LEX) $(LEX_FILE_1).mll
	$(LEX) $(LEX_FILE_2).mll

.PHONY: parser
parser:	$(YACC_FILE_1).mly $(YACC_FILE_2).mly
	$(YACC) $(YACC_FILE_1).mly
	$(YACC) $(YACC_FILE_2).mly

.PHONY: linking
linking: $(LEX_FILE_1).ml $(YACC_FILE_1).ml $(LEX_FILE_2).ml $(YACC_FILE_2).ml
	$(OCAML_COMPILER) -c $(MODULE).ml
	$(OCAML_COMPILER) -c $(YACC_FILE_1).mli
	$(OCAML_COMPILER) -c $(LEX_FILE_1).ml
	$(OCAML_COMPILER) -c $(YACC_FILE_1).ml
	$(OCAML_COMPILER) -c $(YACC_FILE_2).mli
	$(OCAML_COMPILER) -c $(LEX_FILE_2).ml
	$(OCAML_COMPILER) -c $(YACC_FILE_2).ml
	$(OCAML_COMPILER) -c $(MAIN_FILE).ml
	$(OCAML_COMPILER) -o $(FINAL_EXEC) $(MODULE).cmo $(YACC_FILE_1).cmo $(LEX_FILE_1).cmo $(YACC_FILE_2).cmo $(LEX_FILE_2).cmo $(MAIN_FILE).cmo 

.PHONY: execute
execute:
	./$(FINAL_EXEC) $(INPUT_FILE)

.PHONY: clean
clean:
	rm $(FINAL_EXEC) *.cm* *.o $(YACC_FILE_1).ml $(LEX_FILE_1).ml $(YACC_FILE_2).ml $(LEX_FILE_2).ml *.mli -f
