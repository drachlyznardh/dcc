
YACC := ocamlyacc
LEX  := ocamllex
CC   := ocamlc

APP  := interpreter

eval:
	$(CC) -c syntaxtree.ml
	$(YACC) parser.mly
	$(CC) -c parser.mli	
	$(CC) -c parser.ml	
	$(LEX) lexer.mll
	$(CC) -c lexer.ml
	$(CC) -c mem.ml
	$(CC) -c interpreter.ml
	$(CC) -c main.ml	
	$(CC) -o $(APP) lexer.cmo parser.cmo syntaxtree.cmo mem.cmo interpreter.cmo main.cmo

run: 
	./$(APP) < progs/heap/win/double_malloc.cre

runall:
	for i in input/*; do echo "\nNow executing $$i"; ./$(APP) < $$i; done

test: 
	./.test.sh

clean:
	rm -f parser.mli parser.ml lexer.ml *.cmo *.cmi *~

veryclean: clean
	rm $(APP)

.PHONY: test clean veryclean
