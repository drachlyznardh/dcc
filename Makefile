
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
	$(CC) -c interpreter.ml
	$(CC) -c main.ml	
	$(CC) -o $(APP) lexer.cmo parser.cmo syntaxtree.cmo interpreter.cmo main.cmo

run: eval
	./$(APP) < input/memory.cre

test: eval
	./test.sh

clean:
	rm -f parser.mli parser.ml lexer.ml *.cmo *.cmi *~

veryclean: clean
	rm $(APP)

.PHONY: test clean veryclean
