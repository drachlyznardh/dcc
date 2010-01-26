
YACC := ocamlyacc
LEX  := ocamllex
CC   := ocamlc

APP  := interpreter

MLS = $(wildcard *.ml)
CMOS = $(MLS:.ml=.cmo)

eval: syntaxtree.cmo parser.ml lexer.cmo parser.cmo $(CMOS)
	$(CC) -o $(APP) mem.cmo lexer.cmo parser.cmo $(CMOS)

interpreter.cmo: mem.cmo 

lexer.cmo: lexer.mll
	$(LEX) lexer.mll
	$(CC) -c lexer.ml

%.cmo: %.ml
	$(CC) -c $<

parser.ml: syntaxtree.cmo parser.mly
	$(YACC) parser.mly
	$(CC) -c parser.mli

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
