
YACC := ocamlyacc
LEX  := ocamllex
CC   := ocamlc

APP  := interpreter

MLS = syntaxtree.ml parser.ml lexer.ml mem.ml interpreter.ml main.ml
CMOS = $(MLS:.ml=.cmo)

eval: $(CMOS)
	$(CC) -o $(APP) $(CMOS)

mem.cmo: mem.ml lexer.cmo parser.cmo
interpreter.cmo: mem.ml mem.cmo
main.cmo: mem.ml mem.cmo

lexer.ml: lexer.mll
	$(LEX) lexer.mll

%.cmo: %.ml
	$(CC) -c $<

parser.ml: parser.mly
	$(YACC) parser.mly
	$(CC) -c parser.mli

run: 
	./$(APP) < progs/win/darray-access.cre

runall:
	for i in $$(find . -name '*.cre'); do echo "\nNow executing $$i"; ./$(APP) < $$i; done

test: 
	./.test.sh

clean:
	rm -f parser.mli parser.ml lexer.ml *.cmo *.cmi *~

veryclean: clean
	rm -f $(APP)

.PHONY: test clean veryclean
