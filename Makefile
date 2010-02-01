
YACC := ocamlyacc
LEX  := ocamllex
CC   := ocamlc
DEP  := ocamldep

APP  := interpreter

MLS  := syntaxtree.ml parser.ml lexer.ml exception.ml types.ml common.ml \
        env.ml store.ml heap.ml interpreter.ml main.ml
CMOS := $(MLS:.ml=.cmo)

eval: $(CMOS)
	$(CC) -o $(APP) $(CMOS)

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

count:
	cat *.ml | wc -l

dep:
	$(DEP) $(MLS) > .depend

include .depend
.PHONY: test clean veryclean
