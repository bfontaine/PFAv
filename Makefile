# PFAv 2013 project
#

SRC=src
TESTS=tests
TESTS_EXE=t

PART1=sujet_search

OCAMLFIND=ocamlfind

.PHONY: test

$(SRC)/%.cmi: $(SRC)/%.mli
	ocamlc -c $<

$(SRC)/%.cmo: $(SRC)/%.ml
	ocamlc -c $<

test: $(SRC)/$(PART1).cmi
	@$(OCAMLFIND) ocamlc -I $(SRC) -o $(TESTS_EXE) -package oUnit -linkpkg -g \
		$(SRC)/$(PART1).ml $(TESTS)/stream_tests.ml
	@./$(TESTS_EXE)

clean:
	rm -f $(TESTS_EXE)
