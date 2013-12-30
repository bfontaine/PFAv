# PFAv 2013 project
#

SRC=src
TESTS_DIR=tests
TESTS_EXE=t

TESTS=\
	$(TESTS_DIR)/stream_tests.ml \
	$(TESTS_DIR)/prob_tests.ml

PART1=sujet_search
PART2=sujet_prob

OCAMLFIND=ocamlfind

.PHONY: test clean cleanall

$(SRC)/%.cmi: $(SRC)/%.mli
	ocamlc -c $<

$(SRC)/%.cmo: $(SRC)/%.ml
	ocamlc -c $<

test: $(SRC)/$(PART1).cmi $(SRC)/$(PART2).cmo
	@$(OCAMLFIND) ocamlc -I $(SRC) -o $(TESTS_EXE) -package oUnit -linkpkg -g \
		$(SRC)/$(PART1).ml $(SRC)/$(PART2).ml $(TESTS)
	@./$(TESTS_EXE)

clean:
	find . -name *~ -delete
	rm -f $(TESTS_EXE)

cleanall: clean
	find . -name *.cmo -delete
	find . -name *.cmi -delete
