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
OCAMLC=ocamlc -g

.PHONY: test autotest clean cleanall

$(SRC)/%.cmi: $(SRC)/%.mli
	$(OCAMLC) -c $<

$(SRC)/%.cmo: $(SRC)/%.ml
	$(OCAMLC) -c $<

test: $(SRC)/$(PART1).cmi $(SRC)/$(PART2).cmo
	@$(OCAMLFIND) $(OCAMLC) -I $(SRC) -o $(TESTS_EXE) -package oUnit -linkpkg -g \
		$(SRC)/$(PART1).ml $(SRC)/$(PART2).ml $(TESTS)
	@./$(TESTS_EXE)

autotest:
	@# 'gem install kicker'
	@# use 'gtimeout' on osx, 'timeout' on Linux (from coreutils)
	kicker -ce "gtimeout 4 make test" src

clean:
	find . -name *~ -delete
	rm -f $(TESTS_EXE)

cleanall: clean
	find . -name *.cmo -delete
	find . -name *.cmi -delete
