#
# lexbor_erl Demo
# Verifies published version from hex.pm
#

.PHONY: all
all: clean deps compile run

.PHONY: deps
deps:
	rebar3 get-deps
	
.PHONY: compile
compile: deps
	rebar3 compile
	
.PHONY: run
run: compile
	@erl -pa _build/default/lib/*/ebin \
		-noshell \
		-eval 'lexbor_demo:main([])' \
		-s init stop

.PHONY: clean
clean:
	rebar3 clean
	rm -rf _build rebar.lock
