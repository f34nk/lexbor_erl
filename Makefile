.PHONY: all compile test doc clean examples

all: compile test examples

compile: clean
	#
	# Compile
	#
	rebar3 compile && c_src/build.sh

test:
	#
	# Run tests
	#
	rebar3 ct

doc:
	#
	# Generate documentation
	#
	rm -rf doc && rebar3 edoc
	
clean:
	rm -rf c_src/build _build doc erl_crash.dump && rebar3 clean

examples: select_example.erl unicode_example.erl

%.erl:
	#
	# Run $@
	#
	cd examples && \
	erlc -o . $@ && erl -pa . -pa ../_build/default/lib/lexbor_erl/ebin -noshell -eval '$(basename $@):run(), halt().' && \
	rm -f $(basename $@).beam
