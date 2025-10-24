.PHONY: all compile test test-c doc clean examples

all: compile test-c test examples clean

compile:
	#
	# Compile
	#
	rebar3 compile && c_src/build.sh

test:
	#
	# Run erlang tests
	#
	rebar3 ct

test-c:
	#
	# Run C unit tests
	#
	cd c_src/build && ctest --verbose --output-on-failure

doc:
	#
	# Generate documentation
	#
	rm -rf doc && rebar3 edoc
	
clean:
	#
	# Clean
	#
	rm -rf c_src/build _build priv doc erl_crash.dump && rebar3 clean

examples: select_example.erl unicode_example.erl

%.erl:
	#
	# Run $@
	#
	cd examples && \
	erlc -o . $@ && \
	erl -pa . -pa ../_build/default/lib/lexbor_erl/ebin -noshell -eval '$(basename $@):run(), halt().' && \
	rm -f $(basename $@).beam
