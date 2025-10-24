all: compile test

compile: clean
	rebar3 compile && c_src/build.sh

test:
	rebar3 ct

doc:
	rm -rf doc && rebar3 edoc
	
clean:
	rm -rf c_src/build _build erl_crash.dump && rebar3 clean

shell: clean compile
	#
	# Debug test
	#
	erlc -o . debug_test.erl && erl -pa _build/default/lib/lexbor_erl/ebin -noshell -eval 'debug_test:run(), halt().'
	rm -f debug_test.beam
	#
	# Unicode test
	#
	erlc -o . test_unicode.erl && erl -pa _build/default/lib/lexbor_erl/ebin -noshell -eval 'test_unicode:run(), halt().'
	rm -f test_unicode.beam

.PHONY: all test clean
