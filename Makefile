.PHONY: all compile test doc clean demo

all: compile test

compile: clean
	rebar3 compile && c_src/build.sh

test:
	rebar3 ct

doc:
	rm -rf doc && rebar3 edoc

clean:
	rm -rf c_src/build _build doc erl_crash.dump && rebar3 clean

demo: clean compile
	#
	# Basic demo
	#
	erlc -o . demo1.erl && erl -pa _build/default/lib/lexbor_erl/ebin -noshell -eval 'demo1:run(), halt().'
	rm -f demo1.beam
	#
	# Unicode demo
	#
	erlc -o . demo2.erl && erl -pa _build/default/lib/lexbor_erl/ebin -noshell -eval 'demo2:run(), halt().'
	rm -f demo2.beam
