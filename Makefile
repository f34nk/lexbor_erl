X:=$(shell find examples -type f -name "*.erl" -not -name examples -maxdepth 1 -exec basename {} \;)
EXAMPLES:=$(foreach x,$(X),$(x))

.PHONY: all
all: clean compile test-c test examples

.PHONY: compile
compile:
	#
	# Compile
	#
	rebar3 compile && c_src/build.sh

.PHONY: test
test:
	#
	# Run erlang tests
	#
	rebar3 ct

.PHONY: test-c
test-c:
	#
	# Run C unit tests
	#
	cd c_src/build && ctest --verbose --output-on-failure

.PHONY: doc
doc:
	#
	# Generate documentation
	#
	rm -rf doc && rebar3 edoc
	
.PHONY: clean
clean:
	#
	# Clean
	#
	rm -rf c_src/build _build build .cache priv doc erl_crash.dump examples/*.beam examples/*.dump && \
	rebar3 clean

.PHONY: examples
examples: $(EXAMPLES)

.PHONY: %.erl
%.erl:
	#
	# Run $@
	#
	cd examples && \
	erlc -o . $@ && \
	erl -pa . -pa ../_build/default/lib/lexbor_erl/ebin -noshell -eval '$(basename $@):run(), halt().' && \
	rm -f $(basename $@).beam && \
	echo "OK" || echo "FAILED"

.PHONY: docker-test
docker-test: docker/build docker/test docker/clean

.PHONY: docker/build
docker/build:
	#
	# docker build
	#
	docker build -t test -f Dockerfile.test .

.PHONY: docker/test
docker/test:
	#
	# docker test
	#
	docker run --name test \
		-v "$(shell pwd):/workspace" \
		-w /workspace \
		test \
		bash -c "make"

.PHONY: docker/clean
docker/clean:
	#
	# docker clean
	#
	docker rm -f test >/dev/null 2>&1 || true
	docker rmi -f test >/dev/null 2>&1 || true
