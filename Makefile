X:=$(shell find examples -type f -name "*.erl" -not -name examples -maxdepth 1 -exec basename {} \;)
EXAMPLES:=$(foreach x,$(X),$(x))
NEW_VERSION:=$(shell cat version.txt)

.PHONY: all
all: clean compile test-c test examples

.PHONY: install
install:
	#
	# Install plugins
	#
	rebar3 plugins upgrade --all

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

.PHONY: format
format:
	@ERL_FLAGS="-enable-feature maybe_expr" rebar3 format

.PHONY: format-verify
format-verify:
	@ERL_FLAGS="-enable-feature maybe_expr" rebar3 format --verify

.PHONY: doc
doc:
	#
	# Generate documentation
	#
	rm -rf doc && rebar3 ex_doc
	
.PHONY: clean
clean:
	#
	# Clean
	#
	rm -rf c_src/build _build build .cache priv doc erl_crash.dump examples/*.beam examples/*.dump

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

.PHONY: publish/build
publish/build: clean compile
	#
	# hex build
	#
	rebar3 hex build
	#
	# hex publish --dry-run
	#
	rebar3 hex publish --dry-run
	tree _build/default/lib/lexbor_erl

.PHONY: publish/release
publish/release: _build/default/lib/lexbor_erl
	#
	# hex publish release
	#
	rebar3 hex publish
	
CURRENT_VERSION:=$(shell awk -F'"' '/vsn/ {print $$2}' src/lexbor_erl.app.src)
.PHONY: publish/bump-version
publish/bump-version:
	#
	# bump version $(CURRENT_VERSION) to $(NEW_VERSION)
	#
	sed -i '' 's/{vsn, "$(CURRENT_VERSION)"}/{vsn, "$(NEW_VERSION)"}/g' src/lexbor_erl.app.src
	sed -i '' 's/{lexbor_erl, "$(CURRENT_VERSION)"}/{lexbor_erl, "$(NEW_VERSION)"}/g' README.md
	sed -i '' 's/{lexbor_erl, "$(CURRENT_VERSION)"}/{lexbor_erl, "$(NEW_VERSION)"}/g' demo/rebar.config

.PHONY: publish/reset-version
publish/reset-version:
	git restore README.md demo/rebar.config src/lexbor_erl.app.src 
	
.PHONY: demo
demo:
	#
	# Run demo
	#
	cd demo && make

.PHONY: demo
demo/clean:
	#
	# Clean demo
	#
	cd demo && make clean
