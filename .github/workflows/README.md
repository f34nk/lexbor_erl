# GitHub Actions CI/CD

## Overview

This directory contains GitHub Actions workflows for continuous integration of the `lexbor_erl` project, which provides Erlang bindings for the Lexbor HTML parser library.

## Workflows

### CI Workflow (`.github/workflows/ci.yml`)

The main CI workflow runs on every push and pull request to feature branches and main.

#### Matrix Build Strategy

The workflow tests against multiple versions and platforms to ensure compatibility:
- **Operating Systems:** Ubuntu (latest), macOS (latest)
- **Erlang/OTP versions:** 26.0, 27.0

This creates 4 build combinations (2 OS × 2 OTP versions).

#### Build Steps

1. **Checkout code** - Clones the repository with full history
2. **Set up Erlang/OTP** - Installs specified Erlang/OTP version and rebar3 3.22
3. **Install dependencies** - Installs system packages:
   - Ubuntu: `cmake`, `build-essential`, `git`
   - macOS: `cmake` (via Homebrew)
4. **Install lexbor library** - Clones and builds Lexbor v2.3.0 from source:
   - Builds with CMake in Release mode
   - Installs system-wide
   - Runs `ldconfig` on Linux to update library cache
5. **Compile** - Runs `make compile` which:
   - Compiles Erlang code (`rebar3 compile`)
   - Builds C port (`c_src/build.sh`)
6. **Run C unit tests** - Executes `make test-c` (38+ C unit tests via CTest)
7. **Run Erlang tests** - Executes `make test` (51 integration tests via Common Test)
8. **Run examples** - Executes `make examples` (6 example programs)
9. **Generate documentation** - Creates EDoc documentation

#### Triggers

- **Push:** Runs on commits to `main`, `parallelism`, `dom-manipulation`, `chunk-based-streaming-parser`, and `ci` branches
- **Pull Request:** Runs on PRs targeting `main` and feature branches
- **Manual:** Can be triggered manually via `workflow_dispatch`

#### Artifacts

On build failure, the following artifacts are uploaded for debugging:
- `**/rebar3.crashdump` - Rebar3 crash dumps
- `**/_build/test/logs/` - Common Test execution logs
- `**/erl_crash.dump` - Erlang VM crash dumps
- `c_src/build/Testing/` - CTest output logs

Artifacts are retained for 7 days.

## Running Locally

To run the same steps locally:

```bash
# Full CI pipeline
make

# Or run individual steps:

# 1. Compile Erlang and C code
make compile

# 2. Run C unit tests (38+ tests)
make test-c

# 3. Run Erlang integration tests (51 tests)
make test

# 4. Run example programs (6 examples)
make examples

# 5. Generate documentation
make doc

# Clean build artifacts
make clean
```

## Requirements

### System Dependencies

The CI environment requires:
- **Erlang/OTP:** 26.0 or 27.0
- **rebar3:** 3.22 or later
- **CMake:** 3.15 or later
- **C Compiler:** GCC (Linux) or Clang (macOS)
- **Lexbor library:** v2.3.0 (automatically installed in CI)

### Local Development Setup

#### Ubuntu/Debian

```bash
# Install Erlang/OTP
sudo apt-get update
sudo apt-get install -y erlang-dev erlang-parsetools

# Install rebar3
wget https://s3.amazonaws.com/rebar3/rebar3
chmod +x rebar3
sudo mv rebar3 /usr/local/bin/

# Install build tools
sudo apt-get install -y cmake build-essential git

# Install lexbor
git clone --depth 1 --branch v2.3.0 https://github.com/lexbor/lexbor.git
cd lexbor
cmake -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build
sudo cmake --install build
sudo ldconfig
```

#### macOS

```bash
# Install Erlang/OTP via Homebrew
brew install erlang rebar3

# Install CMake
brew install cmake

# Install lexbor
git clone --depth 1 --branch v2.3.0 https://github.com/lexbor/lexbor.git
cd lexbor
cmake -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build
sudo cmake --install build

# Set library path for runtime (add to ~/.zshrc or ~/.bash_profile)
export DYLD_LIBRARY_PATH=/usr/local/lib:$DYLD_LIBRARY_PATH
```

## Test Coverage

The CI runs comprehensive tests across multiple dimensions:

### C Unit Tests (`make test-c`)
- 38+ unit tests covering C port implementation
- Tests for parsing, querying, DOM manipulation, and streaming
- Executed via CTest with verbose output

### Erlang Integration Tests (`make test`)
- 51 integration tests organized in 6 test suites:
  - **Basic Suite** (9 tests): Lifecycle and stateless operations
  - **Stateful Suite** (15 tests): Document operations, errors, edge cases
  - **DOM Suite** (14 tests): Attribute, text, HTML, and node manipulation
  - **Stream Suite** (7 tests): Chunk-based streaming parser
  - **Parallel Suite** (4 tests): Concurrent operations and worker isolation
  - **Fault Suite** (2 tests): Worker crash recovery and isolation

### Example Programs (`make examples`)
- 6 runnable examples demonstrating API usage
- Validates real-world usage patterns

## Debugging Failed Builds

### Local Reproduction

1. Clone the repository
2. Install dependencies (see Requirements above)
3. Run the failing step:
   ```bash
   make compile  # If compilation fails
   make test-c   # If C tests fail
   make test     # If Erlang tests fail
   ```

### CI Debugging

1. Check the GitHub Actions tab in the repository
2. Click on the failed workflow run
3. Expand the failed step to see detailed logs
4. Download artifacts if available:
   - Build logs and crash dumps are automatically uploaded on failure
   - Available for 7 days after the run
5. Look for common issues:
   - **Lexbor installation failure:** Check if v2.3.0 tag exists
   - **CMake errors:** Verify CMake version ≥ 3.15
   - **Linker errors:** Ensure lexbor libraries are in system path
   - **macOS dyld errors:** Set `DYLD_LIBRARY_PATH=/usr/local/lib`
   - **Test failures:** Check test logs in artifacts

## Platform-Specific Notes

### Ubuntu (Linux)
- Uses system's default GCC compiler
- Requires `ldconfig` after lexbor installation
- Library path: `/usr/local/lib`

### macOS
- Uses Clang compiler
- No `ldconfig` equivalent needed
- Library path: `/usr/local/lib` (on Intel) or `/opt/homebrew/lib` (on Apple Silicon)
- Requires `DYLD_LIBRARY_PATH=/usr/local/lib` for runtime library loading
- CI sets this environment variable automatically for all build/test steps

## Future Enhancements

Potential improvements to the CI pipeline:

- **Code Coverage:** Add `rebar3 cover` and upload to Codecov
- **Static Analysis:** Add Dialyzer for type checking
- **Performance Benchmarks:** Add benchmark suite with historical tracking
- **Docker Images:** Cache pre-built lexbor in Docker images
- **Release Automation:** Publish releases to Hex.pm
- **Windows Support:** Add Windows runner with MSVC
- **Caching:** Cache lexbor builds between runs
- **Nightly Builds:** Test against Erlang/OTP development versions
- **Documentation Deployment:** Deploy EDoc to GitHub Pages
- **Code Quality:** Add Elvis linter for style checking

## Makefile Targets Reference

| Target | Description |
|--------|-------------|
| `make` | Run full CI pipeline (clean, compile, test-c, test, examples) |
| `make compile` | Compile Erlang code and build C port |
| `make test` | Run all 51 Erlang integration tests |
| `make test-c` | Run all 38+ C unit tests |
| `make examples` | Execute all 6 example programs |
| `make doc` | Generate EDoc documentation |
| `make clean` | Remove all build artifacts |

## Contributing

When adding new features:
1. Add C unit tests to `c_src/port_main_test.c`
2. Add Erlang integration tests to appropriate `test/*_SUITE.erl` file
3. Update CHANGELOG.md
4. Ensure `make` passes locally before pushing
5. CI will automatically run on your pull request
