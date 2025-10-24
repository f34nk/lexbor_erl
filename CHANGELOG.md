# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Added
- Worker pool architecture with configurable parallelism
- Multiple independent port workers for true concurrent processing
- Time-based hash distribution for stateless operations
- DocId encoding for deterministic routing of stateful operations
- Worker isolation with independent supervision
- Fault tolerance with automatic worker recovery
- Parallel processing tests (3 tests)
- Fault tolerance tests (2 tests)
- Worker pool coordinator (`lexbor_erl_pool`)
- Individual worker processes (`lexbor_erl_worker`)

### Changed
- Architecture upgraded from single-threaded to worker pool
- Test suite expanded from 24 to 30 tests
- Routing strategy uses time-based hash instead of sequential round-robin
- Build process uses `rebar3 compile && sh c_src/build.sh`

### Fixed
- Corrected routing strategy documentation from "round-robin" to "time-based hash distribution"
- Added inline documentation for worker selection algorithm

## [Unreleased] - single-threaded

### Added
- HTML5-tolerant parsing with Lexbor C library
- CSS selector queries (class, ID, tag, attributes, combinators, pseudo-classes)
- Stateless operations: `parse_serialize/1`, `select_html/2`
- Stateful operations: `parse/1`, `release/1`, `select/2`, `outer_html/2`
- Port-based architecture for BEAM VM safety
- Worker pool with time-based hash distribution
- Document lifecycle management with DocId encoding
- Application and supervisor structure
- Common Test suite with 24 integration tests covering:
  - Lifecycle management
  - Stateless and stateful operations
  - Error handling and edge cases
  - Unicode support
  - Large documents
- Comprehensive EDoc documentation
- CMake-based C build system
- Example programs demonstrating API usage

### Security
- Port isolation prevents C crashes from affecting BEAM VM
- No atom leaks - all user input stays as binaries
