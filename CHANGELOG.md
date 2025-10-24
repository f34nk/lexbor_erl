# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased] - DOM Manipulation

### Added
- DOM manipulation API with 11 new functions:
  - Attributes: `get_attribute/3`, `set_attribute/4`, `remove_attribute/3`
  - Text/HTML: `get_text/2`, `set_text/3`, `inner_html/2`, `set_inner_html/3`, `serialize/1`
  - Nodes: `create_element/2`, `append_child/3`, `insert_before/4`, `remove_node/2`
- C unit test suite with 38 tests
- 14 DOM manipulation integration tests
- 5 example programs: `attribute_example`, `text_example`, `node_example`, `select_example`, `unicode_example`

### Changed
- Test suite expanded to 44 tests (30 core + 14 DOM)
- C port supports 12 additional DOM operations

## [Unreleased] - Parallelism

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

### Fixed
- Corrected routing strategy documentation from "round-robin" to "time-based hash distribution"

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
