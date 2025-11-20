# Changelog

## [Released] - 0.1.0
- Published to hex.pm: https://hex.pm/packages/lexbor_erl

## [Unreleased] - Improve document serialization to preserve DOCTYPE

### Improved
- **Enhanced `serialize_full_doc` implementation**: Refactored to serialize from document node 
  instead of document element, which now preserves DOCTYPE declarations. This simplifies the code 
  (5 lines → 3 lines, 40% reduction), aligns with all lexbor examples, and produces complete 
  HTML5 documents. All 55 tests pass (51 original + 4 new verification tests).

### Added
- DOCTYPE declarations are now preserved during serialization
- Complete HTML5 document structure maintained in round-trip parsing
- Better HTML5 compliance with proper document structure
- New verification test suite to validate serialization behavior

### Changed
- Serialization output now includes DOCTYPE when present in parsed HTML
- Example: Input `<!DOCTYPE html><html>...</html>` now serializes with DOCTYPE preserved
- Matches idiomatic lexbor pattern used in all official examples
- Simpler implementation (3 function calls → 1, 67% reduction)

### Technical Details
- Changed from `lxb_dom_document_element()` to direct document node serialization
- Verified safe through comprehensive testing (4 new tests + 51 existing tests)
- This is an improvement, not a breaking change (adds data, doesn't remove)

## [Unreleased] - Refactor get_attribute to use convenience API

### Changed
- **Refactored `get_attribute` implementation**: Replaced two-step attribute retrieval 
  (get attribute object, then extract value) with lexbor's convenience function 
  `lxb_dom_element_get_attribute()`. This simplifies the code, improves readability, 
  and aligns with lexbor examples. All 51 tests continue to pass.

### Improved
- Simpler attribute retrieval logic (3 API calls → 1 API call, 67% reduction)
- Better code readability with clearer intent
- Follows idiomatic lexbor pattern from official examples
- Simpler error handling (single NULL check instead of two checks)

## [Unreleased] - Refactor set_inner_html to use official lexbor API

### Changed
- **Refactored `set_inner_html` implementation**: Replaced manual node importation with lexbor's official 
  `lxb_html_element_inner_html_set()` API. This simplifies the code from 89 to 63 lines (29% reduction), 
  improves maintainability, and adds context-aware HTML parsing (e.g., proper handling of innerHTML on 
  `<table>` elements). All 51 tests continue to pass.

### Improved
- `set_inner_html` now uses the standard lexbor approach for innerHTML operations
- Better alignment with lexbor best practices and examples
- Simplified code maintenance (80% reduction in core logic lines)
- Context-aware parsing follows HTML5 specification more accurately

## [Unreleased] - Bug in DOM maniplation

### Fixed
- Refactored `set_inner_html/3` to use `lxb_dom_document_import_node` API to properly copy node data to target document's memory pool, preventing use-after-free memory leak when temporary document is destroyed 

## [Unreleased] - Chunk Based Streaming Parser

### Added
- Streaming HTML parser for incremental document processing
- Three-phase streaming API:
  - `parse_stream_begin/0` - Initialize parse session
  - `parse_stream_chunk/2` - Feed HTML chunks incrementally
  - `parse_stream_end/1` - Finalize and get document
- Parse session registry in C port with independent session tracking
- Support for arbitrary chunk boundaries (can split mid-tag, mid-attribute)
- 7 streaming parser integration tests covering:
  - Basic streaming with multiple chunks
  - Splitting in middle of tags and attributes
  - Large document streaming (1000+ elements)
  - Equivalence with normal parsing
  - Invalid session handling
  - Parallel streaming sessions
- 8 C unit tests for streaming operations covering:
  - Basic begin/end sequence
  - Multiple chunks processing
  - Tag boundary splitting
  - Invalid session handling
  - Session reuse prevention
  - Large document streaming (50 chunks)
  - Empty chunk handling
- `chunk_based_streaming_example.erl` with 5 comprehensive examples
- Session ID encoding with worker affinity for proper routing

### Changed
- Test suite expanded to 51 Erlang tests (44 core + 7 streaming)
- C unit tests expanded to 46 tests (38 core + 8 streaming)
- Worker pool routing enhanced to handle invalid DocIds/SessionIds gracefully
- C port supports 3 additional streaming operations

### Benefits
- Memory-efficient parsing of very large documents
- Suitable for network streaming scenarios
- Early processing before full document arrival
- Progress monitoring during parse

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
