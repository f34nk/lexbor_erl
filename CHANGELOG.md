# Changelog

## [Unreleased] - Add `prepend_content/3` operation

### Added
- **`prepend_content/3` operation**: Prepend HTML content to all elements matching a CSS selector
  - Inserts content as first child (before existing children)
  - Combines CSS selector matching, HTML parsing, and DOM manipulation in a single atomic operation
  - Returns count of modified elements
  - C implementation for optimal performance
  - Erlang wrapper with comprehensive documentation

### Technical Details
- Uses lexbor's CSS selector engine for matching (same as append_content)
- Parses HTML fragments using `lxb_html_element_inner_html_set()`
- Inserts nodes before first child using `lxb_dom_node_insert_before()`
- Maintains document order when prepending multiple nodes
- Comprehensive error handling
- O(n + m + k×m') complexity where n=DOM nodes, m=HTML length, k=matches, m'=parsed nodes

## [Unreleased] - Add `append_content/3` operation

### Added
- **`append_content/3` operation**: Append HTML content to all elements matching a CSS selector
  - Combines CSS selector matching, HTML parsing, and DOM manipulation in a single atomic operation
  - Returns count of modified elements
  - Erlang wrapper with comprehensive documentation

### Technical Details
- Uses lexbor's CSS selector engine for matching
- Parses HTML fragments using `lxb_html_element_inner_html_set()`
- O(n + m + k×m') complexity where n=DOM nodes, m=HTML length, k=matches, m'=parsed nodes

## [Unreleased] - Demo application

### Added
- **Demo application** in `demo/` directory that verifies the published hex.pm package works correctly
- Makefile for convenient demo execution (`make` in demo directory)
- Comprehensive README in `demo/` explaining why escripts don't work with port-based applications

### Technical Details
- Demo fetches `lexbor_erl` v0.1.0 from hex.pm (not local source)
- Uses `erl -pa` execution instead of escript (required for port-based applications)
- Provides verification that the published package compiles and runs correctly

## [Released] - 0.1.0
- Published to hex.pm: https://hex.pm/packages/lexbor_erl

## [Unreleased] - Improve document serialization to preserve DOCTYPE

### Improved
- **Enhanced `serialize_full_doc` implementation**: Refactored to serialize from document node 
  instead of document element, which now preserves DOCTYPE declarations. This simplifies the code and aligns with all lexbor examples, and produces complete 
  HTML5 documents.

### Added
- DOCTYPE declarations are now preserved during serialization
- Complete HTML5 document structure maintained in round-trip parsing
- Better HTML5 compliance with proper document structure
- New verification test suite to validate serialization behavior

### Changed
- Serialization output now includes DOCTYPE when present in parsed HTML
- Example: Input `<!DOCTYPE html><html>...</html>` now serializes with DOCTYPE preserved
- Matches idiomatic lexbor pattern used in all official examples
- Simpler implementation

### Technical Details
- Changed from `lxb_dom_document_element()` to direct document node serialization

## [Unreleased] - Refactor get_attribute to use convenience API

### Changed
- **Refactored `get_attribute` implementation**: Replaced two-step attribute retrieval 
  (get attribute object, then extract value) with lexbor's convenience function 
  `lxb_dom_element_get_attribute()`. This simplifies the code, improves readability, 
  and aligns with lexbor examples. All 51 tests continue to pass.

### Improved
- Simpler attribute retrieval logic
- Better code readability with clearer intent
- Follows idiomatic lexbor pattern from official examples
- Simpler error handling (single NULL check instead of two checks)

## [Unreleased] - Refactor set_inner_html to use official lexbor API

### Changed
- **Refactored `set_inner_html` implementation**: Replaced manual node importation with lexbor's official `lxb_html_element_inner_html_set()` API improves maintainability, and adds context-aware HTML parsing (e.g., proper handling of innerHTML on `<table>` elements)

### Improved
- `set_inner_html` now uses the standard lexbor approach for innerHTML operations
- Better alignment with lexbor best practices and examples
- Simplified code maintenance
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
- streaming parser integration tests covering:
  - Basic streaming with multiple chunks
  - Splitting in middle of tags and attributes
  - Large document streaming
  - Equivalence with normal parsing
  - Invalid session handling
  - Parallel streaming sessions
- C unit tests for streaming operations covering:
  - Basic begin/end sequence
  - Multiple chunks processing
  - Tag boundary splitting
  - Invalid session handling
  - Session reuse prevention
  - Large document streaming
  - Empty chunk handling
- `chunk_based_streaming_example.erl` with examples
- Session ID encoding with worker affinity for proper routing

## [Unreleased] - DOM Manipulation

### Added
- DOM manipulation API with 11 new functions:
  - Attributes: `get_attribute/3`, `set_attribute/4`, `remove_attribute/3`
  - Text/HTML: `get_text/2`, `set_text/3`, `inner_html/2`, `set_inner_html/3`, `serialize/1`
  - Nodes: `create_element/2`, `append_child/3`, `insert_before/4`, `remove_node/2`
- C unit test suite with 38 tests
- example programs: `attribute_example`, `text_example`, `node_example`, `select_example`, `unicode_example`

## [Unreleased] - Parallelism

### Added
- Worker pool architecture with configurable parallelism
- Multiple independent port workers for true concurrent processing
- Time-based hash distribution for stateless operations
- DocId encoding for deterministic routing of stateful operations
- Worker isolation with independent supervision
- Fault tolerance with automatic worker recovery
- Parallel processing tests
- Fault tolerance tests
- Worker pool coordinator (`lexbor_erl_pool`)
- Individual worker processes (`lexbor_erl_worker`)

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
- Common Test suite
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
