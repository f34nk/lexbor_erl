# lexbor_erl Demo Application

This demo application verifies that **lexbor_erl** from [hex.pm](https://hex.pm/packages/lexbor_erl) works correctly.

## Prerequisites

- Erlang/OTP 24 or later
- rebar3
- Make (optional, for convenience)

## Quick Start

```bash
# Run everything (fetch deps, compile, run demo)
make
```

## What the Demo Tests

### 1. Basic HTML Parsing
- Parse a simple HTML document
- Create and release document handles

### 2. CSS Selectors
- Select by ID (`#first`)
- Select by class (`.highlight`)
- Select by tag name (`p`)
- Complex selectors (`div.container > p`)

### 3. Attribute Operations
- Get attributes (`get_attribute`)
- Set attributes (`set_attribute`)
- Remove attributes (`remove_attribute`)

### 4. Text Content
- Get text content recursively (`get_text`)
- Set text content (`set_text`)

### 5. DOM Manipulation
- Create elements (`create_element`)
- Append children (`append_child`)
- Get outer HTML (`get_outer_html`)

### 6. Serialization
- Serialize document to HTML (`serialize`)
- Round-trip parsing (parse → serialize → parse)

### 7. Streaming Parser
- Start streaming parse (`parse_start`)
- Parse chunks (`parse_chunk`)
- Finalize parse (`parse_end`)

## Why Not Escript?

This demo does **not** use escript because lexbor_erl is a port-based application (uses C code).
Port executables cannot be properly embedded in escripts due to how Erlang's code loading works.
The demo runs using standard `erl -pa` to ensure the C port can be found and loaded correctly.

## Verifying hex.pm Installation

You can verify that lexbor_erl was indeed fetched from hex.pm:

```bash
# Check the dependency
cat _build/default/lib/lexbor_erl/ebin/lexbor_erl.app
```
