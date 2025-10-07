# lexbor_erl

An Erlang wrapper for the [Lexbor](https://github.com/lexbor/lexbor) HTML parser and DOM library via a port-based architecture.

## Overview

`lexbor_erl` provides safe, fast HTML parsing, CSS selector querying, and DOM manipulation for Erlang applications. It wraps the high-performance Lexbor C library using a port (separate OS process) for isolation and safety.

## Features

- **HTML5-tolerant parsing** with automatic error recovery
- **CSS selector queries** (class, ID, tag, attributes, combinators, pseudo-classes)
- **Stateless operations** for quick one-off tasks
- **Stateful document management** for complex workflows
- **Safe for the BEAM** - crashes in native code don't bring down the VM
- **No atom leaks** - all user input stays as binaries

## Prerequisites

- Erlang/OTP (tested with OTP 24+)
- CMake 3.10+
- [Lexbor library](https://github.com/lexbor/lexbor) installed on your system

### Installing Lexbor

On macOS with Homebrew:
```bash
brew install lexbor
```

On Ubuntu/Debian:
```bash
sudo apt-get install liblexbor-dev
```

Or build from source:
```bash
git clone https://github.com/lexbor/lexbor.git
cd lexbor
mkdir build && cd build
cmake ..
make
sudo make install
```

## Building

```bash
rebar3 compile
sh c_src/build.sh
```

## Quick Start

```erlang
1> lexbor_erl:start().
ok

%% Stateless: parse and serialize
2> {ok, Html} = lexbor_erl:parse_serialize(<<"<div>Hello<span>World">>).
{ok,<<"<html><head></head><body><div>Hello<span>World</span></div></body></html>">>}

%% Stateless: select elements
3> {ok, List} = lexbor_erl:select_html(
     <<"<ul><li class=a>A</li><li class=b>B</li></ul>">>, 
     <<"li.b">>).
{ok,[<<"<li class=\"b\">B</li>">>]}

%% Stateful: parse document
4> {ok, Doc} = lexbor_erl:parse(
     <<"<div id=app><ul><li class=a>A</li><li class=b>B</li></ul></div>">>).
{ok,1}

%% Select nodes
5> {ok, Nodes} = lexbor_erl:select(Doc, <<"#app li">>).
{ok,[{node,140735108544752},{node,140735108544896}]}

%% Get node HTML
6> [lexbor_erl:outer_html(Doc, N) || N <- Nodes].
[{ok,<<"<li class=\"a\">A</li>">>},{ok,<<"<li class=\"b\">B</li>">>}]

%% Release document
7> ok = lexbor_erl:release(Doc).
ok

8> lexbor_erl:stop().
ok
```

## API

### Lifecycle

```erlang
-spec start() -> ok | {error, term()}.
-spec stop() -> ok.
-spec alive() -> boolean().
```

### Stateless Operations

```erlang
%% Parse HTML and serialize back (normalized)
-spec parse_serialize(binary() | iolist()) -> {ok, binary()} | {error, term()}.

%% Parse, select with CSS, return outerHTML of matches
-spec select_html(binary() | iolist(), binary()) -> {ok, [binary()]} | {error, term()}.
```

### Stateful Operations

```erlang
%% Parse HTML into a document handle
-spec parse(binary() | iolist()) -> {ok, doc_id()} | {error, term()}.

%% Free document resources
-spec release(doc_id()) -> ok | {error, term()}.

%% Select nodes using CSS selector
-spec select(doc_id(), binary()) -> {ok, [node_ref()]} | {error, term()}.

%% Get outerHTML of a node
-spec outer_html(doc_id(), node_ref()) -> {ok, binary()} | {error, term()}.
```

## Architecture

- **Erlang façade**: Public API in `lexbor_erl` module
- **Port manager**: `lexbor_erl_port` gen_server manages the native process
- **Native port**: C program using Lexbor, communicates via stdin/stdout
- **Protocol**: Length-prefixed binary frames (`{packet, 4}`)
- **Safety**: Port crashes don't affect the BEAM VM

## Configuration

In your `sys.config`:

```erlang
{lexbor_erl, [
  {port_cmd, "priv/lexbor_port"},
  {op_timeout_ms, 3000}
]}.
```

## Thread Safety

The current implementation is thread-safe by virtue of being single-threaded:
- All calls are serialized through a single gen_server
- The C port handles one request at a time
- No shared mutable state between concurrent operations

## License

MIT License (or specify your license)

## Credits

Built on top of the [Lexbor](https://github.com/lexbor/lexbor) HTML parser library.
