# lexbor_erl

[![CI](https://github.com/f34nk/lexbor_erl/actions/workflows/ci.yml/badge.svg)](https://github.com/f34nk/lexbor_erl/actions/workflows/ci.yml)
[![lexbor_erl version](https://img.shields.io/hexpm/v/lexbor_erl.svg)](https://hex.pm/packages/lexbor_erl)
[![Hex.pm](https://img.shields.io/hexpm/dt/lexbor_erl.svg)](https://hex.pm/packages/lexbor_erl)

An Erlang wrapper for the [Lexbor](https://github.com/lexbor/lexbor) HTML parser and DOM library via a port-based architecture.

## Overview

`lexbor_erl` provides safe, fast HTML parsing, CSS selector querying, DOM manipulation, and streaming parser capabilities for Erlang applications. It wraps the high-performance Lexbor C library using a port-based worker pool architecture for isolation, safety, and parallel processing.

## Features

- **HTML5-tolerant parsing** with automatic error recovery
- **CSS selector queries** (class, ID, tag, attributes, combinators, pseudo-classes)
- **DOM manipulation** - modify attributes, text content, and tree structure
- **Streaming parser** - parse large HTML documents incrementally
- **Stateless operations** for quick one-off tasks
- **Stateful document management** for complex workflows
- **Parallel processing** - worker pool architecture for concurrent operations
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
make
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

%% DOM manipulation: modify attributes
7> {ok, [Li]} = lexbor_erl:select(Doc, <<"li.a">>).
{ok,[{node,140735108544752}]}

8> lexbor_erl:set_attribute(Doc, Li, <<"class">>, <<"modified">>).
ok

9> lexbor_erl:get_attribute(Doc, Li, <<"class">>).
{ok,<<"modified">>}

%% DOM manipulation: modify text content
10> lexbor_erl:set_text(Doc, Li, <<"New Text">>).
ok

11> lexbor_erl:get_text(Doc, Li).
{ok,<<"New Text">>}

%% Content manipulation: append HTML to matching elements
12> {ok, NumModified} = lexbor_erl:append_content(Doc, <<"ul">>, <<"<li>New Item</li>">>).
{ok,1}

13> {ok, Html} = lexbor_erl:serialize(Doc).
{ok,<<"<!DOCTYPE html><html><head></head><body><div id=\"app\"><ul><li class=\"modified\">New Text</li><li class=\"b\">B</li><li>New Item</li></ul></div></body></html>">>}

%% Streaming parser: parse incrementally
14> {ok, Session} = lexbor_erl:parse_stream_begin().
{ok,72057594037927937}

15> ok = lexbor_erl:parse_stream_chunk(Session, <<"<div><p>He">>).
ok

16> ok = lexbor_erl:parse_stream_chunk(Session, <<"llo</p></div>">>).
ok

17> {ok, StreamDoc} = lexbor_erl:parse_stream_end(Session).
{ok,72057594037927938}

%% Release documents
18> ok = lexbor_erl:release(Doc).
ok

19> ok = lexbor_erl:release(StreamDoc).
ok

20> lexbor_erl:stop().
ok
```

Also check out [examples/](https://github.com/f34nk/lexbor_erl/tree/main/examples) directory.

## How to use it in your application?

Add to your `rebar.config`:

```erlang
{deps, [
    {lexbor_erl, "0.1.0"}
]}.
```

Then run:

```shell
rebar3 get-deps
rebar3 compile
```

**Note**: lexbor_erl is a port-based application and cannot be packaged as an escript. 
It must be used as a library dependency with access to the compiled C port executable.

See the [demo/](https://github.com/f34nk/lexbor_erl/tree/main/demo) directory for complete working application.

## Additional configuration

In your `sys.config`:

```erlang
{lexbor_erl, [
  {port_cmd, "priv/lexbor_port"},
  {op_timeout_ms, 3000}
]}.
```

## Parallelism and Concurrency

`lexbor_erl` uses a **worker pool architecture** to enable true parallel processing of HTML operations:

### Architecture

- **Multiple port workers**: Configurable pool of independent C port processes
- **Smart routing**: 
  - Stateless operations (e.g., `parse_serialize/1`, `select_html/2`) use time-based hash distribution for load balancing
  - Stateful operations route by `DocId` to ensure the same worker handles all operations for a given document
- **Isolation**: Each worker process is independent with its own document registry
- **Individual supervision**: Each worker is supervised independently - if one crashes, only that worker restarts
- **Fault tolerance**: Worker crashes don't affect other workers or the BEAM VM; documents on crashed worker are lost but other workers continue serving

### Configuration

Set the pool size in your `sys.config`:

```erlang
{lexbor_erl, [
  {pool_size, 8},              % Number of parallel workers (default: scheduler count)
  {op_timeout_ms, 3000}        % Timeout per operation
]}.
```

Or via environment variable when starting the application:

```erlang
application:set_env(lexbor_erl, pool_size, 8).
```

### Thread Safety and Fault Tolerance

- **Safe by design**: Each worker is single-threaded, processing one request at a time
- **No shared state**: Documents are isolated to their respective workers
- **Concurrent operations**: Multiple workers can process different documents simultaneously
- **Deterministic routing**: A document always routes to the same worker via the worker ID encoded in the `DocId`
- **Individual worker restart**: If a worker crashes, only that worker is restarted by the supervisor
- **Limited blast radius**: Worker crashes only affect documents on that specific worker
- **Automatic recovery**: Crashed workers are automatically restarted and can accept new documents

### Performance Characteristics

- **Parallelism**: Leverages all CPU cores for concurrent HTML parsing and manipulation
- **No contention**: No locks or shared mutable state between workers
- **Linear scaling**: Performance scales linearly with the number of workers (up to CPU core count)
- **Stateless optimization**: Stateless operations (`parse_serialize`, `select_html`) can use any available worker

## License

LGPL-2.1-or-later

## Credits

Built on top of the [Lexbor](https://github.com/lexbor/lexbor) HTML parser library.
