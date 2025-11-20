-module(chunk_based_streaming_example).
-export([run/0]).

%% @doc Comprehensive example of streaming HTML parsing with lexbor_erl
%%
%% This example demonstrates:
%% - Basic streaming in multiple chunks
%% - Splitting HTML at arbitrary boundaries
%% - Streaming large documents
%% - Using streaming with queries and DOM manipulation
%% - Simulating network streaming

run() ->
    io:format("~n=== Streaming Parser Examples ===~n~n"),
    
    ok = lexbor_erl:start(),
    
    example_basic_streaming(),
    example_split_in_tag(),
    example_large_document(),
    example_with_queries(),
    example_network_simulation(),
    
    lexbor_erl:stop(),
    
    io:format("~n=== All examples completed successfully ===~n").

%% Example 1: Basic streaming in multiple chunks
example_basic_streaming() ->
    io:format("1. Basic Streaming~n"),
    io:format("   Parsing HTML in 3 chunks~n"),
    
    {ok, Session} = lexbor_erl:parse_stream_begin(),
    
    ok = lexbor_erl:parse_stream_chunk(Session, <<"<html><body>">>),
    ok = lexbor_erl:parse_stream_chunk(Session, <<"<h1>Hello, Streaming!</h1>">>),
    ok = lexbor_erl:parse_stream_chunk(Session, <<"</body></html>">>),
    
    {ok, Doc} = lexbor_erl:parse_stream_end(Session),
    
    {ok, [H1]} = lexbor_erl:select(Doc, <<"h1">>),
    {ok, Text} = lexbor_erl:get_text(Doc, H1),
    
    io:format("   Result: ~s~n", [Text]),
    
    ok = lexbor_erl:release(Doc),
    io:format("   ✓ Success~n~n").

%% Example 2: Splitting HTML at arbitrary boundaries
%% This demonstrates that chunks can split in the middle of tags
example_split_in_tag() ->
    io:format("2. Splitting in Middle of Tag~n"),
    io:format("   Chunks split within attribute values~n"),
    
    {ok, Session} = lexbor_erl:parse_stream_begin(),
    
    %% Split "<div class='container' id='main'>" across chunks
    ok = lexbor_erl:parse_stream_chunk(Session, <<"<html><body><div cla">>),
    ok = lexbor_erl:parse_stream_chunk(Session, <<"ss='contai">>),
    ok = lexbor_erl:parse_stream_chunk(Session, <<"ner' id='">>),
    ok = lexbor_erl:parse_stream_chunk(Session, <<"main'>Content">>),
    ok = lexbor_erl:parse_stream_chunk(Session, <<"</div></body></html>">>),
    
    {ok, Doc} = lexbor_erl:parse_stream_end(Session),
    
    {ok, [Div]} = lexbor_erl:select(Doc, <<"div">>),
    {ok, Class} = lexbor_erl:get_attribute(Doc, Div, <<"class">>),
    {ok, Id} = lexbor_erl:get_attribute(Doc, Div, <<"id">>),
    
    io:format("   Class: ~s, ID: ~s~n", [Class, Id]),
    
    ok = lexbor_erl:release(Doc),
    io:format("   ✓ Success~n~n").

%% Example 3: Streaming a large document
example_large_document() ->
    io:format("3. Large Document Streaming~n"),
    io:format("   Building 1000 list items incrementally~n"),
    
    {ok, Session} = lexbor_erl:parse_stream_begin(),
    
    ok = lexbor_erl:parse_stream_chunk(Session, <<"<html><body><ul>">>),
    
    %% Stream 1000 items, one at a time
    lists:foreach(fun(N) ->
        Item = iolist_to_binary(io_lib:format("<li>Item ~p</li>", [N])),
        ok = lexbor_erl:parse_stream_chunk(Session, Item)
    end, lists:seq(1, 1000)),
    
    ok = lexbor_erl:parse_stream_chunk(Session, <<"</ul></body></html>">>),
    
    {ok, Doc} = lexbor_erl:parse_stream_end(Session),
    
    {ok, Items} = lexbor_erl:select(Doc, <<"li">>),
    Count = length(Items),
    
    io:format("   Parsed ~p list items~n", [Count]),
    
    ok = lexbor_erl:release(Doc),
    io:format("   ✓ Success~n~n").

%% Example 4: Using streaming with queries and DOM manipulation
example_with_queries() ->
    io:format("4. Streaming with Queries and DOM Manipulation~n"),
    io:format("   Building document incrementally and modifying it~n"),
    
    {ok, Session} = lexbor_erl:parse_stream_begin(),
    
    ok = lexbor_erl:parse_stream_chunk(Session, <<"<html><body>">>),
    ok = lexbor_erl:parse_stream_chunk(Session, <<"<div id='header'><h1>Title</h1></div>">>),
    ok = lexbor_erl:parse_stream_chunk(Session, <<"<div id='content'><p>Original content</p></div>">>),
    ok = lexbor_erl:parse_stream_chunk(Session, <<"</body></html>">>),
    
    {ok, Doc} = lexbor_erl:parse_stream_end(Session),
    
    %% Query elements
    {ok, [ContentDiv]} = lexbor_erl:select(Doc, <<"#content">>),
    {ok, [Para]} = lexbor_erl:select(Doc, <<"#content p">>),
    
    %% Modify the document
    ok = lexbor_erl:set_text(Doc, Para, <<"Updated content via streaming!">>),
    ok = lexbor_erl:set_attribute(Doc, ContentDiv, <<"class">>, <<"highlighted">>),
    
    %% Create and add a new element
    {ok, Footer} = lexbor_erl:create_element(Doc, <<"div">>),
    ok = lexbor_erl:set_attribute(Doc, Footer, <<"id">>, <<"footer">>),
    ok = lexbor_erl:set_text(Doc, Footer, <<"Added after streaming">>),
    
    {ok, [Body]} = lexbor_erl:select(Doc, <<"body">>),
    ok = lexbor_erl:append_child(Doc, Body, Footer),
    
    %% Verify changes
    {ok, [UpdatedPara]} = lexbor_erl:select(Doc, <<"#content p">>),
    {ok, UpdatedText} = lexbor_erl:get_text(Doc, UpdatedPara),
    
    io:format("   Updated text: ~s~n", [UpdatedText]),
    
    ok = lexbor_erl:release(Doc),
    io:format("   ✓ Success~n~n").

%% Example 5: Simulating network streaming
%% This simulates receiving HTML over a network in small chunks
example_network_simulation() ->
    io:format("5. Network Streaming Simulation~n"),
    io:format("   Simulating 50-byte chunks from network~n"),
    
    %% Full HTML document
    Html = <<"
        <html>
        <head><title>Network Example</title></head>
        <body>
            <div class='article'>
                <h1>Article Title</h1>
                <p>This is a paragraph that demonstrates streaming HTML as if it was 
                   arriving from a network connection. The content is split into small 
                   chunks to simulate realistic network conditions.</p>
                <p>Another paragraph with <strong>bold text</strong> and 
                   <em>italic text</em> to show that complex HTML is properly handled.</p>
            </div>
        </body>
        </html>
    ">>,
    
    {ok, Session} = lexbor_erl:parse_stream_begin(),
    
    %% Split into 50-byte chunks and stream
    ChunkSize = 50,
    stream_in_chunks(Session, Html, ChunkSize),
    
    {ok, Doc} = lexbor_erl:parse_stream_end(Session),
    
    %% Verify the complete document
    {ok, [Title]} = lexbor_erl:select(Doc, <<"title">>),
    {ok, TitleText} = lexbor_erl:get_text(Doc, Title),
    
    {ok, Paragraphs} = lexbor_erl:select(Doc, <<"p">>),
    
    io:format("   Title: ~s~n", [TitleText]),
    io:format("   Paragraphs found: ~p~n", [length(Paragraphs)]),
    
    ok = lexbor_erl:release(Doc),
    io:format("   ✓ Success~n~n").

%% Helper: Stream binary in fixed-size chunks
stream_in_chunks(Session, Binary, ChunkSize) ->
    stream_in_chunks(Session, Binary, ChunkSize, 0).

stream_in_chunks(_Session, <<>>, _ChunkSize, _Count) ->
    ok;
stream_in_chunks(Session, Binary, ChunkSize, Count) ->
    case Binary of
        <<Chunk:ChunkSize/binary, Rest/binary>> ->
            ok = lexbor_erl:parse_stream_chunk(Session, Chunk),
            stream_in_chunks(Session, Rest, ChunkSize, Count + 1);
        LastChunk ->
            ok = lexbor_erl:parse_stream_chunk(Session, LastChunk),
            ok
    end.
