-module(lexbor_demo).
-export([main/1, run_all/0]).

%% Demo application for lexbor_erl from hex.pm
%% This verifies that the published library works correctly

main([]) ->
    io:format("~n=== lexbor_erl Demo from hex.pm ===~n~n"),
    
    case application:ensure_all_started(lexbor_erl) of
        {ok, _} ->
            io:format("✓ lexbor_erl application started~n~n"),
            run_all(),
            io:format("~n=== All demos completed successfully! ===~n~n"),
            halt(0);
        {error, Reason} ->
            io:format("✗ Failed to start lexbor_erl: ~p~n", [Reason]),
            halt(1)
    end;
main(_) ->
    halt(1).

run_all() ->
    demo_1_basic_parsing(),
    demo_2_css_selectors(),
    demo_3_attributes(),
    demo_4_text_content(),
    demo_5_dom_manipulation(),
    demo_6_serialization(),
    demo_7_streaming_parse().

%% Demo 1: Basic HTML Parsing
demo_1_basic_parsing() ->
    io:format("--- Demo 1: Basic HTML Parsing ---~n"),
    
    Html = <<"<html><head><title>Test</title></head><body><h1>Hello World</h1></body></html>">>,
    
    {ok, Doc} = lexbor_erl:parse(Html),
    io:format("✓ Parsed HTML document (ID: ~p)~n", [Doc]),
    
    lexbor_erl:release(Doc),
    io:format("✓ Released document~n~n"),
    ok.

%% Demo 2: CSS Selectors
demo_2_css_selectors() ->
    io:format("--- Demo 2: CSS Selectors ---~n"),
    
    Html = <<"<html><body>",
             "<div class='container'>",
             "<p id='first'>First paragraph</p>",
             "<p class='highlight'>Second paragraph</p>",
             "<p class='highlight'>Third paragraph</p>",
             "</div></body></html>">>,
    
    {ok, Doc} = lexbor_erl:parse(Html),
    
    %% Select by ID
    {ok, [FirstP]} = lexbor_erl:select(Doc, <<"#first">>),
    io:format("✓ Selected by ID: #first -> ~p~n", [FirstP]),
    
    %% Select by class
    {ok, Highlights} = lexbor_erl:select(Doc, <<".highlight">>),
    io:format("✓ Selected by class: .highlight -> ~p elements~n", [length(Highlights)]),
    
    %% Select by element
    {ok, AllPs} = lexbor_erl:select(Doc, <<"p">>),
    io:format("✓ Selected by tag: p -> ~p elements~n", [length(AllPs)]),
    
    %% Complex selector
    {ok, Complex} = lexbor_erl:select(Doc, <<"div.container > p">>),
    io:format("✓ Complex selector: 'div.container > p' -> ~p elements~n", [length(Complex)]),
    
    lexbor_erl:release(Doc),
    io:format("~n"),
    ok.

%% Demo 3: Attribute Operations
demo_3_attributes() ->
    io:format("--- Demo 3: Attribute Operations ---~n"),
    
    Html = <<"<html><body><a href='/home' class='link'>Link</a></body></html>">>,
    
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, [Link]} = lexbor_erl:select(Doc, <<"a">>),
    
    %% Get attributes
    {ok, Href} = lexbor_erl:get_attribute(Doc, Link, <<"href">>),
    io:format("✓ Get href: ~p~n", [Href]),
    
    {ok, Class} = lexbor_erl:get_attribute(Doc, Link, <<"class">>),
    io:format("✓ Get class: ~p~n", [Class]),
    
    %% Set attribute
    ok = lexbor_erl:set_attribute(Doc, Link, <<"href">>, <<"/new-path">>),
    {ok, NewHref} = lexbor_erl:get_attribute(Doc, Link, <<"href">>),
    io:format("✓ Set href to: ~p~n", [NewHref]),
    
    %% Remove attribute
    ok = lexbor_erl:remove_attribute(Doc, Link, <<"class">>),
    {ok, undefined} = lexbor_erl:get_attribute(Doc, Link, <<"class">>),
    io:format("✓ Removed class attribute~n"),
    
    lexbor_erl:release(Doc),
    io:format("~n"),
    ok.

%% Demo 4: Text Content
demo_4_text_content() ->
    io:format("--- Demo 4: Text Content ---~n"),
    
    Html = <<"<html><body><div>Hello <span>Beautiful</span> World</div></body></html>">>,
    
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, [Div]} = lexbor_erl:select(Doc, <<"div">>),
    
    %% Get text (recursive)
    {ok, Text} = lexbor_erl:get_text(Doc, Div),
    io:format("✓ Get text: ~p~n", [Text]),
    
    %% Set text (replaces children)
    ok = lexbor_erl:set_text(Doc, Div, <<"New text content">>),
    {ok, NewText} = lexbor_erl:get_text(Doc, Div),
    io:format("✓ Set text: ~p~n", [NewText]),
    
    lexbor_erl:release(Doc),
    io:format("~n"),
    ok.

%% Demo 5: DOM Manipulation
demo_5_dom_manipulation() ->
    io:format("--- Demo 5: DOM Manipulation ---~n"),
    
    Html = <<"<html><body><div id='container'></div></body></html>">>,
    
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, [Container]} = lexbor_erl:select(Doc, <<"#container">>),
    
    %% Create element
    {ok, NewP} = lexbor_erl:create_element(Doc, <<"p">>),
    io:format("✓ Created <p> element~n"),
    
    %% Set attributes and text
    ok = lexbor_erl:set_attribute(Doc, NewP, <<"class">>, <<"dynamic">>),
    ok = lexbor_erl:set_text(Doc, NewP, <<"Dynamic content">>),
    io:format("✓ Set attributes and text~n"),
    
    %% Append to container
    ok = lexbor_erl:append_child(Doc, Container, NewP),
    io:format("✓ Appended child~n"),
    
    %% Get outer HTML
    {ok, OuterHtml} = lexbor_erl:outer_html(Doc, Container),
    io:format("✓ Outer HTML: ~s~n", [OuterHtml]),
    
    lexbor_erl:release(Doc),
    io:format("~n"),
    ok.

%% Demo 6: Serialization
demo_6_serialization() ->
    io:format("--- Demo 6: Serialization ---~n"),
    
    Html = <<"<html><head><title>Page</title></head><body><h1>Content</h1></body></html>">>,
    
    {ok, Doc} = lexbor_erl:parse(Html),
    
    %% Serialize document
    {ok, Serialized} = lexbor_erl:serialize(Doc),
    io:format("✓ Serialized document (~p bytes)~n", [byte_size(Serialized)]),
    io:format("  Output: ~s~n", [Serialized]),
    
    %% Round-trip test
    {ok, Doc2} = lexbor_erl:parse(Serialized),
    {ok, [H1]} = lexbor_erl:select(Doc2, <<"h1">>),
    {ok, Content} = lexbor_erl:get_text(Doc2, H1),
    io:format("✓ Round-trip successful: ~p~n", [Content]),
    
    lexbor_erl:release(Doc),
    lexbor_erl:release(Doc2),
    io:format("~n"),
    ok.

%% Demo 7: Streaming Parser
demo_7_streaming_parse() ->
    io:format("--- Demo 7: Streaming Parser ---~n"),
    
    %% Parse HTML in chunks (simulates streaming)
    Chunk1 = <<"<html><head><title>Streaming">>,
    Chunk2 = <<" Test</title></head><body>">>,
    Chunk3 = <<"<p>Content</p></body></html>">>,
    
    {ok, Doc} = lexbor_erl:parse_stream_begin(),
    io:format("✓ Started streaming parser~n"),
    
    ok = lexbor_erl:parse_stream_chunk(Doc, Chunk1),
    io:format("✓ Parsed chunk 1 (~p bytes)~n", [byte_size(Chunk1)]),
    
    ok = lexbor_erl:parse_stream_chunk(Doc, Chunk2),
    io:format("✓ Parsed chunk 2 (~p bytes)~n", [byte_size(Chunk2)]),
    
    ok = lexbor_erl:parse_stream_chunk(Doc, Chunk3),
    io:format("✓ Parsed chunk 3 (~p bytes)~n", [byte_size(Chunk3)]),
    
    {ok, FinalDoc} = lexbor_erl:parse_stream_end(Doc),
    io:format("✓ Finalized streaming parse~n"),
    
    %% Verify parsing worked
    {ok, [Title]} = lexbor_erl:select(FinalDoc, <<"title">>),
    {ok, TitleText} = lexbor_erl:get_text(FinalDoc, Title),
    io:format("✓ Title: ~p~n", [TitleText]),
    
    lexbor_erl:release(FinalDoc),
    io:format("~n"),
    ok.
