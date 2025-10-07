-module(lexbor_erl_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2]).

-export([
    % Lifecycle tests
    test_start_stop/1,
    test_alive/1,
    
    % Stateless operations
    test_parse_serialize_basic/1,
    test_parse_serialize_malformed/1,
    test_parse_serialize_entities/1,
    test_select_html_basic/1,
    test_select_html_multiple/1,
    test_select_html_no_match/1,
    test_select_html_complex_selector/1,
    
    % Stateful operations
    test_parse_release/1,
    test_parse_select_basic/1,
    test_parse_select_multiple/1,
    test_select_with_classes/1,
    test_select_with_id/1,
    test_select_descendant/1,
    test_select_child/1,
    test_outer_html/1,
    
    % Error cases
    test_invalid_selector/1,
    test_release_twice/1,
    test_use_after_release/1,
    
    % Edge cases
    test_empty_html/1,
    test_large_document/1,
    test_nested_elements/1,
    test_unicode_content/1
]).

all() ->
    [
        {group, lifecycle},
        {group, stateless},
        {group, stateful},
        {group, errors},
        {group, edge_cases}
    ].

groups() ->
    [
        {lifecycle, [sequence], [
            test_start_stop,
            test_alive
        ]},
        {stateless, [sequence], [
            test_parse_serialize_basic,
            test_parse_serialize_malformed,
            test_parse_serialize_entities,
            test_select_html_basic,
            test_select_html_multiple,
            test_select_html_no_match,
            test_select_html_complex_selector
        ]},
        {stateful, [sequence], [
            test_parse_release,
            test_parse_select_basic,
            test_parse_select_multiple,
            test_select_with_classes,
            test_select_with_id,
            test_select_descendant,
            test_select_child,
            test_outer_html
        ]},
        {errors, [sequence], [
            test_invalid_selector,
            test_release_twice,
            test_use_after_release
        ]},
        {edge_cases, [sequence], [
            test_empty_html,
            test_large_document,
            test_nested_elements,
            test_unicode_content
        ]}
    ].

init_per_suite(Config) ->
    % Ensure the application is loaded and started
    case application:ensure_all_started(lexbor_erl) of
        {ok, _Started} ->
            % Wait a moment for the port to initialize
            timer:sleep(100),
            case lexbor_erl:alive() of
                true -> 
                    Config;
                false ->
                    ct:fail("lexbor_erl port failed to start")
            end;
        {error, Reason} ->
            ct:fail({failed_to_start, Reason})
    end.

end_per_suite(_Config) ->
    application:stop(lexbor_erl),
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

%%====================================================================
%% Lifecycle Tests
%%====================================================================

test_start_stop(_Config) ->
    % Application is already started in init_per_suite
    % Just verify it's alive
    ?assert(lexbor_erl:alive()),
    ok.

test_alive(_Config) ->
    ?assert(lexbor_erl:alive()),
    ok.

%%====================================================================
%% Stateless Operations Tests
%%====================================================================

test_parse_serialize_basic(_Config) ->
    Html = <<"<div>Hello World</div>">>,
    {ok, Result} = lexbor_erl:parse_serialize(Html),
    
    % Should be wrapped in html/head/body
    ?assert(is_binary(Result)),
    ?assert(byte_size(Result) > byte_size(Html)),
    
    % Should contain original content
    ?assertMatch({_, _}, binary:match(Result, <<"Hello World">>)),
    ok.

test_parse_serialize_malformed(_Config) ->
    % Unclosed tags should be auto-closed
    Html = <<"<div>Hello<span>World">>,
    {ok, Result} = lexbor_erl:parse_serialize(Html),
    
    ?assert(is_binary(Result)),
    ?assertMatch({_, _}, binary:match(Result, <<"</span>">>)),
    ?assertMatch({_, _}, binary:match(Result, <<"</div>">>)),
    ok.

test_parse_serialize_entities(_Config) ->
    Html = <<"<div>&lt;tag&gt; &amp; &quot;text&quot;</div>">>,
    {ok, Result} = lexbor_erl:parse_serialize(Html),
    
    ?assert(is_binary(Result)),
    ok.

test_select_html_basic(_Config) ->
    Html = <<"<div><p>First</p><p>Second</p></div>">>,
    {ok, Results} = lexbor_erl:select_html(Html, <<"p">>),
    
    ?assertEqual(2, length(Results)),
    [First, Second] = Results,
    ?assertMatch({_, _}, binary:match(First, <<"First">>)),
    ?assertMatch({_, _}, binary:match(Second, <<"Second">>)),
    ok.

test_select_html_multiple(_Config) ->
    Html = <<"<ul><li>A</li><li>B</li><li>C</li></ul>">>,
    {ok, Results} = lexbor_erl:select_html(Html, <<"li">>),
    
    ?assertEqual(3, length(Results)),
    ok.

test_select_html_no_match(_Config) ->
    Html = <<"<div><p>Text</p></div>">>,
    {ok, Results} = lexbor_erl:select_html(Html, <<"span">>),
    
    ?assertEqual([], Results),
    ok.

test_select_html_complex_selector(_Config) ->
    Html = <<"<div id='main'><p class='intro'>A</p><p>B</p></div>">>,
    {ok, Results} = lexbor_erl:select_html(Html, <<"p.intro">>),
    
    ?assertEqual(1, length(Results)),
    [Match] = Results,
    ?assertMatch({_, _}, binary:match(Match, <<"intro">>)),
    ok.

%%====================================================================
%% Stateful Operations Tests
%%====================================================================

test_parse_release(_Config) ->
    Html = <<"<div>Test</div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    
    ?assert(is_integer(Doc)),
    ?assert(Doc > 0),
    
    ok = lexbor_erl:release(Doc),
    ok.

test_parse_select_basic(_Config) ->
    Html = <<"<div><p>Test</p></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    
    {ok, Nodes} = lexbor_erl:select(Doc, <<"p">>),
    ?assertEqual(1, length(Nodes)),
    
    [{node, Handle}] = Nodes,
    ?assert(is_integer(Handle)),
    
    ok = lexbor_erl:release(Doc),
    ok.

test_parse_select_multiple(_Config) ->
    Html = <<"<div><span>A</span><span>B</span><span>C</span></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    
    {ok, Nodes} = lexbor_erl:select(Doc, <<"span">>),
    ?assertEqual(3, length(Nodes)),
    
    lists:foreach(fun({node, H}) -> ?assert(is_integer(H)) end, Nodes),
    
    ok = lexbor_erl:release(Doc),
    ok.

test_select_with_classes(_Config) ->
    Html = <<"<ul><li class='a'>One</li><li class='b'>Two</li><li class='a'>Three</li></ul>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    
    {ok, Nodes} = lexbor_erl:select(Doc, <<"li.a">>),
    ?assertEqual(2, length(Nodes)),
    
    ok = lexbor_erl:release(Doc),
    ok.

test_select_with_id(_Config) ->
    Html = <<"<div><p id='first'>One</p><p id='second'>Two</p></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    
    {ok, Nodes} = lexbor_erl:select(Doc, <<"#first">>),
    ?assertEqual(1, length(Nodes)),
    
    ok = lexbor_erl:release(Doc),
    ok.

test_select_descendant(_Config) ->
    Html = <<"<div id='main'><p>A</p><section><p>B</p></section></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    
    % Descendant combinator (space) - should match all p under #main
    {ok, Nodes} = lexbor_erl:select(Doc, <<"#main p">>),
    ?assertEqual(2, length(Nodes)),
    
    ok = lexbor_erl:release(Doc),
    ok.

test_select_child(_Config) ->
    Html = <<"<div id='main'><p>A</p><section><p>B</p></section></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    
    % Child combinator (>) - should only match direct children
    {ok, Nodes} = lexbor_erl:select(Doc, <<"#main > p">>),
    ?assertEqual(1, length(Nodes)),
    
    ok = lexbor_erl:release(Doc),
    ok.

test_outer_html(_Config) ->
    Html = <<"<div><p class='test'>Content</p></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    
    {ok, Nodes} = lexbor_erl:select(Doc, <<"p">>),
    ?assertEqual(1, length(Nodes)),
    
    [Node] = Nodes,
    {ok, OuterHtml} = lexbor_erl:outer_html(Doc, Node),
    
    ?assert(is_binary(OuterHtml)),
    ?assertMatch({_, _}, binary:match(OuterHtml, <<"<p">>)),
    ?assertMatch({_, _}, binary:match(OuterHtml, <<"test">>)),
    ?assertMatch({_, _}, binary:match(OuterHtml, <<"Content">>)),
    ?assertMatch({_, _}, binary:match(OuterHtml, <<"</p>">>)),
    
    ok = lexbor_erl:release(Doc),
    ok.

%%====================================================================
%% Error Cases Tests
%%====================================================================

test_invalid_selector(_Config) ->
    Html = <<"<div><p>Test</p></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    
    % Invalid selector syntax should return error
    Result = lexbor_erl:select(Doc, <<":::">>),
    ?assertMatch({error, _}, Result),
    
    ok = lexbor_erl:release(Doc),
    ok.

test_release_twice(_Config) ->
    Html = <<"<div>Test</div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    
    ok = lexbor_erl:release(Doc),
    
    % Releasing again should return error
    Result = lexbor_erl:release(Doc),
    ?assertMatch({error, _}, Result),
    ok.

test_use_after_release(_Config) ->
    Html = <<"<div><p>Test</p></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    
    ok = lexbor_erl:release(Doc),
    
    % Using doc after release should return error
    Result = lexbor_erl:select(Doc, <<"p">>),
    ?assertMatch({error, _}, Result),
    ok.

%%====================================================================
%% Edge Cases Tests
%%====================================================================

test_empty_html(_Config) ->
    Html = <<"">>,
    {ok, Result} = lexbor_erl:parse_serialize(Html),
    
    ?assert(is_binary(Result)),
    % Should create minimal valid HTML document
    ?assertMatch({_, _}, binary:match(Result, <<"<html>">>)),
    ok.

test_large_document(_Config) ->
    % Generate a large HTML document
    Items = [io_lib:format("<li>Item ~p</li>", [N]) || N <- lists:seq(1, 1000)],
    Html = iolist_to_binary([<<"<ul>">>, Items, <<"</ul>">>]),
    
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, Nodes} = lexbor_erl:select(Doc, <<"li">>),
    
    ?assertEqual(1000, length(Nodes)),
    
    ok = lexbor_erl:release(Doc),
    ok.

test_nested_elements(_Config) ->
    % Deeply nested structure
    Html = <<"<div><div><div><div><div><p>Deep</p></div></div></div></div></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    
    {ok, Nodes} = lexbor_erl:select(Doc, <<"p">>),
    ?assertEqual(1, length(Nodes)),
    
    [Node] = Nodes,
    {ok, OuterHtml} = lexbor_erl:outer_html(Doc, Node),
    ?assertMatch({_, _}, binary:match(OuterHtml, <<"Deep">>)),
    
    ok = lexbor_erl:release(Doc),
    ok.

test_unicode_content(_Config) ->
    % Test with UTF-8 encoded HTML
    % Build the HTML with UTF-8 Unicode characters using codepoints
    World = unicode:characters_to_binary("世界", utf8),
    Hello = unicode:characters_to_binary("Здравствуй", utf8),
    Marhaba = unicode:characters_to_binary("مرحبا", utf8),
    
    Html = iolist_to_binary([
        <<"<div><p>Hello ">>, World, <<"</p><p>">>, Hello, 
        <<" world</p><p>">>, Marhaba, <<" world</p></div>">>
    ]),
    
    {ok, Doc} = lexbor_erl:parse(Html),
    
    {ok, Nodes} = lexbor_erl:select(Doc, <<"p">>),
    ?assertEqual(3, length(Nodes)),
    
    % Get HTML of first node
    [First | _] = Nodes,
    {ok, FirstHtml} = lexbor_erl:outer_html(Doc, First),
    
    % Verify we got HTML back
    ?assert(is_binary(FirstHtml)),
    ?assert(byte_size(FirstHtml) > 0),
    
    % Verify it contains "Hello" at minimum (ASCII chars should always work)
    ?assertMatch({_, _}, binary:match(FirstHtml, <<"Hello">>)),
    
    ok = lexbor_erl:release(Doc),
    ok.
