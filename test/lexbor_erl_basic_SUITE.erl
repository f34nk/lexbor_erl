-module(lexbor_erl_basic_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([test_start_stop/1, test_alive/1, test_parse_serialize_basic/1,
         test_parse_serialize_malformed/1, test_parse_serialize_entities/1,
         test_select_html_basic/1, test_select_html_multiple/1, test_select_html_no_match/1,
         test_select_html_complex_selector/1]).

all() ->
    [{group, lifecycle}, {group, stateless}].

groups() ->
    [{lifecycle, [sequence], [test_start_stop, test_alive]},
     {stateless,
      [sequence],
      [test_parse_serialize_basic,
       test_parse_serialize_malformed,
       test_parse_serialize_entities,
       test_select_html_basic,
       test_select_html_multiple,
       test_select_html_no_match,
       test_select_html_complex_selector]}].

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
