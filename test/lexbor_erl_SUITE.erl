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
    test_unicode_content/1,
    
    % Parallel tests
    test_parallel_parse_serialize/1,
    test_parallel_stateful_ops/1,
    test_parallel_mixed_ops/1,
    test_worker_isolation/1,
    
    % Fault tolerance tests
    test_worker_crash_recovery/1,
    test_worker_isolation_after_crash/1,
    
    % DOM manipulation - attributes
    test_get_attribute/1,
    test_set_attribute/1,
    test_remove_attribute/1,
    test_attribute_not_found/1,
    
    % DOM manipulation - text and HTML
    test_get_text/1,
    test_set_text/1,
    test_inner_html/1,
    test_set_inner_html/1,
    test_serialize/1,
    
    % DOM manipulation - node creation
    test_create_element/1,
    test_append_child/1,
    test_insert_before/1,
    test_remove_node/1,
    test_complex_dom_building/1,
    
    % Streaming parser
    test_stream_basic/1,
    test_stream_multiple_chunks/1,
    test_stream_split_in_tag/1,
    test_stream_large_document/1,
    test_stream_with_queries/1,
    test_stream_invalid_session/1,
    test_stream_parallel/1
]).

all() ->
    [
        {group, lifecycle},
        {group, stateless},
        {group, stateful},
        {group, streaming},
        {group, dom_manipulation},
        {group, errors},
        {group, edge_cases},
        {group, parallel},
        {group, fault_tolerance}
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
        {streaming, [sequence], [
            test_stream_basic,
            test_stream_multiple_chunks,
            test_stream_split_in_tag,
            test_stream_large_document,
            test_stream_with_queries,
            test_stream_invalid_session,
            test_stream_parallel
        ]},
        {dom_manipulation, [sequence], [
            % Attributes
            test_get_attribute,
            test_set_attribute,
            test_remove_attribute,
            test_attribute_not_found,
            % Text and HTML
            test_get_text,
            test_set_text,
            test_inner_html,
            test_set_inner_html,
            test_serialize,
            % Node creation and tree manipulation
            test_create_element,
            test_append_child,
            test_insert_before,
            test_remove_node,
            test_complex_dom_building
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
        ]},
        {parallel, [parallel], [
            test_parallel_parse_serialize,
            test_parallel_stateful_ops,
            test_parallel_mixed_ops,
            test_worker_isolation
        ]},
        {fault_tolerance, [sequence], [
            test_worker_crash_recovery,
            test_worker_isolation_after_crash
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

%% ===================================================================
%% Parallel Tests
%% ===================================================================

%% Test concurrent stateless operations (parse_serialize)
test_parallel_parse_serialize(_Config) ->
    Count = 50,
    Self = self(),
    
    % Spawn multiple concurrent parse operations
    [spawn(fun() ->
        Html = <<"<html><body><h1>Title ", (integer_to_binary(N))/binary, 
                 "</h1><p>Content</p></body></html>">>,
        Result = lexbor_erl:parse_serialize(Html),
        Self ! {done, N, Result}
    end) || N <- lists:seq(1, Count)],
    
    % Collect all results
    Results = [receive 
        {done, N, R} -> {N, R} 
    after 10000 -> 
        ct:fail({timeout, N}) 
    end || N <- lists:seq(1, Count)],
    
    % Verify all succeeded
    ?assertEqual(Count, length(Results)),
    lists:foreach(fun({_N, {ok, Html}}) -> 
        ?assert(is_binary(Html)),
        ?assert(byte_size(Html) > 0)
    end, Results),
    
    ok.

%% Test concurrent stateful operations (parse, select, release)
test_parallel_stateful_ops(_Config) ->
    Count = 40,
    Self = self(),
    
    % Spawn multiple concurrent stateful workflows
    [spawn(fun() ->
        Html = <<"<html><body><div class='test'>Item ", 
                 (integer_to_binary(N))/binary, "</div></body></html>">>,
        
        % Each process does: parse -> select -> outer_html -> release
        {ok, DocId} = lexbor_erl:parse(Html),
        {ok, Nodes} = lexbor_erl:select(DocId, <<"div.test">>),
        ?assertEqual(1, length(Nodes)),
        
        [Node] = Nodes,
        {ok, NodeHtml} = lexbor_erl:outer_html(DocId, Node),
        ?assert(byte_size(NodeHtml) > 0),
        
        ok = lexbor_erl:release(DocId),
        Self ! {done, N}
    end) || N <- lists:seq(1, Count)],
    
    % Wait for all to complete
    [receive {done, N} -> ok after 10000 -> ct:fail({timeout, N}) end 
     || N <- lists:seq(1, Count)],
    
    ok.

%% Test mixed stateless and stateful operations concurrently
test_parallel_mixed_ops(_Config) ->
    Count = 30,
    Self = self(),
    
    % Spawn stateless operations
    [spawn(fun() ->
        Html = <<"<p>Stateless ", (integer_to_binary(N))/binary, "</p>">>,
        {ok, Result} = lexbor_erl:parse_serialize(Html),
        ?assert(is_binary(Result)),
        Self ! {done, stateless, N}
    end) || N <- lists:seq(1, Count)],
    
    % Spawn stateful operations
    [spawn(fun() ->
        Html = <<"<div><span>Stateful ", (integer_to_binary(N))/binary, "</span></div>">>,
        {ok, DocId} = lexbor_erl:parse(Html),
        {ok, Nodes} = lexbor_erl:select(DocId, <<"span">>),
        ?assertEqual(1, length(Nodes)),
        ok = lexbor_erl:release(DocId),
        Self ! {done, stateful, N}
    end) || N <- lists:seq(1, Count)],
    
    % Wait for all to complete
    [receive {done, stateless, N} -> ok after 10000 -> ct:fail({timeout_stateless, N}) end 
     || N <- lists:seq(1, Count)],
    [receive {done, stateful, N} -> ok after 10000 -> ct:fail({timeout_stateful, N}) end 
     || N <- lists:seq(1, Count)],
    
    ok.

%% Test that workers are properly isolated (DocId routing)
test_worker_isolation(_Config) ->
    % Create multiple documents concurrently
    Html1 = <<"<div id='doc1'>Document 1</div>">>,
    Html2 = <<"<div id='doc2'>Document 2</div>">>,
    Html3 = <<"<div id='doc3'>Document 3</div>">>,
    
    {ok, Doc1} = lexbor_erl:parse(Html1),
    {ok, Doc2} = lexbor_erl:parse(Html2),
    {ok, Doc3} = lexbor_erl:parse(Html3),
    
    % Verify different documents got different IDs
    ?assert(Doc1 =/= Doc2),
    ?assert(Doc2 =/= Doc3),
    ?assert(Doc1 =/= Doc3),
    
    % Perform operations on different documents in parallel
    Self = self(),
    spawn(fun() ->
        {ok, Nodes1} = lexbor_erl:select(Doc1, <<"#doc1">>),
        ?assertEqual(1, length(Nodes1)),
        Self ! {done, 1}
    end),
    spawn(fun() ->
        {ok, Nodes2} = lexbor_erl:select(Doc2, <<"#doc2">>),
        ?assertEqual(1, length(Nodes2)),
        Self ! {done, 2}
    end),
    spawn(fun() ->
        {ok, Nodes3} = lexbor_erl:select(Doc3, <<"#doc3">>),
        ?assertEqual(1, length(Nodes3)),
        Self ! {done, 3}
    end),
    
    % Wait for all
    receive {done, 1} -> ok after 5000 -> ct:fail(timeout_doc1) end,
    receive {done, 2} -> ok after 5000 -> ct:fail(timeout_doc2) end,
    receive {done, 3} -> ok after 5000 -> ct:fail(timeout_doc3) end,
    
    % Clean up
    ok = lexbor_erl:release(Doc1),
    ok = lexbor_erl:release(Doc2),
    ok = lexbor_erl:release(Doc3),
    
    ok.

%% ===================================================================
%% Fault Tolerance Tests
%% ===================================================================

%% Test that a worker crash is recovered and doesn't affect other workers
test_worker_crash_recovery(_Config) ->
    % Create documents on multiple workers
    Html1 = <<"<div id='doc1'>Document 1</div>">>,
    Html2 = <<"<div id='doc2'>Document 2</div>">>,
    
    {ok, Doc1} = lexbor_erl:parse(Html1),
    {ok, Doc2} = lexbor_erl:parse(Html2),
    
    % Extract worker IDs from DocIds
    Worker1Id = (Doc1 bsr 56) band 16#FF,
    Worker2Id = (Doc2 bsr 56) band 16#FF,
    
    % Verify they're on different workers (with retries if needed)
    Doc1Final = case Worker1Id =:= Worker2Id of
        true ->
            % Try again to get a document on a different worker
            {ok, Doc1New} = lexbor_erl:parse(Html1),
            Doc1New;
        false ->
            Doc1
    end,
    
    Worker1IdFinal = (Doc1Final bsr 56) band 16#FF,
    Worker2IdFinal = (Doc2 bsr 56) band 16#FF,
    
    ct:log("Doc1 on worker ~p, Doc2 on worker ~p~n", [Worker1IdFinal, Worker2IdFinal]),
    
    % Kill worker 1
    Worker1Name = lexbor_erl_worker:worker_name(Worker1IdFinal),
    Worker1Pid = whereis(Worker1Name),
    ?assert(is_pid(Worker1Pid)),
    
    ct:log("Killing worker ~p (pid ~p)~n", [Worker1IdFinal, Worker1Pid]),
    exit(Worker1Pid, kill),
    
    % Wait for supervisor to restart it
    timer:sleep(100),
    
    % Verify worker 1 was restarted with new PID
    Worker1NewPid = whereis(Worker1Name),
    ?assert(is_pid(Worker1NewPid)),
    ?assert(Worker1NewPid =/= Worker1Pid),
    ct:log("Worker restarted with new pid ~p~n", [Worker1NewPid]),
    
    % Doc1 should be gone (it was in crashed worker's memory)
    % Doc2 should still work if on different worker
    if Worker2IdFinal =/= Worker1IdFinal ->
        {ok, Nodes2} = lexbor_erl:select(Doc2, <<"#doc2">>),
        ?assertEqual(1, length(Nodes2)),
        ok = lexbor_erl:release(Doc2);
    true ->
        % Both were on same worker, both lost
        ok
    end,
    
    % Verify restarted worker accepts new documents
    {ok, Doc3} = lexbor_erl:parse(<<"<div id='doc3'>Document 3</div>">>),
    {ok, Nodes3} = lexbor_erl:select(Doc3, <<"#doc3">>),
    ?assertEqual(1, length(Nodes3)),
    ok = lexbor_erl:release(Doc3),
    
    ok.

%% Test that workers remain isolated after a crash
test_worker_isolation_after_crash(_Config) ->
    % Get pool size
    PoolSize = lexbor_erl_pool:get_pool_size(),
    ct:log("Pool size: ~p~n", [PoolSize]),
    
    % Create documents on all workers
    Docs = [begin
        Html = iolist_to_binary([<<"<div id='worker">>, integer_to_list(I), <<"'>Worker ">>, integer_to_list(I), <<"</div>">>]),
        {ok, Doc} = lexbor_erl:parse(Html),
        WorkerId = (Doc bsr 56) band 16#FF,
        ct:log("Created Doc ~p on worker ~p~n", [Doc, WorkerId]),
        {I, Doc, WorkerId}
    end || I <- lists:seq(1, PoolSize)],
    
    % Verify we can access all documents
    lists:foreach(fun({I, Doc, WorkerId}) ->
        SelectorId = iolist_to_binary([<<"#worker">>, integer_to_list(I)]),
        {ok, Nodes} = lexbor_erl:select(Doc, SelectorId),
        ?assertEqual(1, length(Nodes)),
        ct:log("Worker ~p document OK~n", [WorkerId])
    end, Docs),
    
    % Kill worker 1
    Worker1Name = lexbor_erl_worker:worker_name(1),
    Worker1Pid = whereis(Worker1Name),
    ct:log("Killing worker 1 (pid ~p)~n", [Worker1Pid]),
    exit(Worker1Pid, kill),
    
    % Wait for restart
    timer:sleep(100),
    
    % Verify worker 1 was restarted
    Worker1NewPid = whereis(Worker1Name),
    ?assert(is_pid(Worker1NewPid)),
    ?assert(Worker1NewPid =/= Worker1Pid),
    ct:log("Worker 1 restarted~n"),
    
    % Clean up documents and verify other workers still work
    lists:foreach(fun({_I, Doc, WorkerId}) ->
        if WorkerId =:= 1 ->
            % Document on crashed worker is gone, skip it
            ct:log("Skipping doc on crashed worker ~p~n", [WorkerId]),
            ok;
        true ->
            % Documents on other workers should still be accessible
            ok = lexbor_erl:release(Doc),
            ct:log("Released doc on worker ~p~n", [WorkerId])
        end
    end, Docs),
    
    % Verify restarted worker 1 can handle new documents
    {ok, NewDoc} = lexbor_erl:parse(<<"<div id='new'>New Doc</div>">>),
    {ok, Nodes} = lexbor_erl:select(NewDoc, <<"#new">>),
    ?assertEqual(1, length(Nodes)),
    ok = lexbor_erl:release(NewDoc),
    
    ok.

%% ========================================================================
%% DOM Manipulation Tests - Attributes
%% ========================================================================

test_get_attribute(_Config) ->
    Html = <<"<a href='/home' class='link' id='main'>Link</a>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, [Link]} = lexbor_erl:select(Doc, <<"a">>),
    
    %% Get existing attributes
    {ok, Href} = lexbor_erl:get_attribute(Doc, Link, <<"href">>),
    ?assertEqual(<<"/home">>, Href),
    
    {ok, Class} = lexbor_erl:get_attribute(Doc, Link, <<"class">>),
    ?assertEqual(<<"link">>, Class),
    
    {ok, Id} = lexbor_erl:get_attribute(Doc, Link, <<"id">>),
    ?assertEqual(<<"main">>, Id),
    
    ok = lexbor_erl:release(Doc).

test_set_attribute(_Config) ->
    Html = <<"<div id='test'>Content</div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, [Div]} = lexbor_erl:select(Doc, <<"div">>),
    
    %% Update existing attribute
    ok = lexbor_erl:set_attribute(Doc, Div, <<"id">>, <<"updated">>),
    {ok, NewId} = lexbor_erl:get_attribute(Doc, Div, <<"id">>),
    ?assertEqual(<<"updated">>, NewId),
    
    %% Add new attribute
    ok = lexbor_erl:set_attribute(Doc, Div, <<"class">>, <<"active">>),
    {ok, Class} = lexbor_erl:get_attribute(Doc, Div, <<"class">>),
    ?assertEqual(<<"active">>, Class),
    
    %% Verify in HTML
    {ok, Html2} = lexbor_erl:outer_html(Doc, Div),
    ?assert(binary:match(Html2, <<"id=\"updated\"">>) =/= nomatch),
    ?assert(binary:match(Html2, <<"class=\"active\"">>) =/= nomatch),
    
    ok = lexbor_erl:release(Doc).

test_remove_attribute(_Config) ->
    Html = <<"<div class='test' id='main' data-value='123'>Content</div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, [Div]} = lexbor_erl:select(Doc, <<"div">>),
    
    %% Remove attribute
    ok = lexbor_erl:remove_attribute(Doc, Div, <<"class">>),
    {ok, undefined} = lexbor_erl:get_attribute(Doc, Div, <<"class">>),
    
    %% Other attributes still present
    {ok, Id} = lexbor_erl:get_attribute(Doc, Div, <<"id">>),
    ?assertEqual(<<"main">>, Id),
    
    %% Remove another
    ok = lexbor_erl:remove_attribute(Doc, Div, <<"data-value">>),
    {ok, undefined} = lexbor_erl:get_attribute(Doc, Div, <<"data-value">>),
    
    ok = lexbor_erl:release(Doc).

test_attribute_not_found(_Config) ->
    Html = <<"<div>Content</div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, [Div]} = lexbor_erl:select(Doc, <<"div">>),
    
    %% Non-existent attribute returns undefined
    {ok, undefined} = lexbor_erl:get_attribute(Doc, Div, <<"class">>),
    {ok, undefined} = lexbor_erl:get_attribute(Doc, Div, <<"id">>),
    {ok, undefined} = lexbor_erl:get_attribute(Doc, Div, <<"data-anything">>),
    
    ok = lexbor_erl:release(Doc).

%% ========================================================================
%% DOM Manipulation Tests - Text and HTML
%% ========================================================================

test_get_text(_Config) ->
    Html = <<"<div>Hello <b>World</b>!</div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, [Div]} = lexbor_erl:select(Doc, <<"div">>),
    
    %% Get text content (no HTML tags)
    {ok, Text} = lexbor_erl:get_text(Doc, Div),
    ?assertEqual(<<"Hello World!">>, Text),
    
    %% Get text from nested element
    {ok, [Bold]} = lexbor_erl:select(Doc, <<"b">>),
    {ok, BoldText} = lexbor_erl:get_text(Doc, Bold),
    ?assertEqual(<<"World">>, BoldText),
    
    ok = lexbor_erl:release(Doc).

test_set_text(_Config) ->
    Html = <<"<div>Old <b>content</b></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, [Div]} = lexbor_erl:select(Doc, <<"div">>),
    
    %% Set text (replaces all children)
    ok = lexbor_erl:set_text(Doc, Div, <<"New text">>),
    
    {ok, NewText} = lexbor_erl:get_text(Doc, Div),
    ?assertEqual(<<"New text">>, NewText),
    
    %% Verify HTML has no child elements
    {ok, Html2} = lexbor_erl:outer_html(Doc, Div),
    ?assert(binary:match(Html2, <<"<b>">>) =:= nomatch),
    ?assert(binary:match(Html2, <<"New text">>) =/= nomatch),
    
    ok = lexbor_erl:release(Doc).

test_inner_html(_Config) ->
    Html = <<"<div><p>First</p><p>Second</p></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, [Div]} = lexbor_erl:select(Doc, <<"div">>),
    
    %% Get inner HTML
    {ok, Inner} = lexbor_erl:inner_html(Doc, Div),
    ?assertEqual(<<"<p>First</p><p>Second</p>">>, Inner),
    
    ok = lexbor_erl:release(Doc).

test_set_inner_html(_Config) ->
    Html = <<"<div>Old content</div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, [Div]} = lexbor_erl:select(Doc, <<"div">>),
    
    %% Set inner HTML
    ok = lexbor_erl:set_inner_html(Doc, Div, <<"<p>New</p><span>Content</span>">>),
    
    {ok, NewInner} = lexbor_erl:inner_html(Doc, Div),
    ?assert(binary:match(NewInner, <<"<p>New</p>">>) =/= nomatch),
    ?assert(binary:match(NewInner, <<"<span>Content</span>">>) =/= nomatch),
    
    %% Verify we can select the new elements
    {ok, [_P]} = lexbor_erl:select(Doc, <<"p">>),
    {ok, [_Span]} = lexbor_erl:select(Doc, <<"span">>),
    
    ok = lexbor_erl:release(Doc).

test_serialize(_Config) ->
    Html = <<"<div id='main'><p>Content</p></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, [Div]} = lexbor_erl:select(Doc, <<"div">>),
    
    %% Modify document
    ok = lexbor_erl:set_attribute(Doc, Div, <<"class">>, <<"active">>),
    
    %% Serialize entire document
    {ok, FullHtml} = lexbor_erl:serialize(Doc),
    
    %% Verify modifications are in serialized output
    ?assert(binary:match(FullHtml, <<"id=\"main\"">>) =/= nomatch),
    ?assert(binary:match(FullHtml, <<"class=\"active\"">>) =/= nomatch),
    ?assert(binary:match(FullHtml, <<"<p>Content</p>">>) =/= nomatch),
    ?assert(binary:match(FullHtml, <<"<html>">>) =/= nomatch),
    
    ok = lexbor_erl:release(Doc).

%% ========================================================================
%% DOM Manipulation Tests - Node Creation and Tree Manipulation
%% ========================================================================

test_create_element(_Config) ->
    Html = <<"<html><body></body></html>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    
    %% Create new elements
    {ok, Div} = lexbor_erl:create_element(Doc, <<"div">>),
    ?assertMatch({node, _}, Div),
    
    {ok, Span} = lexbor_erl:create_element(Doc, <<"span">>),
    ?assertMatch({node, _}, Span),
    
    %% Set attributes on created elements
    ok = lexbor_erl:set_attribute(Doc, Div, <<"id">>, <<"new">>),
    {ok, Id} = lexbor_erl:get_attribute(Doc, Div, <<"id">>),
    ?assertEqual(<<"new">>, Id),
    
    ok = lexbor_erl:release(Doc).

test_append_child(_Config) ->
    Html = <<"<ul id='list'></ul>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, [List]} = lexbor_erl:select(Doc, <<"ul">>),
    
    %% Create and append items
    {ok, Item1} = lexbor_erl:create_element(Doc, <<"li">>),
    ok = lexbor_erl:set_text(Doc, Item1, <<"First">>),
    ok = lexbor_erl:append_child(Doc, List, Item1),
    
    {ok, Item2} = lexbor_erl:create_element(Doc, <<"li">>),
    ok = lexbor_erl:set_text(Doc, Item2, <<"Second">>),
    ok = lexbor_erl:append_child(Doc, List, Item2),
    
    %% Verify items are in list
    {ok, Items} = lexbor_erl:select(Doc, <<"li">>),
    ?assertEqual(2, length(Items)),
    
    {ok, ListHtml} = lexbor_erl:outer_html(Doc, List),
    ?assert(binary:match(ListHtml, <<"<li>First</li>">>) =/= nomatch),
    ?assert(binary:match(ListHtml, <<"<li>Second</li>">>) =/= nomatch),
    
    ok = lexbor_erl:release(Doc).

test_insert_before(_Config) ->
    Html = <<"<ul><li id='second'>Second</li></ul>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, [List]} = lexbor_erl:select(Doc, <<"ul">>),
    {ok, [Second]} = lexbor_erl:select(Doc, <<"#second">>),
    
    %% Create and insert first item
    {ok, First} = lexbor_erl:create_element(Doc, <<"li">>),
    ok = lexbor_erl:set_attribute(Doc, First, <<"id">>, <<"first">>),
    ok = lexbor_erl:set_text(Doc, First, <<"First">>),
    ok = lexbor_erl:insert_before(Doc, List, First, Second),
    
    %% Verify order
    {ok, Items} = lexbor_erl:select(Doc, <<"li">>),
    ?assertEqual(2, length(Items)),
    
    [FirstItem, SecondItem] = Items,
    {ok, FirstText} = lexbor_erl:get_text(Doc, FirstItem),
    {ok, SecondText} = lexbor_erl:get_text(Doc, SecondItem),
    ?assertEqual(<<"First">>, FirstText),
    ?assertEqual(<<"Second">>, SecondText),
    
    ok = lexbor_erl:release(Doc).

test_remove_node(_Config) ->
    Html = <<"<div><p id='remove'>Remove</p><p id='keep'>Keep</p></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, [RemoveP]} = lexbor_erl:select(Doc, <<"#remove">>),
    
    %% Remove node
    ok = lexbor_erl:remove_node(Doc, RemoveP),
    
    %% Verify it's gone
    {ok, Paragraphs} = lexbor_erl:select(Doc, <<"p">>),
    ?assertEqual(1, length(Paragraphs)),
    
    [KeepP] = Paragraphs,
    {ok, Id} = lexbor_erl:get_attribute(Doc, KeepP, <<"id">>),
    ?assertEqual(<<"keep">>, Id),
    
    ok = lexbor_erl:release(Doc).

test_complex_dom_building(_Config) ->
    Html = <<"<div id='container'></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, [Container]} = lexbor_erl:select(Doc, <<"#container">>),
    
    %% Build complex structure
    {ok, Header} = lexbor_erl:create_element(Doc, <<"h1">>),
    ok = lexbor_erl:set_attribute(Doc, Header, <<"class">>, <<"title">>),
    ok = lexbor_erl:set_text(Doc, Header, <<"Page Title">>),
    ok = lexbor_erl:append_child(Doc, Container, Header),
    
    {ok, Section} = lexbor_erl:create_element(Doc, <<"section">>),
    ok = lexbor_erl:set_attribute(Doc, Section, <<"id">>, <<"content">>),
    ok = lexbor_erl:append_child(Doc, Container, Section),
    
    {ok, Para1} = lexbor_erl:create_element(Doc, <<"p">>),
    ok = lexbor_erl:set_text(Doc, Para1, <<"First paragraph">>),
    ok = lexbor_erl:append_child(Doc, Section, Para1),
    
    {ok, Para2} = lexbor_erl:create_element(Doc, <<"p">>),
    ok = lexbor_erl:set_attribute(Doc, Para2, <<"class">>, <<"highlight">>),
    ok = lexbor_erl:set_text(Doc, Para2, <<"Second paragraph">>),
    ok = lexbor_erl:append_child(Doc, Section, Para2),
    
    %% Add list
    {ok, List} = lexbor_erl:create_element(Doc, <<"ul">>),
    ok = lexbor_erl:append_child(Doc, Section, List),
    
    lists:foreach(fun(N) ->
        {ok, Item} = lexbor_erl:create_element(Doc, <<"li">>),
        Text = iolist_to_binary(io_lib:format("Item ~p", [N])),
        ok = lexbor_erl:set_text(Doc, Item, Text),
        ok = lexbor_erl:append_child(Doc, List, Item)
    end, lists:seq(1, 3)),
    
    %% Verify structure
    {ok, FinalHtml} = lexbor_erl:serialize(Doc),
    
    ?assert(binary:match(FinalHtml, <<"<h1 class=\"title\">Page Title</h1>">>) =/= nomatch),
    ?assert(binary:match(FinalHtml, <<"<section id=\"content\"">>) =/= nomatch),
    ?assert(binary:match(FinalHtml, <<"First paragraph">>) =/= nomatch),
    ?assert(binary:match(FinalHtml, <<"class=\"highlight\"">>) =/= nomatch),
    ?assert(binary:match(FinalHtml, <<"<li>Item 1</li>">>) =/= nomatch),
    ?assert(binary:match(FinalHtml, <<"<li>Item 3</li>">>) =/= nomatch),
    
    %% Verify selection works
    {ok, AllParagraphs} = lexbor_erl:select(Doc, <<"p">>),
    ?assertEqual(2, length(AllParagraphs)),
    
    {ok, ListItems} = lexbor_erl:select(Doc, <<"li">>),
    ?assertEqual(3, length(ListItems)),
    
    {ok, [HighlightP]} = lexbor_erl:select(Doc, <<".highlight">>),
    {ok, HighlightText} = lexbor_erl:get_text(Doc, HighlightP),
    ?assertEqual(<<"Second paragraph">>, HighlightText),
    
    ok = lexbor_erl:release(Doc).

%% ============================================================================
%% Streaming Parser Tests
%% ============================================================================

test_stream_basic(_Config) ->
    %% Stream a simple document in one chunk
    {ok, Session} = lexbor_erl:parse_stream_begin(),
    ?assert(is_integer(Session)),
    
    ok = lexbor_erl:parse_stream_chunk(Session, <<"<html><body><p>Hello World</p></body></html>">>),
    
    {ok, Doc} = lexbor_erl:parse_stream_end(Session),
    ?assert(is_integer(Doc)),
    
    %% Verify document is queryable
    {ok, Nodes} = lexbor_erl:select(Doc, <<"p">>),
    ?assertEqual(1, length(Nodes)),
    
    {ok, Html} = lexbor_erl:outer_html(Doc, hd(Nodes)),
    ?assert(binary:match(Html, <<"Hello World">>) =/= nomatch),
    
    ok = lexbor_erl:release(Doc).

test_stream_multiple_chunks(_Config) ->
    %% Stream a document in multiple chunks
    {ok, Session} = lexbor_erl:parse_stream_begin(),
    
    ok = lexbor_erl:parse_stream_chunk(Session, <<"<html><body>">>),
    ok = lexbor_erl:parse_stream_chunk(Session, <<"<h1>Title</h1>">>),
    ok = lexbor_erl:parse_stream_chunk(Session, <<"<p>Paragraph 1</p>">>),
    ok = lexbor_erl:parse_stream_chunk(Session, <<"<p>Paragraph 2</p>">>),
    ok = lexbor_erl:parse_stream_chunk(Session, <<"</body></html>">>),
    
    {ok, Doc} = lexbor_erl:parse_stream_end(Session),
    
    %% Verify all elements are present
    {ok, H1Nodes} = lexbor_erl:select(Doc, <<"h1">>),
    ?assertEqual(1, length(H1Nodes)),
    
    {ok, PNodes} = lexbor_erl:select(Doc, <<"p">>),
    ?assertEqual(2, length(PNodes)),
    
    ok = lexbor_erl:release(Doc).

test_stream_split_in_tag(_Config) ->
    %% Stream with chunk boundary in the middle of a tag
    {ok, Session} = lexbor_erl:parse_stream_begin(),
    
    %% Split "<p class='test'>" into multiple chunks
    ok = lexbor_erl:parse_stream_chunk(Session, <<"<html><body><p cla">>),
    ok = lexbor_erl:parse_stream_chunk(Session, <<"ss='te">>),
    ok = lexbor_erl:parse_stream_chunk(Session, <<"st'>Content">>),
    ok = lexbor_erl:parse_stream_chunk(Session, <<"</p></body></html>">>),
    
    {ok, Doc} = lexbor_erl:parse_stream_end(Session),
    
    %% Verify the element and attribute are correctly parsed
    {ok, [Node]} = lexbor_erl:select(Doc, <<"p.test">>),
    {ok, Class} = lexbor_erl:get_attribute(Doc, Node, <<"class">>),
    ?assertEqual(<<"test">>, Class),
    
    {ok, Text} = lexbor_erl:get_text(Doc, Node),
    ?assertEqual(<<"Content">>, Text),
    
    ok = lexbor_erl:release(Doc).

test_stream_large_document(_Config) ->
    %% Stream a large document in many small chunks
    {ok, Session} = lexbor_erl:parse_stream_begin(),
    
    ok = lexbor_erl:parse_stream_chunk(Session, <<"<html><body><ul>">>),
    
    %% Add 100 list items in separate chunks
    lists:foreach(fun(N) ->
        Chunk = iolist_to_binary(io_lib:format("<li>Item ~p</li>", [N])),
        ok = lexbor_erl:parse_stream_chunk(Session, Chunk)
    end, lists:seq(1, 100)),
    
    ok = lexbor_erl:parse_stream_chunk(Session, <<"</ul></body></html>">>),
    
    {ok, Doc} = lexbor_erl:parse_stream_end(Session),
    
    %% Verify all items are present
    {ok, Items} = lexbor_erl:select(Doc, <<"li">>),
    ?assertEqual(100, length(Items)),
    
    %% Verify first and last items
    {ok, FirstHtml} = lexbor_erl:outer_html(Doc, hd(Items)),
    ?assert(binary:match(FirstHtml, <<"Item 1">>) =/= nomatch),
    
    {ok, LastHtml} = lexbor_erl:outer_html(Doc, lists:last(Items)),
    ?assert(binary:match(LastHtml, <<"Item 100">>) =/= nomatch),
    
    ok = lexbor_erl:release(Doc).

test_stream_with_queries(_Config) ->
    %% Test that streaming produces the same result as regular parsing
    Html = <<"<html><body><div class='container'><p id='p1'>First</p><p id='p2'>Second</p></div></body></html>">>,
    
    %% Parse normally
    {ok, NormalDoc} = lexbor_erl:parse(Html),
    {ok, NormalNodes} = lexbor_erl:select(NormalDoc, <<"p">>),
    {ok, NormalHtml} = lexbor_erl:serialize(NormalDoc),
    
    %% Parse via streaming (split into 3 chunks)
    {ok, Session} = lexbor_erl:parse_stream_begin(),
    Size = byte_size(Html),
    Chunk1Size = Size div 3,
    Chunk2Size = Size div 3,
    
    <<Chunk1:Chunk1Size/binary, Chunk2:Chunk2Size/binary, Chunk3/binary>> = Html,
    
    ok = lexbor_erl:parse_stream_chunk(Session, Chunk1),
    ok = lexbor_erl:parse_stream_chunk(Session, Chunk2),
    ok = lexbor_erl:parse_stream_chunk(Session, Chunk3),
    
    {ok, StreamDoc} = lexbor_erl:parse_stream_end(Session),
    {ok, StreamNodes} = lexbor_erl:select(StreamDoc, <<"p">>),
    {ok, StreamHtml} = lexbor_erl:serialize(StreamDoc),
    
    %% Results should be equivalent
    ?assertEqual(length(NormalNodes), length(StreamNodes)),
    ?assertEqual(NormalHtml, StreamHtml),
    
    ok = lexbor_erl:release(NormalDoc),
    ok = lexbor_erl:release(StreamDoc).

test_stream_invalid_session(_Config) ->
    %% Test error handling for invalid session operations
    
    %% Chunk with invalid session
    Result1 = lexbor_erl:parse_stream_chunk(999999, <<"<p>Test</p>">>),
    ?assertMatch({error, _}, Result1),
    
    %% End with invalid session
    Result2 = lexbor_erl:parse_stream_end(999999),
    ?assertMatch({error, _}, Result2),
    
    %% Try to use session after it's been ended
    {ok, Session} = lexbor_erl:parse_stream_begin(),
    ok = lexbor_erl:parse_stream_chunk(Session, <<"<p>Test</p>">>),
    {ok, Doc} = lexbor_erl:parse_stream_end(Session),
    
    %% Session is now closed, should fail
    Result3 = lexbor_erl:parse_stream_chunk(Session, <<"<p>More</p>">>),
    ?assertMatch({error, _}, Result3),
    
    ok = lexbor_erl:release(Doc).

test_stream_parallel(_Config) ->
    %% Test that multiple streaming sessions can run in parallel
    Parent = self(),
    
    %% Start multiple streaming sessions in parallel
    Pids = [spawn_link(fun() ->
        {ok, Session} = lexbor_erl:parse_stream_begin(),
        
        %% Each session streams its own document
        lists:foreach(fun(N) ->
            Chunk = iolist_to_binary(io_lib:format("<p>Worker ~p Item ~p</p>", [WorkerId, N])),
            ok = lexbor_erl:parse_stream_chunk(Session, Chunk)
        end, lists:seq(1, 10)),
        
        {ok, Doc} = lexbor_erl:parse_stream_end(Session),
        
        %% Verify the document
        {ok, Nodes} = lexbor_erl:select(Doc, <<"p">>),
        ?assertEqual(10, length(Nodes)),
        
        ok = lexbor_erl:release(Doc),
        Parent ! {done, WorkerId}
    end) || WorkerId <- lists:seq(1, 5)],
    
    %% Wait for all workers to complete
    lists:foreach(fun(WorkerId) ->
        receive
            {done, WorkerId} -> ok
        after 5000 ->
            ct:fail("Worker ~p timed out", [WorkerId])
        end
    end, lists:seq(1, 5)),
    
    %% Ensure all workers terminated normally
    lists:foreach(fun(Pid) ->
        ?assert(not is_process_alive(Pid))
    end, Pids).
