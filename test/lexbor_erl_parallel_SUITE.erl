-module(lexbor_erl_parallel_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).

-export([
    test_parallel_parse_serialize/1,
    test_parallel_stateful_ops/1,
    test_parallel_mixed_ops/1,
    test_worker_isolation/1
]).

all() ->
    [
        {group, parallel}
    ].

groups() ->
    [
        {parallel, [parallel], [
            test_parallel_parse_serialize,
            test_parallel_stateful_ops,
            test_parallel_mixed_ops,
            test_worker_isolation
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
