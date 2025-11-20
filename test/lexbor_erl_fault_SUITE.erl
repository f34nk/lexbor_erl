-module(lexbor_erl_fault_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([test_worker_crash_recovery/1, test_worker_isolation_after_crash/1]).

all() ->
    [{group, fault_tolerance}].

groups() ->
    [{fault_tolerance,
      [sequence],
      [test_worker_crash_recovery, test_worker_isolation_after_crash]}].

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
    Doc1Final =
        case Worker1Id =:= Worker2Id of
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
    Docs =
        [begin
             Html =
                 iolist_to_binary([<<"<div id='worker">>,
                                   integer_to_list(I),
                                   <<"'>Worker ">>,
                                   integer_to_list(I),
                                   <<"</div>">>]),
             {ok, Doc} = lexbor_erl:parse(Html),
             WorkerId = (Doc bsr 56) band 16#FF,
             ct:log("Created Doc ~p on worker ~p~n", [Doc, WorkerId]),
             {I, Doc, WorkerId}
         end
         || I <- lists:seq(1, PoolSize)],

    % Verify we can access all documents
    lists:foreach(fun({I, Doc, WorkerId}) ->
                     SelectorId = iolist_to_binary([<<"#worker">>, integer_to_list(I)]),
                     {ok, Nodes} = lexbor_erl:select(Doc, SelectorId),
                     ?assertEqual(1, length(Nodes)),
                     ct:log("Worker ~p document OK~n", [WorkerId])
                  end,
                  Docs),

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
                  end,
                  Docs),

    % Verify restarted worker 1 can handle new documents
    {ok, NewDoc} = lexbor_erl:parse(<<"<div id='new'>New Doc</div>">>),
    {ok, Nodes} = lexbor_erl:select(NewDoc, <<"#new">>),
    ?assertEqual(1, length(Nodes)),
    ok = lexbor_erl:release(NewDoc),

    ok.
