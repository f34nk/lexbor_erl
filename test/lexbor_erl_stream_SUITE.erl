-module(lexbor_erl_stream_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([test_stream_basic/1, test_stream_multiple_chunks/1, test_stream_split_in_tag/1,
         test_stream_large_document/1, test_stream_with_queries/1, test_stream_invalid_session/1,
         test_stream_parallel/1]).

all() ->
    [{group, streaming}].

groups() ->
    [{streaming,
      [sequence],
      [test_stream_basic,
       test_stream_multiple_chunks,
       test_stream_split_in_tag,
       test_stream_large_document,
       test_stream_with_queries,
       test_stream_invalid_session,
       test_stream_parallel]}].

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

%% ============================================================================
%% Streaming Parser Tests
%% ============================================================================

test_stream_basic(_Config) ->
    %% Stream a simple document in one chunk
    {ok, Session} = lexbor_erl:parse_stream_begin(),
    ?assert(is_integer(Session)),

    ok =
        lexbor_erl:parse_stream_chunk(Session,
                                      <<"<html><body><p>Hello World</p></body></html>">>),

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
                  end,
                  lists:seq(1, 100)),

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
    Html =
        <<"<html><body><div class='container'><p id='p1'>First</p><p id='p2'>Second</p></div></body></html>">>,

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
    Pids =
        [spawn_link(fun() ->
                       {ok, Session} = lexbor_erl:parse_stream_begin(),

                       %% Each session streams its own document
                       lists:foreach(fun(N) ->
                                        Chunk =
                                            iolist_to_binary(io_lib:format("<p>Worker ~p Item ~p</p>",
                                                                           [WorkerId, N])),
                                        ok = lexbor_erl:parse_stream_chunk(Session, Chunk)
                                     end,
                                     lists:seq(1, 10)),

                       {ok, Doc} = lexbor_erl:parse_stream_end(Session),

                       %% Verify the document
                       {ok, Nodes} = lexbor_erl:select(Doc, <<"p">>),
                       ?assertEqual(10, length(Nodes)),

                       ok = lexbor_erl:release(Doc),
                       Parent ! {done, WorkerId}
                    end)
         || WorkerId <- lists:seq(1, 5)],

    %% Wait for all workers to complete
    lists:foreach(fun(WorkerId) ->
                     receive
                         {done, WorkerId} ->
                             ok
                     after 5000 ->
                         ct:fail("Worker ~p timed out", [WorkerId])
                     end
                  end,
                  lists:seq(1, 5)),

    %% Ensure all workers terminated normally
    lists:foreach(fun(Pid) -> ?assert(not is_process_alive(Pid)) end, Pids).
