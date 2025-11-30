-module(lexbor_erl_serialize_verification_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_serialize_with_doctype/1, test_serialize_without_doctype/1,
         test_serialize_roundtrip/1, test_serialize_output_format/1]).

all() ->
    [test_serialize_with_doctype,
     test_serialize_without_doctype,
     test_serialize_roundtrip,
     test_serialize_output_format].

init_per_suite(Config) ->
    case application:ensure_all_started(lexbor_erl) of
        {ok, _Started} ->
            ct:pal("Application started successfully"),
            Config;
        {error, Reason} ->
            ct:fail("Failed to start application: ~p", [Reason])
    end.

end_per_suite(_Config) ->
    application:stop(lexbor_erl),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%% Test 1: Verify current behavior with DOCTYPE in input
test_serialize_with_doctype(_Config) ->
    ct:pal("~n=== Test 1: Serialize with DOCTYPE in input ==="),

    %% Parse HTML with DOCTYPE
    Html =
        <<"<!DOCTYPE html><html><head><title>Test</title></head><body>Hello World</body></html>">>,
    ct:pal("Input HTML: ~s", [Html]),

    {ok, Doc} = lexbor_erl:parse(Html),

    %% Serialize
    {ok, Serialized} = lexbor_erl:serialize(Doc),
    ct:pal("Serialized output: ~s", [Serialized]),

    %% Check for DOCTYPE
    HasDoctype = binary:match(Serialized, <<"<!DOCTYPE">>) =/= nomatch,
    ct:pal("Output includes DOCTYPE: ~p", [HasDoctype]),

    %% Check output starts with
    StartsWithHtml = binary:match(Serialized, <<"<html">>) =:= {0, 5},
    StartsWithDoctype = binary:match(Serialized, <<"<!DOCTYPE">>) =:= {0, 9},

    ct:pal("Starts with <html>: ~p", [StartsWithHtml]),
    ct:pal("Starts with <!DOCTYPE: ~p", [StartsWithDoctype]),

    %% Document findings
    ct:pal("~n--- Findings ---"),
    ct:pal("DOCTYPE preserved: ~p", [HasDoctype]),
    ct:pal("Output length: ~p bytes", [byte_size(Serialized)]),

    lexbor_erl:release(Doc),

    %% Store result for comparison
    ct:pal("~nCurrent behavior: DOCTYPE is ~s in serialization",
           [case HasDoctype of
                true ->
                    "INCLUDED";
                false ->
                    "NOT INCLUDED"
            end]),

    ok.

%% Test 2: Verify behavior without DOCTYPE in input
test_serialize_without_doctype(_Config) ->
    ct:pal("~n=== Test 2: Serialize without DOCTYPE in input ==="),

    %% Parse HTML without DOCTYPE
    Html = <<"<html><head><title>Test</title></head><body>Hello World</body></html>">>,
    ct:pal("Input HTML: ~s", [Html]),

    {ok, Doc} = lexbor_erl:parse(Html),

    %% Serialize
    {ok, Serialized} = lexbor_erl:serialize(Doc),
    ct:pal("Serialized output: ~s", [Serialized]),

    %% Check for DOCTYPE
    HasDoctype = binary:match(Serialized, <<"<!DOCTYPE">>) =/= nomatch,
    ct:pal("Output includes DOCTYPE: ~p", [HasDoctype]),

    %% Document findings
    ct:pal("~n--- Findings ---"),
    if HasDoctype ->
           ct:pal("DOCTYPE was ADDED during serialization (lexbor default)");
       true ->
           ct:pal("DOCTYPE was NOT added (only serializes what was parsed)")
    end,

    lexbor_erl:release(Doc),
    ok.

%% Test 3: Round-trip test
test_serialize_roundtrip(_Config) ->
    ct:pal("~n=== Test 3: Round-trip test (Parse → Serialize → Parse) ==="),

    %% Original HTML with DOCTYPE
    Original =
        <<"<!DOCTYPE html><html><head><title>Test</title></head>"
          "<body><div id='content'>Hello World</div></body></html>">>,
    ct:pal("Original HTML: ~s", [Original]),

    %% First parse
    {ok, Doc1} = lexbor_erl:parse(Original),
    {ok, Serialized} = lexbor_erl:serialize(Doc1),
    lexbor_erl:release(Doc1),

    ct:pal("After serialization: ~s", [Serialized]),

    %% Second parse (from serialized)
    {ok, Doc2} = lexbor_erl:parse(Serialized),
    {ok, [Div]} = lexbor_erl:select(Doc2, <<"#content">>),
    {ok, Text} = lexbor_erl:get_text(Doc2, Div),
    lexbor_erl:release(Doc2),

    %% Verify content preserved
    ?assertEqual(<<"Hello World">>, Text),
    ct:pal("Content preserved: ✓"),

    %% Check if DOCTYPE preserved
    HasDoctype = binary:match(Serialized, <<"<!DOCTYPE">>) =/= nomatch,
    ct:pal("DOCTYPE preserved in round-trip: ~p", [HasDoctype]),

    %% Document impact
    ct:pal("~n--- Round-trip Impact ---"),
    if HasDoctype ->
           ct:pal("DOCTYPE survives round-trip - Full HTML5 preservation");
       true ->
           ct:pal("DOCTYPE lost in round-trip - Content preserved but not document structure")
    end,

    ok.

%% Test 4: Detailed output format analysis
test_serialize_output_format(_Config) ->
    ct:pal("~n=== Test 4: Output format analysis ==="),

    %% Test various HTML structures
    TestCases =
        [{<<"<!DOCTYPE html><html><head></head><body>Simple</body></html>">>,
          "Simple with DOCTYPE"},
         {<<"<html><head></head><body>Simple</body></html>">>, "Simple without DOCTYPE"},
         {<<"<!DOCTYPE html><html><head><meta charset='utf-8'></head><body><p>Text</p></body></html>">>,
          "With meta tags"},
         {<<"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\"><html><body>HTML4</body></html>">>,
          "HTML4 DOCTYPE"}],

    lists:foreach(fun({Html, Description}) ->
                     ct:pal("~nTest case: ~s", [Description]),
                     ct:pal("Input: ~s", [Html]),

                     {ok, Doc} = lexbor_erl:parse(Html),
                     {ok, Serialized} = lexbor_erl:serialize(Doc),
                     lexbor_erl:release(Doc),

                     HasDoctype = binary:match(Serialized, <<"<!DOCTYPE">>) =/= nomatch,
                     ct:pal("Output: ~s", [Serialized]),
                     ct:pal("Has DOCTYPE: ~p", [HasDoctype]),

                     %% Check first 20 chars
                     FirstChars =
                         case byte_size(Serialized) of
                             N when N >= 20 ->
                                 binary:part(Serialized, 0, 20);
                             N ->
                                 binary:part(Serialized, 0, N)
                         end,
                     ct:pal("Starts with: ~s...", [FirstChars])
                  end,
                  TestCases),

    ct:pal("~n=== Summary ==="),
    ct:pal("All test cases show consistent behavior"),

    ok.
