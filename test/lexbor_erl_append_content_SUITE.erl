%%%-------------------------------------------------------------------
%%% @doc Common Test suite for lexbor_erl:append_content/3
%%%
%%% Tests for the APPEND_CONTENT operation which appends HTML content
%%% to all elements matching a CSS selector.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(lexbor_erl_append_content_SUITE).

-include_lib("common_test/include/ct.hrl").

%% CT callbacks
-export([all/0, init_per_suite/1, end_per_suite/1]).
%% Test cases
-export([append_basic_single_match/1, append_multiple_matches/1, append_no_matches/1,
         append_complex_html/1, append_nested_elements/1, append_void_elements/1,
         append_to_empty_element/1, append_multiple_times/1, invalid_selector/1,
         invalid_document/1, append_with_attributes/1, append_preserves_existing_content/1]).

%%--------------------------------------------------------------------
%% CT Callbacks
%%--------------------------------------------------------------------

all() ->
    [append_basic_single_match,
     append_multiple_matches,
     append_no_matches,
     append_complex_html,
     append_nested_elements,
     append_void_elements,
     append_to_empty_element,
     append_multiple_times,
     invalid_selector,
     invalid_document,
     append_with_attributes,
     append_preserves_existing_content].

init_per_suite(Config) ->
    ok = lexbor_erl:start(),
    Config.

end_per_suite(_Config) ->
    ok = lexbor_erl:stop(),
    ok.

%%--------------------------------------------------------------------
%% Test Cases
%%--------------------------------------------------------------------

%% @doc Test basic append with single match
append_basic_single_match(_Config) ->
    Html = <<"<div><p>Hello</p></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:append_content(Doc, <<"div">>, <<"<p>World</p>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),

    %% Verify both paragraphs exist in order
    true = binary:match(Result, <<"<p>Hello</p>">>) =/= nomatch,
    true = binary:match(Result, <<"<p>World</p>">>) =/= nomatch,

    %% Verify Hello comes before World
    {HelloPos, _} = binary:match(Result, <<"<p>Hello</p>">>),
    {WorldPos, _} = binary:match(Result, <<"<p>World</p>">>),
    true = HelloPos < WorldPos,
    ok.

%% @doc Test append with multiple matches
append_multiple_matches(_Config) ->
    Html = <<"<div class='target'>A</div><div class='target'>B</div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 2} = lexbor_erl:append_content(Doc, <<".target">>, <<"<span>!</span>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),

    %% Both divs should have spans appended
    true =
        binary:match(Result, <<"A</div><span>!</span>">>) =/= nomatch
        orelse binary:match(Result, <<"A<span>!</span></div>">>) =/= nomatch,
    true =
        binary:match(Result, <<"B</div><span>!</span>">>) =/= nomatch
        orelse binary:match(Result, <<"B<span>!</span></div>">>) =/= nomatch,
    ok.

%% @doc Test append with no matches
append_no_matches(_Config) ->
    Html = <<"<div>Hello</div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 0} = lexbor_erl:append_content(Doc, <<"span">>, <<"<p>World</p>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),

    %% Original content should remain, new content should not appear
    true = binary:match(Result, <<"Hello">>) =/= nomatch,
    true = binary:match(Result, <<"World">>) =:= nomatch,
    ok.

%% @doc Test append with complex nested HTML
append_complex_html(_Config) ->
    Html = <<"<div id='container'></div>">>,
    NewHtml = <<"<ul><li>First</li><li>Second</li></ul>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:append_content(Doc, <<"#container">>, NewHtml),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),

    %% Verify structure is preserved
    true = binary:match(Result, <<"<ul>">>) =/= nomatch,
    true = binary:match(Result, <<"<li>First</li>">>) =/= nomatch,
    true = binary:match(Result, <<"<li>Second</li>">>) =/= nomatch,
    ok.

%% @doc Test append to nested elements
append_nested_elements(_Config) ->
    Html = <<"<div><div><div class='deep'></div></div></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:append_content(Doc, <<".deep">>, <<"<p>Content</p>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),

    true = binary:match(Result, <<"<p>Content</p>">>) =/= nomatch,
    ok.

%% @doc Test append with void elements (self-closing tags)
append_void_elements(_Config) ->
    Html = <<"<div></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:append_content(Doc, <<"div">>, <<"<br><hr>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),

    %% Void elements should be present
    true = binary:match(Result, <<"<br>">>) =/= nomatch,
    true = binary:match(Result, <<"<hr>">>) =/= nomatch,
    ok.

%% @doc Test append to empty element
append_to_empty_element(_Config) ->
    Html = <<"<div id='empty'></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:append_content(Doc, <<"#empty">>, <<"<p>Now has content</p>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),

    true = binary:match(Result, <<"Now has content">>) =/= nomatch,
    ok.

%% @doc Test multiple append operations on the same document
append_multiple_times(_Config) ->
    Html = <<"<ul id='list'></ul>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:append_content(Doc, <<"#list">>, <<"<li>First</li>">>),
    {ok, 1} = lexbor_erl:append_content(Doc, <<"#list">>, <<"<li>Second</li>">>),
    {ok, 1} = lexbor_erl:append_content(Doc, <<"#list">>, <<"<li>Third</li>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),

    %% All three items should be present in order
    true = binary:match(Result, <<"<li>First</li>">>) =/= nomatch,
    true = binary:match(Result, <<"<li>Second</li>">>) =/= nomatch,
    true = binary:match(Result, <<"<li>Third</li>">>) =/= nomatch,

    %% Check order
    {FirstPos, _} = binary:match(Result, <<"First">>),
    {SecondPos, _} = binary:match(Result, <<"Second">>),
    {ThirdPos, _} = binary:match(Result, <<"Third">>),
    true = FirstPos < SecondPos,
    true = SecondPos < ThirdPos,
    ok.

%% @doc Test error handling for invalid selector
invalid_selector(_Config) ->
    Html = <<"<div>Test</div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {error, "invalid_selector"} =
        lexbor_erl:append_content(Doc, <<"[[[invalid">>, <<"<p>X</p>">>),
    ok = lexbor_erl:release(Doc),
    ok.

%% @doc Test error handling for invalid document ID
invalid_document(_Config) ->
    %% Use an ID that doesn't exist
    InvalidDocId = 999999999,
    {error, "doc_not_found"} =
        lexbor_erl:append_content(InvalidDocId, <<"div">>, <<"<p>X</p>">>),
    ok.

%% @doc Test append with HTML containing attributes
append_with_attributes(_Config) ->
    Html = <<"<div id='container'></div>">>,
    NewHtml = <<"<a href='/link' class='link'>Click me</a>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:append_content(Doc, <<"#container">>, NewHtml),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),

    %% Attributes should be preserved
    true = binary:match(Result, <<"href=\"/link\"">>) =/= nomatch,
    true = binary:match(Result, <<"class=\"link\"">>) =/= nomatch,
    true = binary:match(Result, <<"Click me">>) =/= nomatch,
    ok.

%% @doc Test that append preserves existing content
append_preserves_existing_content(_Config) ->
    Html = <<"<div><p>Existing</p></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:append_content(Doc, <<"div">>, <<"<p>New</p>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),

    %% Both existing and new content should be present
    true = binary:match(Result, <<"Existing">>) =/= nomatch,
    true = binary:match(Result, <<"New">>) =/= nomatch,

    %% Existing should come before new
    {ExistingPos, _} = binary:match(Result, <<"Existing">>),
    {NewPos, _} = binary:match(Result, <<"New">>),
    true = ExistingPos < NewPos,
    ok.
