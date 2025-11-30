%%%-------------------------------------------------------------------
%%% @doc Common Test suite for lexbor_erl:prepend_content/3
%%%
%%% Tests for the PREPEND_CONTENT operation which prepends HTML content
%%% to all elements matching a CSS selector (as first child).
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(lexbor_erl_prepend_content_SUITE).

-include_lib("common_test/include/ct.hrl").

%% CT callbacks
-export([all/0, init_per_suite/1, end_per_suite/1]).

%% Test cases
-export([
    prepend_basic_single_match/1,
    prepend_multiple_matches/1,
    prepend_no_matches/1,
    prepend_complex_html/1,
    prepend_nested_elements/1,
    prepend_void_elements/1,
    prepend_to_empty_element/1,
    prepend_multiple_times/1,
    invalid_selector/1,
    invalid_document/1,
    prepend_with_attributes/1,
    prepend_preserves_existing_content/1,
    prepend_vs_append_order/1
]).

%%--------------------------------------------------------------------
%% CT Callbacks
%%--------------------------------------------------------------------

all() ->
    [
        prepend_basic_single_match,
        prepend_multiple_matches,
        prepend_no_matches,
        prepend_complex_html,
        prepend_nested_elements,
        prepend_void_elements,
        prepend_to_empty_element,
        prepend_multiple_times,
        invalid_selector,
        invalid_document,
        prepend_with_attributes,
        prepend_preserves_existing_content,
        prepend_vs_append_order
    ].

init_per_suite(Config) ->
    ok = lexbor_erl:start(),
    Config.

end_per_suite(_Config) ->
    ok = lexbor_erl:stop(),
    ok.

%%--------------------------------------------------------------------
%% Test Cases
%%--------------------------------------------------------------------

%% @doc Test basic prepend with single match
prepend_basic_single_match(_Config) ->
    Html = <<"<div><p>World</p></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:prepend_content(Doc, <<"div">>, <<"<p>Hello</p>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Verify both paragraphs exist in correct order
    true = binary:match(Result, <<"<p>Hello</p>">>) =/= nomatch,
    true = binary:match(Result, <<"<p>World</p>">>) =/= nomatch,
    
    %% Verify Hello comes before World (prepend puts it first)
    {HelloPos, _} = binary:match(Result, <<"<p>Hello</p>">>),
    {WorldPos, _} = binary:match(Result, <<"<p>World</p>">>),
    true = HelloPos < WorldPos,
    ok.

%% @doc Test prepend with multiple matches
prepend_multiple_matches(_Config) ->
    Html = <<"<div class='target'>A</div><div class='target'>B</div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 2} = lexbor_erl:prepend_content(Doc, <<".target">>, <<"<span>!</span>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Both divs should have spans prepended (before content)
    true = binary:match(Result, <<"<span>!</span">>) =/= nomatch,
    
    %% Check that prepended content comes before existing content
    {SpanPos, _} = binary:match(Result, <<"<span>!</span>">>),
    {APos, _} = binary:match(Result, <<"A</div>">>),
    true = SpanPos < APos,
    ok.

%% @doc Test prepend with no matches
prepend_no_matches(_Config) ->
    Html = <<"<div>Hello</div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 0} = lexbor_erl:prepend_content(Doc, <<"span">>, <<"<p>World</p>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Original content should remain, new content should not appear
    true = binary:match(Result, <<"Hello">>) =/= nomatch,
    true = binary:match(Result, <<"World">>) =:= nomatch,
    ok.

%% @doc Test prepend with complex nested HTML
prepend_complex_html(_Config) ->
    Html = <<"<div id='container'><p>Last</p></div>">>,
    NewHtml = <<"<ul><li>First</li><li>Second</li></ul>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:prepend_content(Doc, <<"#container">>, NewHtml),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Verify structure is preserved and order is correct
    true = binary:match(Result, <<"<ul>">>) =/= nomatch,
    true = binary:match(Result, <<"<li>First</li>">>) =/= nomatch,
    true = binary:match(Result, <<"<li>Second</li>">>) =/= nomatch,
    true = binary:match(Result, <<"<p>Last</p>">>) =/= nomatch,
    
    %% Verify prepended content comes before existing content
    {UlPos, _} = binary:match(Result, <<"<ul>">>),
    {PPos, _} = binary:match(Result, <<"<p>Last</p>">>),
    true = UlPos < PPos,
    ok.

%% @doc Test prepend to nested elements
prepend_nested_elements(_Config) ->
    Html = <<"<div><div><div class='deep'><p>End</p></div></div></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:prepend_content(Doc, <<".deep">>, <<"<p>Start</p>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Verify both paragraphs exist
    true = binary:match(Result, <<"<p>Start</p>">>) =/= nomatch,
    true = binary:match(Result, <<"<p>End</p>">>) =/= nomatch,
    
    %% Verify Start comes before End
    {StartPos, _} = binary:match(Result, <<"<p>Start</p>">>),
    {EndPos, _} = binary:match(Result, <<"<p>End</p>">>),
    true = StartPos < EndPos,
    ok.

%% @doc Test prepend with void elements (self-closing tags)
prepend_void_elements(_Config) ->
    Html = <<"<div><p>Content</p></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:prepend_content(Doc, <<"div">>, <<"<br><hr>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Void elements should be present
    true = binary:match(Result, <<"<br>">>) =/= nomatch,
    true = binary:match(Result, <<"<hr>">>) =/= nomatch,
    
    %% They should come before existing content
    {BrPos, _} = binary:match(Result, <<"<br>">>),
    {ContentPos, _} = binary:match(Result, <<"<p>Content</p>">>),
    true = BrPos < ContentPos,
    ok.

%% @doc Test prepend to empty element
prepend_to_empty_element(_Config) ->
    Html = <<"<div id='empty'></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:prepend_content(Doc, <<"#empty">>, <<"<p>Now has content</p>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    true = binary:match(Result, <<"Now has content">>) =/= nomatch,
    ok.

%% @doc Test multiple prepend operations on the same document
prepend_multiple_times(_Config) ->
    Html = <<"<ul id='list'></ul>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:prepend_content(Doc, <<"#list">>, <<"<li>Third</li>">>),
    {ok, 1} = lexbor_erl:prepend_content(Doc, <<"#list">>, <<"<li>Second</li>">>),
    {ok, 1} = lexbor_erl:prepend_content(Doc, <<"#list">>, <<"<li>First</li>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% All three items should be present
    true = binary:match(Result, <<"<li>First</li>">>) =/= nomatch,
    true = binary:match(Result, <<"<li>Second</li>">>) =/= nomatch,
    true = binary:match(Result, <<"<li>Third</li>">>) =/= nomatch,
    
    %% Check order (First should come first since it was prepended last)
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
    {error, "invalid_selector"} = lexbor_erl:prepend_content(Doc, <<"[[[invalid">>, <<"<p>X</p>">>),
    ok = lexbor_erl:release(Doc),
    ok.

%% @doc Test error handling for invalid document ID
invalid_document(_Config) ->
    %% Use an ID that doesn't exist
    InvalidDocId = 999999999,
    {error, "doc_not_found"} = lexbor_erl:prepend_content(InvalidDocId, <<"div">>, <<"<p>X</p>">>),
    ok.

%% @doc Test prepend with HTML containing attributes
prepend_with_attributes(_Config) ->
    Html = <<"<div id='container'><p>Last</p></div>">>,
    NewHtml = <<"<a href='/link' class='link'>First Link</a>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:prepend_content(Doc, <<"#container">>, NewHtml),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Attributes should be preserved
    true = binary:match(Result, <<"href=\"/link\"">>) =/= nomatch,
    true = binary:match(Result, <<"class=\"link\"">>) =/= nomatch,
    true = binary:match(Result, <<"First Link">>) =/= nomatch,
    
    %% Prepended link should come before existing paragraph
    {LinkPos, _} = binary:match(Result, <<"First Link">>),
    {LastPos, _} = binary:match(Result, <<"Last">>),
    true = LinkPos < LastPos,
    ok.

%% @doc Test that prepend preserves existing content
prepend_preserves_existing_content(_Config) ->
    Html = <<"<div><p>Existing</p></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:prepend_content(Doc, <<"div">>, <<"<p>New</p>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Both existing and new content should be present
    true = binary:match(Result, <<"Existing">>) =/= nomatch,
    true = binary:match(Result, <<"New">>) =/= nomatch,
    
    %% New should come before existing (prepend)
    {NewPos, _} = binary:match(Result, <<"New">>),
    {ExistingPos, _} = binary:match(Result, <<"Existing">>),
    true = NewPos < ExistingPos,
    ok.

%% @doc Test that prepend and append have opposite order
prepend_vs_append_order(_Config) ->
    %% Test prepend
    Html1 = <<"<div><p>Middle</p></div>">>,
    {ok, Doc1} = lexbor_erl:parse(Html1),
    {ok, 1} = lexbor_erl:prepend_content(Doc1, <<"div">>, <<"<p>First</p>">>),
    {ok, 1} = lexbor_erl:append_content(Doc1, <<"div">>, <<"<p>Last</p>">>),
    {ok, Result1} = lexbor_erl:serialize(Doc1),
    ok = lexbor_erl:release(Doc1),
    
    %% Verify order: First < Middle < Last
    {FirstPos, _} = binary:match(Result1, <<"First">>),
    {MiddlePos, _} = binary:match(Result1, <<"Middle">>),
    {LastPos, _} = binary:match(Result1, <<"Last">>),
    true = FirstPos < MiddlePos,
    true = MiddlePos < LastPos,
    ok.
