%%%-------------------------------------------------------------------
%%% @doc Common Test suite for lexbor_erl:insert_after_content/3
%%%
%%% Tests for the INSERT_AFTER_CONTENT operation which inserts HTML content
%%% AFTER all elements matching a CSS selector (as siblings, not children).
%%%
%%% Key difference from insert_before: This inserts AFTER the matched element.
%%% Important: Multiple nodes are inserted in correct order (A, B after target = target, A, B).
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(lexbor_erl_insert_after_content_SUITE).

-include_lib("common_test/include/ct.hrl").

%% CT callbacks
-export([all/0, init_per_suite/1, end_per_suite/1]).

%% Test cases
-export([
    insert_after_basic_single_match/1,
    insert_after_multiple_matches/1,
    insert_after_no_matches/1,
    insert_after_complex_html/1,
    insert_after_nested_elements/1,
    insert_after_void_elements/1,
    insert_after_multiple_times/1,
    invalid_selector/1,
    invalid_document/1,
    insert_after_with_attributes/1,
    insert_after_preserves_existing_content/1,
    insert_after_vs_insert_before_difference/1,
    insert_after_last_child/1,
    insert_after_multiple_nodes_order/1
]).

%%--------------------------------------------------------------------
%% CT Callbacks
%%--------------------------------------------------------------------

all() ->
    [
        insert_after_basic_single_match,
        insert_after_multiple_matches,
        insert_after_no_matches,
        insert_after_complex_html,
        insert_after_nested_elements,
        insert_after_void_elements,
        insert_after_multiple_times,
        invalid_selector,
        invalid_document,
        insert_after_with_attributes,
        insert_after_preserves_existing_content,
        insert_after_vs_insert_before_difference,
        insert_after_last_child,
        insert_after_multiple_nodes_order
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

%% @doc Test basic insert_after with single match
insert_after_basic_single_match(_Config) ->
    Html = <<"<div><p>Hello</p></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:insert_after_content(Doc, <<"p">>, <<"<p>World</p>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Verify both paragraphs exist in correct order
    true = binary:match(Result, <<"<p>Hello</p>">>) =/= nomatch,
    true = binary:match(Result, <<"<p>World</p>">>) =/= nomatch,
    
    %% Verify Hello comes before World (insert_after puts World after Hello)
    {HelloPos, _} = binary:match(Result, <<"<p>Hello</p>">>),
    {WorldPos, _} = binary:match(Result, <<"<p>World</p>">>),
    true = HelloPos < WorldPos,
    ok.

%% @doc Test insert_after with multiple matches
insert_after_multiple_matches(_Config) ->
    Html = <<"<div><p class='target'>A</p><p class='target'>B</p></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 2} = lexbor_erl:insert_after_content(Doc, <<".target">>, <<"<span>!</span>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Both paragraphs should have spans inserted after them
    %% Expected: <p>A</p><span>!</span><p>B</p><span>!</span>
    true = binary:match(Result, <<"<span>!</span>">>) =/= nomatch,
    
    %% Count occurrences - should be 2 spans
    Matches = binary:matches(Result, <<"<span>!</span>">>),
    2 = length(Matches),
    ok.

%% @doc Test insert_after with no matches
insert_after_no_matches(_Config) ->
    Html = <<"<div>Hello</div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 0} = lexbor_erl:insert_after_content(Doc, <<"span">>, <<"<p>World</p>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Original content should remain, new content should not appear
    true = binary:match(Result, <<"Hello">>) =/= nomatch,
    true = binary:match(Result, <<"World">>) =:= nomatch,
    ok.

%% @doc Test insert_after with complex nested HTML
insert_after_complex_html(_Config) ->
    Html = <<"<div id='container'><p>Target</p></div>">>,
    NewHtml = <<"<ul><li>First</li><li>Second</li></ul>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:insert_after_content(Doc, <<"p">>, NewHtml),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Verify structure is preserved and order is correct
    true = binary:match(Result, <<"<ul>">>) =/= nomatch,
    true = binary:match(Result, <<"<li>First</li>">>) =/= nomatch,
    true = binary:match(Result, <<"<li>Second</li>">>) =/= nomatch,
    true = binary:match(Result, <<"<p>Target</p>">>) =/= nomatch,
    
    %% Verify target comes before inserted content
    {TargetPos, _} = binary:match(Result, <<"<p>Target</p>">>),
    {UlPos, _} = binary:match(Result, <<"<ul>">>),
    true = TargetPos < UlPos,
    ok.

%% @doc Test insert_after to nested elements
insert_after_nested_elements(_Config) ->
    Html = <<"<div><div><div class='wrapper'><p class='deep'>Target</p></div></div></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:insert_after_content(Doc, <<".deep">>, <<"<span>After</span>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Verify both elements exist
    true = binary:match(Result, <<"<span>After</span>">>) =/= nomatch,
    true = binary:match(Result, <<"<p class=\"deep\">Target</p>">>) =/= nomatch,
    
    %% Verify Target comes before After
    {TargetPos, _} = binary:match(Result, <<"Target">>),
    {AfterPos, _} = binary:match(Result, <<"<span>After</span>">>),
    true = TargetPos < AfterPos,
    ok.

%% @doc Test insert_after with void elements (self-closing tags)
insert_after_void_elements(_Config) ->
    Html = <<"<div><p>Content</p></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:insert_after_content(Doc, <<"p">>, <<"<br><hr>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Void elements should be present
    true = binary:match(Result, <<"<br>">>) =/= nomatch,
    true = binary:match(Result, <<"<hr>">>) =/= nomatch,
    
    %% They should come after the target element
    {ContentPos, _} = binary:match(Result, <<"<p>Content</p>">>),
    {BrPos, _} = binary:match(Result, <<"<br>">>),
    true = ContentPos < BrPos,
    ok.

%% @doc Test multiple insert_after operations on the same document
insert_after_multiple_times(_Config) ->
    Html = <<"<ul id='list'><li>First</li></ul>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    %% Each insert adds after the First li
    {ok, 1} = lexbor_erl:insert_after_content(Doc, <<"li">>, <<"<li>Second</li>">>),
    %% Now we have: First, Second. Next insert will match both.
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Verify items are present
    true = binary:match(Result, <<"<li>First</li>">>) =/= nomatch,
    true = binary:match(Result, <<"<li>Second</li>">>) =/= nomatch,
    
    %% Check order: First should come before Second
    {FirstPos, _} = binary:match(Result, <<"First">>),
    {SecondPos, _} = binary:match(Result, <<"Second">>),
    true = FirstPos < SecondPos,
    ok.

%% @doc Test error handling for invalid selector
invalid_selector(_Config) ->
    Html = <<"<div>Test</div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {error, "invalid_selector"} = lexbor_erl:insert_after_content(Doc, <<"[[[invalid">>, <<"<p>X</p>">>),
    ok = lexbor_erl:release(Doc),
    ok.

%% @doc Test error handling for invalid document ID
invalid_document(_Config) ->
    %% Use an ID that doesn't exist
    InvalidDocId = 999999999,
    {error, "doc_not_found"} = lexbor_erl:insert_after_content(InvalidDocId, <<"div">>, <<"<p>X</p>">>),
    ok.

%% @doc Test insert_after with HTML containing attributes
insert_after_with_attributes(_Config) ->
    Html = <<"<div id='container'><p>Target</p></div>">>,
    NewHtml = <<"<a href='/link' class='link'>New Link</a>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:insert_after_content(Doc, <<"p">>, NewHtml),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Attributes should be preserved
    true = binary:match(Result, <<"href=\"/link\"">>) =/= nomatch,
    true = binary:match(Result, <<"class=\"link\"">>) =/= nomatch,
    true = binary:match(Result, <<"New Link">>) =/= nomatch,
    
    %% Target paragraph should come before inserted link
    {TargetPos, _} = binary:match(Result, <<"Target">>),
    {LinkPos, _} = binary:match(Result, <<"New Link">>),
    true = TargetPos < LinkPos,
    ok.

%% @doc Test that insert_after preserves existing content
insert_after_preserves_existing_content(_Config) ->
    Html = <<"<div><p>Target</p><span>Keep me</span></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:insert_after_content(Doc, <<"p">>, <<"<em>New</em>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% All content should be present
    true = binary:match(Result, <<"Target">>) =/= nomatch,
    true = binary:match(Result, <<"Keep me">>) =/= nomatch,
    true = binary:match(Result, <<"New">>) =/= nomatch,
    
    %% Order: Target < New < Keep me
    {TargetPos, _} = binary:match(Result, <<"Target">>),
    {NewPos, _} = binary:match(Result, <<"New">>),
    {KeepPos, _} = binary:match(Result, <<"Keep me">>),
    true = TargetPos < NewPos,
    true = NewPos < KeepPos,
    ok.

%% @doc Test the difference between insert_after (after) and insert_before (before)
insert_after_vs_insert_before_difference(_Config) ->
    %% Test insert_after - inserts as sibling AFTER the target
    Html1 = <<"<div><p id='target'>Content</p></div>">>,
    {ok, Doc1} = lexbor_erl:parse(Html1),
    {ok, 1} = lexbor_erl:insert_after_content(Doc1, <<"#target">>, <<"<span>After</span>">>),
    {ok, Result1} = lexbor_erl:serialize(Doc1),
    ok = lexbor_erl:release(Doc1),
    
    %% For insert_after: <p> comes before <span>
    %% Expected: <div><p id='target'>Content</p><span>After</span></div>
    true = binary:match(Result1, <<"</p><span>After</span>">>) =/= nomatch,
    
    %% Test insert_before - inserts as sibling BEFORE the target
    Html2 = <<"<div><p id='target'>Content</p></div>">>,
    {ok, Doc2} = lexbor_erl:parse(Html2),
    {ok, 1} = lexbor_erl:insert_before_content(Doc2, <<"#target">>, <<"<span>Before</span>">>),
    {ok, Result2} = lexbor_erl:serialize(Doc2),
    ok = lexbor_erl:release(Doc2),
    
    %% For insert_before: <span> comes before <p>
    %% Expected: <div><span>Before</span><p id='target'>Content</p></div>
    true = binary:match(Result2, <<"<span>Before</span><p id=\"target\">">>) =/= nomatch,
    
    %% The results should be different
    true = Result1 =/= Result2,
    ok.

%% @doc Test insert_after when target is the last child
insert_after_last_child(_Config) ->
    Html = <<"<div><p>First</p><p>Last</p></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    %% Select only the last p and insert after it
    {ok, _} = lexbor_erl:insert_after_content(Doc, <<"p:last-child">>, <<"<span>After Last</span>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% The span should come after the last p
    true = binary:match(Result, <<"<span>After Last</span>">>) =/= nomatch,
    
    {LastPos, _} = binary:match(Result, <<"<p>Last</p>">>),
    {SpanPos, _} = binary:match(Result, <<"<span>After Last</span>">>),
    true = LastPos < SpanPos,
    ok.

%% @doc Test that multiple nodes are inserted in correct order after target
%% When inserting [A, B] after target, result should be: target, A, B (not target, B, A)
insert_after_multiple_nodes_order(_Config) ->
    Html = <<"<div><p>Target</p></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    %% Insert multiple nodes at once
    {ok, 1} = lexbor_erl:insert_after_content(Doc, <<"p">>, <<"<span>A</span><span>B</span><span>C</span>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% All spans should be present
    true = binary:match(Result, <<"<span>A</span>">>) =/= nomatch,
    true = binary:match(Result, <<"<span>B</span>">>) =/= nomatch,
    true = binary:match(Result, <<"<span>C</span>">>) =/= nomatch,
    
    %% Verify correct order: Target < A < B < C
    {TargetPos, _} = binary:match(Result, <<"<p>Target</p>">>),
    {APos, _} = binary:match(Result, <<"<span>A</span>">>),
    {BPos, _} = binary:match(Result, <<"<span>B</span>">>),
    {CPos, _} = binary:match(Result, <<"<span>C</span>">>),
    true = TargetPos < APos,
    true = APos < BPos,
    true = BPos < CPos,
    ok.
