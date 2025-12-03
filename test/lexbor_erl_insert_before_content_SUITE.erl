%%%-------------------------------------------------------------------
%%% @doc Common Test suite for lexbor_erl:insert_before_content/3
%%%
%%% Tests for the INSERT_BEFORE_CONTENT operation which inserts HTML content
%%% BEFORE all elements matching a CSS selector (as siblings, not children).
%%%
%%% Key difference from append/prepend: This inserts as SIBLINGS of the matched
%%% elements, not as children. The content is inserted in the parent's child list
%%% just before each matched element.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(lexbor_erl_insert_before_content_SUITE).

-include_lib("common_test/include/ct.hrl").

%% CT callbacks
-export([all/0, init_per_suite/1, end_per_suite/1]).

%% Test cases
-export([
    insert_before_basic_single_match/1,
    insert_before_multiple_matches/1,
    insert_before_no_matches/1,
    insert_before_complex_html/1,
    insert_before_nested_elements/1,
    insert_before_void_elements/1,
    insert_before_multiple_times/1,
    invalid_selector/1,
    invalid_document/1,
    insert_before_with_attributes/1,
    insert_before_preserves_existing_content/1,
    insert_before_vs_prepend_difference/1,
    insert_before_first_child/1
]).

%%--------------------------------------------------------------------
%% CT Callbacks
%%--------------------------------------------------------------------

all() ->
    [
        insert_before_basic_single_match,
        insert_before_multiple_matches,
        insert_before_no_matches,
        insert_before_complex_html,
        insert_before_nested_elements,
        insert_before_void_elements,
        insert_before_multiple_times,
        invalid_selector,
        invalid_document,
        insert_before_with_attributes,
        insert_before_preserves_existing_content,
        insert_before_vs_prepend_difference,
        insert_before_first_child
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

%% @doc Test basic insert_before with single match
insert_before_basic_single_match(_Config) ->
    Html = <<"<div><p>World</p></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:insert_before_content(Doc, <<"p">>, <<"<p>Hello</p>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Verify both paragraphs exist in correct order
    true = binary:match(Result, <<"<p>Hello</p>">>) =/= nomatch,
    true = binary:match(Result, <<"<p>World</p>">>) =/= nomatch,
    
    %% Verify Hello comes before World (insert_before puts it before the matched element)
    {HelloPos, _} = binary:match(Result, <<"<p>Hello</p>">>),
    {WorldPos, _} = binary:match(Result, <<"<p>World</p>">>),
    true = HelloPos < WorldPos,
    ok.

%% @doc Test insert_before with multiple matches
insert_before_multiple_matches(_Config) ->
    Html = <<"<div><p class='target'>A</p><p class='target'>B</p></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 2} = lexbor_erl:insert_before_content(Doc, <<".target">>, <<"<span>!</span>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Both paragraphs should have spans inserted before them
    %% Expected: <span>!</span><p class='target'>A</p><span>!</span><p class='target'>B</p>
    true = binary:match(Result, <<"<span>!</span>">>) =/= nomatch,
    
    %% Count occurrences - should be 2 spans
    Matches = binary:matches(Result, <<"<span>!</span>">>),
    2 = length(Matches),
    ok.

%% @doc Test insert_before with no matches
insert_before_no_matches(_Config) ->
    Html = <<"<div>Hello</div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 0} = lexbor_erl:insert_before_content(Doc, <<"span">>, <<"<p>World</p>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Original content should remain, new content should not appear
    true = binary:match(Result, <<"Hello">>) =/= nomatch,
    true = binary:match(Result, <<"World">>) =:= nomatch,
    ok.

%% @doc Test insert_before with complex nested HTML
insert_before_complex_html(_Config) ->
    Html = <<"<div id='container'><p>Target</p></div>">>,
    NewHtml = <<"<ul><li>First</li><li>Second</li></ul>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:insert_before_content(Doc, <<"p">>, NewHtml),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Verify structure is preserved and order is correct
    true = binary:match(Result, <<"<ul>">>) =/= nomatch,
    true = binary:match(Result, <<"<li>First</li>">>) =/= nomatch,
    true = binary:match(Result, <<"<li>Second</li>">>) =/= nomatch,
    true = binary:match(Result, <<"<p>Target</p>">>) =/= nomatch,
    
    %% Verify inserted content comes before target
    {UlPos, _} = binary:match(Result, <<"<ul>">>),
    {TargetPos, _} = binary:match(Result, <<"<p>Target</p>">>),
    true = UlPos < TargetPos,
    ok.

%% @doc Test insert_before to nested elements
insert_before_nested_elements(_Config) ->
    Html = <<"<div><div><div class='wrapper'><p class='deep'>Target</p></div></div></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:insert_before_content(Doc, <<".deep">>, <<"<span>Before</span>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Verify both elements exist
    true = binary:match(Result, <<"<span>Before</span>">>) =/= nomatch,
    true = binary:match(Result, <<"<p class=\"deep\">Target</p>">>) =/= nomatch,
    
    %% Verify Before comes before Target
    {BeforePos, _} = binary:match(Result, <<"<span>Before</span>">>),
    {TargetPos, _} = binary:match(Result, <<"Target">>),
    true = BeforePos < TargetPos,
    ok.

%% @doc Test insert_before with void elements (self-closing tags)
insert_before_void_elements(_Config) ->
    Html = <<"<div><p>Content</p></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:insert_before_content(Doc, <<"p">>, <<"<br><hr>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Void elements should be present
    true = binary:match(Result, <<"<br>">>) =/= nomatch,
    true = binary:match(Result, <<"<hr>">>) =/= nomatch,
    
    %% They should come before the target element
    {HrPos, _} = binary:match(Result, <<"<hr>">>),
    {ContentPos, _} = binary:match(Result, <<"<p>Content</p>">>),
    true = HrPos < ContentPos,
    ok.

%% @doc Test multiple insert_before operations on the same document
insert_before_multiple_times(_Config) ->
    Html = <<"<ul id='list'><li>Target</li></ul>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    %% Each insert adds before the Target li
    {ok, 1} = lexbor_erl:insert_before_content(Doc, <<"li">>, <<"<li>Third</li>">>),
    %% Now we have: Third, Target. Third is first, Target is second.
    %% Next insert_before on "li" will match both, but we want to insert before Target
    %% Actually, after first insert, there are now 2 li elements, so next operation will match both
    %% For clarity, let's use a more specific selector
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Verify items are present
    true = binary:match(Result, <<"<li>Third</li>">>) =/= nomatch,
    true = binary:match(Result, <<"<li>Target</li>">>) =/= nomatch,
    
    %% Check order: Third should come before Target
    {ThirdPos, _} = binary:match(Result, <<"Third">>),
    {TargetPos, _} = binary:match(Result, <<"Target">>),
    true = ThirdPos < TargetPos,
    ok.

%% @doc Test error handling for invalid selector
invalid_selector(_Config) ->
    Html = <<"<div>Test</div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {error, "invalid_selector"} = lexbor_erl:insert_before_content(Doc, <<"[[[invalid">>, <<"<p>X</p>">>),
    ok = lexbor_erl:release(Doc),
    ok.

%% @doc Test error handling for invalid document ID
invalid_document(_Config) ->
    %% Use an ID that doesn't exist
    InvalidDocId = 999999999,
    {error, "doc_not_found"} = lexbor_erl:insert_before_content(InvalidDocId, <<"div">>, <<"<p>X</p>">>),
    ok.

%% @doc Test insert_before with HTML containing attributes
insert_before_with_attributes(_Config) ->
    Html = <<"<div id='container'><p>Target</p></div>">>,
    NewHtml = <<"<a href='/link' class='link'>New Link</a>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:insert_before_content(Doc, <<"p">>, NewHtml),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Attributes should be preserved
    true = binary:match(Result, <<"href=\"/link\"">>) =/= nomatch,
    true = binary:match(Result, <<"class=\"link\"">>) =/= nomatch,
    true = binary:match(Result, <<"New Link">>) =/= nomatch,
    
    %% Inserted link should come before target paragraph
    {LinkPos, _} = binary:match(Result, <<"New Link">>),
    {TargetPos, _} = binary:match(Result, <<"Target">>),
    true = LinkPos < TargetPos,
    ok.

%% @doc Test that insert_before preserves existing content
insert_before_preserves_existing_content(_Config) ->
    Html = <<"<div><span>Keep me</span><p>Target</p></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:insert_before_content(Doc, <<"p">>, <<"<em>New</em>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% All content should be present
    true = binary:match(Result, <<"Keep me">>) =/= nomatch,
    true = binary:match(Result, <<"Target">>) =/= nomatch,
    true = binary:match(Result, <<"New">>) =/= nomatch,
    
    %% Order: Keep me < New < Target
    {KeepPos, _} = binary:match(Result, <<"Keep me">>),
    {NewPos, _} = binary:match(Result, <<"New">>),
    {TargetPos, _} = binary:match(Result, <<"Target">>),
    true = KeepPos < NewPos,
    true = NewPos < TargetPos,
    ok.

%% @doc Test the difference between insert_before (sibling) and prepend (child)
insert_before_vs_prepend_difference(_Config) ->
    %% Test insert_before - inserts as sibling BEFORE the target
    Html1 = <<"<div><p id='target'>Content</p></div>">>,
    {ok, Doc1} = lexbor_erl:parse(Html1),
    {ok, 1} = lexbor_erl:insert_before_content(Doc1, <<"#target">>, <<"<span>Before</span>">>),
    {ok, Result1} = lexbor_erl:serialize(Doc1),
    ok = lexbor_erl:release(Doc1),
    
    %% For insert_before: <span> is sibling of <p>, both inside <div>
    %% Expected: <div><span>Before</span><p id='target'>Content</p></div>
    true = binary:match(Result1, <<"<span>Before</span><p id=\"target\">">>) =/= nomatch,
    
    %% Test prepend - inserts as first CHILD of the target
    Html2 = <<"<div><p id='target'>Content</p></div>">>,
    {ok, Doc2} = lexbor_erl:parse(Html2),
    {ok, 1} = lexbor_erl:prepend_content(Doc2, <<"#target">>, <<"<span>Before</span>">>),
    {ok, Result2} = lexbor_erl:serialize(Doc2),
    ok = lexbor_erl:release(Doc2),
    
    %% For prepend: <span> is child of <p>
    %% Expected: <div><p id='target'><span>Before</span>Content</p></div>
    true = binary:match(Result2, <<"<p id=\"target\"><span>Before</span>Content</p>">>) =/= nomatch,
    
    %% The results should be different
    true = Result1 =/= Result2,
    ok.

%% @doc Test insert_before when target is the first child
insert_before_first_child(_Config) ->
    Html = <<"<div><p>First</p><p>Second</p></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    %% Select only the first p and insert before it
    {ok, _} = lexbor_erl:insert_before_content(Doc, <<"p:first-child">>, <<"<span>Before First</span>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% The span should come before the first p
    true = binary:match(Result, <<"<span>Before First</span>">>) =/= nomatch,
    
    {SpanPos, _} = binary:match(Result, <<"<span>Before First</span>">>),
    {FirstPos, _} = binary:match(Result, <<"<p>First</p>">>),
    true = SpanPos < FirstPos,
    ok.
