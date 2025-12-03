%%%-------------------------------------------------------------------
%%% @doc Common Test suite for lexbor_erl:replace_content/3
%%%
%%% Tests for the REPLACE_CONTENT operation which replaces all elements
%%% matching a CSS selector with HTML content.
%%%
%%% Key difference from other operations: This removes the matched elements
%%% after inserting the new content. The matched elements are destroyed.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(lexbor_erl_replace_content_SUITE).

-include_lib("common_test/include/ct.hrl").

%% CT callbacks
-export([all/0, init_per_suite/1, end_per_suite/1]).

%% Test cases
-export([
    replace_basic_single_match/1,
    replace_multiple_matches/1,
    replace_no_matches/1,
    replace_complex_html/1,
    replace_nested_elements/1,
    replace_void_elements/1,
    replace_with_empty_content/1,
    replace_multiple_times/1,
    invalid_selector/1,
    invalid_document/1,
    replace_with_attributes/1,
    replace_preserves_siblings/1,
    replace_with_multiple_elements/1,
    replace_vs_set_inner_html_difference/1
]).

%%--------------------------------------------------------------------
%% CT Callbacks
%%--------------------------------------------------------------------

all() ->
    [
        replace_basic_single_match,
        replace_multiple_matches,
        replace_no_matches,
        replace_complex_html,
        replace_nested_elements,
        replace_void_elements,
        replace_with_empty_content,
        replace_multiple_times,
        invalid_selector,
        invalid_document,
        replace_with_attributes,
        replace_preserves_siblings,
        replace_with_multiple_elements,
        replace_vs_set_inner_html_difference
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

%% @doc Test basic replace with single match
replace_basic_single_match(_Config) ->
    Html = <<"<div><p>Old</p></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:replace_content(Doc, <<"p">>, <<"<span>New</span>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Original element should be gone
    true = binary:match(Result, <<"<p>Old</p>">>) =:= nomatch,
    true = binary:match(Result, <<"Old">>) =:= nomatch,
    
    %% New element should be present
    true = binary:match(Result, <<"<span>New</span>">>) =/= nomatch,
    ok.

%% @doc Test replace with multiple matches
replace_multiple_matches(_Config) ->
    Html = <<"<div><p class='target'>A</p><p class='target'>B</p></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 2} = lexbor_erl:replace_content(Doc, <<".target">>, <<"<span>X</span>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Original elements should be gone
    true = binary:match(Result, <<">A</p>">>) =:= nomatch,
    true = binary:match(Result, <<">B</p>">>) =:= nomatch,
    
    %% Should have two replacement spans
    Matches = binary:matches(Result, <<"<span>X</span>">>),
    2 = length(Matches),
    ok.

%% @doc Test replace with no matches
replace_no_matches(_Config) ->
    Html = <<"<div>Hello</div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 0} = lexbor_erl:replace_content(Doc, <<"span">>, <<"<p>World</p>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Original content should remain
    true = binary:match(Result, <<"Hello">>) =/= nomatch,
    %% New content should not appear
    true = binary:match(Result, <<"World">>) =:= nomatch,
    ok.

%% @doc Test replace with complex nested HTML
replace_complex_html(_Config) ->
    Html = <<"<div id='container'><p>Replace me</p></div>">>,
    NewHtml = <<"<ul><li>First</li><li>Second</li></ul>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:replace_content(Doc, <<"p">>, NewHtml),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Original paragraph should be gone
    true = binary:match(Result, <<"<p>Replace me</p>">>) =:= nomatch,
    
    %% New structure should be present
    true = binary:match(Result, <<"<ul>">>) =/= nomatch,
    true = binary:match(Result, <<"<li>First</li>">>) =/= nomatch,
    true = binary:match(Result, <<"<li>Second</li>">>) =/= nomatch,
    
    %% Container should still exist
    true = binary:match(Result, <<"id=\"container\"">>) =/= nomatch,
    ok.

%% @doc Test replace of nested elements
replace_nested_elements(_Config) ->
    Html = <<"<div><div><div class='wrapper'><p class='deep'>Target</p></div></div></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:replace_content(Doc, <<".deep">>, <<"<span>Replaced</span>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Original element should be gone
    true = binary:match(Result, <<"<p class=\"deep\">">>) =:= nomatch,
    true = binary:match(Result, <<"Target">>) =:= nomatch,
    
    %% Replacement should be present
    true = binary:match(Result, <<"<span>Replaced</span>">>) =/= nomatch,
    
    %% Wrapper should still exist
    true = binary:match(Result, <<"class=\"wrapper\"">>) =/= nomatch,
    ok.

%% @doc Test replace with void elements (self-closing tags)
replace_void_elements(_Config) ->
    Html = <<"<div><p>Content</p></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:replace_content(Doc, <<"p">>, <<"<br><hr>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Original paragraph should be gone
    true = binary:match(Result, <<"<p>Content</p>">>) =:= nomatch,
    
    %% Void elements should be present
    true = binary:match(Result, <<"<br>">>) =/= nomatch,
    true = binary:match(Result, <<"<hr>">>) =/= nomatch,
    ok.

%% @doc Test replace with empty content (effectively removes the element)
replace_with_empty_content(_Config) ->
    Html = <<"<div><p>Remove me</p><span>Keep me</span></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:replace_content(Doc, <<"p">>, <<"">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% The paragraph should be gone
    true = binary:match(Result, <<"<p>">>) =:= nomatch,
    true = binary:match(Result, <<"Remove me">>) =:= nomatch,
    
    %% The span should remain
    true = binary:match(Result, <<"<span>Keep me</span>">>) =/= nomatch,
    ok.

%% @doc Test multiple replace operations on the same document
replace_multiple_times(_Config) ->
    Html = <<"<div><p id='first'>First</p><p id='second'>Second</p></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    
    %% Replace first paragraph
    {ok, 1} = lexbor_erl:replace_content(Doc, <<"#first">>, <<"<span>One</span>">>),
    %% Replace second paragraph
    {ok, 1} = lexbor_erl:replace_content(Doc, <<"#second">>, <<"<span>Two</span>">>),
    
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Original paragraphs should be gone
    true = binary:match(Result, <<"<p id=\"first\">">>) =:= nomatch,
    true = binary:match(Result, <<"<p id=\"second\">">>) =:= nomatch,
    
    %% Replacements should be present
    true = binary:match(Result, <<"<span>One</span>">>) =/= nomatch,
    true = binary:match(Result, <<"<span>Two</span>">>) =/= nomatch,
    
    %% Check order: One should come before Two
    {OnePos, _} = binary:match(Result, <<"One">>),
    {TwoPos, _} = binary:match(Result, <<"Two">>),
    true = OnePos < TwoPos,
    ok.

%% @doc Test error handling for invalid selector
invalid_selector(_Config) ->
    Html = <<"<div>Test</div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {error, "invalid_selector"} = lexbor_erl:replace_content(Doc, <<"[[[invalid">>, <<"<p>X</p>">>),
    ok = lexbor_erl:release(Doc),
    ok.

%% @doc Test error handling for invalid document ID
invalid_document(_Config) ->
    %% Use an ID that doesn't exist
    InvalidDocId = 999999999,
    {error, "doc_not_found"} = lexbor_erl:replace_content(InvalidDocId, <<"div">>, <<"<p>X</p>">>),
    ok.

%% @doc Test replace with HTML containing attributes
replace_with_attributes(_Config) ->
    Html = <<"<div id='container'><p>Target</p></div>">>,
    NewHtml = <<"<a href='/link' class='link'>New Link</a>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:replace_content(Doc, <<"p">>, NewHtml),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Original should be gone
    true = binary:match(Result, <<"<p>Target</p>">>) =:= nomatch,
    
    %% Attributes should be preserved
    true = binary:match(Result, <<"href=\"/link\"">>) =/= nomatch,
    true = binary:match(Result, <<"class=\"link\"">>) =/= nomatch,
    true = binary:match(Result, <<"New Link">>) =/= nomatch,
    ok.

%% @doc Test that replace preserves siblings
replace_preserves_siblings(_Config) ->
    Html = <<"<div><span>Before</span><p>Target</p><span>After</span></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:replace_content(Doc, <<"p">>, <<"<em>New</em>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Siblings should be preserved
    true = binary:match(Result, <<"<span>Before</span>">>) =/= nomatch,
    true = binary:match(Result, <<"<span>After</span>">>) =/= nomatch,
    
    %% Original target should be gone
    true = binary:match(Result, <<"<p>Target</p>">>) =:= nomatch,
    
    %% Replacement should be present
    true = binary:match(Result, <<"<em>New</em>">>) =/= nomatch,
    
    %% Order should be preserved: Before < New < After
    {BeforePos, _} = binary:match(Result, <<"Before">>),
    {NewPos, _} = binary:match(Result, <<"New">>),
    {AfterPos, _} = binary:match(Result, <<"After">>),
    true = BeforePos < NewPos,
    true = NewPos < AfterPos,
    ok.

%% @doc Test replace with multiple elements
replace_with_multiple_elements(_Config) ->
    Html = <<"<div><p>Old</p></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, 1} = lexbor_erl:replace_content(Doc, <<"p">>, <<"<span>A</span><span>B</span><span>C</span>">>),
    {ok, Result} = lexbor_erl:serialize(Doc),
    ok = lexbor_erl:release(Doc),
    
    %% Original should be gone
    true = binary:match(Result, <<"<p>Old</p>">>) =:= nomatch,
    
    %% All replacement elements should be present
    true = binary:match(Result, <<"<span>A</span>">>) =/= nomatch,
    true = binary:match(Result, <<"<span>B</span>">>) =/= nomatch,
    true = binary:match(Result, <<"<span>C</span>">>) =/= nomatch,
    
    %% They should be in order: A < B < C
    {APos, _} = binary:match(Result, <<">A<">>),
    {BPos, _} = binary:match(Result, <<">B<">>),
    {CPos, _} = binary:match(Result, <<">C<">>),
    true = APos < BPos,
    true = BPos < CPos,
    ok.

%% @doc Test the difference between replace (removes element) and set_inner_html (keeps element)
replace_vs_set_inner_html_difference(_Config) ->
    %% Test replace - removes the element entirely
    Html1 = <<"<div><p id='target'>Content</p></div>">>,
    {ok, Doc1} = lexbor_erl:parse(Html1),
    {ok, 1} = lexbor_erl:replace_content(Doc1, <<"#target">>, <<"<span>New</span>">>),
    {ok, Result1} = lexbor_erl:serialize(Doc1),
    ok = lexbor_erl:release(Doc1),
    
    %% For replace: <p> is gone, only <span> remains inside <div>
    %% Expected: <div><span>New</span></div>
    true = binary:match(Result1, <<"<p">>) =:= nomatch,
    true = binary:match(Result1, <<"<div><span>New</span></div>">>) =/= nomatch,
    
    %% Test set_inner_html - keeps the element, replaces its children
    Html2 = <<"<div><p id='target'>Content</p></div>">>,
    {ok, Doc2} = lexbor_erl:parse(Html2),
    {ok, [Target]} = lexbor_erl:select(Doc2, <<"#target">>),
    ok = lexbor_erl:set_inner_html(Doc2, Target, <<"<span>New</span>">>),
    {ok, Result2} = lexbor_erl:serialize(Doc2),
    ok = lexbor_erl:release(Doc2),
    
    %% For set_inner_html: <p> remains with new children
    %% Expected: <div><p id='target'><span>New</span></p></div>
    true = binary:match(Result2, <<"<p id=\"target\"><span>New</span></p>">>) =/= nomatch,
    
    %% The results should be different
    true = Result1 =/= Result2,
    ok.
