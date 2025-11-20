-module(lexbor_erl_dom_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).

-export([
    % Attributes
    test_get_attribute/1,
    test_set_attribute/1,
    test_remove_attribute/1,
    test_attribute_not_found/1,
    
    % Text and HTML
    test_get_text/1,
    test_set_text/1,
    test_inner_html/1,
    test_set_inner_html/1,
    test_serialize/1,
    
    % Node creation and tree manipulation
    test_create_element/1,
    test_append_child/1,
    test_insert_before/1,
    test_remove_node/1,
    test_complex_dom_building/1
]).

all() ->
    [
        {group, dom_manipulation}
    ].

groups() ->
    [
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

    %% Check for element tags (serialization may vary across platforms)
    ?assert(binary:match(NewInner, <<"<p>">>) =/= nomatch),
    ?assert(binary:match(NewInner, <<"</p>">>) =/= nomatch),
    ?assert(binary:match(NewInner, <<"<span>">>) =/= nomatch),
    ?assert(binary:match(NewInner, <<"</span>">>) =/= nomatch),
    ?assert(binary:match(NewInner, <<"New">>) =/= nomatch),
    ?assert(binary:match(NewInner, <<"Content">>) =/= nomatch),
    
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
