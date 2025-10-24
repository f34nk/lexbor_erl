-module(node_example).
-export([run/0]).

run() ->
    io:format("Create and manipulate nodes...~n"),
    
    %% Start application
    ok = lexbor_erl:start(),
    
    %% Test create_element and append_child
    Html1 = <<"<html><body><ul id='list'></ul></body></html>">>,
    {ok, Doc1} = lexbor_erl:parse(Html1),
    {ok, [List]} = lexbor_erl:select(Doc1, <<"ul#list">>),
    
    {ok, Item1} = lexbor_erl:create_element(Doc1, <<"li">>),
    io:format("Created li element: ~p~n", [Item1]),
    ok = lexbor_erl:set_text(Doc1, Item1, <<"Item 1">>),
    ok = lexbor_erl:append_child(Doc1, List, Item1),
    
    {ok, Item2} = lexbor_erl:create_element(Doc1, <<"li">>),
    ok = lexbor_erl:set_text(Doc1, Item2, <<"Item 2">>),
    ok = lexbor_erl:append_child(Doc1, List, Item2),
    
    {ok, ListHtml} = lexbor_erl:outer_html(Doc1, List),
    io:format("List with appended items: ~p~n", [ListHtml]),
    true = binary:match(ListHtml, <<"Item 1">>) =/= nomatch,
    true = binary:match(ListHtml, <<"Item 2">>) =/= nomatch,
    
    ok = lexbor_erl:release(Doc1),
    
    %% Test insert_before
    Html2 = <<"<ul><li>Second</li></ul>">>,
    {ok, Doc2} = lexbor_erl:parse(Html2),
    {ok, [List2]} = lexbor_erl:select(Doc2, <<"ul">>),
    {ok, [Second]} = lexbor_erl:select(Doc2, <<"li">>),
    
    {ok, First} = lexbor_erl:create_element(Doc2, <<"li">>),
    ok = lexbor_erl:set_text(Doc2, First, <<"First">>),
    ok = lexbor_erl:insert_before(Doc2, List2, First, Second),
    
    {ok, List2Html} = lexbor_erl:outer_html(Doc2, List2),
    io:format("List with inserted item: ~p~n", [List2Html]),
    
    %% Verify order
    {ok, Items} = lexbor_erl:select(Doc2, <<"li">>),
    {ok, FirstText} = lexbor_erl:get_text(Doc2, lists:nth(1, Items)),
    {ok, SecondText} = lexbor_erl:get_text(Doc2, lists:nth(2, Items)),
    io:format("Order: ~p, ~p~n", [FirstText, SecondText]),
    true = FirstText =:= <<"First">>,
    true = SecondText =:= <<"Second">>,
    
    ok = lexbor_erl:release(Doc2),
    
    %% Test remove_node
    Html3 = <<"<div><p>Remove me</p><p>Keep me</p></div>">>,
    {ok, Doc3} = lexbor_erl:parse(Html3),
    {ok, [Div]} = lexbor_erl:select(Doc3, <<"div">>),
    {ok, Paragraphs} = lexbor_erl:select(Doc3, <<"p">>),
    
    io:format("Before removal: ~p paragraphs~n", [length(Paragraphs)]),
    true = length(Paragraphs) =:= 2,
    
    [ToRemove|_] = Paragraphs,
    ok = lexbor_erl:remove_node(Doc3, ToRemove),
    
    {ok, RemainingParagraphs} = lexbor_erl:select(Doc3, <<"p">>),
    io:format("After removal: ~p paragraphs~n", [length(RemainingParagraphs)]),
    true = length(RemainingParagraphs) =:= 1,
    
    {ok, DivHtml} = lexbor_erl:outer_html(Doc3, Div),
    io:format("Div after removal: ~p~n", [DivHtml]),
    true = binary:match(DivHtml, <<"Remove me">>) =:= nomatch,
    true = binary:match(DivHtml, <<"Keep me">>) =/= nomatch,
    
    ok = lexbor_erl:release(Doc3),
    
    %% Test complex manipulation
    Html4 = <<"<div id='container'></div>">>,
    {ok, Doc4} = lexbor_erl:parse(Html4),
    {ok, [Container]} = lexbor_erl:select(Doc4, <<"#container">>),
    
    %% Build a structure
    {ok, Header} = lexbor_erl:create_element(Doc4, <<"h1">>),
    ok = lexbor_erl:set_text(Doc4, Header, <<"Title">>),
    ok = lexbor_erl:append_child(Doc4, Container, Header),
    
    {ok, Para} = lexbor_erl:create_element(Doc4, <<"p">>),
    ok = lexbor_erl:set_attribute(Doc4, Para, <<"class">>, <<"intro">>),
    ok = lexbor_erl:set_text(Doc4, Para, <<"Introduction">>),
    ok = lexbor_erl:append_child(Doc4, Container, Para),
    
    {ok, FinalHtml} = lexbor_erl:serialize(Doc4),
    io:format("Final HTML: ~p~n", [FinalHtml]),
    true = binary:match(FinalHtml, <<"<h1>Title</h1>">>) =/= nomatch,
    true = binary:match(FinalHtml, <<"class=\"intro\"">>) =/= nomatch,
    
    ok = lexbor_erl:release(Doc4),
    
    lexbor_erl:stop(),
    ok.
