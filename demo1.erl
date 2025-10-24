-module(demo1).
-export([run/0]).

run() ->
    ok = lexbor_erl:start(),
    
    % Test 1: select_html
    io:format("~n=== Test 1: select_html ===~n"),
    Html1 = <<"<div><p>First</p><p>Second</p></div>">>,
    {ok, Results1} = lexbor_erl:select_html(Html1, <<"p">>),
    io:format("Results: ~p~n", [Results1]),
    lists:foreach(fun(R) -> 
        io:format("  HTML: ~s~n", [R]),
        io:format("  Contains 'First': ~p~n", [binary:match(R, <<"First">>)])
    end, Results1),
    
    % Test 2: outer_html
    io:format("~n=== Test 2: outer_html ===~n"),
    Html2 = <<"<div><p class='test'>Content</p></div>">>,
    {ok, Doc2} = lexbor_erl:parse(Html2),
    {ok, Nodes2} = lexbor_erl:select(Doc2, <<"p">>),
    [Node2] = Nodes2,
    {ok, OuterHtml2} = lexbor_erl:outer_html(Doc2, Node2),
    io:format("Outer HTML: ~s~n", [OuterHtml2]),
    io:format("Contains 'Content': ~p~n", [binary:match(OuterHtml2, <<"Content">>)]),
    lexbor_erl:release(Doc2),
    
    % Test 3: nested
    io:format("~n=== Test 3: nested ===~n"),
    Html3 = <<"<div><div><div><div><div><p>Deep</p></div></div></div></div></div>">>,
    {ok, Doc3} = lexbor_erl:parse(Html3),
    {ok, Nodes3} = lexbor_erl:select(Doc3, <<"p">>),
    [Node3] = Nodes3,
    {ok, OuterHtml3} = lexbor_erl:outer_html(Doc3, Node3),
    io:format("Outer HTML: ~s~n", [OuterHtml3]),
    io:format("Contains 'Deep': ~p~n", [binary:match(OuterHtml3, <<"Deep">>)]),
    lexbor_erl:release(Doc3),
    
    lexbor_erl:stop(),
    ok.
