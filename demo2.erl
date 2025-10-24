-module(demo2).
-export([run/0]).

run() ->
    ok = lexbor_erl:start(),
    
    Html = <<"<div><p>Hello 世界</p><p>Test</p></div>">>,
    io:format("Input HTML: ~p~n", [Html]),
    io:format("Input HTML as string: ~ts~n", [Html]),
    
    {ok, Doc} = lexbor_erl:parse(Html),
    {ok, Nodes} = lexbor_erl:select(Doc, <<"p">>),
    io:format("Found ~p nodes~n", [length(Nodes)]),
    
    [First | _] = Nodes,
    {ok, FirstHtml} = lexbor_erl:outer_html(Doc, First),
    
    io:format("~nFirst node HTML (as binary): ~p~n", [FirstHtml]),
    io:format("First node HTML (as string): ~ts~n", [FirstHtml]),
    io:format("Size: ~p bytes~n", [byte_size(FirstHtml)]),
    
    % Try to find the Unicode characters
    Result = binary:match(FirstHtml, <<"世界"/utf8>>),
    io:format("Search for '世界': ~p~n", [Result]),
    
    % Also try searching for "Hello"
    Result2 = binary:match(FirstHtml, <<"Hello">>),
    io:format("Search for 'Hello': ~p~n", [Result2]),
    
    lexbor_erl:release(Doc),
    lexbor_erl:stop(),
    ok.
