-module(text_example).
-export([run/0]).

run() ->
    io:format("Get and set text ...~n"),
    
    %% Start application
    ok = lexbor_erl:start(),

    %% Test get_text and set_text
    Html1 = <<"<div>Hello <b>World</b>!</div>">>,
    {ok, Doc1} = lexbor_erl:parse(Html1),
    {ok, [Div]} = lexbor_erl:select(Doc1, <<"div">>),
    
    {ok, Text} = lexbor_erl:get_text(Doc1, Div),
    io:format("Got text: ~p~n", [Text]),
    true = Text =:= <<"Hello World!">>,
    
    ok = lexbor_erl:set_text(Doc1, Div, <<"New text">>),
    {ok, NewText} = lexbor_erl:get_text(Doc1, Div),
    io:format("New text: ~p~n", [NewText]),
    true = NewText =:= <<"New text">>,
    
    {ok, Html1Updated} = lexbor_erl:outer_html(Doc1, Div),
    io:format("Updated div: ~p~n", [Html1Updated]),
    ok = lexbor_erl:release(Doc1),
    
    %% Test inner_html and set_inner_html
    Html2 = <<"<div><p>Old</p></div>">>,
    {ok, Doc2} = lexbor_erl:parse(Html2),
    {ok, [Div2]} = lexbor_erl:select(Doc2, <<"div">>),
    
    {ok, Inner} = lexbor_erl:inner_html(Doc2, Div2),
    io:format("Got inner HTML: ~p~n", [Inner]),
    true = Inner =:= <<"<p>Old</p>">>,
    
    ok = lexbor_erl:set_inner_html(Doc2, Div2, <<"<p>New</p><span>Content</span>">>),
    {ok, NewInner} = lexbor_erl:inner_html(Doc2, Div2),
    io:format("New inner HTML: ~p~n", [NewInner]),
    
    {ok, Html2Updated} = lexbor_erl:outer_html(Doc2, Div2),
    io:format("Updated div: ~p~n", [Html2Updated]),
    ok = lexbor_erl:release(Doc2),
    
    lexbor_erl:stop(),
    ok.
