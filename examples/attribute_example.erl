-module(attribute_example).
-export([run/0]).

run() ->
    io:format("Get and set attributes...~n"),
    
    %% Start application
    ok = lexbor_erl:start(),
    
    %% Parse HTML
    Html = <<"<div id='test'><a href='/old' class='link'>Link</a></div>">>,
    {ok, Doc} = lexbor_erl:parse(Html),
    io:format("Parsed document: ~p~n", [Doc]),
    
    %% Select link
    {ok, [Link]} = lexbor_erl:select(Doc, <<"a">>),
    io:format("Selected link: ~p~n", [Link]),
    
    %% Test get_attribute
    {ok, Href} = lexbor_erl:get_attribute(Doc, Link, <<"href">>),
    io:format("Got href: ~p~n", [Href]),
    true = Href =:= <<"/old">>,
    
    {ok, Class} = lexbor_erl:get_attribute(Doc, Link, <<"class">>),
    io:format("Got class: ~p~n", [Class]),
    true = Class =:= <<"link">>,
    
    {ok, undefined} = lexbor_erl:get_attribute(Doc, Link, <<"title">>),
    io:format("Non-existent attribute returns undefined: ok~n"),
    
    %% Test set_attribute
    ok = lexbor_erl:set_attribute(Doc, Link, <<"href">>, <<"/new">>),
    io:format("Set href to /new~n"),
    
    {ok, NewHref} = lexbor_erl:get_attribute(Doc, Link, <<"href">>),
    io:format("Got new href: ~p~n", [NewHref]),
    true = NewHref =:= <<"/new">>,
    
    ok = lexbor_erl:set_attribute(Doc, Link, <<"target">>, <<"_blank">>),
    io:format("Added target attribute~n"),
    
    {ok, Target} = lexbor_erl:get_attribute(Doc, Link, <<"target">>),
    io:format("Got target: ~p~n", [Target]),
    true = Target =:= <<"_blank">>,
    
    %% Test remove_attribute
    ok = lexbor_erl:remove_attribute(Doc, Link, <<"class">>),
    io:format("Removed class attribute~n"),
    
    {ok, undefined} = lexbor_erl:get_attribute(Doc, Link, <<"class">>),
    io:format("Class attribute removed successfully~n"),
    
    %% Verify changes in outer_html
    {ok, LinkHtml} = lexbor_erl:outer_html(Doc, Link),
    io:format("Link HTML: ~p~n", [LinkHtml]),

    %% Cleanup
    ok = lexbor_erl:release(Doc),

    lexbor_erl:stop(),
    ok.
