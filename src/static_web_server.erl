%% ---
%%  Excerpted from "Programming Erlang, Second Edition",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang2 for more book information.
%%---
-module(static_web_server).
-compile(export_all). 

-record(env, {root}).

start() ->
    start(4000).

start(Port) ->
    E0 = #env{root="./"},
    N_acceptors = 10, %%<label id="web.app.acc"/>
    Dispatch = cowboy_router:compile([{'_',[{'_', ?MODULE, E0}]}]),
    cowboy:start_http(static_web_server,
		      N_acceptors,       %%<label id="web.app.accu"/>
		      [{port, Port}],
		      [{env, [{dispatch, Dispatch}]}]
		     ).


init({tcp, http}, Req, E0) ->
    Resource = path(Req),
    io:format("init Resource =~p Env=~p~n",[Resource, E0]),
%%    {ok, Req, undefined}.
    {ok, Req, E0}.
    
handle(Req, Env) ->
%%    {Path, Req1} = cowboy_req:path(Req),                %%<label id="web.handle1"/>
%%    Response = read_file(Path),                              %%<label id="web.handle2"/>
%%   {ok, Req2} = cowboy_req:reply(200, [], Response, Req1), %%<label id="web.handle3"/>
%%    {ok, Req2, State}.  %%<label id="web.handle4"/>
    Resource = filename:join(path(Req)),
    io:format("ezwebframe:handle ~p~n",[Resource]),
    F = Env#env.root,
    [ _| R] = Resource,
    Res1 = F ++ R,
    io:format("mapped to:~p~n",[Res1]),
    case Resource of
	"/" ->
	    serve_file("index.html", Req, Env); 
	"/files" ->
	    list_dir(F, Req, Env);
	_ ->
	    serve_file(Res1, Req, Env)
    end.    

terminate(_,_) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.

read_file(Path) ->
    File = ["."|binary_to_list(Path)],
    case file:read_file(File) of
	{ok, Bin} -> Bin;
	_ -> ["<pre>cannot read:", File, "</pre>"]
    end.

serve_file(File, Req, Env) ->
    case filelib:is_dir(File) of
	true ->
	    list_dir(File, Req, Env);
	false ->
	    serve_abs_file(File, Req, Env)
    end.

serve_abs_file(File, Req, Env) ->
    Val = file:read_file(File),
    case Val of 
	{error, _} ->
	    io:format("*** no page called ~p~n",[File]),
	    reply_html(pre({no_page_called,File}), Req, Env);
	{ok, Bin} ->
	    io:format("*** serve file: ~p~n",[File]),
	    Ext  = filename:extension(File),
	    Bin1 = if_erlang_add_pre(Ext, Bin),
	    Req1 = send_page(classify_extension(Ext), Bin1, Req),
	    {ok, Req1, Env}
    end.

if_erlang_add_pre(".erl", B) -> ["<pre>", B, "</pre>"];
if_erlang_add_pre(_, B)      -> B.

list_dir(Root, Req, Env) ->
    {ok, Files} = file:list_dir(Root),
    Files1 = [add_slash(I, Root) || I <- Files],
    L1 = [["<li><a href='",I,"'>",I,"</a></li>\n"] || I <- lists:sort(Files1)],
    reply_html(["<h1> Directory ",Root, "</h1>\n",
		"<ul>\n",L1,"</ul>\n"], Req, Env).

add_slash(I, Root) ->
    Full = filename:join(Root, I),
    case filelib:is_dir(Full) of
	true  -> I ++ "/";
	false -> I
    end.

send_page(Type, Data, Req) ->
    cowboy_req:reply(200, [{<<"Content-Type">>,
			    list_to_binary(mime_type(Type))}],
		     Data, Req).


%%path(Req) ->
%%    Path = cowboy_req:path(Req),
%%    filename:split(binary_to_list(Path)).

reply_html(Obj, Req, Env) ->
    Req1 = send_page(html, Obj, Req),
    {ok, Req1, Env}.

classify_extension(".ico")  -> ico;    
classify_extension(".gif") -> gif;
classify_extension(".jpg") -> jpg;
classify_extension(".png") -> png;
classify_extension(".js")  -> js;
classify_extension(".css") -> css;
classify_extension(_)      -> html.

mime_type(ico)     -> "image/x-icon";
mime_type(gif)     -> "image/gif";
mime_type(jpg)     -> "image/jpeg";
mime_type(png)     -> "image/png";
mime_type(css)     -> "text/css";
mime_type(special) -> "text/plain; charset=x-user-defined";
mime_type(json)    -> "application/json";
mime_type(swf)     -> "application/x-shockwave-flash";
mime_type(html)    -> "text/html";
mime_type(xul)     -> "application/vnd.mozilla.xul+xml";
mime_type(js)      -> "application/x-javascript";
mime_type(svg)     -> "image/svg+xml".


pre(X) ->
    ["<pre>\n",quote(lists:flatten(io_lib:format("~p",[X]))), "</pre>"].

quote("<" ++ T) -> "&lt;" ++ quote(T);
quote("&" ++ T) -> "&amp;" ++ quote(T);
quote([H|T])    -> [H|quote(T)];
quote([])       -> [].

path(Req) ->
    {Path,_} = cowboy_req:path(Req),
    P = filename:split(binary_to_list(Path)),
    io:format("Path=~p~n",[P]),
    P.


args(Req) ->
    {Args, _} = cowboy_req:qs_vals(Req),
    Args.

%%reply_html(Obj, Req, Env) ->
%%   {ok, Req1} = send_page(html, Obj, Req),
%%    {ok, Req1, Env}.