%% ---
%%  Excerpted from "Programming Erlang, Second Edition",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang2 for more book information.
%%---
-module(websockets).

-export([start_link/1, 
	 start_embedded/1,
	 init/3,      websocket_init/3,
	 handle/2,    websocket_handle/3, 
	 terminate/2, terminate/3, websocket_terminate/3,
	 websocket_info/3,
	 append_div/3,
	 fill_div/3
	]).

%% env has only one parameter - reserved for future expansion

-record(env, {root}).

start_embedded(Port) ->
    %ok   = application:start(ssl),
    %ok   = application:start(ranch),
    %ok   = application:start(cowboy),
    web_server_start(Port, "./"),
    receive
	after 
	    infinity ->
		true
	end.

start_link([PortAtom, DirAtom]=Z) ->
    io:format("Here Z=~p~n",[Z]),
    %% io:format("code:~p~n",[code:get_path()]),
    ok   = application:start(asn1),
    ok   = application:start(public_key),   
    ok   = application:start(crypto),
    ok   = application:start(ssl),
    ok   = application:start(ranch),  
    ok   = application:start(cowboy),

    Port = list_to_integer(atom_to_list(PortAtom)),
    Dir  = atom_to_list(DirAtom),
    ok  = web_server_start(Port, Dir),
    receive
	after 
	    infinity ->
		true
	end.

web_server_start(Port, Dir) ->
    E0 = #env{root=Dir},
    {ok, Files} = file:list_dir(Dir),
    io:format("Files: ~p~n",[Files]),
    %%Dispatch = [{'_', [{'_', ?MODULE, []}]}], 
    Dispatch = cowboy_router:compile([{'_',[{'_', ?MODULE, E0}]}]),
    %% server is the name of this module
    NumberOfAcceptors = 10,
    Status = 
	cowboy:start_http(my_named_thing,
			  NumberOfAcceptors,
			  [{port, Port}],
			  [{env, [{dispatch, Dispatch}]}]),
    case Status of
	{error, _} ->
	    io:format("websockets could not be started -- "
		      "port ~p probably in use~n", [Port]),
	    init:stop();
	{ok, _Pid} ->
	    io:format("websockets started on port:~p~n",[Port])
    end.

init(_, Req, E0) ->   
    Resource = path(Req),
    io:format("init Resource =~p Env=~p~n",[Resource, E0]),
    %%io:format("dir: ~p~n",[os:cmd(E0#env.root)]),
    F = E0#env.root,
    case Resource of
	["/", "websocket", ModStr] ->
	    {upgrade, protocol, cowboy_websocket, Req, E0};
    _ ->
        {ok, Req, E0}
    end. 
    
terminate(_,_) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.
    
handle(Req, Env) ->
%%    Resource = path(Req),
%%    io:format("server_file:~p~n",[Resource]),
%%    serve_file(Resource, Req, Env).
    Resource = filename:join(path(Req)),
    io:format("ezwebframe:handle ~p~n",[Resource]),
    F = Env#env.root,
    [ _| R] = Resource,
    Res1 = F ++ R,
    io:format("mapped to:~p~n",[Res1]),
    case Resource of
	"/" ->
	    serve_file(F ++ "index.html", Req, Env); 
	"/files" ->
	    list_dir(F, Req, Env);
	_ ->
	    serve_file(Res1, Req, Env)
    end.
    
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
    %%io:format("send page: ~p~n",[Data]),
    cowboy_req:reply(200, [{<<"Content-Type">>,
			    list_to_binary(mime_type(Type))}],
		     Data, Req).


classify_extension(".ico") -> ico;
classify_extension(".gif") -> gif;
classify_extension(".jpg") -> jpg;
classify_extension(".png") -> png;
classify_extension(".js")  -> js;
classify_extension(".css") -> css;
classify_extension(_)      -> html.

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

reply_html(Obj, Req, Env) ->
    {ok, Req1} = send_page(html, Obj, Req),
    {ok, Req1, Env}.

%%----------------------------------------------------------------------
%% websocket stuff

websocket_init(_Transport, Req, _Env) ->
    io:format("Initialising a web socket:(~p)(~p)(~p)~n",
		   [_Transport, _Env, path(Req)]),
    ["/", "websocket", ModStr] = path(Req),
    %% Args = args(Req),
    Req1 = cowboy_req:compact(Req),
    Self = self(),
    Mod = list_to_atom(ModStr),
    %% Spawn an erlang handler
    Pid = spawn_link(Mod, start, [Self]),
    io:format("[websocket] Pid (~p)",[Pid]),
    {ok, Req1, Pid, hibernate}.

websocket_handle({text, Msg}, Req, Pid) ->
    %% This is a Json message from the browser
    io:format("Handle a web socket:(~p) to (~p)~n",
        [Msg, Pid]),
    case catch mochijson2:decode(Msg) of
	{'EXIT', _Why} ->
	    Pid ! {invalidMessageNotJSON, Msg};
	{struct, X} = Z ->
	    X1 = atomize(Z),
	    io:format("cmd: (~p) (~p) ~n",[Z,X1]),
	    Pid ! {self(), X1};
	Other ->
	    io:format("cmd invalide: (~p) ~n",[Other]),
	    Pid ! {self(), #{ clicked => <<"stop">>}}
	    %%Pid ! {invalidMessageNotStruct, Other}
    end,
    {ok, Req, Pid}.

websocket_info({send,Str}, Req, Pid) ->
    {reply, {text, Str}, Req, Pid, hibernate};
websocket_info([{cmd,_}|_]=L, Req, Pid) ->
    B = list_to_binary(mochijson2:encode([{struct,L}])),
    {reply, {text, B}, Req, Pid, hibernate};
websocket_info(Info, Req, Pid) ->
    io:format("Handle_info Info:~p from Pid:~p~n",[Info,Pid]),
    %%B = list_to_binary(mochijson2:encode([{struct,Info}])),
    B = list_to_binary(mochijson2:encode(Info)),
    {reply, {text, B}, Req, Pid, hibernate}.

websocket_terminate(_Reason, _Req, Pid) ->
    io:format("websocket.erl terminate:~n"),
    exit(Pid, socketClosed),
    ok.

reply_json(Obj, Req, Env) ->
    %% Encode Obj as JSON and send to the browser
    Json = mochijson2:encode(Obj),
    {ok, Req1} = send_page(json, Json, Req),
    {ok, Req1, Env}.

binary_to_atom(B) ->
    list_to_atom(binary_to_list(B)).

rpc(Pid, M) ->
    S = self(),
    Pid ! {S, M},
    receive
	{Pid, Reply} ->
	    Reply
    end.

%%----------------------------------------------------------------------
%% atomize turns all the keys in a struct to atoms

atomize({struct,L}) ->
    {struct, [{binary_to_atom(I), atomize(J)} || {I, J} <- L]};
atomize(X) ->
    X.

%%----------------------------------------------------------------------
%% these are to be called from the gui client code

append_div(Ws, Div, X) ->
    Bin = list_to_binary(X),
    send_websocket(Ws, 
		   [{cmd,append_div},{id,Div},{txt,Bin}]).

fill_div(Ws, Div, X) ->
    io:format("websockets X=~p~n",[X]),
    Bin = list_to_binary(X),
    send_websocket(Ws, 
		   [{cmd,fill_div},{id,Div},{txt,Bin}]).
    

send_websocket(Ws, X) ->
    Ws ! {send, list_to_binary(mochijson2:encode([{struct,X}]))}.

