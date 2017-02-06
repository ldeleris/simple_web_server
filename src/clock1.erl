%% ---
%%  Excerpted from "Programming Erlang, Second Edition",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang2 for more book information.
%%---
-module(clock1).
-export([start/1, current_time/0]). 

start(Browser) ->
    io:format("[~p] started. ~n",[?MODULE]),
    Msg = [{ cmd , fill_div}, {id , clock}, {txt , current_time()}],
    io:format("[~p] send msg: (~p) to (~p)~n",[?MODULE, Msg, Browser]),
	Browser ! Msg,
    running(Browser).

running(Browser) ->
    receive
	{Browser, {struct,[{clicked,<<"stop">>}]} } ->
	    io:format("[~p] clicked stop: (~p) ~n",[?MODULE, Browser]),
	    idle(Browser);
	{Browser, Cmd} -> io:format("[~p] cmd: (~p) to (~p) ~n",[?MODULE, Cmd, Browser]),
	    idle(Browser)
    after 1000 ->
        Msg = [{ cmd , fill_div}, {id , clock}, {txt , current_time()}],
        io:format("[~p] send msg: (~p) to (~p)~n",[?MODULE, Msg, Browser]),
	    Browser ! Msg,
	    running(Browser)
    end.

idle(Browser) ->
    receive
	{Browser, {struct,[{clicked,<<"start">>}]} } ->
	    io:format("[~p] clicked start: (~p) ~n",[?MODULE, Browser]),
	    running(Browser);
	{Browser, Cmd} -> io:format("[~p] cmd: (~p) to (~p) ~n",[?MODULE, Cmd, Browser]),
	    idle(Browser)
    end.

current_time() ->
    {Hour,Min,Sec} = time(),
    list_to_binary(io_lib:format("~2.2.0w:~2.2.0w:~2.2.0w",
				 [Hour,Min,Sec])).

