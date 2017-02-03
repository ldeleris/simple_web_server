%%%-------------------------------------------------------------------
%% @doc simple_web_server public API
%% @end
%%%-------------------------------------------------------------------

-module(simple_web_server_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/0, stop/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc Starts the application
start() ->
  application:ensure_all_started(simple_web_server).

%% @doc Stops the application
stop() ->
  application:stop(simple_web_server).

start(_StartType, _StartArgs) ->
    io:format("~p starting~n",[?MODULE]),
    simple_web_server_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    io:format("~p stoping~n",[?MODULE]),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
