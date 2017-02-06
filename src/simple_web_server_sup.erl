%%%-------------------------------------------------------------------
%% @doc simple_web_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(simple_web_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {{one_for_one, 0, 1}, 
	  [{tag1, 
	    {static_web_server, start, []},
	    permanent, 
	    10000, 
	    worker, 
	    [static_web_server]},
		{tag2, 
	    {cgi_web_server, start, []},
	    permanent, 
	    10000, 
	    worker , 
	    [cgi_web_server]} ,
		{tag3, 
	    {websockets, start_embedded, [2234]},
	    permanent, 
	    10000, 
	    worker, 
	    [websockets]}
	  ]}}.

%%====================================================================
%% Internal functions
%%====================================================================
