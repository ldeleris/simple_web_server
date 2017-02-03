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
    {ok, {{one_for_one, 3, 10}, 
	  [{tag1, 
	    {simple_web_server, start, []},
	    permanent, 
	    10000, 
	    worker, 
	    [simple_web_server]}
	  ]}}.

%%====================================================================
%% Internal functions
%%====================================================================
