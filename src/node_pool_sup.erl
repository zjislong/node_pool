%%%-------------------------------------------------------------------
%% @doc node_pool top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(node_pool_sup).

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

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { 
            {simple_one_for_one, 0, 1},
            [
                #{id => node_pool_srv,
                 start => {node_pool_srv, start_link, []},
                 restart => temporary}
            ]
        } 
    }.

%%====================================================================
%% Internal functions
%%====================================================================
