
-module(erl9000_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Overlord = {
    	overlord,
    	{
			overlord,
			start_link,
			[]
      	},
      	permanent,
      	5000,
      	supervisor,
      	[overlord]
    },
    {ok, { {one_for_one, 5, 10}, [Overlord]} }.

