
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
	Bot = {
    	bot,
    	{
			bot,
			start_link,
			["irc.freenode.net",6667,<<"p8bot">>,<<"#projecteight">>]
      	},
      	permanent,
      	5000,
      	worker,
      	[bot]
    },
    {ok, { {one_for_one, 5, 10}, [Bot]} }.

