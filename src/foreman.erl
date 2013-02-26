-module(foreman).
-behavior(supervisor).

%% API
-export([start_link/6]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Index, BotName, Server, Port, Channel, Handlers) ->
	SuperName = erl9000_util:buildatom('foreman_',Index),
	Args = [Index, BotName, Server, Port, Channel, Handlers],
    supervisor:start_link({local, SuperName}, ?MODULE, Args).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Index, BotNick, Server, Port, Channel, Handlers]) ->
	% the strategy
	SupStrat = {one_for_all, 5, 10},
	BotName = erl9000_util:buildatom('bot_', Index),
	MgrName = erl9000_util:buildatom('bot_ev_mgr_', Index),

	Bot = make_bot_spec(BotName, MgrName, BotNick, Server, Port, Channel, Handlers),
	Mgr = make_mgr_spec(MgrName, BotName),
	{ok, {SupStrat, [Mgr,Bot]}}.

make_bot_spec(BotName, MgrName, BotNick, Server, Port, Channel, Handlers) ->
	Args = [BotName, MgrName, BotNick, Server, Port, Channel, Handlers],
	{
		bot,
		{
			bot,
			start_link,
			Args
		},
		permanent,
		5000,
		worker,
		[bot]
	}.

make_mgr_spec(MgrName, BotName) ->
	Args = [MgrName, BotName],
	{
		bot_ev_mgr,
		{
			bot_ev_mgr,
			start_link,
			Args
		},
		permanent,
		5000,
		worker,
		[bot_ev_mgr]
	}.