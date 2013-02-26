-module(overlord).
-behavior(supervisor).

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
	% the strategy
	SupStrat = {one_for_one, 5, 10},

	% parse the config file.
	{ok, Cfg} = file:consult("bots.cfg"),
	Res = case parse_bots_cfg(Cfg) of
			{ok, BotGroups} ->
				{ok, {SupStrat, BotGroups}};
			{error, _Reason}=E ->
				E
			end,
	Res.	

parse_bots_cfg(Cfg) ->
	parse_bots_cfg(Cfg, 0, []).
parse_bots_cfg([], _Idx, Acc) ->
	{ok, Acc};
parse_bots_cfg([{bot, Data}|R], Idx, Acc) ->
	{ok, N} = get_nick(Data),
	{ok, S} = get_server(Data),
	{ok, P} = get_port(Data),
	{ok, C} = get_channel(Data),
	{ok, H} = get_handlers(Data),
	{ok, D} = get_handler_cfg(Data),
	Hd = fold_handlers_and_data(H,D),
	parse_bots_cfg(R, Idx + 1, [make_foreman_spec(Idx,N,S,P,C,Hd)|Acc]).

get_nick(Data) ->
	{nick, Val} = proplists:lookup(nick, Data),
	{ok, Val}.
get_server(Data) ->
	extract_key(Data, server).
get_port(Data) ->
	extract_key(Data, port).
get_channel(Data) ->
	{channel, C} = proplists:lookup(channel, Data),
	{ok, C}.
get_handlers(Data) ->
	case proplists:lookup(addons, Data) of
		none -> 
			{ok, []};
		{addons, H} ->
			{ok, H}
	end.
get_handler_cfg(Data) ->
	case proplists:lookup(addon_opts, Data) of
		none ->
			{ok, []};
		{addon_opts, Opts} ->
			{ok, Opts}
	end.

fold_handlers_and_data(Names, Cfg) ->
	fold_handlers_and_data_1(Names, Cfg, []).
fold_handlers_and_data_1([], _Cfg, Acc) ->
	Acc;
fold_handlers_and_data_1([N|Rest],Cfg,Acc) ->
	fold_handlers_and_data_1(Rest, Cfg, [{N,proplists:get_value(N, Cfg, [])}|Acc]).

extract_key(Data, Key) ->
	case proplists:lookup(Key,Data) of
		{Key, Value} ->
			{ok, Value};
		none ->
			{error, {no_key, Key}}
	end.

make_foreman_spec(Index, Nick, Server, Port, Channel, Handlers) ->
	ForemanName = erl9000_util:buildatom('foreman_',Index), 
	Args = [Index, Nick, Server, Port, Channel, Handlers],
	{
		ForemanName,
		{
			foreman,
			start_link,
			Args
		},
		permanent,
		5000,
		supervisor,
		[foreman]
	}.