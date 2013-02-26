-module(console_log).

-behaviour(gen_addon).

%% gen_event callbacks
-export([init/1, handle_irc_msg/2, terminate/2]).

-record(state, {}).

init([]) ->
	{ok, #state{}}.

handle_irc_msg(IRCMsg, State) ->
	lager:info("~p",[irc_data:get_raw(IRCMsg)]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.