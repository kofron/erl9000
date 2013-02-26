-module(greeter).

-behaviour(gen_addon).

%% gen_event callbacks
-export([init/1, handle_irc_msg/2, terminate/2]).

-record(state, {}).

init([]) ->
    {ok, #state{}}.

handle_irc_msg(IRCMsg, State) ->
	case irc_data:get_bot_cmd(IRCMsg) of
		<<"greet">> ->
			{reply, <<"PRIVMSG #projecteight :hello\r\n">>, State};
		_Ignorable ->
			{noreply, State}
	end.

terminate(_Reason, _State) ->
    ok.
