-module(irc_data).

-type maybe_type(Type) :: none | Type.
-type nick_type() :: maybe_type(binary()).
-type host_type() :: maybe_type(binary()).
-type command_type() :: maybe_type(binary()).
-type channel_type() :: maybe_type(binary()).
-type msg_type() :: maybe_type(binary()).
-type error_tuple() :: {error, term()}.
-record(irc, {
				send_nick = none :: nick_type(),
				send_host = none :: host_type(),
				command   = none :: command_type(),
				channel   = none :: channel_type(),
				msg       = none :: msg_type()
			}).

-opaque irc_data() :: #irc{}.

% "constructors"
-export([new/0, to_n/1, from_n/1]).
-export([get_bot_cmd/1, get_trailing/1, 
		get_nick/1,
		get_raw/1, get_command/1]).

-spec new() -> irc_data().
new() ->
	[].

-spec to_n(irc_data()) -> binary().
to_n(#irc{}) ->
	<<>>.

-spec from_n(binary()) -> {ok, irc_data()} | error_tuple().
from_n(Bin) ->
	case irc_grammar:parse(Bin) of
		{fail, _Reason}=Err ->
			Err;
		AnyOther ->
			{ok, [{raw,Bin}|AnyOther]}
	end.

get_bot_cmd(IRCData) ->
	proplists:get_value(bot_cmd_name, IRCData).

get_trailing(IRCData) ->
	case get_bot_cmd(IRCData) of
		undefined ->
			proplists:get_value(trailing, IRCData);
		_BotCmd ->
			<<>>
	end.

get_raw(IRCData) ->
	proplists:get_value(raw, IRCData).

get_command(IRCData) ->
	proplists:get_value(command, IRCData).

get_nick(IRCData) ->
	proplists:get_value(nick, IRCData).

%%%%%%%%%%%%%
%%% EUNIT %%%
%%%%%%%%%%%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

privmsg_parse_test() ->
	Src = <<":kofron!~kofron@D-173-250-190-122.dhcp4.washington.edu PRIVMSG #projecteight :hi\r\n">>,
	{C, _R} = from_n(Src),
	?assertEqual(ok, C).

long_privmsg_parse_test() ->
	Src = <<":kofron!~kofron@D-173-250-190-122.dhcp4.washington.edu PRIVMSG #projecteight :hi this is a long message\r\n">>,
	{C, _R} = from_n(Src),
	?assertEqual(ok, C).

ipv6_host_parse_test() ->
	Src = <<":kofron!~kofron@D-173-250-190-122.dhcp4.washington.edu PRIVMSG #projecteight :hi\r\n">>,
	{C, _R} = from_n(Src),
	?assertEqual(ok, C).

service_parse_test() ->
	T = <<":services. 328 zoltar05131983 #ubuntu :http://www.ubuntu.com\r\n">>,
	{C, _R} = from_n(T),
	?assertEqual(ok, C).

service_2_parse_test() ->
	T = <<":hobana.freenode.net 333 zoltar05131983 #ubuntu IdleOne!~idleone@ubuntu/member/idleone 1351468761\r\n">>,
	{C, _R} = from_n(T),
	?assertEqual(ok, C).

bot_command_parse_test() ->
	Src = <<":kofron!~kofron@D-173-250-190-122.dhcp4.washington.edu PRIVMSG #projecteight :!hello\r\n">>,
	{C, _R} = from_n(Src),
	?assertEqual(ok, C).

bot_command_parse_2_test() ->
	Src = <<":kofron!~kofron@D-173-250-190-122.dhcp4.washington.edu PRIVMSG #projecteight :hello !hello\r\n">>,
	{C, _R} = from_n(Src),
	?assertEqual(ok, C).

weird_name_test() ->
	Src = <<":MK`!mk@unaffiliated/mk/x-7191235 PRIVMSG #ubuntu :_Dude_: can you install ia32-libs?\r\n">>,
	{C, _R} = from_n(Src),
	?assertEqual(ok, C).


-endif.