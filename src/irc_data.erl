-module(irc_data).

-type maybe_type(Type) :: none | Type.
-type nick_type() :: maybe_type(binary()).
-type host_type() :: maybe_type(binary()).
-type command_type() :: maybe_type(binary()).
-type command_par_type() :: maybe_type([binary()]).
-type channel_type() :: maybe_type(binary()).
-type msg_type() :: maybe_type(binary()).
-type bot_cmd_type() :: maybe_type(binary()).
-type bot_cmd_args_type() :: maybe_type(binary()).
-type error_tuple() :: {error, term()}.
-record(irc, {
				nick  = none :: nick_type(),
				host  = none :: host_type(),
				cmd   = none :: command_type(),
				cmd_par = none :: command_par_type(),
				chn   = none :: channel_type(),
				trail = none :: msg_type(),
				bot_cmd = none :: bot_cmd_type(),
				bot_cmd_args = none :: bot_cmd_args_type(),
				raw = <<>> :: msg_type()
			}).

-opaque irc_data() :: #irc{}.

% "constructors"
-export([new/0, to_n/1, from_n/1]).
-export([get_bot_cmd/1, get_trailing/1, 
		get_nick/1,
		get_raw/1, get_command/1]).

-spec new() -> irc_data().
new() ->
	#irc{}.

-spec to_n(irc_data()) -> binary().
to_n(#irc{}) ->
	<<>>.

-spec from_n(binary()) -> {ok, irc_data()} | error_tuple().
from_n(Bin) ->
	case irc_grammar:parse(Bin) of
		{fail, _Reason}=Err ->
			Err;
		AnyOther ->
			rec_from_parsed(AnyOther,#irc{raw=Bin})
	end.

-spec rec_from_parsed(term(), irc_data()) -> {ok, irc_data()} | error_tuple().
rec_from_parsed([], IRCData) ->
	{ok, IRCData};
rec_from_parsed([{nick, Nick}|R],IRCData) ->
	rec_from_parsed(R, IRCData#irc{nick=Nick});
rec_from_parsed([{host, Host}|R], IRCData) ->
	rec_from_parsed(R, IRCData#irc{host=Host});
rec_from_parsed([{channel, Chn}|R], IRCData) ->
	rec_from_parsed(R, IRCData#irc{chn=Chn});
rec_from_parsed([{command, Cmd}|R], IRCData) ->
	rec_from_parsed(R, IRCData#irc{cmd=Cmd});
rec_from_parsed([{trailing, Trl}|R], IRCData) ->
	rec_from_parsed(R, IRCData#irc{trail=Trl});
rec_from_parsed([{bot_cmd_name, BotCmd}|R], IRCData) ->
	rec_from_parsed(R, IRCData#irc{bot_cmd=BotCmd});
rec_from_parsed([{param, P}|R], #irc{cmd_par=none}=IRCData) ->
	rec_from_parsed(R, IRCData#irc{cmd_par=[P]});
rec_from_parsed([{param, P}|R], #irc{cmd_par=Pars}=IRCData) ->
	rec_from_parsed(R, IRCData#irc{cmd_par=Pars ++ [P]}).

get_bot_cmd(#irc{bot_cmd=BC}) ->
	BC.

get_trailing(#irc{bot_cmd=none, trail=Trl}) ->
    Trl;
get_trailing(#irc{bot_cmd=BC, trail=Trl}) ->
    <<>>.

get_raw(#irc{raw=Raw}) ->
	Raw.

get_command(#irc{cmd=Cmd}) ->
	Cmd.

get_nick(#irc{nick=Nick}) ->
	Nick.

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
