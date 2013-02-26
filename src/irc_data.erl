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
		get_nick/1, get_host/1, get_channel/1,
		get_raw/1, get_command/1, get_cmd_pars/1]).

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
rec_from_parsed([{bot_cmd_arg, BotArg}|R], IRCData) ->
	rec_from_parsed(R, IRCData#irc{bot_cmd_args=BotArg});
rec_from_parsed([{param, P}|R], #irc{cmd_par=none}=IRCData) ->
	rec_from_parsed(R, IRCData#irc{cmd_par=[P]});
rec_from_parsed([{param, P}|R], #irc{cmd_par=Pars}=IRCData) ->
	rec_from_parsed(R, IRCData#irc{cmd_par=Pars ++ [P]}).

get_bot_cmd(#irc{bot_cmd=BC}) ->
	BC.

get_trailing(#irc{bot_cmd=none, trail=Trl}) ->
    Trl;
get_trailing(#irc{bot_cmd=BC, bot_cmd_args=BA}) ->
	erl9000_util:bcat([<<$!>>,BC,<<" ">>,BA]).

get_raw(#irc{raw=Raw}) ->
	Raw.

get_host(#irc{host=Host}) ->
	Host.

get_command(#irc{cmd=Cmd}) ->
	Cmd.

get_nick(#irc{nick=Nick}) ->
	Nick.

get_cmd_pars(#irc{cmd_par=Pars}) ->
	Pars.

get_channel(#irc{chn = Chn}) ->
	Chn.

%%%%%%%%%%%%%
%%% EUNIT %%%
%%%%%%%%%%%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

privmsg_parse_nick_test() ->
	Src = <<":kofron!~kofron@D-173-250-190-122.dhcp4.washington.edu PRIVMSG #projecteight :hi\r\n">>,
	{ok, R} = from_n(Src),
	?assertEqual(<<"kofron">>, irc_data:get_nick(R)).

privmsg_parse_host_test() ->
	Src = <<":kofron!~kofron@D-173-250-190-122.dhcp4.washington.edu PRIVMSG #projecteight :hi\r\n">>,
	{ok, R} = from_n(Src),
	?assertEqual(<<"~kofron@D-173-250-190-122.dhcp4.washington.edu">>, irc_data:get_host(R)).

privmsg_parse_command_test() ->
	Src = <<":kofron!~kofron@D-173-250-190-122.dhcp4.washington.edu PRIVMSG #projecteight :hi\r\n">>,
	{ok,R} = from_n(Src),
	?assertEqual(<<"PRIVMSG">>, irc_data:get_command(R)).

privmsg_parse_channel_test() ->
	Src = <<":kofron!~kofron@D-173-250-190-122.dhcp4.washington.edu PRIVMSG #projecteight :hi\r\n">>,
	{ok,R} = from_n(Src),
	?assertEqual(<<"#projecteight">>,irc_data:get_channel(R)).

privmsg_parse_trailing_test() ->
	Src = <<":kofron!~kofron@D-173-250-190-122.dhcp4.washington.edu PRIVMSG #projecteight :hi\r\n">>,
	{ok,R} = from_n(Src),
	?assertEqual(<<"hi">>, irc_data:get_trailing(R)).

long_privmsg_parse_nick_test() ->
	Src = <<":kofron!~kofron@D-173-250-190-122.dhcp4.washington.edu PRIVMSG #projecteight :hi this is a long message\r\n">>,
	{ok,R} = from_n(Src),
	?assertEqual(<<"kofron">>, irc_data:get_nick(R)).

long_privmsg_parse_host_test() ->
	Src = <<":kofron!~kofron@D-173-250-190-122.dhcp4.washington.edu PRIVMSG #projecteight :hi this is a long message\r\n">>,
	{ok,R} = from_n(Src),
	?assertEqual(<<"~kofron@D-173-250-190-122.dhcp4.washington.edu">>, irc_data:get_host(R)).

long_privmsg_parse_command_test() ->
	Src = <<":kofron!~kofron@D-173-250-190-122.dhcp4.washington.edu PRIVMSG #projecteight :hi this is a long message\r\n">>,
	{ok,R} = from_n(Src),
	?assertEqual(<<"PRIVMSG">>, irc_data:get_command(R)).

long_privmsg_parse_channel_test() ->
	Src = <<":kofron!~kofron@D-173-250-190-122.dhcp4.washington.edu PRIVMSG #projecteight :hi this is a long message\r\n">>,
	{ok,R} = from_n(Src),
	?assertEqual(<<"#projecteight">>,irc_data:get_channel(R)).

long_privmsg_parse_trailing_test() ->
	Src = <<":kofron!~kofron@D-173-250-190-122.dhcp4.washington.edu PRIVMSG #projecteight :hi this is a long message\r\n">>,
	{ok,R} = from_n(Src),
	?assertEqual(<<"hi this is a long message">>, irc_data:get_trailing(R)).

service_parse_nick_test() ->
	T = <<":services. 328 zoltar05131983 #ubuntu :http://www.ubuntu.com\r\n">>,
	{ok,R} = from_n(T),
	?assertEqual(<<"services">>, irc_data:get_nick(R)).

service_parse_command_test() ->
	T = <<":services. 328 zoltar05131983 #ubuntu :http://www.ubuntu.com\r\n">>,
	{ok,R} = from_n(T),
	?assertEqual(<<"328">>, irc_data:get_command(R)).

service_parse_trailing_test() ->
	T = <<":services. 328 zoltar05131983 #ubuntu :http://www.ubuntu.com\r\n">>,
	{ok,R} = from_n(T),
	?assertEqual(<<"http://www.ubuntu.com">>, irc_data:get_trailing(R)).

service_parse_channel_test() ->
	T = <<":services. 328 zoltar05131983 #ubuntu :http://www.ubuntu.com\r\n">>,
	{ok,R} = from_n(T),
	?assertEqual(<<"#ubuntu">>, irc_data:get_channel(R)).

service_parse_cmd_pars_test() ->
	T = <<":services. 328 zoltar05131983 #ubuntu :http://www.ubuntu.com\r\n">>,
	{ok,R} = from_n(T),
	Tgt = [<<"zoltar05131983">>],
	?assertEqual(Tgt, irc_data:get_cmd_pars(R)).

service_2_parse_test() ->
	T = <<":hobana.freenode.net 333 zoltar05131983 #ubuntu IdleOne!~idleone@ubuntu/member/idleone 1351468761\r\n">>,
	{ok,R} = from_n(T),
	TgtPar = [<<"zoltar05131983">>, 
			<<"IdleOne!~idleone@ubuntu/member/idleone">>, 
			<<"1351468761">>],
	?assertEqual(TgtPar, irc_data:get_cmd_pars(R)).

bot_command_parse_nick_test() ->
	Src = <<":kofron!~kofron@D-173-250-190-122.dhcp4.washington.edu PRIVMSG #projecteight :!hello\r\n">>,
	{ok, R} = from_n(Src),
	?assertEqual(<<"kofron">>, irc_data:get_nick(R)).

bot_command_with_args_trailing_test() ->
	Src = <<":kofron!~kofron@D-173-250-190-122.dhcp4.washington.edu PRIVMSG #projecteight :!hello a b c\r\n">>,
	Trl = <<"!hello a b c">>,
	{ok, R} = from_n(Src),
	?assertEqual(Trl, irc_data:get_trailing(R)).

bot_command_parse_2_nick_test() ->
	Src = <<":kofron!~kofron@D-173-250-190-122.dhcp4.washington.edu PRIVMSG #projecteight :hello !hello\r\n">>,
	{ok, R} = from_n(Src),
	?assertEqual(<<"kofron">>, irc_data:get_nick(R)).

weird_name_test() ->
	Src = <<":MK`!mk@unaffiliated/mk/x-7191235 PRIVMSG #ubuntu :_Dude_: can you install ia32-libs?\r\n">>,
	{ok, R} = from_n(Src),
	?assertEqual(<<"MK`">>, irc_data:get_nick(R)).
-endif.
