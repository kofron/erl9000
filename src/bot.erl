-module(bot).
-behaviour(gen_server).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server api and callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start_link/7]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
			terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal server state %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state, {
					ev_mgr = none :: pid(), 
					active_sock = none,
					nick = none,
					ch = none
				}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server API and callback definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link(BotName, MgrName, BotNick, Server, Port, Channel, Handlers) ->
	BotArgs = [MgrName, BotNick, Server, Port, Channel, Handlers],
	gen_server:start_link({local, BotName}, ?MODULE, BotArgs, []).

init([MgrName, BotNick, Server, Port, Channel, Handlers]) ->
	% start our gen_event manager
	EvMgr = MgrName,
	ok = register_handlers(EvMgr, Handlers),
	Options = default_sock_opts(),
	State0 = #state{nick=BotNick, ch=Channel},
    {ok, Sock} = connect_and_identify(Server, Port, Options, BotNick, Channel),
    {ok, State0#state{active_sock=Sock, ev_mgr=EvMgr}}.

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({tcp, S, <<"PING :",Server/binary>>}, #state{active_sock=S}=State) ->
	ok = send_pong(S,Server),
	{noreply, State};
handle_info({tcp, S, Data}, #state{active_sock=S,ev_mgr=E}=State) ->
	case irc_data:from_n(Data) of
		{fail, _Reason}=Rsn ->
			lager:error("parsing failed! ~p",[Data]),
			lager:error("~p",[Rsn]);
		{ok, Parsed} ->
			ok = gen_event:notify(E, {irc, Parsed, S}),
			ok
	end,
	{noreply, State};
handle_info({gen_event_EXIT, H, _Reason}=Msg, #state{ev_mgr=E}=State) ->
	lager:error("~p:~p~n",[H,Msg]),
	lager:info("restarting handler named ~p~n.",[H]),
	ok = register_handler(E,H),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
connect_and_identify(Ser, Prt, Opt, Nick, Chn) ->
	Res = case gen_tcp:connect(Ser, Prt, Opt) of
    	{ok, Sock} ->
    		ok = identify_self(Nick, Sock),
    		ok = join_channel(Chn, Sock),
    		lager:info("connected successfully."),
    		{ok, Sock};
    	{error, Reason}=E ->
    		lager:info("connection barfed! (~p)", [Reason]),
    		E
    	end,
    Res.

default_sock_opts() ->
	[
		binary, 
		{active, true}, 
		{packet, line}, 
		{keepalive, true},
		{send_timeout, 1000}
	].

identify_self(Nick, Sock) ->
	gen_tcp:send(Sock, <<"NICK ",Nick/binary,$\r,$\n>>),
	gen_tcp:send(Sock, <<"USER ",Nick/binary," 0 * :project8 bot",$\r,$\n>>).

join_channel(Channel, Sock) ->
	gen_tcp:send(Sock, <<"JOIN ",Channel/binary,$\r,$\n>>).

send_pong(Sock, Target) ->
	gen_tcp:send(Sock, <<"PONG :",Target/binary>>).

register_handler(EventManager, {Handler, Opts}) ->
	gen_event:add_sup_handler(EventManager, 
								{gen_addon, Handler}, 
								[Handler|Opts]).
register_handlers(EventManager, Handler) when is_atom(Handler) ->
	register_handler(EventManager, Handler);
register_handlers(EventManager, Handlers) when is_list(Handlers) ->
	lists:foreach(fun (X) -> register_handler(EventManager,X) end, Handlers).
