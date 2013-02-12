-module(bot).
-behaviour(gen_server).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server api and callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start_link/4]).
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
start_link(Server, Port, Nick, Channel) ->
	BotArgs = [Server, Port, Nick, Channel],
	gen_server:start_link({local, bot_derived}, ?MODULE, BotArgs, []).

init([Server, Port, Nick, Channel]) ->
	Options = [binary, 
				{active, true}, 
				{packet, line}, 
				{keepalive, true},
                {send_timeout, 1000}],
    Res = case gen_tcp:connect(Server, Port, Options) of
    		{ok, Sock} ->
    			ok = identify_self(Nick, Sock),
    			ok = join_channel(Channel, Sock),
    			State0 = #state{nick=Nick, ch=Channel},
    			lager:info("connected successfully."),
    			{ok, State0#state{active_sock = Sock}};
    		{error, Reason}=E ->
    			lager:info("connection barfed! (~p)", [Reason]),
    			E
    		end,
    Res.

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({tcp, S, Data}, #state{active_sock=S}=State) ->
	lager:info("~p recvd",[Data]),
	{noreply, State};
handle_info({tcp, S, <<"PING :",_R/binary>>}, #state{active_sock=S}=State) ->
	ok = send_pong(S),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
identify_self(Nick, Sock) ->
	gen_tcp:send(Sock, <<"NICK ",Nick/binary,$\r,$\n>>),
	gen_tcp:send(Sock, <<"USER ",Nick/binary," 0 * :project8 bot",$\r,$\n>>).

join_channel(Channel, Sock) ->
	gen_tcp:send(Sock, <<"JOIN ",Channel/binary,$\r,$\n>>).

send_pong(Sock) ->
	gen_tcp:send(Sock, <<"PONG",$\r,$\n>>).
