-module(basecamp_log).
-behavior(gen_addon).

-export([init/1, handle_irc_msg/2, terminate/2]).
-record(state, 
		{
			last_date :: {integer(), integer(), integer()},
			msg_id :: integer,
			agent_hdr :: string(),
			root_url :: string(),
			project_id :: integer,
			post_msg :: fun(), 
			post_cmt :: fun(),
			msg_ep :: fun()
		}).

init(Opts) ->
	[User, Pass, Agent, AcctID, ProjName] = parse_opts(Opts),
	CheckURL = form_check_url(User, Pass, AcctID),
	RootURL = project_root_url(User, Pass, AcctID),
	AgentHdr = {"User-Agent", Agent},
	case check_project_connect(CheckURL, Agent, erlang:list_to_binary(ProjName)) of
		{ok, ProjectID} ->	
			State = #state{
				last_date = {0,0,0},
				root_url = RootURL,
				agent_hdr = AgentHdr,
				project_id = ProjectID,
				post_msg = fun(Date) -> 
								create_new_irc_log_thread(User,Pass,AgentHdr,AcctID,ProjectID,Date) 
							end,
				post_cmt = fun(MsgID, Cmt) ->
								create_bc_comment(User,Pass,AgentHdr,AcctID,ProjectID,MsgID,Cmt)
							end
			},
			{ok, State};
		{error, cannot_post}=E ->
			E
	end.

%  When a message is received, the basecamp logger checks to see if
%  the date has changed since the last message.  If it has, it needs
%  to create a new thread and use the new message ID for storing the
%  IRC logs.  If not, it can use the old message ID and simply add
%  a comment.
handle_irc_msg(IRCMsg, #state{post_msg=M,post_cmt=C,last_date=D,msg_id=ID,root_url=R}=State) ->
	case irc_data:get_command(IRCMsg) of
		<<"PRIVMSG">> ->			
			{{YY,MM,DD}=Dp, {_,_,_}} = calendar:local_time(),
			MsgID = case Dp of
					D ->
						ID;
					NotD ->
						{ok, NewID} = M(NotD),
						NewID
					end,
			Nick = irc_data:get_nick(IRCMsg),
			Msg = irc_data:get_trailing(IRCMsg),
			Parts = [<<"<b>",$[>>,Nick,<<$],"</b> -- ">>,Msg],
			ok = C(MsgID, erl9000_util:bcat(Parts)),
			{noreply, State#state{last_date=Dp,msg_id=MsgID}};
		_AnyOther ->
			{noreply, State}
	end.

terminate(_Reason, _State) ->
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_opts(OptsList) ->
	{ok, Name} = parse_opts(user, OptsList),
	{ok, Pass} = parse_opts(pass, OptsList),
	{ok, Agent} = parse_opts(user_agent, OptsList),
	{ok, AcctID} = parse_opts(account_id, OptsList),
	{ok, Project} = parse_opts(project, OptsList),
	[Name, Pass, Agent, AcctID, Project].
parse_opts(Key, List) ->
	case proplists:get_value(Key,List) of
		undefined ->
			undefined;
		Val ->
			{ok, Val}
	end.

form_check_url(User, Pass, AcctID) ->
	project_root_url(User, Pass, AcctID) ++
	"/api/v1/projects.json".

check_project_connect(CheckURL, Agent, Project) ->
	AgentHdr = {"User-Agent",Agent},
	Req = {CheckURL, [AgentHdr]},
	{ok, {200, Res}} = httpc:request(get, Req,[],httpc_get_options()),
	case can_post_to_project(Project, Res) of
		{ok, ProjID}=R ->
			R;
		false ->
			{error, cannot_post}
	end.

httpc_get_options() ->
	[{full_result, false}, {body_format, binary}].

can_post_to_project_1(Project, []) ->
	false;
can_post_to_project_1(Project, [Proj|Rest]=Res) ->
	case proplists:get_value(<<"name">>, Proj) of
		Project ->
			{ok, proplists:get_value(<<"id">>, Proj)};
		_Other ->
			can_post_to_project_1(Project, Rest)
	end.
can_post_to_project(Project, Result) ->
	P = jsx:decode(Result),
	can_post_to_project_1(Project, P).

create_bc_comment(User,Pass,AgentHdr,AcctID,ProjectID,MessageID,Cmt) ->
	EP = bc_comments_endpoint(User, Pass, AcctID, ProjectID, MessageID),
	Msg = [{<<"content">>,Cmt}],
	Req = {EP, [AgentHdr], "application/json", jsx:encode(Msg)},
	{ok, {201, Res}} = httpc:request(post, Req, [], httpc_get_options()),
	ok.

% endpoint functions.  these translate combinations of user IDs, 
% passwords, etc into the actual endpoints that we care about.
project_root_url(User, Pass, AcctID) ->
	"https://" ++ User ++ 
	":" ++ Pass ++ 
	"@basecamp.com/" ++ integer_to_list(AcctID).

bc_project_endpoint(User, Pass, AcctID, ProjID) ->
	project_root_url(User, Pass, AcctID) ++
	"/api/v1/projects/" ++
	integer_to_list(ProjID).

bc_message_endpoint(User, Pass, AcctID, ProjID) ->
	bc_project_endpoint(User, Pass, AcctID, ProjID) ++
	"/messages.json".

bc_comments_endpoint(User,Pass,AcctID,ProjID,MessageID) ->
	bc_project_endpoint(User, Pass, AcctID, ProjID) ++
	"/messages/" ++
	integer_to_list(MessageID) ++
	"/comments.json".

bc_topics_endpoint(User, Pass, AcctID, ProjID) ->
	bc_project_endpoint(User, Pass, AcctID, ProjID) ++
	"/topics.json".

create_new_irc_log_thread(User,Pass,AgentHdr,AcctID,ProjID,{YY,MM,DD}=Date) ->
	Tgt = bc_message_endpoint(User, Pass, AcctID, ProjID),
	MsgName = io_lib:format("#projecteight IRC Log: ~2.10.0B/~2.10.0B/~4.10.0B",[MM,DD,YY]),
	Topics = bc_topics_endpoint(User, Pass, AcctID, ProjID),
	lager:info("got topics"),
	case old_msg_exists(Topics, AgentHdr, list_to_binary(MsgName)) of
		false ->
			Msg = [{<<"subject">>,list_to_binary(MsgName)},
			{<<"content">>,<<"Daily IRC log digest, courtesy of erl9000.">>}],
			Req = {Tgt, [AgentHdr], "application/json", jsx:encode(Msg)},
			{ok, {201, Res}} = httpc:request(post, Req, [], httpc_get_options()),
			{ok, get_new_msg_id(Res)};
		MsgID ->
			{ok, MsgID}
	end.

old_msg_exists(Topics, AgentHdr, MsgName) ->
	old_msg_exists(Topics, AgentHdr, MsgName, 1).
old_msg_exists(Topics, AgentHdr, MsgName, N) ->
	lager:info("getting page ~p",[N]),
	{ok, Res} = get_topics_pg(Topics, AgentHdr, N),
	Parsed = jsx:decode(Res),
	lager:info("parsed down to ~p",[Parsed]),
	case Parsed of
		[] ->
			false;
		Other ->
			case msg_in_topics(MsgName,Parsed) of
				false ->
					old_msg_exists(Topics, AgentHdr, MsgName, N+1);
				MsgID ->
					lager:info("found it! ~p",[MsgID]),
					MsgID
			end
	end.

msg_in_topics(MsgName, []) ->
	false;
msg_in_topics(MsgName, [Topic|Rest]) ->
	lager:info("checking for msg (~p) in topics...",[MsgName]),
	case proplists:get_value(<<"title">>, Topic) of
		MsgName ->
			Topicable = proplists:get_value(<<"topicable">>,Topic),
			proplists:get_value(<<"id">>, Topicable);
		_Other ->
			msg_in_topics(MsgName, Rest)
	end.

get_topics_pg(Topics, AgentHdr, N) ->
	Tgt = Topics ++ "?page=" ++ integer_to_list(N),
	Req = {Tgt, [{"User-Agent","abc123"}]},
	lager:info("sending ~p",[Req]),
	{ok, {200, Res}} = httpc:request(get, Req,[],httpc_get_options()),
	{ok, Res}.

get_new_msg_id(Res) ->
	Native = jsx:decode(Res),
	proplists:get_value(<<"id">>, Native).

%%%%%%%%%%%%%
%%% EUNIT %%%
%%%%%%%%%%%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

form_check_url_test() ->
	User = "someuser",
	Pass = "abddef",
	AcctID = 12412,
	Tgt = "https://someuser:abddef@basecamp.com/12412/api/v1/projects.json",
	?assertEqual(form_check_url(User,Pass,AcctID),Tgt).

bc_proj_endpoint_test() ->
	User = "someuser",
	Pass = "abddef",
	AcctID = 12412,
	ProjID = 123531,
	Tgt = "https://someuser:abddef@basecamp.com/12412/api/v1/projects/123531",
	?assertEqual(bc_project_endpoint(User,Pass,AcctID,ProjID), Tgt).

bc_message_endpoint_test() ->
	User = "someuser",
	Pass = "abddef",
	AcctID = 12412,
	ProjID = 123531,
	Tgt = "https://someuser:abddef@basecamp.com/12412/api/v1/projects/123531/messages.json",
	?assertEqual(bc_message_endpoint(User,Pass,AcctID,ProjID), Tgt).

bc_comments_endpoint_test() ->
	User = "someuser",
	Pass = "abddef",
	AcctID = 12412,
	ProjID = 123531,
	MessageID = 5453453,
	Tgt = "https://someuser:abddef@basecamp.com/12412/api/v1/projects/123531/messages/5453453/comments.json",
	?assertEqual(bc_comments_endpoint(User,Pass,AcctID,ProjID,MessageID), Tgt).		


-endif.