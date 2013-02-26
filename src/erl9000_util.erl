-module(erl9000_util).
-export([buildatom/2, bcat/1]).

buildatom(A,B) when is_atom(A) andalso is_integer(B) ->
	IL = erlang:integer_to_list(B),
	erlang:list_to_atom(erlang:atom_to_list(A) ++ IL).

bcat(Arg) ->
	bcat(Arg,<<>>).
bcat([],Acc) ->
	Acc;
bcat([B1|Rest],Acc) ->
	bcat(Rest, <<Acc/binary, B1/binary>>).

%%%%%%%%%%%%%
%%% EUNIT %%%
%%%%%%%%%%%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

bcat_test() ->
	Bins = [<<"a">>, <<"b">>, <<"c">>],
	?assertEqual(<<"abc">>, bcat(Bins)).

-endif.