-module(fsm).

-behaviour(gen_fsm).

-export([init/0]).


init() ->
	{ok, follower, {[], []}}.

start_link() ->
	gen_fsm:start_link({local, node_raft}, ?MODULE, [], []),
	ElectionPid = spawn(talk, election, []),
	.

get_state() ->
	try
		gen_fsm:sync_send_event(node_raft, get_state)
	catch 
		exit:{noproc, _} -> closed
	end.

election() -> 
	



%% Lida com a finalização normal e desligamento do FSM
terminate(normal, _StateName, _State) ->
	do_clean_up(),
	ok;

terminate(shutdown, _StateName, _State) ->
	do_clean_up(),
	ok.

