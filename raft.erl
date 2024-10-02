-module(raft).

% imports
-behaviour(gen_server).

% Exports
-export([init/2]).
-export([start_election/2, state_machine/1, election/1]).

% Constantes
-define(FOLLOWER, follower).
-define(CANDIDATE, candidate).
-define(LEADER, leader).

-define(ELECTION_TIMEOUT_MIN, 150).
-define(ELECTION_TIMEOUT_MAX, 300).

% Records
-record(raft_state, {
    node_id,
    state = ?FOLLOWER,
    current_term = 0,
    voted_for = undefined,
    votes_received = 0,
    election_timer = undefined,
    peers = []
}).

init(NodeId, [Peers]) ->
    RaftState = #raft_state{node_id = NodeId, peers = Peers},
    register(machine_state, spawn(raft, state_machine, [RaftState])),
    register(election, spawn(raft, start_election, [])).

start_election() ->

    receive 
        {restart} ->
            start_election();
    end.

    {ok, State} = machine_state ! {get_state},

    % Gera um atraso aleatório entre 1000 e 5000 milissegundos
    RandomDelay = rand:uniform(4000) + 1000,
    erlang:send_after(RandomDelay, self(), {election}),
    % Continua a execução normalmente
    log_manager:log_information(
        State#raft_state.node_id,
        State#raft_state.state,
        "Aguardando ~p milissegundos antes de executar a proxima eleicao.~n",
        [RandomDelay]
    ).

election() ->
    {ok, State} = machine_state ! {get_state},
    log_manager:log_information(
        State#raft_state.node_id,
        State#raft_state.state,
        "[ELEICAO] - Consultou estado ~n"
    ),
    timer:sleep(5000).

state_machine(State) ->
    receive
        {state, NewState} ->
            io:format("[INFO] - Updating server"),
            state_machine(NewState);
        {get_state} ->
            {ok, State};
        terminate ->
            exit({custom_exit, "[INFO] - End server ~n"})
    after 5000 ->
        ok = io:format("[INFO] State ~s ~n", [State]),
        state_machine(State)
    end.
