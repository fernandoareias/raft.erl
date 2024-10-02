-module(teste2).

-export([init/0, change_status/2, state_machine/1]).

init() ->
    io:format("[INFO] - Init server... ~n"),
    Status = "FOLLOWER",
    spawn(teste2, state_machine, [Status]).

change_status(NewState, Pid) ->
    io:format("[INFO] - Sending new state to fsm... ~n"),
    Pid ! {state, NewState}.

state_machine(State) ->
    receive
        {state, NewState} ->
            io:format("[INFO] - Updating server"),
            state_machine(NewState);
        terminate ->
            exit({custom_exit, "[INFO] - End server ~n"})
    after 5000 ->
        ok = io:format("[INFO] State ~s ~n", [State]),
        state_machine(State)
    end.
