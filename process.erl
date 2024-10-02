-module(process).
-include_lib("eunit/include/eunit.hrl").

-export([start/1]).

start(NodeId) ->
    StateName = "FOLLOWER",
    log_manager:log_information(NodeId, StateName, "Starting process...").
% raft_state:start(NodeId),
    % raft_state:stop(NodeId).

% start_election(RaftState) ->
    
