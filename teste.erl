-module(teste).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_cast/2, handle_call/3, code_change/3, terminate/2]).
-export([register/1, get_cat_list/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register(Cat) ->
    gen_server:cast(?MODULE, {register, Cat}).

get_cat_list() ->
    gen_server:call(?MODULE, get_cat_list).

init([]) ->
    {ok, []}.

handle_cast({register, Cat}, CatList) ->
    NewCatList = [Cat | CatList],
    {noreply, NewCatList}.

handle_call(get_cat_list, _From, CatList) ->
    {reply, {ok, CatList}, CatList}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
