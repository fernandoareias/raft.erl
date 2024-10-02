-module(dns).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_cast/2, handle_call/3, code_change/3, terminate/2]).
-export([register/2, lookup/1, get_dns_list/0]).

-record(dns_entry, {name, ip}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    log_message("Iniciando servidor...").

register(Name, IP) ->
    gen_server:cast(?MODULE, {register, Name, IP}).

lookup(Name) ->
    gen_server:call(?MODULE, {lookup, Name}).

get_dns_list() ->
    gen_server:call(?MODULE, get_dns_list).

init([]) ->
    {ok, []}.

handle_cast({register, Name, IP}, DNSList) ->
    case
        lists:any(
            fun(#dns_entry{name = Name2, ip = IP2}) -> Name =:= Name2 orelse IP =:= IP2 end, DNSList
        )
    of
        true ->
            log_message(io_lib:format("O IP ~s ou o DNS ~s ja esta registrado", [IP, Name])),
            {noreply, DNSList};
        false ->
            NewEntry = #dns_entry{name = Name, ip = IP},
            NewDNSList = [NewEntry | DNSList],
            log_message(io_lib:format("Registrando ~s com IP ~s", [Name, IP])),
            {noreply, NewDNSList}
    end.

handle_call({lookup, Name}, _From, DNSList) ->
    case lists:keyfind(Name, #dns_entry.name, DNSList) of
        false -> {reply, {error, not_found}, DNSList};
        #dns_entry{name = Name, ip = IP} -> {reply, {ok, IP}, DNSList}
    end;
handle_call(get_dns_list, _From, DNSList) ->
    {reply, {ok, DNSList}, DNSList}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

log_message(Message) ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime(
        {MegaSecs, Secs, MicroSecs}
    ),

    Timestamp = io_lib:format(
        "~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
        [Year, Month, Day, Hour, Minute, Second]
    ),

    LogMessage = io_lib:format(
        "[+][~s][DNS SERVER][~p] - ~s~n",
        [lists:flatten(Timestamp), self(), Message]
    ),

    {ok, File} = file:open("dns_logs.data", [append]),
    io:fwrite(File, "~s", [lists:flatten(LogMessage)]),
    file:close(File),

    io:format("~s", [lists:flatten(LogMessage)]).
