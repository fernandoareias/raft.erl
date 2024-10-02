-module(log_manager).

-export([log_information/3]).

log_information(NodeId, StateName, Message) ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime(
        {MegaSecs, Secs, MicroSecs}
    ),

    Timestamp = io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [
        Year, Month, Day, Hour, Minute, Second
    ]),

    %% Formatar a mensagem de log
    LogMessage = io_lib:format("[+][~s][PROCESSO ~p][~s] - ~s~n", [
        lists:flatten(Timestamp), NodeId, StateName, Message
    ]),

    %% Formatar o nome do arquivo
    FormattedFileName = io_lib:format("process_~s_logs.data", [integer_to_list(NodeId)]),
    FileName = lists:flatten(FormattedFileName),

    %% Usar o diretório /tmp/logs em vez de /logs
    LogDir = "/tmp/logs",
    FilePath = lists:flatten(io_lib:format("~s/~s", [LogDir, FileName])),

    %% Verificar se o diretório /tmp/logs/ existe, criar se não existir
    case file:list_dir(LogDir) of
        % Diretório não existe
        {error, enoent} ->
            case file:make_dir(LogDir) of
                ok ->
                    ok;
                {error, Reason} ->
                    io:format("Falha ao criar o diretório: ~p~n", [Reason]),
                    exit(Reason)
            end;
        % Diretório existe
        {ok, _} ->
            ok;
        {error, Reason} ->
            io:format("Falha ao verificar o diretório: ~p~n", [Reason]),
            exit(Reason)
    end,

    %% Abrir o arquivo para acrescentar
    {ok, File} = file:open(FilePath, [append]),

    %% Escrever a mensagem de log no arquivo
    io:fwrite(File, "~s", [lists:flatten(LogMessage)]),

    %% Fechar o arquivo
    file:close(File),

    %% Opcionalmente imprimir a mensagem de log no console
    io:format("~s", [lists:flatten(LogMessage)]).
