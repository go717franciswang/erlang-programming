-module(e2).
-export([server/0]).
-export([get_request/2]).

server() ->
    inets:start(),
    {ok, ListenSocket} = gen_tcp:listen(1500, [binary, {active, false}]),
    wait_connect(ListenSocket).

wait_connect(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    Pid = spawn(?MODULE, get_request, [Socket, []]),
    gen_tcp:controlling_process(Socket, Pid),
    wait_connect(ListenSocket).

get_request(Socket, BinaryList) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, Binary} ->
            handle([Binary], Socket);
            % get_request(Socket, [Binary|BinaryList]);
        {error, closed} ->
            handle(lists:reverse(BinaryList), Socket);
        {error, timeout} ->
            handle(lists:reverse(BinaryList), Socket)
    end.

handle(Binary, Socket) ->
    Chunk = lists:map(fun(X) -> binary_to_list(X) end, Binary),
    Payload = string:join(Chunk, ""),

    [Req|_Rest] = string:tokens(Payload, "\r\n"),
    [Method, Path, _Version] = string:tokens(Req, " "),
    case Method of
        "GET" ->
            respond(Socket, Path);
        _ ->
            ignored
    end,
    gen_tcp:close(Socket).

respond(Socket, Url) ->
    io:format("Requesting URL: ~s~n", [Url]),
    {ok, Result} = httpc:request(Url),
    Response = buildResponse(Result),
    send(Socket, Response).

buildResponse({{Version,Code,CodeStr}, Headers, Payload}) ->
    H = string:join(lists:map(fun({K,V}) -> K++" "++V end, Headers), "\r\n"),
    S = Version++" "++integer_to_list(Code)++" "++CodeStr++"\r\n"++H++"\r\n\r\n"++Payload,
    list_to_binary(S).

send(Socket, <<Chunk:100/binary, Rest/binary>>) ->
    gen_tcp:send(Socket, Chunk),
    send(Socket, Rest);
send(Socket, Rest) ->
    gen_tcp:send(Socket, Rest).



