-module(peer).
-export([start/0, connect/1, send/1, stop/0]).
-export([handle_call/3, handle_cast/2, init/1, terminate/2]).
-export([wait_connect/1, get_request/1]).
-behavior(gen_server).

start() ->
    case gen_server:start_link({local, peer}, ?MODULE, [], []) of
        {ok, _} -> ok;
        _ -> {error, already_started}
    end.

init(_) ->
    {ok, ListenSocket} = gen_tcp:listen(1234, [binary, {active, false}]),
    spawn(?MODULE, wait_connect, [ListenSocket]),
    {ok, {ListenSocket,false,false}}.

wait_connect(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    Pid = spawn(?MODULE, get_request, [Socket]),
    gen_tcp:controlling_process(Socket, Pid),
    gen_server:cast(peer, {connected, Socket}),
    wait_connect(ListenSocket).

get_request(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Binary} ->
            io:format("Got message: ~s~n", [binary_to_list(Binary)]),
            get_request(Socket);
        {error, closed} ->
            io:format("Connection to client is closed~n");
        Other ->
            io:format("Unknown msg: ~w~n", [Other])
    end.

stop() -> gen_server:cast(peer, stop).
connect(Ip) -> gen_server:call(peer, {connect, Ip}).
send(String) -> gen_server:call(peer, {send, String}).

handle_cast(stop, _) ->
    {stop, normal};
handle_cast({connected, Socket}, {ListenSocket, SendSocket, _}) ->
    {noreply, {ListenSocket, SendSocket, Socket}}.

terminate(_Reason, {ListenSocket, Socket}) ->
    gen_tcp:close(ListenSocket),
    gen_tcp:close(Socket).

handle_call({connect, Ip}, _From, {ListenSocket, _, RcvSocket}) ->
    {ok, Socket} = gen_tcp:connect(Ip, 1234, [binary, {active, false}]),
    {reply, ok, {ListenSocket, Socket, RcvSocket}};
handle_call({send, String}, _From, {ListenSocket, SendSocket, RcvSocket}) ->
    gen_tcp:send(SendSocket, String),
    {reply, ok, {ListenSocket, SendSocket, RcvSocket}}.

