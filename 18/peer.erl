%% @author go717franciswang [https://github.com/go717franciswang]
%% @doc Peer to peer exercise in chapter 15 of the Programming Erlang book
%% @reference <a href="http://oreilly.com/catalog/9780596518189/">Erlang Programming</a>
-module(peer).
-export([start/0, connect/1, send/1, stop/0]).
-export([handle_call/3, handle_cast/2, init/1, terminate/2]).
-export([wait_connect/1, get_request/1]).
-behavior(gen_server).

%% @doc Start the peer to peer server, and create the listener socket.
-spec start() -> ok | {error, already_started}.
start() ->
    case gen_server:start_link({local, peer}, ?MODULE, [], []) of
        {ok, _} -> ok;
        _ -> {error, already_started}
    end.

%% @hidden
init(_) ->
    {ok, ListenSocket} = gen_tcp:listen(1234, [binary, {active, false}]),
    spawn(?MODULE, wait_connect, [ListenSocket]),
    {ok, {ListenSocket,false,false}}.

%% @hidden
wait_connect(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            Pid = spawn(?MODULE, get_request, [Socket]),
            gen_tcp:controlling_process(Socket, Pid),
            gen_server:cast(peer, {connected, Socket}),
            wait_connect(ListenSocket);
        {error, closed} -> ok
    end.

%% @hidden
get_request(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Binary} ->
            io:format("Got message: ~s~n", [binary_to_list(Binary)]),
            get_request(Socket);
        {error, closed} ->
            gen_server:cast(disconnected),
            io:format("Connection to client is closed~n");
        Other ->
            io:format("Unknown msg: ~w~n", [Other])
    end.

%% @doc Stop the peer to peer server, and close all sockets.
-spec stop() -> ok | {error, not_started}.
stop() -> 
    case whereis(peer) of
        undefined -> {error, not_started};
        _ -> gen_server:cast(peer, stop), ok
    end.

%% @doc Connect to a remote peer by IP address.
-spec connect(IpAddress::string()) -> ok | {error, Reason::atom()}.
connect(Ip) -> gen_server:call(peer, {connect, Ip}).

%% @doc Send a message to the connected remote peer.
-spec send(String::string()) -> ok | {error, not_connected}.
send(String) -> gen_server:call(peer, {send, String}).

%% @hidden
handle_cast(stop, LoopData) ->
    {stop, normal, LoopData};
handle_cast(disconnected, {ListenSocket, SendSocket, _}) ->
    {noreply, {ListenSocket, SendSocket, false}};
handle_cast({connected, Socket}, {ListenSocket, SendSocket, _}) ->
    {noreply, {ListenSocket, SendSocket, Socket}}.

%% @hidden
terminate(_Reason, {ListenSocket, SendSocket, RcvSocket}) ->
    closeSocket(ListenSocket),
    closeSocket(SendSocket),
    closeSocket(RcvSocket).

closeSocket(false) -> ok;
closeSocket(Socket) -> gen_tcp:close(Socket).

%% @hidden
handle_call({connect, Ip}, _From, {ListenSocket, _, RcvSocket}=LoopData) ->
    case gen_tcp:connect(Ip, 1234, [binary, {active, false}]) of
        {ok, Socket} -> {reply, ok, {ListenSocket, Socket, RcvSocket}};
        {error, Reason} -> {reply, {error,Reason}, LoopData}
    end;
handle_call({send, String}, _From, {_, SendSocket, _}=LoopData) ->
    case SendSocket of
        false -> {reply, {error, not_connected}, LoopData};
        _ -> 
            gen_tcp:send(SendSocket, String), 
            {reply, ok, LoopData}
    end.

