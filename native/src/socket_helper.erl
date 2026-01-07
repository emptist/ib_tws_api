-module(socket_helper).
-export([connect/2, send/2, recv/3, close/1]).

%% Connect to a TCP server
%% Returns {ok, Socket} or {error, Reason}
connect(Host, Port) when is_list(Host), is_integer(Port) ->
    case inet:parse_address(Host) of
        {ok, IpAddress} ->
            Options = [binary, {packet, raw}, {active, false}, {reuseaddr, true}],
            case gen_tcp:connect(IpAddress, Port, Options, 10000) of
                {ok, Socket} -> {ok, Socket};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Send data to socket
%% Returns ok or {error, Reason}
send(Socket, Data) when is_binary(Data) ->
    case gen_tcp:send(Socket, Data) of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% Receive data from socket
%% Returns {ok, Data} or {error, Reason}
recv(Socket, Length, Timeout) when is_integer(Length), is_integer(Timeout) ->
    case gen_tcp:recv(Socket, Length, Timeout) of
        {ok, Data} -> {ok, Data};
        {error, Reason} -> {error, Reason}
    end.

%% Close the socket
%% Returns ok
close(Socket) ->
    gen_tcp:close(Socket),
    ok.