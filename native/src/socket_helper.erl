-module(socket_helper).
-export([connect/2, set_passive/1]).

connect(Address, Port) ->
    gen_tcp:connect(Address, Port, [binary, {packet, raw}, {active, false}]).

set_passive(Socket) ->
    inet:setopts(Socket, [{active, false}]).
