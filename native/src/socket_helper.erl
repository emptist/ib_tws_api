-module(socket_helper).
-export([connect/3, set_passive/1]).

connect(Address, Port, Options) ->
    gen_tcp:connect(Address, Port, Options).

set_passive(Socket) ->
    inet:setopts(Socket, [{active, false}]).
