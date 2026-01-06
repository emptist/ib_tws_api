-module(socket_options).
-export([make_option/2, make_packet_option/1, make_active_option/1, make_options/2]).

make_option(Name, Value) when is_list(Name) ->
    list_to_tuple([list_to_atom(Name), Value]).

make_packet_option(Mode) when is_integer(Mode) ->
    make_option("packet", Mode).

make_active_option(Mode) when is_integer(Mode) ->
    Bool = case Mode of
        0 -> false;
        1 -> true
    end,
    make_option("active", Bool).

make_options(PacketMode, ActiveMode) when is_integer(PacketMode), is_integer(ActiveMode) ->
    [make_packet_option(PacketMode), make_active_option(ActiveMode)].
