-module(kinetic_iso8601).

-export([format/1, format_basic/1, parse/1]).

%% API
format_basic({{Y,Mo,D}, {H,Mn,S}}) ->
    FmtStr = "~4.10.0B~2.10.0B~2.10.0BT~2.10.0B~2.10.0B~2.10.0BZ",
    IsoStr = io_lib:format(FmtStr, [Y, Mo, D, H, Mn, S]),
    binary_to_list(list_to_binary(IsoStr)).

format({{Y,Mo,D}, {H,Mn,S}}) ->
    FmtStr = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
    IsoStr = io_lib:format(FmtStr, [Y, Mo, D, H, Mn, S]),
    binary_to_list(list_to_binary(IsoStr)).

parse(DateValue) ->
    case io_lib:fread("~4d-~2d-~2dT~2d:~2d:~2dZ", DateValue) of
        {ok, [Year, Month, Day, Hour, Minute, Second], _} ->
            {{Year, Month, Day}, {Hour, Minute, Second}}
    end.


