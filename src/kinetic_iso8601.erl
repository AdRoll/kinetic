% Copyright (c) 2012 Sean Sawyer
% Copyright (c) 2014 Valentino Volonghi

% Permission is hereby granted, free of charge, to any person obtaining a copy of
% this software and associated documentation files (the "Software"), to deal in
% the Software without restriction, including without limitation the rights to
% use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
% the Software, and to permit persons to whom the Software is furnished to do so,
% subject to the following conditions:

% The above copyright notice and this permission notice shall be included in all
% copies or substantial portions of the Software.

% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
% FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
% COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
% IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-module(kinetic_iso8601).

-export([add_time/4,
         format/1,
         format_basic/1,
         parse/1]).

-export_types([datetime/0,
               timestamp/0]).

-define(MIDNIGHT, {0,0,0}).
-define(V, proplists:get_value).

-type datetime() :: tuple(calendar:date(), calendar:time()).
-type timestamp() :: tuple(integer(), integer(), integer()).

%% API

-spec add_time (datetime(), integer(), integer(), integer()) -> datetime().
%% @doc Add some time to the supplied `datetime()'.
add_time(Datetime, H, M, S) ->
    apply_offset(Datetime, H, M, S).

-spec format (datetime() | timestamp()) -> binary().
%% @doc Convert a `util:timestamp()' or a calendar-style `{date(), time()}'
%% tuple to an ISO 8601 formatted string. Note that this function always
%% returns a string with no offset (i.e., ending in "Z").
format({_,_,_}=Timestamp) ->
    format(calendar:now_to_datetime(Timestamp));
format({{Y,Mo,D}, {H,Mn,S}}) ->
    FmtStr = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
    IsoStr = io_lib:format(FmtStr, [Y, Mo, D, H, Mn, S]),
    list_to_binary(IsoStr).
format_basic({{Y,Mo,D}, {H,Mn,S}}) ->
    FmtStr = "~4.10.0B~2.10.0B~2.10.0BT~2.10.0B~2.10.0B~2.10.0BZ",
    IsoStr = io_lib:format(FmtStr, [Y, Mo, D, H, Mn, S]),
    binary_to_list(list_to_binary(IsoStr)).

-spec parse (string()) -> datetime().
%% @doc Convert an ISO 8601 formatted string to a 
parse(Bin) when is_binary(Bin) ->
    parse(binary_to_list(Bin));
parse(Str) ->
    year(Str, []).

%% Private functions

year([Y1,Y2,Y3,Y4|Rest], Acc) ->
    acc([Y1,Y2,Y3,Y4], Rest, year, Acc, fun month/2);
year(_, _) ->
    erlang:error(badarg).

month([], Acc) ->
    datetime(Acc);
month([$-,M1,M2|Rest], Acc) ->
    acc([M1,M2], Rest, month, Acc, fun day/2);
month(_, _) ->
    erlang:error(badarg).

day([], Acc) ->
    datetime(Acc);
day([$-,D1,D2|Rest], Acc) ->
    acc([D1,D2], Rest, day, Acc, fun hour/2);
day(_, _) ->
    erlang:error(badarg).

hour([], Acc) ->
    datetime(Acc);
hour([$T,H1,H2|Rest], Acc) ->
    acc([H1,H2], Rest, hour, Acc, fun minute/2);
hour(_, _) ->
    erlang:error(badarg).

minute([], Acc) ->
    datetime(Acc);
minute([$:,M1,M2|Rest], Acc) ->
    acc([M1,M2], Rest, minute, Acc, fun second/2);
minute(_, _) ->
    erlang:error(badarg).

second([], Acc) ->
    datetime(Acc);
second([$:,S1,S2|Rest], Acc) ->
    acc([S1,S2], Rest, second, Acc, fun microsecond/2);
second(_, _) ->
    erlang:error(badarg).

microsecond([], Acc) ->
    datetime(Acc);
microsecond([$., S1, S2, S3|Rest], Acc) ->
    acc([S1, S2, S3], Rest, microsecond, Acc, fun offset_hour/2);
microsecond(Rest, Acc) ->
    offset_hour(Rest, Acc).

offset_hour([], Acc) ->
    datetime(Acc);
offset_hour([$Z], Acc) ->
    acc([$0], [], offset, Acc, fun datetime/2);
offset_hour([$+,H1,H2|Rest], Acc) ->
    acc([H1,H2], Rest, offset_hour, Acc, fun offset_minute/2);
offset_hour([$-,H1,H2|Rest], Acc) ->
    acc([H1,H2], Rest, offset_hour, [{offset_sign, -1}|Acc], fun offset_minute/2);
offset_hour([_|Rest], Acc) ->
    offset_hour(Rest, Acc).

offset_minute([], Acc) ->
    datetime(Acc);
offset_minute([M1,M2], Acc) ->
    acc([M1,M2], [], offset_minute, Acc, fun datetime/2);
offset_minute([$:,M1,M2], Acc) ->
    acc([M1,M2], [], offset_minute, Acc, fun datetime/2);
offset_minute(_, _) ->
    erlang:error(badarg).

acc(IntStr, Rest, Key, Acc, NextF) ->
    Acc1 = [{Key, erlang:list_to_integer(IntStr)}|Acc],
    NextF(Rest, Acc1).

datetime(Plist) ->
    Year = ?V(year, Plist),
    Year =/= undefined orelse erlang:error(badarg),
    Date = {Year, ?V(month, Plist, 1), ?V(day, Plist, 1)},
    Time = {?V(hour, Plist, 0), ?V(minute, Plist, 0), ?V(second, Plist, 0)},
    OffsetSign = ?V(offset_sign, Plist, 1),
    OffsetH = OffsetSign * ?V(offset_hour, Plist, 0),
    OffsetM = OffsetSign * ?V(offset_minute, Plist, 0),
    apply_offset({Date, Time}, OffsetH, OffsetM, 0).

datetime(_, Plist) ->
    datetime(Plist).

apply_offset(Datetime, H, M, S) ->
    OffsetS = S + (60 * (M + (60 * H))),
    Gs = OffsetS + calendar:datetime_to_gregorian_seconds(Datetime),
    calendar:gregorian_seconds_to_datetime(Gs).
