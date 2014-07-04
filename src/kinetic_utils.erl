-module(kinetic_utils).

-export([fetch_and_return_url/1, fetch_and_return_url/2]).
-export([endpoint/2, decode/1, encode/1]).

endpoint("kinesis", "us-east-1") -> "kinesis.us-east-1.amazonaws.com";
endpoint("kinesis", "us-west-1") -> "kinesis.us-west-1.amazonaws.com";
endpoint("kinesis", "us-west-2") -> "kinesis.us-west-2.amazonaws.com";
endpoint("kinesis", "eu-west-1") -> "kinesis.eu-west-1.amazonaws.com";
endpoint("kinesis", "ap-northeast-1") -> "kinesis.ap-northeast-1.amazonaws.com";
endpoint("kinesis", "ap-southeast-1") -> "kinesis.ap-southeast-1.amazonaws.com".


fetch_and_return_url(Url) ->
    fetch_and_return_url(Url, json).
fetch_and_return_url(Url, json) ->
    case fetch_and_return_body(Url) of
        {ok, Body} ->
            {ok, decode(Body)};

        {error, E} ->
            {error, E}
    end;
fetch_and_return_url(Url, text) ->
    fetch_and_return_body(Url).

decode(<<"">>) ->
    [];
decode(Body) ->
    try jiffy:decode(Body) of
        {Decoded} -> % enforces the dictionary
            Decoded;

        _ ->
            {error, not_a_dict}
    catch
        {error, E} ->
            {error, E}
    end.

encode(Body) ->
    try jiffy:encode(Body)
    catch
        {error, E} ->
            {error, E}
    end.

% Internal

fetch_and_return_body(Url) ->
    case httpc:request(get, {Url, http_client_headers()}, http_options(), []) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            {ok, Body};

        {ok, {{_, Code, _}, _Headers, _Body}} ->
            {error, Code};

        {error, Reason} ->
            error_logger:error_msg("~p~n", [Reason]),
            {error, Reason}
    end.

http_options() ->
    [{timeout, 200000}, {relaxed, true}].

http_client_headers() ->
    [{"Connection", "close"}].
