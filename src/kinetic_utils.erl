-module(kinetic_utils).

-export([endpoint/2, decode/1, encode/1]).

endpoint("kinesis", "us-east-1") ->
    "kinesis.us-east-1.amazonaws.com";
endpoint("kinesis", "us-west-1") ->
    "kinesis.us-west-1.amazonaws.com";
endpoint("kinesis", "us-west-2") ->
    "kinesis.us-west-2.amazonaws.com";
endpoint("kinesis", "eu-west-1") ->
    "kinesis.eu-west-1.amazonaws.com";
endpoint("kinesis", "ap-northeast-1") ->
    "kinesis.ap-northeast-1.amazonaws.com";
endpoint("kinesis", "ap-southeast-1") ->
    "kinesis.ap-southeast-1.amazonaws.com".

decode(<<"">>) ->
    [];
decode(Body) ->
    try jiffy:decode(Body) of
        {Decoded} -> % enforces the dictionary
            Decoded;
        _ ->
            {error, not_a_dict}
    catch
        _:E ->
            {error, E}
    end.

encode(Body) ->
    try
        jiffy:encode(Body)
    catch
        _:E ->
            {error, E}
    end.
