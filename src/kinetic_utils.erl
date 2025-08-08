-module(kinetic_utils).

-export([pool_name/1, regions/0, region/1, endpoint/1, decode/1, encode/1]).

pool_name("us-east-1" ++ _R) ->
    'kinetic_pool_us-east-1';
pool_name("us-west-2" ++ _R) ->
    'kinetic_pool_us-west-2';
pool_name("ap-southeast-1" ++ _R) ->
    'kinetic_pool_ap-southeast-1';
pool_name("eu-west-1" ++ _R) ->
    'kinetic_pool_eu-west-1'.

regions() ->
    ["us-east-1", "us-west-2", "ap-southeast-1", "eu-west-1"].

region("us-east-1" ++ _R) ->
    "us-east-1";
region("us-west-2" ++ _R) ->
    "us-west-2";
region("ap-southeast-1" ++ _R) ->
    "ap-southeast-1";
region("eu-west-1" ++ _R) ->
    "eu-west-1".

endpoint("us-east-1") ->
    "kinesis.us-east-1.amazonaws.com";
endpoint("us-west-2") ->
    "kinesis.us-west-2.amazonaws.com";
endpoint("eu-west-1") ->
    "kinesis.eu-west-1.amazonaws.com";
endpoint("ap-southeast-1") ->
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
