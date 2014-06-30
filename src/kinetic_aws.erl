-module(kinetic_aws).

-export([sign_v4/7, bench/1]).

% On my Macbook Air from Mid 2013 I can sign about 150MB/sec of Payloads
% It's basically 3000 signatures per second with Body being 50KB on a single
% erlang process.
sign_v4(AccessKeyId, SecretAccessKey, Service, Region, Date, Target, Body) ->
    DateOnly = lists:sublist(Date, 8),
    EndPoint = kinetic_utils:endpoint(Service, Region),

    % Changes once a day
    Key0 = crypto:hmac(sha256, "AWS4" ++ SecretAccessKey, DateOnly),
    Key1 = crypto:hmac(sha256, Key0, Region),
    Key2 = crypto:hmac(sha256, Key1, Service),
    SigningKey = crypto:hmac(sha256, Key2, "aws4_request"),


    % Canonical Request
    CanonicalRequest = ["POST", $\n,
                        "/", $\n,
                        $\n,
                        "host:", EndPoint, $\n,
                        "x-amz-date:", Date, $\n,
                        "x-amz-target:", Target, $\n,
                        $\n,
                        "host;x-amz-date;x-amz-target", $\n,
                        hex_from_bin(crypto:hash(sha256, Body))],

    % String to sign
    StringToSign = ["AWS4-HMAC-SHA256", $\n,
                    Date, $\n,
                    % Credential scope
                    DateOnly, "/", Region, "/", Service, "/aws4_request", $\n,
                    % hash_encode of the Request
                    hex_from_bin(crypto:hash(sha256, CanonicalRequest))],


    % Signing
    Signature = hex_from_bin(crypto:hmac(sha256, SigningKey, StringToSign)),
    {ok,
      ["AWS4-HMAC-SHA256 Credential=",
       AccessKeyId, $/, DateOnly, $/, Region, $/, Service, "/aws4_request",
       ",SignedHeaders=host;x-amz-date;x-amz-target,Signature=",
       Signature]}.

bench(N) ->
    crypto:start(),
    % Start = erlang:now(),
    % run(N),
    % End = erlang:now(),
    % io:format("~p~n", [timer:now_diff(End, Start)]).
    S = list_to_binary(string:chars($a, 50000)),
    Start1 = erlang:now(),
    run2(N, S),
    End1 = erlang:now(),
    io:format("~p~n", [timer:now_diff(End1, Start1)]).

%% Internal

run2(0, V) ->
    sign_v4("BLABLABLA", "BLABLABLA", "kinesis", "us-east-1",
            "20140629T022822Z", "Kinesis_20131202.ListStreams",
            V);
run2(N, V) ->
    sign_v4("BLABLABLA", "BLABLABLA", "kinesis", "us-east-1",
            "20140629T022822Z", "Kinesis_20131202.ListStreams",
            V),
    run2(N-1, V).

hex_from_bin(Md5_bin) ->
    Md5_list = binary_to_list(Md5_bin),
    list_to_hex(Md5_list).
    % list_to_binary(lists:flatten(list_to_hex(Md5_list))).

list_to_hex(L) ->
    lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
    $0+N;
hex(N) when N >= 10, N < 16 ->
    $a + (N-10).

