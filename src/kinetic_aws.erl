-module(kinetic_aws).

-export([authorization_headers_v4/6, sign_v4/7]).

-include("kinetic.hrl").

authorization_headers_v4(AwsCreds, Service, Region, Date, Target, Body) ->

    #aws_credentials{secret_access_key = SecretAccessKey, access_key_id = AccessKeyId,
        security_token = SecurityToken} = AwsCreds,

    DateOnly = lists:sublist(Date, 8),
    EndPoint = kinetic_utils:endpoint(Service, Region),

    % Changes once a day
    Key0 = crypto:hmac(sha256, "AWS4" ++ SecretAccessKey, DateOnly),
    Key1 = crypto:hmac(sha256, Key0, Region),
    Key2 = crypto:hmac(sha256, Key1, Service),
    SigningKey = crypto:hmac(sha256, Key2, "aws4_request"),

    Headers = [
        {"Host", EndPoint},
        {"x-amz-date", Date},
        {"x-amz-target", Target}
        |
        case SecurityToken of
            undefined -> [];
            _ -> [{"x-amz-security-token", SecurityToken}]
        end
    ],

    NormalizedHeaders = [{string:to_lower(Name), Value} || {Name, Value} <-
        lists:keysort(1, Headers)],

    SignedHeaders = string:join([Name || {Name, _} <- NormalizedHeaders], ";"),

    % Canonical Request
    CanonicalRequest = ["POST", $\n,
        "/", $\n,
        $\n,
        [[string:to_lower(Key), $:, Value, $\n] || {Key, Value} <- NormalizedHeaders], $\n,
        SignedHeaders, $\n,
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
    AuthorizationHeader =
        ["AWS4-HMAC-SHA256 Credential=",
            AccessKeyId, $/, DateOnly, $/, Region, $/, Service, "/aws4_request",
            ",SignedHeaders=", SignedHeaders, ",Signature=",
            Signature],

    {ok, [{"Authorization", AuthorizationHeader} | Headers]}.



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
                        "host;x-amz-date;x-amz-security-token;x-amz-target", $\n,
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


%% Internal

hex_from_bin(Bin) ->
    List = binary_to_list(Bin),
    list_to_hex(List).

list_to_hex(L) ->
    lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
    $0+N;
hex(N) when N >= 10, N < 16 ->
    $a + (N-10).

