-module(kinetic_aws_tests).

-include("kinetic.hrl").
-include_lib("eunit/include/eunit.hrl").


signature_test_() ->
    [
        ?_test(test_signature_valid()),
        ?_test(test_valid_authorization_headers())
    ].


%%
%% Tests
%%

test_valid_authorization_headers() ->
    {ok,[{"Authorization",
        ["AWS4-HMAC-SHA256 Credential=","accesskey",47,"20140629",47,
            "us-east-1",47,"kinesis","/aws4_request",",SignedHeaders=",
            "host;x-amz-date;x-amz-security-token;x-amz-target",",Signature=",
            ["84","7f","ee","48","56","82","98","91","17","72","35","6f","e3","32",
                "44","3b","f2","67","9c","48","fd","42","69","5a","84","aa","a0","d0",
                "e7","f2","8c","66"]]},
        {"Host","kinesis.us-east-1.amazonaws.com"},
        {"x-amz-date","20140629T022822Z"},
        {"x-amz-target","Kinesis_20131202.ListStreams"},
        {"x-amz-security-token","securitytoken"}]}
        =
        kinetic_aws:authorization_headers_v4({aws_credentials, "accesskey", "secretkey",
            "securitytoken", undefined}, "kinesis", "us-east-1",
            "20140629T022822Z", "Kinesis_20131202.ListStreams", "something"),


    {ok,[{"Authorization",
        ["AWS4-HMAC-SHA256 Credential=","accesskey",47,"20140629",47,
            "us-east-1",47,"kinesis","/aws4_request",",SignedHeaders=",
            "host;x-amz-date;x-amz-target",",Signature=",
            ["5d","8f","2b","69","6f","37","0e","69","13","c1","29","6e","20","45",
                "c5","81","73","ec","1e","06","43","ca","0a","f3","5f","d6","6e","13",
                "dd","78","f2","7d"]]},
        {"Host","kinesis.us-east-1.amazonaws.com"},
        {"x-amz-date","20140629T022822Z"},
        {"x-amz-target","Kinesis_20131202.ListStreams"}]}
        =
        kinetic_aws:authorization_headers_v4({aws_credentials, "accesskey", "secretkey",
            undefined, undefined}, "kinesis", "us-east-1", "20140629T022822Z",
            "Kinesis_20131202.ListStreams", "something"),

    kinetic_aws:bench(100),
    kinetic_aws:bench_headers(100),

    ok.

test_signature_valid() ->
    {ok,
     ["AWS4-HMAC-SHA256 Credential=","BLABLABLA",47,"20140629",47,"us-east-1",
      47,"kinesis","/aws4_request",
      ",SignedHeaders=host;x-amz-date;x-amz-target,Signature=",
      ["5e","0d","38","47","b0","ec","5c","3e","92","ba","2f","ff","ea","30",
       "58","32","c3","f4","55","f2","0a","a3","31","5e","b6","1c","5e","95",
       "c8","d0","43","91"]]} =
     kinetic_aws:sign_v4("BLABLABLA", "BLABLABLA", "kinesis", "us-east-1",
                           "20140629T022822Z", "Kinesis_20131202.ListStreams",
                           "something"),

    {ok,
     ["AWS4-HMAC-SHA256 Credential=","BLABLABLA",47,"20140629",47,"us-west-1",
      47,"kinesis","/aws4_request",
      ",SignedHeaders=host;x-amz-date;x-amz-target,Signature=", _]} =
     kinetic_aws:sign_v4("BLABLABLA", "BLABLABLA", "kinesis", "us-west-1",
                           "20140629T022822Z", "Kinesis_20131202.ListStreams",
                           "something"),
    kinetic_aws:bench(1),
    ok.


