-module(kinetic_aws_tests).

-include("kinetic.hrl").
-include_lib("eunit/include/eunit.hrl").


signature_test_() ->
    [
        ?_test(test_signature_valid())
    ].


%%
%% Tests
%%

test_signature_valid() ->
    {ok,
     ["AWS4-HMAC-SHA256 Credential=","BLABLABLA",47,"20140629",47,"us-east-1",
      47,"kinesis","/aws4_request",
      ",SignedHeaders=host;x-amz-date;x-amz-target,Signature=",
      ["4d","d9","0e","c7","89","9f","0d","98","09","4e","f1","f8","8f","29",
       "cf","8f","45","ff","73","35","8b","22","bf","a0","5a","e5","36","36",
       "3e","18","ea","f4"]]} =
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


