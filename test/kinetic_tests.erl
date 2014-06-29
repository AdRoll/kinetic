-module(kinetic_tests).


-include_lib("eunit/include/eunit.hrl").


% start_app() ->
%     application:start(crypto),
%     application:start(public_key),
%     ok = application:start(ssl),
%     ok = lhttpc:start(),
%     ok = kinetic:start().

% stop_app(_) ->
%     ok = kinetic:stop(),
%     ok = lhttpc:stop(),
%     ok = application:stop(ssl).


signature_test_() ->
    [
        ?_test(signature_valid())
    ].


%%
%% Tests
%%

signature_valid() ->
    {ok,{"Authorization",
     ["AWS4-HMAC-SHA256 Credential=","BLABLABLA",47,"20140629",47,"us-east-1",
      47,"kinesis","/aws4_request",
      ",SignedHeaders=host;x-amz-date;x-amz-target,Signature=",
      ["4d","d9","0e","c7","89","9f","0d","98","09","4e","f1","f8","8f","29",
       "cf","8f","45","ff","73","35","8b","22","bf","a0","5a","e5","36","36",
       "3e","18","ea","f4"]]}} =
     aws_signature:sign_v4("BLABLABLA", "BLABLABLA", "kinesis", "us-east-1",
                           "20140629T022822Z", "Kinesis_20131202.ListStreams",
                           "something"),

    {ok,{"Authorization",
     ["AWS4-HMAC-SHA256 Credential=","BLABLABLA",47,"20140629",47,"us-west-1",
      47,"kinesis","/aws4_request",
      ",SignedHeaders=host;x-amz-date;x-amz-target,Signature=", _]}} =
     aws_signature:sign_v4("BLABLABLA", "BLABLABLA", "kinesis", "us-west-1",
                           "20140629T022822Z", "Kinesis_20131202.ListStreams",
                           "something"),


    ok.
