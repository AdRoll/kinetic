-module(kinetic_tests).

-include("kinetic.hrl").
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


test_setup() ->
    meck:new(kinetic_utils),
    meck:expect(kinetic_utils, fetch_and_return_url, fun(Url) ->
                case string:str(Url, ?IAM_ROLE_URL) of
                    0 ->
                        % This is the credentials URL
                        {ok, [{<<"Code">>,<<"Success">>},
                              {<<"AccessKeyId">>,<<"SOMERANDOMVALUE">>},
                              {<<"SecretAccessKey">>, <<"SomeSecretAccessKey">>},
                              {<<"Expiration">>,<<"2014-06-30T02:23:41Z">>}]};
                    _ ->
                        {ok, [{<<"Code">>,<<"Success">>},
                              {<<"LastUpdated">>,<<"2014-06-29T19:55:22Z">>},
                              {<<"InstanceProfileArn">>,
                               <<"arn:aws:iam::SOMEACCOUNT_ID:instance-profile/something">>},
                              {<<"InstanceProfileId">>,<<"SOMEPROFILE_ID">>}]}
                end
        end).

test_teardown(_) ->
    meck:unload(kinetic_utils).

iam_fetching_test_() ->
    {inorder,
        {setup,
            fun test_setup/0,
            fun test_teardown/1,
            [
                ?_test(test_fetching_keys_with_role()),
                ?_test(test_fetching_keys())
            ]
        }
    }.

signature_test_() ->
    [
        ?_test(test_signature_valid())
    ].


%%
%% Tests
%%
test_fetching_keys_with_role() ->
    {ok,{<<"SOMERANDOMVALUE">>,
     <<"SomeSecretAccessKey">>,
     <<"2014-06-30T02:23:41Z">>}} =
            kinetic_iam:get_aws_keys("http://some_url", "something"),
    ok.

test_fetching_keys() ->
    {ok,{<<"SOMERANDOMVALUE">>,
     <<"SomeSecretAccessKey">>,
     <<"2014-06-30T02:23:41Z">>}} =
            kinetic_iam:get_aws_keys("http://some_url"),
    ok.

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

    ok.


