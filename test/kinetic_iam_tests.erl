-module(kinetic_iam_tests).

-include("kinetic.hrl").
-include_lib("eunit/include/eunit.hrl").

test_setup() ->
    meck:new(kinetic_utils),
    meck:expect(kinetic_utils, fetch_and_return_url,
        fun("code_error" ++ Rest) ->
               case string:str(Rest, ?IAM_ROLE_URL) of
                   0 ->
                       % This is the credentials URL
                       {ok, [{<<"Code">>,<<"Broken">>},
                             {<<"AccessKeyId">>,<<"SOMERANDOMVALUE">>},
                             {<<"SecretAccessKey">>, <<"SomeSecretAccessKey">>},
                             {<<"Token">>, <<"SomeToken">>},
                             {<<"Expiration">>,<<"2014-06-30T02:23:41Z">>}]};
                   _ ->
                       {ok, [{<<"Code">>,<<"Broken">>},
                             {<<"LastUpdated">>,<<"2014-06-29T19:55:22Z">>},
                             {<<"InstanceProfileArn">>,
                              <<"arn:aws:iam::SOMEACCOUNT_ID:instance-profile/something">>},
                             {<<"InstanceProfileId">>,<<"SOMEPROFILE_ID">>}]}
               end;
           ("error" ++ Rest) ->
               case string:str(Rest, ?IAM_ROLE_URL) of
                   0 ->
                       % This is the credentials URL
                       {error, tokens};
                   _ ->
                       {error, role_stuff}
               end;
           (Url) ->
               case string:str(Url, ?IAM_ROLE_URL) of
                   0 ->
                       % This is the credentials URL
                       {ok, [{<<"Code">>,<<"Success">>},
                             {<<"AccessKeyId">>,<<"SOMERANDOMVALUE">>},
                             {<<"SecretAccessKey">>, <<"SomeSecretAccessKey">>},
                             {<<"Token">>, <<"SomeToken">>},
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

test_fetching_keys_with_role() ->
    {ok, #aws_credentials{access_key_id = "SOMERANDOMVALUE",
     secret_access_key = "SomeSecretAccessKey",
     security_token = "SomeToken",
     expiration_seconds = 63571314221}} =
            kinetic_iam:get_aws_keys("http://some_url", "something"),
    {error, no_credentials_found} = kinetic_iam:get_aws_keys("code_error", "something"),
    {error, _} = kinetic_iam:get_aws_keys("code_error", "something"),
    {error, tokens} = kinetic_iam:get_aws_keys("error", "something"),
    ok.

test_fetching_keys() ->
    {ok,#aws_credentials{access_key_id = "SOMERANDOMVALUE",
        secret_access_key = "SomeSecretAccessKey",
        security_token = "SomeToken",
        expiration_seconds = 63571314221}} =
            kinetic_iam:get_aws_keys("http://some_url"),
    {ok,#aws_credentials{access_key_id = "SOMERANDOMVALUE",
        secret_access_key = "SomeSecretAccessKey",
        security_token = "SomeToken",
        expiration_seconds = 63571314221}} =
            kinetic_iam:get_aws_keys("http://some_url", undefined),
    {error, role_stuff} = kinetic_iam:get_aws_keys("error"),
    {error, no_success} = kinetic_iam:get_aws_keys("code_error"),
    ok.

