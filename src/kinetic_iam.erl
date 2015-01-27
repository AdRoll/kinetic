-module(kinetic_iam).

-export([get_aws_keys/1, get_aws_keys/2]).

-include("kinetic.hrl").

get_aws_keys(MetaData) ->
    case get_current_iam_role(MetaData) of
        {ok, CurrentIAMRole} ->
            get_aws_keys(MetaData, CurrentIAMRole);

        {error, E} ->
            {error, E}
    end.


get_aws_keys(MetaData, undefined) ->
    get_aws_keys(MetaData);
get_aws_keys(MetaData, CurrentIAMRole) ->
    case kinetic_utils:fetch_and_return_url(MetaData ++ ?SECURITY_CREDENTIALS_PARTIAL_URL ++ CurrentIAMRole) of
        {ok, Body} ->
            case proplists:get_value(<<"Code">>, Body) of
                <<"Success">> ->
                    {ok, #aws_credentials{
                        access_key_id =
                            binary_to_list(proplists:get_value(<<"AccessKeyId">>, Body)),
                        secret_access_key =
                            binary_to_list(proplists:get_value(<<"SecretAccessKey">>, Body)),
                        security_token =
                            binary_to_list(proplists:get_value(<<"Token">>, Body)),
                        expiration_seconds = calendar:datetime_to_gregorian_seconds(
                            kinetic_iso8601:parse(proplists:get_value(<<"Expiration">>, Body)))}};
                _ ->
                    {error, no_credentials_found}
            end;
        {error, Error} ->
            {error, Error}
    end.

%% Internal

get_current_iam_role(MetaData) ->
    case kinetic_utils:fetch_and_return_url(MetaData ++ ?IAM_ROLE_URL) of
        {ok, Body} ->
            get_role_from_body(proplists:get_value(<<"Code">>, Body),
                               proplists:get_value(<<"InstanceProfileArn">>, Body));
        {error, 404} ->
            %% hologram doesn't currently implement ?IAM_ROLE_URL, but does
            %% support an alternate means of obtaining the current role name:
            get_iam_role_fallback(MetaData);
        {error, Error} ->
            {error, Error}
    end.

% ProfileArn looks like this:
% <<"arn:aws:iam::ACCOUNT_NUMBER:instance-profile/ROLE">>
get_role_from_body(<<"Success">>, ProfileArn) ->
    {ok, binary:bin_to_list(lists:nth(2, binary:split(ProfileArn, <<"/">>)))};
get_role_from_body(_, _) ->
    {error, no_success}.


get_iam_role_fallback(MetaData) ->
    case kinetic_utils:fetch_and_return_url(MetaData
                                            ++ ?SECURITY_CREDENTIALS_PARTIAL_URL, text) of
        {ok, Body} ->
            {ok, Body};
        Error ->
            Error
    end.
