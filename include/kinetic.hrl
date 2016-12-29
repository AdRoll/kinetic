-ifndef(KINETIC_HRL).
-define(KINETIC_HRL, true).

-define(EXPIRATION_REFRESH, 120).
-define(KINETIC_DATA, kinetic_data).
-define(KINETIC_ARGS_KEY, args).
-define(IAM_ROLE_URL, "/latest/meta-data/iam/info").
-define(SECURITY_CREDENTIALS_PARTIAL_URL, "/latest/meta-data/iam/security-credentials/").
-define(KINESIS_MAX_PUT_SIZE, 51200).
-define(DEFAULT_OPERATION_TIMEOUT, 5000).

-record(aws_credentials, {
    access_key_id :: undefined | string(),
    secret_access_key :: undefined | string(),
    security_token :: undefined | string(),
    expiration_seconds :: undefined | no_expire | pos_integer()
}).


-record(kinetic_arguments, {
    region :: undefined | string(),
    date :: undefined | string(),
    host :: undefined | string(),
    url :: undefined | string(),
    lhttpc_opts = [] :: [any()],
    timeout :: undefined | pos_integer(),
    aws_credentials :: #aws_credentials{}
}).

-endif.
