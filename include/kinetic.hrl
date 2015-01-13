-ifndef(KINETIC_HRL). 
-define(KINETIC_HRL, true). 

-define(EXPIRATION_REFRESH, 120).
-define(KINETIC_DATA, kinetic_data).
-define(KINETIC_STREAM, kinetic_stream).
-define(KINETIC_ARGS_KEY, args).
-define(IAM_ROLE_URL, "/latest/meta-data/iam/info").
-define(SECURITY_CREDENTIALS_PARTIAL_URL, "/latest/meta-data/iam/security-credentials/").
-define(KINESIS_MAX_PUT_SIZE, 51200).
-define(DEFAULT_OPERATION_TIMEOUT, 5000).

-record(aws_credentials, {
    access_key_id :: undefined | string(),
    secret_access_key :: undefined | string(),
    security_token :: undefined | string(),
    expiration_seconds :: undefined | pos_integer()
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

-record(kinetic_stream, {
        stream_name :: binary(),
        base_partition_name :: binary(),
        partitions_number=1000 :: pos_integer(),
        timeout=5000 :: pos_integer(),
        buffer= <<"">> :: binary(),
        buffer_size=0 :: pos_integer(),
        current_partition_num=0 :: pos_integer(),
        flush_interval=1000 :: pos_integer(),
        flush_tref :: undefined | term(),
        retries=3 :: pos_integer()
}).


-endif.
