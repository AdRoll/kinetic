-ifndef(KINETIC_HRL). 
-define(KINETIC_HRL, true). 

-define(EXPIRATION_REFRESH, 120).
-define(KINETIC_DATA, kinetic_data).
-define(KINETIC_ARGS_KEY, args).
-define(IAM_ROLE_URL, "/latest/meta-data/iam/info").
-define(SECURITY_CREDENTIALS_PARTIAL_URL, "/latest/meta-data/iam/security-credentials/").
-define(KINESIS_MAX_PUT_SIZE, 51200).

-record(kinetic_arguments, {
    access_key_id :: undefined | string(),
    secret_access_key :: undefined | string(),
    region :: undefined | string(),
    date :: undefined | string(),
    host :: undefined | string(), 
    url :: undefined | string(),
    expiration_seconds :: undefined | pos_integer(),
    lhttpc_opts = [] :: [any()]
}).

-record(kinetic_stream, {
        stream_name :: undefined | string(),
        base_partition_name :: undefined | string(),
        partitions_number=1000 :: pos_integer(),
        timeout=5000 :: pos_integer(),
        buffer= <<"">> :: binary(),
        buffer_size=0 :: pos_integer(),
        current_partition_num=0 :: pos_integer(),
        flush_interval :: undefined | pos_integer(),
        flush_tref :: undefined | term()
}).

-endif.
