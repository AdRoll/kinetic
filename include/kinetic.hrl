-define(EXPIRATION_REFRESH, 120).
-define(KINETIC_DATA, kinetic_data).
-define(KINETIC_STREAM, kinetic_stream).
-define(KINESIS_MAX_PUT_SIZE, 51200).

-record(kinetic_arguments,
        {region :: undefined | string(),
         date :: undefined | string(),
         host :: undefined | string(),
         url :: undefined | string(),
         lhttpc_opts = [] :: [any()],
         timeout :: undefined | pos_integer(),
         aws_credentials}).
