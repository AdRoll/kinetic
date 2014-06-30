-ifndef(KINETIC_HRL). 
-define(KINETIC_HRL, true). 

-define(KINETIC_DATA, kinetic_data).
-define(KINETIC_ARGS_KEY, args).
-define(IAM_ROLE_URL, "/latest/meta-data/iam/info").
-define(SECURITY_CREDENTIALS_PARTIAL_URL, "/latest/meta-data/iam/security-credentials/").

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

-endif.
