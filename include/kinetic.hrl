-define(KINETIC_DATA, kinetic_data).
-define(KINETIC_ARGS_KEY, args).
-define(IAM_ROLE_URL, "/latest/meta-data/iam/info").
-define(SECURITY_CREDENTIALS_PARTIAL_URL, "/latest/meta-data/iam/security-credentials/").

-record(kinetic_arguments, {access_key_id, secret_access_key, region, date, host, url, expiration_seconds, lhttpc_opts}).
