-module(kinetic).
-behaviour(application).


-export([start/0, stop/0]).
-export([start/2, stop/1]).
-export([start/1]).


-export([list_streams/1, list_streams/2]).

-include("kinetic.hrl").

% application behaviour

-spec start() -> ok | {error, any()}.
start() ->
    application:start(kinetic).


-spec stop() -> ok | {error, any()}.
stop() ->
    application:stop(kinetic).


-spec start(normal | {takeover, node()} | {failover, node()}, any()) ->
    {ok, pid()}.
start(Opts) when is_list(Opts) ->
    kinetic_sup:start_link(Opts).

start(_, Opts) when is_list(Opts) ->
    kinetic_sup:start_link(Opts);
start(_, _) ->
    kinetic_sup:start_link().

-spec stop(any()) -> ok.
stop(_) ->
    ok.

% Public API

% -spec api(method(), any(), integer()) ->result().
list_streams(Payload) ->
    execute("ListStreams", Payload, 5000).
list_streams(Payload, Timeout) ->
    execute("ListStreams", Payload, Timeout).


%% Internal
execute(Operation, Payload, Timeout) ->
    case get_args() of
        {error, E} ->
            throw(E);

        {ok, #kinetic_arguments{access_key_id=AccessKeyId, secret_access_key=SecretAccessKey,
                                    region=Region, date=Date, host=Host, url=Url,
                                    lhttpc_opts=LHttpcOpts}} ->
            Target = ["Kinesis_20131202.", Operation],
            Body = jiffy:encode({Payload}),
            {ok, AuthorizationHeader} = kinetic_aws:sign_v4(AccessKeyId, SecretAccessKey, "kinesis",
                                                      Region, Date, Target, Body),
            Headers = [{"Content-Type", "application/x-amz-json-1.1"},
                       {"Connection", "keep-alive"},
                       {"x-amz-target", Target},
                       {"x-amz-date", Date},
                       {"Host", Host},
                       {"Authorization", AuthorizationHeader}],
            case lhttpc:request(Url, post, Headers, Body, Timeout, LHttpcOpts) of
                {ok, {{200, _}, _ResponseHeaders, ResponseBody}} ->
                    {ok, jiffy:decode(ResponseBody)};

                {ok, {{Code, _}, ResponseHeaders, ResponseBody}} ->
                    {error, Code, ResponseHeaders, ResponseBody}
            end
    end.

get_args() ->
    case catch(ets:lookup_element(?KINETIC_DATA, ?KINETIC_ARGS_KEY, 2)) of
        {'EXIT', {badarg, _}} ->
            {error, missing_credentials};
        V ->
            {ok, V}
    end.


