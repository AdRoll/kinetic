-module(kinetic_utils).

-export([fetch_and_return_url/1]).


http_options() ->
    [{timeout, 200000}, {relaxed, true}].

http_client_headers() ->
    [{"Connection", "close"}].

fetch_and_return_url(Url) ->
    case catch(httpc:request(get, {Url, http_client_headers()}, http_options(), [])) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            case catch(jiffy:decode(Body)) of
                {error, Error} ->
                    {error, Error};
                {Decoded} ->
                    {ok, Decoded}
            end;

        {ok, {{_, Code, _}, _Headers, _Body}} ->
            {error, Code};

        _ ->
            {error, unknown_result_from_http}
    end.


