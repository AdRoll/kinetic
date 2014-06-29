-module(kinetic_utils).

-export([fetch_and_return_url/1, fetch_and_return_url/2]).


http_options() ->
    [{timeout, 200000}, {relaxed, true}].

http_client_headers() ->
    [{"Connection", "close"}].

fetch_and_return_url(Url) ->
    fetch_and_return_url(Url, json).

fetch_and_return_url(Url, json) ->
    case fetch_and_return_body(Url) of
        {ok, Body} ->
            case catch(jiffy:decode(Body)) of
                {error, Error} ->
                    {error, Error};
                {Decoded} ->
                    {ok, Decoded}
            end;

        {error, E} ->
            {error, E}
    end;
fetch_and_return_url(Url, text) ->
    fetch_and_return_body(Url).



fetch_and_return_body(Url) ->
    case catch(httpc:request(get, {Url, http_client_headers()}, http_options(), [])) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            {ok, Body};

        {ok, {{_, Code, _}, _Headers, _Body}} ->
            {error, Code};

        _ ->
            {error, unknown_result_from_http}
    end.


