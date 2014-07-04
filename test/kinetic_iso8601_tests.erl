-module(kinetic_iso8601_tests).

-include("kinetic.hrl").
-include_lib("eunit/include/eunit.hrl").



kinetic_utils_test_() ->
    [
        ?_test(test_iso8601_formatters())
    ].



%%
%% Tests
%%

test_iso8601_formatters() ->
    "20140409T101112Z" = kinetic_iso8601:format_basic({{2014, 4, 9}, {10, 11, 12}}),
    "2014-04-09T10:11:12Z" = kinetic_iso8601:format({{2014, 4, 9}, {10, 11, 12}}),
    {{2014, 4, 9}, {10, 11, 12}} = kinetic_iso8601:parse("2014-04-09T10:11:12Z").

