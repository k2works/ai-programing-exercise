-module(fizzbuzz_tests).
-include_lib("eunit/include/eunit.hrl").

convert_test() ->
    ?assertEqual("1", fizzbuzz:convert(1)).