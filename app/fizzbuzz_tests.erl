-module(fizzbuzz_tests).
-include_lib("eunit/include/eunit.hrl").

convert_test() ->
    ?assertEqual("1", fizzbuzz:convert(1)),
    ?assertEqual("2", fizzbuzz:convert(2)),
    ?assertEqual("Fizz", fizzbuzz:convert(3)),
    ?assertEqual("Buzz", fizzbuzz:convert(5)),
    ?assertEqual("FizzBuzz", fizzbuzz:convert(15)).

range_test() ->
    Expected = lists:seq(1, 100),
    ?assertEqual(Expected, fizzbuzz:range(1, 100)).