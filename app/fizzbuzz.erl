-module(fizzbuzz).
-export([convert/1]).

convert(N) when N rem 3 =:= 0 ->
    "Fizz";
convert(N) ->
    integer_to_list(N).