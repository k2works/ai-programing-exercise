-module(fizzbuzz).
-export([convert/1]).

convert(N) when N =:= 3 ->
    "Fizz";
convert(N) ->
    integer_to_list(N).