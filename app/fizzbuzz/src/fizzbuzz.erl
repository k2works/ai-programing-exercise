-module(fizzbuzz).

-export([generate/1, generate/2, create_list/0, print_fizzbuzz/0,
         generate_with_value_objects/2, create_fizzbuzz_number/1, create_fizzbuzz_type/1]).

generate(N) ->
    generate(N, 1).

generate(N, Type) ->
    Strategy = get_strategy(Type),
    Strategy(N).

get_strategy(1) -> fun fizzbuzz_type:convert_type1/1;
get_strategy(2) -> fun fizzbuzz_type:convert_type2/1;
get_strategy(3) -> fun fizzbuzz_type:convert_type3/1.

create_list() ->
    lists:map(fun generate/1, lists:seq(1, 100)).

print_fizzbuzz() ->
    List = create_list(),
    lists:foreach(fun(Item) -> io:format("~s~n", [Item]) end, List),
    ok.

%% Delegate to fizzbuzz_value module
create_fizzbuzz_number(N) ->
    fizzbuzz_value:create_fizzbuzz_number(N).

create_fizzbuzz_type(T) ->
    fizzbuzz_value:create_fizzbuzz_type(T).

generate_with_value_objects(Number, Type) ->
    fizzbuzz_value:generate_with_value_objects(Number, Type).