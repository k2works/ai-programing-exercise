-module(fizzbuzz_value).

-export([create_fizzbuzz_number/1, create_fizzbuzz_type/1, generate_with_value_objects/2]).

%% Value Objects
-record(fizzbuzz_number, {value}).
-record(fizzbuzz_type, {value}).

%% Value Object constructors
create_fizzbuzz_number(N) when is_integer(N), N > 0 ->
    #fizzbuzz_number{value = N}.

create_fizzbuzz_type(T) when T >= 1, T =< 3 ->
    #fizzbuzz_type{value = T}.

%% Generate using Value Objects
generate_with_value_objects(Number, Type) ->
    #fizzbuzz_number{value = N} = Number,
    #fizzbuzz_type{value = T} = Type,
    fizzbuzz:generate(N, T).