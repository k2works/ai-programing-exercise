-module(fizzbuzz_value).

-export([create_fizzbuzz_number/1, create_fizzbuzz_type/1, generate_with_value_objects/2,
         safe_create_fizzbuzz_number/1, safe_create_fizzbuzz_type/1,
         maybe_bind/2, maybe_map/2, either_bind/2, either_map/2]).

%% Value Objects
-record(fizzbuzz_number, {value}).
-record(fizzbuzz_type, {value}).

%% 関数型エラーハンドリング（Maybe/Either風）
%% Maybe型の実装
safe_create_fizzbuzz_number(N) when is_integer(N), N > 0 ->
    {ok, #fizzbuzz_number{value = N}};
safe_create_fizzbuzz_number(_) ->
    {error, invalid_number}.

safe_create_fizzbuzz_type(T) when T >= 1, T =< 3 ->
    {ok, #fizzbuzz_type{value = T}};
safe_create_fizzbuzz_type(_) ->
    {error, invalid_type}.

%% Monad風の操作
maybe_bind({ok, Value}, Function) ->
    Function(Value);
maybe_bind({error, Reason}, _) ->
    {error, Reason}.

maybe_map({ok, Value}, Function) ->
    {ok, Function(Value)};
maybe_map({error, Reason}, _) ->
    {error, Reason}.

either_bind({ok, Value}, Function) ->
    Function(Value);
either_bind({error, _} = Error, _) ->
    Error.

either_map({ok, Value}, Function) ->
    {ok, Function(Value)};
either_map({error, _} = Error, _) ->
    Error.

%% 安全な値オブジェクト生成と処理
safe_generate_with_value_objects(N, T) ->
    maybe_bind(
        safe_create_fizzbuzz_number(N),
        fun(Number) ->
            maybe_bind(
                safe_create_fizzbuzz_type(T),
                fun(Type) ->
                    {ok, generate_with_value_objects(Number, Type)}
                end
            )
        end
    ).

%% Value Object constructors (例外版)
create_fizzbuzz_number(N) when is_integer(N), N > 0 ->
    #fizzbuzz_number{value = N}.

create_fizzbuzz_type(T) when T >= 1, T =< 3 ->
    #fizzbuzz_type{value = T}.

%% Generate using Value Objects
generate_with_value_objects(Number, Type) ->
    #fizzbuzz_number{value = N} = Number,
    #fizzbuzz_type{value = T} = Type,
    fizzbuzz:generate(N, T).