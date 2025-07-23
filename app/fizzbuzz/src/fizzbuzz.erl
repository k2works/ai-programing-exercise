-module(fizzbuzz).

-export([generate/1, generate/2, create_list/0, create_list/2, 
         create_list_with_type/1, create_list_with_type/3,
         filter_by_pattern/1, filter_only_fizzbuzz/0,
         transform_and_filter/2, print_fizzbuzz/0, print_list/1,
         create_list_comprehension/0, create_lazy_list/0, create_lazy_list/2,
         generate_with_value_objects/2, create_fizzbuzz_number/1, create_fizzbuzz_type/1]).

generate(N) ->
    generate(N, 1).

generate(N, Type) ->
    Strategy = get_strategy(Type),
    Strategy(N).

get_strategy(1) -> fun fizzbuzz_type:convert_type1/1;
get_strategy(2) -> fun fizzbuzz_type:convert_type2/1;
get_strategy(3) -> fun fizzbuzz_type:convert_type3/1.

%% 高階関数によるリスト処理の改善
create_list() ->
    create_list(1, 100).

create_list(Start, End) ->
    lists:map(fun generate/1, lists:seq(Start, End)).

create_list_with_type(Type) ->
    create_list_with_type(Type, 1, 100).

create_list_with_type(Type, Start, End) ->
    lists:map(fun(N) -> generate(N, Type) end, lists:seq(Start, End)).

%% 関数型のフィルタリング
filter_only_fizzbuzz() ->
    filter_by_pattern("FizzBuzz").

filter_by_pattern(Pattern) ->
    lists:filter(
        fun(Item) -> Item =:= Pattern end,
        create_list()
    ).

%% 変換とフィルタの合成
transform_and_filter(Predicate, Transformer) ->
    lists:filtermap(
        fun(N) ->
            Result = generate(N),
            case Predicate(Result) of
                true -> {true, Transformer(Result)};
                false -> false
            end
        end,
        lists:seq(1, 100)
    ).

print_fizzbuzz() ->
    print_list(create_list()).

print_list(List) ->
    lists:foreach(fun(Item) -> io:format("~s~n", [Item]) end, List),
    ok.

%% リスト内包表記（Erlangの場合はリスト理解）
create_list_comprehension() ->
    [generate(N) || N <- lists:seq(1, 100)].

create_list_comprehension_with_filter() ->
    [generate(N) || N <- lists:seq(1, 100), N rem 2 =:= 0].

%% 遅延評価風の実装（ジェネレータ関数）
create_lazy_list() ->
    create_lazy_list(1, 100).

create_lazy_list(Start, End) ->
    fun() -> lazy_generate(Start, End) end.

lazy_generate(Current, End) when Current > End ->
    [];
lazy_generate(Current, End) ->
    [generate(Current) | fun() -> lazy_generate(Current + 1, End) end].

%% ストリーム風の処理
take(0, _) -> [];
take(_, []) -> [];
take(N, [H|T]) -> [H | take(N-1, T)];
take(N, Generator) when is_function(Generator) ->
    case Generator() of
        [] -> [];
        [H|TailGen] -> [H | take(N-1, TailGen)]
    end.

%% Delegate to fizzbuzz_value module
create_fizzbuzz_number(N) ->
    fizzbuzz_value:create_fizzbuzz_number(N).

create_fizzbuzz_type(T) ->
    fizzbuzz_value:create_fizzbuzz_type(T).

generate_with_value_objects(Number, Type) ->
    fizzbuzz_value:generate_with_value_objects(Number, Type).