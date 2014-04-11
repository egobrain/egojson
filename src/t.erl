-module(t).

-export([
         test_avg/4,
         parallel_test_avg/4
        ]).

test_avg(M, F, A, N) when N > 0 ->
    test_avg(M, F, A, fun test_loop/4, N).

parallel_test_avg(M, F, A, N) when N > 0 ->
    test_avg(M, F, A, fun parallel_test_loop/4, N).

test_loop(M, F, A, N) ->
    test_loop(M, F, A, N, []).
test_loop(_M, _F, _A, 0, List) ->
    List;
test_loop(M, F, A, N, List) ->
    {T, _Result} = timer:tc(M, F, A),
    test_loop(M, F, A, N - 1, [T|List]).

parallel_test_loop(M, F, A, N) ->
    Self = self(),
    Pids = [spawn_link(
                fun() ->
                        receive
                            _ ->
                                {T, _Result} = timer:tc(M, F, A),
                                Self ! T
                        end
                end) || _ <- lists:seq(1, N)],
    lists:foreach(fun(Pid) -> Pid ! start end, Pids),
    [ receive D -> D after 3000 -> throw(timeout) end || _ <- Pids].

test_avg(M, F, A, Loop, LoopOpts) ->
    L = Loop(M, F, A, LoopOpts),
    Length = length(L),
    Min = lists:min(L),
    Max = lists:max(L),
    Med = lists:nth(round((Length / 2)), lists:sort(L)),
    Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
    io:format("Range: ~b - ~b mics~n"
              "Median: ~b mics~n"
              "Average: ~b mics~n",
              [Min, Max, Med, Avg]),
    Med.
