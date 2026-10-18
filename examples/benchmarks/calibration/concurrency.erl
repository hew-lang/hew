%% BEAM processes are independent heaps, including the ready-task processes.
%% Actor setup and a warm-up process precede timing; timer is receive-after.
-module(concurrency).
-export([main/0]).

main() ->
    [Workload, NText, SizeText] = init:get_plain_arguments(),
    N = list_to_integer(NText), Size = list_to_integer(SizeText),
    true = N > 0 andalso Size > 0,
    Parent = self(),
    {Warm, WarmRef} = spawn_monitor(fun() -> Parent ! {self(), 0} end),
    receive {Warm, 0} -> ok end,
    receive {'DOWN', WarmRef, process, Warm, normal} -> ok end,
    Actors = case Workload of
        "actor_roundtrip" -> [spawn_monitor(fun() -> counter(0) end)];
        "actor_fanout" -> [spawn_monitor(fun() -> counter(0) end) || _ <- lists:seq(1, Size)];
        _ -> []
    end,
    Start = now_ns(),
    {Sum, Samples} = case Workload of
        "task_ready" -> {ready_batches(0, N, Size, 0), []};
        "actor_roundtrip" -> [{Actor, _}] = Actors, roundtrips(Actor, N, 0, [], 0);
        "actor_fanout" ->
            Clients = [spawn_monitor(fun() -> Parent ! {self(), client(Actor, N, 0)} end) || {Actor, _} <- Actors],
            {join(Clients, 0), []};
        "timer" -> timers(N, [], 0)
    end,
    Elapsed = now_ns() - Start,
    lists:foreach(fun({Actor, Ref}) -> Actor ! stop, receive {'DOWN', Ref, process, Actor, normal} -> ok end end, Actors),
    Latencies = lists:join(",", [integer_to_list(Value) || Value <- Samples]),
    io:format("{\"workload\":\"~s\",\"iterations\":~B,\"size\":~B,\"elapsed_ns\":~B,\"checksum\":~B,\"latencies_ns\":[~s]}~n", [Workload, N, Size, Elapsed, Sum, Latencies]),
    halt(0).

now_ns() -> erlang:monotonic_time(nanosecond).

ready_batches(Offset, N, _, Sum) when Offset >= N -> Sum;
ready_batches(Offset, N, Size, Sum) ->
    Parent = self(), End = min(Offset + Size, N),
    Tasks = [spawn_monitor(fun() -> Parent ! {self(), Index rem 251} end) || Index <- lists:seq(Offset, End - 1)],
    ready_batches(End, N, Size, join(Tasks, Sum)).

join([], Sum) -> Sum;
join([{Pid, Ref} | Rest], Sum) ->
    Value = receive {Pid, Result} -> Result end,
    receive {'DOWN', Ref, process, Pid, normal} -> ok end,
    join(Rest, Sum + Value).

counter(Count) ->
    receive
        {increment, From} -> From ! {self(), Count + 1}, counter(Count + 1);
        stop -> ok
    end.

increment(Actor) -> Actor ! {increment, self()}, receive {Actor, Count} -> Count end.
client(_, 0, Sum) -> Sum;
client(Actor, N, Sum) -> client(Actor, N - 1, Sum + increment(Actor)).

roundtrips(_, 0, Sum, Samples, _) -> {Sum, lists:reverse(Samples)};
roundtrips(Actor, N, Sum, Samples, Index) ->
    Before = now_ns(), Value = increment(Actor), Elapsed = now_ns() - Before,
    Next = case Index < 10000 of true -> [Elapsed | Samples]; false -> Samples end,
    roundtrips(Actor, N - 1, Sum + Value, Next, Index + 1).

timers(0, Samples, Index) -> {Index, lists:reverse(Samples)};
timers(N, Samples, Index) ->
    Before = now_ns(), receive after 1 -> ok end,
    Overshoot = max(0, now_ns() - Before - 1000000),
    Next = case Index < 10000 of true -> [Overshoot | Samples]; false -> Samples end,
    timers(N - 1, Next, Index + 1).
