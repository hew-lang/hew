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
    {Sum, Samples, MeasuredElapsed} = case Workload of
        "task_ready" -> {ready_batches(0, N, Size, 0), [], undefined};
        "actor_roundtrip" -> [{Actor, _}] = Actors, {Value, Latency} = roundtrips(Actor, N, 0, [], 0), {Value, Latency, undefined};
        "actor_fanout" ->
            Clients = [spawn_monitor(fun() -> Parent ! {self(), client(Actor, N, 0)} end) || {Actor, _} <- Actors],
            {join(Clients, 0), [], undefined};
        "timer" -> {Value, Latency} = timers(N, [], 0), {Value, Latency, undefined};
        "stream_roundtrip" -> stream_roundtrip(N, true);
        "stream_roundtrip_alloc" -> stream_roundtrip(N, false);
        "stream_pipeline" -> stream_pipeline(N, Size, true);
        "stream_pipeline_alloc" -> stream_pipeline(N, Size, false)
    end,
    Elapsed = case MeasuredElapsed of undefined -> now_ns() - Start; ElapsedValue -> ElapsedValue end,
    lists:foreach(fun({Actor, Ref}) -> Actor ! stop, receive {'DOWN', Ref, process, Actor, normal} -> ok end end, Actors),
    Latencies = lists:join(",", [integer_to_list(Latency) || Latency <- Samples]),
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

stream_roundtrip(N, Sampled) ->
    Parent = self(), Gate = make_ref(),
    Requests = spawn_monitor(fun() ->
        Parent ! {stream_ready, self()},
        receive {stream_start, Gate} -> ok end,
        stream_worker(N)
    end),
    Replies = spawn_monitor(fun() ->
        Parent ! {stream_ready, self()},
        receive {stream_start, Gate} -> ok end,
        Before = now_ns(),
        {Sum, Samples} = stream_client(element(1, Requests), N, Sampled, 0, [], 0),
        Parent ! {stream_result, self(), Sum, Samples, now_ns() - Before}
    end),
    ReadyPids = [element(1, Requests), element(1, Replies)],
    await_ready(ReadyPids),
    lists:foreach(fun(Pid) -> Pid ! {stream_start, Gate} end, ReadyPids),
    {Client, ClientRef} = Replies,
    receive {stream_result, Client, Sum, Samples, Elapsed} ->
        await_down(Client, ClientRef),
        {Worker, WorkerRef} = Requests,
        await_down(Worker, WorkerRef),
        {Sum, Samples, Elapsed}
    end.

stream_worker(0) -> ok;
stream_worker(N) ->
    receive
        {stream_request, Client, Value} ->
            Client ! {stream_reply, self(), Value + 1},
            stream_worker(N - 1)
    end.

stream_client(_, 0, _, Sum, Samples, _) -> {Sum, lists:reverse(Samples)};
stream_client(Worker, N, Sampled, Sum, Samples, Index) ->
    IsSampled = Sampled andalso Index < 640000 andalso Index rem 64 =:= 0,
    case IsSampled of
        true ->
            Before = now_ns(),
            Worker ! {stream_request, self(), Index},
            receive {stream_reply, Worker, Value} ->
                case Value =:= Index + 1 of true -> ok; false -> erlang:error(stream_sequence_mismatch) end,
                Elapsed = now_ns() - Before,
                stream_client(Worker, N - 1, Sampled, Sum + Index, [Elapsed | Samples], Index + 1)
            end;
        false ->
            Worker ! {stream_request, self(), Index},
            receive {stream_reply, Worker, Value} ->
                case Value =:= Index + 1 of true -> ok; false -> erlang:error(stream_sequence_mismatch) end,
                stream_client(Worker, N - 1, Sampled, Sum + Index, Samples, Index + 1)
            end
    end.

stream_pipeline(N, Size, Sampled) ->
    Parent = self(), Gate = make_ref(),
    Pairs = [begin
        Id = Pair,
        Drain = spawn_monitor(fun() ->
            Parent ! {pipeline_ready, self()},
            receive {pipeline_start, Gate} -> ok end,
            {Sum, Samples} = pipeline_drain(N, Id, Size, Sampled, 0, [], 0),
            Parent ! {pipeline_drain_done, self(), Sum, Samples}
        end),
        Producer = spawn_monitor(fun() ->
            Parent ! {pipeline_ready, self()},
            receive {pipeline_start, Gate} -> ok end,
            pipeline_producer(element(1, Drain), N, Id, Size, 0),
            Parent ! {pipeline_producer_done, self()}
        end),
        {Producer, Drain}
    end || Pair <- lists:seq(0, Size - 1)],
    await_pipeline_ready(2 * Size),
    Start = now_ns(),
    lists:foreach(fun({Producer, Drain}) ->
        element(1, Producer) ! {pipeline_start, Gate},
        element(1, Drain) ! {pipeline_start, Gate}
    end, Pairs),
    DrainResults = await_pipeline_results(2 * Size, []),
    Elapsed = now_ns() - Start,
    lists:foreach(fun({Producer, Drain}) ->
        await_down(element(1, Producer), element(2, Producer)),
        await_down(element(1, Drain), element(2, Drain))
    end, Pairs),
    Sum = lists:sum([Value || {Value, _} <- DrainResults]),
    Samples = lists:append([Latency || {_, Latency} <- DrainResults]),
    {Sum, Samples, Elapsed}.

await_ready([]) -> ok;
await_ready([Pid | Rest]) -> receive {stream_ready, Pid} -> await_ready(Rest) end.

await_pipeline_ready(0) -> ok;
await_pipeline_ready(Count) -> receive {pipeline_ready, _Pid} -> await_pipeline_ready(Count - 1) end.

pipeline_producer(_, N, _, _, N) -> ok;
pipeline_producer(Drain, N, Id, Size, Index) ->
    Value = Index * Size + Id,
    Drain ! {pipeline_item, self(), Value},
    receive {pipeline_ack, Drain, Value} -> pipeline_producer(Drain, N, Id, Size, Index + 1) end.

pipeline_drain(N, _, _, _, N, Samples, Sum) -> {Sum, lists:reverse(Samples)};
pipeline_drain(N, Id, Size, Sampled, Index, Samples, Sum) ->
    Expected = Index * Size + Id,
    IsSampled = Sampled andalso Expected < 640000 andalso Expected rem 64 =:= 0,
    Value = case IsSampled of
        true -> Before = now_ns(), receive {pipeline_item, Producer, Item} ->
            Elapsed = now_ns() - Before,
            case Item =:= Expected of true -> Producer ! {pipeline_ack, self(), Item}, {Item, [Elapsed | Samples]}; false -> erlang:error(pipeline_sequence_mismatch) end
        end;
        false -> receive {pipeline_item, Producer, Item} ->
            case Item =:= Expected of true -> Producer ! {pipeline_ack, self(), Item}, {Item, Samples}; false -> erlang:error(pipeline_sequence_mismatch) end
        end
    end,
    {ItemValue, NextSamples} = Value,
    pipeline_drain(N, Id, Size, Sampled, Index + 1, NextSamples, Sum + ItemValue rem 251).

await_pipeline_results(0, Drains) -> Drains;
await_pipeline_results(Count, Drains) ->
    receive
        {pipeline_drain_done, _Pid, Value, Latencies} -> await_pipeline_results(Count - 1, [{Value, Latencies} | Drains]);
        {pipeline_producer_done, _Pid} -> await_pipeline_results(Count - 1, Drains)
    end.

await_down(Pid, Ref) -> receive {'DOWN', Ref, process, Pid, normal} -> ok end.
