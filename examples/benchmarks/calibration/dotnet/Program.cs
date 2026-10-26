using System.Diagnostics;
using System.Text.Json;
using System.Threading.Channels;

internal sealed record Result(long Checksum, long ElapsedNs, List<long> Latencies);
internal sealed record PipelineResult(long Checksum, List<long> Latencies);

internal static class Program
{
    private const long SampleCeiling = 640000;

    private static Channel<T> Bounded<T>(int capacity = 1) =>
        Channel.CreateBounded<T>(new BoundedChannelOptions(capacity)
        {
            FullMode = BoundedChannelFullMode.Wait,
            SingleReader = true,
            SingleWriter = false,
        });

    private static long ElapsedNs(long start) =>
        (long)((Stopwatch.GetTimestamp() - start) * (1_000_000_000.0 / Stopwatch.Frequency));

    private static async Task Worker(ChannelReader<long> requests, ChannelWriter<long> replies,
        ChannelWriter<byte> ready, Task start, long iterations)
    {
        await ready.WriteAsync(1);
        await start;
        for (long index = 0; index < iterations; index++)
        {
            var value = await requests.ReadAsync();
            await replies.WriteAsync(value + 1);
        }
    }

    private static async Task<Result> Client(ChannelWriter<long> requests, ChannelReader<long> replies,
        ChannelWriter<byte> ready, Task start, long iterations, bool sampled)
    {
        await ready.WriteAsync(1);
        await start;
        var timer = Stopwatch.GetTimestamp();
        long checksum = 0;
        var samples = new List<long>();
        for (long index = 0; index < iterations; index++)
        {
            if (sampled && index < SampleCeiling && index % 64 == 0)
            {
                var before = Stopwatch.GetTimestamp();
                await requests.WriteAsync(index);
                var reply = await replies.ReadAsync();
                samples.Add(ElapsedNs(before));
                if (reply != index + 1) throw new InvalidOperationException("stream sequence mismatch");
            }
            else
            {
                await requests.WriteAsync(index);
                var reply = await replies.ReadAsync();
                if (reply != index + 1) throw new InvalidOperationException("stream sequence mismatch");
            }
            checksum += index;
        }
        return new Result(checksum, ElapsedNs(timer), samples);
    }

    private static async Task<Result> Roundtrip(long iterations, bool sampled)
    {
        var requests = Bounded<long>();
        var replies = Bounded<long>();
        var ready = Bounded<byte>(2);
        var start = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
        var worker = Worker(requests.Reader, replies.Writer, ready.Writer, start.Task, iterations);
        var client = Client(requests.Writer, replies.Reader, ready.Writer, start.Task, iterations, sampled);
        await ready.Reader.ReadAsync();
        await ready.Reader.ReadAsync();
        start.SetResult();
        var result = await client;
        await worker;
        return result;
    }

    private static async Task<Result> Pipeline(long iterations, int pairs, bool sampled)
    {
        var ready = Bounded<byte>(2 * pairs);
        var start = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
        var producers = new Task[pairs];
        var consumers = new Task<PipelineResult>[pairs];

        for (var pair = 0; pair < pairs; pair++)
        {
            var channel = Bounded<long>();
            producers[pair] = Produce(channel.Writer, ready.Writer, start.Task, iterations, pair, pairs);
            consumers[pair] = Drain(channel.Reader, ready.Writer, start.Task, iterations, pair, pairs, sampled);
        }

        for (var endpoint = 0; endpoint < 2 * pairs; endpoint++) await ready.Reader.ReadAsync();
        var timer = Stopwatch.GetTimestamp();
        start.SetResult();
        await Task.WhenAll(producers);
        var completed = await Task.WhenAll(consumers);
        var elapsed = ElapsedNs(timer);
        return new Result(completed.Sum(item => item.Checksum), elapsed,
            completed.SelectMany(item => item.Latencies).ToList());
    }

    private static async Task Produce(ChannelWriter<long> output, ChannelWriter<byte> ready,
        Task start, long iterations, int id, int pairs)
    {
        await ready.WriteAsync(1);
        await start;
        for (long index = 0; index < iterations; index++)
            await output.WriteAsync(index * pairs + id);
        output.TryComplete();
    }

    private static async Task<PipelineResult> Drain(ChannelReader<long> input, ChannelWriter<byte> ready,
        Task start, long iterations, int id, int pairs, bool sampled)
    {
        await ready.WriteAsync(1);
        await start;
        long checksum = 0;
        var samples = new List<long>();
        for (long index = 0; index < iterations; index++)
        {
            var expected = index * pairs + id;
            long value;
            if (sampled && expected < SampleCeiling && expected % 64 == 0)
            {
                var before = Stopwatch.GetTimestamp();
                value = await input.ReadAsync();
                samples.Add(ElapsedNs(before));
            }
            else
            {
                value = await input.ReadAsync();
            }
            if (value != expected) throw new InvalidOperationException("pipeline sequence mismatch");
            checksum += value % 251;
        }
        return new PipelineResult(checksum, samples);
    }

    public static async Task<int> Main(string[] args)
    {
        if (args.Length != 3) throw new ArgumentException("usage: concurrency WORKLOAD ITERATIONS SIZE");
        var workload = args[0];
        var iterations = long.Parse(args[1]);
        var size = int.Parse(args[2]);
        if (iterations <= 0 || size <= 0) throw new ArgumentOutOfRangeException("iterations and size must be positive");
        Result result = workload switch
        {
            "stream_roundtrip" => await Roundtrip(iterations, true),
            "stream_roundtrip_alloc" => await Roundtrip(iterations, false),
            "stream_pipeline" => await Pipeline(iterations, size, true),
            "stream_pipeline_alloc" => await Pipeline(iterations, size, false),
            _ => throw new ArgumentException("unknown stream workload"),
        };
        Console.WriteLine(JsonSerializer.Serialize(new
        {
            workload,
            iterations,
            size,
            elapsed_ns = result.ElapsedNs,
            checksum = result.Checksum,
            latencies_ns = result.Latencies,
        }));
        return 0;
    }
}
