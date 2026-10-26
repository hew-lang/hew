using System.Text;

const int BodySize = 64;
const int WorkUnits = 200_000;

var builder = WebApplication.CreateBuilder(args);
builder.Logging.ClearProviders();
using var backendClient = new HttpClient(new SocketsHttpHandler { AllowAutoRedirect = false })
{ Timeout = TimeSpan.FromSeconds(10) };
var bind = Environment.GetEnvironmentVariable("BENCH_BIND");
if (string.IsNullOrWhiteSpace(bind))
{
    bind = "127.0.0.1:18080";
}
builder.WebHost.UseUrls($"http://{bind}");
var app = builder.Build();
using var requestSlots = new SemaphoreSlim(32, 32);
app.Use(async (context, next) =>
{
    context.Response.Headers.Connection = "close";
    await requestSlots.WaitAsync(context.RequestAborted);
    try
    {
        await next();
    }
    finally
    {
        requestSlots.Release();
    }
});

app.MapPost("/echo", async context =>
{
    var body = await ReadBody(context.Request);
    if (body is null)
    {
        context.Response.StatusCode = StatusCodes.Status400BadRequest;
        return;
    }
    await WriteBody(context.Response, body);
});

app.MapPost("/wait", async context =>
{
    var body = await ReadBody(context.Request);
    if (body is null)
    {
        context.Response.StatusCode = StatusCodes.Status400BadRequest;
        return;
    }
    await Task.Delay(TimeSpan.FromMilliseconds(5), context.RequestAborted);
    await WriteBody(context.Response, body);
});

app.MapPost("/io", async context =>
{
    var body = await ReadBody(context.Request);
    if (body is null)
    {
        context.Response.StatusCode = StatusCodes.Status400BadRequest;
        return;
    }
    try { await CheckBackend(body, context.RequestAborted); }
    catch (Exception error)
    {
        context.Response.StatusCode = StatusCodes.Status502BadGateway;
        await WriteBody(context.Response, Encoding.UTF8.GetBytes($"backend I/O failed: {error.Message}"));
        return;
    }
    await WriteBody(context.Response, body);
});

app.MapPost("/compute", async context =>
{
    var body = await ReadBody(context.Request);
    if (body is null)
    {
        context.Response.StatusCode = StatusCodes.Status400BadRequest;
        return;
    }
    var seed = body[0];
    var total = Compute(seed, 0, WorkUnits);
    await WriteBody(context.Response, Encoding.ASCII.GetBytes($"{Encoding.ASCII.GetString(body)}:{total}"));
});

app.MapPost("/fanout", (Delegate)FanoutAsync);

async Task FanoutAsync(HttpContext context)
{
    var body = await ReadBody(context.Request);
    if (body is null)
    {
        context.Response.StatusCode = StatusCodes.Status400BadRequest;
        return;
    }
    var seed = body[0];
    var tasks = Enumerable.Range(0, 8)
        .Select(part => Task.Run(() => Compute(seed, part * (WorkUnits / 8), WorkUnits / 8), context.RequestAborted))
        .ToArray();
    var total = (await Task.WhenAll(tasks)).Aggregate(0UL, (sum, partial) => sum + partial);
    await WriteBody(context.Response, Encoding.ASCII.GetBytes($"{Encoding.ASCII.GetString(body)}:{total}"));
}

await app.StartAsync();
Console.WriteLine($"READY {bind}");
await app.WaitForShutdownAsync();

async Task CheckBackend(byte[] body, CancellationToken cancellationToken)
{
    var backend = Environment.GetEnvironmentVariable("BENCH_BACKEND");
    var separator = backend?.LastIndexOf(':') ?? -1;
    if (string.IsNullOrWhiteSpace(backend) || separator < 1 ||
        !int.TryParse(backend[(separator + 1)..], out var port) || port is < 1 or > 65535 ||
        !Uri.TryCreate($"http://{backend}/echo", UriKind.Absolute, out var uri) || uri.Port < 1 ||
        uri.AbsolutePath != "/echo" || !string.IsNullOrEmpty(uri.UserInfo))
        throw new InvalidOperationException("BENCH_BACKEND must be host:port for /io");
    using var request = new HttpRequestMessage(HttpMethod.Post, uri) { Content = new ByteArrayContent(body) };
    request.Headers.ConnectionClose = true;
    request.Headers.AcceptEncoding.ParseAdd("identity");
    using var response = await backendClient.SendAsync(request, cancellationToken);
    var echoed = await response.Content.ReadAsByteArrayAsync(cancellationToken);
    if (response.StatusCode != System.Net.HttpStatusCode.OK || !body.AsSpan().SequenceEqual(echoed))
        throw new HttpRequestException($"backend returned status {(int)response.StatusCode} and {echoed.Length} body bytes");
}

static async Task<byte[]?> ReadBody(HttpRequest request)
{
    if (!HttpMethods.IsPost(request.Method))
    {
        return null;
    }

    using var buffer = new MemoryStream(BodySize + 1);
    var chunk = new byte[BodySize + 1];
    while (buffer.Length <= BodySize)
    {
        var read = await request.Body.ReadAsync(chunk.AsMemory(0, (int)Math.Min(chunk.Length, BodySize + 1 - buffer.Length)), request.HttpContext.RequestAborted);
        if (read == 0)
        {
            break;
        }
        buffer.Write(chunk, 0, read);
    }
    var body = buffer.ToArray();
    return body.Length == BodySize && body.All(value => value <= 127)
        ? body
        : null;
}

static async Task WriteBody(HttpResponse response, byte[] body)
{
    response.Headers.Connection = "close";
    response.ContentLength = body.Length;
    await response.Body.WriteAsync(body);
}

static ulong Compute(byte seedByte, int start, int count)
{
    var seed = seedByte;
    ulong total = 0;
    for (var i = start; i < start + count; i++)
    {
        total += (uint)((seed + i) % 251);
    }
    return total;
}
