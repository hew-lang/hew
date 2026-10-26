-module(calibration_handler).
-export([init/2]).

-define(BODY_SIZE, 64).
-define(WORK_UNITS, 200000).

init(Req, State) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),
    case {Method, route(Path)} of
        {<<"POST">>, unknown} -> reply(404, <<"not found">>, Req, State);
        {<<"POST">>, Route} ->
            case read_input(Req) of
                {ok, Body, Req1} -> serve(Route, Body, Req1, State);
                {error, Req1} -> reply(400, <<"expected exactly 64 ASCII bytes">>, Req1, State)
            end;
        {_, _} when Method =/= <<"POST">> -> reply(405, <<"method not allowed">>, Req, State)
    end.

route(<<"/echo">>) -> echo;
route(<<"/io">>) -> io;
route(<<"/wait">>) -> wait;
route(<<"/compute">>) -> compute;
route(<<"/fanout">>) -> fanout;
route(_) -> unknown.

read_input(Req) ->
    case cowboy_req:read_body(Req, #{length => ?BODY_SIZE + 1, period => 5000}) of
        {ok, Body, Req1} when byte_size(Body) =:= ?BODY_SIZE ->
            case ascii_body(Body) of true -> {ok, Body, Req1}; false -> {error, Req1} end;
        {ok, _Body, Req1} -> {error, Req1};
        {more, _Chunk, Req1} -> {error, Req1}
    end.

ascii_body(Body) ->
    lists:all(fun(Byte) -> Byte < 128 end, binary_to_list(Body)).

serve(echo, Body, Req, State) -> reply(200, Body, Req, State);
serve(io, Body, Req, State) ->
    case backend_echo(Body) of
        ok -> reply(200, Body, Req, State);
        {error, Reason} -> reply(502, <<"backend I/O failed: ", Reason/binary>>, Req, State)
    end;
serve(wait, Body, Req, State) ->
    timer:sleep(5),
    reply(200, Body, Req, State);
serve(compute, Body = <<Seed, _/binary>>, Req, State) ->
    Total = sum(Seed, 0, ?WORK_UNITS, 0),
    reply(200, <<Body/binary, $:, (integer_to_binary(Total))/binary>>, Req, State);
serve(fanout, Body = <<Seed, _/binary>>, Req, State) ->
    Parent = self(),
    Refs = [begin
        Ref = make_ref(),
        spawn(fun() -> Parent ! {Ref, sum(Seed, Part * 25000, (Part + 1) * 25000, 0)} end),
        Ref
    end || Part <- lists:seq(0, 7)],
    Total = gather(Refs, 0),
    reply(200, <<Body/binary, $:, (integer_to_binary(Total))/binary>>, Req, State).

sum(_Seed, Index, End, Total) when Index >= End -> Total;
sum(Seed, Index, End, Total) ->
    sum(Seed, Index + 1, End, Total + ((Seed + Index) rem 251)).

gather([], Total) -> Total;
gather([Ref | Refs], Total) ->
    receive {Ref, Partial} -> gather(Refs, Total + Partial) end.

backend_echo(Body) ->
    case os:getenv("BENCH_BACKEND") of
        false -> {error, <<"BENCH_BACKEND is required for /io">>};
        Backend ->
            case backend_port(Backend) of
                ok -> request_backend(Backend, Body);
                error -> {error, <<"BENCH_BACKEND must be host:port">>}
            end
    end.

backend_port(Backend) ->
    Parts = string:split(Backend, ":", all),
    case string:to_integer(lists:last(Parts)) of
        {Port, []} when Port > 0, Port =< 65535, hd(Parts) =/= [] -> ok;
        _ -> error
    end.

request_backend(Backend, Body) ->
    Url = "http://" ++ Backend ++ "/echo",
    case httpc:request(post, {Url, [{"connection", "close"}, {"accept-encoding", "identity"}],
                         "application/octet-stream", Body},
                       [{timeout, 10000}, {autoredirect, false}], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Body}} -> ok;
        {ok, {{_, 200, _}, _, _}} -> {error, <<"backend body mismatch">>};
        {ok, {{_, Status, _}, _, _}} -> {error, iolist_to_binary(io_lib:format("backend status ~p", [Status]))};
        {ok, _} -> {error, <<"invalid backend response">>};
        {error, Reason} -> {error, iolist_to_binary(io_lib:format("~p", [Reason]))}
    end.

reply(Status, Body, Req, State) ->
    Req1 = cowboy_req:reply(Status,
        #{<<"connection">> => <<"close">>, <<"content-length">> => integer_to_binary(byte_size(Body))},
        Body, Req),
    {ok, Req1, State}.
