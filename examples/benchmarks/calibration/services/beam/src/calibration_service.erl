-module(calibration_service).
-export([start/0]).

start() ->
    Workers = positive_env("BENCH_WORKERS", 2),
    Bind = env("BENCH_BIND", "127.0.0.1:18080"),
    {ok, Address} = inet:parse_address(host(Bind)),
    Port = list_to_integer(port(Bind)),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(inets),
    Dispatch = cowboy_router:compile([{'_', [{'_', calibration_handler, #{}}]}]),
    TransportOpts = #{socket_opts => [{ip, Address}, {port, Port}],
        num_acceptors => Workers, num_conns_sups => 1, max_connections => 32},
    {ok, _} = cowboy:start_clear(calibration_http,
        TransportOpts, #{env => #{dispatch => Dispatch}}),
    Listener = ranch:info(calibration_http),
    ActualTransportOpts = maps:get(transport_options, Listener),
    Workers = maps:get(num_acceptors, ActualTransportOpts),
    1 = maps:get(num_conns_sups, ActualTransportOpts),
    32 = maps:get(max_connections, Listener),
    io:format("LISTENER_OPTIONS num_acceptors=~B num_conns_sups=~B max_connections=~B~n",
        [Workers, maps:get(num_conns_sups, ActualTransportOpts), maps:get(max_connections, Listener)]),
    io:format("READY ~s~n", [Bind]),
    receive stop -> ok end.

env(Name, Default) ->
    case os:getenv(Name) of false -> Default; Value -> Value end.

positive_env(Name, Default) ->
    case os:getenv(Name) of
        false -> Default;
        Raw ->
            case string:to_integer(Raw) of
                {Value, []} when Value > 0 -> Value;
                _ -> erlang:error({bad_environment, Name, "must be a positive integer"})
            end
    end.

host(Bind) -> hd(string:split(Bind, ":", all)).
port(Bind) -> lists:last(string:split(Bind, ":", all)).
