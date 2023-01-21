-module(prometheus_hackney).

-export([new/2, delete/1, increment_counter/1, increment_counter/2, decrement_counter/1,
         decrement_counter/2, update_histogram/2, update_gauge/2, update_meter/2]).
-export(['$handle_undefined_function'/2]).

-include_lib("kernel/include/logger.hrl").

% note the change in name
name([hackney, nb_requests]) ->
    hackney_nb_all_requests;
name([hackney, total_requests]) ->
    hackney_total_requests;
name([hackney, finished_requests]) ->
    hackney_finished_requests;
name([hackney, _Host, nb_requests]) ->
    hackney_nb_requests;
name([hackney, _Host, request_time]) ->
    hackney_request_time;
name([hackney, _Host, connect_time]) ->
    hackney_connect_time;
name([hackney, _Host, response_time]) ->
    hackney_response_time;
name([hackney, _Host, connect_timeout]) ->
    hackney_connect_timeout;
name([hackney, _Host, connect_error]) ->
    hackney_connect_error;
name([hackney_pool, _Host, new_connection]) ->
    hackney_pool_new_connection;
name([hackney_pool, _Host, reuse_connection]) ->
    hackney_pool_reuse_connection;
name([hackney_pool, _Pool, take_rate]) ->
    hackney_pool_take_rate;
name([hackney_pool, _Pool, no_socket]) ->
    hackney_pool_no_socket;
name([hackney_pool, _Pool, in_use_count]) ->
    hackney_pool_in_use_count;
name([hackney_pool, _Pool, free_count]) ->
    hackney_pool_free_count;
name([hackney_pool, _Pool, queue_count]) ->
    hackney_pool_queue_count.

help([_, nb_requests]) ->
    "Number of running requests";
help([_, total_requests]) ->
    "Total number of requests";
help([_, finished_requests]) ->
    "Total number of requests finished";
help([_, _Host, nb_requests]) ->
    "Number of running requests";
help([_, _Host, request_time]) ->
    "Request time";
help([_, _Host, connect_time]) ->
    "Connect time";
help([_, _Host, response_time]) ->
    "Response time";
help([_, _Host, connect_timeout]) ->
    "Number of connect timeout";
help([_, _Host, connect_error]) ->
    "Number of timeout errors";
help([hackney_pool, _Host, new_connection]) ->
    "Number of new pool connections per host";
help([hackney_pool, _Host, reuse_connection]) ->
    "Number of reused pool connections per host";
help([hackney_pool, _Pool, take_rate]) ->
    "Rate at which a connection is retrieved from the pool";
help([hackney_pool, _Pool, no_socket]) ->
    "Count of new connections";
help([hackney_pool, _Pool, in_use_count]) ->
    "How many connections from the pool are used";
help([hackney_pool, _Pool, free_count]) ->
    "Number of free sockets in the pool";
help([hackney_pool, _Pool, queue_count]) ->
    "Number of queued clients".

new(Type, Name) ->
    ?LOG_INFO(#{f => ?FUNCTION_NAME,
                name => Name,
                type => Type}),
    ok.

delete(Name) ->
    ?LOG_INFO(#{f => ?FUNCTION_NAME, name => Name}).

increment_counter(Name) ->
    increment_counter(Name, 1).

increment_counter(Name0 = [hackney, nb_requests], Value) ->
    % nb_requests is actually a gauge; it's incremented/decremented around each request.
    increment_gauge(Name0, Value);
increment_counter(Name0 = [hackney, _Host, nb_requests], Value) ->
    % nb_requests is actually a gauge; it's incremented/decremented around each request.
    increment_gauge(Name0, Value);
increment_counter(Name0 = [hackney, _], Value) ->
    ?LOG_INFO(#{f => ?FUNCTION_NAME,
                name => Name0,
                value => Value}),
    Name = name(Name0),
    ?LOG_INFO(#{name => Name}),
    prometheus_counter:declare([{name, Name}, {help, help(Name0)}]),
    prometheus_counter:inc(Name, Value);
increment_counter(Name0 = [hackney, Host, _], Value) when is_list(Host) ->
    ?LOG_INFO(#{f => ?FUNCTION_NAME,
                name => Name0,
                value => Value}),
    Name = name(Name0),
    ?LOG_INFO(#{name => Name}),
    prometheus_counter:declare([{name, Name}, {labels, [host]}, {help, help(Name0)}]),
    prometheus_counter:inc(Name, [Host], Value).

decrement_counter(Name) ->
    % TODO: This is not allowed by prometheus. Maybe what we want is actually a gauge?
    increment_counter(Name, -1).

decrement_counter(Name, Value) ->
    increment_counter(Name, -Value).

update_histogram(Name, Fun) when is_function(Fun, 0) ->
    {ElapsedUs, Result} = timer:tc(Fun),
    % hackney reports other durations in milliseconds, so...
    ElapsedMs = ElapsedUs / 1000,
    update_histogram(Name, ElapsedMs),
    Result;
update_histogram(Name0 = [hackney, Host, _], Value) when is_number(Value) ->
    ?LOG_INFO(#{f => ?FUNCTION_NAME,
                name => Name0,
                value => Value}),
    Name = name(Name0),
    ?LOG_INFO(#{name => Name}),
    prometheus_histogram:declare([{name, Name},
                                  {labels, [host]},
                                  {buckets, default},
                                  {help, help(Name0)}]),
                                  ?LOG_NOTICE("here"),
    prometheus_histogram:observe(Name, [Host], Value),
    ?LOG_NOTICE("here2"),
    ok;
update_histogram(Name0 = [hackney_pool, _Pool, Metric], Value)
    when Metric =:= in_use_count; Metric =:= free_count; Metric =:= queue_count ->
    % Actually a gauge; see https://github.com/benoitc/hackney/issues/560
    update_gauge(Name0, Value);
update_histogram(Name0 = [hackney_pool, Pool, _], Value) when is_number(Value) ->
    ?LOG_INFO(#{f => ?FUNCTION_NAME,
                name => Name0,
                value => Value}),
    Name = name(Name0),
    ?LOG_INFO(#{name => Name}),
    prometheus_histogram:declare([{name, Name},
                                  {labels, [pool]},
                                  {buckets, default},
                                  {help, help(Name0)}]),
    prometheus_histogram:observe(Name, [Pool], Value),
    ok.

increment_gauge(Name0 = [hackney, _], Value) ->
    ?LOG_INFO(#{f => ?FUNCTION_NAME,
                name => Name0,
                value => Value}),
    Name = name(Name0),
    ?LOG_INFO(#{name => Name}),
    prometheus_gauge:declare([{name, Name}, {help, help(Name0)}]),
    prometheus_gauge:inc(Name, Value);
increment_gauge(Name0 = [hackney, Host, _], Value) ->
    ?LOG_INFO(#{f => ?FUNCTION_NAME,
                name => Name0,
                value => Value}),
    Name = name(Name0),
    ?LOG_INFO(#{name => Name}),
    prometheus_gauge:declare([{name, Name}, {labels, [host]}, {help, help(Name0)}]),
    prometheus_gauge:inc(Name, [Host], Value).

update_gauge(Name0 = [hackney, _], Value) when is_integer(Value) ->
    ?LOG_INFO(#{f => ?FUNCTION_NAME,
                name => Name0,
                value => Value}),
    Name = name(Name0),
    ?LOG_INFO(#{name => Name}),
    prometheus_gauge:declare([{name, Name}, {help, help(Name0)}]),
    prometheus_gauge:set(Name, Value);
update_gauge(Name0 = [hackney, Host, _], Value) when is_integer(Value) ->
    ?LOG_INFO(#{f => ?FUNCTION_NAME,
                name => Name0,
                value => Value}),
    Name = name(Name0),
    ?LOG_INFO(#{name => Name}),
    prometheus_gauge:declare([{name, Name}, {labels, [host]}, {help, help(Name0)}]),
    prometheus_gauge:set(Name, [Host], Value);
update_gauge(Name0 = [hackney_pool, Pool, _], Value) when is_integer(Value) ->
    ?LOG_INFO(#{f => ?FUNCTION_NAME,
                name => Name0,
                value => Value}),
    Name = name(Name0),
    ?LOG_INFO(#{name => Name}),
    prometheus_gauge:declare([{name, Name}, {labels, [pool]}, {help, help(Name0)}]),
    prometheus_gauge:set(Name, [Pool], Value).

update_meter(Name, Value) ->
    % TODO: I'm not entirely sure how to convert this to a prometheus metric.
    ?LOG_INFO(#{f => ?FUNCTION_NAME,
                name => Name,
                value => Value}).

                '$handle_undefined_function'(Func, Args) ->
                    ?LOG_ERROR(#{f => ?FUNCTION_NAME,
                    func => Func,
                    args => Args}).
