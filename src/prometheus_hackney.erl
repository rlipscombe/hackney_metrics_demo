-module(prometheus_hackney).

-export([new/2, delete/1, increment_counter/1, increment_counter/2, decrement_counter/1,
         decrement_counter/2, update_histogram/2, update_gauge/2, update_meter/2]).

-include_lib("kernel/include/logger.hrl").

% note the change in name
name([hackney, nb_requests]) -> hackney_nb_all_requests;
name([hackney, total_requests]) -> hackney_total_requests;
name([hackney, finished_requests]) -> hackney_finished_requests;
name([hackney, _Host, nb_requests]) -> hackney_nb_requests;
name([hackney, _Host, request_time]) -> hackney_request_time;
name([hackney, _Host, connect_time]) -> hackney_connect_time;
name([hackney, _Host, response_time]) -> hackney_response_time;
name([hackney, _Host, connect_timeout]) -> hackney_connect_timeout;
name([hackney, _Host, connect_error]) -> hackney_connect_error;
name([hackney_pool, _Host, new_connection]) -> hackney_pool_new_connection;
name([hackney_pool, _Host, reuse_connection]) -> hackney_pool_reuse_connection;
name([hackney_pool, _Pool, take_rate]) -> hackney_pool_take_rate;
name([hackney_pool, _Pool, no_socket]) -> hackney_pool_no_socket;
name([hackney_pool, _Pool, in_use_count]) -> hackney_pool_in_use_count;
name([hackney_pool, _Pool, free_count]) -> hackney_pool_free_count;
name([hackney_pool, _Pool, queue_count]) -> hackney_pool_queue_count.

help([_, nb_requests]) -> "Number of running requests";
help([_, total_requests]) -> "Total number of requests";
help([_, finished_requests]) -> "Total number of requests finished";
help([_, _Host, nb_requests]) -> "Number of running requests";
help([_, _Host, request_time]) -> "Request time";
help([_, _Host, connect_time]) -> "Connect time";
help([_, _Host, response_time]) -> "Response time";
help([_, _Host, connect_timeout]) -> "Number of connect timeout";
help([_, _Host, connect_error]) -> "Number of timeout errors";
help([hackney_pool, _Host, new_connection]) -> "Number of new pool connections per host";
help([hackney_pool, _Host, reuse_connection]) -> "Number of reused pool connections per host";
help([hackney_pool, _Pool, take_rate]) -> "Rate at which a connection is retrieved from the pool";
help([hackney_pool, _Pool, no_socket]) -> "Count of new connections";
help([hackney_pool, _Pool, in_use_count]) -> "How many connections from the pool are used";
help([hackney_pool, _Pool, free_count]) -> "Number of free sockets in the pool";
help([hackney_pool, _Pool, queue_count]) -> "Number of queued clients".

new(Type = counter, Name) ->
    ?LOG_INFO(#{f => ?FUNCTION_NAME,
                name => Name,
                type => Type}),
    prometheus_counter:declare([{name, name(Name)}, {help, help(Name)}]);
new(Type = histogram, Name) ->
    ?LOG_INFO(#{f => ?FUNCTION_NAME,
                name => Name,
                type => Type}),
    prometheus_histogram:declare([{name, name(Name)}, {help, help(Name)}]).

delete(Name) ->
    ?LOG_INFO(#{f => ?FUNCTION_NAME, name => Name}).

increment_counter(Name) ->
    increment_counter(Name, 1).

increment_counter(Name = [hackney, _], Value) ->
    ?LOG_INFO(#{f => ?FUNCTION_NAME,
                name => Name,
                value => Value}),
    prometheus_counter:inc(name(Name), Value);
increment_counter(Name0 = [hackney, Host, _], Value) when is_list(Host) ->
    ?LOG_INFO(#{f => ?FUNCTION_NAME,
                name => Name0,
                value => Value}),
    Name = name(Name0),
    ?LOG_INFO(#{name => Name}),
    prometheus_counter:declare([{name, Name}, {labels, [host]}, {help, help(Name0)}]),
    prometheus_counter:inc(Name, [Host], Value).

decrement_counter(Name) ->
    decrement_counter(Name, 1).

decrement_counter(Name = undefined, Value) ->
    ?LOG_INFO(#{f => ?FUNCTION_NAME,
                name => Name,
                value => Value}).

update_histogram(Name = undefined, Fun) when is_function(Fun, 0) ->
    Begin = os:timestamp(),
    Result = Fun(),
    Duration =
        timer:now_diff(
            os:timestamp(), Begin)
        div 1000,
    ?LOG_INFO(#{f => ?FUNCTION_NAME,
                name => Name,
                value => Duration}),
    Result;
update_histogram(Name0 = [hackney_pool, Pool, in_use_count], Value)
    when is_number(Value) ->
    ?LOG_INFO(#{f => ?FUNCTION_NAME,
                name => Name0,
                value => Value}),
                Name = name(Name0),
    prometheus_histogram:declare([{name, Name}, {labels, [pool]}, {help, help(Name0)}]),
    prometheus_histogram:observe(Name, [Pool], Value),
    ok.

update_gauge(Name = undefined, Value) ->
    ?LOG_INFO(#{f => ?FUNCTION_NAME,
                name => Name,
                value => Value}).

update_meter(Name = undefined, Value) ->
    ?LOG_INFO(#{f => ?FUNCTION_NAME,
                name => Name,
                value => Value}).
