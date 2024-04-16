-module(hackney_metrics_demo_metrics).

-export([
    new/2,
    delete/1,
    increment_counter/1, increment_counter/2,
    decrement_counter/1,
    decrement_counter/2,
    update_histogram/2,
    update_gauge/2,
    update_meter/2
]).

-include_lib("kernel/include/logger.hrl").

new(_Type, _Name) -> ok.
delete(_Name) -> ok.

increment_counter(Name) -> increment_counter(Name, 1).
increment_counter(Name, Value) -> ?LOG_INFO(#{f => ?FUNCTION_NAME, name => Name, value => Value}).
decrement_counter(Name) -> decrement_counter(Name, 1).
decrement_counter(Name, Value) -> ?LOG_INFO(#{f => ?FUNCTION_NAME, name => Name, value => Value}).

update_histogram(Name, Fun) when is_function(Fun, 0) ->
    Begin = os:timestamp(),
    Result = Fun(),
    Duration = timer:now_diff(os:timestamp(), Begin) div 1000,
    ?LOG_INFO(#{f => ?FUNCTION_NAME, name => Name, value => Duration}),
    Result;
update_histogram(Name, Value) when is_number(Value) ->
    ?LOG_INFO(#{f => ?FUNCTION_NAME, name => Name, value => Value}).

update_gauge(Name, Value) -> ?LOG_INFO(#{f => ?FUNCTION_NAME, name => Name, value => Value}).
update_meter(Name, Value) -> ?LOG_INFO(#{f => ?FUNCTION_NAME, name => Name, value => Value}).
