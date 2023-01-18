-module(hackney_metrics_demo_metrics).

-export([new/2, delete/1, increment_counter/1, increment_counter/2, decrement_counter/1,
         decrement_counter/2, update_histogram/2, update_gauge/2, update_meter/2]).

new(_Type, _Name) -> ok.
delete(_Name) -> ok.

increment_counter(Name) -> increment_counter(Name, 1).
increment_counter(_Name, _Value) -> ok.
decrement_counter(Name) -> decrement_counter(Name, 1).
decrement_counter(_Name, _Value) -> ok.

update_histogram(_Name, Fun) when is_function(Fun, 0) ->
    Fun();
update_histogram(_Name, Value) when is_number(Value) ->
    ok.

update_gauge(_Name, _Value) -> ok.
update_meter(_Name, _Value) -> ok.
