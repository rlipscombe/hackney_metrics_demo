-module(hackney_metrics_demo_app).
-behaviour(application).
-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    hackney_metrics_demo_sup:start_link().

stop(_State) ->
    ok.
