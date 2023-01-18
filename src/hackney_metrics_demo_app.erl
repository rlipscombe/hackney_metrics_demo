%%%-------------------------------------------------------------------
%% @doc hackney_metrics_demo public API
%% @end
%%%-------------------------------------------------------------------

-module(hackney_metrics_demo_app).

-behaviour(application).

-export([start/2, stop/1]).

-include_lib("kernel/include/logger.hrl").

-define(PORT, 8126).

start(_StartType, _StartArgs) ->
    Dispatch =
        cowboy_router:compile([{'_',
                                [{"/metrics/[:registry]", prometheus_cowboy2_handler, []}]}]),
    {ok, _} = cowboy:start_clear(http, [{port, ?PORT}], #{env => #{dispatch => Dispatch}}),
    ?LOG_INFO("Cowboy server listening on port ~p", [?PORT]),

    hackney_metrics_demo_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
