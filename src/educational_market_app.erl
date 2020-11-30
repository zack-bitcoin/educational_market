%%%-------------------------------------------------------------------
%% @doc educational_market public API
%% @end
%%%-------------------------------------------------------------------

-module(educational_market_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    application:start(inets),
    inets:start(),
    start_http(),
    educational_market_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
start_http() ->
    Dispatch =
        cowboy_router:compile(
          [{'_', [
		  {"/", http_handler, []},
		  {"/[...]", file_handler, []}
		 ]}]),
    {ok, _} = cowboy:start_clear(http,
				 [{ip, {0,0,0,0}}, {port, 8000}],
				 #{env => #{dispatch => Dispatch}}),
    ok.
