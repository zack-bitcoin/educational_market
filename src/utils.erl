-module(utils).
-export([off/0]).

off() ->
    educational_market_sup:stop(),
    ok = application:stop(educational_market).
