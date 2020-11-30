-module(nonces).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
        update/2, check/1]).
-define(LOC, "nonces.db").
init(ok) -> 
    process_flag(trap_exit, true),
    X = db:read(?LOC),
    Y = if
            (X == "") -> dict:new();
            true -> X
        end,
    {ok, Y}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, X) -> 
    db:save(?LOC, X),
    io:format("nonces died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({update, Pub, Nonce}, X) -> 
    Prev = case dict:find(Pub, X) of
               error -> 0;
               {ok, N} -> N
           end,
    Next = max(Prev, Nonce),
    X2 = dict:store(Pub, Next, X),
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call({check, Pub}, _From, X) -> 
    N2 = case dict:find(Pub, X) of
             error -> 0;
             {ok, N} -> N
         end,
    {reply, N2, X};
handle_call(_, _From, X) -> {reply, X, X}.

update(Pub, Nonce) ->
    %returns either success or fail.
    true = is_integer(Nonce),
    gen_server:cast(?MODULE, {update, Pub, Nonce}).
check(Pub) ->
    gen_server:call(?MODULE, {check, Pub}).
