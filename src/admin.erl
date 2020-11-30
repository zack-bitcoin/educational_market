
-module(admin).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
        check/1, add/1, remove/1]).
-define(LOC, "admin.db").
init(ok) -> 
    process_flag(trap_exit, true),
    X = db:read(?LOC),
    Y = if
            (X == "") -> [];
            true -> X
        end,
    {ok, Y}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, X) -> 
    db:save(?LOC, X),
    io:format("admin died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({add, Pub}, X) -> 
    B = is_in(Pub, X),
    X2 = if
             B -> X;
             true -> [Pub|X]
         end,
    {noreply, X2};
handle_cast({remove, Pub}, X) -> 
    X2 = remove_helper(Pub, X),
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call({check, Pub}, _From, X) -> 
    B = is_in(Pub, X),
    {reply, B, X};
handle_call(_, _From, X) -> {reply, X, X}.

is_in(_, []) -> false;
is_in(X, [X|_]) -> true;
is_in(X, [_|T]) -> 
    is_in(X, T).

remove_helper(_, []) ->
    [];
remove_helper(X, [X|T]) -> T;
remove_helper(X, [A|T]) -> 
    [A|remove_helper(X, T)].



check(Pub) ->
    gen_server:call(?MODULE, {check, Pub}).
add(Pub) ->
    gen_server:cast(?MODULE, {add, Pub}).
remove(Pub) ->
    gen_server:cast(?MODULE, {remove, Pub}).
