-module(markets).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
        new/3, bet/3, lookup/1, list/0, remove/1]).
-define(LOC, "markets.db").
-record(market, {mid, question, true, false}).

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
    io:format("markets died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({new, Question, MID, True, False}, X) ->
    X2 = case dict:find(MID, X) of
             {ok, _} -> X;
             error ->
                 M = #market{mid = MID,
                             question = Question,
                             true = True,
                             false = False},
                 dict:store(MID, M, X)
         end,
    {noreply, X2};
handle_cast({remove, MID}, X) -> 
    X2 = dict:erase(MID, X),
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call({bet, false, MID, Amount}, _From, X) -> 
    case dict:find(MID, X) of
        error ->
            {reply, <<"does not exist">>, X};
        {ok, M} ->
            T = M#market.true,
            F = M#market.false,
            K = T * F,
            T2 = M#market.true + Amount,
            F2 = round(K / T2),
            M2 = M#market{true = T2,
                          false = F2},
            A = F - F2,
            X2 = dict:store(MID, M2, X),
            {reply, A, X2}
    end;
handle_call({bet, true, MID, Amount}, _From, X) -> 
    case dict:find(MID, X) of
        error ->
            {reply, <<"does not exist">>, X};
        {ok, M} ->
            T = M#market.true,
            F = M#market.false,
            K = T * F,
            F2 = M#market.false + Amount,
            T2 = round(K / F2),
            M2 = M#market{true = T2,
                          false = F2},
            A = T - T2,
            X2 = dict:store(MID, M2, X),
            {reply, A, X2}
    end;
handle_call({lookup, MID}, _From, X) -> 
    case dict:find(MID, X) of
        error ->
            {reply, <<"does not exist">>, X};
        {ok, M} ->
            {reply, M, X}
    end;
handle_call(list, _From, X) -> 
    {reply, dict:fetch_keys(X), X};
handle_call(_, _From, X) -> {reply, X, X}.

new(Question, True, False) ->
    MID = hash:doit(Question),
    gen_server:cast(?MODULE, {new, Question, MID, True, False}),
    MID.
remove(MID) ->
    gen_server:cast(?MODULE, {remove, MID}).
    
bet(B, MID, Amount) ->
    case B of
        true -> ok;
        false -> ok
    end,
    true = Amount > 0,
    gen_server:call(?MODULE, {bet, B, MID, Amount}).

lookup(MID) ->
    gen_server:call(?MODULE, {lookup, MID}).
list() ->
    gen_server:call(?MODULE, list).

