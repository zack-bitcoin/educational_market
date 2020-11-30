-module(balances).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
        balance/2, balance/1, inflate/2, buy_shares/3, combine_shares/2, oracle_result/2, spend/3, get_shares/4, lose_shares/4, add_market/1]).
-define(LOC, "balances.db").
-record(db, {btc = dict:new(), markets = dict:new()}).
    %btc is the base currency. all markets are denominated in this.

init(ok) -> 
    process_flag(trap_exit, true),
    X = db:read(?LOC),
    Y = if
            (X == "") -> #db{};
            true -> X
        end,
    {ok, Y}.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, X) -> 
    db:save(?LOC, X),
    io:format("balances died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({add_market, MID}, X) -> 
    M = X#db.markets,
    M2 = case dict:find(MID, M) of
             error -> dict:store(MID, dict:new(), M);
             {ok, _} -> M
         end,
    X2 = X#db{markets = M2},
    {noreply, X2};
handle_cast({inflate, Pub, Amount}, X) -> 
    BTC = X#db.btc,
    Prev = case dict:find(Pub, BTC) of
               error -> 0;
               {ok, A} -> A
           end,
    BTC2 = dict:store(Pub, Prev+Amount, BTC),
    X2 = X#db{btc = BTC2},
    {noreply, X2};
handle_cast({buy, Pub, Amount, MID}, X) -> 
    BTC = X#db.btc,
    case {dict:find(Pub, X#db.btc),
          dict:find(MID, X#db.markets)} of
        {error, _} ->
            io:fwrite("account does not exist\n"),
            {noreply, X};
        {_, error} ->
            io:fwrite("market does not exist\n"),
            {noreply, X};
        {{ok, Bal}, {ok, Market}} ->
            Amount2 = min(Bal, Amount),%if you try spending more than you have, it spends the max amount.
            BTC2 = dict:store(Pub, Bal - Amount2, BTC),
            Shares = 
                case dict:find(Pub, Market) of
                    error -> {Amount2, Amount2};
                    {ok, {A1, A2}} ->
                        {A1 + Amount2,
                         A2 + Amount2}
                end,
            Market2 = dict:store(Pub, Shares, Market),
            Markets2 = dict:store(MID, Market2, X#db.markets),
            DB2 = X#db{btc = BTC2,
                       markets = Markets2},
            {noreply, DB2}
    end;
handle_cast({combine, Pub, MID}, X) -> 
    case dict:find(MID, X#db.markets) of
        error ->
            io:fwrite("market does not exist"),
            {noreply, X};
        {ok, M} ->
            case dict:find(Pub, M) of
                error ->
                    io:fwrite("you own no shares in that market"),
                    {noreply, X};
                {ok, {A1, A2}} ->
                    A = min(A1, A2),
                    M2 = dict:store(Pub, {A1 - A,
                                          A2 - A},
                                    M),
                    Bal0 = case dict:find(Pub, X#db.btc) of
                              error -> 0;
                              {ok, BY} -> BY
                           end,
                    BTC2 = dict:store(Pub, Bal0 + A, X#db.btc),
                    X2 = X#db{btc = BTC2,
                              markets = dict:store(MID, M2, X#db.markets)},
                    {noreply, X2}
            end
    end;
handle_cast({result, MID, Result}, X) -> 
    case dict:find(MID, X#db.markets) of
        error -> 
            io:fwrite("market does not exist"),
            {noreply, X};
        {ok, M} ->
            Keys = dict:fetch_keys(M),
            BTC2 = result2(Keys, Result, M, X#db.btc),
            M2 = dict:erase(MID, M),
            X2 = X#db{btc = BTC2,
                      markets = dict:store(MID, M2, X#db.markets)},
            {noreply, X2}
    end;
handle_cast({spend, From, To, Amount}, X) -> 
    BTC = X#db.btc,
    Had = case dict:find(To, BTC) of
              error -> 0;
              {ok, H} -> H
          end,
    case dict:find(From, BTC) of
        error -> {noreply, X};
        {ok, Bal} -> 
            Amount2 = min(Amount, Bal),
            BTC2 = dict:store(From, Bal-Amount2, BTC),
            BTC3 = dict:store(To, Had+Amount2, BTC2),
            X2 = X#db{btc= BTC3},
            {noreply, X2}
    end;
handle_cast({get, Pub, MID, Type, Amount}, X) -> 
    case dict:find(MID, X#db.markets) of
        error ->
            io:fwrite("that market does not exist"),
            {noreply, X};
        {ok, M} ->
            {A1, A2} = case dict:find(Pub, M) of
                           error -> {0, 0};
                           {ok, Y} -> Y
                       end,
            Bal = case Type of
                      true -> {A1 + Amount, A2};
                      false -> {A1, A2 + Amount}
                  end,
            M2 = dict:store(Pub, Bal, M),
            Markets2 = dict:store(MID, M2, X#db.markets),
            X2 = X#db{markets = Markets2},
            {noreply, X2}
    end;
handle_cast(_, X) -> {noreply, X}.
handle_call({balance, Pub}, _From, X) -> 
    Bal = case dict:find(Pub, X#db.btc) of
              error -> 0;
              {ok, A} -> A
          end,
    {reply, Bal, X};
handle_call({balance, Pub, MID}, _From, X) -> 
    case dict:find(MID, X#db.markets) of
        error -> {reply, <<"market does not exist">>, X};
        {ok, M} ->
            R = case dict:find(Pub, M) of
                    error -> {0, 0};
                    {ok, S} -> S
                end,
            {reply, R, X}
    end;
handle_call({lose, Pub, MID, Type, Amount}, _From, X) -> 
    case dict:find(MID, X#db.markets) of
        error -> 
            io:fwrite("that market does not exist"),
            {reply, fail, X};
        {ok, M} ->
            case dict:find(Pub, M) of
                error -> 
                    io:fwrite("you don't have an account to lose money from"),
                    {reply, fail, X};
                {ok, {A1, A2}} ->
                    case Type of
                        true ->
                            if
                                Amount > A1 ->
                                    io:fwrite("insufficient balance"),
                                    {reply, fail, X};
                                true ->
                                    M2 = dict:store(Pub, {A1 - Amount, A2}, M),
                                    Markets2 = dict:store(MID, M2, X#db.markets),
                                    X2 = X#db{markets = Markets2},
                                    {reply, success, X2}
                            end;
                        false ->
                            if
                                Amount > A2 ->
                                    io:fwrite("insufficient balance"),
                                    {reply, fail, X};
                                true ->
                                    M2 = dict:store(Pub, {A1, A2 - Amount} , M),
                                    Markets2 = dict:store(MID, M2, X#db.markets),
                                    X2 = X#db{markets = Markets2},
                                    {reply, success, X2}
                            end
                    end
            end
    end;
handle_call(_, _From, X) -> {reply, X, X}.

result2([], _, _, BTC) -> BTC;
result2([H|T], R, M, BTC) ->
    {ok, {A1, A2}} = dict:find(H, M),
    Reward = round((A1 * R div 10000) + (A2 * (10000-R) div 10000)),
    {ok, Bal} = dict:find(H, BTC),
    BTC2 = dict:store(H, Bal + Reward, BTC),
    result2(T, R, M, BTC2).


balance(Pub) ->
    gen_server:call(?MODULE, {balance, Pub}).
balance(Pub, MID) ->
    gen_server:call(?MODULE, {balance, Pub, MID}).
inflate(Pub, Amount) ->
    true = is_integer(Amount),
    gen_server:cast(?MODULE, {inflate, Pub, Amount}).
buy_shares(Pub, Amount, MID) ->
    true = is_integer(Amount),
    gen_server:cast(?MODULE, {buy, Pub, Amount, MID}).
combine_shares(Pub, MID) ->
    gen_server:cast(?MODULE, {combine, Pub, MID}).
oracle_result(MID, Result) ->
    true = is_integer(Result),
    true = Result >= 0,
    true = Result =< 10000,
    gen_server:cast(?MODULE, {result, MID, Result}).
spend(From, To, Amount) ->
    gen_server:cast(?MODULE, {spend, From, To, Amount}).
get_shares(Pub, MID, Type, Amount) ->
    case Type of
        true -> ok;
        false -> ok
    end,
    gen_server:cast(?MODULE, {get, Pub, MID, Type, Amount}).
lose_shares(Pub, MID, Type, Amount) ->
    case Type of
        true -> ok;
        false -> ok
    end,
    gen_server:call(?MODULE, {lose, Pub, MID, Type, Amount}).
add_market(MID) ->
    gen_server:cast(?MODULE, {add_market, MID}).
