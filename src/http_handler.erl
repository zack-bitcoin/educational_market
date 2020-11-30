-module(http_handler).
-export([init/3, handle/2, terminate/3, doit/1,
	init/2, test/0]).
%curl -i -d '[-6,"test"]' http://localhost:8000
init(Req0, Opts) ->
    handle(Req0, Opts).	
handle(Req, State) ->
    {ok, Data, Req2} = cowboy_req:read_body(Req),
    %{IP, _} = cowboy_req:peer(Req2),
    true = is_binary(Data),
    A = packer:unpack(Data),
    B = doit(A),
    C = packer:pack(B),
    Headers = #{ 
      <<"content-type">> => 
          <<"application/octet-stream">>,
      <<"Access-Control-Allow-Origin">> => 
          <<"*">>},
    Req4 = cowboy_req:reply(
             200, Headers, C, Req2),
    {ok, Req4, State}.
init(_Type, Req, _Opts) -> 
    {ok, Req, no_state}.
terminate(_Reason, _Req, _State) -> ok.

%unsigned API
doit({nonce, Pub}) ->
    {ok, nonces:check(Pub)};
doit({balance, Pub}) ->
    {ok, balances:balance(Pub)};
doit({balance, Pub, MID}) ->
    {ok, balances:balance(Pub, MID)};
doit({markets}) ->
    {ok, markets:list()};
doit({market, MID}) ->
    {ok, markets:lookup(MID)};

%signed API
doit({buy_shares, Stx}) ->
    F = fun(Tx) ->
                {buy_shares, Pub, _, Amount,
                 MID} = Tx,
                balances:buy_shares(Pub, Amount, MID)
        end,
    signed_tx(Stx, F, false);
doit({combine_shares, Stx}) ->
    F = fun(Tx) ->
                {combine_shares, Pub, _,
                 MID} = Tx,
                balances:combine_shares(Pub, MID)
        end,
    signed_tx(Stx, F, false);
doit({bet, Stx}) ->
    F = fun(Tx) ->
                {bet, Pub, _, MID,
                 B, Amount} = Tx,
                B2 = case B of
                         1 -> true;
                         0 -> false
                     end,
                success = 
                    balances:lose_shares(
                      Pub, MID, not(B2), Amount),
                Gain = markets:bet(
                         B2, MID, Amount),
                balances:get_shares(
                  Pub, MID, B2, Gain)
        end,
    signed_tx(Stx, F, false);
doit({spend, Stx}) ->
    F = fun(Tx) ->
                {spend, From, _,
                 To, Amount} = Tx,
                balances:spend(From, To, Amount)
        end,
    signed_tx(Stx, F, false);

%admin API
doit({oracle, Stx}) -> 
    F = fun(Tx) ->
                {oracle, _, _,
                 MID, Result} = Tx,
                balances:oracle_result(MID, Result),
                markets:remove(MID)
        end,
    signed_tx(Stx, F, true);
doit({new_market, Stx}) ->
    F = fun(Tx) ->
                {new_market, _, _, 
                 Question, True, False} = Tx,
                MID = markets:new(
                        Question, True, False),
                balances:add_market(MID),
                MID
        end,
    signed_tx(Stx, F, true);
doit({inflate, Stx}) ->
    F = fun(Tx) ->
                {inflate, _, _, To, Amount} = Tx,
                balances:inflate(To, Amount)
        end,
    signed_tx(Stx, F, true);
doit({test}) ->
    {ok, <<"success">>}.

signed_tx(Stx, F, AdminCheck) ->
    true = signing:verify(Stx),
    Tx = element(2, Stx),
    Pub = element(2, Tx),
    Nonce = element(3, Tx),
    true = not(AdminCheck) or 
        admin:check(Pub),
    PrevNonce = nonces:check(Pub),
    true = Nonce > PrevNonce,
    X = F(Tx),
    nonces:update(Pub, Nonce),
    X.

test() ->
    {Pub, Priv} = signing:new_key(),
    {Pub2, Priv2} = signing:new_key(),
    admin:add(Pub),
    Amount = 100000,

    %inflation
    Tx = {inflate, Pub, 1, Pub2, Amount},
    Stx = signing:sign_tx(Tx, Pub, Priv),
    doit({inflate, Stx}),
   
    %spending, balance1
    {ok, Amount} = doit({balance, Pub2}),
    Tx2 = {spend, Pub2, 1, Pub, Amount div 2},
    Stx2 = signing:sign_tx(Tx2, Pub2, Priv2),
    doit({spend, Stx2}),
    A2 = Amount div 2,
    {ok, A2} = doit({balance, Pub2}),

    %new_market
    Tx3 = {new_market, Pub, 2, 
           <<"1+1=2">>, 1000, 1000},
    Stx3 = signing:sign_tx(Tx3, Pub, Priv),
    doit({new_market, Stx3}),

    %markets
    {ok, [MID]} = doit({markets}),

    %second market for browser testing
    Tx3b = {new_market, Pub, 3, 
           <<"1+1=3">>, 100, 1000},
    Stx3b = signing:sign_tx(Tx3b, Pub, Priv),
    doit({new_market, Stx3b}),

    
    %market
    {ok, {market, MID, <<"1+1=2">>, 1000, 1000}} = doit({market, MID}),

    %buy_shares, balance2
    Spend = 2500,
    Tx4 = {buy_shares, Pub2, 2, Spend, MID},
    Stx4 = signing:sign_tx(Tx4, Pub2, Priv2),
    doit({buy_shares, Stx4}),
    A3 = A2 - Spend,
    {ok, A3} = doit({balance, Pub2}),
    {ok, {Spend, Spend}} = 
        doit({balance, Pub2, MID}),

    %bet
    Tx5 = {bet, Pub2, 3, MID, 1, 2000},%the 1 means "true"
    Stx5 = signing:sign_tx(Tx5, Pub2, Priv2),
    doit({bet, Stx5}),
    {ok, {3167, 500}} = doit({balance, Pub2, MID}),
    {ok, {market, MID, _, 333, 3000}} =
        doit({market, MID}),
    
    %nonce
    {ok, 3} = doit({nonce, Pub2}),
    
    %combine_shares
    Tx6 = {combine_shares, Pub2, 4, MID},
    Stx6 = signing:sign_tx(Tx6, Pub2, Priv2),
    doit({combine_shares, Stx6}),
    {ok, {2667, 0}} = doit({balance, Pub2, MID}),
    A4 = A3 + 500,
    {ok, A4} = doit({balance, Pub2}),
    
    %oracle
    Tx7 = {oracle, Pub, 4, MID, 5000},
    Stx7 = signing:sign_tx(Tx7, Pub, Priv),
    doit({oracle, Stx7}),
    A5 = A4 + 1333,
    {ok, A5} = doit({balance, Pub2}),
        
    
    success.


