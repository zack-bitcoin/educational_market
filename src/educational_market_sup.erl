%%%-------------------------------------------------------------------
%% @doc educational_market top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(educational_market_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1, stop/0]).

-define(SERVER, ?MODULE).

stop() -> child_killer(keys()).
child_killer([]) -> [];
child_killer([H|T]) -> 
    supervisor:terminate_child(educational_market_sup, H),
    child_killer(T).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = child_maker(keys()),
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
keys() ->[admin, balances, nonces, markets].
child_maker([]) -> [];
child_maker([H|T]) ->
    [#{id => H,
       start => {H, start_link, []}}|
     child_maker(T)].

