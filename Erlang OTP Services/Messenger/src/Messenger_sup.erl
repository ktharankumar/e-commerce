%%%-------------------------------------------------------------------
%% @doc Messenger top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('Messenger_sup').

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

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
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    Port= application:get_env('Messanger', listener_port, 5555),
    Children = [
        {topic_manager, {topic_manager, start_link, []}, permanent, 5000, worker, [topic_manager]},
        {tcp_listener,  {tcp_listener,  start_link, [Port]}, permanent, 5000, worker, [tcp_listener]}
    ],
    {ok, {SupFlags, Children}}.

%% internal functions
