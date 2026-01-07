%%%-------------------------------------------------------------------
%% @doc Messenger public API
%% @end
%%%-------------------------------------------------------------------

-module('Messenger_app').

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    'Messenger_sup':start_link().

stop(_State) ->
    ok.

%% internal functions
