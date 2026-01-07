%%%-------------------------------------------------------------------
%%% @author ktharnkumar
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Jan 2026 19:41
%%%-------------------------------------------------------------------
-module(tcp_listener).
-behaviour(gen_server).

-author("ktharnkumar").

%% API
-export([start_link/1]).
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

start_link(Port) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

init([Port]) ->
  {ok, LSock} = gen_tcp:listen(Port, [
    binary, {packet, 0}, {active, false},
    {reuseaddr, true}, {backlog, 1024}
  ]),
  self() ! accept,
  {ok, #{lsock => LSock}}.

handle_info(accept, State=#{lsock := LSock}) ->
  {ok, Sock} = gen_tcp:accept(LSock),
  {ok, _Pid} = tcp_conn:start_link(Sock),
  self() ! accept,
  {noreply, State};

handle_info(_Info, State) -> {noreply, State}.
handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
terminate(_R, _S) -> ok.
code_change(_Old, S, _Extra) -> {ok, S}.

