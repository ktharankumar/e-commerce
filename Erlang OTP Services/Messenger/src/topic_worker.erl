%%%-------------------------------------------------------------------
%%% @author ktharnkumar
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Jan 2026 19:45
%%%-------------------------------------------------------------------
-module(topic_worker).
-behaviour(gen_server).

-author("ktharnkumar").

%% API

-export([start_link/1, subscribe/2, publish/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {topic_id, subs = #{}}).

start_link(TopicId) ->
  gen_server:start_link(?MODULE, [TopicId], []).

subscribe(Pid, ConnPid) ->
  gen_server:call(Pid, {subscribe, ConnPid}).

publish(Pid, Payload) ->
  gen_server:cast(Pid, {publish, Payload}).

init([TopicId]) ->
  process_flag(trap_exit, true),
  {ok, #state{topic_id = TopicId}}.

handle_call({subscribe, ConnPid}, _From, S=#state{subs = Subs}) ->
  link(ConnPid),
  {reply, ok, S#state{subs = Subs#{ConnPid => true}}};
handle_call(_Req, _From, S) ->
  {reply, {error, bad_request}, S}.

handle_cast({publish, Payload}, S=#state{topic_id = TopicId, subs = Subs}) ->
  %% Ordered per-topic because gen_server serializes casts
  maps:foreach(fun(ConnPid, _) ->
    ConnPid ! {topic_event, TopicId, Payload}
               end, Subs),
  {noreply, S};
handle_cast(_Msg, S) ->
  {noreply, S}.

handle_info({'EXIT', ConnPid, _Reason}, S=#state{subs = Subs}) ->
  {noreply, S#state{subs = maps:remove(ConnPid, Subs)}};
handle_info(_Info, S) ->
  {noreply, S}.

terminate(_R, _S) -> ok.
code_change(_Old, S, _Extra) -> {ok, S}.

