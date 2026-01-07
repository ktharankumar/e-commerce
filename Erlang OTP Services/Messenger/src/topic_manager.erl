%%%-------------------------------------------------------------------
%%% @author ktharnkumar
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Jan 2026 19:44
%%%-------------------------------------------------------------------
-module(topic_manager).
-behaviour(gen_server).

-author("ktharnkumar").

%% API
-export([start_link/0, publish/2, subscribe/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

publish(TopicId, Payload) ->
  gen_server:cast(?MODULE, {publish, TopicId, Payload}).

subscribe(TopicId, ConnPid) ->
  gen_server:call(?MODULE, {subscribe, TopicId, ConnPid}).

init([]) ->
  {ok, #{topics => #{}}}.

handle_call({subscribe, TopicId, ConnPid}, _From, State=#{topics := Topics}) ->
  {Pid, Topics2} = ensure_topic(TopicId, Topics),
  ok = topic_worker:subscribe(Pid, ConnPid),
  {reply, ok, State#{topics := Topics2}};

handle_call(_Req, _From, State) ->
  {reply, {error, bad_request}, State}.

handle_cast({publish, TopicId, Payload}, State=#{topics := Topics}) ->
  {Pid, Topics2} = ensure_topic(TopicId, Topics),
  ok = topic_worker:publish(Pid, Payload),
  {noreply, State#{topics := Topics2}};

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_R, _S) -> ok.
code_change(_Old, S, _Extra) -> {ok, S}.

ensure_topic(TopicId, Topics) ->
  case maps:get(TopicId, Topics, undefined) of
    undefined ->
      {ok, Pid} = topic_worker:start_link(TopicId),
      {Pid, Topics#{TopicId => Pid}};
    Pid ->
      {Pid, Topics}
  end.

