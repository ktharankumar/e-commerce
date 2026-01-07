%%%-------------------------------------------------------------------
%%% @author ktharnkumar
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Jan 2026 19:43
%%%-------------------------------------------------------------------
-module(tcp_conn).
-behaviour(gen_server).

-author("ktharnkumar").

%% API

-export([start_link/1]).
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

start_link(Sock) ->
  gen_server:start_link(?MODULE, [Sock], []).

init([Sock]) ->
  inet:setopts(Sock, [{active, once}]),
  {ok, #{sock => Sock, buf => <<>>}}.

handle_info({tcp, Sock, Bin}, State=#{sock := Sock, buf := Buf0}) ->
  inet:setopts(Sock, [{active, once}]),
  Buf1 = <<Buf0/binary, Bin/binary>>,
  {Msgs, Remainder} = proto:extract_frames(Buf1),
  lists:foreach(fun(M) -> handle_msg(M, State) end, Msgs),
  {noreply, State#{buf := Remainder}};

handle_info({tcp_closed, _Sock}, State) ->
  {stop, normal, State};

%% Events from topic_worker will arrive here:
handle_info({topic_event, TopicId, Payload}, State=#{sock := Sock}) ->
  Push = proto:encode_pub(TopicId, 0, Payload),
  _ = gen_tcp:send(Sock, Push),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
terminate(_R, _State=#{sock := Sock}) -> catch gen_tcp:close(Sock), ok.
code_change(_Old, S, _Extra) -> {ok, S}.

handle_msg({sub, TopicId, CorrId}, _State) ->
  _ = CorrId,
  topic_manager:subscribe(TopicId, self());

handle_msg({pub, TopicId, _CorrId, Payload}, _State) ->
  topic_manager:publish(TopicId, Payload);

handle_msg({req_get_product, TopicId, CorrId, _ProductId}, _State=#{sock := Sock}) ->
  %% No cache yet: in the next step, broker will call product-service (TCP) synchronously.
  %% For now, return ERR 501 so your cart can handle it.
  Err = proto:encode_err(TopicId, CorrId, 501),
  _ = gen_tcp:send(Sock, Err),
  ok;

handle_msg(_Unknown, _State) ->
  ok.
