%%%-------------------------------------------------------------------
%%% @author ktharnkumar
%%% @copyright (C) 2026, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Jan 2026 19:46
%%%-------------------------------------------------------------------
-module(proto).
-author("ktharnkumar").

%% API
-export([extract_frames/1, decode_frame/1,
  encode_pub/3, encode_resp/3, encode_err/3]).

-define(V1, 1).
-define(SUB, 1).
-define(PUB, 2).
-define(REQ, 3).
-define(RESP, 4).
-define(ERR, 5).

-define(OP_GET_PRODUCT, 1).

%% Safe framing: decode as many frames as possible; keep remainder.
extract_frames(Bin) ->
  extract_frames(Bin, []).

extract_frames(<<Len:32/big, Rest/binary>>, Acc) when byte_size(Rest) >= Len ->
  <<Frame:Len/binary, Tail/binary>> = Rest,
  extract_frames(Tail, [decode_frame(Frame) | Acc]);
extract_frames(Bin, Acc) ->
  {lists:reverse(Acc), Bin}.

decode_frame(<< ?V1:8, ?SUB:8, TopicId:16/big, CorrId:64/big, _/binary >>) ->
  {sub, TopicId, CorrId};

decode_frame(<< ?V1:8, ?PUB:8, TopicId:16/big, CorrId:64/big, Payload/binary >>) ->
  {pub, TopicId, CorrId, Payload};

%% REQ payload format for MVP: <<Op:8, ProductId:32>>
decode_frame(<< ?V1:8, ?REQ:8, TopicId:16/big, CorrId:64/big, Payload/binary >>) ->
  case Payload of
    <<?OP_GET_PRODUCT:8, ProductId:32/big>> ->
      {req_get_product, TopicId, CorrId, ProductId};
    _ ->
      {unknown}
  end;

decode_frame(_) ->
  {unknown}.

encode_pub(TopicId, CorrId, Payload) ->
  Frame = <<?V1:8, ?PUB:8, TopicId:16/big, CorrId:64/big, Payload/binary>>,
  <<(byte_size(Frame)):32/big, Frame/binary>>.

encode_resp(TopicId, CorrId, Payload) ->
  Frame = <<?V1:8, ?RESP:8, TopicId:16/big, CorrId:64/big, Payload/binary>>,
  <<(byte_size(Frame)):32/big, Frame/binary>>.

encode_err(TopicId, CorrId, Code) ->
  %% Code uint16
  Frame = <<?V1:8, ?ERR:8, TopicId:16/big, CorrId:64/big, Code:16/big>>,
  <<(byte_size(Frame)):32/big, Frame/binary>>.