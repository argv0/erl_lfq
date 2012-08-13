%% -------------------------------------------------------------------
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(erl_lfq).
-export([new/0,
         in/2,
         out/1,
         byte_size/1,
         len/1]).

-on_load(init/0).

-define(nif_stub, nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, ?MODULE), 0).


new() ->
    ?nif_stub.

in(_Q, _Item) ->
    ?nif_stub.

out(_Q) ->
    ?nif_stub.

byte_size(_Q) ->
    ?nif_stub.

len(_Q) ->
    ?nif_stub.

-ifdef(TEST).

queue_test() ->
    {ok, Q} = ?MODULE:new(),
    ok = ?MODULE:in(Q, <<"foo">>),
    ok = ?MODULE:in(Q, <<"bar">>),
    <<"foo">> = ?MODULE:out(Q),
    <<"bar">> = ?MODULE:out(Q).

producer_consumer_test() ->
    {ok, Q} = ?MODULE:new(),
    PPid = self(),
    spawn(fun() -> producer_loop(Q, 0) end),
    spawn(fun() -> consumer_loop(Q, PPid) end),
    receive
        consumer_done ->
            ok
    end.

consumer_loop(Q, PPid) ->
    R = ?MODULE:out(Q),
    case R of
        <<100000:32/integer>> ->
            empty = ?MODULE:out(Q),
            PPid ! consumer_done,
            done;
        _ ->
            consumer_loop(Q, PPid)
    end.

producer_loop(_Q, Count) when Count > 100000 ->
    ok;
producer_loop(Q, Count) ->    
    ?MODULE:in(Q, <<Count:32/integer>>),
    producer_loop(Q, Count+1).

-endif.



    
