e%%%-------------------------------------------------------------------
%%% Copyright (c) 2015-2016 Hibari developers.  All rights reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%-------------------------------------------------------------------

-module(eh_client_demo).

-export([demo/0]).

%% DEBUG
-compile(export_all).

%%====================================================================
%% Types / Macros
%%====================================================================

-define(SINGLE_NODE, 'emb-hibari@127.0.0.1').
-define(RPC_TIMEOUT, 60000).

%%====================================================================
%% API
%%====================================================================

demo() ->
    Table = demo,
    bootstrap(),
    create_demo_table(Table),
    txn_demo1(Table).

bootstrap() ->
    io:format("Bootstrapping Hibari... "),
    Res = (catch eh_admin_client:bootstrap()),
    io:format("~p~n", [Res]),
    case Res of
        ok ->
            io:format("Wait for 30 seconds.~n"),
            timer:sleep(30000),
            ok;
        _ ->
            ok
    end.

create_demo_table(Table) ->
    io:format("Creating table '~p'... ", [Table]),
    Res = (catch eh_admin_client:create_table(Table)),
    io:format("~p~n", [Res]),
    io:format("Wait for 30 seconds.~n"),
    timer:sleep(30000),
    ok.

txn_demo1(Table) ->
    KeyPrefix = <<"/100/1">>,
    KeyA      = <<"/100/1/A">>,
    KeyB      = <<"/100/1/B">>,
    ValA      = <<"AAA">>,
    MaxKVs    = 100,

    %% reset
    _ = brick_simple:delete(Table, KeyA),
    _ = brick_simple:delete(Table, KeyB),

    %% add KeyA
    {ok, _}=R1 = brick_simple:add(Table, KeyA, ValA),
    io:format("~nbrick_simple:add(~w, ~p, ~p)~n   -> ~p~n",
              [Table, KeyA, ValA, R1]),

    %% get_many
    {ok, {[{KeyA, _TS1, ValA}], false}}=R2 =
        brick_simple:get_many(Table, KeyPrefix, MaxKVs),
    io:format("~nbrick_simple:get_many(~w, ~p, ~w)~n    -> ~p~n",
              [Table, KeyPrefix, MaxKVs, R2]),

    %% txn set KeyA and set KeyB
    SetA = brick_server:make_set(KeyA, ValA, 0, []),
    SetB = brick_server:make_set(KeyB, ValA, 0, []),
    Txn = [brick_server:make_txn(), SetA, SetB],
    [{ok, TS2}, {ok, TS3}]=R3 = brick_simple:do(Table, Txn),
    io:format("~nbrick_simple:do(~w, ~p)~n    -> ~p~n", [Table, Txn, R3]),

    %% get_many
    {ok, {[{KeyA, TS2, ValA}, {KeyB, TS3, ValA}], false}}=R4 =
        brick_simple:get_many(Table, KeyPrefix, MaxKVs),
    io:format("~nbrick_simple:get_many(~w, ~p, ~w)~n    -> ~p~n~n~n",
              [Table, KeyPrefix, MaxKVs, R4]),
    ok.
