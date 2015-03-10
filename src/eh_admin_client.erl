%%%-------------------------------------------------------------------
%%% Copyright (c) 2015 Hibari developers.  All rights reserved.
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

-module(eh_admin_client).

-export([bootstrap/0]).

-compile(export_all).


%%====================================================================
%% API
%%====================================================================

-spec bootstrap() -> ok.
bootstrap() ->
    TargetNode = 'emb-hibari@127.0.0.1',
    ok = start_epmd(),

    BigDataP = true,
    DiskLoggingP = true,
    SyncWritesP = true,
    VarPrefixP = false,
    VarPrefixSep = '$/',
    VarPrefixNum = 3,
    BricksPerChain = 1,
    NumBricks = 1,
    ok = bootstrap_local(TargetNode, BigDataP, DiskLoggingP, SyncWritesP,
                         VarPrefixP, VarPrefixSep, VarPrefixNum,
                         BricksPerChain, NumBricks),
    ok.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

ping_node(TargetNode) ->
    %% See if the node is currently running  -- if it's not, we'll bail
    case {net_kernel:hidden_connect_node(TargetNode), net_adm:ping(TargetNode)} of
        {true, pong} ->
            io:format("DEBUG: ping_node -> ok~n"),
            ok;
        {_, pang} ->
            io:format("Node ~p not responding to pings.\n"
                      , [TargetNode]),
            error(1);
        Unknown ->
            io:format("Node ~p unknown response to pings: ~p\n"
                      , [TargetNode, Unknown]),
            error(1)
    end.

bootstrap_nodes(TargetNode) ->
    case rpc:call(TargetNode, brick_admin, bootstrap_nodes, [], 60000) of
        BootstrapNodes when is_list(BootstrapNodes) ->
            io:format("DEBUG: bootstrap_nodes: ~p~n", [BootstrapNodes]),
            BootstrapNodes;
        {badrpc, Reason} ->
            io:format("RPC(bootstrap_nodes) to ~p failed: ~p\n"
                      , [TargetNode, Reason]),
            error(1);
        Unknown ->
            io:format("RPC(bootstrap_nodes) to ~p failed due to unknown response: ~p\n"
                      , [TargetNode, Unknown]),
            error(1)
    end.

bootstrap_local(TargetNode, BigDataP, DiskLoggingP, SyncWritesP,
                VarPrefixP, VarPrefixSep, VarPrefixNum,
                BricksPerChain, NumBricks) ->
    %% read bootstrap nodes
    BootstrapNodes = bootstrap_nodes(TargetNode),

    %% check bootstrap nodes
    [ ok = ping_node(Node) || Node <- BootstrapNodes ],

    %% read schema filename
    case rpc:call(hd(BootstrapNodes), brick_admin, schema_filename, [], 60000) of
        SchemaFilename when is_list(SchemaFilename) ->
            io:format("DEBUG: SchemaFilename: ~s~n", [SchemaFilename]),
            ok;
        {badrpc, Reason1} ->
            SchemaFilename = "",
            io:format("RPC(schema_filename) to ~p failed: ~p\n"
                      , [hd(BootstrapNodes), Reason1]),
            error(1);
        Unknown1 ->
            SchemaFilename = "",
            io:format("RPC(schema_filename) to ~p failed due to unknown response: ~p\n"
                      , [hd(BootstrapNodes), Unknown1]),
            error(1)
    end,

    %% bootstrap
    DataProps =
        [ maketab_bigdata || BigDataP ]
        ++ [ maketab_do_logging || DiskLoggingP ]
        ++ [ maketab_do_sync || SyncWritesP ],

    BootstrapArgs = [SchemaFilename, DataProps, VarPrefixP, VarPrefixSep, VarPrefixNum, NumBricks, BricksPerChain, BootstrapNodes],
    case rpc:call(hd(BootstrapNodes), brick_admin, bootstrap_local, BootstrapArgs, 60000) of
        ok ->
            io:format("DEBUG: bootstrap_local -> ok.~n"),
            ok;
        {badrpc, Reason2} ->
            io:format("RPC(bootstrap) to ~p failed: ~p\n"
                      , [hd(BootstrapNodes), Reason2]),
            error(1);
        Unknown2 ->
            io:format("RPC(bootstrap) to ~p failed due to unknown response: ~p\n"
                      , [hd(BootstrapNodes), Unknown2]),
            error(1)
    end,

    %% copy Schema.local to other Brick Admin nodes
    CopyArgs = [tl(BootstrapNodes), SchemaFilename],
    case rpc:call(hd(BootstrapNodes), brick_admin, copy_new_schema, CopyArgs, 60000) of
        {ok, {_,[]}} ->
            io:format("DEBUG: copy_new_schema -> ok.~n"),
            ok;
        {badrpc, Reason3} ->
            io:format("RPC(copy_new_schema) to ~p failed: ~p\n"
                      , [hd(BootstrapNodes), Reason3]),
            error(1);
        Unknown3 ->
            io:format("RPC(copy_new_schmea) to ~p failed due to unknown response: ~p\n"
                      , [hd(BootstrapNodes), Unknown3]),
            error(1)
    end,

    io:format("ok~n"),
    ok.

bootstrap(TargetNode, BigDataP, DiskLoggingP, SyncWritesP,
          VarPrefixP, VarPrefixSep, VarPrefixNum,
          BricksPerChain, Bricks) ->

    %% read bootstrap nodes
    BootstrapNodes = bootstrap_nodes(TargetNode),

    %% check bootstrap nodes
    [ ok = ping_node(Node) || Node <- BootstrapNodes ],

    %% check brick nodes
    [ ok = ping_node(Node) || Node <- Bricks ],

    %% read schema filename
    case rpc:call(hd(BootstrapNodes), brick_admin, schema_filename, [], 60000) of
        SchemaFilename when is_list(SchemaFilename) ->
            ok;
        {badrpc, Reason1} ->
            SchemaFilename = "",
            io:format("RPC(schema_filename) to ~p failed: ~p\n"
                      , [hd(BootstrapNodes), Reason1]),
            error(1);
        Unknown1 ->
            SchemaFilename = "",
            io:format("RPC(schema_filename) to ~p failed due to unknown response: ~p\n"
                      , [hd(BootstrapNodes), Unknown1]),
            error(1)
    end,

    %% bootstrap
    DataProps =
        [ maketab_bigdata || BigDataP ]
        ++ [ maketab_do_logging || DiskLoggingP ]
        ++ [ maketab_do_sync || SyncWritesP ],

    BootstrapArgs = [SchemaFilename, DataProps, VarPrefixP, VarPrefixSep, VarPrefixNum,
                     Bricks, BricksPerChain, BootstrapNodes],
    case rpc:call(hd(BootstrapNodes), brick_admin, bootstrap, BootstrapArgs, 60000) of
        ok ->
            ok;
        {badrpc, Reason2} ->
            io:format("RPC(bootstrap) to ~p failed: ~p\n"
                      , [hd(BootstrapNodes), Reason2]),
            error(1);
        Unknown2 ->
            io:format("RPC(bootstrap) to ~p failed due to unknown response: ~p\n"
                      , [hd(BootstrapNodes), Unknown2]),
            error(1)
    end,

    %% copy Schema.local to other Brick Admin nodes
    CopyArgs = [tl(BootstrapNodes), SchemaFilename],
    case rpc:call(hd(BootstrapNodes), brick_admin, copy_new_schema, CopyArgs, 60000) of
        {ok, {_,[]}} ->
            ok;
        {badrpc, Reason3} ->
            io:format("RPC(copy_new_schema) to ~p failed: ~p\n"
                      , [hd(BootstrapNodes), Reason3]),
            error(1);
        Unknown3 ->
            io:format("RPC(copy_new_schmea) to ~p failed due to unknown response: ~p\n"
                      , [hd(BootstrapNodes), Unknown3]),
            error(1)
    end,

    io:format("ok~n"),
    ok.

start_epmd() ->
    [] = os:cmd(epmd_path() ++ " -daemon"),
    ok.

epmd_path() ->
    %% ErtsBinDir = filename:dirname(escript:script_name()),
    Name = "epmd",
    %% case os:find_executable(Name, ErtsBinDir) of
    %%     false ->
            case os:find_executable(Name) of
                false ->
                    io:format("Could not find epmd.~n"),
                    error(1);
                GlobalEpmd ->
                    GlobalEpmd
        %%     end;
        %% Epmd ->
        %%     Epmd
    end.
