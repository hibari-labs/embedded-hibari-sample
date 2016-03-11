%%%-------------------------------------------------------------------
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

-module(eh_admin_client).

-export([bootstrap/0,
         create_table/1,
         create_table/3
        ]).

%% DEBUG -compile(export_all).

%%====================================================================
%% Types / Macros
%%====================================================================

-define(SINGLE_NODE, 'emb-hibari@127.0.0.1').
-define(RPC_TIMEOUT, 60000).

%%====================================================================
%% API
%%====================================================================

-spec bootstrap() -> ok | no_return().
bootstrap() ->
    TargetNode = ?SINGLE_NODE,
    ok = start_epmd(),

    BigDataP       = true,
    DiskLoggingP   = true,
    SyncWritesP    = false,
    VarPrefixP     = true,
    VarPrefixSep   = $/,
    VarPrefixNum   = 3,
    BricksPerChain = 1,
    NumBricks      = 1,
    ok = do_bootstrap_local(TargetNode, BigDataP, DiskLoggingP, SyncWritesP,
                            VarPrefixP, VarPrefixSep, VarPrefixNum,
                            BricksPerChain, NumBricks),
    ok.

-spec create_table(atom()) -> ok | no_return().
create_table(Table) ->
    create_table(Table, ?SINGLE_NODE, []).

-spec create_table(atom(), node(), [node()]) -> ok | no_return().
create_table(Table, TargetNode, _Bricks) when is_atom(Table) ->
    %% LocalP           = true,
    BigDataP         = true,
    DiskLoggingP     = true,
    SyncWritesP      = false,
    VarPrefixP       = true,
    VarPrefixSep     = $/,
    VarPrefixNum     = 1,
    BricksPerChain   = 3,
    NumNodesPerBlock = 0,
    BlockMultFactor  = 0,

    EffBricks = begin
                    NumBricks = 12,
                    [ TargetNode || _ <- lists:seq(1, NumBricks) ]
                end,

    %% EffBricks =
    %%     if LocalP ->
    %%             NumBricks = 12,
    %%             [ TargetNode || _ <- lists:seq(1, NumBricks) ];
    %%        true ->
    %%             _Bricks
    %%     end,
    ok = do_create_table(TargetNode, Table,
                         BigDataP, DiskLoggingP, SyncWritesP,
                         VarPrefixP, VarPrefixSep, VarPrefixNum,
                         BricksPerChain, NumNodesPerBlock, BlockMultFactor,
                         EffBricks).


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

ping_node(TargetNode) ->
    %% See if the node is currently running  -- if it's not, we'll bail
    case {net_kernel:hidden_connect_node(TargetNode), net_adm:ping(TargetNode)} of
        {true, pong} ->
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
    case rpc:call(TargetNode, brick_admin, bootstrap_nodes, [], ?RPC_TIMEOUT) of
        BootstrapNodes when is_list(BootstrapNodes) ->
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

do_bootstrap_local(TargetNode, BigDataP, DiskLoggingP, SyncWritesP,
                   VarPrefixP, VarPrefixSep, VarPrefixNum,
                   BricksPerChain, NumBricks) ->
    %% read bootstrap nodes
    BootstrapNodes = bootstrap_nodes(TargetNode),

    %% check bootstrap nodes
    [ ok = ping_node(Node) || Node <- BootstrapNodes ],

    %% read schema filename
    case rpc:call(hd(BootstrapNodes), brick_admin, schema_filename, [], ?RPC_TIMEOUT) of
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
        [{maketab_bigdata, BigDataP},
         {maketab_do_logging, DiskLoggingP},
         {maketab_do_sync, SyncWritesP}],

    BootstrapArgs = [SchemaFilename, DataProps, VarPrefixP, VarPrefixSep, VarPrefixNum, NumBricks, BricksPerChain, BootstrapNodes],
    case rpc:call(hd(BootstrapNodes), brick_admin, bootstrap_local, BootstrapArgs, ?RPC_TIMEOUT) of
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
    case rpc:call(hd(BootstrapNodes), brick_admin, copy_new_schema, CopyArgs, ?RPC_TIMEOUT) of
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

%% do_bootstrap(TargetNode, BigDataP, DiskLoggingP, SyncWritesP,
%%              VarPrefixP, VarPrefixSep, VarPrefixNum,
%%              BricksPerChain, Bricks) ->

%%     %% read bootstrap nodes
%%     BootstrapNodes = bootstrap_nodes(TargetNode),

%%     %% check bootstrap nodes
%%     [ ok = ping_node(Node) || Node <- BootstrapNodes ],

%%     %% check brick nodes
%%     [ ok = ping_node(Node) || Node <- Bricks ],

%%     %% read schema filename
%%     case rpc:call(hd(BootstrapNodes), brick_admin, schema_filename, [], ?RPC_TIMEOUT) of
%%         SchemaFilename when is_list(SchemaFilename) ->
%%             ok;
%%         {badrpc, Reason1} ->
%%             SchemaFilename = "",
%%             io:format("RPC(schema_filename) to ~p failed: ~p\n"
%%                       , [hd(BootstrapNodes), Reason1]),
%%             error(1);
%%         Unknown1 ->
%%             SchemaFilename = "",
%%             io:format("RPC(schema_filename) to ~p failed due to unknown response: ~p\n"
%%                       , [hd(BootstrapNodes), Unknown1]),
%%             error(1)
%%     end,

%%     %% bootstrap
%%     DataProps =
%%         [{maketab_bigdata, BigDataP},
%%          {maketab_do_logging, DiskLoggingP},
%%          {maketab_do_sync, SyncWritesP}],

%%     BootstrapArgs = [SchemaFilename, DataProps, VarPrefixP, VarPrefixSep, VarPrefixNum,
%%                      Bricks, BricksPerChain, BootstrapNodes],
%%     case rpc:call(hd(BootstrapNodes), brick_admin, bootstrap, BootstrapArgs, ?RPC_TIMEOUT) of
%%         ok ->
%%             ok;
%%         {badrpc, Reason2} ->
%%             io:format("RPC(bootstrap) to ~p failed: ~p\n"
%%                       , [hd(BootstrapNodes), Reason2]),
%%             error(1);
%%         Unknown2 ->
%%             io:format("RPC(bootstrap) to ~p failed due to unknown response: ~p\n"
%%                       , [hd(BootstrapNodes), Unknown2]),
%%             error(1)
%%     end,

%%     %% copy Schema.local to other Brick Admin nodes
%%     CopyArgs = [tl(BootstrapNodes), SchemaFilename],
%%     case rpc:call(hd(BootstrapNodes), brick_admin, copy_new_schema, CopyArgs, ?RPC_TIMEOUT) of
%%         {ok, {_,[]}} ->
%%             ok;
%%         {badrpc, Reason3} ->
%%             io:format("RPC(copy_new_schema) to ~p failed: ~p\n"
%%                       , [hd(BootstrapNodes), Reason3]),
%%             error(1);
%%         Unknown3 ->
%%             io:format("RPC(copy_new_schmea) to ~p failed due to unknown response: ~p\n"
%%                       , [hd(BootstrapNodes), Unknown3]),
%%             error(1)
%%     end,

%%     io:format("ok~n"),
%%     ok.

do_create_table(TargetNode, Table,
                BigDataP, DiskLoggingP, SyncWritesP,
                VarPrefixP, VarPrefixSep, VarPrefixNum,
                BricksPerChain, NumNodesPerBlock, BlockMultFactor,
                Bricks) ->

    %% read bootstrap nodes
    BootstrapNodes = bootstrap_nodes(TargetNode),

    %% check bootstrap nodes
    [ ok = ping_node(Node) || Node <- BootstrapNodes ],

    %% check brick nodes
    [ ok = ping_node(Node) || Node <- Bricks ],

    MakeChainArgs = [Table, BricksPerChain, Bricks, NumNodesPerBlock, BlockMultFactor],
    case rpc:call(hd(BootstrapNodes), brick_admin, make_chain_description, MakeChainArgs, ?RPC_TIMEOUT) of
        Chains when is_list(Chains) ->
            DataProps =
                [{maketab_bigdata, BigDataP},
                 {maketab_do_logging, DiskLoggingP},
                 {maketab_do_sync, SyncWritesP}],
            MakeTableArgs = [DataProps, VarPrefixP, VarPrefixSep, VarPrefixNum, Chains],
            case rpc:call(hd(BootstrapNodes), brick_admin, make_common_table_opts, MakeTableArgs, ?RPC_TIMEOUT) of
                BrickOpts when is_list(BrickOpts) ->
                    %% io:format("DEBUG: BrickOpts: ~p~n", [BrickOpts]),
                    AddTableArgs = [Table, Chains, BrickOpts],
                    case rpc:call(hd(BootstrapNodes), brick_admin, add_table, AddTableArgs, ?RPC_TIMEOUT) of
                        ok ->
                            case rpc:call(hd(BootstrapNodes), brick_admin, spam_gh_to_all_nodes, []) of
                                ok ->
                                    ok;
                                {badrpc, Reason4} ->
                                    io:format("RPC(createtable-4) to ~p failed: ~p\n"
                                              , [hd(BootstrapNodes), Reason4]),
                                    error(1);
                                Unknown4 ->
                                    io:format("RPC(createtable-4) to ~p failed due to unknown response: ~p\n"
                                              , [hd(BootstrapNodes), Unknown4]),
                                    error(1)
                            end;
                        {badrpc, Reason3} ->
                            io:format("RPC(createtable-3) to ~p failed: ~p\n"
                                      , [hd(BootstrapNodes), Reason3]),
                            error(1);
                        Unknown3 ->
                            io:format("RPC(createtable-3) to ~p failed due to unknown response: ~p\n"
                                      , [hd(BootstrapNodes), Unknown3]),
                            error(1)
                    end;
                {badrpc, Reason2} ->
                    io:format("RPC(createtable-2) to ~p failed: ~p\n"
                              , [hd(BootstrapNodes), Reason2]),
                    error(1);
                Unknown2 ->
                    io:format("RPC(createtable-2) to ~p failed due to unknown response: ~p\n"
                              , [hd(BootstrapNodes), Unknown2]),
                    error(1)
            end;
        {badrpc, Reason1} ->
            io:format("RPC(createtable-1) to ~p failed: ~p\n"
                      , [hd(BootstrapNodes), Reason1]),
            error(1);
        Unknown1 ->
            io:format("RPC(createtable-1) to ~p failed due to unknown response: ~p\n"
                      , [hd(BootstrapNodes), Unknown1]),
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
