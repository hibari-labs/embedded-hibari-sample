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

-module(eh_app).

-behaviour(application).

%% API
-export([manual_start/0,
         manual_stop/0
        ]).

%% Application callbacks
-export([start/2,
         stop/1,
         config_change/3
        ]).


%%%===================================================================
%%% types, specs and records
%%%===================================================================


%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%%  start up the app and all the dependent apps.
manual_start() ->
    ok = application:start(sasl),
    ok = application:start(crypto),
    ok = application:start(inets),
    %% lager:start(),
    ok = application:start(lager),
    ok = application:start(cluster_info),
    ok = application:start(gmt_util),
    ok = application:start(partition_detector),
    ok = application:start(congestion_watcher),
    ok = application:start(gdss_brick),
    ok = application:start(gdss_client),
    ok = application:start(gdss_admin),
    ok = application:start(embedded_hibari),
    ok.

%% @doc
%%  stop the app manually
manual_stop() ->
    application:stop(embedded_hibari).


%% ===================================================================
%% Application callbacks
%% ===================================================================

%% @private
-spec start(normal | {takeover, atom()} | {failover, atom()}, term()) ->
                   {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    case eh_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Other ->
            {error, Other}
    end.

%% @private
-spec stop(term()) -> ok.
stop(_State) ->
    ok.

%% @private
-spec config_change([{atom(), term()}], [{atom(), term()}], [atom()]) -> ok.
config_change(_, _, _) ->
    ok.

%% ===================================================================
%% Private functions
%% ===================================================================

%% @private
