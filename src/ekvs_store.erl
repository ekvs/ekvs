%% Copyright (c) 2018 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(ekvs_store).

-behaviour(ra_machine).

-export([create_table/2]).
-export([read/2]).

%% RA machine callbacks
-export([init/1, apply/3]).

-opaque(state() :: #{term() => term()}).

-type(command() :: {write, Tab :: atom(), Record :: tuple()} |
                   {delete, Tab :: atom(), Key :: term()}).

-export_type([state/0, command/0]).

%% @doc Create a local disc table.
create_table(Name, Opts) ->
    Opts1 = lists:ukeysort(1, [{local_content, true},
                               {disc_copies, [node()]} | Opts]),
    case mnesia:create_table(Name, Opts1) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, _Name}} -> ok;
        {aborted, {already_exists, _Name, _Node}} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

read(Tab, Key) ->
    mnesia:ets(fun mnesia:read/2, [Tab, Key]).

%%--------------------------------------------------------------------
%% RA machine callbacks
%%--------------------------------------------------------------------

init(_Config) ->
    #{}.

apply(_Meta, {write, Tab, Record}, State) ->
    Result = mnesia:dirty_write(Tab, Record),
    {State, Result, []};

apply(_Meta, {delete, Tab, Key}, State) ->
    Result = mnesia:dirty_delete(Tab, Key),
    {State, Result , []}.

