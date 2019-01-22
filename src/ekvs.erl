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

-module(ekvs).

-include("ekvs.hrl").

-export([start/0, stop/0]).
-export([create_table/2]).
-export([read/1, read/2]).
-export([write/1, write/2]).
-export([delete/1, delete/2]).

start() ->
    RaMachine = {module, ekvs_store, #{}},
    RaMembers = [{?RA, Node} || Node <- ra_nodes()],
    case ra:start_or_restart_cluster(?RA, RaMachine, RaMembers) of
        {ok, _, _} = RaResult ->
            RaResult;
        Error -> Error
    end.

stop() ->
    ra:stop_server({?RA, node()}).

ra_nodes() ->
    lists:usort([node() | mnesia:system_info(running_db_nodes)]).

create_table(Name, Opts) when is_atom(Name) ->
    ekvs_store:create_table(Name, Opts).

read({Tab, Key}) ->
    read(Tab, Key).

read(Tab, Key) ->
    ekvs_store:read(Tab, Key).

write(Record) ->
    write(element(1, Record), Record).

write(Tab, Record) ->
    ra_process({write, Tab, Record}).

delete({Tab, Key}) ->
    delete(Tab, Key).

delete(Tab, Key) ->
    ra_process({delete, Tab, Key}).

ra_process(Command) ->
    case ra:process_command({?RA, node()}, Command) of
        {ok, Result, _} ->
            Result;
        {error, Reason} ->
            error(Reason)
    end.

