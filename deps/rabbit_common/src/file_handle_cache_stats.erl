%% This Source Code Form is subject to the terms of the Mozilla Public
%% License, v. 2.0. If a copy of the MPL was not distributed with this
%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%
%% Copyright (c) 2007-2024 Broadcom. All Rights Reserved. The term “Broadcom” refers to Broadcom Inc. and/or its subsidiaries.  All rights reserved.
%%

-module(file_handle_cache_stats).

%% stats about read / write operations that go through the fhc.

-export([init/0, update/3, update/2, update/1, get/0, inc/2]).

-define(TABLE, ?MODULE).

-define(COUNT,
        [io_reopen, mnesia_ram_tx, mnesia_disk_tx,
         msg_store_read, msg_store_write,
         queue_index_write, queue_index_read]).
-define(COUNT_TIME, [io_sync, io_seek]).
-define(COUNT_TIME_BYTES, [io_read, io_write]).

-import(rabbit_misc, [safe_ets_update_counter/3, safe_ets_update_counter/4]).

init() ->
    _ = ets:new(?TABLE, [public, named_table, {write_concurrency,true}]),
    [ets:insert(?TABLE, {{Op, Counter}, 0}) || Op      <- ?COUNT_TIME_BYTES,
                                               Counter <- [count, bytes, time]],
    [ets:insert(?TABLE, {{Op, Counter}, 0}) || Op      <- ?COUNT_TIME,
                                               Counter <- [count, time]],
    [ets:insert(?TABLE, {{Op, Counter}, 0}) || Op      <- ?COUNT,
                                               Counter <- [count]].

update(Op, Bytes, Thunk) ->
    {Time, Res} = timer_tc(Thunk),
    _ = safe_ets_update_counter(?TABLE, {Op, count}, 1),
    _ = safe_ets_update_counter(?TABLE, {Op, bytes}, Bytes),
    _ = safe_ets_update_counter(?TABLE, {Op, time}, Time),
    Res.

update(Op, Thunk) ->
    {Time, Res} = timer_tc(Thunk),
    _ = safe_ets_update_counter(?TABLE, {Op, count}, 1),
    _ = safe_ets_update_counter(?TABLE, {Op, time}, Time),
    Res.

update(Op) ->
    _ = safe_ets_update_counter(?TABLE, {Op, count}, 1),
    ok.

inc(Op, Count) ->
    _ = safe_ets_update_counter(?TABLE, {Op, count}, Count),
    ok.

get() ->
    lists:sort(ets:tab2list(?TABLE)).

timer_tc(Thunk) ->
    T1 = erlang:monotonic_time(),
    Res = Thunk(),
    T2 = erlang:monotonic_time(),
    Diff = erlang:convert_time_unit(T2 - T1, native, micro_seconds),
    {Diff, Res}.
