%% Copyright (c) 2008 Nick Gerakines <nick@gerakines.net>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%% 
%% Changes
%% - 2009-02-05 ngerakines
%%   - Initial release
%% 
%% @type item() = {itemtype(), {string(), string(), string()}}.
%% @type itemtype() = character | guild | achievement_summary.
%% @author Nick Gerakines <nick@gerakines.net>
%% @copyright 2008 Nick Gerakines
%% @version 0.5
%% @doc Provides access to the World of Warcraft Armory. This module is 
%% signifigantly different than the simpler armory.erl module. Instead of
%% having a gen_server process on each node that handles it's own queues
%% and crawler function, this module uses the global module to have a single
%% queue that all crawlers pull from. At any point if the queue goes down
%% then the first client to attempt to perform an action will discover this
%% and attempt to bootstrap the queue on it's local node.
%% 
%% This model isn't 'better' than the one used by the armory module, but it is
%% different and has it's advantages. The queue itself is no longer stored as
%% a queue data structure type, but instead it uses a protected, named ets
%% table using the ordered_set model. Having a single, centralized queue
%% makes it easier to have a distributed crawling system with a simplified
%% take-over policy. Instead of trying to merge queue data, it just looses
%% what it had and starts over.
%% 
%% The caveat that I see here regards node splits. If you've got two queue
%% processes on two seperate grids and then attempt to merge the grids, there
%% needs to be code that handles that merge and selects a master node. Once
%% the master is selected then the queue needs to be drained on any non-master
%% queue processes and have them kill themselves.
-module(armory2).

-export([start/0]).
-export([queue/1, queue/2, bootstrap_queue/0, bootstrap_crawler/0]).
-export([start_queue/0, start_crawler/0, queue_loop/0, crawler_loop/0]).
-export([dequeue/0, queue_info/0, queue_length/0]).

-define(FETCH_DELAY, 1500).

%% @doc Returns a locally registered crawler pid().
start() ->
    inets:start(),
    bootstrap_crawler(),
    whereis(armory_crawler).

%% @spec queue(Item) -> pid()
%% @equiv queue(Item, fun(_) -> ok end)
queue(Item) ->
    queue(Item, fun(_) -> ok end).

%% @spec queue(Item, Fun) -> pid()
%%       Item = item()
%%       Fun = function()
%% @doc Add an item to the fetch queue and send the results to a function.
queue(Item, Fun) ->
    bootstrap_queue(),
    spawn(fun() ->
        global:whereis_name(armory_queue) ! {self(), queue, Item},
        receive X ->
            Fun(X)
        end
    end).

%% @private
%% @doc Asserts that the armory_queue process is running somewhere. The idea
%% is that the system just wants to make sure that the armory_queue process
%% has been started at some point is is available. The actual queue is an ets
%% table registered as 'armory_queue' and is created in a protected scope.
%% 
%% The queue exists as an ordered_set FIFO queue that gives priority to types
%% of items. The dequeuing process attempts to find character items first,
%% then guild and finally achievement_summary.
bootstrap_queue() ->
    case global:whereis_name(armory_queue) of
        undefined -> proc_lib:start(?MODULE, start_queue, []);
        _ -> ok
    end.

%% @private
start_queue() ->
    error_logger:info_report([armory_queue, start_queue]),
    global:register_name(armory_queue, self()),
    ets:new(armory_queue, [ordered_set, named_table, protected]),
    proc_lib:init_ack(ok),
    armory2:queue_loop().

%% @private
%% @doc The loop used by the queue process to handle the popping and pushing
%% of items to and from the queue.
queue_loop() ->
    receive
        {From, queue, Item} ->
            ets:insert(
                armory_queue,
                {term_to_binary({now(), make_ref()}), From, Item}
            );
        {From, dequeue} ->
            Data = fetch(),
            From ! Data;
        {From, info} ->
            From ! ets:info(armory_queue)
    end,
    armory2:queue_loop().

%% @private
%% @doc Pops an item off of the queue, ording the items by importance.
fetch() -> fetch([character, guild, achievement_summary, character_achievements]).
fetch([]) -> empty;
fetch([Type | Tail]) ->
    case ets:match(armory_queue, {'$1', '_', {Type, '_'}}, 1) of
        '$end_of_table' -> fetch(Tail);
        {[[Key]], _} ->
            Data = ets:lookup(armory_queue, Key),
            ets:delete(armory_queue, Key),
            Data;
        _ ->
            fetch(Tail)
    end.

%% @doc Attempts to create a locally registered armory_crawler process.
bootstrap_crawler() ->
    case whereis(armory_crawler) of
        undefined -> proc_lib:start(?MODULE, start_crawler, []);
        _ -> ok
    end.

%% @private
%% @doc Starts a local crawler process and registers the name on the node.
start_crawler() ->
    error_logger:info_report([armory_crawler, start_queue]),
    register(armory_crawler, self()),
    proc_lib:init_ack(ok),
    crawler_loop().

%% @private
%% @doc The loop used by the local crawler process to dequeue work and act on
%% it. At this point the only items processable are character,
%% achievement_summary, guild and character_achievements.
crawler_loop() ->
    try armory2:dequeue() of
        [{_, FromPid, {character, CharacterData}}] ->
            armory:process_character(FromPid, CharacterData);
        [{_, FromPid, {achievement_summary, CharacterData}}] ->
            armory:process_achievement_summary(FromPid, CharacterData);
        [{_, FromPid, {guild, GuildData}}] ->
            armory:process_guild(FromPid, GuildData);
        [{_, FromPid, {character_achievements, CharacterData}}] ->
            armory:process_character_achievements(FromPid, CharacterData);
        empty ->
            ok;
        Other ->
            error_logger:warning_report([armory_crawler, other_data, Other]),
            ok
    catch
        _:_ -> {error, queue_error}
    end,
    timer:sleep(?FETCH_DELAY),
    crawler_loop().

%% @spec dequeue() -> [item()] | empty
%% @doc Takes the last item off of the queue. If the queue is empty or hasn't
%% been started then the 'empty' term is returned. This function will attempt
%% to bootstrap a local armory_queue process if it can't find the master.
dequeue() ->
    case global:whereis_name(armory_queue) of
        undefined ->
            bootstrap_queue(),
            empty;
        Queue ->
            Queue ! {self(), dequeue},
            receive X -> X end
    end.

%% @spec queue_info() -> Result
%%       Result = [Options]
%%       Options = {memory, integer()} | {owner, pid()}, {name, atom()} | {size, integer()} | {node, atom()} | {type, atom()} | {keypos, integer()} | {protection, atom()}
%% @doc Returns information about the queue process's ets table.
queue_info() ->
    case global:whereis_name(armory_queue) of
        undefined ->
            bootstrap_queue(),
            none;
        Queue ->
            Queue ! {self(), info},
            receive X -> X end
    end.

%% @spec queue_length() -> integer()
%% @doc Returns the current queue length if possible. This function exists to
%% provide backwards compatability.
queue_length() ->
    case armory2:queue_info() of
        none -> 0;
        Data when is_list(Data) ->
            proplists:get_value(size, Data, 0);
        _ -> 0
    end.
