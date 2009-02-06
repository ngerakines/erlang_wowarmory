-module(armory2).

-export([start/0]).
-export([queue/1, queue/2, bootstrap_queue/0, bootstrap_crawler/0]).
-export([start_queue/0, start_crawler/0, queue_loop/1, crawler_loop/0]).
-export([dequeue/0]).

-record(armory_queue, {queue = []}).

start() ->
    bootstrap_crawler(),
    inets:start(),
    whereis(armory_crawler).

%% [armory2:queue({character, {"US", "Medivh", XX}}, fun(X) -> io:format("Done~n", []) end ) || XX <- ["Korale", "Jeanelly", "Invis", "Enyo", "Sleepybear"]].
queue(Item) ->
    queue(Item, fun(_) -> ok end).

queue(Item, Fun) ->
    bootstrap_queue(),
    spawn(fun() ->
        global:whereis_name(armory_queue) ! {self(), queue, Item},
        receive X ->
            Fun(X)
        end
    end).

bootstrap_queue() ->
    case global:whereis_name(armory_queue) of
        undefined -> proc_lib:start(?MODULE, start_queue, []);
        _ -> ok
    end.

start_queue() ->
    error_logger:info_report([armory_queue, start_queue]),
    global:register_name(armory_queue, self()),
    ets:new(armory_queue, [ordered_set, named_table, public]),
    proc_lib:init_ack(ok),
    armory2:queue_loop(#armory_queue{}).

queue_loop(State) ->
    receive
        {From, queue, Item} ->
            ets:insert(
                armory_queue,
                {term_to_binary({now(), make_ref()}), From, Item}
            );
        {From, dequeue} ->
            Data = fetch(),
            From ! Data
    end,
    armory2:queue_loop(State).

fetch() -> fetch([character, guild, achievement_summary]).
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

bootstrap_crawler() ->
    case whereis(armory_crawler) of
        undefined -> proc_lib:start(?MODULE, start_crawler, []);
        _ -> ok
    end.

start_crawler() ->
    error_logger:info_report([armory_crawler, start_queue]),
    register(armory_crawler, self()),
    proc_lib:init_ack(ok),
    crawler_loop().

crawler_loop() ->
    try armory2:dequeue() of
        [{_, FromPid, {character, CharacterData}}] ->
            armory:process_character(FromPid, CharacterData);
        [{_, FromPid, {achievement_summary, CharacterData}}] ->
            armory:process_achievement_summary(FromPid, CharacterData);
        [{_, FromPid, {guild, GuildData}}] ->
            armory:process_guild(FromPid, GuildData);
        empty ->
            ok;
        Other ->
            error_logger:warning_report([armory_crawler, other_data, Other]),
            ok
    catch
        _:_ -> {error, queue_error}
    end,
    timer:sleep(10000),
    crawler_loop().

dequeue() ->
    case global:whereis_name(armory_queue) of
        undefined ->
            bootstrap_queue(),
            empty;
        Queue ->
            Queue ! {self(), dequeue},
            receive X -> X end
    end.
