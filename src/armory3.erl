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
%% - 2009-05-10 ngerakines
%%   - Initial release
%%
%% @todo Handle priority queues
%% @todo Determine if the timer:sleep/1 model will fill process inboxes until they explode.
-module(armory3).
-compile(export_all).

-define(FETCH_DELAY, 2000).

%% armory3:bootstrap_queue().
%% armory3:start().
%% armory3:queue({character, {"US", "Medivh", "Korale"}}, fun(X) -> io:format("X ~p~n", [X]) end).
%% armory3:queue({character, {"US", "Medivh", "Jeanelly"}}, fun(X) -> io:format("X ~p~n", [X]) end).

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
    spawn_link(fun() ->
        global:whereis_name(armory_queue) ! {self(), queue, Item},
        receive X ->
            Fun(X)
        end
    end).

%% @private
bootstrap_queue() ->
    case global:whereis_name(armory_queue) of
        undefined -> proc_lib:start_link(?MODULE, start_queue, []);
        _ -> ok
    end.

%% @private
start_queue() ->
    error_logger:info_report([armory_queue, start_queue]),
    global:register_name(armory_queue, self()),
    ets:new(armory_queue, [ordered_set, named_table, protected]),
    proc_lib:init_ack(ok),
    Connection = amqp_connection:start_link("guest", "guest", "localhost"),
    Channel = lib_amqp:start_channel(Connection),
    X = uuid(),
    {'exchange.declare_ok'} = lib_amqp:declare_exchange(Channel, X),
    RoutingKey = uuid(),
    armory3:queue_loop({Channel, X, RoutingKey}).

%% @private
%% @doc The loop used by the queue process to handle the popping and pushing
%% of items to and from the queue.
queue_loop({Channel, X, RoutingKey}) ->
    receive
        {From, queue, Item} ->
            lib_amqp:publish(Channel, X, RoutingKey, erlang:term_to_binary({From, Item}));
        {From, info} ->
            From ! {Channel, X, RoutingKey}
    end,
    armory3:queue_loop({Channel, X, RoutingKey}).

uuid() ->
    {A, B, C} = now(),
    <<A:32, B:32, C:32>>.

%% @spec dequeue() -> [item()] | empty
%% @doc Takes the last item off of the queue. If the queue is empty or hasn't
%% been started then the 'empty' term is returned. This function will attempt
%% to bootstrap a local armory_queue process if it can't find the master.
info() ->
    case global:whereis_name(armory_queue) of
        undefined ->
            bootstrap_queue(),
            none;
        Queue ->
            Queue ! {self(), info},
            receive X -> X end
    end.

%% @todo Handle bad responses from info/0
bootstrap_crawler() ->
    case whereis(armory_crawler) of
        undefined ->
            case ?MODULE:info() of
                {Channel, X, RoutingKey} -> proc_lib:start_link(?MODULE, start_crawler, [{Channel, X, RoutingKey}]);
                _ ->
                    nok
            end;
        _ -> ok
    end.

%% @private
%% @doc Starts a local crawler process and registers the name on the node.
%% @todo Find a better way to generate Tag
start_crawler({Channel, X, RoutingKey}) ->
    error_logger:info_report([armory_crawler, start_queue]),
    register(armory_crawler, self()),

    Q = lib_amqp:declare_queue(Channel),
    {'queue.bind_ok'} = lib_amqp:bind_queue(Channel, X, Q, RoutingKey),
    {_, _, Tag} = now(),
    lib_amqp:subscribe(Channel, Q, self(), <<Tag:32>>),

    proc_lib:init_ack(ok),
    crawler_loop().

%% @private
%% @doc The loop used by the local crawler process to dequeue work and act on
%% it. At this point the only items processable are character,
%% achievement_summary, guild and character_achievements.
crawler_loop() ->
    receive
        {'basic.consume_ok', _} -> ok;
        {{'basic.deliver', _, _, _, _, _}, {content, _, _, _, [Data]}} ->
            process_data(erlang:binary_to_term(Data));
        XXX ->
            error_logger:error_report([armory3, {crawler_loop, 0}, unsupported_receive, XXX])
    end,
    timer:sleep(?FETCH_DELAY),
    ?MODULE:crawler_loop().

process_data({FromPid, {character, CharacterData}}) ->
    armory:process_character(FromPid, CharacterData);
process_data({FromPid, {achievement_summary, CharacterData}}) ->
    armory:process_achievement_summary(FromPid, CharacterData);
process_data({FromPid, {guild, GuildData}}) ->
    armory:process_guild(FromPid, GuildData);
process_data({FromPid, {character_achievements, CharacterData}}) ->
    armory:process_character_achievements(FromPid, CharacterData);
process_data(Other) ->
    error_logger:error_report([{armory3, {process_data, 1}, unsupported_action, Other}]),
    ok.
