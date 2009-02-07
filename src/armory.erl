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
%% - 2009-02-01 ngerakines
%%   - Added support for fetching achievement summary information
%% - 2009-01-16 ngerakines
%%   - Added queue_length/0 function
%% - 2008-12-13 ngerakines
%%   - Documentation updaets
%%   - Misc package changes and cleanup
%% 
%% @author Nick Gerakines <nick@gerakines.net>
%% @copyright 2008 Nick Gerakines
%% @version 0.4
%% @doc Provides access to the World of Warcraft Armory.
-module(armory).

-author("Nick Gerakines <nick@gerakines.net>").
-version("0.4.1").

-define(FETCH_DELAY, 1500).
-define(USER_AGENT, "Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US; rv:1.8.1.9) Gecko/20071025 Firefox/2.0.0.9").
-define(ACCEPT_CHARSET, "utf-8").

%% gen_server exports
-export([
    init/1, terminate/2, code_change/3,
    handle_call/3, handle_cast/2, handle_info/2
]).

%% public API exports
-export([start/0, queue/1, queue/2, info/0, dequeue/0, queue_length/0]).

%% internal exports
-export([fetchloop/0, process_character/2, parse_character/1, parse_character_gear/1]).
-export([parse_character_skills/1, process_guild/2, parse_guild/1, parse_guild_members/1]).
-export([armory_url/1, armory_fetch/1, process_achievement_summary/2, queuew/2]).
-export([csr/3]).

-include_lib("xmerl/include/xmerl.hrl").

-record(armory_state, {fetchloop, queue}).

%% @spec start() -> {error, any()} | {ok, pid()}
%% @doc Starts an armory process that calls are made to.
start() ->
    inets:start(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @private
init(_) ->
    ok = pg2:create(wowarmory_grp),
    ok = pg2:join(wowarmory_grp, self()),
    PidFetchLoop = spawn_link(?MODULE, fetchloop, []),
    {ok, #armory_state{
        queue = queue:new(),
        fetchloop = PidFetchLoop}
    }.

%% @private
handle_call({info}, _From, State) ->
    {reply, State, State};

%% @private
handle_call({queue, Item}, From, State) ->
    OldQueue = State#armory_state.queue,    
    {reply, ok, State#armory_state{ queue = queue:in({From, Item}, OldQueue) }};

%% @private
handle_call({dequeue}, _From, State) ->
    {Item, Queue} = case queue:out(State#armory_state.queue) of
        {{value, I1}, Q2} -> {I1, Q2};
        {empty, Q1} -> {empty, Q1}
    end,
    {reply, Item, State#armory_state{ queue = Queue }};

%% @private
handle_call(stop, _From, State) -> {stop, normalStop, State};

%% @private
handle_call(_, _From, State) -> {noreply, ok, State}.

%% @private
handle_cast(_Msg, State) -> {noreply, State}.

%% @private
handle_info(_Info, State) -> {noreply, State}.

%% @private
terminate(_Reason, _State) -> ok.

%% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% @spec fetchloop() -> infinate
%% @doc The loop that dequeues items in the fetch queue and processes them.
fetchloop() ->
    try armory:dequeue() of
        {{FromPid, _FromRef}, {character, CharacterData}} ->
            armory:process_character(FromPid, CharacterData);
        {{FromPid, _FromRef}, {character_achievements, CharacterData}} ->
            armory:process_character_achievements(FromPid, CharacterData);
        {{FromPid, _FromRef}, {achievement_summary, CharacterData}} ->
            armory:process_achievement_summary(FromPid, CharacterData);
        {{FromPid, _FromRef}, {guild, GuildData}} ->
            armory:process_guild(FromPid, GuildData);
        _Other -> ok
    catch
        _:_ -> {error, queue_error}
    end,
    timer:sleep(?FETCH_DELAY),
    armory:fetchloop().

%% @spec process_character(FromPid, {RealmClass, Realm, Name}) -> ok
%% where 
%%       FromPid = pid()
%%       RealmClass = string()
%%       Realm = string()
%%       Name = string()
%% @doc Fetch a character from the armory and process the response.
process_character(FromPid, {RealmClass, Realm, Name}) ->
    Response = case armory_fetch({character, RealmClass, Realm, Name}) of
        {error, Reason} -> {error, Reason};
        {ok, Body} ->
            parse_character(Body)
    end,
    FromPid ! Response,
    ok.

process_character_achievements(FromPid, {RealmClass, Realm, Name}) ->
    Response = case armory_fetch({achievement_summary, RealmClass, Realm, Name}) of
        {error, Reason} -> {error, Reason};
        {ok, Body} ->
		try parse_achievement_summary(Body) catch _:_ -> {error, parse_error} end
    end,
    FromPid ! Response,
    ok.

%% @spec process_achievement_summary(FromPid, {RealmClass, Realm, Name}) -> ok
%% where 
%%       FromPid = pid()
%%       RealmClass = string()
%%       Realm = string()
%%       Name = string()
%% @doc Fetch and process a character's achievement summary from the armory.
process_achievement_summary(FromPid, {RealmClass, Realm, Name}) ->
    Response = case armory_fetch({achievement_summary, RealmClass, Realm, Name}) of
        {error, Reason} -> {error, Reason};
        {ok, Body} ->
		try parse_achievement_summary(Body) catch _:_ -> {error, parse_error} end
    end,
    FromPid ! Response,
    ok.

%% @spec parse_achievement_summary(XmlBody) -> Result
%% where 
%%       XmlBody = string()
%%       Result = {ok, list(any())} | {error, atom()}
%% @doc Parse a chunk of xml that supposedly represents a character's recent
%% achievement summary.
parse_achievement_summary(XmlBody) ->
    try xmerl_scan:string(XmlBody, [{quiet, true}]) of
        {Xml, _Rest} ->
            MainAttribs = [
                {"name", "/page/characterInfo/character/@name"},
                {"realm", "/page/characterInfo/character/@realm"},
                {"errorcode", "/page/characterInfo/@errCode"}
            ],
            Attribs = lists:foldl(fun({Name, Xpath}, Acc) ->
                case xmerl_xpath:string(Xpath, Xml) of
                    [#xmlAttribute{value = Value}] -> [{Name, Value} | Acc];
                    _ -> Acc
                end
            end, [], MainAttribs),
            case length(Attribs) of
                0 -> {error, parse_error};
                1 -> {ok, Attribs};
                _ ->
                    Achievements = parse_summary_achievements(Xml),
                    {ok, [{achievements, Achievements} | Attribs]}
            end;
        _ -> {error, parse_arror}
    catch
        _:_ -> {error, parse_arror}
    end.

%% @spec parse_character(XmlBody) -> Result
%% where 
%%       XmlBody = string()
%%       Result = {ok, list(any())} | {error, atom()}
%% @doc Parse a chunk of xml that supposedly represents a character.
parse_character(XmlBody) ->
    try xmerl_scan:string(XmlBody, [{quiet, true}]) of
        {Xml, _Rest} ->
            MainAttribs = [
                {"name", "/page/characterInfo/character/@name"},
                {"realm", "/page/characterInfo/character/@realm"},
                {"class", "/page/characterInfo/character/@class"},
                {"points", "/page/characterInfo/character/@points"},
                {"classid", "/page/characterInfo/character/@classId"},
                {"race", "/page/characterInfo/character/@race"},
                {"raceid", "/page/characterInfo/character/@raceId"},
                {"level", "/page/characterInfo/character/@level"},
                {"lastmodified", "/page/characterInfo/character/@lastModified"},
                {"guild", "/page/characterInfo/character/@guildName"},
                {"gender", "/page/characterInfo/character/@gender"},
                {"genderid", "/page/characterInfo/character/@genderId"},
                {"talent1", "/page/characterInfo/characterTab/talentSpec/@treeOne"},
                {"talent2", "/page/characterInfo/characterTab/talentSpec/@treeTwo"},
                {"talent3", "/page/characterInfo/characterTab/talentSpec/@treeThree"},
                {"honorkill", "/page/characterInfo/characterTab/pvp/lifetimehonorablekills/@value"},
                {"str", "/page/characterInfo/characterTab/baseStats/strength/@base"},
                {"agi", "/page/characterInfo/characterTab/baseStats/agility/@base"},
                {"sta", "/page/characterInfo/characterTab/baseStats/stamina/@base"},
                {"int", "/page/characterInfo/characterTab/baseStats/intellect/@base"},
                {"spr", "/page/characterInfo/characterTab/baseStats/spirit/@base"},
                {"arm", "/page/characterInfo/characterTab/baseStats/armor/@base"},                
                {"arcane_resistance", "/page/characterInfo/characterTab/resistances/arcane/@value"},
                {"fire_resistance", "/page/characterInfo/characterTab/resistances/fire/@value"},
                {"frost_resistance", "/page/characterInfo/characterTab/resistances/frost/@value"},
                {"holy_resistance", "/page/characterInfo/characterTab/resistances/holy/@value"},
                {"nature_resistance", "/page/characterInfo/characterTab/resistances/nature/@value"},
                {"shadow_resistance", "/page/characterInfo/characterTab/resistances/shadow/@value"},
                {"errorcode", "/page/characterInfo/@errCode"}
            ],
            Attribs = lists:foldl(fun({Name, Xpath}, Acc) ->
                case xmerl_xpath:string(Xpath, Xml) of
                    [#xmlAttribute{value = Value}] -> [{Name, Value} | Acc];
                    _ -> Acc
                end
            end, [], MainAttribs),
            case length(Attribs) of
                0 -> {error, parse_error};
                1 -> {ok, Attribs};
                _ ->
                    Gear = parse_character_gear(Xml),
                    Skills = parse_character_skills(Xml),
                    ArenaTeams = parse_character_arena_teams(Xml),
                    {ok, [ {gear, Gear}, {skills, Skills}, {arena, ArenaTeams} | Attribs]}
            end;
        _ -> {error, parse_arror}
    catch
        _:_ -> {error, parse_arror}
    end.

%% @spec parse_summary_achievements(Xml) -> Result
%%       where Xml = any()
%%       Result = list(Achivement)
%%       Achivement = {Category, CompletedDate, Description, Icon, ID, Points, Title}
%% @doc Parse any achievement records available.
parse_summary_achievements(Xml) ->
    [begin
        [#xmlAttribute{value = CategoryID}] = xmerl_xpath:string("@categoryId", Node),
        [#xmlAttribute{value = Completed}] = xmerl_xpath:string("@dateCompleted", Node),
        [#xmlAttribute{value = Desc}] = xmerl_xpath:string("@desc", Node),
        [#xmlAttribute{value = Icon}] = xmerl_xpath:string("@icon", Node),
        [#xmlAttribute{value = Id}] = xmerl_xpath:string("@id", Node),
        [#xmlAttribute{value = Points}] = xmerl_xpath:string("@points", Node),
        [#xmlAttribute{value = Title}] = xmerl_xpath:string("@title", Node),
        {CategoryID, Completed, Desc, Icon, Id, Points, Title}
    end|| Node <- xmerl_xpath:string("/page/achievements/summary/achievement", Xml)].

%% @spec parse_character_gear(Xml) -> Result
%% where 
%%       Xml = any()
%%       Result = list(Gear)
%%       Gear = {string(), string()}
%% @doc Parse any character gear records available.
parse_character_gear(Xml) ->
    [begin
        [#xmlAttribute{value = Id}] = xmerl_xpath:string("@id", Node),
        [#xmlAttribute{value = Slot}] = xmerl_xpath:string("@slot", Node),
        {"slot" ++ Slot, Id}
    end|| Node <- xmerl_xpath:string("/page/characterInfo/characterTab/items/item", Xml)].

%% @spec parse_character_skills(Xml) -> Result
%% where 
%%       Xml = any()
%%       Result = list(Skill)
%%       Skill = {string(), string()}
%% @doc Parse any character skills available.
parse_character_skills(Xml) ->
    [begin
        [#xmlAttribute{value = Name}] = xmerl_xpath:string("@name", Node),
        [#xmlAttribute{value = Value}] = xmerl_xpath:string("@value", Node),
        {Name, Value}
    end|| Node <- xmerl_xpath:string("/page/characterInfo/characterTab/professions/skill", Xml)].

%% @spec parse_character_arena_teams(Xml) -> Result
%% where 
%%       Xml = any()
%%       Result = list(Skill)
%%       Team = {string(), string()}
%% @doc Parse any character arena teams available.
%% @todo Suck out the character's team rating.
parse_character_arena_teams(Xml) ->
    [begin
        [#xmlAttribute{value = Name}] = xmerl_xpath:string("@name", Node),
        [#xmlAttribute{value = Rating}] = xmerl_xpath:string("@rating", Node),
        [#xmlAttribute{value = Size}] = xmerl_xpath:string("@size", Node),
        [#xmlAttribute{value = Ranking}] = xmerl_xpath:string("@ranking", Node),
        [#xmlAttribute{value = GamesPlayed}] = xmerl_xpath:string("@gamesPlayed", Node),
        {Name, Size, Rating, Ranking, GamesPlayed}
    end|| Node <- xmerl_xpath:string("/page/characterInfo/character/arenaTeams/arenaTeam", Xml)].

%% @spec process_guild(FromPid, {RealmClass, Realm, Name}) -> ok
%% where 
%%       FromPid = pid()
%%       RealmClass = string()
%%       Realm = string()
%%       Name = string()
%% @doc Fetch a given guild from the armory and process the response.
process_guild(FromPid, {RealmClass, Realm, Name}) ->
    Response = case armory_fetch({guild, RealmClass, Realm, Name}) of
        {error, Reason} -> {error, Reason};
        {ok, Body} ->
            parse_guild(Body)
    end,
    FromPid ! Response,
    ok.

%% @spec parse_guild(XmlBody) -> Result
%% where 
%%       XmlBody = string()
%%       Result = {ok, list(any())} | {error, atom()}
%% @doc Parse a chunk of xml that supposedly represents a character. The
%% caveat here is that the only information that is publicaly avaiable on a
%% guild page is its member list.
parse_guild(XmlBody) ->
    try xmerl_scan:string(XmlBody, [{quiet, true}]) of
        {Xml, _Rest} ->
            MainAttribs = [
                {"Name", "/page/guildKey/@name"},
                {"Realm", "/page/guildKey/@realm"}
            ],
            Attribs = lists:foldl(fun({Name, Xpath}, Acc) ->
                case xmerl_xpath:string(Xpath, Xml) of
                    [#xmlAttribute{value = Value}] -> [{Name, Value} | Acc];
                    _ -> Acc
                end
            end, [], MainAttribs),
            case length(Attribs) of
                0 -> {error, parse_error};
                1 -> {ok, Attribs};
                _ ->
                    Members = parse_guild_members(Xml),
                    {ok, [{members, Members} | Attribs]}
            end;
        _ -> {error, parse_arror}
    catch
        _:_ -> {error, parse_arror}
    end.

%% @spec parse_guild_members(Xml) -> Result
%% where 
%%       Xml = any()
%%       Result = list(Skill)
%%       Skill = {string(), string()}
%% @doc Parse any character skills available.
parse_guild_members(Xml) ->
    [begin
        [#xmlAttribute{value = Name}] = xmerl_xpath:string("@name", Node),
        [#xmlAttribute{value = Rank}] = xmerl_xpath:string("@rank", Node),
        [#xmlAttribute{value = Level}] = xmerl_xpath:string("@level", Node),
        {Name, Rank, Level}
    end|| Node <- xmerl_xpath:string("/page/guildInfo/guild/members/character", Xml)].

%% @spec armory_fetch(FetchData) -> Result
%% where 
%%       FetchData = {FetchType, RealmClass, Realm, Name}
%%       RealmClass = string()
%%       Realm = string()
%%       RealmClass = string()
%%       Name = character | guild
%%       Result = {error, any()} | {ok, string()}
armory_fetch(FetchData) ->
    Url = armory_url(FetchData),
    try http:request(get, {Url, [{"User-Agent", ?USER_AGENT}]}, [], []) of
        {ok, {_Status, _Headers, Body}} -> {ok, Body};
        F -> {error, F}
    catch
        _:_ -> {error, something_caught}
    end.

%% @private
armory_url({guild, "US", Realm, Name}) ->
    lists:concat(["http://www.wowarmory.com/guild-info.xml?r=", mochiweb_util:quote_plus(Realm), "&n=", mochiweb_util:quote_plus(Name)]);
armory_url({guild, "EU", Realm, Name}) ->
    lists:concat(["http://eu.wowarmory.com/guild-info.xml?r=", mochiweb_util:quote_plus(Realm), "&n=", mochiweb_util:quote_plus(Name)]);
armory_url({achievement_summary, "US", Realm, Name}) ->
    lists:concat(["http://www.wowarmory.com/character-achievements.xml?r=", mochiweb_util:quote_plus(Realm), "&n=", mochiweb_util:quote_plus(Name)]);
armory_url({achievement_summary, "EU", Realm, Name}) ->
    lists:concat(["http://eu.wowarmory.com/character-achievements.xml?r=", mochiweb_util:quote_plus(Realm), "&n=", mochiweb_util:quote_plus(Name)]);
armory_url({character, "US", Realm, Name}) ->
    lists:concat(["http://www.wowarmory.com/character-sheet.xml?r=", mochiweb_util:quote_plus(Realm), "&n=", mochiweb_util:quote_plus(Name)]);
armory_url({character, "EU", Realm, Name}) ->
    lists:concat(["http://eu.wowarmory.com/character-sheet.xml?r=", mochiweb_util:quote_plus(Realm), "&n=", mochiweb_util:quote_plus(Name)]).

%% @spec queue(Item) -> Result
%% where 
%%       Item = any()
%%       Result = {ok, pid()}
%% @doc Adds an item to the queue and waits for the item to finish processing.
%% This function will wait indefinitely for the response to come.
queue(Item) ->
    Self = self(),
    queue(Item, fun(X) -> Self ! X end),
    receive Data -> Data end.

%% @spec queue(Item, Fun) -> Result
%% where 
%%       Item = {Type, {RealmClass, Realm, Name}}
%%       Type = character | guild
%%       RealmClass = string()
%%       Realm = string()
%%       Name = string()
%%       Fun = fun()
%%       Result = {ok, pid()}
%% @doc Adds an item to the queue to be processed and the reponse passed to
%% the callback function.
queue(Item, Fun) ->
    spawn(fun() ->
        gen_server:call(pg2:get_closest_pid(wowarmory_grp), {queue, Item}, infinity),
        receive X ->
            Fun(X)
        end
    end).

%% @spec queuew(Item, Fun) -> Result
%% where 
%%       Item = {Type, {RealmClass, Realm, Name}}
%%       Type = character | guild
%%       RealmClass = string()
%%       Realm = string()
%%       Name = string()
%%       Fun = fun()
%%       Result = {ok, pid()}
%% @doc Waits for an item sent to the queue to be processed by a given
%% callback function.
queuew(Item, Fun) ->
    Self = self(),
    spawn(fun() ->
        gen_server:call(pg2:get_closest_pid(wowarmory_grp), {queue, Item}, infinity),
        receive X ->
            Self ! Fun(X)
        end
    end),
    receive Data -> Data end.

%% @spec info() -> any()
%% @doc Returns the state used by the armory queue.
%% 
%% This function is useful for debugging the armory server process. It will
%% return the full state including the fetchloop process and queue as is.
info() ->
    gen_server:call(pg2:get_closest_pid(wowarmory_grp), {info}, infinity).

%% @spec dequeue() -> any()
%% @doc Pops an item off of the list and returns it. This function should
%% almost never be used directly as it is used by the internal fetchloop
%% process. It can be used as a simple way to remove all items off of the
%% queue without trying to set its state manually.
dequeue() ->
    gen_server:call(pg2:get_closest_pid(wowarmory_grp), {dequeue}, infinity).

%% @spec queue_length() -> integer()
%% @doc Returns the total number of items in all armory queues.
queue_length() ->
    lists:foldl(
        fun(Pid, Count) ->
            {_, _, X} = gen_server:call(Pid, {info}, infinity),
            queue:len(X) + Count
        end,
        0,
        pg2:get_members(wowarmory_grp)
    ).

%% @spec csr(A, B, C) -> item()
%%       A = string() | binary()
%%       B = string() | binary()
%%       C = string() | binary()
csr(A, B, C) when is_binary(A) ->
    csr(binary_to_list(A), B, C);
csr(A, B, C) when is_binary(B) ->
    csr(A, binary_to_list(B), C);
csr(A, B, C) when is_binary(C) ->
    csr(A, B, binary_to_list(A));
csr(A, B, "US") ->
    csr("US", B, A);
csr(A, B, "EU") ->
    csr("EU", B, A);
csr(A, B, C) ->
    {A, B, C}.
