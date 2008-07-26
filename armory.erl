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
%% @author Nick Gerakines <nick@gerakines.net>
%% @copyright 2008 Nick Gerakines
%% @version 0.1
%% @doc Provides access to the World of Warcraft Armory.
-module(armory).

-author("Nick Gerakines <nick@gerakines.net>").
-version("0.1").

-define(FETCH_DELAY, 2000).
-define(USER_AGENT, "Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US; rv:1.8.1.9) Gecko/20071025 Firefox/2.0.0.9").
-define(ACCEPT_CHARSET, "utf-8").

-export([
    init/1, terminate/2, code_change/3,
    handle_call/3, handle_cast/2, handle_info/2
]).

-compile(export_all).

-include_lib("xmerl/include/xmerl.hrl").

-record(armory_state, {fetchloop, queue}).

start() ->
    inets:start(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    process_flag(trap_exit, true),
    ok = pg2:create(armory),
    ok = pg2:join(armory, self()),
    {ok, #armory_state{
        queue = queue:new(),
        fetchloop = spawn(?MODULE, fetchloop, [])}
    }.

fetchloop() ->
    try armory:dequeue() of
        {{FromPid, _FromRef}, {character, CharacterData}} ->
            armory:process_character(FromPid, CharacterData);
        {{FromPid, _FromRef}, {guild, GuildData}} ->
            armory:process_guild(FromPid, GuildData);
        _ -> timer:sleep(?FETCH_DELAY * 3)
    catch
        _:_ -> {error, parse_arror}
    end,
    timer:sleep(?FETCH_DELAY),
    armory:fetchloop().

process_character(FromPid, {RealmClass, Realm, Name}) ->
    io:format("Processing character ~s of ~s-~s.~n", [Name, Realm, RealmClass]),
    Response = case armory_fetch({character, RealmClass, Realm, Name}) of
        {error, Reason} -> {error, Reason};
        {ok, Body} ->
            parse_character(Body)
    end,
    FromPid ! Response,
    ok.

parse_character(XmlBody) ->
    try xmerl_scan:string(XmlBody, [{quiet, true}]) of
        {Xml, _Rest} ->
            MainAttribs = [
                {"Name", "/page/characterInfo/character/@name"},
                {"Realm", "/page/characterInfo/character/@realm"},
                {"Class", "/page/characterInfo/character/@class"},
                {"Race", "/page/characterInfo/character/@race"},
                {"Level", "/page/characterInfo/character/@level"},
                {"LastModified", "/page/characterInfo/character/@lastModified"},
                {"Guild", "/page/characterInfo/character/@guildName"},
                {"Gender", "/page/characterInfo/character/@gender"},
                {"Talent1", "/page/characterInfo/characterTab/talentSpec/@treeOne"},
                {"Talent2", "/page/characterInfo/characterTab/talentSpec/@treeTwo"},
                {"Talent3", "/page/characterInfo/characterTab/talentSpec/@treeThree"},
                {"HonorKill", "/page/characterInfo/characterTab/pvp/lifetimehonorablekills/@value"},
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
                {"shadow_resistance", "/page/characterInfo/characterTab/resistances/shadow/@value"}
            ],
            Attribs = lists:foldl(fun({Name, Xpath}, Acc) ->
                case xmerl_xpath:string(Xpath, Xml) of
                    [#xmlAttribute{value = Value}] -> [{Name, Value} | Acc];
                    _ -> Acc
                end
            end, [], MainAttribs),
            case length(Attribs) of
                0 -> {error, parse_error};
                _ ->
                    Gear = parse_character_gear(Xml),
                    Skills = parse_character_skills(Xml),            
                    {ok, [ {gear, Gear}, {skills, Skills} | Attribs]}
            end;
        _ -> {error, parse_arror}
    catch
        _:_ -> {error, parse_arror}
    end.

parse_character_gear(Xml) ->
    [begin
        [#xmlAttribute{value = Id}] = xmerl_xpath:string("@id", Node),
        [#xmlAttribute{value = Slot}] = xmerl_xpath:string("@slot", Node),
        {"slot" ++ Slot, Id}
    end|| Node <- xmerl_xpath:string("/page/characterInfo/characterTab/items/item", Xml)].

parse_character_skills(Xml) ->
    [begin
        [#xmlAttribute{value = Name}] = xmerl_xpath:string("@name", Node),
        [#xmlAttribute{value = Value}] = xmerl_xpath:string("@value", Node),
        {Name, Value}
    end|| Node <- xmerl_xpath:string("/page/characterInfo/characterTab/professions/skill", Xml)].

process_guild(FromPid, {RealmClass, Realm, Name}) ->
    io:format("Processing guild ~s of ~s-~s.~n", [Name, Realm, RealmClass]),
    Response = case armory_fetch({guild, RealmClass, Realm, Name}) of
        {error, Reason} -> {error, Reason};
        {ok, Body} ->
            parse_guild(Body)
    end,
    FromPid ! Response,
    ok.

parse_guild(XmlBody) ->
    try xmerl_scan:string(XmlBody, [{quiet, true}]) of
        {Xml, _Rest} ->
            MainAttribs = [
                {"Name", "/page/guildKey/@name"},
                {"Realm", "/page/guildKey/@realm"}
            ],
            Attribs = [begin
                [#xmlAttribute{value = Value}] = xmerl_xpath:string(Xpath, Xml), {Name, Value}
            end|| {Name, Xpath} <- MainAttribs],
            Members = parse_guild_members(Xml),
            {ok, [ {members, Members} | Attribs]};
        _ -> {error, parse_arror}
    catch
        _:_ -> {error, parse_arror}
    end.

parse_guild_members(Xml) ->
    [begin
        [#xmlAttribute{value = Name}] = xmerl_xpath:string("@name", Node),
        [#xmlAttribute{value = Rank}] = xmerl_xpath:string("@rank", Node),
        {Name, Rank}
    end|| Node <- xmerl_xpath:string("/page/guildInfo/guild/members/character", Xml)].

handle_call({info}, _From, State) ->
    {reply, State, State};

handle_call({queue, Item}, From, State) ->
    OldQueue = State#armory_state.queue,    
    {reply, ok, State#armory_state{ queue = queue:in({From, Item}, OldQueue) }};

handle_call({dequeue}, _From, State) ->
    {Item, Queue} = case queue:out(State#armory_state.queue) of
        {{value, I1}, Q2} -> {I1, Q2};
        {empty, Q1} -> {empty, Q1}
    end,
    {reply, Item, State#armory_state{ queue = Queue }};

handle_call(stop, _From, State) -> {stop, normalStop, State};

handle_call(_, _From, State) -> {noreply, ok, State}.

%% @private
handle_cast(_Msg, State) -> {noreply, State}.

%% @private
handle_info(_Info, State) -> {noreply, State}.

%% @private
terminate(_Reason, _State) -> ok.

%% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% pragma mark -
%% pragma mark Internal methods

armory_fetch(FetchData) ->
    Url = armory_url(FetchData),
    case http:request(get, {Url, [{"User-Agent", ?USER_AGENT}]}, [], []) of
        {ok, {_Status, _Headers, Body}} -> {ok, Body};
        F -> {error, F}
    end.

armory_url({guild, "US", Realm, Name}) ->
    lists:concat(["http://www.wowarmory.com/guild-info.xml?r=", yaws_api:url_encode(Realm), "&n=", yaws_api:url_encode(Name)]);

armory_url({guild, "EU", Realm, Name}) ->
    lists:concat(["http://eu.wowarmory.com/guild-info.xml?r=", yaws_api:url_encode(Realm), "&n=", yaws_api:url_encode(Name)]);

armory_url({character, "US", Realm, Name}) ->
    lists:concat(["http://www.wowarmory.com/character-sheet.xml?r=", yaws_api:url_encode(Realm), "&n=", yaws_api:url_encode(Name)]);

armory_url({character, "EU", Realm, Name}) ->
    lists:concat(["http://eu.wowarmory.com/character-sheet.xml?r=", yaws_api:url_encode(Realm), "&n=", yaws_api:url_encode(Name)]).

%% Public methods

%% armory:queue({character, {"US", "Medivh", "Korale"}}).
%% armory:queue({character, {"US", "Medivh", "Jeanelly"}}).
%% armory:queue({character, {"US", "Medivh", "Enyo"}}).
%% armory:queue({character, {"US", "Medivh", "Invis"}}).
queue(Item) ->
    queue(Item, fun(_) -> ok end).

queue(Item, Fun) ->
    spawn(fun() ->
        gen_server:call(pg2:get_closest_pid(armory), {queue, Item}, infinity),
        receive X ->
            Fun(X)
        end
    end).

%% armory:info().
info() ->
    Resp = gen_server:call(pg2:get_closest_pid(armory), {info}, infinity),
    Resp.

dequeue() ->
    gen_server:call(pg2:get_closest_pid(armory), {dequeue}, infinity).
