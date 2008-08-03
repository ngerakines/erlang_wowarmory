Armory is an gen_server interface to the World of Warcraft Armory. It provides a number of methods to queue requests, monitor that queue and perform actions on requests when they finish in a safe (throttled way).

    1> armory:start().
    ok
    2> HppyFun = fun(X) -> io:format("Doing stuff to ~p.~n", [X]) end.
    #Fun<erl_eval.6.35866844>
    3> Cs = ["Korale", "Jeanelly", "Enyo", "Invis"].
    ["Korale","Jeanelly","Enyo","Invis"]
    4> [armory:queue({character, {"US", "Medivh", C}}, HppyFun) || C <- Cs].
    [<0.756.0>,<0.757.0>,<0.758.0>,<0.759.0>]
    5>
    Processing character Korale of Medivh-US.
    Doing stuff to {ok,[{gear,[{"slot0","29049"}, ...
    
    Processing character Jeanelly of Medivh-US.
    Doing stuff to {ok,[{gear,[{"slot0","29076"}, ...
    
    ...

The idea is that using the armory:queue/2 function you can create queues to do work on armory data without taking the entire thing down. This is why I love Erlang.

The I Play WoW Facebook application uses this module as-is. Not all character and guild attributes are supported, although most of them are. I'll add more as I Play WoW uses it more.

This module does not performing any crawling functionality on its own although it could be modified relatively easily to crawl data. The chain would be something like:

* Character to arena teams and guild
* Guilds to characters
* Arena teams to characters

## Parsing

The data structure sent to the callback function is either an error tuple or
an ok tuple.

    `{ok, list()}`
    `{error, term()}`

An error tuple indicates that there was an error either in fetching the data or parsing the response. This could be due to http level issues (closed connection, server down, etc) or invalid xml being parsed.

The ok tuple indicates that the page was requested and presented but can also indicate an error. Currently there are only two cases where this can happen.

* The character exists but is below the minimum character requirement for presentation in the armory.
* The character doesn't exist.

For both cases he return value will be something like:

    `{ok,[{"errorcode","noCharacter"}]}`
