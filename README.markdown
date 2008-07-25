Armory is an gen_server interface to the World of Warcraft Armory. It provides a number of methods to queue requests, monitor that queue and perform actions on requests when they finish in a safe (throttled way).

    1> armory:start().
    ok
    2> HappyFun = fun(X) -> io:format("Doing stuff to ~p.~n", [X]) end.
    #Fun<erl_eval.6.35866844>
    3> Chars = ["Korale", "Jeanelly", "Enyo", "Invis"].
    ["Korale","Jeanelly","Enyo","Invis"]
    4> [ armory:queue({character, {"US", "Medivh", C}}, HappyFun) || C <- Chars].
    [<0.756.0>,<0.757.0>,<0.758.0>,<0.759.0>]
    5>
    Processing character Korale of Medivh-US.
    Doing stuff to {ok,[{gear,[{"slot0","29049"}, ...
    
    Processing character Jeanelly of Medivh-US.
    Doing stuff to {ok,[{gear,[{"slot0","29076"}, ...
    
    ...

The idea is that using the armory:queue/2 function you can create queues to do work on armory data without taking the entire thing down. This is why I love Erlang.

The I Play WoW Facebook application uses this module as-is. Not all character and guild attributes are supported, although most of them are. I'll add more as I Play WoW uses it more.
