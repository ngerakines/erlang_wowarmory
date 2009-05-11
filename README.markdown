Armory is a lightweight crawler for the World of Warcraft Armory. It provides a number of methods to queue requests, monitor the request queue and process armory data. This module has several restrictions in place to allow developers to pull data from the World of Warcraft armory in a safe, throttled way.

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

When the armory server is started, character and guild requests can be queued using the armory:queue/1 and armory:queue/2 functions. Each request is handled synchronously with an additional amount of time between each processed queue item. 

The I Play WoW Facebook application uses this module as-is. Not all character and guild attributes are supported, although most of them are. I'll add more as I Play WoW uses it more. This module does not support extra data such as character talents, achievements, etc.

## Parsing

The data structure sent to the callback function is either an error tuple or
an ok tuple.

    {ok, list()}
    {error, term()}

An error tuple indicates that there was an error either in fetching the data or parsing the response. This could be due to http level issues (closed connection, server down, etc) or invalid xml being parsed.

An ok tuple indicates that the page was requested and presented but can also indicate an error. Currently there are only two cases where this can happen.

 * The character exists but is below the minimum character requirement for presentation in the armory.
 * The character doesn't exist.
 *

For both cases he return value will be something like:

    {ok,[{"errorcode","noCharacter"}]}

# armory2

The armory2 module is provides an alternative queuing model. As opposed to the core armory module, the armory2 module uses an ets table to store queue information that crawlers fetch from.

# armory3

The armory3 module provides another alternative queuing model. It will attempt to use the libampq module that is part of the rabbitmq erlang client library (developed against version 1.5.1). In this case, the master queue process will create an initial connection, channel, exchange and routing key to a rabbitmq server while crawler processes will attempt to create queues on that exchange and subscribe to them.

This module is experimental at best and has gone through no production qualifying tests or use.

## TODO

 * Fail gracefully when crawler processes die.
 * Restart gracefully when the master queue process dies or it's node goes down.
 * Hard-code the channel/exchange/etc names for reproducibility on failure and the entire system needs to be restarted or brought back up after failure.
 * Determine if the timer:sleep/1 model will fill process inboxes until they explode.
 * Handle failure in the bootstrap_crawler/0 function gracefully.
 * Find a good, clean way to safely generate subscriber tags when bringing crawlers up.