#!/usr/bin/env escript
%% -*- erlang -*-
%%! -noshell

main(_) ->
    etap:plan(16),
    etap_can:loaded_ok(armory, "Module 'armory' loaded"),
    etap_can:can_ok(armory, start),
    etap_can:can_ok(armory, start, 0),
    etap_can:can_ok(armory, queue),
    etap_can:can_ok(armory, queue, 1),
    etap_can:can_ok(armory, queue, 2),
    etap_can:can_ok(armory, info),
    etap_can:can_ok(armory, info, 0),
    etap_can:can_ok(armory, dequeue),
    etap_can:can_ok(armory, dequeue, 0),
    etap_can:can_ok(armory, queue_length),
    etap_can:can_ok(armory, queue_length, 0),
    
    {Status, ArmoryProcess} = armory:start(),
    etap:is(Status, ok, "armory:start/0 ok"),
    etap_application:pg2_group_exists(wowarmory_grp, "pg2 group wowarmory_grp exists"),
    
    {armory_state, Process, Queue} = armory:info(),
    etap:is(queue:len(Queue), 0, "Queue length 0"),
    etap:ok(erlang:is_process_alive(Process), "Process is alive"),
    
    etap:end_tests().
