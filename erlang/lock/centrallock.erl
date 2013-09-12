%%
%% This module explains how to simulate a centralized lock that is
%% served fifo


-module(centrallock).
-export([acquire/1, release/1]).


%% The operations are

send_mesg(Pid,Mesg) ->
    Pid ! { self(), Mesg}.

acquire(ServerPid) ->
    send_mesg(ServerPid, acquire),
    receive
	{Pid, _} when Pid == ServerPid ->
	    ok
    end.

release(ServerPid) ->
    send_mesg(ServerPid, release).
