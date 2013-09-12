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

%% The state of the lock is either unlocked or a tuple whose first
%% element is the current process that has the lock and the list of
%% processes that are waiting

stateRelease(Pid, {Pid, Queue}) ->
    case Queue of
	[HPid|Rest] ->
	    send_mesg(HPid, ok),
	    {HPid,Rest};
	[] ->
	    unlocked
    end;
stateRelease(Pid, {_,_}) ->
    send_mesg(Pid, 'you dont own the lock').

stateAcquire(Pid, unlocked) ->
    send_mesg(Pid, ok),
    {Pid,[]};
stateAcquire(Pid,{CurPid,Queue}) ->
    if Pid =:= CurPid ->
	    {CurPid,lists:append(Queue,[Pid])};
       true -> send_mesg(Pid, ok),
	       {CurPid,Queue}
    end.
