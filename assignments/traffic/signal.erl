%
% A simple traffic signal light that consists of two states, red and
% green. The controller is supposed to use two such lights.
%

-module(signal).
-export([signal_light/2, send_mesg/2]).

send_mesg(Pid,Message) ->
    Pid ! {self(), Message}.

switch_state(Pid, State, NewState) ->
    case NewState of
	red   ->
	    send_mesg(Pid, 'now red'),
	    signal_light(Pid, red);
	green ->
	    send_mesg(Pid, 'now green'),
	    signal_light(Pid, green);
	error ->
	    send_mesg(Pid,{'unknown new state', NewState}),
	    signal_light(Pid, State)
    end.


signal_light(State) ->
    receive
	{Pid, ask} ->
	    send_mesg(Pid,State),
	    signal_light(State);
	{Pid, NewState} ->
	    switch_state(Pid, State, NewState);
	error ->
	    signal_light(State)
    end.
