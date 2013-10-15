%
% A simple traffic signal light that consists of two states, red and
% green. The controller is supposed to use two such lights.
%

-module(signal).
-export([signal_light/2, send_mesg/2]).

send_mesg(Pid,Message) ->
    Pid ! {self(), Message}.

switch_state(ControllerPid, State, NewState) ->
    case NewState of
	red   -> send_mesg(ControllerPid, 'now red'),
	         signal_light(ControllerPid, red);
	green -> send_mesg(ControllerPid, 'now green'),
		 signal_light(ControllerPid, green);
	error -> send_mesg(ControllerPid,
			      {'unknown new state', NewState}),
		 signal_light(ControllerPid, State)
    end.


signal_light(ControllerPid, State) ->
    receive
	{Pid, ask} -> send_mesg(Pid,State),
		      signal_light(ControllerPid, State);
	{Pid, NewState} when Pid == ControllerPid ->
	    switch_state(ControllerPid, State, NewState);
	{Pid, _} ->
	    send_mesg(Pid, 'Permission Denied'),
	    signal_light(ControllerPid, State);
	error ->
	    signal_light(ControllerPid, State)
    end.
