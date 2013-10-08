:- include('delhi-metro.pl').

%% Check whether there is a direct connection between the stations.
direct(X,Y,L) :- line(L,R), member(X,R), member(Y,R).

%% Check whether a given line passes through a given station.
passesThrough(L,X) :- line(L,R), member(X,R).

%% Check whether a given station is a junction (more than one line meets).
junction(X) :- bagof(L,passesThrough(L,X),Routes), length(Routes,U), U>1.

%% Checks whether there is a path from X to Y with N-changes.
path(X,X,_,[]).
path(X,Y,_,[[X,L,Y]]) :- direct(X,Y,L).
path(X,Y,N,Route) :-
        N > 1,
        M is N - 1,
        path(X,Y, M, Route).

path(X,Y,N,[[X,L,I]|Rest]) :-
        N > 0,
        M is N -1, junction(I), direct(X,I,L),
        path(I,Y,M, Rest).

%% Printing routines.
printStep(Step) :- format('From ~w take ~w line to ~w~n', Step).

printRoute([]) :- format('You are at your destination~n').
printRoute([Step|Rest]) :- printStep(Step), printRoute(Rest).


findRoute(X,Y) :-
        findall(L, line(L,_), Lines),
        length(Lines,N),
        path(X,Y,N,Route),
        printRoute(Route).
