-module(vendingmachine).
-export([start/1, get_cost/2, buy/3]).

%% State of the vending machine is the following
%% { money , items }
%% items are set of objects with price and stock
%% Error conditions


%% Action
get_cost(VM,Name) ->
    request(VM,{cost,Name}).

buy(VM, Name, Money) ->
    request(VM,{buy,Name, Money}).

request(VM,Req) ->
    VM ! { self(), Req}.

start(Items) ->
    spawn(fun () -> loop(Items) end).

loop(Items) ->
    receive
	{Pid, {cost, Name}}->
	    case orddict:find(Name,Items) of
		{ok, Cost} ->
		    Pid ! {ok, 'cost of', Name , Cost };
		error ->
		    does_not_exists(Pid,Name)
	    end;
	{Pid, {buy, Name, Money}} ->
	    case orddict:find(Name, Items) of
		{ok, Cost} ->
		    if Cost > Money ->
			    in_your_dreams(Pid,Name);
		       Cost < Money ->
			    Pid ! { ok, Name, Money - Cost};
		       true ->
			    Pid ! { ok, Name}
		    end;
		error ->
		    does_not_exists(Pid,Name)
	    end
    end,
    loop(Items).


does_not_exists(Pid, Name) ->
    Pid ! {error,'does not exists item', Name}.

in_your_dreams(Pid,Name) ->
    Pid ! {error, 'in your dreams', Name}.
