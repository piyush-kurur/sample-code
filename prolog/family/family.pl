human(X) :- man(X).
human(X) :- women(X).
women(X) :- wifeof(Y,X), human(Y).
husbandof(X,Y) :- wifeof(Y,X).
man(X) :- fatherof(Y,X), human(Y).

man(arjuna).
wifeof(arjuna,draupadi).
fatherof(arjuna,pandu).
