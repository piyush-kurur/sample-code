
human(hector).
human(paris).
human(hellen).
human(achilles).

logician(plato).
logician(aristotle).

god(appolo).
god(venus).

mortal(X) :- human(X).
immortal(X) :- god(X).
human(X) :- logician(X).
