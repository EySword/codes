% a nonassertive version of nani search

nani :-
    write(‘Welcome to Nani Search’),
    nl,
    initial_state(State),
    control_loop(State).

control_loop(State) :-
    end_condition(State).
control_loop(State) :-
    repeat,
    write('>'),
    read(X),
    constraint(State, X),
    do(State, NewState, X),
    control_loop(NewState).


% initial dynamic state

initial_state([
    here(kitchen),
    have([]),
    location([
        kitchen/apple,
        kitchen/broccoli,
        office/desk,
        office/flashlight,
        cellar/nani ]),
    status([
        flashlight/off,
        game/on])
    ]).

% static state

rooms([office, kitchen, cellar]).

doors([office/kitchen, cellar/kitchen]).

connect(X,Y) :-
    doors(DoorList),
    member(X/Y, DoorList).
connect(X,Y) :-
    doors(DoorList),
    member(Y/X, DoorList).

% list utilities

member(X,[X|Y]).
member(X,[Y|Z]) :- member(X,Z).

delete(X, [], []).
delete(X, [X|T], T).
delete(X, [H|T], [H|Z]) :- delete(X, T, Z).

% state manipulation utilities

get_state(State, here, X) :-
    member(here(X), State).
get_state(State, have, X) :-
    member(have(Haves), State),
    member(X, Haves).
get_state(State, location, Loc/X) :-
    member(location(Locs), State),
    member(Loc/X, Locs).
get_state(State, status, Thing/Stat) :-
    member(status(Stats), State),
    member(Thing/Stat, Stats).

del_state(OldState, [location(NewLocs) | Temp], location, Loc/X):-
    delete(location(Locs), OldState, Temp),
    delete(Loc/X, Locs, NewLocs).

add_state(OldState, [here(X)|Temp], here, X) :-
    delete(here(), OldState, Temp).
add_state(OldState, [have([X|Haves])|Temp], have, X) :-
    delete(have(Haves), OldState, Temp).
add_state(OldState, [status([Thing/Stat|TempStats])|Temp], status, Thing/Stat) :-
    delete(status(Stats), OldState, Temp),
    delete(Thing/, Stats, TempStats).

% end condition

end_condition(State) :-
    get_state(State, have, nani),
    write(‘You win’).
end_condition(State) :-
    get_state(State, status, game/off),
    write(‘quitter’).

% constraints and puzzles together

constraint(State, goto(cellar)) :-
    !, can_go_cellar(State).
constraint(State, goto(X)) :-
    !, can_go(State, X).
constraint(State, take(X)) :-
    !, can_take(State, X).
constraint(State, turn_on(X)) :-
    !, can_turn_on(State, X).
constraint(_, _).

can_go(State,X) :-
    get_state(State, here, H),
connect(X,H).
can_go(_, X) :-
    write(‘You can’‘t get there from here’),
    nl, fail.

can_go_cellar(State) :-
    can_go(State, cellar),
    !, cellar_puzzle(State).

cellar_puzzle(State) :-
    get_state(State, have, flashlight),
    get_state(State, status, flashlight/on).
cellar_puzzle(_) :-
    write(‘It’‘s dark in the cellar’),
    nl, fail.

can_take(State, X) :-
    get_state(State, here, H),
    get_state(State, location, H/X).
can_take(State, X) :-
    write(‘it is not here’),
    nl, fail.

can_turn_on(State, X) :-
    get_state(State, have, X).
can_turn_on(_, X) :-
    write(‘You don’‘t have it’),
    nl, fail.

% commands

do(Old, New, goto(X)) :- goto(Old, New, X), !.
do(Old, New, take(X)) :- take(Old, New, X), !.
do(Old, New, turn_on(X)) :- turn_on(Old, New, X), !.
do(State, State, look) :- look(State), !.
do(Old, New, quit) :- quit(Old, New).
do(State, State, _) :- write(‘illegal command’), nl.

look(State) :-
    get_state(State, here, H),
    write('You are in '), write(H),
    nl,
    list_things(State, H), nl.

list_things(State, H) :-
    get_state(State, location, H/X),
    tab(2), write(X),
    fail.
list_things(_, _).

goto(Old, New, X) :-
    add_state(Old, New, here, X),
    look(New).

take(Old, New, X) :-
    get_state(Old, here, H),
    del_state(Old, Temp, location, H/X),
    add_state(Temp, New, have, X).

turn_on(Old, New, X) :-
    add_state(Old, New, status, X/on).

quit(Old, New) :-
    add_state(Old, New, status, game/off).