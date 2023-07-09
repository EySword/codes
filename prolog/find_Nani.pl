% 使用room/1谓词定义房间
room(kitchen).
room(office).
room(hall).
room('dining room').
room(cellar).

% 使用具有两个参数的谓词location/2来定义物体的位置
location(desk, office).
location(apple, kitchen).
location(flashlight, desk).
location('washing machine', cellar).
location(nani, 'washing maching').
location(broccoli, kitchen).
location(crackers, kitchen).
location(computer, office).

% 使用door/2来表示两个房间有门相连
% 如果我们想要表达一种双向的联系，就必须把每种联系都定义一遍
door(office, hall).
door(kitchen, office).
door(hall, 'dining room').
door(kitchen, cellar).
door('dining room', kitchen).

% 下面定义某些物体的属性
edible(apple).
edible(crackers).
tastes_yucky(broccoli).

% 最后，定义手电筒的状态和玩家的初始位置
turned_off(flashlight).
here(kitchen).
:-dynamic here/1.

where_food(X,Y):-location(X,Y),edible(X).

% 使用规则解决单向门的问题
connect(X,Y):-door(X,Y).
connect(X,Y):-door(Y,X).

% 列出房间中的物品
list_things(Place):-location(X,Place), tab(2), write(X), nl, fail.
list_things(_).
%列出与某个房间相连的所有房间
list_connections(Place):-connect(Place,X), tab(2), write(X), nl, fail.
list_connections(_).

% 定义look/0，它能够显示玩家所在的房间，以及此房间中的物品和所有的出口
look:- here(Place), write('You are in the '), write(Place), nl, 
write('You can see: '), nl, list_things(Place), 
write('You can go to: '), nl, list_connections(Place).

% 编写移动的命令
goto(Place):- can_go(Place), move(Place), look.

can_go(Place):- here(X), connect(X, Place).
can_go(_Place):- write('you can\'t get there from here.'), nl, fail.

move(Place):- retract(here(_X)), asserta(here(Place)).

% 编写拿取和丢弃
take(X):- can_take(X), take_object(X).

can_take(Thing):- here(Place), location(Thing, Place).
can_take(Thing):- write("there is no "), write(Thing), write(" here."), nl, fail.

:-dynamic location/2.
take_object(Thing):- retract(location(Thing,_)), asserta(have(Thing)), write('taken'), nl.


% 用结构描述物品的颜色、大小以及重量
% object(candle, red, small, 1).
% object(apple, red, small, 1).
% object(apple, green, small, 1).
% object(table, blue, big, 50).

% 再定义一个谓词location_s/2
location_s(object(apple, red, small, 1), kitchen).
location_s(object(apple, green, small, 1), kitchen).
location_s(object(table, blue, big, 50), kitchen).

can_take_s(Thing) :- here(Room), location_s(object(Thing,_,small,_),Room).
can_take_s(Thing) :- here(Room), location_s(object(Thing,_,big,_),Room),
write('It\'s too big to carry.'), nl, fail.
can_take_s(Thing) :- here(Room), not(location_s(object(Thing,_,_,_),Room)),
write('There is no '), write(Thing), write(' here.'), nl, fail.

list_things_s(Place) :- location_s(object(Thing, Color, Size, Weight),Place),
write('A '),write(Size),tab(1),
write(Color),tab(1),
write(Thing), write(', weighing '),
write(Weight), write(' pounds'), nl,
fail.
list_things_s(_).

loc_list([apple, broccoli, crackers], kitchen).
loc_list([desk, computer], office).
loc_list([flashlight, envelope], desk).
loc_list([stamp, key], envelope).
loc_list(['washing machine'], cellar).
loc_list([nani], 'washing machine').

add_thing(NewThing,Container,NewList):-loc_list(OldList,Container),append([NewThing],OldList,NewList).
add_thing2(NewThing,Container,NewList):-NewList=[NewThing|OldList],loc_list(OldList,Container).
add_thing3(NewThing,Container,[NewThing|OldList]):-loc_list(OldList,Container).

:-dynamic loc_list/2.  
put_thing(Thing,Place):-retract(loc_list(List,Place)),asserta(loc_list([Thing|List],Place)).

puzzle(goto(cellar)) :-
    have(apple),
    write('Success').
    % turned_on(flashlight),
    % ! .