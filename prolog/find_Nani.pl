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