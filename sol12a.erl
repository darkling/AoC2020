#!/usr/bin/escript

main([]) ->
	main(["sol12.data"]);
main([Filename]) ->
	{ok, File} = file:read_file(Filename),
	Lines = string:lexemes(File, "\r\n"),
	Codes = [parse(L) || L <- Lines],
	State = {0, 0, east},
	{X, Y, _} = lists:foldl(fun (C, Acc) -> iterate_code(C, Acc) end,
							State, Codes),
	io:format("~B~n", [abs(X)+abs(Y)]).

parse(<<D:1/binary, Dist/binary>>) ->
	{map_direction(D), binary_to_integer(Dist)}.

map_direction(<<"N">>) -> north;
map_direction(<<"S">>) -> south;
map_direction(<<"E">>) -> east;
map_direction(<<"W">>) -> west;
map_direction(<<"L">>) -> left;
map_direction(<<"R">>) -> right;
map_direction(<<"F">>) -> forward.

iterate_code({north, N}, {X, Y, Dir}) ->
	{X, Y+N, Dir};
iterate_code({south, N}, {X, Y, Dir}) ->
	{X, Y-N, Dir};
iterate_code({east, N}, {X, Y, Dir}) ->
	{X+N, Y, Dir};
iterate_code({west, N}, {X, Y, Dir}) ->
	{X-N, Y, Dir};
iterate_code({forward, N}, {X, Y, Dir}) ->
	iterate_code({Dir, N}, {X, Y, Dir});
iterate_code({Rot, 0}, State) when Rot =:= left; Rot =:= right ->
	State;
iterate_code({left, N}, {X, Y, Dir}) ->
	iterate_code({left, N-90}, {X, Y, rot_left(Dir)});
iterate_code({right, N}, {X, Y, Dir}) ->
	iterate_code({right, N-90}, {X, Y, rot_right(Dir)}).

rot_left(north) -> west;
rot_left(west) -> south;
rot_left(south) -> east;
rot_left(east) -> north.

rot_right(north) -> east;
rot_right(east) -> south;
rot_right(south) -> west;
rot_right(west) -> north.
