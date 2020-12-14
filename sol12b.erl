#!/usr/bin/escript

main([]) ->
	main(["sol12.data"]);
main([Filename]) ->
	{ok, File} = file:read_file(Filename),
	Lines = string:lexemes(File, "\r\n"),
	Codes = [parse(L) || L <- Lines],
	State = {0, 0, 10, 1},
	{Sx, Sy, _Wx, _Wy} = lists:foldl(fun (C, Acc) -> iterate_code(C, Acc) end,
									 State, Codes),
	io:format("~B~n", [abs(Sx)+abs(Sy)]).

parse(<<D:1/binary, Dist/binary>>) ->
	{map_direction(D), binary_to_integer(Dist)}.

map_direction(<<"N">>) -> north;
map_direction(<<"S">>) -> south;
map_direction(<<"E">>) -> east;
map_direction(<<"W">>) -> west;
map_direction(<<"L">>) -> left;
map_direction(<<"R">>) -> right;
map_direction(<<"F">>) -> forward.

iterate_code({north, N}, {Sx, Sy, Wx, Wy}) ->
	{Sx, Sy, Wx, Wy+N};
iterate_code({south, N}, {Sx, Sy, Wx, Wy}) ->
	{Sx, Sy, Wx, Wy-N};
iterate_code({east, N}, {Sx, Sy, Wx, Wy}) ->
	{Sx, Sy, Wx+N, Wy};
iterate_code({west, N}, {Sx, Sy, Wx, Wy}) ->
	{Sx, Sy, Wx-N, Wy};
iterate_code({forward, N}, {Sx, Sy, Wx, Wy}) ->
	{Sx + Wx*N, Sy + Wy*N, Wx, Wy};
iterate_code({Rot, 0}, State) when Rot =:= left; Rot =:= right ->
	State;
iterate_code({left, N}, {Sx, Sy, Wx, Wy}) ->
	iterate_code({left, N-90}, {Sx, Sy, -Wy, Wx});
iterate_code({right, N}, {Sx, Sy, Wx, Wy}) ->
	iterate_code({right, N-90}, {Sx, Sy, Wy, -Wx}).
