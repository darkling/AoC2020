#!/usr/bin/escript

main([]) ->
	main(["sol11.data"]);
main([Filename]) ->
	{ok, File} = file:read_file(Filename),
	Lines = string:lexemes(File, "\r\n"),
	State0 = lists:foldl(fun (X, Acc) -> make_state(X, Acc) end,
						 #{},
						 lists:zip(lists:seq(1, length(Lines)), Lines)),
	State = cache_neighbours(State0),
	%io:format("~p~n", [State]),
	%dump_state(State),
	Result = iterate_to_stability(State),
	%io:format("~n"),
	%dump_state(Result),
	Total = [X || {X, _} <- maps:values(Result),
				  X =:= occupied],
	io:format("~B~n", [length(Total)]).

make_state({Y, Line}, Acc0) ->
	Seats = binary_to_list(Line),
	lists:foldl(fun (S, Acc) ->	make_state_seats(S, Y, Acc) end,
				Acc0,
				lists:zip(lists:seq(1, length(Seats)), Seats)).

make_state_seats({_X, $.}, _Y, Acc) ->
	Acc;
make_state_seats({X, $L}, Y, Acc) ->
	Acc#{{X, Y} => {empty, []}}.

iterate_to_stability(State) ->
	io:format("Iterating~n"),
	%io:format("~p~n", [State]),
	%dump_state(State),
	NewState = maps:fold(fun (K, V, Acc) -> next_iter(K, V, State, Acc) end,
						 #{},
						 State),
	case NewState of
		State -> NewState;
		_ -> iterate_to_stability(NewState)
	end.

next_iter(Pos, {empty, Nbrs}, State, Acc) ->
	case count_occupied(surroundings(Nbrs, State)) of
		0 -> Acc#{Pos => {occupied, Nbrs}};
		_ -> Acc#{Pos => {empty, Nbrs}}
	end;
next_iter(Pos, {occupied, Nbrs}, State, Acc) ->
	case count_occupied(surroundings(Nbrs, State)) of
		N when N >= 5 -> Acc#{Pos => {empty, Nbrs}};
		_ -> Acc#{Pos => {occupied, Nbrs}}
	end.

state_at(Pos, State) ->
	{S, _} = maps:get(Pos, State, {floor, []}),
	S.

count_occupied(S) ->
	length([X || X <- S, X =:= occupied]).

surroundings(Nbrs, State) ->
	[state_at(N, State) || N <- Nbrs].

cache_neighbours(State) ->
	MaxX = lists:max([X || {X, _} <- maps:keys(State)]),
	MaxY = lists:max([Y || {_, Y} <- maps:keys(State)]),
	maps:fold(fun (Pos, Type, Acc) ->
					  cache_neighbours_at(Pos, Type, State, {MaxX, MaxY}, Acc)
			  end,
			  #{},
			  State).

cache_neighbours_at(Pos, {Type, _}, State, Max, Acc) ->
	Nbrs = [find_nearest_neighbour(Pos, {+1, +1}, Max, State),
			find_nearest_neighbour(Pos, { 0, +1}, Max, State),
			find_nearest_neighbour(Pos, {-1, +1}, Max, State),
			find_nearest_neighbour(Pos, {+1,  0}, Max, State),
			find_nearest_neighbour(Pos, {-1,  0}, Max, State),
			find_nearest_neighbour(Pos, {+1, -1}, Max, State),
			find_nearest_neighbour(Pos, { 0, -1}, Max, State),
			find_nearest_neighbour(Pos, {-1, -1}, Max, State)],
	Acc#{Pos => {Type, [N || N <- Nbrs, N =/= none]}}.

find_nearest_neighbour({X, Y}, {Dx, Dy}, {MaxX, MaxY}, _State)
  when X + Dx > MaxX;	
	   X + Dx < 1;
	   Y + Dy > MaxY;
	   Y + Dy < 1 ->
	none;
find_nearest_neighbour({X, Y}, {Dx, Dy}, Max, State) ->
	Pos = {X+Dx, Y+Dy},
	case maps:get(Pos, State, {floor, []}) of
		{floor, _} ->
			find_nearest_neighbour(Pos, {Dx, Dy}, Max, State);
		_ ->
			Pos
	end.

dump_state(State) ->
	MaxX = lists:max([X || {X, _} <- maps:keys(State)]),
	MaxY = lists:max([Y || {_, Y} <- maps:keys(State)]),
	lists:foreach(
	  fun(Y) ->
			  lists:foreach(
				fun (X) ->
						Ch = case maps:get({X, Y}, State, {floor, []}) of
								 {empty, _} -> $L;
								 {occupied, _} -> $#;
								 {floor, _} -> $.
							 end,
						io:format("~ts", [[Ch]])
				end,
				lists:seq(1, MaxX)),
			  io:format("~n")
	  end,
	  lists:seq(1, MaxY)).
