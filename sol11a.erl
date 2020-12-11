#!/usr/bin/escript

main([]) ->
	main(["sol11.data"]);
main([Filename]) ->
	{ok, File} = file:read_file(Filename),
	Lines = string:lexemes(File, "\r\n"),
	State = lists:foldl(fun (X, Acc) -> make_state(X, Acc) end,
						#{},
						lists:zip(lists:seq(1, length(Lines)), Lines)),
	Result = iterate_to_stability(State),
	Total = [X || X <- maps:values(Result),
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
	Acc#{{X, Y} => empty}.

iterate_to_stability(State) ->
	io:format("Iterating~n"),
	NewState = maps:fold(fun (K, V, Acc) -> next_iter(K, V, State, Acc) end,
						 #{},
						 State),
	case NewState of
		State -> NewState;
		_ -> iterate_to_stability(NewState)
	end.

next_iter(Pos, empty, State, Acc) ->
	case count_occupied(surroundings(Pos, State)) of
		0 -> Acc#{Pos => occupied};
		_ -> Acc#{Pos => empty}
	end;
next_iter(Pos, occupied, State, Acc) ->
	case count_occupied(surroundings(Pos, State)) of
		N when N >= 4 -> Acc#{Pos => empty};			
		_ -> Acc#{Pos => occupied}
	end.

count_occupied(S) ->
	length([X || X <- S, X =:= occupied]).

surroundings({X, Y}, State) ->
	[maps:get({X+1, Y+1}, State, floor),
	 maps:get({X  , Y+1}, State, floor),
	 maps:get({X-1, Y+1}, State, floor),
	 maps:get({X+1, Y  }, State, floor),
	 maps:get({X-1, Y  }, State, floor),
	 maps:get({X+1, Y-1}, State, floor),
	 maps:get({X  , Y-1}, State, floor),
	 maps:get({X-1, Y-1}, State, floor)
	].

dump_state(State) ->
	MaxX = lists:max([X || {X, _} <- maps:keys(State)]),
	MaxY = lists:max([Y || {_, Y} <- maps:keys(State)]),
	lists:foreach(
	  fun(Y) ->
			  lists:foreach(
				fun (X) ->
						Ch = case maps:get({X, Y}, State, floor) of
								 empty -> $L;
								 occupied -> $#;
								 floor -> $.
							 end,
						io:format("~ts", [[Ch]])
				end,
				lists:seq(1, MaxX)),
			  io:format("~n")
	  end,
	  lists:seq(1, MaxY)).
