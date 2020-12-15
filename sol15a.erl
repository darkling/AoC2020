#!/usr/bin/escript

main([]) ->
	main(["sol15.data"]);
main([Filename]) ->
	{ok, File} = file:read_file(Filename),
	NumsT = string:lexemes(File, ",\r\n"),
	Nums = [binary_to_integer(N) || N <- NumsT],
	{State, Turn, Next} = preload(Nums, 1, #{}),
	EndState = iterate(State, Turn, Next),
	io:format("~p~n", [EndState]).	

preload([N], Turn, State) ->
	{State, Turn, N};
preload([N|Rest], Turn, State) ->
	preload(Rest, Turn+1, State#{N => Turn}).

iterate(_State, 2020, Next) ->
	Next;
iterate(State, Turn, Next) ->
	case State of
		#{Next := LastSeen} ->
			iterate(State#{Next => Turn}, Turn+1, Turn-LastSeen);
		_ ->
			iterate(State#{Next => Turn}, Turn+1, 0)
	end.
