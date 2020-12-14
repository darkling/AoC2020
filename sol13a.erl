#!/usr/bin/escript

main([]) ->
	main(["sol13.data"]);
main([Filename]) ->
	{ok, File} = file:read_file(Filename),
	[Estimate|Entries] = [parse_entry(E) || E <- string:lexemes(File, ",\r\n"),
											E =/= <<"x">>],
	Offsets = [next_after(Estimate, Bus) || Bus <- Entries],
	[{Delay, Bus}|_] = lists:sort(Offsets),
	io:format("Bus ~B, delay ~B, solution ~B~n", [Bus, Delay, Bus*Delay]).

parse_entry(<<"x">>) ->
	x;
parse_entry(N) ->
	binary_to_integer(N).

next_after(Est, Bus) ->
	MissedBy = Est rem Bus,
	Delay = Bus - MissedBy,
	{Delay, Bus}.
