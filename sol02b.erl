#!/usr/bin/escript

main([]) ->
	main(["sol02.data"]);
main([Filename]) ->
	{ok, Data} = file:read_file(Filename),
	TextItems = string:lexemes(Data, "\r\n"),
	Records = [parse_record(I) || I <- TextItems],
	Count = length([R || R <- Records, is_valid(R)]),
	io:format("~B~n", [Count]).

parse_record(Line) ->
	[P1, P2, <<Char:8>>, Pwd] = string:lexemes(Line, "- :"),
	{binary_to_integer(P1),
	 binary_to_integer(P2),
	 Char,
	 binary_to_list(Pwd)}.

is_valid({P1, P2, Char, Pwd}) ->
	C1 = lists:nth(P1, Pwd),
	C2 = lists:nth(P2, Pwd),
	(C1 =:= Char) xor (C2 =:= Char).
