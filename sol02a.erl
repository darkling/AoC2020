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
	[Lo, Hi, <<Char:8>>, Pwd] = string:lexemes(Line, "- :"),
	{binary_to_integer(Lo),
	 binary_to_integer(Hi),
	 Char,
	 binary_to_list(Pwd)}.

is_valid({Lo, Hi, Char, Pwd}) ->
	CharCount = length([C || C <- Pwd, C =:= Char]),
	(Lo =< CharCount) and (CharCount =< Hi).
