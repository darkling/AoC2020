#!/usr/bin/escript

main([]) ->
	main(["sol03.data"]);
main([Filename]) ->
	{ok, Data} = file:read_file(Filename),
	Lines = string:lexemes(Data, "\r\n"),
	Width = size(hd(Lines)),
	Positions = [(X*3) rem Width || X <- lists:seq(0, length(Lines)-1)],
	Found = [binary:part(L, P, 1) || {L, P} <- lists:zip(Lines, Positions)],
	Total = length([F || F <- Found, F =:= <<"#">>]),
	io:format("~B~n", [Total]).
