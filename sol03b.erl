#!/usr/bin/escript

main([]) ->
	main(["sol03.data"]);
main([Filename]) ->
	{ok, Data} = file:read_file(Filename),
	Lines = string:lexemes(Data, "\r\n"),

	A = count_trees(Lines, 1, 1),
	B = count_trees(Lines, 3, 1),
	C = count_trees(Lines, 5, 1),
	D = count_trees(Lines, 7, 1),
	E = count_trees(Lines, 1, 2),

	io:format("~B~n", [A*B*C*D*E]).

count_trees(Lines0, XSkip, YSkip) ->
	Lines = [L || {L, X} <- lists:zip(Lines0, lists:seq(0, length(Lines0)-1)),
				  (X rem YSkip) =:= 0],
	Width = size(hd(Lines)),
	Positions = [(X*XSkip) rem Width || X <- lists:seq(0, length(Lines)-1)],
	Found = [binary:part(L, P, 1) || {L, P} <- lists:zip(Lines, Positions)],
	length([F || F <- Found, F =:= <<"#">>]).
