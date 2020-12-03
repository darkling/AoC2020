#!/usr/bin/escript

main([]) ->
	main(["sol01.data"]);
main([Filename]) ->
	{ok, Data} = file:read_file(Filename),
	TextItems = string:lexemes(Data, "\r\n\t "),
	Nums = [binary_to_integer(T) || T <- TextItems],
	Low = [N || N <- Nums, N =< 2020/3],
	High = [N || N <- Nums, N >= 2020/3],
	Set = sets:from_list(Nums),
	Triples = [{X, Y, 2020-X-Y} || X <- Low, Y <- High,
								   sets:is_element(2020-X-Y, Set)],
	[{A, B, C}|_] = Triples,
	io:format("~B~n", [A*B*C]).
