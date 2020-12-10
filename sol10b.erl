#!/usr/bin/escript

main([]) ->
	main(["sol10.data"]);
main([Filename]) ->
	{ok, Data} = file:read_file(Filename),
	TextItems = string:lexemes(Data, "\r\n\t "),
	Nums = [binary_to_integer(T) || T <- TextItems],
	Adapter = lists:max(Nums)+3,
	Order = lists:sort([Adapter|Nums]),
	Routes = lists:foldl(
			   fun (N, Memo) -> find_routes(N, Memo) end,
			   #{0 => 1},
			   Order),
	io:format("~B~n", [maps:get(Adapter, Routes)]).

find_routes(N, Acc) ->
	A = maps:get(N-1, Acc, 0),
	B = maps:get(N-2, Acc, 0),
	C = maps:get(N-3, Acc, 0),
	Acc#{N => A+B+C}.
