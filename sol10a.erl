#!/usr/bin/escript

main([]) ->
	main(["sol10.data"]);
main([Filename]) ->
	{ok, Data} = file:read_file(Filename),
	TextItems = string:lexemes(Data, "\r\n\t "),
	Nums = [binary_to_integer(T) || T <- TextItems],
	Adapter = lists:max(Nums)+3,
	Order = lists:sort([0, Adapter|Nums]),
	Dist = diffs(Order, #{1 => 0, 2 => 0, 3 => 0}),
	#{1 := One, 3 := Three} = Dist,
	io:format("~B ~p~n", [One*Three, Dist]).

diffs([_], Acc) ->
	Acc;
diffs([A, B|Rest], Acc) ->
	Tot = maps:get(B-A, Acc),
	diffs([B|Rest], Acc#{(B-A) => Tot+1}).
