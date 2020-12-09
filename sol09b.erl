#!/usr/bin/escript

main([]) ->
	main([25, "sol09.data"]);
main([WindowT, Filename])
  when is_list(WindowT) ->
	Window = list_to_integer(WindowT),
	main([Window, Filename]);
main([Window, Filename]) ->
	{ok, Data} = file:read_file(Filename),
	TextItems = string:lexemes(Data, "\r\n\t "),
	Nums = [binary_to_integer(T) || T <- TextItems],
	Inv = find_invalid(Nums, Window),
	Range = find_sum_range(Nums, Inv),
	io:format("~p~n", [Range]),
	io:format("~B~n", [lists:min(Range)+lists:max(Range)]).


find_sum([], _, _Tgt) ->
	err;
find_sum(_, [], _Tgt) ->
	err;
find_sum([X|_], [Y|_], Tgt)
  when X + Y =:= Tgt ->
	ok;
find_sum([X|XR], [Y|_]=YL, Tgt)
  when X + Y < Tgt ->
	find_sum(XR, YL, Tgt);
find_sum([X|_]=XL, [Y|YR], Tgt)
  when X + Y > Tgt ->
	find_sum(XL, YR, Tgt).

find_invalid(L, Window)
  when length(L) =< Window ->
	-1;
find_invalid([_|Rest] = L, Window) ->
	Preamble = lists:sublist(L, Window),
	[Target|_] = lists:nthtail(Window, L),
	{A, B} = split_list(Preamble, Target div 2),
	case find_sum(A, B, Target) of
		err ->
			Target;
		ok ->
			find_invalid(Rest, Window)
	end.

split_list(List, Mid) ->
	{A, B} = lists:partition(fun (X) -> X =< Mid end, List),
	{lists:sort(A),
	 lists:reverse(lists:sort(B))}.


find_sum_range(Nums, Target) ->
	iterate_sum(Nums, Target, queue:new(), 0).

iterate_sum(_Nums, Target, Queue, Target) ->
	queue:to_list(Queue);
iterate_sum(Nums, Target, Queue, Total)
  when Total > Target ->
	{{value, N}, NewQueue} = queue:out(Queue),
	iterate_sum(Nums, Target, NewQueue, Total-N);
iterate_sum([N|Nums], Target, Queue, Total)
  when Total < Target ->
	NewQueue = queue:in(N, Queue),
	iterate_sum(Nums, Target, NewQueue, Total+N).
