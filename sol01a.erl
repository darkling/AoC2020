#!/usr/bin/escript

main([]) ->
	main(["sol01.data"]);
main([Filename]) ->
	{ok, Data} = file:read_file(Filename),
	TextItems = string:lexemes(Data, "\r\n\t "),
	Nums = [binary_to_integer(T) || T <- TextItems],
	Low = lists:sort([N || N <- Nums, N =< 1010]),
	High = lists:reverse(lists:sort([N || N <- Nums, N > 1010])),
	Product = find_sum(Low, High),
	io:format("~B~n", [Product]).

find_sum([X|_], [Y|_])
  when X + Y =:= 2020 ->
	X * Y;
find_sum([X|XR], [Y|_]=YL)
  when X + Y < 2020 ->
	find_sum(XR, YL);
find_sum([X|_]=XL, [Y|YR])
  when X + Y > 2020 ->
	find_sum(XL, YR).
