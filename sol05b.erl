#!/usr/bin/escript

main([]) ->
	main(["sol05.data"]);
main([Filename]) ->
	{ok, Data} = file:read_file(Filename),
	Lines = string:lexemes(Data, "\n\r \t"),
	Nums = [parse_seat(L) || L <- Lines],
	{{Lo, _, _, _}, {Hi, _, _, _}} = find_gap(lists:sort(Nums)),
	io:format("~B ~B ~B~n", [Lo, Lo+1, Hi]).

parse_seat(<<Yt:7/binary, Xt:3/binary>> = Str) ->
	Row = parse_binary(Yt, <<"F">>, <<"B">>, 64),
	Col = parse_binary(Xt, <<"L">>, <<"R">>, 4),
	{Row*8+Col, Str, Row, Col}.

parse_binary(<<>>, _, _, _) ->
	0;
parse_binary(<<Zero:1/binary, Rest/binary>>, Zero, One, N) ->
	parse_binary(Rest, Zero, One, N bsr 1);
parse_binary(<<One:1/binary, Rest/binary>>, Zero, One, N) ->
	N + parse_binary(Rest, Zero, One, N bsr 1).

find_gap([{N0, _, _, _}, {N1, _, _, _} = B | Rest])
  when N0+1 =:= N1 ->
	find_gap([B | Rest]);
find_gap([A, B | _Rest]) ->
	{A, B}.
