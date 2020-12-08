#!/usr/bin/escript

main([]) ->
	main(["sol05.data"]);
main([Filename]) ->
	{ok, Data} = file:read_file(Filename),
	Lines = string:lexemes(Data, "\n\r \t"),
	Nums = [parse_seat(L) || L <- Lines],
	[Hi|_] = lists:reverse(lists:sort(Nums)),
	io:format("~p~n", [Hi]).

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
