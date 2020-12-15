#!/usr/bin/escript

main([]) ->
	main(["sol14.data"]);
main([Filename]) ->
	{ok, File} = file:read_file(Filename),
	Lines = string:lexemes(File, "\r\n"),
	Instr = [parse_line(L) || L <- Lines],
	Values = lists:foldl(
			   fun (X, Acc) -> process_instruction(X, Acc) end,
			   {16#0, 16#0, [], #{}},
			   Instr),
	{_, _, _, Mem} = Values,
	Total = lists:foldl(fun erlang:'+'/2, 0, maps:values(Mem)),
	io:format("~B~n", [Total]).


parse_line(<<"mask = ", Text/binary>>) ->
	{mask,
	 build_binary(Text, fun(C) -> map_bitset(C) end),
	 build_binary(Text, fun(C) -> map_mask(C) end),
	 build_floating(Text)
	 };
parse_line(<<"mem", _/binary>> = Mem) ->
	[AddrT, ValueT] = string:lexemes(Mem, "me[] ="),
	Addr = binary_to_integer(AddrT),
	Value = binary_to_integer(ValueT),
	{mem, Addr, Value}.

build_binary(Text, Mapper) ->
	Bits = << <<(Mapper(Bit)):1>> || <<Bit>> <= Text>>,
	<<N:36/big-unsigned>> = Bits,
	N.

map_mask($X) -> 0;
map_mask(_) -> 1.

map_bitset($1) -> 1;
map_bitset(_) -> 0.

build_floating(Text) ->
	Bits = [map_float(BitC) || <<BitC>> <= Text],
	[N || {N, X} <- lists:zip(lists:seq(35, 0, -1), Bits), X =:= 1].

map_float($X) -> 1;
map_float(_) -> 0.
	

process_instruction({mask, Set, Mask, Floats}, {_, _, _, Mem}) ->
	{Set, Mask, Floats, Mem};
process_instruction({mem, Addr, Value}, {Set, Mask, Floats, Mem}) ->
	MaskedAddr = (Addr bor Set) band Mask,
	FloatBits = build_float_addrs(Floats),
	AllAddrs = [MaskedAddr bor F || F <- FloatBits],
	NewMem = lists:foldl(
			   fun (A, M) -> M#{A => Value} end,
			   Mem,
			   AllAddrs),
	{Set, Mask, Floats, NewMem}.

build_float_addrs([]) ->
	[0];
build_float_addrs([BitPos|Floats]) ->
	Data = build_float_addrs(Floats),
	Bit = 1 bsl BitPos,
	BitsSet = [N bor Bit || N <- Data],
	BitsSet ++ Data.
