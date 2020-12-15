#!/usr/bin/escript

main([]) ->
	main(["sol14.data"]);
main([Filename]) ->
	{ok, File} = file:read_file(Filename),
	Lines = string:lexemes(File, "\r\n"),
	Instr = [parse_line(L) || L <- Lines],
	Values = lists:foldl(
			   fun (X, Acc) -> process_instruction(X, Acc) end,
			   {16#fffffffff, 16#000000000, #{}},
			   Instr),
	{_, _, Mem} = Values,
	Total = lists:foldl(fun erlang:'+'/2, 0, maps:values(Mem)),
	io:format("~B~n", [Total]).


parse_line(<<"mask = ", Text/binary>>) ->
	{mask,
	 build_binary(Text, fun (X) -> map_mask(X) end),
	 build_binary(Text, fun (X) -> map_bitset(X) end)
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

map_mask($X) -> 1;
map_mask(_) -> 0.

map_bitset($1) -> 1;
map_bitset(_) -> 0.


process_instruction({mask, Mask, Bits}, {_, _, Mem}) ->
	{Mask, Bits, Mem};
process_instruction({mem, Addr, Value}, {Mask, Bits, Mem}) ->
	NewMem = Mem#{Addr => (Value band Mask) bor Bits},
	{Mask, Bits, NewMem}.
