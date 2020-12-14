#!/usr/bin/escript

main([]) ->
	main(["sol13.data"]);
main([Filename]) ->
	{ok, File} = file:read_file(Filename),
	[_|Toks] = string:lexemes(File, ",\r\n"),
	%% We are looking for x fulfilling the equations:
	%%  X + I = 0 mod Ni
	%% which more usefully translates to
	%%  X = Ni-I mod Ni
	%% so we store Ni-I and Ni
	Raw = [{N, binary_to_integer(E)} 
		   || {N, E} <- lists:zip(
						  lists:seq(0, length(Toks)-1),
						  Toks),
			  E =/= <<"x">>],
	Entries = [{(E-N) rem E, E} || {N, E} <- Raw],
	{Result, _} = fold_items(Entries),
	io:format("~B~n", [Result]).

fold_items([Item]) ->
	Item;
fold_items([{A1, N1}, {A2, N2}|Rest]) when N2 > N2 ->
	fold_items([{A2, N2}, {A1, N1}|Rest]);
fold_items([{A1, N1}, {A2, N2}|Rest]) ->
	io:format("folding ~p ~p~n", [{A1, N1}, {A2, N2}]),
	%% Using the Chinese Remainder Theorem:
	%%  X = A1*M2*N2 + A2*M1*N1
	%% where M1, M2 are computed by the Euclidean algorithm.
	{M1, M2} = euclid(N1, N2),
	io:format("euclid(~B, ~B) = ~B, ~B~n", [N1, N2, M1, M2]),
	A12 = case (A1*M2*N2 + A2*M1*N1) rem (N1*N2) of
			  X when X < 0 -> X + N1*N2;
			  X -> X
		  end,
	NewItem = {A12, N1*N2},
	io:format("new item ~p~n", [NewItem]),
	fold_items([NewItem|Rest]).

euclid(N1, N2) ->
	eeuclid(N1, 1, 0, N2, 0, 1).

eeuclid(R0, S0, T0, R1, S1, T1)
  when R0 rem R1 =:= 0 ->
	io:format("  ~B ~B ~B ~B ~B ~B done~n", [R0, S0, T0, R1, S1, T1]),
	{S1, T1};
eeuclid(R0, S0, T0, R1, S1, T1) ->
	io:format("  ~B ~B ~B ~B ~B ~B iter~n", [R0, S0, T0, R1, S1, T1]),
	Q1 = R0 div R1,
	R2 = R0 rem R1,
	S2 = S0 - Q1*S1,
	T2 = T0 - Q1*T1,
	eeuclid(R1, S1, T1, R2, S2, T2).
