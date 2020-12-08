#!/usr/bin/escript

main([]) ->
	main(["sol07.data"]);
main([Filename]) ->
	{ok, File} = file:read_file(Filename),
	Lines = string:lexemes(File, "\r\n"),
	Rules = lists:append([split_rules(L) || L <- Lines]),
	SG = {<<"shiny">>, <<"gold">>},

	Total = find_contents_for(Rules, SG),

	io:format("~B~n", [Total]).

split_rules(Line0) ->
	Line = lists:foldl(
			 fun (Match, Str) -> string:replace(Str, Match, <<>>, all) end,
			 string:chomp(Line0),
			 [<<"contain">>, <<"bags">>, <<"bag">>, <<",">>, <<".">>]),
	Parts = string:lexemes(Line, " "),
	[O1, O2 | AllInner] = Parts,
	Outer = {O1, O2},
	Inners = split_inners(AllInner),
	[{Outer, N, Inner} || {N, Inner} <- Inners].

split_inners([]) ->
	[];
split_inners([<<"no">>, <<"other">>]) ->
	[];
split_inners([IN, I1, I2 | Rest]) ->
	[{binary_to_integer(IN), {I1, I2}} | split_inners(Rest)].


find_contents_for(Rules, Colour) ->
	Contents = [N*(1+find_contents_for(Rules, Inner))
				|| {Outer, N, Inner} <- Rules,
				   Outer =:= Colour],
	lists:foldl(fun erlang:'+'/2, 0, Contents).
