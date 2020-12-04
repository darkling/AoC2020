#!/usr/bin/escript

main([]) ->
	main(["sol04.data"]);
main([Filename]) ->
	{ok, File} = file:open(Filename, read),
	Lines = [string:chomp(L) || L <- read_lines(File)],
	Records = lists:foldl(fun (L, Acc) -> parse_data(L, Acc) end, [#{}], Lines),
	Valid = [R || R <- Records, is_valid(R)],
	io:format("~B~n", [length(Valid)]).


read_lines(File) ->
	lists:reverse(read_next_line(File, [])).

read_next_line(File, Acc) ->
	case file:read_line(File) of
		{ok, Line} ->
			read_next_line(File, [Line|Acc]);
		eof ->
			Acc
	end.

parse_data([], Acc) ->
	[#{} | Acc];
parse_data(Line, [Cur | Rest]) ->
	[maps:merge(parse_line(Line), Cur) | Rest].

parse_line(Line) ->
	Elements = string:lexemes(Line, " "),
	KVs = [string:split(Elt, ":") || Elt <- Elements],
	maps:from_list([{K, V} || [K, V] <- KVs]).

is_valid(#{"byr" := _, "iyr" := _, "eyr" := _, "hgt" := _,
		   "hcl" := _, "ecl" := _, "pid" := _}) ->
	true;
is_valid(_) ->
	false.
