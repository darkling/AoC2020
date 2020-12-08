#!/usr/bin/escript

main([]) ->
	main(["sol06.data"]);
main([Filename]) ->
	{ok, File} = file:open(Filename, [read]),
	Data = parse_file(File),
	file:close(File),
	Counts = [count_universal_answers(G) || G <- Data],
	Total = lists:foldl(fun erlang:'+'/2, 0, Counts),
	io:format("~B~n", [Total]).


parse_file(File) ->
	parse_line(File, read_line(File), [[]]).

parse_line(_File, eof, Acc) ->
	Acc;
parse_line(File, "", Acc) ->
	parse_line(File, read_line(File), [[]|Acc]);
parse_line(File, Line, [First | Rest]) ->
	parse_line(File, read_line(File), [[Line | First] | Rest]).

read_line(File) ->
	case file:read_line(File) of
		{ok, Line} ->
			string:chomp(Line);
		Other ->
			Other
	end.

count_universal_answers(Group) ->
	People = length(Group),
	Answers = lists:sort(lists:append(Group)),
	AnswersByQuestion = split_answers(Answers),
	length([A || A <- AnswersByQuestion,
				 length(A) =:= People]).

split_answers([]) ->
	[];
split_answers([X | _] = A) ->
	{Group, Rest} = lists:splitwith(fun (E) -> E =:= X end, A),  
	[Group | split_answers(Rest)].
