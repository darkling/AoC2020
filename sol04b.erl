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

is_valid(#{"byr" := BirthYear, "iyr" := IssueYear, "eyr" := ExpYear,
		   "hgt" := Height, "hcl" := HairCol, "ecl" := EyeCol,
		   "pid" := PID}) ->

	case valid_pid(PID) of
		true ->
			io:format("  ~ts~n", [PID]);
		false ->
			io:format("x ~ts~n", [PID])
	end,

	valid_year(BirthYear, 1920, 2002)
		and valid_year(IssueYear, 2010, 2020)
		and valid_year(ExpYear, 2020, 2030)
		and valid_height(Height)
		and valid_hair_col(HairCol)
		and valid_eye_col(EyeCol)
		and valid_pid(PID);
is_valid(_) ->
	false.

valid_year(Text, Lo, Hi) ->
	try
		Num = list_to_integer(Text),
		(Lo =< Num) and (Num =< Hi)
	catch
		_:_ ->
			false
	end.

valid_height(Text) ->
	try
		{match, [_, NumT, Size]} = re:run(Text, "^([0-9]+)(in|cm)$",
										  [{capture, all, list}]),
		Num = list_to_integer(NumT),
		case Size of
			"in" ->
				(59 =< Num) and (Num =< 76);
			"cm" ->
				(150 =< Num) and (Num =< 193)
		end
	catch
		_:_ ->
			false
	end.

valid_hair_col(Text) ->
	case re:run(Text, "^#[0-9a-f]{6}$") of
		{match, _} ->
			true;
		_ ->
			false
	end.

valid_eye_col(Text) 
  when Text =:= "amb";
	   Text =:= "blu";
	   Text =:= "brn";
	   Text =:= "gry";
	   Text =:= "grn";
	   Text =:= "hzl";
	   Text =:= "oth" ->
	true;
valid_eye_col(_) ->
	false.

valid_pid(Text) ->
	case re:run(Text, "^[0-9]{9}$") of
		{match, _} ->
			true;
		_ ->
			false
	end.
