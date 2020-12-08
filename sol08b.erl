#!/usr/bin/escript

-record(i, {op :: atom(),
			param :: integer(),
			anno :: map()
}).

-record(state, {acc :: integer(),
				pc :: integer(),
				return :: halt | break
}).

main([]) ->
	main(["sol08.data"]);
main([Filename]) ->
	{ok, File} = file:open(Filename, [read, binary]),
	Code = compile(File),
	file:close(File),

	ChangePoints =
		lists:filter(
		  fun ({_, #i{op=Op}}) -> (Op =:= nop) or (Op =:= jmp) end,
		  maps:to_list(Code)),

	Results = [try_change_exec(Code, CP) || CP <- ChangePoints],
	Terminations =
		lists:filter(
		  fun (#state{return=R}) -> R =:= halt end,
		 Results),

	[#state{acc=Acc}] = Terminations,

	io:format("~B~n", [Acc]).

compile(File) ->
	compile(File, 0, #{}).

compile(File, LineNo, Ins) ->
	case file:read_line(File) of
		eof ->
			Ins;
		{ok, Line} ->
			NewIns = Ins#{LineNo => compile_line(string:chomp(Line))},
			compile(File, LineNo+1, NewIns)
	end.

compile_line(<<Ins:3/binary, " ", Sgn:1/binary, Value/binary>>) ->
	#i{op=map_instruction(Ins), param=map_param(Sgn, Value), anno=#{reps=>0}}.

map_instruction(Ins) ->
	binary_to_atom(Ins).

map_param(<<"-">>, Value) ->
	-binary_to_integer(Value);
map_param(<<"+">>, Value) ->
	binary_to_integer(Value).

exec(Code, #state{pc=PC} = State) ->
	Instr = maps:get(PC, Code, #i{anno=#{halt => 1}}),
	#i{op=Op, param=Param, anno=Anno} = Instr,
	case Anno of
		#{halt:=_} ->
			State#state{return=halt};
		#{reps:=N} when N < 1 ->
			NewState = exec_instruction(Op, Param, State),
			NewAnno = Anno#{reps => N+1},
			NewInstr = Instr#i{anno=NewAnno},
			NewCode = Code#{PC => NewInstr},
			exec(NewCode, NewState);
		#{reps:=N} when N >= 1 ->
			State#state{return=break}
	end.

exec_instruction(nop, _, #state{pc=PC} = State) ->
	State#state{pc=PC+1};
exec_instruction(acc, N, #state{acc=Acc, pc=PC} = State) ->
	State#state{acc=Acc+N, pc=PC+1};
exec_instruction(jmp, N, #state{pc=PC} = State) ->
	State#state{pc=PC+N}.

try_change_exec(Code, {L, Ins}) ->
	NewIns = case Ins of
				 #i{op=jmp} -> Ins#i{op=nop};
				 #i{op=nop} -> Ins#i{op=jmp}
			 end,
	io:format("Changing ~p to ~p~n", [Ins, NewIns]),

	State = #state{acc=0, pc=0},
	exec(Code#{L => NewIns}, State).
