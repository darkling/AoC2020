#!/usr/bin/escript

-record(i, {op :: atom(),
			param :: integer(),
			anno :: map()
}).

-record(state, {acc :: integer(),
				pc :: integer()
}).

main([]) ->
	main(["sol08.data"]);
main([Filename]) ->
	{ok, File} = file:open(Filename, [read, binary]),
	Code = compile(File),
	file:close(File),

	State = #state{acc=0, pc=0},
	NewState = exec(Code, State),

	io:format("~p~n", [NewState#state.acc]).

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
	io:format("Exec ~p ~p~n", [Instr, State]),
	case Anno of
		#{halt:=_} ->
			State;
		#{reps:=N} when N < 1 ->
			NewState = exec_instruction(Op, Param, State),
			NewAnno = Anno#{reps => N+1},
			NewInstr = Instr#i{anno=NewAnno},
			NewCode = Code#{PC => NewInstr},
			exec(NewCode, NewState);
		#{reps:=N} when N >= 1 ->
			State
	end.

exec_instruction(nop, _, #state{pc=PC} = State) ->
	State#state{pc=PC+1};
exec_instruction(acc, N, #state{acc=Acc, pc=PC} = State) ->
	State#state{acc=Acc+N, pc=PC+1};
exec_instruction(jmp, N, #state{pc=PC} = State) ->
	State#state{pc=PC+N}.
