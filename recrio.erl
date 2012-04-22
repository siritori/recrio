-module(recrio).
-compile(export_all).

rctrl(Context, Rs, Rt, Rd, Fun) ->
   Storage = context:get_storage(Context),
   RsReg = storage:read_reg(Storage, Rs),
   RtReg = storage:read_reg(Storage, Rt),
   storage:write_reg(Storage, Rd, Fun(RsReg, RtReg)).

ictrl(Context, Rs, Rt, Immediate, Fun) ->
   Storage = context:get_storage(Context),
   RsReg = storage:read_reg(Storage, Rs),
   storage:write_reg(Storage, Rt, Fun(RsReg, Immediate)).

% add
exec(<<16#00:6, Rs:5, Rt:5, Rd:5, _Shamt:5, 16#20:6>>, Context) ->
   io:format("add ~p ~p ~p~n", [Rs, Rt, Rd]),
   rctrl(Context, Rs, Rt, Rd, fun bit:bit_add/2),
   context:pc_add4(Context);

% addi
exec(<<16#08:6, Rs:5, Rt:5, SignExtImm:16>>, Context) ->
   io:format("addi reg(~p) := reg(~p) + ~p~n", [Rt, Rs, <<SignExtImm:16>>]),
   ictrl(Context, Rs, Rt, <<0:16, SignExtImm:16>>, fun bit:bit_add/2),
   context:pc_add4(Context);

% addiu
exec(<<16#09:6, Rs:5, Rt:5, SignExtImm:16>>, Context) ->
   io:format("addiu reg(~p) := reg(~p) + ~p~n", [Rt, Rs, SignExtImm]),
   ictrl(Context, Rs, Rt, <<0:16, SignExtImm:16>>, fun bit:bit_addu/2),
   context:pc_add4(Context);

% addu
exec(<<16#00:6, Rs:5, Rt:5, Rd:5, _Shamt:6, 16#21:6>>, Context) ->
   io:format("addu ~p ~p ~p~n", [Rs, Rt, Rd]),
   rctrl(Context, Rs, Rt, Rd, fun bit:bit_addu/2),
   context:pc_add4(Context);

% and
exec(<<16#00:6, Rs:5, Rt:5, Rd:5, _Shamt:5, 16#24:6>>, Context) ->
   io:format("and reg(~p) := reg(~p) & reg(~p)~n", [Rd, Rs, Rt]),
   rctrl(Context, Rs, Rt, Rd, fun bit:bit_and/2),
   context:pc_add4(Context);

% andi
exec(<<16#0C:6, Rs:5, Rt:5, SignExtImm:16>>, Context) ->
   io:format("andi reg(~p) := reg(~p) & ~p~n", [Rt, Rs, SignExtImm]),
   ictrl(Context, Rs, Rt, bit:zero_extension(SignExtImm), fun bit:bit_and/2),
   context:pc_add4(Context);

% or
exec(<<16#00:6, Rs:5, Rt:5, Rd:5, _Shamt:5, 16#25:6>>, Context) ->
   io:format("or reg(~p) := reg(~p) | reg(~p)~n", [Rd, Rs, Rt]),
   rctrl(Context, Rs, Rt, Rd, fun bit:bit_or/2),
   context:pc_add4(Context);

% ori
exec(<<16#0D:6, Rs:5, Rt:5, SignExtImm:16>>, Context) ->
   io:format("ori reg(~p) := reg(~p) | ~p~n", [Rt, Rs, SignExtImm]),
   ictrl(Context, Rs, Rt, bit:zero_extension(SignExtImm), fun bit:bit_or/2),
   context:pc_add4(Context);

% xor
exec(<<16#00:6, Rs:5, Rt:5, Rd:5, _Shamt:5, 16#26:6>>, Context) ->
   io:format("xor reg(~p) := reg(~p) ^ reg(~p)~n", [Rd, Rs, Rt]),
   rctrl(Context, Rs, Rt, Rd, fun bit:bit_xor/2),
   context:pc_add4(Context);

% nor
exec(<<16#00:6, Rs:5, Rt:5, Rd:5, _Shamt:5, 16#27:6>>, Context) ->
   io:format("nor reg(~p) := ! (reg(~p) | reg(~p)", [Rd, Rs, Rt]),
   rctrl(Context, Rs, Rt, Rd, fun bit:bit_nor/2),
   context:pc_add4(Context);

% xori
exec(<<16#0E:6, Rs:5, Rt:5, SignExtImm:16>>, Context) ->
   io:format("xori reg(~p) := reg(~p) | ~p~n", [Rt, Rs, SignExtImm]),
   ictrl(Context, Rs, Rt, bit:zero_extension(SignExtImm), fun bit:bit_xor/2),
   context:pc_add4(Context);

% sll
exec(<<16#00:6, 0:5, Rt:5, Rd:5, Shamt:5, 16#00:6>>, Context) ->
   io:format("sll reg(~p) := reg(~p) << ~p~n", [Rd, Rt, Shamt]),
   ictrl(Context, Rt, Rd, Shamt, fun bit:bit_bsl/2),
   context:pc_add4(Context);

% srl
exec(<<16#00:6, 0:5, Rt:5, Rd:5, Shamt:5, 16#02:6>>, Context) ->
   io:format("srl reg(~p) := reg(~p) >> ~p~n", [Rd, Rt, Shamt]),
   ictrl(Context, Rt, Rd, Shamt, fun bit:bit_bsr/2),
   context:pc_add4(Context);

% sllv
exec(<<16#00:6, Rs:5, Rt:5, Rd:5, 0:5, 16#04:6>>, Context) ->
   io:format("sllv reg(~p) := reg(~p) << reg(~p)~n", [Rd, Rs, Rt]),
   Storage = context:get_storage(Context),
   <<_:27, Shamt:5>> = storage:read_reg(Storage, Rs),
   ictrl(Context, Rt, Rd, Shamt, fun bit:bit_bsl/2),
   context:pc_add4(Context);

% sra
exec(<<16#00:6, 0:5, Rt:5, Rd:5, Shamt:5, 16#03:6>>, Context) ->
   io:format("sra reg(~p) := reg(~p) >> ~p~n", [Rd, Rt, Shamt]),
   ictrl(Context, Rt, Rd, Shamt, fun bit:bit_bsra/2),
   context:pc_add4(Context);

% srav
exec(<<16#00:6, Rs:5, Rt:5, Rd:5, 0:5, 16#07:6>>, Context) ->
   io:format("srav reg(~p) := reg(~p) >> reg(~p)~n", [Rd, Rt, Rs]),
   Storage = context:get_storage(Context),
   <<_:27, Shamt:5>> = storage:read_reg(Storage, Rs),
   ictrl(Context, Rt, Rd, Shamt, fun bit:bit_bsra/2),
   context:pc_add4(Context);

% sw
exec(<<16#2B:6, Rs:5, Rt:5, Offset:16>>, Context) ->
   io:format("sw mem(reg(~p) + ~p) := reg(~p)~n", [Rs, Offset, Rt]),
   rctrl(Context, Rs, Rt, 0, fun(RsReg, RtReg) ->
      Addr = bit:bit_add(RsReg, bit:sign_extension(Offset)),
      Storage = context:get_storage(Context),
      storage:write_mem(Storage, Addr, RtReg)
   end),
   context:pc_add4(Context);

% lw
exec(<<16#23:6, Rs:5, Rt:5, Offset:16>>, Context) ->
   io:format("lw reg(~p) := mem(reg(~p) + ~p)~n", [Rt, Rs, Offset]),
   ictrl(Context, Rs, Rt, Offset, fun(RsReg, Immediate) ->
      Addr = bit:bit_add(RsReg, bit:sign_extension(Immediate)),
      Storage = context:get_storage(Context),
      storage:read_mem(Storage, Addr)
   end),
   context:pc_add4(Context);

% slt
exec(<<16#00:6, Rs:5, Rt:5, Rd:5, 0:5, 16#2A:6>>, Context) ->
   io:format("slt reg(~p) := reg(~p) < reg(~p)?~n", [Rd, Rs, Rt]),
   rctrl(Context, Rs, Rt, Rd, fun bit:bit_slt/2),
   context:pc_add4(Context);

% slti
exec(<<16#0A:6, Rs:5, Rt:5, Immediate:16>>, Context) ->
   io:format("slti reg(~p) := reg(~p) < ~p?~n", [Rt, Rs, Immediate]),
   ictrl(Context, Rs, Rt, <<Immediate:32>>, fun bit:bit_slt/2),
   context:pc_add4(Context);

% lui
exec(<<16#0F:6, _Rs:5, Rt:5, Immediate:16>>, Context) ->
   io:format("lui reg(~p) := ~p << 16~n", [Rt, Immediate]),
   ictrl(Context, 0, Rt, Immediate, fun(_, Imm) -> <<Imm:16, 0:16>> end),
   context:pc_add4(Context);

% j
exec(<<16#02:6, Target0:26>>, Context) ->
   <<Prefix:4, _/bits>> = context:get_pc(Context),
   Target = <<Prefix:4, Target0:26, 0:2>>,
   io:format("j ~p~n", [Target]),
   context:update_pc(Context, Target);

% beq
exec(<<16#04:6, Rs:5, Rt:5, Offset0:16>>, Context) ->
   Offset = <<0:14, Offset0:16, 0:2>>,
   NewPC = bit:bit_add(context:get_pc(Context), Offset),
   io:format("beq reg(~p) == reg(~p)? --> ~p~n", [Rs, Rt, NewPC]),
   Storage = context:get_storage(Context),
   RsReg = storage:read_reg(Storage, Rs),
   RtReg = storage:read_reg(Storage, Rt),
   if RsReg =:= RtReg ->
      context:update_pc(Context, NewPC);
   true ->
      context:pc_add4(Context)
   end;

% bne
exec(<<16#05:6, Rs:5, Rt:5, Offset0:16>>, Context) ->
    NewPC = bit:bit_add(context:get_pc(Context), bit:sign_extension(Offset0)),
   io:format("beq reg(~p) != reg(~p)? --> ~p~n", [Rs, Rt, NewPC]),
   Storage = context:get_storage(Context),
   RsReg = storage:read_reg(Storage, Rs),
   RtReg = storage:read_reg(Storage, Rt),
   if RsReg =/= RtReg ->
      context:update_pc(Context, NewPC);
   true ->
      Context
   end;

% break
exec(<<16#00:6, Code:20, 16#0D:6>>, Context) ->
   io:format("break ~p~n", [Code]),
   self() ! {break, Code},
   Context;

% unknown instruction
exec(Other, Context) ->
   io:format("unknown instruction : "),
   bit:print_bin(Other),
   io:format("~n"),
   self() ! illegal_instruction,
   Context.

start(HexData, EntryPoint) ->
   spawn(fun() ->
      Context = context:new_context(EntryPoint),
      Storage = context:get_storage(Context),
      lists:foreach(fun({Addr, Data}) ->
         storage:write_mem(Storage, <<Addr:32>>, Data)
      end, HexData),
      run(Context)
   end).

run(Context) ->
   receive
      Msg ->
         io:format("************************************~n"),
         io:format(" INTERRUPT(at ~p) : ~p~n", [context:get_pc(Context), Msg]),
         io:format("************************************~n")
   after 1000 ->
      case context:get_pc(Context) of
         end_of_program -> ok;
         Address ->
            Storage = context:get_storage(Context),
            Instruction = storage:read_mem(Storage, Address),
            NewContext = exec(Instruction, Context),
            recrio_debug:dump_context(NewContext),
            run(NewContext)
      end
   end.

