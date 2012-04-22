-module(recrio_debug).
-compile(export_all).

r(Op, Funct, Rs, Rt, Rd, Shamt) ->
   <<Op:6, Rs:5, Rt:5, Rd:5, Shamt:5, Funct:6>>.
i(Op, Rs, Rt, Immediate) ->
   <<Op:6, Rs:5, Rt:5, Immediate:16>>.
j(Op, Address) ->
   <<Op:6, Address:26>>.

dump_context(Context) ->
   case context:get_pc(Context) of
   end_of_program ->
      ok;
   <<PC:32>> ->
      io:format("at ~p:~n", [PC]),
      Storage = context:get_storage(Context),
      lists:foreach(fun(I) ->
         case storage:read_reg(Storage, I) of
            <<0:32>> -> ok;
            Bin ->
               io:format("reg~2p : ", [I]),
               bit:print_bin(Bin),
               io:format("~n")
         end
      end, lists:seq(0, 31)),
      Memories = lists:sort([{Addr, Data} || {{mem, Addr}, Data} <- ets:tab2list(Storage)]),
      lists:foreach(fun({Addr, Data}) ->
         io:format("~4.10B : ", [Addr]),
         bit:print_bin(Data),
         io:format("~n")
      end, Memories),
      io:format("~n~n")
   end.

gen_hex(Instructions) ->
   {_, Ret} = lists:foldl(fun(I, {PC, Acc}) ->
      {PC+4, [{PC, I}|Acc]}
   end, {0, []}, Instructions),
   Ret.

test() ->
   HexData = [
      {0,  i(8, 0, 1, 16#FFFF)},
      {4,  i(8, 0, 2, 16#FF00)},
      {8,  r(0, 16#24, 1, 2, 3, 0)},
      {12, j(12, 0)}
   ],
   recrio:start(HexData, <<0:32>>).

test2() ->
   HexData = [
      {8#10, i(8, 1, 0, 16#FFFF)},
      {8#14, i(8, 0, 0, 16#FFFF)},
      {8#20, i(8, 0, 0, 16#FFFF)},
      {8#24, r(0, 16#24, 1, 2, 3, 0)},
      {8#30, j(18, 0)}
   ],
   recrio:start(HexData, <<8#10:32>>).

test3() ->
   HexData = gen_hex([
      i(8, 1, 0, 16#FFFF),
      r(0, 0, 0, 0, 3, 2),
      r(0, 0, 0, 3, 3, 2),
      r(0, 0, 0, 3, 3, 2),
      r(0, 2, 0, 3, 3, 2),
      i(8, 1, 1, 2#11),
      r(0, 4, 1, 3, 3, 0),
      j(18, 0)
   ]),
   recrio:start(HexData, <<0:32>>).

test4() ->
   HexData = gen_hex([
      i(8, 0, 1, 16#FFFF),      % addi $1, $0, 0xFFFF
      i(8, 0, 2, 16#50F0),      % addi $2, $0, 0x50F0
      r(0, 16#00, 0, 1, 1, 16), % sll $1, $1, 16
      r(0, 16#25, 1, 2, 1, 0),  % or $1, $1, $2
      i(16#2B, 0, 1, 16),       % sw $1, $0(16)
      i(16#23, 0, 3, 16),       % lw $3, $0(16)
      j(18, 0)
   ]),
   recrio:start(HexData, <<0:32>>).

test5() ->
   HexData = gen_hex([
      i(8, 0, 1, 16#FF00),
      i(16#0A, 1, 3, 16#FFFF),
      i(16#0F, 0, 4, 16#F0F4),
      i(16#09, 4, 4, 16#7171),
      r(0, 16#27, 0, 4, 4, 0),
      r(0, 16#2A, 3, 1, 5, 0),
      j(2, 1),
      j(18, 0)
   ]),
   recrio:start(HexData, <<0:32>>).

test6() ->
   HexData = gen_hex([
      i(8, 0, 2, 8),       % addi $2, $0, 8
      i(8, 1, 1, 1),       % addi $1, $1, 1
      i(4, 1, 2, 2),       % beq  $2, $1, 2
      j(2, 1),             % j 1
      r(0, 0, 0, 1, 1, 3), % sll  $1, $1, 3
      <<0:6, 16#FF:20, 16#0D:6>>% break
   ]),
   recrio:start(HexData, <<0:32>>).

