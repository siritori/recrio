-module(storage).
-compile(export_all).
-define(ZERO, <<0:32>>).

new() ->
   TableID = ets:new(virtual_memory, []),
   lists:foreach(fun(I) ->
      ets:insert(TableID, {{reg, I}, ?ZERO})
   end, lists:seq(0, 31)),
   TableID.

read_reg(TableID, Index) ->
   [{{reg, Index}, Data}] = ets:lookup(TableID, {reg, Index}),
   Data.

write_reg(_TableID, 0, _Data) ->
   true;
write_reg(TableID, Index, Data) ->
   true = ets:insert(TableID, {{reg, Index}, Data}).

write_mem(TableID, <<Addr:32>>, Data) when size(Data) =:= 4 ->
   io:format("addr:~p~n", [Addr]),
   if Addr rem 4 =:= 0 ->
      true = ets:insert(TableID, {{mem, Addr}, Data});
   true ->
      self() ! address_error
   end.

read_mem(TableID, <<Addr:32>>) ->
   if Addr rem 4 =:= 0 ->
         case ets:lookup(TableID, {mem, Addr}) of
            [{{mem, Addr}, Data}] -> Data;
            [] -> ?ZERO
         end;
   true ->
         self() ! address_error,
         ?ZERO
   end.

