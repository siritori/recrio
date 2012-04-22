-module(context).
-compile(export_all).
-define(ZERO, <<0:32>>).

new_context(EntryPoint) ->
   PC = EntryPoint,
   Hi = ?ZERO,
   Lo = ?ZERO,
   Cause = ?ZERO,
   EPC = ?ZERO,
   Storage = storage:new(), % memory & registers
   {PC, {Hi, Lo}, EPC, Storage}.

get_storage(Context) ->
   {_, _, Storage} = Context,
   Storage.

hilo_of(Context) ->
   {_, HiLo, _} = Context,
   HiLo.

get_pc(Context) ->
   {PC, _, _} = Context,
   PC.
update_pc({_, HiLo, Storage}, NewPC) ->
   {NewPC, HiLo, Storage}.

pc_add4(Context) ->
   <<OldPC:32>> = get_pc(Context),
   update_pc(Context, <<(OldPC+4):32>>).


