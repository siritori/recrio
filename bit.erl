-module(bit).
-export([
   print_bin/1,
   decode_int32/1,
   bit_not/1,
   bit_and/2,
   bit_or/2,
   bit_xor/2,
   bit_add/2,
   bit_addu/2,
   bit_sub/2,
   bit_subu/2,
   bit_bsl/2,
   bit_bsr/2,
   bit_bsra/2,
   bit_nor/2,
   bit_slt/2,
   sign_extension/1,
   zero_extension/1
]).
-define(MAX, 16#7FFFFFFF).
-define(MIN, 2#10000000000000000000000000000000).

print_bin(<<>>) -> ok;
print_bin(<<B:1, Rest/bits>>) ->
   io:format("~p", [B]),
   print_bin(Rest).

decode_int32(<<Bin:32>>) ->
   case <<Bin:32>> of
      <<0:1, _/bits>> ->
         Bin;
      <<1:1, _/bits>> ->
         - binary:decode_unsigned(<<((bnot Bin)+1):32>>)
   end.


bit_not(<<A:32>>) ->
   <<bnot A:32>>.

bit_and(<<A:32>>, <<B:32>>) ->
   <<(A band B):32>>.

bit_or(<<A:32>>, <<B:32>>) ->
   <<(A bor B):32>>.

bit_xor(<<A:32>>, <<B:32>>) ->
   <<(A bxor B):32>>.

bit_bsl(Bin, Shift) ->
   <<_:Shift, Rest/bits>> = Bin,
   <<Rest/bits, 0:Shift>>.

bit_bsr(Bin, Shift) ->
   Size = size(Bin) * 8 - Shift,
   <<Rest:Size, _/bits>> = Bin,
   <<0:Shift, Rest:Size>>.

bit_bsra(<<1:1, _/bits>> = Bin, Shift) ->
   Size = size(Bin) * 8 - Shift,
   <<Rest:Size, _/bits>> = Bin,
   <<1:Shift, Rest:Size>>;
bit_bsra(<<0:1, _/bits>> = Bin, Shift) ->
   bit_bsr(Bin, Shift).

bit_add(<<A:32>>, <<B:32>>) ->
   C = A + B,
   if
      C > ?MAX ->
         self() ! recrio_overflow,
         <<0:32>>;
      true ->
         <<C:32>>
   end.

bit_addu(<<A:32>>, <<B:32>>) ->
   <<(A+B):32>>.

bit_sub(<<A:32>>, <<B:32>>) ->
   C = A - B,
   if
      C < ?MIN -> self() ! recrio_underflow;
      true     -> <<C:32>>
   end.

bit_subu(<<A:32>>, <<B:32>>) ->
   <<(A-B):32>>.

bit_nor(A, B) ->
   bit_not(bit_or(A, B)).

bit_slt(A, B) ->
   IntA = decode_int32(A),
   IntB = decode_int32(B),
   if IntA < IntB -> <<1:32>>;
   true -> <<0:32>>
   end.


sign_extension(Bin) -> % 16bit -> 32bit
   case <<Bin:16>> of
      <<1:0, _/bits>> -> <<1:16, Bin:16>>;
      <<0:1, _/bits>> -> <<0:16, Bin:16>>
   end.

zero_extension(Bin) -> % 16bit -> 32bit
   <<0:16, Bin:16>>.
