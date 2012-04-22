#include "erl_nif.h"
#include <string.h>
#define MEMORY_SIZE 0xFFFFFFFF

// @spec write_at(binary(), integer(), binary()) :: binary()
static ERL_NIF_TERM write_at(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
   ErlNifBinary bin;
   ErlNifBinary write_bin;
   ERL_NIF_TERM bin_ret;
   unsigned char *buf;
   unsigned int offset;
   if(!enif_inspect_binary(env, argv[0], &bin)) {
      return enif_make_badarg(env);
   }
   if(!enif_inspect_binary(env, argv[2], &write_bin)) {
      return enif_make_badarg(env);
   }
   if(!enif_get_uint(env, argv[1], &offset)) {
      return enif_make_badarg(env);
   }
   printf("%u + %u <= %u\r\n", write_bin.size, offset, bin.size);
   if(write_bin.size + offset > bin.size ) {
      return enif_make_badarg(env);
   }
   buf = enif_make_new_binary(env, bin.size, &bin_ret);
   memcpy(buf, bin.data, bin.size);
   for(unsigned int i = 0; i < write_bin.size; ++i) {
      buf[offset+i] = write_bin.data[i];
   }
   return bin_ret;
}

static ErlNifFunc nif_funcs[] = {
   {"write_at", 3, write_at},
};

ERL_NIF_INIT(memory, nif_funcs, NULL, NULL, NULL, NULL)

