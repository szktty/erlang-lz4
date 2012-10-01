#include <stdbool.h>
#include <string.h>
#include "erl_nif.h"
#include "lz4.h"
#include "lz4hc.h"

typedef struct block_context    block_context;

struct block_context {
  ErlNifTid tid;
  char *src, *dest;
  size_t src_size;
  int dest_size;
};

static ERL_NIF_TERM nif_compress(ErlNifEnv* env, int argc,
    const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_uncompress(ErlNifEnv* env, int argc,
    const ERL_NIF_TERM argv[]);
static void *compress_block_th(void *args);

static ErlNifFunc nif_funcs[] =
{
    {"compress", 2, nif_compress},
    {"uncompress", 2, nif_uncompress}
};

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_high;
static ERL_NIF_TERM atom_block;

static ERL_NIF_TERM
nif_compress(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM opts_term, head_term, tail_term, ret_term;
  const ERL_NIF_TERM *terms;
  ErlNifBinary src_bin, res_bin;
  bool high = false, error;
  long block_size = 0, procs;
  int arity, real_size, i, j;
  size_t res_size, rest_size, src_block_size;
  block_context *blocks;
  char *src, *dest, *compact;
  void **th_args[3];

  if (!enif_inspect_binary(env, argv[0], &src_bin) ||
      !enif_is_list(env, argv[1]))
    return 0;

  opts_term = argv[1];
  while (enif_get_list_cell(env, opts_term, &head_term, &tail_term)) {
    if (enif_is_identical(head_term, atom_high))
      high = true;
    else if (enif_get_tuple(env, head_term, &arity, &terms)) {
      if (arity == 3) {
        if (enif_is_identical(terms[0], atom_block)) {
          if (!enif_get_long(env, terms[1], &block_size) ||
              !enif_get_long(env, terms[2], &procs))
            /* TODO: verbose error */
            return 0;
        }
      }
    }
    opts_term = tail_term;
  }

  res_size = LZ4_compressBound(src_bin.size);
  enif_alloc_binary(res_size, &res_bin);

  if (block_size <= 0 || block_size >= src_bin.size) {
    if (high)
      real_size = LZ4_compressHC((char *)src_bin.data,
          (char *)res_bin.data, src_bin.size);
    else
      real_size = LZ4_compress((char *)src_bin.data,
          (char *)res_bin.data, src_bin.size);

    if (real_size >= 0)
      goto ok;
    else
      goto error;
  } else {
    /* TODO: uncompress failed */
    blocks = (block_context *)enif_alloc(sizeof(block_context) * procs);
    rest_size = src_bin.size;
    src = (char *)src_bin.data;
    dest = (char *)res_bin.data;
    compact = (char *)res_bin.data;
    th_args[0] = (void *)env;
    th_args[1] = (void *)&high;
    i = 0;
    src_block_size = block_size;
    while (rest_size > 0) {
      th_args[2] = (void *)&blocks[i];
      blocks[i].src = src;
      blocks[i].dest = dest;
      if (rest_size < block_size)
        src_block_size = rest_size;
      blocks[i].src_size = src_block_size;
      src += src_block_size;
      dest += src_block_size;
      rest_size -= src_block_size;
      enif_thread_create("lz4:compress", &blocks[i].tid,
          compress_block_th, th_args, NULL);
      i++;

      if (i == procs || rest_size == 0) {
        j = i;
        error = false;
        for (i = 0; i < j; i++) {
          enif_thread_join(blocks[i].tid, NULL);
          if (blocks[i].dest_size < 0)
            error = true;
          else if (!error) {
            memmove(compact, blocks[i].dest, blocks[i].dest_size);
            compact += blocks[i].dest_size;
          }
        }

        if (error) {
          enif_free(blocks);
          goto error;
        } else if (rest_size == 0)
          break;
        else
          i = 0;
      }
    }

    enif_free(blocks);
    if (!error) {
      real_size = (uintptr_t)compact - (uintptr_t)res_bin.data;
      goto ok;
    } else
      goto error;
  }

ok:
  enif_realloc_binary(&res_bin, real_size);
  ret_term = enif_make_tuple2(env, atom_ok,
      enif_make_binary(env, &res_bin));
  enif_release_binary(&res_bin);
  return ret_term;

error:
  enif_release_binary(&res_bin);
  /* TODO: verbose error */
  return atom_error;
}

static void *
compress_block_th(void *argsp)
{
  ErlNifEnv *env;
  void **args;
  bool high;
  block_context *block;

  args = (void **)argsp;
  env = (ErlNifEnv *)args[0];
  high = *(bool *)args[1];
  block = (block_context *)args[2];

  if (high)
    block->dest_size = LZ4_compressHC(block->src, block->dest,
        block->src_size);
  else
    block->dest_size = LZ4_compress(block->src, block->dest,
        block->src_size);

  enif_thread_exit(NULL);
  return NULL;
}

static ERL_NIF_TERM
nif_uncompress(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM ret_term;
  ErlNifBinary src_bin, res_bin;
  long res_size;
  int read_bytes;

  if (!enif_inspect_binary(env, argv[0], &src_bin) ||
      !enif_get_long(env, argv[1], &res_size))
    return 0;

  enif_alloc_binary((size_t)res_size, &res_bin);

  read_bytes = LZ4_uncompress((char *)src_bin.data, (char *)res_bin.data,
      res_bin.size);
  if (read_bytes >= 0) {
    ret_term = enif_make_tuple2(env, atom_ok,
        enif_make_binary(env, &res_bin));
    enif_release_binary(&res_bin);
    return ret_term;
  } else {
    enif_release_binary(&res_bin);
    /* TODO: verbose error */
    return atom_error;
  }
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  atom_ok = enif_make_atom(env, "ok");
  atom_error = enif_make_atom(env, "error");
  atom_high = enif_make_atom(env, "high");
  atom_block = enif_make_atom(env, "block");
  return 0;
}

ERL_NIF_INIT(lz4, nif_funcs, &on_load, NULL, NULL, NULL);

