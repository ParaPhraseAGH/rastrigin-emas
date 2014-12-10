#include "erl_nif.h"
#include "stdio.h"
#include "stdlib.h"
#include "time.h"
#include "c_randerl.h"


#define CHECK(env, obj) if (!(obj)){return enif_make_badarg((env));}


static int
nif_load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info){
  create_seeds();
  return 0;
}

static int
nif_reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info){
  return 0;
}

static int
nif_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info){
  return 0;
}

static void
nif_unload(ErlNifEnv* env, void* priv_data){

}

static ERL_NIF_TERM
nif_seed_all(ErlNifEnv* env, int arc, const ERL_NIF_TERM argv[]){
  
  return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
nif_uniform(ErlNifEnv* env, int arc, const ERL_NIF_TERM argv[]){
  double Random = uniform();
  return enif_make_double(env, Random);
}


static ErlNifFunc nif_funcs[] = {
  {"seed", 1, nif_seed_all},
  {"uniform", 0, nif_uniform}
};


ERL_NIF_INIT(c_randerl, nif_funcs, nif_load, nif_reload, nif_upgrade, nif_unload)