#include "erl_nif.h"
#include "stdio.h"
#include "stdlib.h"
#include "time.h"


#define CHECK(env, obj) if (!(obj)){return enif_make_badarg((env));}


double uniform();
void create_seeds();

typedef struct {
  int a;
  int b;
  int c;
} Seed;


#define PRIME1 30269
#define PRIME2 30307
#define PRIME3 30323


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
  srand(time(NULL));
  return enif_make_atom(env, "ok");
}

// TODO remove inicial seed
Seed seed = {3172, 9814, 20125};

static ERL_NIF_TERM
nif_uniform(ErlNifEnv* env, int arc, const ERL_NIF_TERM argv[]){
  double Random = uniform(&seed);
  return enif_make_double(env, Random);
}

double
uniform (Seed *seed) {
  seed->a = (seed->a * 171) % PRIME1;
  seed->b = (seed->b * 172) % PRIME2;
  seed->c = (seed->c * 170) % PRIME3;

  double Random = (double)seed->a / PRIME1
    + (double)seed->b / PRIME2
    + (double)seed->c / PRIME3;

  return Random - (int)Random;
}

void
create_seeds() {
  // notes 
  ErlNifSysInfo sys_info;
  enif_system_info(&sys_info, sizeof(sys_info));
  int scheduler_threads  = sys_info.scheduler_threads;
  int dirty_threads = sys_info.dirty_scheduler_support ? scheduler_threads : 0;
  int driver_threads = sys_info.async_threads;
  printf("Schedulers from C: %d + %d + %d  \n", scheduler_threads, dirty_threads, driver_threads);
  // 

  return;
}

static ErlNifFunc nif_funcs[] = {
  {"seed", 1, nif_seed_all},
  {"uniform", 0, nif_uniform}
};


ERL_NIF_INIT(c_randerl, nif_funcs, nif_load, nif_reload, nif_upgrade, nif_unload)
