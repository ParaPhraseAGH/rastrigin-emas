#include "erl_nif.h"
#include "stdio.h"
#include "stdlib.h"
#include "time.h"
#include "c_randerl.h"


#define PRIME1 30269
#define PRIME2 30307
#define PRIME3 30323


typedef struct {
  uint a;
  uint b;
  uint c;
} Seed;

typedef struct {
  Seed* seed;
  ErlNifRWLock* lock;
} SeedLock;

typedef struct {
  SeedLock * locks;
  uint count;
} SeedLocks;


SeedLock* erl_get_seed();
void free_seed();
double uniform_s(Seed* seed);
uint threads_count();

SeedLock global_seed_lock;
Seed global_seed = {
  3172,
  9814,
  20125,
};


double
uniform () {
  SeedLock* lock = erl_get_seed();
  double random = uniform_s(lock->seed);
  free_seed(lock);
  return random;
}

SeedLock*
erl_get_seed() {
  enif_rwlock_rwlock(global_seed_lock.lock);
  return &global_seed_lock;
}

void free_seed(SeedLock* lock){
  enif_rwlock_rwunlock(lock->lock);
  return;
}


double
uniform_s (Seed *seed) {
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
  threads_count();
  //

  SeedLock seed_lock =  {
    .lock = enif_rwlock_create("nif_random_seed_lock"),
    .seed = &global_seed,
  };

  global_seed_lock = seed_lock;

  return;
}

uint
threads_count() {
  ErlNifSysInfo sys_info;
  enif_system_info(&sys_info, sizeof(sys_info));
  int scheduler_threads  = sys_info.scheduler_threads;
  int dirty_threads = sys_info.dirty_scheduler_support ? scheduler_threads : 0;
  int driver_threads = sys_info.async_threads;
  printf("Schedulers from C: %d + %d + %d  \n", scheduler_threads, dirty_threads, driver_threads);
  return scheduler_threads + dirty_threads * driver_threads;
}
