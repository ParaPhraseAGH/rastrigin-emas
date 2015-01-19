#include "stdio.h"
#include "stdlib.h"
#include "time.h"
#include "c_randerl.h"


#define PRIME1 30269
#define PRIME2 30307
#define PRIME3 30323


SeedLock* erl_get_seed(SeedLocks* all_seeds);
void free_seed();
double uniform_s(Seed* seed);
uint threads_count();

Seed* new_seed(uint a, uint b, uint c);
SeedLock* new_seed_lock(Seed* seed, char* name);
SeedLocks* new_seed_locks();

double
uniform (SeedLocks* all_seeds) {
  SeedLock* lock = erl_get_seed(all_seeds);
  double random = uniform_s(lock->seed);
  free_seed(lock);
  return random;
}


SeedLock*
erl_get_seed(SeedLocks* all_seeds) {
  int i;
  SeedLock* seed_lock;
  for (i = 0; ;  ++i) {
    seed_lock = all_seeds->locks[i];
    if(enif_rwlock_tryrwlock(seed_lock->lock) == 0) {
      break;
    }

    if (i == all_seeds->count -1) {
      printf("reseting nif count\n");
      i = -1;
    }
  }

  return seed_lock;
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


SeedLocks*
create_seeds() {
  return new_seed_locks();
}


Seed*
new_seed(uint a, uint b, uint c) {
  Seed* seed = (Seed*) enif_alloc(sizeof (Seed));
  seed->a = a;
  seed->b = b;
  seed->c = c;
  return seed;
}


SeedLock*
new_seed_lock(Seed* seed, char* name) {
  SeedLock* seed_lock = (SeedLock*) enif_alloc(sizeof (SeedLock));

  seed_lock->seed = seed;
  seed_lock->lock = enif_rwlock_create(name);

  return seed_lock;
}

SeedLocks*
new_seed_locks() {
  SeedLocks* all_locks = (SeedLocks*) enif_alloc(sizeof(SeedLocks));
  uint lock_count = threads_count();
  SeedLock** lock_array = (SeedLock**) enif_alloc(sizeof(SeedLock*) * lock_count);
  char name[30];
  uint i;


  all_locks->count = lock_count;
  all_locks->locks = lock_array;

  for (i = 0; i < lock_count; i++) {
    sprintf(name, "nif_random_seed_lock_%03d", i+1);
    Seed* seed = new_seed(rand(), rand(), rand());
    all_locks->locks[i] = new_seed_lock(seed, name);
  }


  return all_locks;
}

uint
threads_count() {
  ErlNifSysInfo sys_info;
  enif_system_info(&sys_info, sizeof(sys_info));
  int scheduler_threads  = sys_info.scheduler_threads;
  int driver_threads = sys_info.async_threads;
  return scheduler_threads + driver_threads;
}
