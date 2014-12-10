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
SeedLock* new_seed_lock();
SeedLocks* new_seed_locks();

Seed global_seed = {
  3172,
  9814,
  20125,
};


double
uniform (SeedLocks* all_seeds) {
  SeedLock* lock = erl_get_seed(all_seeds);
  double random = uniform_s(lock->seed);
  free_seed(lock);
  return random;
}


SeedLock*
erl_get_seed(SeedLocks* all_seeds) {
  SeedLock* seed_lock = all_seeds->locks[0];
  enif_rwlock_rwlock(seed_lock->lock);
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
new_seed_lock(Seed* seed) {
  SeedLock* seed_lock = (SeedLock*) enif_alloc(sizeof (SeedLock));

  seed_lock->seed = seed;
  seed_lock->lock = enif_rwlock_create("nif_random_seed_lock");

  return seed_lock;
}

SeedLocks*
new_seed_locks() {
  SeedLocks* all_locks = (SeedLocks*) enif_alloc(sizeof(SeedLocks));
  uint lock_count = threads_count();
  SeedLock** lock_array = (SeedLock**) enif_alloc(sizeof(SeedLock) * lock_count);

  all_locks->count = lock_count;
  all_locks->locks = lock_array;

  all_locks->locks[0] = new_seed_lock(&global_seed);

  return all_locks;
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
