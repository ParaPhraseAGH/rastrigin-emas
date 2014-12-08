#include "erl_nif.h"
#include "stdio.h"
#include "stdlib.h"
#include "time.h"
#include "c_randerl.h"


#define PRIME1 30269
#define PRIME2 30307
#define PRIME3 30323


Seed* erl_get_seed();
double uniform_s(Seed* seed);

double
uniform () {
  Seed *seed = erl_get_seed();
  return uniform_s(seed);
}


// TODO remove inicial seed
Seed global_seed = {3172, 9814, 20125};

Seed* 
erl_get_seed() {
  return &global_seed;
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
  ErlNifSysInfo sys_info;
  enif_system_info(&sys_info, sizeof(sys_info));
  int scheduler_threads  = sys_info.scheduler_threads;
  int dirty_threads = sys_info.dirty_scheduler_support ? scheduler_threads : 0;
  int driver_threads = sys_info.async_threads;
  printf("Schedulers from C: %d + %d + %d  \n", scheduler_threads, dirty_threads, driver_threads);
  // 

  return;
}

