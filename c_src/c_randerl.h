#pragma once

#include "erl_nif.h"


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
  SeedLock** locks;
  uint count;
} SeedLocks;


double uniform();
SeedLocks* create_seeds();


