#include "rastrigin.h"
#include "stdio.h"
#include "stdlib.h"
#include "c_randerl.h"


double randdouble(double lower, double upper, SeedLocks* all_seeds){
  return lower + (upper - lower) * uniform(all_seeds);
}

void print_solution(Solution* sol, const char* desc){
    int i;
    printf("%s:",desc);
    for(i=0; i < sol->len; i++){
        printf("%.10f ", sol->genotype[i]);
    }
    printf("\n");
}

/// genetic operations

double fitness_rastrigin(Solution * sol){
    double s = 0.0;
    int i;

    for (i=0; i<sol->len; i++){
        double x = sol->genotype[i];
        s += x*x - 10.0*cos(M_TWOPI*x) + 10.0;
    }

    return -s;
}


double mutate_feature(double feature, double mutation_range, SeedLocks* all_seeds){
    double range;
    range = randdouble(0.0, mutation_range, all_seeds);
    if (range < 0.2){
        range = 5.0;
    } else if (range < 0.4){
        range = 0.2;
    } else {
        range = 1.0;
    }
    return feature + range * tan(M_PI * randdouble(-0.5, 0.5, all_seeds));
}

void mutate(Solution* prev, Solution* out, double range, double rate, SeedLocks* all_seeds){
    int i;
    for (i=0;i<prev->len;i++){
        if (randdouble(0.0,1.0, all_seeds) < rate){
            out->genotype[i] = mutate_feature(prev->genotype[i], range, all_seeds);
        } else {
            out->genotype[i] = prev->genotype[i];
        }
    }
}

void recombine(Solution** parents, Solution** children, SeedLocks* all_seeds){
    int i;    
    // recombine parent solutions into 2 child solutions
    for (i=0;i<parents[0]->len;i++){
        double lower, upper;
        if (parents[0]->genotype[i] >= parents[1]->genotype[i]){
            lower = parents[1]->genotype[i];
            upper = parents[0]->genotype[i];
        } else {
            lower = parents[0]->genotype[i];
            upper = parents[1]->genotype[i];
        }
        children[0]->genotype[i] = randdouble(lower,upper, all_seeds);
        children[1]->genotype[i] = randdouble(lower,upper, all_seeds);
    }
}
