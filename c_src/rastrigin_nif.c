#include "erl_nif.h"
#include "stdio.h"
#include "stdlib.h"
#include "time.h"
#include "rastrigin.h"
#include "c_randerl.h"

// #define SEED 0
// #define DEBUG

#define CHECK(env, obj) if (!(obj)){return enif_make_badarg((env));}


extern double   fitness_rastrigin(Solution* sol);
extern double   mutate_feature(double feature, double mutation_range, SeedLocks* all_seeds);
extern void     mutate(Solution* prev, Solution* out, double range, double rate, SeedLocks* all_seeds);
extern void     recombine(Solution** parents, Solution** children, SeedLocks* all_seeds);
extern double   randdouble(double lower, double upper, SeedLocks* all_seeds);
extern void     print_solution(Solution* sol, const char* desc);
extern unsigned int get_seed();


// ErlNifResourceType* SOL_TYPE;

static void solution_dtor(ErlNifEnv* env, void* obj){
    Solution* sol = (Solution*) obj;
    if (sol != NULL && sol->genotype != NULL){
        free(sol->genotype);
    }
}

typedef struct {
  ErlNifResourceType* sol_type;
  SeedLocks* all_seeds;
} PrivData;



static int nif_load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info){
    const char* mod = "rastrigin_ops_nif";
    const char* name = "Solution";
    int flags = ERL_NIF_RT_CREATE; // | ERL_NIF_RT_TAKEOVER;



    PrivData* priv_data = enif_alloc(sizeof(priv_data));

    priv_data->all_seeds = create_seeds();
    priv_data->sol_type = enif_open_resource_type(env, mod, name, solution_dtor, flags, NULL);

    *priv =  priv_data;

    return 0;
}

static int
nif_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info){
  return 0;
}

static void
nif_unload(ErlNifEnv* env, void* priv_data){
  return;
}



static ERL_NIF_TERM evaluate_solution(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    PrivData* priv_data = (PrivData*) enif_priv_data(env);
    CHECK(env, priv_data)

    ErlNifResourceType* sol_type = priv_data->sol_type;

    Solution* sol;
    double fitness;

    CHECK(env, argc == 1)
    CHECK(env, enif_get_resource(env, argv[0], sol_type, (void**) &sol))

    #ifdef DEBUG
    print_solution(sol,"Genotype");
    #endif

    fitness = fitness_rastrigin(sol);


    return enif_make_double(env, fitness);

}



static ERL_NIF_TERM recombine_solutions(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    PrivData* priv_data = (PrivData*) enif_priv_data(env);
    CHECK(env, priv_data)

    ErlNifResourceType* sol_type = priv_data->sol_type;
    SeedLocks* all_seeds = priv_data->all_seeds;

    ERL_NIF_TERM terms[2];
    Solution *sols[2], *new_sols[2]; // arrays of two pointers of type Solution*
    unsigned int i;
    unsigned int len;

    CHECK(env, argc == 2)

    // read parent solutions
    for (i=0; i<2; i++){
        CHECK(env, enif_get_resource(env, argv[i], sol_type, (void**) &sols[i]))
    }


    #ifdef DEBUG
    print_solution(sols[0],"Genotype1");
    print_solution(sols[1],"Genotype2");
    #endif

    // allocate 2 child solution structures
    len = sols[0]->len;
    for (i=0; i<2; i++){
        new_sols[i] = (Solution*) enif_alloc_resource(sol_type, sizeof(Solution));
        CHECK(env, new_sols[i])

        terms[i] = enif_make_resource(env, new_sols[i]);
        CHECK(env,terms[i])
        enif_release_resource(new_sols[i]);

        new_sols[i]->len = len;
        new_sols[i]->genotype = (double*) malloc(sizeof(double)*len);
    }


    recombine(sols, new_sols, all_seeds);

    #ifdef DEBUG
    print_solution(new_sols[0],"RecombinedGenotype1");
    print_solution(new_sols[1],"RecombinedGenotype2");
    #endif

    return enif_make_tuple2(env, terms[0], terms[1]);
}


static ERL_NIF_TERM mutate_solution(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    PrivData* priv_data = (PrivData*) enif_priv_data(env);
    CHECK(env, priv_data)

    ErlNifResourceType* sol_type = priv_data->sol_type;
    SeedLocks* all_seeds = priv_data->all_seeds;

    ERL_NIF_TERM term;
    Solution *sol, *mut_sol;
    double range, rate;

    CHECK(env, argc == 3)


    CHECK(env, enif_get_resource(env, argv[0], sol_type, (void**) &sol))
    CHECK(env, enif_get_double(env, argv[1], &range))
    CHECK(env, enif_get_double(env, argv[2], &rate))

    mut_sol = (Solution*) enif_alloc_resource(sol_type, sizeof(Solution));
    CHECK(env, mut_sol)

    term = enif_make_resource(env, mut_sol);
    CHECK(env,term)
    enif_release_resource(mut_sol);

    mut_sol->len = sol->len;
    mut_sol->genotype = (double*) malloc(sizeof(double)*sol->len);

    mutate(sol, mut_sol, range, rate, all_seeds);

    #ifdef DEBUG
    print_solution(sol, "Genotype");
    print_solution(mut_sol, "MutatedGenotype");
    #endif


    return term;

}

static ERL_NIF_TERM create_solution(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

    PrivData* priv_data = (PrivData*) enif_priv_data(env);
    CHECK(env, priv_data)

    ErlNifResourceType* sol_type = priv_data->sol_type;
    SeedLocks* all_seeds = priv_data->all_seeds;

    ERL_NIF_TERM term;
    Solution* sol;
    unsigned int len;
    unsigned int i;

    CHECK(env, argc == 1)
    CHECK(env, enif_get_uint(env, argv[0], &len))


    sol = (Solution*) enif_alloc_resource(sol_type, sizeof(Solution));
    CHECK(env, sol)

    term = enif_make_resource(env, sol);
    CHECK(env,term)
    enif_release_resource(sol);

    sol->len = len;
    sol->genotype = (double*) malloc(sizeof(double)*len);
    for (i=0;i<len;i++){
      sol->genotype[i] = randdouble(-50.0, 50.0, all_seeds);
    }

    return term;
}


static ErlNifFunc nif_funcs[] = {
    {"nif_solution", 1, create_solution},
    {"nif_evaluation", 1, evaluate_solution},
    {"nif_mutation", 3, mutate_solution},
    {"nif_recombination", 2, recombine_solutions}
};

ERL_NIF_INIT(rastrigin_nif_ops, nif_funcs, nif_load, NULL, nif_upgrade, nif_unload)
