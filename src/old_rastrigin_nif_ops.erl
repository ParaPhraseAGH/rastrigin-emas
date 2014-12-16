-module (old_rastrigin_nif_ops).

-behaviour (emas_genetic_ops).

-include_lib("emas/include/emas.hrl").

-export([solution/1,
         evaluation/2,
         mutation/2,
         recombination/3]).

-on_load(init/0).
-define (LIBNAME, "old_rastrigin_nif").

-type sim_params() :: emas:sim_params().
-type solution() :: emas:solution(binary()).

-spec init() -> ok.
init() ->
    SOName = filename:join([priv, ?LIBNAME]) ,
    ok = erlang:load_nif(SOName, 0).

%% @doc Generates a random solution.
-spec solution(sim_params()) -> solution().
solution(SP) ->
    nif_solution(SP#sim_params.problem_size).

nif_solution(_ProblemSize) ->
    erlang:nif_error(nif_not_loaded).

%% @doc Evaluates a given solution. Higher is better.
-spec evaluation(solution(), sim_params()) -> float().
evaluation(Solution, _SP) ->
    nif_evaluation(Solution).

nif_evaluation(_Solution) ->
    erlang:nif_error(nif_not_loaded).

%% @doc FReproduction function for a pair of agents (crossover and mutation).
-spec recombination(solution(), solution(), sim_params()) -> {solution(), solution()}.
recombination(_Solution1, _Solution2, _SP) ->
    nif_recombination(_Solution1, _Solution2).

nif_recombination(_Solution1, _Solution2) ->
    erlang:nif_error(nif_not_loaded).


-spec mutation(solution(), sim_params()) -> solution().
mutation(_Solution, SP) ->
    nif_mutation(_Solution, SP#sim_params.mutation_range, SP#sim_params.mutation_rate).

%% @doc Reproduction function for a single agent (mutation only).
-spec nif_mutation(solution(), float(), float()) -> solution().
nif_mutation(_Solution, _Range, _Rate) ->
    erlang:nif_error(nif_not_loaded).

