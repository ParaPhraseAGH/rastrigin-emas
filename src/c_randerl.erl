-module(c_randerl).

%%% @doc Top level interface for Erlang-alike, thread safe algorithm
%%% implemented in pure C.  This mostly offeres possibility to seed
%%% random.

-export([seed/1,
         uniform/0,
         load_nif/0]).
-on_load(load_nif/0).

-define(LIBNAME, ?MODULE).


%% @doc `ran' type copied from `random' module.
-type ran() :: {integer(), integer(), integer()}.


load_nif() ->
    SOName = filename:join([priv, ?LIBNAME]),
    ok = erlang:load_nif(SOName, []).



%% @doc This update seed to same one on all schedulers.
-spec seed(Seed :: ran()) -> ok.
seed(_Seed) ->
    erlang:nif_error(nif_not_loaded).


%% @doc Return float from 0.0 to 1.0; just like `random:uniform/0'
-spec uniform() -> float().
uniform() ->
    erlang:nif_error(nif_not_loaded).
    
