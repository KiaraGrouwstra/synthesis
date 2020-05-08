-- import Data.Function.Memoize
-- -- memoize

-- deriveMemoizable ''Double
-- -- deriveMemoizable ''Int
-- deriveMemoizable ''HparComb

module Synthesis.Evolutionary (module Synthesis.Evolutionary) where

-- def eval_loss(dct):
--     FLAGS = argparse.Namespace(**dct)
--     # ^ does this work to be in scope for train_mlp_pytorch?
--     print_flags()
--     return train()

-- run_ea = get_ea(options, eval_loss)
-- best_member, best_loss = run_ea(n=8, rounds=20)


-- '''generic evolutionary algorithm that we can use for hyperparameter tuning'''
-- import random
-- from frozendict import frozendict
-- from joblib import Memory
-- cachedir = '/tmp'
-- mem = Memory(cachedir)

-- # handle closures. is it obvious i'm not a fan of OOP?
-- def get_ea(options, eval_loss):
--     # memoize the evaluation function to cut down on expensive evaluations at the cost of some storage
--     # this is a bit verbose though, sorry!
--     loss_fn = mem.cache(eval_loss)

--     # sole variation operator: mutation
--     # TODO: also add recombination
--     def mutate_comb(dct):
--         k = random.choice(list(options.keys()))
--         v = random.choice(options[k])
--         # TODO: extend to int (np.random.randint) / float (np.random.uniform)
--         return frozendict({**dct, k:v})

--     random_comb = lambda: frozendict({k: random.choice(opts) for k, opts in options.items()})

--     def comb_kv(dct):
--         return (dct, loss_fn(dct))

--     make_population = lambda n: dict([comb_kv(random_comb()) for i in range(n)])

--     # best in front as we ascendingly sort by loss
--     ranking = lambda population: sorted(population.items(), key=lambda tpl: tpl[1])

--     # quartiles get 2/1/1/0 mutations.
--     # so the upper quartile eats the bottom one.
--     # hopefully this makes it more robust against deduped populations!
--     def variation_selection(rnking, n):
--         qrt = int(n/4)
--         ks = [tpl[0] for tpl in rnking]
--         return dict(zip(ks, [
--             *([2]*qrt),
--             *([1]*qrt),
--             *([1]*qrt),
--             *([0]*qrt),
--         ]))

--     # all members replaced by mutants eached round
--     def next_population(population, n):
--         rnking = ranking(population)
--         num_mutations = variation_selection(rnking, n)
--         # anyone from before dies now, sorry!
--         return dict([comb_kv(mutate_comb(comb)) for comb, n in num_mutations.items() for i in range(n)])

--     # steady population of n members
--     def run_ea(n, rounds):
--         assert n % 4 == 0  # selection quartiles need this as my rounding is naive
--         population = make_population(n)
--         # termination condition: maximum number of rounds reached
--         for i in range(rounds):
--             # print(f'round {i}')
--             population = next_population(population, n)
--             # TODO: track champion
--         return ranking(population)[0]

--     # this function is the only public interface we need
--     return run_ea

-- # best_member, best_loss = get_ea({'a':[0,1,2],'b':[0,1,2]}, lambda d: d['a'] + d['b'])(n=4, rounds=5)
-- # assert best_loss <= 1
