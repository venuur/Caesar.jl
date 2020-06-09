import Pkg; Pkg.activate(".")
using Revise

import Tokenize
const TM = Tokenize
const TS = Tokenize.Tokens

using Caesar

#%%
@show st = tokenize("""
(define x 3)
(+ x 2)
""")
@show pt = parse(st)
env = Ceasar.SchemeEnvironment()
Ceasar.extend!(env, :+, +)
Juno.@enter Ceasar.interp!(env, pt)
