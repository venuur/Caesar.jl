import Pkg; Pkg.activate(".")
using Revise

import Tokenize
const TM = Tokenize
const TS = Tokenize.Tokens

using Caesar

#%%
env = Caesar.SchemeEnvironment()
Caesar.init_env!(env)
open("scheme\\ex03.scm", "r") do io
   code = read(io, String)
   @show pt = tokenize(code) |> parse
   Caesar.interp!(env, pt)
end
