import Pkg; Pkg.activate(".")
using Revise

import Tokenize
const TM = Tokenize
const TS = Tokenize.Tokens

using Caesar

#%%
env = Caesar.SchemeEnvironment()
Caesar.extend!(env, :+, +)
Caesar.extend!(env, :-, -)
Caesar.extend!(env, :*, *)
Caesar.extend!(env, :display, print)
Caesar.extend!(env, :newline, () -> println())
Caesar.extend!(env, Symbol("eq?"), ==)

open("scheme\\ex02.scm", "r") do io
   code = read(io, String)
   @show pt = tokenize(code) |> parse
   Caesar.interp!(env, pt)
end
