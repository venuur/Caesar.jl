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
env = Caesar.SchemeEnvironment()
Caesar.extend!(env, :+, +)
Caesar.extend!(env, :display, print)
Caesar.extend!(env, :newline, () -> println())
@show Caesar.interp!(env, pt)

#%%
open("scheme\\ex01.scm", "r") do io
   code = read(io, String)
   @show pt = tokenize(code) |> parse
   Caesar.interp!(env, pt)
end
