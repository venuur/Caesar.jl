module Caesar

import Tokenize
const TM = Tokenize
const TS = Tokenize.Tokens

using ReplMaker: initrepl

export tokenize, parse, interp!, interp_sexp_string!, enable_repl

struct SchemeToken
    kind
    startpos
    endpos
    startbyte
    endbyte
    val
end
kind(t::SchemeToken) = t.kind
untokenize(t::SchemeToken) = t.val

@enum(
    Kind,
    ENDMARKER, # EOF
    LPAREN,
    RPAREN,
    SYMBOL,
    INTEGER,
    FLOAT,
    STRING,
    CHAR,
    WHITESPACE,
)

function scheme_kind(tokenize_kind::TS.Kind)
    if tokenize_kind === TS.INTEGER
        return INTEGER
    elseif tokenize_kind === TS.FLOAT
        return FLOAT
    elseif tokenize_kind === TS.LPAREN
        return LPAREN
    elseif tokenize_kind === TS.RPAREN
        return RPAREN
    elseif tokenize_kind === TS.STRING
        return STRING
    elseif tokenize_kind === TS.CHAR
        return CHAR
    elseif tokenize_kind === TS.WHITESPACE
        return WHITESPACE
    elseif tokenize_kind === TS.ENDMARKER
        return ENDMARKER
    else
        return SYMBOL
    end
end


function tokenize(string)
    tokens = TM.tokenize(string)
    raw_tokens = []
    last_token = nothing
    for t in tokens
        if is_identifier_part(TS.kind(t))
            if is_identifier_part(TS.kind(last_token))
                push!(raw_tokens[end], t)
            else
                push!(raw_tokens, [t])
            end
        else
            push!(raw_tokens, t)
        end
        last_token = t
    end

    scheme_tokens = Array{SchemeToken}(undef, length(raw_tokens))
    for i in eachindex(raw_tokens)
        t = raw_tokens[i]
        if t isa TS.Token
            scheme_tokens[i] = SchemeToken(
                scheme_kind(TS.kind(t)),
                TS.startpos(t),
                TS.endpos(t),
                TS.startbyte(t),
                TS.endbyte(t),
                TS.untokenize(t),
            )
        else
            # We have a list of tokens that should become a Symbol.
            val = join(map(TS.untokenize, t))
            scheme_tokens[i] = SchemeToken(
                SYMBOL,
                TS.startpos(t[1]),
                TS.endpos(t[end]),
                TS.startbyte(t[1]),
                TS.endbyte(t[end]),
                val,
            )
        end
    end
    return scheme_tokens
end

function is_identifier_part(tokenize_kind)
    return scheme_kind(tokenize_kind) === SYMBOL
end

struct SchemeData{T}
    val::T
    token::Union{SchemeToken,Nothing}
end
value(s::SchemeData) = s.val

Base.show(io::IO, s::SchemeData) = show(io, s.val)

struct SExpr
    terms::Array{Union{SchemeData,SExpr}}
end
car(s::SExpr) = s.terms[1]
cdr(s::SExpr) = SExpr(@view s.terms[2:end])
cadr(s::SExpr) = car(cdr(s))
cddr(s::SExpr) = cdr(cdr(s))
caddr(s::SExpr) = car(cdr(cdr(s)))
cadddr(s::SExpr) = car(cdr(cdr(cdr(s))))
null(s::SExpr) = length(s.terms) == 0
Base.length(s::SExpr) = length(s.terms)
Base.push!(s::SExpr, a) = push!(s.terms, a)
Base.iterate(s::SExpr) = iterate(s.terms)
Base.iterate(s::SExpr, i::Int) = iterate(s.terms, i)

struct SchemeEnvironment
    bindings::Dict{Symbol, Any}
    parent::Union{SchemeEnvironment, Nothing}
end
SchemeEnvironment() = SchemeEnvironment(Dict{Symbol, Any}(), nothing)
extend!(env::SchemeEnvironment, name, value) = env.bindings[name] = value

function Base.get(env::SchemeEnvironment, name)
    if haskey(env.bindings, name)
        return env.bindings[name]
    elseif env.parent !== nothing
        return get(env.parent, name)
    else
        throw(ArgumentError("Unbound variable `$(name)`."))
    end
end

SchemeEnvironment(parent::SchemeEnvironment) = SchemeEnvironment(Dict{Symbol, Any}(), parent)

struct SchemeProcedure
    env::SchemeEnvironment
    formals::Union{Array{Symbol}, Symbol}
    body::SExpr
    SchemeProcedure(parent::SchemeEnvironment, formals, body) = new(
        SchemeEnvironment(parent), formals, body)
end
function (proc::SchemeProcedure)(args...)
    if proc.formals isa Symbol
        # varargs
        extend!(proc.env, proc.formals, args)
    else
        for (variable, value) in zip(proc.formals, args)
            extend!(proc.env, variable, value)
        end
    end
    return interp!(proc.env, proc.body)
end

function Base.show(io::IO, s::SExpr)
    write(io, "(")
    n = length(s.terms)
    for (i, t) in enumerate(s.terms)
        show(io, t)
        i < n && write(io, " ")
    end
    write(io, ")")
end

function Base.parse(tokens::Array{SchemeToken})
    toplevel = SExpr([SchemeData(:begin, nothing)])
    prev = []
    current = toplevel
    for t in tokens
        if kind(t) === WHITESPACE
            continue
        elseif kind(t) === LPAREN
            push!(prev, current)
            new = SExpr(SchemeData[])
            push!(current, new)
            current = new
        elseif kind(t) === RPAREN
            current = pop!(prev)
        elseif kind(t) === ENDMARKER
            break
        else
            push!(current, parse(t))
        end
    end
    return toplevel
end

function Base.parse(token::SchemeToken)
    if kind(token) === SYMBOL
        return SchemeData(Symbol(untokenize(token)), token)
    elseif kind(token) === INTEGER
        return SchemeData(parse(Int, untokenize(token)), token)
    elseif kind(token) === FLOAT
        return SchemeData(parse(Float64, untokenize(token)), token)
    elseif kind(token) === STRING
        return SchemeData(strip(untokenize(token), '\"'), token)
    elseif kind(token) === CHAR
        return SchemeData(first(strip(untokenize(token), '\'')), token)
    else
        throw(ArgumentError("Unknown token kind found: `$(token)`"))
    end
end

function interp!(env::SchemeEnvironment, s::SExpr)
    # @show env s
    result = nothing
    if null(s)
        return nothing
    end

    head = car(s)
    if typeof(head) !== SchemeData{Symbol}
        throw(ArgumentError("Invalid s-expression. Must begin with symbol. Found `$(head)`"))
    end

    head = value(head)
    if head === :define
        result = interp_define!(env, s)
    elseif head === :begin
        result = interp_begin!(env, s)
    elseif head === :lambda
        result = interp_lambda!(env, s)
    elseif head === :if
        result = interp_if!(env, s)
    else
        # must be a procedure call
        result = interp_call!(env, s)
    end
    return result
end

function interp!(env::SchemeEnvironment, s::SchemeData)
    # @show env s
    if typeof(s) === SchemeData{Symbol}
        return get(env, value(s))
    else
        # literal value
        return value(s)
    end
end

function interp_define!(env::SchemeEnvironment, s::SExpr)
    if length(s) != 3
        throw(ArgumentError("`define` expression must have exactly 3 terms `(define <var> <value>)`."))
    end

    variable = cadr(s)
    if typeof(variable) != SchemeData{Symbol}
        throw(ArgumentError("`<var>` in `(define <var> <value>)` must be an identifier. Got `$(variable)`"))
    end
    variable = value(variable)
    result = interp!(env, caddr(s))
    extend!(env, variable, result)
    return nothing
end

function interp_begin!(env::SchemeEnvironment, s::SExpr)
    if length(s) < 1
        throw(ArgumentError("`begin` expression must have form `(begin <expr>*)`."))
    end

    result = nothing
    for expr in cdr(s)
        result = interp!(env, expr)
    end
    return result
end

function interp_lambda!(env::SchemeEnvironment, s::SExpr)
    if length(s) < 3
        throw(ArgumentError(
            "`lambda` expression must have form `(lambda <formals> <body-expr>*)`."
        ))
    end

    function _push_formal!(f, v, err)
        if v isa SchemeData{Symbol}
            push!(f, value(v))
        else
            throw(ArgumentError(err))
        end
    end

    formals = Symbol[]
    formals_expr = cadr(s)
    if formals_expr isa SchemeData{Symbol}
        # (lambda <var> <body>*)
        _push_formal!(formals, formals_expr, "`<formals>` must be a symbol.")
    else
        # (lambda (<var>*) <body>*)
        for var in formals_expr
            _push_formal!(formals, var, "`<formals>` must be a list of symbols.")
        end
    end
    body = SExpr([SchemeData(:begin, nothing), cddr(s)...])
    return SchemeProcedure(env, formals, body)
end

function interp_if!(env::SchemeEnvironment, s::SExpr)
    if length(s) < 3 || length(s) > 4
        throw(ArgumentError(
            "`if` expression must have form `(if <pred> <true-case> [<false-case>])`."
        ))
    end

    pred = cadr(s)
    pred_value = interp!(env, pred)
    if pred_value
        true_case = caddr(s)
        return interp!(env, true_case)
    else
        if length(s) == 4
            false_case = cadddr(s)
            return interp!(env, false_case)
        end
    end

    return nothing
end

function interp_call!(env::SchemeEnvironment, s::SExpr)
    procedure = interp!(env, car(s))
    args = [interp!(env, expr) for expr in cdr(s)]
    result = procedure(args...)
    return result
end

interp_sexp_string!(env, sexp_str) = interp!(env, parse(tokenize(sexp_str)))

function init_env!(env)
    Caesar.extend!(env, :+, +)
    Caesar.extend!(env, :-, -)
    Caesar.extend!(env, :*, *)
    Caesar.extend!(env, :display, print)
    Caesar.extend!(env, :newline, () -> println())
    Caesar.extend!(env, Symbol("eq?"), ==)
end

function enable_repl()
    env = Caesar.SchemeEnvironment()
    init_env!(env)

    initrepl((x) -> interp_sexp_string!(env, x);
        prompt_text="caesar> ",
        prompt_color=:blue,
        start_key=')',
        mode_name="Caesar_mode",
    )
end

end # module
