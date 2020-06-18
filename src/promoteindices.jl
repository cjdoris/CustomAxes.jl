# TODO: promote indices in reshape(x, inds) too

# TYPE PIRACY
function similar(::Type{T}, inds::Tuple) where {T<:AbstractArray}
    newinds = promote_indices(inds...)
    typeof(newinds) === typeof(inds) && error("indices promotion did nothing")
    Base.similar(T, newinds...)
end

# TYPE PIRACY
function reshape(x::AbstractArray, inds::Tuple)
    newinds = promote_indices(inds...)
    typeof(newinds) === typeof(inds) && error("indices promotion did nothing")
    reshape(x, newinds)
end

function promote_indices(inds...)
    newinds = promote_indices_once(inds...)
    typeof(newinds) === typeof(inds) && return inds
    promote_indices(newinds...)
end

promote_indices_once() = ()
promote_indices_once(x) = (x,)
function promote_indices_once(a, b)
    aa, bb = promote_indices_rule(a, b)
    typeof((aa, bb)) !== typeof((a, b)) && return (aa, bb)
    bb, aa = promote_indices_rule(b, a)
    typeof((aa, bb)) !== typeof((a, b)) && return (aa, bb)
    (a, b)
end
function promote_indices_once(a, b, c)
    aa, bb = promote_indices_rule(a, b)
    typeof((aa, bb)) !== typeof((a, b)) && return (aa, bb, c)
    bb, aa = promote_indices_rule(b, a)
    typeof((aa, bb)) !== typeof((a, b)) && return (aa, bb, c)
    aa, cc = promote_indices_rule(a, c)
    typeof((aa, cc)) !== typeof((a, b)) && return (aa, b, cc)
    cc, aa = promote_indices_rule(c, a)
    typeof((aa, cc)) !== typeof((a, b)) && return (aa, b, cc)
    bb, cc = promote_indices_rule(b, c)
    typeof((bb, cc)) !== typeof((a, b)) && return (a, bb, cc)
    cc, bb = promote_indices_rule(c, b)
    typeof((bb, cc)) !== typeof((a, b)) && return (b, bb, cc)
    (a, b, c)
end

promote_indices_rule(a::T, b::T) where {T} = (a, b)
promote_indices_rule(a, b) = (a, b)
