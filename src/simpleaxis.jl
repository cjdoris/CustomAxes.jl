### SIMPLE AXIS

"""
    SimpleAxis(axis)

Just a wrapper for `axis` that ensures the axis API is satisfied.

This is used for axis type promotion (see `promoteindices.jl`).
"""
struct SimpleAxis{S,I,Is<:AxisLike{I}} <: AbstractAxis{S,I}
    parent :: Is
    function SimpleAxis(parent::AxisLike)
        check_api(parent)
        S = secondarykeytype(parent)
        new{S,eltype(parent),typeof(parent)}(parent)
    end
end

SimpleAxis(ax::SimpleAxis) = ax

function showarg(io::IO, a::SimpleAxis, top)
    print(io, "SimpleAxis(")
    showarg(io, parent(a), false)
    print(io, ")")
end

convert(::Type{SimpleAxis{S,I,Is}}, a::SimpleAxis) where {S,I,Is} =
    SimpleAxis(convert(Is, parent(a)))
