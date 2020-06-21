### ABSTRACT AXIS ARRAY

"""
    AbstractAxisArray{T,N} <: AbstractArray{T,N}

Abstract type of arrays supporting an extended interface.

In particular, these have default support for `AbstractAxis` and support richer indexing behaviour.

Array types using `AbstractAxis` do not have to be a sub type of this.
"""
abstract type AbstractAxisArray{T,N} <: AbstractArray{T,N} end

const AbstractAxisVector{T} = AbstractAxisArray{T,1}
const AbstractAxisMatrix{T} = AbstractAxisArray{T,2}

axes(x::AbstractAxisArray) = getfield(x, :axes)
size(x::AbstractAxisArray) = map(length, axes(x))


## DROPDIMS
# Julia currently assumes that all axes of an array are the same type.

Base._dropdims(x::AbstractAxisArray, ds) = dropdims_axisarray(x, ds)
Base._dropdims(x::AbstractAxisArray, d::Integer) = dropdims_axisarray(x, d)

dropdims_axisarray(x, ds) =
    dropdims_axisarray(x, finddims(ds)::Base.Dims)

function dropdims_axisarray(x, ds::Base.Dims)
    for d in ds
        1 ≤ d ≤ ndims(x) || throw(ArgumentError("dropped dims must be in range 1:ndims(A)"))
        size(x, d) == 1 || throw(ArgumentError("dropped dims must all be size 1"))
    end
    newaxes = filter(x -> x !== nothing, ntuple(d -> d in ds ? nothing : axes(x, d), ndims(x)))
    reshape(x, newaxes)
end


Base.reduced_indices(x::AbstractAxisArray, d) = reduced_indices_axisarray(x, d)

reduced_indices_axisarray(x, d::Integer) =
    reduced_indices_axisarray(x, (d,))

function reduced_indices_axisarray(x, ds)
    all(d->d≥1, ds) || throw(ArgumentError("dimensions must be ≥ 1, got $ds"))
    ntuple(Val(ndims(x))) do d
        ax = axes(x, d)
        d in ds ? Base.reduced_index(ax) : ax
    end
end

show(io::IO, m::MIME"text/plain", x::AbstractAxisArray) =
    show_axisarray(io, m, x)
show(io::IO, m::MIME"text/plain", x::SubArray{T,N,A}) where {T,N,A<:AbstractAxisArray} =
    show_axisarray(io, m, x)

include("show.jl")
