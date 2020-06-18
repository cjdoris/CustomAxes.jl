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

Base.getindex(x::AbstractAxisArray, args...; opts...) = getindex_axisarray(x, args, opts)

function getindex_axisarray(x, args, opts=())
    inds, axes = to_indices_and_axes(x, args...; opts...)
    inds = map(unwrap_indices_for_parent, inds)
    getindex_axisarray_impl(x, inds, axes)
end

Base.view(x::AbstractAxisArray, args...; opts...) = view_axisarray(x, args, opts)

function view_axisarray(x, args, opts=())
    inds, axes = to_indices_and_axes(x, args...; opts...)
    inds = map(unwrap_indices_for_parent, inds)
    view_axisarray_impl(x, inds, axes)
end

Base.dropdims(x::AbstractAxisArray; dims) = dropdims_axisarray(x, dims)

function dropdims_axisarray(x, dims)
    dims = finddims(dims)
    newaxes = filter(a -> a !== nothing, ntuple(d -> d in dims ? nothing : axes(x, d), ndims(x)))
    dropdims_axisarray_impl(x, dims, newaxes)
end

Base.reduced_indices(x::AbstractAxisArray, d::Int) = reduced_indices_axisarray(x, d)

function reduced_indices_axisarray(x, d)
    d ≥ 1 || throw(ArgumentError("dimension must be ≥ 1, got $d"))
    ntuple(ndims(x)) do dd
        ax = axes(x, dd)
        d == dd ? Base.reduced_index(ax) : ax
    end
end

# TODO: the above for similar
# TODO: the above for reshape

include("show.jl")
