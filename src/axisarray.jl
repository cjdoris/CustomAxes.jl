### AXIS ARRAY

"""
    AxisArray(array, axes)

An `AxisArray` wrapping the given `array` with the given `axes`.
"""
struct AxisArray{T,N,A<:AbstractArray{T,N},X<:NTuple{N,AxisLike}} <: AbstractAxisArray{T,N}
    parent :: A
    axes :: X
    function AxisArray(arr::AbstractArray{T,N}, _axes::Vararg{AxisLike,N}) where {T,N}
        check_api(_axes...)
        axes(arr) == _axes || error("axes mismatch")
        new{T,N,typeof(arr),typeof(_axes)}(arr, _axes)
    end
end

AxisArray(arr::AxisArray{T,N}, axes::Vararg{AxisLike,N}) where {T,N} = AxisArray(parent(arr), axes...)

function showarg(io::IO, x::AxisArray, toplevel)
    print(io, "AxisArray(")
    showarg(io, parent(x), false)
    print(io, ", ...)")
end

unwrap_indices_for_parent(x) = x
unwrap_indices_for_parent(x::AxisArray) =
    AxisArray(parent(x), map(unwrap_for_similar, axes(x))...)
unwrap_indices_for_parent(x::Base.Slice) =
    Base.Slice(unwrap_for_similar(x.indices))

AxisArray(parent::AbstractArray{T,N}, _axes::Vararg{Any,N}) where {T,N} =
    AxisArray(parent, map((a,b) -> a===nothing ? b : to_axis(a), _axes, axes(parent)))

parent(x::AbstractAxisArray) = getfield(x, :parent)

size(x::AxisArray) = size(parent(x))

getindex_axisarray_impl(x::AxisArray, inds, axes) = AxisArray(getindex(parent(x), inds...), axes...)
getindex_axisarray_impl(x::AxisArray, inds, ::Tuple{}) = getindex(parent(x), inds...)
getindex(x::AxisArray, i::Int...) = getindex(parent(x), i...)

view_axisarray_impl(x::AxisArray, inds, axes) =
    AxisArray(view(parent(x), inds...), axes...)

view_axisarray_impl(x::AbstractAxisArray, inds, axes) =
    AxisArray(invoke(view, Tuple{AbstractArray, map(typeof, inds)...}, x, inds...), axes...)

setindex!(x::AxisArray, v, i::Int...) = setindex!(parent(x), v, i...)

Base.IndexStyle(x::AxisArray) = IndexStyle(parent(x))

function similar(a::AbstractAxisArray{T,N}, ::Type{T2}, dims::Tuple) where {T,N,T2}
    axes = map(to_axis, dims)
    AxisArray(similar(Array{T2,length(dims)}, map(unwrap_for_similar, axes)), axes...)
end
function similar(a::AxisArray{T,N}, ::Type{T2}, dims::Tuple) where {T,N,T2}
    axes = map(to_axis, dims)
    AxisArray(similar(parent(a), T2, map(unwrap_for_similar, axes)), axes...)
end
similar(a::AbstractAxisArray, ::Type{T}, dims::Tuple{Vararg{Int}}) where {T} =
    invoke(similar, Tuple{typeof(a), Type{T}, Tuple}, a, T, dims)
similar(a::AxisArray, ::Type{T}, dims::Tuple{Vararg{Int}}) where {T} =
    invoke(similar, Tuple{typeof(a), Type{T}, Tuple}, a, T, dims)

dropdims_axisarray_impl(x::AxisArray, dims, newaxes) =
    AxisArray(dropdims(parent(x); dims=dims), newaxes...)
