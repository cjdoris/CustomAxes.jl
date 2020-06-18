### ABSTRACT AXIS

"""
    AbstractAxis{S,I} <: AxisLike{I}

Abstract type for axes supporting a rich version of the `AxisLike` interface.

The parameter `S` is the element type of an optional "secondary axis", the axis can be indexed with scalars or vectors of type `S`.

# Invariants
* `axes(a) = (a,)`
* `axes(parent(a),1) == parent(a) == a` (this only implies they are equal *as axes*, so have the same limits; they can differ in other ways)
"""
abstract type AbstractAxis{S,I} <: AxisLike{I} end

secondarykeytype(::Type{A}) where {S,I,A<:AbstractAxis{S,I}} = S

parent(a::AbstractAxis) = getfield(a, :parent)

first(a::AbstractAxis) = first(parent(a))

last(a::AbstractAxis) = last(parent(a))

axes(a::AbstractAxis) = (a,)

size(a::AbstractAxis) = size(parent(a))

function describe(io::IO, a::AbstractAxis)
    describe(io, parent(a))
    print(io, ", wrapped as a ", typeof(a))
end

function getindex(a::AbstractAxis, i::Integer)
    checkbounds(a, i)
    convert(eltype(a), i)
end
function getindex(a::AbstractAxis, i)
    v = getindex(parent(a), unwrap_indices_for_parent(i))
    v isa Integer ? convert(eltype(a), v) : v isa AbstractVector ? AxisArray(v, similaraxis(a, i, axes(v,1))) : AxisArray(v, map(x->nothing, axes(v))...)
end
getindex(ax::AbstractAxis, i::AbstractUnitRange{<:Integer}) = invoke(getindex, Tuple{AbstractAxis,Any}, ax, i)
getindex(ax::AbstractAxis, i::StepRange{<:Integer}) = invoke(getindex, Tuple{AbstractAxis,Any}, ax, i)
getindex(ax::AbstractAxis{S}, i::S) where {S} = getindex(ax, secondary_to_index(ax, i))
getindex(ax::AbstractAxis{S}, i::AbstractArray{<:S}) where {S} = getindex(ax, secondary_to_index(ax, i))

secondary_to_index(ax::AbstractAxis{S}, i::S) where {S} = secondary_to_index(parent(ax), i)
secondary_to_index(ax::AbstractAxis{S}, i::AbstractArray{S}) where {S} = secondary_to_index(parent(ax), i)

lazygetindex(a::AbstractAxis, args...) = getindex(a, args...)

unwrap(a::AbstractAxis) = parent(a)

unwrap_for_similar(a::AbstractAxis) = unwrap(a)

check_secondarykeytype(::Type{Bool}, S::Type) =
    typeintersect(S, Union{Number, CartesianIndex, AbstractArray, Colon}) === Union{}
check_secondarykeytype(S::Type) =
    check_secondarykeytype(Bool, S) ? nothing : error("secondary key type may not intersect `Number`, `CartesianIndex`, `AbstractArray` or `Colon`")

find_secondarykeytype(T::Type, dflt::Type=Union{}) =
    check_secondarykeytype(Bool, T) ? find_secondarykeytype(supertype(T), T) : dflt

similar(::Type{A}, axes::NTuple{N,AbstractAxis}) where {T,N,A<:AbstractArray{T,N}} =
    AxisArray(similar(A, map(unwrap_for_similar, axes)), axes...)

reshape(x::AbstractArray, axes::Tuple{Vararg{AbstractAxis}}) =
    AxisArray(reshape(x, map(length, axes)), axes...)

to_indices(x::AbstractArray, axes::Tuple{AbstractAxis{S}, Vararg}, inds::Tuple{S, Vararg}) where {S} =
    (lazygetindex(axes[1], inds[1]), to_indices(x, axes[2:end], inds[2:end])...)

to_indices(x::AbstractArray, axes::Tuple{AbstractAxis{S}, Vararg}, inds::Tuple{AbstractArray{<:S}, Vararg}) where {S} =
    (lazygetindex(axes[1], inds[1]), to_indices(x, axes[2:end], inds[2:end])...)

to_indices(x::AbstractArray, axes::Tuple{AbstractAxis, Vararg}, inds::Tuple{AbstractVector, Vararg}) =
    (lazygetindex(axes[1], inds[1]), to_indices(x, axes[2:end], inds[2:end])...)

promote_indices_rule(x::AbstractAxis, y::AbstractAxis) = (x,y)
promote_indices_rule(x::AbstractAxis, y::AxisLike) = (x, SimpleAxis(y))

check_api(::Type{Bool}, a::AbstractAxis) = true

Base.IdentityUnitRange(x::AbstractAxis) = x

Base.reduced_index(x::AbstractAxis) = Base.reduced_index(parent(x))

similaraxis(x::AbstractAxis, i) = similaraxis(x, i, axes(parent(x)[unwrap_indices_for_parent(i)], 1))

convert(::Type{T}, x) where {T<:AbstractAxis} = throw(MethodError(convert, (T, x)))
convert(::Type{T}, x::T) where {T<:AbstractAxis} = x

show(io::IO, m::MIME"text/plain", x::AbstractAxis) = show_axisarray(io, m, x)
