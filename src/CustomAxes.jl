module CustomAxes

using Crayons, Random

import Base: axes, length, size, parent, first, last, getindex, setindex!, similar, reshape, to_indices, show, summary, showarg, convert

export AbstractAxisArray, AxisArray, AbstractAxis, SimpleAxis, LabeledAxis, AxisLabels, labeledarray

include("promote_indices.jl")

function to_indices_and_axes(a, args...)
    inds = to_indices(a, args)
    axes = Base.index_shape(inds...)
    inds, axes
end

finddims(dims::Tuple) = map(finddim, dims)
finddims(dims) = finddims((dims,))

finddim(d::Integer) = convert(Int, d)

### AXIS API

# All axes are axis-like, but we additionally require that axes(a)==(a,). See check_api(a).
const AxisLike{I<:Integer} = AbstractUnitRange{I}

to_axis(a::Integer) = Base.OneTo(a)
to_axis(a::AxisLike) = a

unwrap(a::AxisLike) = a

unwrap_for_similar(a::AxisLike) = a

secondarykeytype(::Type{<:AxisLike}) = Union{}
secondarykeytype(a::AxisLike) = secondarykeytype(typeof(a))

check_api(::Type{Bool}, a::AxisLike) = axes(a)==(a,)
check_api(::Type{Bool}, a::Base.OneTo) = true
check_api(::Type{Bool}, a::Base.IdentityUnitRange) = true

check_api(::Type{Bool}, a::AxisLike, bs::AxisLike...) =
    check_api(Bool, a) && check_api(Bool, bs...)

check_api(::Type{Bool}) = true

check_api() = nothing

check_api(a::AxisLike) =
    check_api(Bool, a) ? nothing : error("axis does not satisfy required API")

check_api(a::AxisLike, b::AxisLike, cs::AxisLike...) =
    check_api(Bool, a, b, cs...) ? nothing : error("axes do not satisfy required API")

getlabel(x::AxisLike, i) = nothing

labeltype(x::AxisLike) = typeof(getlabel(x, firstindex(x)))

describe(io, x::AxisLike) = print(io, first(x), ":", last(x))

colorhash(ax::AxisLike, h) = h

### ABSTRACT AXIS ARRAY

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

### AXIS ARRAY

struct AxisArray{T,N,A<:AbstractArray{T,N},X<:NTuple{N,AxisLike}} <: AbstractAxisArray{T,N}
    parent :: A
    axes :: X
    function AxisArray(arr::AbstractArray{T,N}, _axes::Vararg{AxisLike,N}) where {T,N}
        arr isa AxisArray && return AxisArray(parent(arr), _axes...)
        check_api(_axes...)
        axes(arr) == _axes || error("axes mismatch")
        new{T,N,typeof(arr),typeof(_axes)}(arr, _axes)
    end
end

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

getlabel(x::AbstractAxis, i) = getlabel(parent(x), i)

Base.IdentityUnitRange(x::AbstractAxis) = x

Base.reduced_index(x::AbstractAxis) = Base.reduced_index(parent(x))

similaraxis(x::AbstractAxis, i) = similaraxis(x, i, axes(parent(x)[unwrap_indices_for_parent(i)], 1))

convert(::Type{T}, x) where {T<:AbstractAxis} = throw(MethodError(convert, (T, x)))
convert(::Type{T}, x::T) where {T<:AbstractAxis} = x

### SIMPLE AXIS

struct SimpleAxis{S,I,Is<:AxisLike{I}} <: AbstractAxis{S,I}
    parent :: Is
    function SimpleAxis(parent::AxisLike)
        check_api(parent)
        S = secondarykeytype(parent)
        new{S,eltype(parent),typeof(parent)}(parent)
    end
end

function showarg(io::IO, a::SimpleAxis, top)
    print(io, "SimpleAxis(")
    showarg(io, parent(a), false)
    print(io, ")")
end

convert(::Type{SimpleAxis{S,I,Is}}, a::SimpleAxis) where {S,I,Is} =
    SimpleAxis(convert(Is, parent(a)))

### LABELED AXIS

struct LabeledAxis{L,I,Ls<:AbstractVector,Is<:AxisLike,S} <: AbstractAxis{S,I}
    labels :: Ls
    parent :: Is
    function LabeledAxis(labels::AbstractVector, parent::AxisLike=axes(labels, 1), L::Type=find_secondarykeytype(eltype(labels)))
        check_api(parent)
        axes(labels, 1) == parent || error("axis mismatch")
        S = Union{secondarykeytype(parent), L}
        check_secondarykeytype(S)
        new{L,eltype(parent),typeof(labels),typeof(parent),S}(labels, parent)
    end
end

function describe(io::IO, a::LabeledAxis)
    describe(io, parent(a))
    print(io, " with labels ")
    show(IOContext(io, :limit=>true, :compact=>true), rawlabels(a))
end

function showarg(io::IO, a::LabeledAxis, top)
    print(io, "LabeledAxis(")
    showarg(io, rawlabels(a), false)
    print(io, ", ")
    showarg(io, parent(a), false)
    print(io, ")")
end

rawlabels(a::AxisLike) = typeof(a)===typeof(unwrap(a)) ? missing : rawlabels(unwrap(a))
rawlabels(a::LabeledAxis) = getfield(a,:labels)

similaraxis(a::LabeledAxis{L}, i, b) where {L} =
    LabeledAxis(rawlabels(a)[unwrap_indices_for_parent(i)], b, L)

secondary_to_index(ax::LabeledAxis{L}, i::L) where {L} =
    find_label(rawlabels(ax), i)

secondary_to_index(ax::LabeledAxis{L}, i::AbstractVector{<:L}) where {L} =
    find_labels(rawlabels(ax), i)

function find_label(ks, k)
    i = findfirst(==(k), ks)
    i===nothing ? error("no such label: $(repr(k))") : i
end
function find_label(ks::AbstractRange, k)
    i = searchsortedlast(ks, k)
    checkbounds(Bool, ks, i) && @inbounds(ks[i])==k ? i : error("no such label: $(repr(k))")
end
find_labels(ks, k) = map(k->find_label(ks, k), k)
function find_labels(ks::AbstractRange, k::AbstractRange)
    a = find_label(ks, first(k))
    b = find_label(ks, last(k))
    s, r = divrem(step(k), step(ks))
    iszero(r) ? (a:s:b) : error("no such label: $(repr(k[a+r]))")
end

getlabel(ax::LabeledAxis, i) = rawlabels(ax)[i]

convert(::Type{LabeledAxis{L,I,Ls,Is,S}}, a::LabeledAxis) where {L,I,Ls,Is,S} =
    LabeledAxis(convert(Ls, rawlabels(a)), convert(Is, parent(a)), L)

colorhash(ax::LabeledAxis, h) = colorhash(parent(ax), hash(eltype(rawlabels(ax)), h))


### ACCESS LABELS

struct AxisLabels{T, A<:AxisLike} <: AbstractAxisArray{T,1}
    axis :: A
    function AxisLabels(ax::AxisLike)
        check_api(ax)
        new{labeltype(ax), typeof(ax)}(ax)
    end
end

AxisLabels(args...) = AxisLabels(LabeledAxis(args...))

axes(x::AxisLabels) = (getfield(x, :axis),)
length(x::AxisLabels) = length(axes(x,1))
size(x::AxisLabels) = (length(x),)
getindex(x::AxisLabels, i) = getlabel(axes(x,1), i)
function showarg(io::IO, x::AxisLabels, top)
    print(io, "AxisLabels(...)")
end


### TODO: RESHAPED ARRAY

### TODO: SUB-ARRAY

### SUGAR

labeledarray(arr::AbstractArray{T,N}, labels::Vararg{AbstractVector, N}) where {T,N} =
    AxisArray(arr, map(LabeledAxis, labels)...)

labeledarray(A::Type{<:AbstractArray{T,N}}, labels::Vararg{AbstractVector, N}) where {T,N} =
    similar(A, map(LabeledAxis, labels))

labeledarray(T::Type, init::Union{UndefInitializer,Nothing,Missing}, labels::Vararg{AbstractVector, N}) where {N} =
    labeledarray(Array{T,N}(init, map(length, labels)), labels...)

# #### KEYS AND INDICES

# """
#     Key(x)

# Forces `x` to be interpreted as a key when used as an index.
# """
# struct Key{T}
#     value :: T
# end

# value(x::Key) = getfield(x, :value)

# """
#     Named{name}(x)

# Forces `x` to be interpreted as a named value when used as an index.
# """
# struct Named{name,T}
#     value :: T
# end

# value(x::Named) = getfield(x, :value)

# const NamedKey{name,T} = Named{name,Key{T}}


# #### AXIS

# const AxisLike{V<:Integer} = AbstractUnitRange{V}

# """
#     AbstractAxis{S, V} <: AbstractUnitRange{V}

# Abstract super type for all axis kinds, with values of type `V`.
# """
# abstract type AbstractAxis{S, V<:Integer} <: AbstractUnitRange{V} end

# axes(ax :: AbstractAxis) = (ax,)
# parent(ax :: AbstractAxis) = axisparent(ax)
# first(ax :: AbstractAxis) = first(parent(ax))
# last(ax :: AbstractAxis) = last(parent(ax))

# axisunwrap(ax :: AbstractAxis) = axisunwrap(parent(ax))
# axisunwrap(ax :: AbstractUnitRange{<:Integer}) = ax


# Base.IdentityUnitRange(ax::AbstractAxis) = ax

# """
#     WrappedAxis(axis :: AbstractUnitRange{<:Integer})

# An axis with values from `axis`.
# """
# struct WrappedAxis{V, Vs<:AbstractUnitRange{V}} <: AbstractAxis{Union{}, V}
#     parent :: Vs
#     function WrappedAxis(ax :: AbstractUnitRange{V}) where {V<:Integer}
#         axes(ax, 1) == ax || error("axes don't match")
#         new{V,typeof(ax)}(ax)
#     end
#     WrappedAxis(ax::AbstractAxis) = error("refusing to wrap what is already an axis")
# end

# axisparent(ax::WrappedAxis) = getfield(ax, :parent)
# to_axis(ax::AbstractAxis) = ax
# to_axis(ax::AbstractUnitRange{<:Integer}) = WrappedAxis(ax)
# to_axis(n::Integer) = to_axis(Base.OneTo(n))
# similaraxis(ax::WrappedAxis, i, v) = WrappedAxis(axes(v,1))

# """
#     AbstractKeyedAxis{T, K, S, V} <: AbstractAxis{S, V}

# Abstract super type for axes with keys (or secondary values) of type `K`.
# """
# abstract type AbstractKeyedAxis{SK, K, S, V} <: AbstractAxis{S, V} end

# axiskeytype(::Type{T}) where {SK,K,S,V,T<:AbstractKeyedAxis{SK,K,S,V}} = K

# """
#     KeyedAxis([S], keys, axis)

# A keyed axis equal to `axis` with given `keys`.
# """
# struct KeyedAxis{SK, K, S, V, Ks<:AbstractVector{K}, Vs<:AbstractAxis{S0,V} where S0} <: AbstractKeyedAxis{SK, K, S, V}
#     keys :: Ks
#     parent :: Vs
#     function KeyedAxis(::Type{SK}, keys :: AbstractVector{K}, axis :: AbstractAxis{S,V}) where {SK,K,S,V}
#         check_supertype(SK)
#         axes(keys, 1) == axis || error("given axis must be the axis of the given keys")
#         new{SK,K,Union{SK,S},V,typeof(keys),typeof(axis)}(keys, axis)
#     end
# end

# KeyedAxis(keys::AbstractVector, axis::Union{AbstractAxis,AbstractUnitRange{<:Integer}}=axes(keys,1)) = KeyedAxis(find_supertype(eltype(keys)), keys, to_axis(axis))

# axisparent(ax::KeyedAxis) = getfield(ax, :parent)
# axiskeys(ax::KeyedAxis) = getfield(ax, :keys)
# similaraxis(ax::KeyedAxis, i, v) = KeyedAxis(axiskeys(ax)[i], axes(v,1))

# key_value_to_index(keys, key) = scalar_key_value_to_index(keys, key)

# function scalar_key_value_to_index(keys, key)
#     i = findfirst(==(key), keys)
#     i === nothing ? error("key not found: $(repr(key))") : i
# end

# function scalar_key_value_to_index(keys::AbstractRange, key)
#     i = searchsortedlast(key, keys)
#     checkbounds(Bool, keys, i) && keys[i] == key ? i : error("key not found: $(repr(key))")
# end

# key_value_to_index(keys, key::AbstractArray) = map(x->scalar_key_value_to_index(keys, x), key)

# key_value_to_index(keys, key::Function) = findall(key, keys)

# function keyed_expr(ex)
#     if ex isa Expr
#         args = map(keyed_expr, ex.args)
#         if ex.head == :ref
#             args = [i==1 ? a : :($Key($a)) for (i,a) in enumerate(args)]
#         end
#         return Expr(ex.head, args...)
#     else
#         return ex
#     end
# end

# macro keyed(ex)
#     return esc(keyed_expr(ex))
# end

# """
#     AbstractNamedAxis{S, V} <: AbstractAxis{S, V}

# Abstract super type for axes which may be referred to by name.
# """
# abstract type AbstractNamedAxis{S, V} <: AbstractAxis{S, V} end



# ### CONVERSION TO INDICES



end # module
