"""
    rawlabels(axis)
    rawlabels(array, d)

The labels of `axis`, or `nothing` if it has no labels.

The second form returns the labels of the `d`th axis of `array`.

See also [`labels`](@ref).
"""
rawlabels(a::AxisLike) = typeof(a)===typeof(unwrap(a)) ? nothing : rawlabels(unwrap(a))
rawlabels(x::AbstractArray, d) = rawlabels(axes(x, d))

"""
    haslabels(axis)

True if the given axis has labels.
"""
haslabels(a::AxisLike) = rawlabels(a) !== nothing

"""
    getlabel(axis, i)

The label at index `i` of `axis`, or `nothing` if it does not have labels.
"""
getlabel(x::AxisLike, i) = nothing
getlabel(x::AbstractAxis, i) = getlabel(parent(x), i)

"""
    labeltype(axis)

The type of labels of `axis`.
"""
labeltype(x::AxisLike) = haslabels(x) ? eltype(rawlabels(x)) : Nothing

"""
    labels(axis)
    labels(array, d)

The labels of `axis` as an `AxisLabels <: AbstractAxisVector`, which has `axis` as its axis.

This is useful to preserve axis information, but see also [`rawlabels`](@ref).

The second form returns the labels of the `d`th axis of `array`.
"""
labels(ax::AxisLike) = _labels(ax, rawlabels(ax))
_labels(ax, ::Nothing) = ax
_labels(ax, labels::AbstractVector) = AxisLabels(ax)

labels(x::AbstractArray, d) = labels(axes(x,d))

### LABELED AXIS

"""
    LabeledAxis(labels, [axis, L])

An axis with the given `labels`.

This axis is equivalent to `axis`, which defaults to the axis of `labels`.

Indexing this axis with `i::L` will find the index whose label is `i`. By default `L` is the largest supertype of `eltype(labels)` which does not contain standard indexing types.
"""
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

LabeledAxis(labels::AbstractVector, L::Type) =
    LabeledAxis(labels, axes(labels, 1), L)

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

rawlabels(a::LabeledAxis) = getfield(a, :labels)

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


### AXIS LABELS

"""
    AxisLabels(axis)

A vector of labels of `axis`. The axis of this vector is `axis`.
"""
struct AxisLabels{T, A<:AxisLike} <: AbstractAxisArray{T,1}
    axes :: Tuple{A}
    function AxisLabels(ax::AxisLike)
        check_api(ax)
        new{labeltype(ax), typeof(ax)}((ax,))
    end
end

rawlabels(x::AxisLabels) = rawlabels(axes(x,1))

length(x::AxisLabels) = length(axes(x,1))
size(x::AxisLabels) = (length(x),)
getindex(x::AxisLabels, i::Int) = getlabel(axes(x,1), i)
function showarg(io::IO, x::AxisLabels, top)
    print(io, "AxisLabels(...)")
end
function print(io::IO, x::AxisLabels)
    print(io, rawlabels(x))
end
function show(io::IO, x::AxisLabels)
    if get(io, :typeinfo, Any) != typeof(x)
        print(io, "AxisLabels: ")
    end
    show(io, rawlabels(x))
end

### CARTESIAN LABELS
# (like cartesian indices, but for labels)
# TODO: Make a CartesianLabel type, and return these instead of tuples?

struct CartesianLabels{T,N,A<:NTuple{N,AxisLike}} <: AbstractAxisArray{T,N}
    axes :: A
    function CartesianLabels(axes::NTuple{N,AxisLike}) where {N}
        check_api(axes...)
        T = Tuple{map(labeltype, axes)...}
        new{T, N, typeof(axes)}(axes)
    end
end

CartesianLabels(x::AbstractArray) = CartesianLabels(axes(x))
size(x) = map(length, axes(x))
getindex(x::CartesianLabels{T,N}, inds::Vararg{Int, N}) where {T,N} =
    map(getlabel, axes(x), inds) :: T


### SUGAR

"""
    newlabels(labels, [axis])

Equivalent to `labels(LabeledAxis(...))`. A vector equivalent to `labels`, but whose axis also has the given labels.
"""
newlabels(args...) = labels(LabeledAxis(args...))

"""
    labeledarray(array, labels...)
    labeledarray(arraytype, labels...)
    labeledarray(eltype, init, labels...)

Shorthand constructors for an `AxisArray` with the given `labels` on its axes.

In the first form, the labels may be `nothing` to inherit the axis directly from `array` instead.

In the final form, `init` may be `undef`, `missing` or `nothing`.
"""
labeledarray(arr::AbstractArray{T,N}, labels::Vararg{AbstractVector, N}) where {T,N} =
    AxisArray(arr, map((lab,ax) -> lab===nothing ? ax : LabeledAxis(lab,ax), labels, axes(arr))...)

labeledarray(A::Type{<:AbstractArray{T,N}}, labels::Vararg{AbstractVector, N}) where {T,N} =
    similar(A, map(LabeledAxis, labels))

labeledarray(T::Type, init::Union{UndefInitializer,Nothing,Missing}, labels::Vararg{AbstractVector, N}) where {N} =
    labeledarray(Array{T,N}(init, map(length, labels)), labels...)
