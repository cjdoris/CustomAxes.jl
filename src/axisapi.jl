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

describe(io, x::AxisLike) = print(io, first(x), ":", last(x))

colorhash(ax::AxisLike, h) = h
