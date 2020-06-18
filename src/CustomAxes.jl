module CustomAxes

using Crayons, Random

import Base: axes, length, size, parent, first, last, getindex, setindex!, similar, reshape, to_indices, show, print, summary, showarg, convert

export
    # abstractaxisarray.jl
    AbstractAxisArray,
    # axisarray.jl
    AxisArray,
    # abstractaxis.jl
    AbstractAxis,
    # simpleaxis.jl
    SimpleAxis,
    # labels.jl
    LabeledAxis,
    AxisLabels,
    labeledarray,
    rawlabels,
    labels,
    newlabels,
    getlabel

# a promotion scheme for axes (TYPE PIRACY AHEAD!)
include("promoteindices.jl")

# the core of the module
include("utils.jl")
include("axisapi.jl")
include("abstractaxisarray.jl")
include("axisarray.jl")
include("abstractaxis.jl")
include("simpleaxis.jl")

# batteries included
include("labels.jl")

end # module
