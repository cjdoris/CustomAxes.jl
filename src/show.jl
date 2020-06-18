show(io::IO, m::MIME"text/plain", x::AbstractAxisArray) =
    show_axisarray(io, m, x)

function print_axisarray_pagelabel(io::IO, x::AbstractArray{T,N}, p::NTuple{N2}, crayons) where {T,N,N2}
    @assert N2==N-2
    print(io, "[:, :")
    ntuple(Val(N2)) do d
        pd = p[d]
        label = something(getlabel(axes(x, d+2), pd), pd)
        print(io, ", ")
        crayons===nothing || print(io, crayons[d+2])
        show(io, label)
        crayons===nothing || print(io, Crayon(reset=true))
    end
    print(io, "] =")
end

function print_axisarray_collabels(io, colaxis, align, colsep, colsplit, colsplitsep)
    for (i,(c,(a,b))) in enumerate(align)
        if i-1 == colsplit
            print(io, colsplitsep)
        elseif i > 1
            print(io, colsep)
        end
        w = a+b
        label = sprint(show, getlabel(colaxis, c), context=io)
        if length(label) > w
            print(io, label[1 : nextind(label, 0, w-1)], '…')
        else
            print(io, label)
            for _ in length(label)+1:w
                print(io, ' ')
            end
        end
    end
end

function print_axisarray_row(io, x, align, colsep, colsplit, colsplitsep)
    for (i,(c,(a,b))) in enumerate(align)
        if i-1 == colsplit
            print(io, colsplitsep)
        elseif i > 1
            print(io, colsep)
        end
        print_aligned(io, x[c], (a,b))
    end
end

function print_aligned(io, x, (a,b))
    ax, bx = Base.alignment(io, x)
    for _ in ax+1:a
        print(io, ' ')
    end
    show(io, x)
    for _ in bx+1:b
        print(io, ' ')
    end
end

function print_axisarray_dotsrow(io, dots, align, colsep, colsplit, colsplitsep)
    dotslen = length(dots)
    for (i,(c,(a,b))) in enumerate(align)
        if i-1 == colsplit
            print(io, colsplitsep)
        elseif i > 1
            print(io, colsep)
        end
        dotsidx = a==0 ? 1 : a
        for j in 1:(a+b)
            if j == dotsidx
                print(io, dots)
            else
                print(io, ' ')
            end
        end
    end
end

function print_axisarray(io::IO, x::AbstractArray{T,N}, (h,w)::NTuple{2,Integer}; crayons=nothing) where {T,N}
    rowaxis = axes(x,1)
    colaxis = axes(x,2)
    pageaxes = axes(x)[3:N]
    numrows = length(rowaxis)
    numcols = length(colaxis)
    allpages = Base.Iterators.product(pageaxes...)
    resetcrayon = Crayon(reset=true)
    colsep = " "
    colseplen = length(colsep)
    coldots = " ⋯ "
    coldotslen = length(coldots)
    diagdots = " ⋱ "
    @assert length(diagdots) == length(coldots)
    rowdots = "⋮"
    @assert length(rowdots) == 1
    rowdotsnlines = 1
    pagepad = 1
    pagedots = "..."
    pagedotsnlines = 1
    coldotsspace = repeat(" ", coldotslen)
    # when the dimension is greater than 2, we display the data in "pages" of matrices
    # decide which pages to show, and which rows
    dopagelabels = N > 2
    numpages = isempty(pageaxes) ? 1 : prod(map(length, pageaxes))
    dorowlabels = getlabel(rowaxis, first(rowaxis)) !== nothing
    docollabels = getlabel(colaxis, first(colaxis)) !== nothing
    pageheight = dopagelabels + docollabels + numrows
    if (pageheight + pagepad) * numpages ≤ h + pagepad
        # the whole matrix fits on screen
        pageinds = allpages
        rowinds = rowaxis
        rowsplit = nothing
        etcpages = false
    elseif pageheight ≤ h + 1
        # a whole page fits on screen
        # fit as many whole pages as possible
        numpages = min(numpages, (h+pagepad)÷(pageheight+pagepad))
        @assert numpages ≥ 1
        pageinds = collect(Base.Iterators.take(allpages, numpages))
        rowinds = rowaxis
        rowsplit = nothing
        etcpages = true
    else
        # only part of a page fits on screen
        # just print the first page, and as many rows as we can fit
        etcpages = numpages > 1
        numpages = 1
        pageinds = collect(Base.Iterators.take(allpages, numpages))
        numrows = min(numrows, h - (dopagelabels + docollabels + rowdotsnlines + etcpages*pagedotsnlines))
        @assert numrows > 0
        rowsplit = cld(numrows, 2)
        rowinds = [rowaxis[(0:rowsplit-1) .+ firstindex(rowaxis)]; rowaxis[(1-(numrows-rowsplit):0) .+ lastindex(rowaxis)]]
    end
    # compute alignment of row labels
    rowlabelalign = (0, 0)
    if dorowlabels
        for r in rowinds
            rowlabelalign = map(max, rowlabelalign, Base.alignment(io, getlabel(rowaxis, r)))
        end
    end
    w_rowlabel = sum(rowlabelalign)
    w_data = w - w_rowlabel - colseplen
    rowlabelspace = repeat(" ", w_rowlabel)
    # print each page
    for (i,p) in enumerate(pageinds)
        # one newline because the previous page didn't end with one, plus padding
        if i > 1
            for _ in 1:pagepad+1
                println(io)
            end
        end
        # print page labels
        if dopagelabels
            print_axisarray_pagelabel(io, x, p, crayons)
            println(io)
        end
        # compute alignment
        align = Base.alignment(io, view(x, :, :, p...), rowinds, colaxis, w_data, w_data, colseplen)
        align = collect(zip(Base.Iterators.take(colaxis, length(align)), align))
        colsplit = nothing
        coltruncate = false
        if length(align) < length(colaxis)
            # we didn't fit everything, so there will be a vertical split
            # compute alignment for the right hand side first, on at most half of the width
            rw = (w_data-coldotslen) ÷ 2
            ralign = Base.alignment(io, view(x, :, :, p...), rowinds, reverse(colaxis), rw, rw, colseplen)
            rw = sum(sum, ralign) + colseplen*max(0, length(ralign)-1)
            ralign = collect(zip(Base.Iterators.take(reverse(colaxis), length(ralign)), ralign))
            # now compute alignment for the left hand side on whatever width remains
            lw = (w_data-coldotslen) - rw
            lalign = Base.alignment(io, view(x, :, :, p...), rowinds, colaxis, lw, lw, colseplen)
            lw = sum(sum, lalign) + colseplen*max(0, length(lalign)-1)
            lalign = collect(zip(Base.Iterators.take(colaxis, length(lalign)), lalign))
            align = [lalign; reverse(ralign)]
            colsplit = length(lalign)
        elseif length(colaxis) == 1 && sum(align[1][2]) ≥ w_data
            # the one and only column is too wide, so we will truncate it when printing
            coltruncate = true
        elseif docollabels
            # everything fits and there are column labels
            # increase the column sizes until things fit
            colwidths = [sum(Base.alignment(io, getlabel(colaxis, c))) for c in colaxis]
            while true
                newalign = [(c, w>a+b ? (a,b+1) : (a,b)) for (w,(c,(a,b))) in zip(colwidths, align)]
                newalign == align && break
                sum(a+b for (c,(a,b)) in newalign) + colseplen*max(0, length(colaxis)-1) ≥ w_data && break
                align = newalign
            end
        end
        # print column labels
        if docollabels
            if dorowlabels
                print(io, rowlabelspace, colsep)
            end
            print(io, crayons[2])
            print_axisarray_collabels(io, colaxis, align, colsep, colsplit, coldotsspace)
            print(io, resetcrayon)
            println(io)
        end
        # print rows
        for (j,r) in enumerate(rowinds)
            if j > 1
                println(io)
            end
            if dorowlabels
                print(io, crayons[1])
                print_aligned(io, getlabel(rowaxis, r), rowlabelalign)
                print(io, resetcrayon)
                print(io, colsep)
            end
            if coltruncate
                str = sprint(show, x[r, first(colaxis), p...], context=io)
                if length(str) ≥ w_data
                    print(io, str[1:w_data-2], '…')
                else
                    print(io, str)
                end
            else
                print_axisarray_row(io, view(x, r, :, p...), align, colsep, colsplit, mod(j,5)==1 ? coldots : coldotsspace)
            end
            if j == rowsplit
                println(io)
                if dorowlabels
                    print(io, rowlabelspace, colsep)
                end
                print_axisarray_dotsrow(io, rowdots, align, colsep, colsplit, diagdots)
            end
        end
    end
    # etc
    if etcpages
        println(io)
        print(io, pagedots)
    end
end

const AXIS_COLORS = [:blue, :cyan, :green, :light_blue, :light_cyan, :light_green, :light_magenta, :light_red, :light_yellow, :magenta, :red, :yellow]

const AXIS_COLOR_SEED = Ref{UInt}(123)

"""
    axiscolor(ax, [seed])

Select a color for axis `ax`.

This depends only on the type of `ax`, and is pseudorandomly chosen from `AXIS_COLORS`. The default `seed` is `AXIS_COLOR_SEED[]`, which can be changed if you find colors clashing.
"""
function axiscolor(ax, seed=AXIS_COLOR_SEED[])
    AXIS_COLORS[rand(Random.MersenneTwister(colorhash(ax, UInt(seed))), axes(AXIS_COLORS, 1))]
end

function show_axisarray(io::IO, ::MIME"text/plain", x)
    crayons = map(ax->Crayon(foreground=axiscolor(ax)), axes(x))
    # summary line
    print(io, Base.dims2string(size(x)), " ")
    showarg(io, x, true)
    print(io, " with axes:")
    # axes lines
    for (d,ax) in enumerate(axes(x))
        println(io)
        print(io, crayons[d])
        print(io, "(", d, ") ")
        describe(io, ax)
        print(io, Crayon(reset=true))
    end
    # if empty, we're done
    isempty(x) && return
    # the display size remaining
    if get(io, :limit, false)
        h, w = displaysize(io)
    else
        h = w = typemax(Int)
    end
    h -= ndims(x) + 5
    if h ≤ 1
        println(io)
        print(io, '…')
        return
    end
    # values
    println(io)
    print(io, "and values:")
    println(io)
    if !haskey(io, :compact) && size(x,2)>1
        io = IOContext(io, :compact=>true)
    end
    io = IOContext(io, :typeinfo=>eltype(x), :SHOWN_SET=>x)
    print_axisarray(io, x, (h,w), crayons=crayons)
end
