function to_indices_and_axes(a, args...)
    inds = to_indices(a, args)
    axes = Base.index_shape(inds...)
    inds, axes
end

finddims(dims::Tuple) = map(finddim, dims)
finddims(dims) = finddims((dims,))

finddim(d::Integer) = convert(Int, d)
