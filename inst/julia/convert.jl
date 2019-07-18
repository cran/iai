module IAIConvert

  using Main.IAI
  using DataFrames
  using CategoricalArrays

  convert_to_jl(value) = value
  function convert_to_jl(df::DataFrame)
    for j in 1:size(df, 2)
      df[j] = convert_to_jl(df[j])
    end
    df
  end

  function convert_to_jl(col::Vector{Any})
    n = length(col)
    ordinal_levels = Vector{String}(undef, n)

    max_order = 0
    # Parse the ordinal levels from the string
    for i in 1:n
      val = col[i]
      if !ismissing(val) && occursin(r"^O\d+_", string(val))
        order = parse(Int, replace(val, r"^O(?<num>\d+)\_\w*" => s"\g<num>"))
        max_order = max(order, max_order)

        level = replace(val, r"^O\d+_" => "")
        ordinal_levels[order] = level
        col[i] = level
      end
    end

    if max_order == 0
      col = IAI.make_mixed_data(col)
    else
      ordinal_levels = ordinal_levels[1:max_order]
      col = IAI.make_mixed_data(col, ordinal_levels)
    end
    col
  end

  convert_to_R(o) = o
  convert_to_R(o::Tuple) = convert_to_R.(o)
  convert_to_R(o::SubArray) = convert_to_R(getindex(parent(o), o.indices...))
  convert_to_R(o::AbstractVector{Symbol}) = String.(o)

  function convert_to_R(df::AbstractDataFrame)
    for j in 1:size(df, 2)
      df[j] = convert_to_R(df[j])
    end
    df
  end

  function convert_to_R(col::AbstractVector{T}) where T<:IAI.IAIBase.MixedDatum
    out = Vector{Any}(IAI.undo_mixed_data(col))

    if T == IAI.IAIBase.OrdinalMixedDatum
      for i in 1:length(out)
        old = col[i]
        if !old.iscat
          # Put back the Ox_ prefix for ordinals
          order = CategoricalArrays.order(old.value_else)
          out[i] = string("O", order, "_", out[i])
        end
      end
    end

    out
  end

  for f in names(IAI)
    getfield(IAI, f) isa Module && continue

    f_mod = Symbol(replace(String(f), "!" => ""), "_convert")
    # Define new function in IAI to extend
    @eval IAI function $f_mod end
    # Define wrapper for IAI function to convert
    @eval Main begin
      function (IAI.$f_mod)(args...; kwargs...)
        out = (IAI.$f)(IAIConvert.convert_to_jl.(args)...; kwargs...)
        IAIConvert.convert_to_R(out)
      end
    end
  end

  # Hack to avoid Int64 being returned to R and failing conversion to Int32
  @eval Main begin
    function IAI.get_params_convert(lnr::IAI.Learner, args...; kwargs...)
      out = IAI.get_params(IAIConvert.convert_to_jl(lnr),
                           IAIConvert.convert_to_jl.(args)...; kwargs...)
      for (k, v) in out
        if v == typemax(Int64)
          out[k] = typemax(Int32)
        end
      end
      IAIConvert.convert_to_R(out)
    end
  end
end
