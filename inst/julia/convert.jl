module IAIConvert

  using Main.IAI
  using DataFrames
  using CategoricalArrays

  convert_to_jl(value) = value
  convert_to_jl(p::Float64) = isinteger(p) ? Int(p) : p
  convert_to_jl(p::Vector{Float64}) = all(isinteger, p) ? Int.(p) : p
  function convert_to_jl(p::Vector{Any})
    if all(v -> !(v isa Number) || isinteger(v), p)
      [v isa Number ? Int(v) : convert_to_jl(v) for v in p]
    else
      convert_to_jl.(p)
    end
  end

  function convert_to_jl(df::DataFrame)
    DataFrame(convert_to_jl_col.(eachcol(df)), names(df))
  end
  convert_to_jl_col(col) = col
  function convert_to_jl_col(col::Vector{Any})
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

  function convert_to_jl(p::Pair)
    convert_to_jl(p.first) => convert_to_jl(p.second)
  end
  function convert_to_jl(pairs::Base.Iterators.Pairs)
    Pair[convert_to_jl(p) for p in pairs]
  end
  function convert_to_jl(dict::AbstractDict)
    Dict(convert_to_jl(p) for p in dict)
  end


  # In multi-visualizations `questions` is read in as OrderedDict
  # It needs special parsing into a nested `Pair` layout
  convert_to_jl_pairs(value) = value
  function convert_to_jl_pairs(p::Pair)
    convert_to_jl_pairs(p.first) => convert_to_jl_pairs(p.second)
  end
  function convert_to_jl_pairs(dict::AbstractDict)
    if length(dict) == 1
      convert_to_jl_pairs(first(pairs(dict)))
    else
      Pair[convert_to_jl_pairs(p) for p in dict]
    end
  end


  convert_to_R(o) = o
  convert_to_R(o::Tuple) = convert_to_R.(o)
  convert_to_R(o::SubArray) = convert_to_R(getindex(parent(o), o.indices...))
  convert_to_R(o::AbstractVector{Symbol}) = String.(o)

  function convert_to_R(df::AbstractDataFrame)
    DataFrame(convert_to_R.(eachcol(df)), names(df))
  end
  function convert_to_R(row::DataFrameRow)
    convert_to_R(DataFrame(row))
  end

  function convert_to_R(col::AbstractVector)
    T = nonmissingtype(eltype(col))
    if T <: IAI.IAIBase.MixedDatum
      out = Vector{Any}(IAI.undo_mixed_data(col))

      if T <: IAI.IAIBase.OrdinalMixedDatum
        for i in 1:length(out)
          old = col[i]
          if !ismissing(old) && !old.iscat
            # Put back the Ox_ prefix for ordinals
            f = if isdefined(CategoricalArrays, :levelcode)
              CategoricalArrays.levelcode
            else
              CategoricalArrays.order
            end
            order = f(old.value_else)
            out[i] = string("O", order, "_", out[i])
          end
        end
      end

      out
    else
      col
    end
  end

  for f in names(IAI)
    getfield(IAI, f) isa Module && continue

    f_mod = Symbol(replace(String(f), "!" => ""), "_convert")
    # Define new function in IAI to extend
    @eval IAI function $f_mod end
    # Define wrapper for IAI function to convert
    @eval Main begin
      function (IAI.$f_mod)(args...; kwargs...)
        out = (IAI.$f)(IAIConvert.convert_to_jl.(args)...;
                       IAIConvert.convert_to_jl(kwargs)...)
        IAIConvert.convert_to_R(out)
      end
    end
  end

  # Hack to avoid Int64 being returned to R and failing conversion to Int32
  @eval Main begin
    function IAI.get_params_convert(lnr::IAI.Learner, args...; kwargs...)
      out = IAI.get_params(IAIConvert.convert_to_jl(lnr),
                           IAIConvert.convert_to_jl.(args)...;
                           IAIConvert.convert_to_jl(kwargs)...)
      for (k, v) in out
        if v == typemax(Int64)
          out[k] = typemax(Int32)
        end
      end
      IAIConvert.convert_to_R(out)
    end
  end

  @eval Main begin
    if isdefined(IAI, :MultiTreePlot_convert)
      function IAI.MultiTreePlot_convert(dict::AbstractDict)
        IAI.MultiTreePlot_convert(IAIConvert.convert_to_jl_pairs(dict))
      end
    end
    if isdefined(IAI, :MultiQuestionnaire_convert)
      function IAI.MultiQuestionnaire_convert(dict::AbstractDict)
        IAI.MultiQuestionnaire_convert(IAIConvert.convert_to_jl_pairs(dict))
      end
    end
  end

  # Add julia methods for HTML output
  @eval IAI begin
    function to_html(obj)
      if showable("text/html", obj)
        # Buttons don't seem to work in RStudio viewer
        sprint(io -> show(io, MIME("text/html"), obj, html_show_buttons=false))
      else
        nothing
      end
    end
  end

  # Add fallback definition for `get_machine_id` so we can use it on older IAI
  if !isdefined(IAI, :get_machine_id_convert)
    if isdefined(IAI, :IAILicensing)
      @eval IAI begin
        get_machine_id_convert() = IAILicensing.machine_id()
      end
    else
      @eval IAI begin
        get_machine_id_convert() = IAIBase.machine_id()
      end
    end
  end

  # Wrap `validate_license` to just return the messages
  if isdefined(IAI, :IAILicensing) &&
     isdefined(IAI.IAILicensing, :validate_license)
    @eval IAI.IAILicensing begin
      function validate_license_convert(args...)
        [m for (_, m) in validate_license(args...)]
      end
    end
  end
end
