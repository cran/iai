#' Set the random seed in Julia
#'
#' Julia Equivalent:
#' \href{https://docs.julialang.org/en/v1/stdlib/Random/index.html#Random.seed!}{\code{Random.seed!}}
#'
#' @param seed The seed to set
#'
#' @examples \dontrun{iai::set_julia_seed(1)}
#'
#' @export
set_julia_seed <- function(seed) {
  julia_library("Random")
  jleval <- stringr::str_interp("Random.seed!(${seed})")
  julia_eval(jleval)
}

#' Add additional Julia worker processes to parallelize workloads
#'
#' Julia Equivalent:
#' \href{https://docs.julialang.org/en/v1/stdlib/Distributed/#Distributed.addprocs}{\code{Distributed.addprocs!}}
#'
#' For more information, refer to the
#' \href{https://docs.interpretable.ai/stable/IAIBase/advanced/#IAIBase-Parallelization-1}{documentation on parallelization}
#'
#' @param ... Refer to the Julia documentation for available parameters
#'
#' @examples \dontrun{iai::add_julia_processes(3)}
#'
#' @export
add_julia_processes <- function(...) {
  julia_library("Distributed")
  out <- jl_func("Distributed.addprocs", ...)

  # We need to load IAI on all processes
  # NB: If using system image this is automatic
  if (!JuliaCall::julia_exists("IAISysImg")) {
    JuliaCall::julia_eval("Distributed.@everywhere import IAI")
  }

  out
}


#' Convert a vector of values to IAI mixed data format
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.make_mixed_data}{\code{IAI.make_mixed_data}}
#'
#' @param values The vector of values to convert
#' @param categorical_levels The values in \code{values} to treat as categoric
#'                           levels
#' @param ordinal_levels (optional) The values in \code{values} to treat as
#'                       ordinal levels, in the order supplied
#'
#' @example examples/as.mixeddata.R
#'
#' @export
as.mixeddata <- function(values, categorical_levels, ordinal_levels = c()) {
  n <- length(values)
  output <- list()
  for (i in 1:n) {
    if (values[i] %in% ordinal_levels) {
      order <- match(values[i], ordinal_levels)
      output[[i]] <- paste0("O", order, "_", values[i])
    } else if (values[i] %in% categorical_levels) {
      output[[i]] <- values[i]
    } else {
      # TODO: add assert that it has to be double or missing
      output[[i]] <- as.double(values[i])
    }
  }
  output
}
