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


#' Return the machine ID for the current computer.
#'
#' This ID ties the IAI license file to your machine.
#'
#' @examples \dontrun{iai::get_machine_id()}
#'
#' @export
get_machine_id <- function() {
  jl_func("IAI.get_machine_id_convert")
}


#' Acquire an IAI license for the current session.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.2.2/installation/#IAI.acquire_license}{\code{IAI.acquire_license}}
#'
#' @param ... Refer to the Julia documentation for available parameters
#'
#' @examples \dontrun{iai::acquire_license()}
#'
#' @section IAI Compatibility:
#' Requires IAI version 3.1 or higher.
#'
#' @export
acquire_license <- function(...) {
  requires_iai_version("3.1.0", "acquire_license")
  jl_func("IAI.acquire_license_convert", ...)
}


#' Release any IAI license held by the current session.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.2.2/installation/#IAI.release_license}{\code{IAI.release_license}}
#'
#' @examples \dontrun{iai::release_license()}
#'
#' @section IAI Compatibility:
#' Requires IAI version 3.1 or higher.
#'
#' @export
release_license <- function() {
  requires_iai_version("3.1.0", "release_license")
  jl_func("IAI.release_license_convert")
}


#' Loads the Julia Graphviz library to permit certain visualizations.
#'
#' The library will be installed if not already present.
#'
#' @examples \dontrun{iai::load_graphviz()}
#'
#' @export
load_graphviz <- function() {
  julia_install_package_if_needed("Graphviz_jll")
  julia_library("Graphviz_jll")
}


#' Convert a vector of values to IAI mixed data format
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.2.2/IAIBase/reference/#IAI.make_mixed_data}{\code{IAI.make_mixed_data}}
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
