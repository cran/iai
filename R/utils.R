#' Set the random seed in Julia
#'
#' Julia Equivalent:
#' \href{https://docs.julialang.org/en/v1/stdlib/Random/index.html#Random.seed!}{\code{Random.seed!}}
#'
#' @usage set_julia_seed(seed)
#'
#' @param seed The seed to set
#'
#' @examples \dontrun{iai::set_julia_seed(1)}
#'
#' @export
set_julia_seed <- function(seed) {
  JuliaCall::julia_library("Random")
  jleval <- stringr::str_interp("Random.seed!(${seed})")
  JuliaCall::julia_eval(jleval)
}


#' Convert a vector of values to IAI mixed data format
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/IAIBase/stable/reference/#IAI.make_mixed_data}{\code{IAI.make_mixed_data}}
#'
#' @usage as.mixeddata(values, categorical_levels, ordinal_levels = c())
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
