#' Learner for conducting Optimal Feature Selection on classification problems
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/OptimalFeatureSelection/reference/#IAI.OptimalFeatureSelectionClassifier}{\code{IAI.OptimalFeatureSelectionClassifier}}
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::optimal_feature_selection_classifier()}
#'
#' @section IAI Compatibility:
#' Requires IAI version 1.1 or higher.
#'
#' @export
optimal_feature_selection_classifier <- function(...) {
  requires_iai_version("1.1.0", "optimal_feature_selection_classifier")
  set_obj_class(jl_func("IAI.OptimalFeatureSelectionClassifier_convert", ...))
}


#' Learner for conducting Optimal Feature Selection on regression problems
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/OptimalFeatureSelection/reference/#IAI.OptimalFeatureSelectionRegressor}{\code{IAI.OptimalFeatureSelectionRegressor}}
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::optimal_feature_selection_regressor()}
#'
#' @section IAI Compatibility:
#' Requires IAI version 1.1 or higher.
#'
#' @export
optimal_feature_selection_regressor <- function(...) {
  requires_iai_version("1.1.0", "optimal_feature_selection_regressor")
  set_obj_class(jl_func("IAI.OptimalFeatureSelectionRegressor_convert", ...))
}


#' Return the constant term in the prediction in the trained learner
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/OptimalFeatureSelection/reference/#IAI.get_prediction_constant}{\code{IAI.get_prediction_constant}}
#'
#' @param lnr The learner to query.
#' @param ... If a GLMNet learner, the index of the fit in the path to query,
#'            defaulting to the best fit if not supplied.
#'
#' @examples \dontrun{iai::get_prediction_constant(lnr)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 1.1 or higher.
#'
#' @export
get_prediction_constant <- function(lnr, ...) {
  requires_iai_version("1.1.0", "get_prediction_constant")
  jl_func("IAI.get_prediction_constant_convert", lnr, ...)
}


#' Return the weights for numeric and categoric features used for prediction in
#' the trained learner
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/OptimalFeatureSelection/reference/#IAI.get_prediction_weights}{\code{IAI.get_prediction_weights}}
#'
#' @param lnr The learner to query.
#' @param ... If a GLMNet learner, the index of the fit in the path to query,
#'            defaulting to the best fit if not supplied.
#'
#' @examples \dontrun{iai::get_prediction_weights(lnr)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 1.1 or higher.
#'
#' @export
get_prediction_weights <- function(lnr, ...) {
  requires_iai_version("1.1.0", "get_prediction_weights")
  out <- jl_func("IAI.get_prediction_weights_convert", lnr, ...)
  names(out) <- c("numeric", "categoric")
  out
}



#' Construct a
#' \href{https://ggplot2.tidyverse.org/reference/ggplot.html}{\code{ggplot2::ggplot}}
#' object plotting grid search results for Optimal Feature Selection learners
#'
#' @param x The grid search to plot
#' @param type The type of plot to construct (either \code{"validation"} or
#'             \code{"importance"}, for more information refer to the
#'             \href{https://docs.interpretable.ai/v2.2.0/OptimalFeatureSelection/visualization/#Plotting-Grid-Search-Results-1}{Julia documentation for plotting grid search results}
#'             )
#' @param ... Additional arguments (unused)
#'
#' @examples \dontrun{ggplot2::autoplot(grid)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.2 or higher.
#'
#' @importFrom ggplot2 autoplot
#' @importFrom rlang .data
#' @export
autoplot.grid_search <- function(x, type = stop("`type` is required"), ...) {
  if ("optimal_feature_selection_learner" %in% class(get_learner(x)) &&
      !iai_version_less_than("2.2.0")) {
    d <- jl_func("IAI.OptimalFeatureSelection.get_plot_data", x)

    if (type == "validation") {
      plot_data <- data.frame(sparsity = d$sparsity, score = d$score)
      ggplot2::ggplot(plot_data,
                      ggplot2::aes(x = .data$sparsity, y = .data$score)) +
        ggplot2::geom_line() +
        ggplot2::labs(x = "Sparsity", y = "Validation Score") +
        ggplot2::ggtitle("Validation Score against Sparsity")

    } else if (type == "importance") {
      plot_data <- expand.grid(y = d$feature_names, x = d$sparsity)
      plot_data$z <- as.vector(d$importance)
      ggplot2::ggplot(plot_data,
                      ggplot2::aes(x = .data$x, y = .data$y, fill = .data$z)) +
        ggplot2::geom_tile() +
        ggplot2::labs(x = "Sparsity", y = "") +
        ggplot2::ggtitle("Normalized Variable Importance") +
        ggplot2::theme(legend.title = ggplot2::element_blank())

    } else {
      stop("`type` has to be \"validation\" or \"importance\"")
    }
  } else {
    NextMethod() # nocov
  }
}


#' Plot a grid search results for Optimal Feature Selection learners
#'
#' @param x The grid search to plot
#' @param ... Additional arguments (passed to
#'            \href{https://docs.interpretable.ai/v2.2.0/IAI-R/reference/#ggplot2::autoplot.grid_search}{\code{ggplot2::autoplot.grid_search}})
#'
#' @examples \dontrun{plot(grid)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.2 or higher.
#'
#' @importFrom graphics plot
#' @export
plot.grid_search <- function(x, ...) {
  if ("optimal_feature_selection_learner" %in% class(get_learner(x)) &&
      !iai_version_less_than("2.2.0")) {
    print(ggplot2::autoplot(x, ...))
  } else {
    NextMethod() # nocov
  }
}
