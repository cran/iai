#' Learner for conducting Optimal Feature Selection on classification problems
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.2.1/OptimalFeatureSelection/reference/#IAI.OptimalFeatureSelectionClassifier}{\code{IAI.OptimalFeatureSelectionClassifier}}
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
#' \href{https://docs.interpretable.ai/v3.2.1/OptimalFeatureSelection/reference/#IAI.OptimalFeatureSelectionRegressor}{\code{IAI.OptimalFeatureSelectionRegressor}}
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


#' Fits an Optimal Feature Selection learner to the training data
#'
#' When the \code{coordinated_sparsity} parameter of the learner is \code{TRUE},
#' additional keyword arguments are required - please refer to the Julia
#' documentation.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.2.1/OptimalFeatureSelection/reference/#IAI.fit\%21-Tuple\%7BOptimalFeatureSelectionLearner\%7D}{\code{IAI.fit!}}
#'
#' @param obj The learner or grid to fit.
#' @param X The features of the data.
#' @param ... Other parameters, including zero or more target vectors as
#'            required by the problem type. Refer to the Julia documentation for
#'            available parameters.
#'
#' @examples \dontrun{iai::fit(lnr, X)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 1.1 or higher.
#'
#' @export
fit.optimal_feature_selection_learner <- function(obj, X, ...) {
  requires_iai_version("1.1.0", "fit",
                       "with `optimal_feature_selection_learner`")
  fit_common(obj, X, ...)
}


#' Return the constant term in the prediction in a trained
#' Optimal Feature Selection learner
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.2.1/OptimalFeatureSelection/reference/#IAI.get_prediction_constant-Tuple\%7BOptimalFeatureSelectionLearner\%7D}{\code{IAI.get_prediction_constant}}
#'
#' @param obj The learner to query.
#' @param fit_index The index of the cluster to use for prediction, if the
#'                  \code{coordinated_sparsity} parameter on the learner is
#'                  \code{TRUE}.
#' @param ... Additional arguments (unused)
#'
#' @examples \dontrun{iai::get_prediction_constant(lnr)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 1.1 or higher.
#'
#' @export
get_prediction_constant.optimal_feature_selection_learner <- function(
    obj, fit_index = NULL, ...
) {
  requires_iai_version("1.1.0", "get_prediction_constant",
                       "with `optimal_feature_selection_learner")
  if (is.null(fit_index) && iai_version_less_than("3.0.0")) {
    get_prediction_constant_common(obj, ...)
  } else {
    get_prediction_constant_common(obj, fit_index = fit_index, ...)
  }
}


#' Return the weights for numeric and categoric features used for prediction in
#' a trained Optimal Feature Selection learner
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.2.1/OptimalFeatureSelection/reference/#IAI.get_prediction_weights-Tuple\%7BOptimalFeatureSelectionLearner\%7D}{\code{IAI.get_prediction_weights}}
#'
#' @param obj The learner to query.
#' @param fit_index The index of the cluster to use for prediction, if the
#'                  \code{coordinated_sparsity} parameter on the learner is
#'                  \code{TRUE}.
#' @param ... Additional arguments (unused)
#'
#' @examples \dontrun{iai::get_prediction_weights(lnr)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 1.1 or higher.
#'
#' @export
get_prediction_weights.optimal_feature_selection_learner <- function(
    obj, fit_index = NULL, ...
) {
  requires_iai_version("1.1.0", "get_prediction_weights",
                       "with `optimal_feature_selection_learner")
  if (is.null(fit_index) && iai_version_less_than("3.0.0")) {
    get_prediction_weights_common(obj, ...)
  } else {
    get_prediction_weights_common(obj, fit_index = fit_index, ...)
  }
}


#' Return the number of fits along the path in a trained Optimal Feature
#' Selection learner
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.2.1/OptimalFeatureSelection/reference/#IAI.get_num_fits-Tuple\%7BOptimalFeatureSelectionLearner\%7D}{\code{IAI.get_num_fits}}
#'
#' @param obj The Optimal Feature Selection learner to query.
#' @param ... Additional arguments (unused)
#'
#' @examples \dontrun{iai::get_num_fits(lnr)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 3.0 or higher.
#'
#' @export
get_num_fits.optimal_feature_selection_learner <- function(obj, ...) {
  requires_iai_version("3.0.0", "get_num_fits",
                       "with `optimal_feature_selection_learner`")
  get_num_fits_common(obj, ...)
}


#' Calculate the score for an Optimal Feature Selection learner
#' on the given data
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.2.1/OptimalFeatureSelection/reference/#IAI.score-Tuple\%7BOptimalFeatureSelectionLearner\%7D}{\code{IAI.score}}
#'
#' @param obj The learner or grid to evaluate.
#' @param X The features of the data.
#' @param ... Other parameters, including zero or more target vectors as
#'            required by the problem type. If the \code{coordinated_sparsity}
#'            parameter on the learner is \code{TRUE}, then \code{fit_index}
#'            must be used to specify which cluster should be used. Refer to
#'            the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::score(lnr, X, y, fit_index=1)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 1.1 or higher.
#'
#' @export
score.optimal_feature_selection_learner <- function(obj, X, ...) {
  requires_iai_version("1.1.0", "score",
                       "with `optimal_feature_selection_learner`")
  score_common(obj, X, ...)
}


#' Return the predictions made by an Optimal Feature Selection learner for each
#' point in the features
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.2.1/OptimalFeatureSelection/reference/#IAI.predict-Tuple\%7BOptimalFeatureSelectionLearner\%2C\%20Union\%7BDataFrames.AbstractDataFrame\%2C\%20AbstractMatrix\%7B\%3C\%3AReal\%7D\%7D\%7D}{\code{IAI.predict}}
#'
#' @param obj The learner or grid to use for prediction.
#' @param X The features of the data.
#' @param fit_index The index of the cluster to use for prediction, if the
#'                  \code{coordinated_sparsity} parameter on the learner is
#'                  \code{TRUE}.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::predict(lnr, X)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 1.1 or higher.
#'
#' @export
predict.optimal_feature_selection_learner <- function(obj, X, fit_index = NULL,
                                                      ...) {
  requires_iai_version("1.1.0", "predict",
                       "with `optimal_feature_selection_learner`")
  if (is.null(fit_index) && iai_version_less_than("3.0.0")) {
    predict_common(obj, X, ...)
  } else {
    predict_common(obj, X, fit_index = fit_index, ...)
  }
}


#' Generate a ranking of the variables in an Optimal Feature Selection learner
#' according to their importance during training. The results are normalized so
#' that they sum to one.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.2.1/OptimalFeatureSelection/reference/#IAI.variable_importance-Tuple\%7BOptimalFeatureSelectionLearner\%7D}{\code{IAI.variable_importance}}
#'
#' @param obj The learner to query.
#' @param fit_index The index of the cluster to use for prediction, if the
#'                  \code{coordinated_sparsity} parameter on the learner is
#'                  \code{TRUE}.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::variable_importance(lnr, ...)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 1.1 or higher.
#'
#' @export
variable_importance.optimal_feature_selection_learner <- function(
    obj, fit_index = NULL, ...
) {
  requires_iai_version("1.1.0", "variable_importance",
                       "with `optimal_feature_selection_learner`")
  if (is.null(fit_index) && iai_version_less_than("3.1.0")) {
    variable_importance_common(obj, ...)
  } else {
    variable_importance_common(obj, fit_index = fit_index, ...)
  }
}


#' Specify an interactive questionnaire of an Optimal Feature Selection learner
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.2.1/OptimalFeatureSelection/reference/#IAI.Questionnaire-Tuple\%7BOptimalFeatureSelectionLearner\%7D}{\code{IAI.Questionnaire}}
#'
#' @param obj The learner to visualize.
#' @param ... Refer to the \href{https://docs.interpretable.ai/v3.2.1/IAITrees/advanced/#Advanced-Visualization-1}{Julia documentation} for available parameters.
#'
#' @examples \dontrun{iai::questionnaire(lnr)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @export
questionnaire.optimal_feature_selection_learner <- function(obj, ...) {
  requires_iai_version("2.1.0", "questionnaire",
                       "with `optimal_feature_selection_learner`")
  questionnaire_common(obj, ...)
}


#' Output an Optimal Feature Selection learner as an interactive questionnaire
#' in HTML format
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.2.1/OptimalFeatureSelection/reference/#IAI.write_questionnaire-Tuple\%7BAny\%2C\%20OptimalFeatureSelectionLearner\%7D}{\code{IAI.write_questionnaire}}
#'
#' @param filename Where to save the output.
#' @param obj The learner or grid to output.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::write_questionnaire(file.path(tempdir(), "questionnaire.html"), lnr)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @export
write_questionnaire.optimal_feature_selection_learner <- function(filename, obj,
                                                                  ...) {
  requires_iai_version("2.1.0", "write_questionnaire",
                       "with `optimal_feature_selection_learner`")
  write_questionnaire_common(filename, obj, ...)
}


#' Show an interactive questionnaire based on an Optimal Feature Selection
#' learner in default browser
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.2.1/OptimalFeatureSelection/reference/#IAI.show_questionnaire-Tuple\%7BOptimalFeatureSelectionLearner\%7D}{\code{IAI.show_questionnaire}}
#'
#' @param obj The learner or grid to visualize.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::show_questionnaire(lnr)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @export
show_questionnaire.optimal_feature_selection_learner <- function(obj, ...) {
  requires_iai_version("2.1.0", "show_questionnaire",
                       "with `optimal_feature_selection_learner`")
  show_questionnaire_common(obj, ...) # nocov
}


#' Construct a
#' \href{https://ggplot2.tidyverse.org/reference/ggplot.html}{\code{ggplot2::ggplot}}
#' object plotting grid search results for Optimal Feature Selection learners
#'
#' @param object The grid search to plot
#' @param type The type of plot to construct (either \code{"validation"} or
#'             \code{"importance"}, for more information refer to the
#'             \href{https://docs.interpretable.ai/v3.2.1/OptimalFeatureSelection/visualization/#Plotting-Grid-Search-Results-1}{Julia documentation for plotting grid search results}
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
autoplot.grid_search <- function(object, type = stop("`type` is required"),
                                 ...) {
  if ("optimal_feature_selection_learner" %in% class(get_learner(object)) &&
      !iai_version_less_than("2.2.0")) {
    d <- jl_func("IAI.OptimalFeatureSelection.get_plot_data", object)

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
#'            \code{\link{autoplot.grid_search}})
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
