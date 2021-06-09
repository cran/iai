#' Split the data into training and test datasets
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.split_data}{\code{IAI.split_data}}
#'
#' @param task The type of problem.
#' @param X The features of the data.
#' @param ... Other parameters, including zero or more target vectors as
#'            required by the problem type. Refer to the Julia documentation for
#'            available parameters.
#'
#' @example examples/split_data.R
#'
#' @export
split_data <- function(task, X, ...) {
  out <- jl_func("IAI.split_data_convert", task, X, ...)

  names(out) <- c("train", "test")
  if (task %in% c("classification", "regression")) {
    names(out$train) <- names(out$test) <- c("X", "y")
  } else if (task %in% c("survival")) {
    names(out$train) <- names(out$test) <- c("X", "deaths", "times")
  } else if (task %in% c("prescription_minimize", "prescription_maximize",
                         "policy_minimize", "policy_maximize")) {
    if (length(out$train) == 3) {
      names(out$train) <- names(out$test) <- c("X", "treatments", "outcomes")
    } else {
      names(out$train) <- names(out$test) <- c("X", "treatments", "deaths",
                                               "times")
    }
  } else if (task %in% c("imputation")) {
    names(out$train) <- names(out$test) <- c("X")
  }

  out
}


#' Fits a model to the training data
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.fit!}{\code{IAI.fit!}}
#'
#' @param lnr The learner or grid to fit.
#' @param X The features of the data.
#' @param ... Other parameters, including zero or more target vectors as
#'            required by the problem type. Refer to the Julia documentation for
#'            available parameters.
#'
#' @example examples/fit.R
#'
#' @export
fit <- function(lnr, X, ...) {
  jl_func("IAI.fit_convert", lnr, X, ...)
  lnr
}


#' Return the predictions made by the model for each point in the features
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.predict}{\code{IAI.predict}}
#'
#' @param lnr The learner or grid to use for prediction.
#' @param X The features of the data.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::predict(lnr, X)}
#'
#' @export
predict <- function(lnr, X, ...) {
  out <- jl_func("IAI.predict_convert", lnr, X, ...)
  if (typeof(out) == "list" && length(out) == 2) {
    names(out) <- c("treatments", "outcomes")
  }
  out
}


#' Calculate the score for a model on the given data
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.score}{\code{IAI.score}}
#'
#' @param lnr The learner or grid to evaluate.
#' @param X The features of the data.
#' @param ... Other parameters, including zero or more target vectors as
#'            required by the problem type. Refer to the Julia documentation for
#'            available parameters.
#'
#' @examples \dontrun{iai::score(lnr, X, y)}
#'
#' @export
score <- function(lnr, X, ...) {
  jl_func("IAI.score_convert", lnr, X, ...)
}


#' Generate a ranking of the variables in the learner according to their
#' importance during training. The results are normalized so that they sum to
#' one.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.variable_importance}{\code{IAI.variable_importance}}
#'
#' @param lnr The learner to query.
#'
#' @examples \dontrun{iai::variable_importance(lnr)}
#'
#' @export
variable_importance <- function(lnr) {
  jl_func("IAI.variable_importance_convert", lnr)
}


#' Return the names of the features used by the learner
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.get_features_used}{\code{IAI.get_features_used}}
#'
#' @param lnr The learner to query.
#'
#' @examples \dontrun{iai::get_features_used(lnr)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.2 or higher.
#'
#' @export
get_features_used <- function(lnr) {
  requires_iai_version("2.2.0", "get_features_used")
  jl_func("IAI.get_features_used_convert", lnr)
}


#' Output a learner or grid in JSON format
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.write_json}{\code{IAI.write_json}}
#'
#' @param filename Where to save the output.
#' @param obj The learner or grid to output.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::write_json(file.path(tempdir(), "out.json"), obj)}
#'
#' @export
write_json <- function(filename, obj, ...) {
  jl_func("IAI.write_json_convert", filename, obj, ...)
}


#' Read in a learner or grid saved in JSON format
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.read_json}{\code{IAI.read_json}}
#'
#' @param filename The location of the JSON file.
#'
#' @examples \dontrun{obj <- iai::read_json("out.json")}
#'
#' @export
read_json <- function(filename) {
  set_obj_class(jl_func("IAI.read_json_convert", filename))
}


#' Return the value of all parameters on a learner
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.get_params}{\code{IAI.get_params}}
#'
#' @param lnr The learner to query.
#'
#' @examples \dontrun{iai::get_params(lnr)}
#'
#' @export
get_params <- function(lnr) {
  jl_func("IAI.get_params_convert", lnr)
}


#' Set all supplied parameters on a learner
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.set_params!}{\code{IAI.set_params!}}
#'
#' @param lnr The learner to modify.
#' @param ... The parameters to set on the learner.
#'
#' @examples \dontrun{iai::set_params(lnr, random_seed = 1)}
#'
#' @export
set_params <- function(lnr, ...) {
  jl_func("IAI.set_params_convert", lnr, ...)
  lnr
}


#' Return an unfitted copy of a learner with the same parameters
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.clone}{\code{IAI.clone}}
#'
#' @param lnr The learner to copy.
#'
#' @examples \dontrun{new_lnr <- iai::clone(lnr)}
#'
#' @export
clone <- function(lnr) {
  set_obj_class(jl_func("IAI.clone_convert", lnr))
}


#' Show interactive visualization of an object (such as a learner or curve) in
#' the default browser
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.show_in_browser}{\code{IAI.show_in_browser}}
#'
#' @param obj The object to visualize.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::show_in_browser(lnr)}
#'
#' @section IAI Compatibility:
#' Showing a grid search requires IAI version 2.0 or higher.
#'
#' @export
show_in_browser <- function(obj, ...) {
  jl_func("IAI.show_in_browser_convert", obj, ...) # nocov
}


#' Sets a global rich output parameter
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.set_rich_output_param!}{\code{IAI.set_rich_output_param!}}
#'
#' @param key The parameter to set.
#' @param value The value to set
#'
#' @examples \dontrun{iai::set_rich_output_param("simple_layout", TRUE)}
#'
#' @export
set_rich_output_param <- function(key, value) {
  jl_func("IAI.set_rich_output_param_convert", key, value)
}


#' Return the current global rich output parameter settings
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.get_rich_output_params}{\code{IAI.get_rich_output_params}}
#'
#' @examples \dontrun{iai::get_rich_output_params()}
#'
#' @export
get_rich_output_params <- function() {
  jl_func("IAI.get_rich_output_params_convert")
}


#' Delete a global rich output parameter
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.delete_rich_output_param!}{\code{IAI.delete_rich_output_param!}}
#'
#' @param key The parameter to delete.
#'
#' @examples \dontrun{iai::delete_rich_output_param("simple_layout")}
#'
#' @export
delete_rich_output_param <- function(key) {
  jl_func("IAI.delete_rich_output_param_convert", key)
}


#' Controls grid search over parameter combinations
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.GridSearch}{\code{IAI.GridSearch}}
#'
#' @param lnr The learner to use when validating.
#' @param ... The parameters to validate over.
#'
#' @example examples/grid_search.R
#'
#' @export
grid_search <- function(lnr, ...) {
  set_obj_class(jl_func("IAI.GridSearch", lnr, ...))
}


#' Fits a grid search to the training data with cross-validation
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.fit_cv!}{\code{IAI.fit_cv!}}
#'
#' @param grid The grid to fit.
#' @param X The features of the data.
#' @param ... Other parameters, including zero or more target vectors as
#'            required by the problem type. Refer to the Julia documentation for
#'            available parameters.
#'
#' @example examples/fit_cv.R
#'
#' @export
fit_cv <- function(grid, X, ...) {
  jl_func("IAI.fit_cv_convert", grid, X, ...)
  grid
}


#' Return the fitted learner using the best parameter combination from a grid
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.get_learner}{\code{IAI.get_learner}}
#'
#' @param grid The grid to query.
#'
#' @examples \dontrun{lnr <- iai::get_learner(grid)}
#'
#' @export
get_learner <- function(grid) {
  set_obj_class(jl_func("IAI.get_learner_convert", grid))
}


#' Return the best parameter combination from a grid
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.get_best_params}{\code{IAI.get_best_params}}
#'
#' @param grid The grid search to query.
#'
#' @examples \dontrun{iai::get_best_params(grid)}
#'
#' @export
get_best_params <- function(grid) {
  jl_func("IAI.get_best_params_convert", grid)
}


#' Return a summary of the results from the grid search
#'
#' This function was deprecated and renamed to [get_grid_result_summary()]
#' in iai 2.1.0. This is for consistency with the IAI v2.2.0 Julia release.
#'
#' @param grid The grid search to query.
#'
#' @examples \dontrun{iai::get_grid_results(grid)}
#'
#' @export
get_grid_results <- function(grid) {
  lifecycle::deprecate_warn("2.1.0", "iai::get_grid_results()",
                            "get_grid_result_summary()")
  get_grid_result_summary(grid)
}


#' Return a summary of the results from the grid search
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.get_grid_result_summary}{\code{IAI.get_grid_result_summary}}
#'
#' @param grid The grid search to query.
#'
#' @examples \dontrun{iai::get_grid_result_summary(grid)}
#'
#' @export
get_grid_result_summary <- function(grid) {
  if (iai_version_less_than("2.2.0")) {
    jl_func("IAI.get_grid_results_convert", grid)
  } else {
    jl_func("IAI.get_grid_result_summary_convert", grid)
  }
}


#' Return a vector of lists detailing the results of the grid search
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.get_grid_result_details}{\code{IAI.get_grid_result_details}}
#'
#' @param grid The grid search to query.
#'
#' @examples \dontrun{iai::get_grid_result_details(grid)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.2 or higher.
#'
#' @export
get_grid_result_details <- function(grid) {
  requires_iai_version("2.2.0", "get_grid_result_details")
  details <- as.list(jl_func("IAI.get_grid_result_details_convert", grid))

  # Tag all learners in the grid with the right class attrs
  for (i in seq(details)) {
    d <- details[[i]]
    for (j in seq(d$fold_results)) {
      d$fold_results[[j]]$learner <- set_obj_class(d$fold_results[[j]]$learner)
    }
  }

  details
}


#' Return the probabilities of class membership predicted by a model for each
#' point in the features
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.predict_proba}{\code{IAI.predict_proba}}
#'
#' @param lnr The learner or grid to use for prediction.
#' @param X The features of the data.
#'
#' @examples \dontrun{iai::predict_proba(lnr, X)}
#'
#' @export
predict_proba <- function(lnr, X) {
  jl_func("IAI.predict_proba_convert", lnr, X)
}


#' Generic function for constructing an ROC curve
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.ROCCurve}{\code{IAI.ROCCurve}}
#'
#' @param obj The object controlling which method is used
#' @param ... Arguments depending on the specific method used
#'
#' @export
roc_curve <- function(obj, ...) {
  UseMethod("roc_curve", obj)
}


#' Construct an ROC curve using a trained model on the given data
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.ROCCurve}{\code{IAI.ROCCurve}}
#'
#' @param obj The learner or grid to use for prediction.
#' @param X The features of the data.
#' @param y The labels of the data.
#' @param ... Additional arguments (unused)
#'
#' @examples \dontrun{iai::roc_curve(lnr, X, y)}
#'
#' @export
roc_curve.learner <- function(obj, X, y, ...) {
  set_obj_class(jl_func("IAI.ROCCurve_convert", obj, X, y, ...))
}


#' Construct an ROC curve from predicted probabilities and true labels
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.ROCCurve}{\code{IAI.ROCCurve}}
#'
#' @param obj The predicted probabilities for each point in the data.
#' @param y The true labels of the data.
#' @param positive_label The label for which probability is being predicted.
#' @param ... Additional arguments (unused)
#'
#' @examples \dontrun{iai::roc_curve(probs, y, positive_label=positive_label)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.0 or higher.
#'
#' @export
roc_curve.default <- function(
    obj,
    y,
    positive_label = stop("`positive_label` is required"),
    ...
  ) {
  requires_iai_version("2.0.0", "roc_curve",
                       "with probabilities and true labels")
  set_obj_class(jl_func("IAI.ROCCurve_convert", obj, y,
                        positive_label = positive_label))
}


#' Construct a \href{https://ggplot2.tidyverse.org/reference/ggplot.html}{\code{ggplot2::ggplot}} object plotting the ROC curve
#'
#' @param x The ROC curve to plot
#' @param ... Additional arguments (unused)
#'
#' @examples \dontrun{ggplot2::autoplot(roc)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @importFrom ggplot2 autoplot
#' @importFrom rlang .data
#' @export
autoplot.roc_curve <- function(x, ...) {
  requires_iai_version("2.1.0", "ggplot2::autoplot.roc_curve")

  d <- get_roc_curve_data(x)

  tpr <- sapply(d$coords, `[[`, "tpr")
  fpr <- sapply(d$coords, `[[`, "fpr")

  plot_data <- data.frame(tpr = tpr, fpr = fpr)
  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$fpr, y = .data$tpr)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "False Positive Rate", y = "True Positive Rate") +
    ggplot2::ggtitle(paste("AUC", round(d$auc, digits = 3)))
}


#' Plot an ROC curve
#'
#' @param x The ROC curve to plot
#' @param ... Additional arguments (unused)
#'
#' @examples \dontrun{plot(roc)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @importFrom graphics plot
#' @export
plot.roc_curve <- function(x, ...) {
  if (!iai_version_less_than("2.1.0")) {
    print(ggplot2::autoplot(x, ...))
  } else {
    NextMethod() # nocov
  }
}


#' Extract the underlying data from an ROC curve (as returned by
#' \href{https://docs.interpretable.ai/v2.2.0/IAI-R/reference/#iai::roc_curve}{\code{roc_curve}})
#'
#' The data is returned as a list with two keys: \code{auc} giving the
#' area-under-the-curve, and \code{coords} containing a vector of lists
#' representing each point on the curve, each with keys \code{fpr} (the false
#' positive rate), \code{tpr} (the true positive rate) and \code{threshold}
#' (the threshold).
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.get_roc_curve_data}{\code{IAI.get_roc_curve_data}}
#'
#' @param curve The curve to query.
#'
#' @examples \dontrun{iai::get_roc_curve_data(curve)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @export
get_roc_curve_data <- function(curve) {
  requires_iai_version("2.1.0", "get_roc_curve_data")
  jl_func("IAI.get_roc_curve_data_convert", curve)
}


#' Extract the underlying data from a survival curve (as returned by
#' \href{https://docs.interpretable.ai/v2.2.0/IAI-R/reference/#iai::predict}{\code{predict}}
#' or
#' \href{https://docs.interpretable.ai/v2.2.0/IAI-R/reference/#iai::get_survival_curve}{\code{get_survival_curve}})
#'
#' The data is returned as a list with two keys: \code{times} containing the
#' time for each breakpoint on the curve, and \code{coefs} containing the
#' probability for each breakpoint on the curve.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.get_survival_curve_data}{\code{IAI.get_survival_curve_data}}
#'
#' @param curve The curve to query.
#'
#' @examples \dontrun{iai::get_survival_curve_data(curve)}
#'
#' @export
get_survival_curve_data <- function(curve) {
  jl_func("IAI.get_survival_curve_data_convert", curve)
}


#' Return the fitted hazard coefficient estimate made by a model for each
#' point in the features.
#'
#' A higher hazard coefficient estimate corresponds to a smaller predicted
#' survival time.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.predict_hazard}{\code{IAI.predict_hazard}}
#'
#' @param lnr The learner or grid to use for prediction.
#' @param X The features of the data.
#'
#' @examples \dontrun{iai::predict_hazard(lnr, X)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 1.2 or higher.
#'
#' @export
predict_hazard <- function(lnr, X) {
  requires_iai_version("1.2.0", "predict_hazard")
  jl_func("IAI.predict_hazard_convert", lnr, X)
}


#' Return the expected survival time estimate made by a model for each
#' point in the features.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.predict_expected_survival_time}{\code{IAI.predict_expected_survival_time}}
#'
#' @param lnr The learner or grid to use for prediction.
#' @param X The features of the data.
#'
#' @examples \dontrun{iai::predict_expected_survival_time(lnr, X)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.0 or higher.
#'
#' @export
predict_expected_survival_time <- function(lnr, X) {
  requires_iai_version("2.0.0", "predict_expected_survival_time")
  jl_func("IAI.predict_expected_survival_time_convert", lnr, X)
}


#' Return the predicted outcome for each treatment made by a model for each
#' point in the features
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAIBase-API-Reference-1}{\code{IAI.predict_outcomes}}
#' (for prescription or policy learners as appropriate)
#'
#' @param lnr The learner or grid to use for prediction.
#' @param X The features of the data.
#' @param ... For policy learners only, the reward matrix.
#'
#' @examples \dontrun{iai::predict_outcomes(lnr, X, ...)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.0 or higher for policy learners.
#'
#' @export
predict_outcomes <- function(lnr, X, ...) {
  jl_func("IAI.predict_outcomes_convert", lnr, X, ...)
}


#' Return the treatments in ranked order of effectiveness for each point in the
#' features
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.predict_treatment_rank}{\code{IAI.predict_treatment_rank}}
#'
#' @param lnr The learner or grid to use for prediction.
#' @param X The features of the data.
#'
#' @examples \dontrun{iai::predict_treatment_rank(lnr, X)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @export
predict_treatment_rank <- function(lnr, X) {
  requires_iai_version("2.1.0", "predict_treatment_rank")
  jl_func("IAI.predict_treatment_rank_convert", lnr, X)
}


#' Return the estimated quality of each treatment in the trained model of the
#' learner for each point in the features
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.predict_treatment_outcome}{\code{IAI.predict_treatment_outcome}}
#'
#' @param lnr The learner or grid to use for prediction.
#' @param X The features of the data.
#'
#' @examples \dontrun{iai::predict_treatment_outcome(lnr, X)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @export
predict_treatment_outcome <- function(lnr, X) {
  requires_iai_version("2.1.0", "predict_treatment_outcome")
  jl_func("IAI.predict_treatment_outcome_convert", lnr, X)
}


#' Impute missing values in a dataframe using a fitted imputation model
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.transform}{\code{IAI.transform}}
#'
#' @param lnr The learner or grid to use for imputation
#' @param X The features of the data.
#'
#' @examples \dontrun{iai::transform(lnr, X)}
#'
#' @export
transform <- function(lnr, X) {
  jl_func("IAI.transform_convert", lnr, X)
}


#' Fit an imputation model using the given features and impute the missing
#' values in these features
#'
#' Similar to calling
#' \href{https://docs.interpretable.ai/v2.2.0/IAI-R/reference/#iai::fit}{\code{fit}}
#' followed by
#' \href{https://docs.interpretable.ai/v2.2.0/IAI-R/reference/#iai::transform}{\code{transform}}
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.fit_transform!}{\code{IAI.fit_transform!}}
#'
#' @param lnr The learner or grid to use for imputation
#' @param X The features of the data.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @example examples/fit_transform.R
#'
#' @export
fit_transform <- function(lnr, X, ...) {
  jl_func("IAI.fit_transform_convert", lnr, X, ...)
}


#' Train a grid using cross-validation with features and impute all missing
#' values in these features
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/IAIBase/reference/#IAI.fit_transform_cv!}{\code{IAI.fit_transform_cv!}}
#'
#' @param grid The grid to use for imputation
#' @param X The features of the data.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @example examples/fit_transform_cv.R
#'
#' @export
fit_transform_cv <- function(grid, X, ...) {
  jl_func("IAI.fit_transform_cv_convert", grid, X, ...)
}
