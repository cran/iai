#' Split the data into training and test datasets
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.split_data}{\code{IAI.split_data}}
#'
#' @usage split_data(task, X, ...)
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
    names(out$train) <- names(out$test) <- c("X", "treatments", "outcomes")
  } else if (task %in% c("imputation")) {
    names(out$train) <- names(out$test) <- c("X")
  }

  out
}


#' Fits a model to the training data
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.fit!}{\code{IAI.fit!}}
#'
#' @usage fit(lnr, X, ...)
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.predict}{\code{IAI.predict}}
#'
#' @usage predict(lnr, X, ...)
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.score}{\code{IAI.score}}
#'
#' @usage score(lnr, X, ...)
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.variable_importance}{\code{IAI.variable_importance}}
#'
#' @usage variable_importance(lnr)
#'
#' @param lnr The learner to query.
#'
#' @examples \dontrun{iai::variable_importance(lnr)}
#'
#' @export
variable_importance <- function(lnr) {
  jl_func("IAI.variable_importance_convert", lnr)
}


#' Output a learner or grid in JSON format
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.write_json}{\code{IAI.write_json}}
#'
#' @usage write_json(filename, obj, ...)
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.read_json}{\code{IAI.read_json}}
#'
#' @usage read_json(filename)
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.get_params}{\code{IAI.get_params}}
#'
#' @usage get_params(lnr)
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.set_params!}{\code{IAI.set_params!}}
#'
#' @usage set_params(lnr, ...)
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.clone}{\code{IAI.clone}}
#'
#' @usage clone(lnr)
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.show_in_browser}{\code{IAI.show_in_browser}}
#'
#' @usage show_in_browser(obj, ...)
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.set_rich_output_param!}{\code{IAI.set_rich_output_param!}}
#'
#' @usage set_rich_output_param(key, value)
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.get_rich_output_params}{\code{IAI.get_rich_output_params}}
#'
#' @usage get_rich_output_params()
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.delete_rich_output_param!}{\code{IAI.delete_rich_output_param!}}
#'
#' @usage delete_rich_output_param(key)
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.GridSearch}{\code{IAI.GridSearch}}
#'
#' @usage grid_search(lnr, ...)
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.fit_cv!}{\code{IAI.fit_cv!}}
#'
#' @usage fit_cv(grid, X, ...)
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.get_learner}{\code{IAI.get_learner}}
#'
#' @usage get_learner(grid)
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.get_best_params}{\code{IAI.get_best_params}}
#'
#' @usage get_best_params(grid)
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
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.get_grid_results}{\code{IAI.get_grid_results}}
#'
#' @usage get_grid_results(grid)
#'
#' @param grid The grid search to query.
#'
#' @examples \dontrun{iai::get_grid_results(grid)}
#'
#' @export
get_grid_results <- function(grid) {
  jl_func("IAI.get_grid_results_convert", grid)
}


#' Return the probabilities of class membership predicted by a model for each
#' point in the features
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.predict_proba}{\code{IAI.predict_proba}}
#'
#' @usage predict_proba(lnr, X)
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.ROCCurve}{\code{IAI.ROCCurve}}
#'
#' @usage roc_curve(obj, ...)
#'
#' @param obj The object controlling which method is used
#' @param ... Arguments depending on the specific method used
#'
#' @export
roc_curve <- function (obj, ...) {
  UseMethod("roc_curve", obj)
}


#' Construct an ROC curve using a trained model on the given data
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.ROCCurve}{\code{IAI.ROCCurve}}
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.ROCCurve}{\code{IAI.ROCCurve}}
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


#' Extract the underlying data from an ROC curve (as returned by
#' \href{https://docs.interpretable.ai/v2.1.0/IAI-R/reference/#iai::roc_curve}{\code{roc_curve}})
#'
#' The data is returned as a list with two keys: \code{auc} giving the
#' area-under-the-curve, and \code{coords} containing a vector of lists
#' representing each point on the curve, each with keys \code{fpr} (the false
#' positive rate), \code{tpr} (the true positive rate) and \code{threshold}
#' (the threshold).
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.get_roc_curve_data}{\code{IAI.get_roc_curve_data}}
#'
#' @usage get_roc_curve_data(curve)
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAI-R/reference/#iai::predict}{\code{predict}}
#' or
#' \href{https://docs.interpretable.ai/v2.1.0/IAI-R/reference/#iai::get_survival_curve}{\code{get_survival_curve}})
#'
#' The data is returned as a list with two keys: \code{times} containing the
#' time for each breakpoint on the curve, and \code{coefs} containing the
#' probability for each breakpoint on the curve.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.get_survival_curve_data}{\code{IAI.get_survival_curve_data}}
#'
#' @usage get_survival_curve_data(curve)
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.predict_hazard}{\code{IAI.predict_hazard}}
#'
#' @usage predict_hazard(lnr, X)
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.predict_expected_survival_time}{\code{IAI.predict_expected_survival_time}}
#'
#' @usage predict_expected_survival_time(lnr, X)
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAIBase-API-Reference-1}{\code{IAI.predict_outcomes}}
#' (for prescription or policy learners as appropriate)
#'
#' @usage predict_outcomes(lnr, X, ...)
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.predict_treatment_rank}{\code{IAI.predict_treatment_rank}}
#'
#' @usage predict_treatment_rank(lnr, X)
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.predict_treatment_outcome}{\code{IAI.predict_treatment_outcome}}
#'
#' @usage predict_treatment_outcome(lnr, X)
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.transform}{\code{IAI.transform}}
#'
#' @usage transform(lnr, X)
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAI-R/reference/#iai::fit}{\code{fit}}
#' followed by
#' \href{https://docs.interpretable.ai/v2.1.0/IAI-R/reference/#iai::transform}{\code{transform}}
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.fit_transform!}{\code{IAI.fit_transform!}}
#'
#' @usage fit_transform(lnr, X, ...)
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
#' \href{https://docs.interpretable.ai/v2.1.0/IAIBase/reference/#IAI.fit_transform_cv!}{\code{IAI.fit_transform_cv!}}
#'
#' @usage fit_transform_cv(grid, X, ...)
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
