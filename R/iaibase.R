#' Split the data into training and test datasets
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.split_data}{\code{IAI.split_data}}
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
  if (task %in% c("classification", "regression", "multi_classification",
                  "multi_regression")) {
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
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.fit\%21-Tuple\%7BLearner\%7D}{\code{IAI.fit!}}
#'
#' @param obj The learner to fit.
#' @param X The features of the data.
#' @param ... Other parameters, including zero or more target vectors as
#'            required by the problem type. Refer to the Julia documentation for
#'            available parameters.
#'
#' @examples \dontrun{iai::fit(lnr, X, y)}
#'
#' @export
fit.learner <- function(obj, X, ...) {
  fit_common(obj, X, ...)
}
#' Fits a \code{\link{grid_search}} to the training data
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.fit\%21-Tuple\%7BGridSearch\%7D}{\code{IAI.fit!}}
#'
#' @param obj The grid search to fit.
#' @param X The features of the data.
#' @param ... Other parameters, including zero or more target vectors as
#'            required by the problem type. Refer to the Julia documentation for
#'            available parameters.
#'
#' @example examples/fit.grid_search.R
#'
#' @export
fit.grid_search <- function(obj, X, ...) {
  fit_common(obj, X, ...)
}
#' Fits an imputation learner to the training data.
#'
#' Additional keyword arguments are available for fitting imputation learners -
#' please refer to the Julia documentation.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/OptImpute/reference/#IAI.fit\%21-Tuple\%7BImputationLearner\%7D}{\code{IAI.fit!}}
#'
#' @param obj The learner or grid to fit.
#' @param X The features of the data.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::fit(lnr, X)}
#'
#' @export
fit.imputation_learner <- function(obj, X, ...) {
  fit_common(obj, X, ...)
}


#' Return the predictions made by a supervised learner for each point in the
#' features
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.predict-Tuple\%7BSupervisedLearner\%2C\%20Union\%7BDataFrames.AbstractDataFrame\%2C\%20AbstractMatrix\%7B\%3C\%3AReal\%7D\%7D\%7D}{\code{IAI.predict}}
#'
#' @param obj The learner or grid to use for prediction.
#' @param X The features of the data.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::predict(lnr, X)}
#'
#' @export
predict.supervised_learner <- function(obj, X, ...) {
  predict_common(obj, X, ...)
}
#' Return the predictions made by a multi-task supervised learner for each point in the features
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.predict-Tuple\%7BLearner\%7BIAIBase.MultiTask\%7BT\%7D\%7D\%20where\%20T\%3C\%3AIAIBase.SupervisedTask\%2C\%20Union\%7BDataFrames.AbstractDataFrame\%2C\%20AbstractMatrix\%7B\%3C\%3AReal\%7D\%7D\%7D}{\code{IAI.predict}}
#' and
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.predict-Tuple\%7BLearner\%7BIAIBase.MultiTask\%7BT\%7D\%7D\%20where\%20T\%3C\%3AIAIBase.SupervisedTask\%2C\%20Union\%7BDataFrames.AbstractDataFrame\%2C\%20AbstractMatrix\%7B\%3C\%3AReal\%7D\%7D\%2C\%20Symbol\%7D}{\code{IAI.predict}}
#'
#' @param obj The learner or grid to use for prediction.
#' @param X The features of the data.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::predict(lnr, X)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 3.2 or higher.
#'
#' @export
predict.supervised_multi_learner <- function(obj, X, ...) {
  requires_iai_version("3.2.0", "predict", "with `supervised_multi_learner`")
  NextMethod()
}
#' Return the predictions made by a survival learner for each point in the
#' features
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.predict-Tuple\%7BSurvivalLearner\%2C\%20Union\%7BDataFrames.AbstractDataFrame\%2C\%20AbstractMatrix\%7B\%3C\%3AReal\%7D\%7D\%7D}{\code{IAI.predict}}
#'
#' @param obj The learner or grid to use for prediction.
#' @param X The features of the data.
#' @param t The time for which to predict survival probability, defaulting to
#'          returning the entire survival curve if not supplied
#' @param ... Additional arguments (unused)
#'
#' @examples \dontrun{iai::predict(lnr, X, t = 10)}
#'
#' @export
predict.survival_learner <- function(obj, X, t = NULL, ...) {
  if (is.null(t) && iai_version_less_than("2.1.0")) {
    predict_common(obj, X, ...)
  } else {
    predict_common(obj, X, t = t, ...)
  }
}


#' Calculate the score for a model on the given data
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.score-Tuple\%7BSupervisedLearner\%7D}{\code{IAI.score}}
#'
#' @param obj The learner or grid to evaluate.
#' @param X The features of the data.
#' @param ... Other parameters, including zero or more target vectors as
#'            required by the problem type. Refer to the Julia documentation for
#'            available parameters.
#'
#' @examples \dontrun{iai::score(lnr, X, y)}
#'
#' @export
score.supervised_learner <- function(obj, X, ...) {
  score_common(obj, X, ...)
}
#' Calculate the score for a multi-task model on the given data
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.score-Tuple\%7BLearner\%7BIAIBase.MultiTask\%7BT\%7D\%7D\%20where\%20T\%3C\%3AIAIBase.SupervisedTask\%7D}{\code{IAI.score}}
#' and
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.score-Tuple\%7BLearner\%7BIAIBase.MultiTask\%7BT\%7D\%7D\%20where\%20T\%3C\%3AIAIBase.SupervisedTask\%2C\%20Symbol\%7D}{\code{IAI.score}}
#'
#' @param obj The learner or grid to evaluate.
#' @param X The features of the data.
#' @param ... Other parameters, including zero or more target vectors as
#'            required by the problem type. Refer to the Julia documentation for
#'            available parameters.
#'
#' @examples \dontrun{iai::score(lnr, X, y)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 3.2 or higher.
#'
#' @export
score.supervised_multi_learner <- function(obj, X, ...) {
  requires_iai_version("3.2.0", "score", "with `supervised_multi_learner`")
  NextMethod()
}
#' Calculate the score for a set of predictions on the given data
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.score-Tuple\%7BUnion\%7BAbstractString\%2C\%20Symbol\%7D\%2C\%20Vararg\%7BAny\%7D\%7D}{\code{IAI.score}}
#'
#' @param obj The type of problem.
#' @param predictions The predictions to evaluate.
#' @param truths The true target values for these observations.
#' @param ... Other parameters, including the criterion. Refer to the Julia
#'            documentation for available parameters.
#'
#' @examples \dontrun{iai::score("regression", y_pred, y_true, criterion="mse")}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @export
score.default <- function(obj, predictions, truths, ...) {
  requires_iai_version("2.1.0", "score", "with predictions and true labels")
  score_common(obj, predictions, truths, ...)
}


#' Generate a ranking of the variables in a learner according to their
#' importance during training. The results are normalized so that they sum to
#' one.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.variable_importance-Tuple\%7BLearner\%7D}{\code{IAI.variable_importance}}
#'
#' @param obj The learner to query.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::variable_importance(lnr, ...)}
#'
#' @export
variable_importance.learner <- function(obj, ...) {
  variable_importance_common(obj, ...)
}


#' Return the names of the features used by the learner
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.get_features_used}{\code{IAI.get_features_used}}
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
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.write_json}{\code{IAI.write_json}}
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
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.read_json}{\code{IAI.read_json}}
#'
#' @param filename The location of the JSON file.
#'
#' @examples \dontrun{obj <- iai::read_json("out.json")}
#'
#' @export
read_json <- function(filename) {
  set_obj_class(jl_func("IAI.read_json_convert", filename))
}


#' Resume training from a checkpoint file
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.resume_from_checkpoint}{\code{IAI.resume_from_checkpoint}}
#'
#' @param checkpoint_file The location of the checkpoint file.
#'
#' @examples \dontrun{obj <- iai::resume_from_checkpoint("checkpoint.json")}
#'
#' @section IAI Compatibility:
#' Requires IAI version 3.1 or higher.
#'
#' @export
resume_from_checkpoint <- function(checkpoint_file) {
  requires_iai_version("3.1.0", "resume_from_checkpoint")
  out <- jl_func("IAI.resume_from_checkpoint_convert", checkpoint_file)
  if (typeof(out) == "list" && length(out) == 2) {
    list(learner = set_obj_class(out[[1]]),
         results = process_fit_predict(out[[2]]))
  } else {
    set_obj_class(out)
  }
}


#' Return the value of all parameters on a learner
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.get_params}{\code{IAI.get_params}}
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
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.set_params!}{\code{IAI.set_params!}}
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
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.clone}{\code{IAI.clone}}
#'
#' @param lnr The learner to copy.
#'
#' @examples \dontrun{new_lnr <- iai::clone(lnr)}
#'
#' @export
clone <- function(lnr) {
  set_obj_class(jl_func("IAI.clone_convert", lnr))
}


#' Show interactive visualization of an object in the default browser
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.show_in_browser-Tuple\%7BAbstractVisualization\%7D}{\code{IAI.show_in_browser}}
#'
#' @param obj The object to visualize.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::show_in_browser(lnr)}
#'
#' @export
show_in_browser.abstract_visualization <- function(obj, ...) {
  show_in_browser_common(obj, ...) # nocov
}
#' Show interactive visualization of a \code{\link{roc_curve}} in the default
#' browser
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.show_in_browser-Tuple\%7BROCCurve\%7D}{\code{IAI.show_in_browser}}
#'
#' @param obj The curve to visualize.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::show_in_browser(curve)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 1.1 or higher.
#'
#' @export
show_in_browser.roc_curve <- function(obj, ...) {
  requires_iai_version("1.1.0", "show_in_browser", "with `roc_curve`")
  show_in_browser_common(obj, ...) # nocov
}


#' Sets a global rich output parameter
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.set_rich_output_param!}{\code{IAI.set_rich_output_param!}}
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
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.get_rich_output_params}{\code{IAI.get_rich_output_params}}
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
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.delete_rich_output_param!}{\code{IAI.delete_rich_output_param!}}
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
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.GridSearch}{\code{IAI.GridSearch}}
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
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.fit_cv\%21-Tuple\%7BGridSearch\%7D}{\code{IAI.fit_cv!}}
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
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.get_learner}{\code{IAI.get_learner}}
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
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.get_best_params}{\code{IAI.get_best_params}}
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
#' in iai 1.5.0. This is for consistency with the IAI v2.2.0 Julia release.
#'
#' @param grid The grid search to query.
#'
#' @examples \dontrun{iai::get_grid_results(grid)}
#'
#' @export
get_grid_results <- function(grid) {
  lifecycle::deprecate_warn("1.5.0", "iai::get_grid_results()",
                            "get_grid_result_summary()")
  get_grid_result_summary(grid)
}


#' Return a summary of the results from the grid search
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.get_grid_result_summary}{\code{IAI.get_grid_result_summary}}
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
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.get_grid_result_details}{\code{IAI.get_grid_result_details}}
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


#' Return the probabilities of class membership predicted by a classification
#' learner for each point in the features
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.predict_proba-Tuple\%7BClassificationLearner\%2C\%20Union\%7BDataFrames.AbstractDataFrame\%2C\%20AbstractMatrix\%7B\%3C\%3AReal\%7D\%7D\%7D}{\code{IAI.predict_proba}}
#'
#' @param obj The learner or grid to use for prediction.
#' @param X The features of the data.
#' @param ... Additional arguments (unused)
#'
#' @examples \dontrun{iai::predict_proba(lnr, X)}
#'
#' @export
predict_proba.classification_learner <- function(obj, X, ...) {
  predict_proba_common(obj, X, ...)
}
#' Return the probabilities of class membership predicted by a multi-task
#' classification learner for each point in the features
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.predict_proba-Tuple\%7BLearner\%7BIAIBase.MultiTask\%7BIAIBase.ClassificationTask\%7D\%7D\%2C\%20Union\%7BDataFrames.AbstractDataFrame\%2C\%20AbstractMatrix\%7B\%3C\%3AReal\%7D\%7D\%7D}{\code{IAI.predict_proba}}
#' and
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.predict_proba-Tuple\%7BLearner\%7BIAIBase.MultiTask\%7BIAIBase.ClassificationTask\%7D\%7D\%2C\%20Union\%7BDataFrames.AbstractDataFrame\%2C\%20AbstractMatrix\%7B\%3C\%3AReal\%7D\%7D\%2C\%20Symbol\%7D}{\code{IAI.predict_proba}}
#'
#' @param obj The learner or grid to use for prediction.
#' @param X The features of the data.
#' @param ... Additional arguments (unused)
#'
#' @examples \dontrun{iai::predict_proba(lnr, X)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 3.2 or higher.
#'
#' @export
predict_proba.classification_multi_learner <- function(obj, X, ...) {
  requires_iai_version("3.2.0", "predict_proba",
                       "with `classification_multi_learner`")
  NextMethod()
}


#' Construct an ROC curve using a trained classification learner on the given
#' data
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.ROCCurve-Tuple\%7BClassificationLearner\%2C\%20Union\%7BDataFrames.AbstractDataFrame\%2C\%20AbstractMatrix\%7B\%3C\%3AReal\%7D\%7D\%2C\%20AbstractVector\%7D}{\code{IAI.ROCCurve}}
#'
#' @param obj The learner or grid to use for prediction.
#' @param X The features of the data.
#' @param y The labels of the data.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::roc_curve(lnr, X, y)}
#'
#' @export
roc_curve.classification_learner <- function(obj, X, y, ...) {
  roc_curve_common(obj, X, y, ...)
}
#' Construct an ROC curve using a trained multi-task classification learner on
#' the given data
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.ROCCurve-Tuple\%7BLearner\%7BIAIBase.MultiTask\%7BIAIBase.ClassificationTask\%7D\%7D\%2C\%20Union\%7BDataFrames.AbstractDataFrame\%2C\%20AbstractMatrix\%7B\%3C\%3AReal\%7D\%7D\%2C\%20Union\%7BDataFrames.AbstractDataFrame\%2C\%20AbstractMatrix\%7D\%7D}{\code{IAI.ROCCurve}}
#' and
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.ROCCurve-Tuple\%7BLearner\%7BIAIBase.MultiTask\%7BIAIBase.ClassificationTask\%7D\%7D\%2C\%20Union\%7BDataFrames.AbstractDataFrame\%2C\%20AbstractMatrix\%7B\%3C\%3AReal\%7D\%7D\%2C\%20Union\%7BDataFrames.AbstractDataFrame\%2C\%20AbstractMatrix\%7D\%2C\%20Symbol\%7D}{\code{IAI.ROCCurve}}
#'
#' @param obj The learner or grid to use for prediction.
#' @param X The features of the data.
#' @param y The labels of the data.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::roc_curve(lnr, X, y)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 3.2 or higher.
#'
#' @export
roc_curve.classification_multi_learner <- function(obj, X, y, ...) {
  requires_iai_version("3.2.0", "roc_curve",
                       "with `classification_multi_learner`")
  NextMethod()
}
#' Construct an ROC curve from predicted probabilities and true labels
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.ROCCurve-Tuple\%7BAbstractVector\%7B\%3C\%3AReal\%7D\%2C\%20AbstractVector\%7D}{\code{IAI.ROCCurve}}
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
  roc_curve_common(obj, y, positive_label = positive_label)
}


#' Construct a \href{https://ggplot2.tidyverse.org/reference/ggplot.html}{\code{ggplot2::ggplot}} object plotting the ROC curve
#'
#' @param object The ROC curve to plot
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
autoplot.roc_curve <- function(object, ...) {
  requires_iai_version("2.1.0", "ggplot2::autoplot.roc_curve")

  d <- get_roc_curve_data(object)

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


#' Extract the underlying data from an ROC curve
#'
#' ROC curves are returned by \code{roc_curve}, e.g.
#' \code{\link{roc_curve.classification_learner}}
#'
#' The data is returned as a list with two keys: \code{auc} giving the
#' area-under-the-curve, and \code{coords} containing a vector of lists
#' representing each point on the curve, each with keys \code{fpr} (the false
#' positive rate), \code{tpr} (the true positive rate) and \code{threshold}
#' (the threshold).
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.get_roc_curve_data}{\code{IAI.get_roc_curve_data}}
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
#' \code{\link{predict.survival_learner}}
#' or
#' \code{\link{get_survival_curve}})
#'
#' The data is returned as a list with two keys: \code{times} containing the
#' time for each breakpoint on the curve, and \code{coefs} containing the
#' probability for each breakpoint on the curve.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.get_survival_curve_data}{\code{IAI.get_survival_curve_data}}
#'
#' @param curve The curve to query.
#'
#' @examples \dontrun{iai::get_survival_curve_data(curve)}
#'
#' @export
get_survival_curve_data <- function(curve) {
  jl_func("IAI.get_survival_curve_data_convert", curve)
}


#' Return the fitted hazard coefficient estimate made by a survival learner for
#' each point in the features.
#'
#' A higher hazard coefficient estimate corresponds to a smaller predicted
#' survival time.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.predict_hazard-Tuple\%7BSurvivalLearner\%2C\%20Union\%7BDataFrames.AbstractDataFrame\%2C\%20AbstractMatrix\%7B\%3C\%3AReal\%7D\%7D\%7D}{\code{IAI.predict_hazard}}
#'
#' @param obj The learner or grid to use for prediction.
#' @param X The features of the data.
#' @param ... Additional arguments (unused)
#'
#' @examples \dontrun{iai::predict_hazard(lnr, X)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 1.2 or higher.
#'
#' @export
predict_hazard.survival_learner <- function(obj, X, ...) {
  requires_iai_version("1.2.0", "predict_hazard", "with `survival_learner`")
  predict_hazard_common(obj, X, ...)
}


#' Return the expected survival time estimate made by a survival learner for
#' each point in the features.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.predict_expected_survival_time-Tuple\%7BSurvivalLearner\%2C\%20Union\%7BDataFrames.AbstractDataFrame\%2C\%20AbstractMatrix\%7B\%3C\%3AReal\%7D\%7D\%7D}{\code{IAI.predict_expected_survival_time}}
#'
#' @param obj The learner or grid to use for prediction.
#' @param X The features of the data.
#' @param ... Additional arguments (unused)
#'
#' @examples \dontrun{iai::predict_expected_survival_time(lnr, X)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.0 or higher.
#'
#' @export
predict_expected_survival_time.survival_learner <- function(obj, X, ...) {
  requires_iai_version("2.0.0", "predict_expected_survival_time",
                       "with `survival_learner`")
  predict_expected_survival_time_common(obj, X, ...)
}
#' Return the expected survival time estimate made by a survival curve
#' (as returned by
#' \code{\link{predict.survival_learner}}
#' or
#' \code{\link{get_survival_curve}})
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.predict_expected_survival_time-Tuple\%7BSurvivalCurve\%7D}{\code{IAI.predict_expected_survival_time}}
#'
#' @param obj The survival curve to use for prediction.
#' @param ... Additional arguments (unused)
#'
#' @examples \dontrun{iai::predict_expected_survival_time(curve)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.2 or higher.
#'
#' @export
predict_expected_survival_time.survival_curve <- function(obj, ...) {
  requires_iai_version("2.2.0", "predict_expected_survival_time",
                       "with `survival_curve`")
  predict_expected_survival_time_common(obj, ...)
}


#' Return the predicted outcome for each treatment made by a prescription
#' learner for each point in the features
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.predict_outcomes-Tuple\%7BPrescriptionLearner\%2C\%20Union\%7BDataFrames.AbstractDataFrame\%2C\%20AbstractMatrix\%7B\%3C\%3AReal\%7D\%7D\%7D}{\code{IAI.predict_outcomes}}
#'
#' @param obj The learner or grid to use for prediction.
#' @param X The features of the data.
#' @param ... Additional arguments (unused)
#'
#' @examples \dontrun{iai::predict_outcomes(lnr, X)}
#'
#' @export
predict_outcomes.prescription_learner <- function(obj, X, ...) {
  predict_outcomes_common(obj, X, ...)
}
#' Return the predicted outcome for each treatment made by a policy
#' learner for each point in the features
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.predict_outcomes-Tuple\%7BPolicyLearner\%2C\%20Union\%7BDataFrames.AbstractDataFrame\%2C\%20AbstractMatrix\%7B\%3C\%3AReal\%7D\%7D\%2C\%20Union\%7BDataFrames.AbstractDataFrame\%2C\%20AbstractMatrix\%7B\%3C\%3AReal\%7D\%7D\%7D}{\code{IAI.predict_outcomes}}
#'
#' @param obj The learner or grid to use for prediction.
#' @param X The features of the data.
#' @param rewards The estimated reward matrix for the data.
#' @param ... Additional arguments (unused)
#'
#' @examples \dontrun{iai::predict_outcomes(lnr, X, rewards)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.0 or higher
#'
#' @export
predict_outcomes.policy_learner <- function(obj, X, rewards, ...) {
  requires_iai_version("2.0.0", "predict_outcomes", "with `policy_learner`")
  predict_outcomes_common(obj, X, rewards, ...)
}


#' Return the treatments in ranked order of effectiveness for each point in the
#' features
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.predict_treatment_rank}{\code{IAI.predict_treatment_rank}}
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
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.predict_treatment_outcome}{\code{IAI.predict_treatment_outcome}}
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


#' Return the standard error for the estimated quality of each treatment in the
#' trained model of the learner for each point in the features
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.predict_treatment_outcome_standard_error}{\code{IAI.predict_treatment_outcome_standard_error}}
#'
#' @param lnr The learner or grid to use for prediction.
#' @param X The features of the data.
#'
#' @examples \dontrun{iai::predict_treatment_outcome_standard_error(lnr, X)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 3.2 or higher.
#'
#' @export
predict_treatment_outcome_standard_error <- function(lnr, X) {
  requires_iai_version("3.2.0", "predict_treatment_outcome_standard_error")
  jl_func("IAI.predict_treatment_outcome_standard_error_convert", lnr, X)
}


#' Impute missing values in a dataframe using a fitted imputation model
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.transform}{\code{IAI.transform}}
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
#' \code{\link{fit.imputation_learner}}
#' followed by
#' \code{\link{transform}}
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.fit_transform!}{\code{IAI.fit_transform!}}
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
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.fit_transform_cv!}{\code{IAI.fit_transform_cv!}}
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


#' Output an object as an interactive browser visualization in HTML format
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.write_html-Tuple\%7BAny\%2C\%20AbstractVisualization\%7D}{\code{IAI.write_html}}
#'
#' @param filename Where to save the output.
#' @param obj The object to output.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::write_html(file.path(tempdir(), "out.html"), lnr)}
#'
#' @export
write_html.abstract_visualization <- function(filename, obj, ...) {
  write_html_common(filename, obj, ...)
}
#' Output an ROC curve as an interactive browser visualization in HTML format
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.write_html-Tuple\%7BAny\%2C\%20ROCCurve\%7D}{\code{IAI.write_html}}
#'
#' @param filename Where to save the output.
#' @param obj The curve to output.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::write_html(file.path(tempdir(), "roc.html"), lnr)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 1.1 or higher.
#'
#' @export
write_html.roc_curve <- function(filename, obj, ...) {
  requires_iai_version("1.1.0", "write_html", "with `roc_curve`")
  write_html_common(filename, obj, ...)
}


#' Construct an interactive questionnaire from multiple specified learners
#'
#' Refer to the
#' \href{https://docs.interpretable.ai/v3.1.1/IAI-R/julia/#R-Interactive-Visualizations-1}{documentation on advanced tree visualization}
#' for more information.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.MultiQuestionnaire-Tuple\%7BPair\%7D}{\code{IAI.MultiQuestionnaire}}
#'
#' @param obj The questions to visualize. Refer to the \href{https://docs.interpretable.ai/v3.1.1/IAITrees/visualization/#multivis-1}{Julia documentation on multi-learner visualizations} for more information.
#' @param ... Additional arguments (unused)
#'
#' @examples
#' \dontrun{
#' iai::multi_questionnaire(list("Questionnaire for" = list(
#'    "first learner" = lnr1,
#'    "second learner" = lnr2
#' )))
#' }
#'
#' @section IAI Compatibility:
#' Requires IAI version 1.1 or higher.
#'
#' @export
multi_questionnaire.default <- function(obj, ...) {
  requires_iai_version("1.1.0", "multi_questionnaire", "with `list`")
  multi_questionnaire_common(obj, ...)
}


#' Construct an interactive tree questionnaire using multiple learners from the
#' results of a grid search
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.MultiQuestionnaire-Tuple\%7BGridSearch\%7D}{\code{IAI.MultiQuestionnaire}}
#'
#' @param obj The grid to visualize
#' @param ... Additional arguments (unused)
#'
#' @examples \dontrun{iai::multi_questionnaire(grid)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.0 or higher.
#'
#' @export
multi_questionnaire.grid_search <- function(obj, ...) {
  requires_iai_version("2.0.0", "multi_questionnaire", "with `grid_search`")
  multi_questionnaire_common(obj, ...)
}
