#' Generic function for fitting a learner.
#'
#' @param obj The object controlling which method is used
#' @param ... Arguments depending on the specific method used
#'
#' @export
fit <- function(obj, ...) {
  UseMethod("fit", obj)
}
fit_common <- function(...) {
  jl_func("IAI.fit_convert", ...)
}


#' Generic function for fitting a reward estimator on features,
#' treatments and returning predicted counterfactual rewards and scores of the
#' internal estimators.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/RewardEstimation/reference/#IAI.fit_predict\%21-Tuple\%7BRewardEstimator\%7D}{\code{IAI.fit_predict!}}
#'
#' @param obj The object controlling which method is used
#' @param ... Arguments depending on the specific method used
#'
#' @export
fit_predict <- function(obj, ...) {
  UseMethod("fit_predict", obj)
}
fit_predict_common <- function(...) {
  out <- jl_func("IAI.fit_predict_convert", ...)
  process_fit_predict(out)
}
process_fit_predict <- function(out) {
  if (!iai_version_less_than("3.0.0")) {
    names(out) <- c("predictions", "score")
  } else if (!iai_version_less_than("2.1.0")) {
    names(out) <- c("rewards", "score")
  }
  out
}


#' Generic function for returning the number of fits in a trained learner
#'
#' @param obj The object controlling which method is used
#' @param ... Arguments depending on the specific method used
#'
#' @export
get_num_fits <- function(obj, ...) {
  UseMethod("get_num_fits", obj)
}
get_num_fits_common <- function(...) {
  jl_func("IAI.get_num_fits_convert", ...)
}


#' Generic function for returning the prediction constant in a trained learner
#'
#' @param obj The object controlling which method is used
#' @param ... Arguments depending on the specific method used
#'
#' @export
get_prediction_constant <- function(obj, ...) {
  UseMethod("get_prediction_constant", obj)
}
get_prediction_constant_common <- function(...) {
  jl_func("IAI.get_prediction_constant_convert", ...)
}


#' Generic function for returning the prediction weights in a trained learner
#'
#' @param obj The object controlling which method is used
#' @param ... Arguments depending on the specific method used
#'
#' @export
get_prediction_weights <- function(obj, ...) {
  UseMethod("get_prediction_weights", obj)
}
get_prediction_weights_common <- function(...) {
  out <- jl_func("IAI.get_prediction_weights_convert", ...)
  names(out) <- c("numeric", "categoric")
  out
}


#' Generic function for returning the predicted label in the node of a
#' classification tree
#'
#' @param obj The object controlling which method is used
#' @param ... Arguments depending on the specific method used
#'
#' @export
get_classification_label <- function(obj, ...) {
  UseMethod("get_classification_label", obj)
}
get_classification_label_common <- function(...) {
  jl_func("IAI.get_classification_label_convert", ...)
}


#' Generic function for returning the probabilities of class membership at a
#' node of a classification tree
#'
#' @param obj The object controlling which method is used
#' @param ... Arguments depending on the specific method used
#'
#' @export
get_classification_proba <- function(obj, ...) {
  UseMethod("get_classification_proba", obj)
}
get_classification_proba_common <- function(...) {
  jl_func("IAI.get_classification_proba_convert", ...)
}


#' Generic function for returning the constant term in the regression
#' prediction at a node of a tree
#'
#' @param obj The object controlling which method is used
#' @param ... Arguments depending on the specific method used
#'
#' @export
get_regression_constant <- function(obj, ...) {
  UseMethod("get_regression_constant", obj)
}
get_regression_constant_common <- function(...) {
  jl_func("IAI.get_regression_constant_convert", ...)
}


#' Generic function for returning the weights for each feature in the
#' regression prediction at a node of a tree
#'
#' @param obj The object controlling which method is used
#' @param ... Arguments depending on the specific method used
#'
#' @export
get_regression_weights <- function(obj, ...) {
  UseMethod("get_regression_weights", obj)
}
get_regression_weights_common <- function(obj, ...) {
  out <- jl_func("IAI.get_regression_weights_convert", obj, ...)
  if ("tree_multi_learner" %in% class(obj) && length(out[[1]]) > 0 &&
      is.list(out[[1]][[1]])) {
    out <- lapply(out, function (x) {
      names(x) <- c("numeric", "categoric")
      x
    })
  } else {
    names(out) <- c("numeric", "categoric")
  }
  out
}


#' Generic function for constructing an interactive questionnaire with
#' multiple learners
#'
#' @param obj The object controlling which method is used
#' @param ... Arguments depending on the specific method used
#'
#' @export
multi_questionnaire <- function(obj, ...) {
  UseMethod("multi_questionnaire", obj)
}
multi_questionnaire_common <- function(...) {
  set_obj_class(jl_func("IAI.MultiQuestionnaire_convert", ...))
}


#' Generic function for constructing an interactive tree visualization of
#' multiple tree learners
#'
#' @param obj The object controlling which method is used
#' @param ... Arguments depending on the specific method used
#'
#' @export
multi_tree_plot <- function(obj, ...) {
  UseMethod("multi_tree_plot", obj)
}
multi_tree_plot_common <- function(...) {
  set_obj_class(jl_func("IAI.MultiTreePlot_convert", ...))
}


#' Generic function for constructing an interactive questionnaire
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.Questionnaire-Tuple\%7BLearner\%7D}{\code{IAI.Questionnaire}}
#'
#' @param obj The object controlling which method is used
#' @param ... Arguments depending on the specific method used
#'
#' @export
questionnaire <- function(obj, ...) {
  UseMethod("questionnaire", obj)
}
questionnaire_common <- function(...) {
  set_obj_class(jl_func("IAI.Questionnaire_convert", ...))
}


#' Generic function for returning the predictions of a model
#'
#' @param obj The object controlling which method is used
#' @param ... Arguments depending on the specific method used
#'
#' @export
predict <- function(obj, ...) {
  UseMethod("predict", obj)
}
predict_common <- function(obj, ...) {
  out <- jl_func("IAI.predict_convert", obj, ...)

  if (typeof(out) == "list" && length(out) == 2) {
    if ("optimal_tree_multi_classifier" %in% class(obj) ||
        "optimal_tree_multi_regressor" %in% class(obj)) {
      # Already have names from predict
    } else {
      stopifnot("prescription_learner" %in% class(obj))
      names(out) <- c("treatments", "outcomes")
    }
  } else if ("survival_learner" %in% class(obj) &&
             jl_isa(out[1], "IAI.SurvivalCurve")) {
    out <- sapply(out, set_obj_class)
  }

  out
}


#' Generic function for returning the expected survival time predicted by a
#' model
#'
#' @param obj The object controlling which method is used
#' @param ... Arguments depending on the specific method used
#'
#' @export
predict_expected_survival_time <- function(obj, ...) {
  UseMethod("predict_expected_survival_time", obj)
}
predict_expected_survival_time_common <- function(...) {
  jl_func("IAI.predict_expected_survival_time_convert", ...)
}


#' Generic function for returning the hazard coefficient predicted by a model
#'
#' @param obj The object controlling which method is used
#' @param ... Arguments depending on the specific method used
#'
#' @export
predict_hazard <- function(obj, ...) {
  UseMethod("predict_hazard", obj)
}
predict_hazard_common <- function(...) {
  jl_func("IAI.predict_hazard_convert", ...)
}


#' Generic function for returning the outcomes predicted by a model under each
#' treatment
#'
#' @param obj The object controlling which method is used
#' @param ... Arguments depending on the specific method used
#'
#' @export
predict_outcomes <- function(obj, ...) {
  UseMethod("predict_outcomes", obj)
}
predict_outcomes_common <- function(...) {
  jl_func("IAI.predict_outcomes_convert", ...)
}


#' Generic function for returning the probabilities of class membership
#' predicted by a model
#'
#' @param obj The object controlling which method is used
#' @param ... Arguments depending on the specific method used
#'
#' @export
predict_proba <- function(obj, ...) {
  UseMethod("predict_proba", obj)
}
predict_proba_common <- function(...) {
  jl_func("IAI.predict_proba_convert", ...)
}


#' Generic function for returning the counterfactual rewards estimated by a
#' model under each treatment
#'
#' @param obj The object controlling which method is used
#' @param ... Arguments depending on the specific method used
#'
#' @export
predict_reward <- function(obj, ...) {
  UseMethod("predict_reward", obj)
}
predict_reward_common <- function(...) {
  jl_func("IAI.predict_reward_convert", ...)
}


#' Generic function for constructing an ROC curve
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.ROCCurve}{\code{IAI.ROCCurve}}
#'
#' @param obj The object controlling which method is used
#' @param ... Arguments depending on the specific method used
#'
#' @export
roc_curve <- function(obj, ...) {
  UseMethod("roc_curve", obj)
}
roc_curve_common <- function(...) {
  out <- jl_func("IAI.ROCCurve_convert", ...)
  if (is.list(out)) {
    lapply(out, set_obj_class)
  } else {
    set_obj_class(out)
  }
}


#' Generic function for calculating scores
#'
#' @param obj The object controlling which method is used
#' @param ... Arguments depending on the specific method used
#'
#' @export
score <- function(obj, ...) {
  UseMethod("score", obj)
}
score_common <- function(...) {
  jl_func("IAI.score_convert", ...)
}


#' Generic function for showing interactive visualization in browser
#'
#' @param obj The object controlling which method is used
#' @param ... Arguments depending on the specific method used
#'
#' @export
show_in_browser <- function(obj, ...) {
  UseMethod("show_in_browser", obj)
}
show_in_browser_common <- function(...) {
  jl_func("IAI.show_in_browser_convert", ...) # nocov
}


#' Generic function for showing interactive questionnaire in browser
#'
#' @param obj The object controlling which method is used
#' @param ... Arguments depending on the specific method used
#'
#' @export
show_questionnaire <- function(obj, ...) {
  UseMethod("show_questionnaire", obj)
}
show_questionnaire_common <- function(...) {
  jl_func("IAI.show_questionnaire_convert", ...) # nocov
}


#' Generic function for calculating variable importance
#'
#' @param obj The object controlling which method is used
#' @param ... Arguments depending on the specific method used
#'
#' @export
variable_importance <- function(obj, ...) {
  UseMethod("variable_importance", obj)
}
variable_importance_common <- function(...) {
  jl_func("IAI.variable_importance_convert", ...)
}


#' Generic function for writing interactive visualization to file
#'
#' @param filename Where to save the output.
#' @param obj The object controlling which method is used
#' @param ... Arguments depending on the specific method used
#'
#' @export
write_html <- function(filename, obj, ...) {
  UseMethod("write_html", obj)
}
write_html_common <- function(...) {
  jl_func("IAI.write_html_convert", ...)
}


#' Generic function for writing interactive questionnaire to file
#'
#' @param filename Where to save the output.
#' @param obj The object controlling which method is used
#' @param ... Arguments depending on the specific method used
#'
#' @export
write_questionnaire <- function(filename, obj, ...) {
  UseMethod("write_questionnaire", obj)
}
write_questionnaire_common <- function(...) {
  jl_func("IAI.write_questionnaire_convert", ...)
}
