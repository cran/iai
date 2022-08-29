#' Learner for training random forests for classification problems
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/Heuristics/reference/#IAI.RandomForestClassifier}{\code{IAI.RandomForestClassifier}}
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::random_forest_classifier()}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @export
random_forest_classifier <- function(...) {
  requires_iai_version("2.1.0", "random_forest_classifier")
  set_obj_class(jl_func("IAI.RandomForestClassifier_convert", ...))
}


#' Learner for training random forests for regression problems
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/Heuristics/reference/#IAI.RandomForestRegressor}{\code{IAI.RandomForestRegressor}}
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::random_forest_regressor()}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @export
random_forest_regressor <- function(...) {
  requires_iai_version("2.1.0", "random_forest_regressor")
  set_obj_class(jl_func("IAI.RandomForestRegressor_convert", ...))
}


#' Learner for training random forests for survival problems
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/Heuristics/reference/#IAI.RandomForestSurvivalLearner}{\code{IAI.RandomForestSurvivalLearner}}
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::random_forest_survival_learner()}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.2 or higher.
#'
#' @export
random_forest_survival_learner <- function(...) {
  requires_iai_version("2.2.0", "random_forest_survival_learner")
  set_obj_class(jl_func("IAI.RandomForestSurvivalLearner_convert", ...))
}


#' Learner for training XGBoost models for classification problems
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/Heuristics/reference/#IAI.XGBoostClassifier}{\code{IAI.XGBoostClassifier}}
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::xgboost_classifier()}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @export
xgboost_classifier <- function(...) {
  requires_iai_version("2.1.0", "xgboost_classifier")
  set_obj_class(jl_func("IAI.XGBoostClassifier_convert", ...))
}


#' Learner for training XGBoost models for regression problems
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/Heuristics/reference/#IAI.XGBoostRegressor}{\code{IAI.XGBoostRegressor}}
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::xgboost_regressor()}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @export
xgboost_regressor <- function(...) {
  requires_iai_version("2.1.0", "xgboost_regressor")
  set_obj_class(jl_func("IAI.XGBoostRegressor_convert", ...))
}


#' Learner for training XGBoost models for survival problems
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/Heuristics/reference/#IAI.XGBoostSurvivalLearner}{\code{IAI.XGBoostSurvivalLearner}}
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::xgboost_survival_learner()}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.2 or higher.
#'
#' @export
xgboost_survival_learner <- function(...) {
  requires_iai_version("2.2.0", "xgboost_survival_learner")
  set_obj_class(jl_func("IAI.XGBoostSurvivalLearner_convert", ...))
}


#' Write the internal booster saved in the learner to file
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/Heuristics/reference/#IAI.write_booster}{\code{IAI.write_booster}}
#'
#' @param filename Where to save the output.
#' @param lnr The XGBoost learner with the booster to output.
#'
#' @examples \dontrun{iai::write_booster(file.path(tempdir(), "out.json"), lnr)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @export
write_booster <- function(filename, lnr) {
  requires_iai_version("2.1.0", "write_booster")
  jl_func("IAI.write_booster_convert", filename, lnr)
}


#' Calculate SHAP values for all points in the features using the learner
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/Heuristics/reference/#IAI.predict_shap}{\code{IAI.predict_shap}}
#'
#' @param lnr The XGBoost learner or grid to use for prediction.
#' @param X The features of the data.
#'
#' @examples \dontrun{iai::predict_shap(lnr, X)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.2 or higher.
#'
#' @export
predict_shap <- function(lnr, X) {
  requires_iai_version("2.2.0", "predict_shap")
  out <- jl_func("IAI.predict_shap_convert", lnr, X)
  if (!is.matrix(out$shap_values)) {
    out$shap_values <- as.list(out$shap_values)
  }
  out
}


#' Learner for training GLMNet models for regression problems with
#' cross-validation
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/Heuristics/reference/#IAI.GLMNetCVRegressor}{\code{IAI.GLMNetCVRegressor}}
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::glmnetcv_regressor()}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @export
glmnetcv_regressor <- function(...) {
  requires_iai_version("2.1.0", "glmnetcv_regressor")
  set_obj_class(jl_func("IAI.GLMNetCVRegressor_convert", ...))
}


#' Learner for training GLMNet models for classification problems with
#' cross-validation
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/Heuristics/reference/#IAI.GLMNetCVClassifier}{\code{IAI.GLMNetCVClassifier}}
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::glmnetcv_classifier()}
#'
#' @section IAI Compatibility:
#' Requires IAI version 3.0 or higher.
#'
#' @export
glmnetcv_classifier <- function(...) {
  requires_iai_version("3.0.0", "glmnetcv_classifier")
  set_obj_class(jl_func("IAI.GLMNetCVClassifier_convert", ...))
}


#' Construct an ROC curve using a trained \code{\link{glmnetcv_classifier}}
#' on the given data
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/Heuristics/reference/#IAI.ROCCurve-Tuple\%7BGLMNetCVClassifier\%2C\%20Union\%7BDataFrames.AbstractDataFrame\%2C\%20AbstractMatrix\%7B\%3C\%3AReal\%7D\%7D\%2C\%20AbstractVector\%7D}{\code{IAI.ROCCurve}}
#'
#' @param obj The learner or grid to use for prediction.
#' @param X The features of the data.
#' @param y The labels of the data.
#' @param fit_index The index of the fit in the path to use for prediction,
#'                  defaulting to the best fit if not supplied.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::roc_curve(lnr, X, y)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 3.0 or higher.
#'
#' @export
roc_curve.glmnetcv_classifier <- function(obj, X, y, fit_index = NULL, ...) {
  requires_iai_version("3.0.0", "roc_curve", "with `glmnetcv_classifier`")
  roc_curve_common(obj, X, y, fit_index = fit_index, ...)
}


#' Return the probabilities of class membership predicted by a
#' \code{\link{glmnetcv_classifier}} learner for each point in the features
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/Heuristics/reference/#IAI.predict_proba-Tuple\%7BGLMNetCVClassifier\%2C\%20Union\%7BDataFrames.AbstractDataFrame\%2C\%20AbstractMatrix\%7B\%3C\%3AReal\%7D\%7D\%7D}{\code{IAI.predict_proba}}
#'
#' @param obj The learner or grid to use for prediction.
#' @param X The features of the data.
#' @param fit_index The index of the fit in the path to use for prediction,
#'                  defaulting to the best fit if not supplied.
#' @param ... Additional arguments (unused)
#'
#' @examples \dontrun{iai::predict_proba(lnr, X)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 3.0 or higher.
#'
#' @export
predict_proba.glmnetcv_classifier <- function(obj, X, fit_index = NULL, ...) {
  requires_iai_version("3.0.0", "predict_proba", "with `glmnetcv_classifier`")
  predict_proba_common(obj, X, fit_index = fit_index, ...)
}


#' Learner for training GLMNet models for survival problems with
#' cross-validation
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/Heuristics/reference/#IAI.GLMNetCVSurvivalLearner}{\code{IAI.GLMNetCVSurvivalLearner}}
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::glmnetcv_survival_learner()}
#'
#' @section IAI Compatibility:
#' Requires IAI version 3.0 or higher.
#'
#' @export
glmnetcv_survival_learner <- function(...) {
  requires_iai_version("3.0.0", "glmnetcv_survival_learner")
  set_obj_class(jl_func("IAI.GLMNetCVSurvivalLearner_convert", ...))
}


#' Return the expected survival time estimate made by a
#' \code{\link{glmnetcv_survival_learner}} for each point in the features.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/Heuristics/reference/#IAI.predict_expected_survival_time-Tuple\%7BGLMNetCVSurvivalLearner\%2C\%20Union\%7BDataFrames.AbstractDataFrame\%2C\%20AbstractMatrix\%7B\%3C\%3AReal\%7D\%7D\%7D}{\code{IAI.predict_expected_survival_time}}
#'
#' @param obj The learner or grid to use for prediction.
#' @param X The features of the data.
#' @param fit_index The index of the fit in the path to use for prediction,
#'                  defaulting to the best fit if not supplied.
#' @param ... Additional arguments (unused)
#'
#' @examples \dontrun{iai::predict_expected_survival_time(lnr, X)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 3.0 or higher.
#'
#' @export
predict_expected_survival_time.glmnetcv_survival_learner <- function(
    obj, X, fit_index = NULL, ...
) {
  requires_iai_version("3.0.0", "predict_expected_survival_time",
                       "with `glmnetcv_survival_learner`")
  predict_expected_survival_time_common(obj, X, fit_index = fit_index, ...)
}
#' Return the fitted hazard coefficient estimate made by a
#' \code{\link{glmnetcv_survival_learner}} for each point in the features.
#'
#' A higher hazard coefficient estimate corresponds to a smaller predicted
#' survival time.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/Heuristics/reference/#IAI.predict_hazard-Tuple\%7BGLMNetCVSurvivalLearner\%2C\%20Union\%7BDataFrames.AbstractDataFrame\%2C\%20AbstractMatrix\%7B\%3C\%3AReal\%7D\%7D\%7D}{\code{IAI.predict_hazard}}
#'
#' @param obj The learner or grid to use for prediction.
#' @param X The features of the data.
#' @param fit_index The index of the fit in the path to use for prediction,
#'                  defaulting to the best fit if not supplied.
#' @param ... Additional arguments (unused)
#'
#' @examples \dontrun{iai::predict_hazard(lnr, X)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 3.0 or higher.
#'
#' @export
predict_hazard.glmnetcv_survival_learner <- function(obj, X, fit_index = NULL,
                                                     ...) {
  requires_iai_version("3.0.0", "predict_hazard",
                       "with `glmnetcv_survival_learner`")
  predict_hazard_common(obj, X, fit_index = fit_index, ...)
}


#' Return the number of fits along the path in a trained GLMNet learner
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/Heuristics/reference/#IAI.get_num_fits-Tuple\%7BGLMNetCVLearner\%7D}{\code{IAI.get_num_fits}}
#'
#' @param obj The GLMNet learner to query.
#' @param ... Additional arguments (unused)
#'
#' @examples \dontrun{lnr <- iai::get_num_fits(lnr)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @export
get_num_fits.glmnetcv_learner <- function(obj, ...) {
  requires_iai_version("2.1.0", "get_num_fits", "with `glmnetcv_learner`")
  get_num_fits_common(obj, ...)
}


#' Return the constant term in the prediction in a trained
#' GLMNet learner
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/Heuristics/reference/#IAI.get_prediction_constant-Tuple\%7BGLMNetCVLearner\%7D}{\code{IAI.get_prediction_constant}}
#'
#' @param obj The learner to query.
#' @param fit_index The index of the fit in the path to use for prediction,
#'                  defaulting to the best fit if not supplied.
#' @param ... Additional arguments (unused)
#'
#' @examples \dontrun{iai::get_prediction_constant(lnr)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @export
get_prediction_constant.glmnetcv_learner <- function(obj, fit_index = NULL,
                                                      ...) {
  requires_iai_version("2.1.0", "get_prediction_constant",
                       "with `glmnet_cv_learner")
  if (iai_version_less_than("3.0.0")) {
    get_prediction_constant_common(obj, fit_index, ...)
  } else {
    get_prediction_constant_common(obj, fit_index = fit_index, ...)
  }
}


#' Return the weights for numeric and categoric features used for prediction in
#' a trained GLMNet learner
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/Heuristics/reference/#IAI.get_prediction_weights-Tuple\%7BGLMNetCVLearner\%7D}{\code{IAI.get_prediction_weights}}
#'
#' @param obj The learner to query.
#' @param fit_index The index of the fit in the path to use for prediction,
#'                  defaulting to the best fit if not supplied.
#' @param ... Additional arguments (unused)
#'
#' @examples \dontrun{iai::get_prediction_weights(lnr)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @export
get_prediction_weights.glmnetcv_learner <- function(obj, fit_index = NULL,
                                                     ...) {
  requires_iai_version("2.1.0", "get_prediction_weights",
                       "with `glmnet_cv_learner")
  if (iai_version_less_than("3.0.0")) {
    get_prediction_weights_common(obj, fit_index, ...)
  } else {
    get_prediction_weights_common(obj, fit_index = fit_index, ...)
  }
}


#' Calculate the score for a GLMNet learner on the given data
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/Heuristics/reference/#IAI.score-Tuple\%7BGLMNetCVLearner\%7D}{\code{IAI.score}}
#'
#' @param obj The learner or grid to evaluate.
#' @param X The features of the data.
#' @param ... Other parameters, including zero or more target vectors as
#'            required by the problem type. \code{fit_index} can be used to
#'            specify the index of the fit in the path to use for prediction,
#'            defaulting to the best fit if not supplied. Refer to the Julia
#'            documentation for other available parameters.
#'
#' @examples \dontrun{iai::score(lnr, X, y, fit_index=1)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @export
score.glmnetcv_learner <- function(obj, X, ...) {
  requires_iai_version("2.1.0", "score", "with `glmnetcv_learner`")
  score_common(obj, X, ...)
}


#' Return the predictions made by a GLMNet learner for each point in the
#' features
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/Heuristics/reference/#IAI.predict-Tuple\%7BGLMNetCVLearner\%2C\%20Union\%7BDataFrames.AbstractDataFrame\%2C\%20AbstractMatrix\%7B\%3C\%3AReal\%7D\%7D\%7D}{\code{IAI.predict}}
#'
#' @param obj The learner or grid to use for prediction.
#' @param X The features of the data.
#' @param fit_index The index of the fit in the path to use for prediction,
#'                  defaulting to the best fit if not supplied.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::predict(lnr, X)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @export
predict.glmnetcv_learner <- function(obj, X, fit_index = NULL, ...) {
  requires_iai_version("2.1.0", "predict", "with `glmnetcv_learner`")
  if (iai_version_less_than("3.0.0")) {
    predict_common(obj, X, fit_index, ...)
  } else {
    predict_common(obj, X, fit_index = fit_index, ...)
  }
}
