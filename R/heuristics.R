#' Learner for training random forests for classification problems
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/Heuristics/reference/#IAI.RandomForestClassifier}{\code{IAI.RandomForestClassifier}}
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
#' \href{https://docs.interpretable.ai/v2.2.0/Heuristics/reference/#IAI.RandomForestRegressor}{\code{IAI.RandomForestRegressor}}
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
#' \href{https://docs.interpretable.ai/v2.2.0/Heuristics/reference/#IAI.RandomForestSurvivalLearner}{\code{IAI.RandomForestSurvivalLearner}}
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
#' \href{https://docs.interpretable.ai/v2.2.0/Heuristics/reference/#IAI.XGBoostClassifier}{\code{IAI.XGBoostClassifier}}
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
#' \href{https://docs.interpretable.ai/v2.2.0/Heuristics/reference/#IAI.XGBoostRegressor}{\code{IAI.XGBoostRegressor}}
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
#' \href{https://docs.interpretable.ai/v2.2.0/Heuristics/reference/#IAI.XGBoostSurvivalLearner}{\code{IAI.XGBoostSurvivalLearner}}
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
#' \href{https://docs.interpretable.ai/v2.2.0/Heuristics/reference/#IAI.write_booster}{\code{IAI.write_booster}}
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
#' \href{https://docs.interpretable.ai/v2.2.0/Heuristics/reference/#IAI.predict_shap}{\code{IAI.predict_shap}}
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


#' Learner for training GLMNet models for regression problems
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/Heuristics/reference/#IAI.GLMNetCVRegressor}{\code{IAI.GLMNetCVRegressor}}
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


#' Return the number of fits along the path in the trained learner
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.2.0/Heuristics/reference/#IAI.get_num_fits}{\code{IAI.get_num_fits}}
#'
#' @param lnr The GLMNet learner to query.
#'
#' @examples \dontrun{lnr <- iai::get_num_fits(lnr)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @export
get_num_fits <- function(lnr) {
  requires_iai_version("2.1.0", "get_num_fits")
  jl_func("IAI.get_num_fits_convert", lnr)
}
