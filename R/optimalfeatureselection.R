#' Learner for conducting Optimal Feature Selection on classification problems
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/OptimalFeatureSelection/stable/reference/#IAI.OptimalFeatureSelectionClassifier}{\code{IAI.OptimalFeatureSelectionClassifier}}
#'
#' @usage optimal_feature_selection_classifier(...)
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
  jl_func("IAI.OptimalFeatureSelectionClassifier", ...)
}


#' Learner for conducting Optimal Feature Selection on regression problems
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/OptimalFeatureSelection/stable/reference/#IAI.OptimalFeatureSelectionRegressor}{\code{IAI.OptimalFeatureSelectionRegressor}}
#'
#' @usage optimal_feature_selection_regressor(...)
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
  jl_func("IAI.OptimalFeatureSelectionRegressor", ...)
}


#' Return the constant term in the prediction in the trained learner
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/OptimalFeatureSelection/stable/reference/#IAI.get_prediction_constant}{\code{IAI.get_prediction_constant}}
#'
#' @usage get_prediction_constant(lnr)
#'
#' @param lnr The learner or grid to query.
#'
#' @examples \dontrun{iai::get_prediction_constant(lnr)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 1.1 or higher.
#'
#' @export
get_prediction_constant <- function(lnr) {
  requires_iai_version("1.1.0", "get_prediction_constant")
  jl_func("IAI.get_prediction_constant", lnr)
}


#' Return the weights for numeric and categoric features used for prediction in
#' the trained learner
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/OptimalFeatureSelection/stable/reference/#IAI.get_prediction_weights}{\code{IAI.get_prediction_weights}}
#'
#' @usage get_prediction_weights(lnr)
#'
#' @param lnr The learner or grid to query.
#'
#' @examples \dontrun{iai::get_prediction_weights(lnr)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 1.1 or higher.
#'
#' @export
get_prediction_weights <- function(lnr) {
  requires_iai_version("1.1.0", "get_prediction_weights")
  out <- jl_func("IAI.get_prediction_weights", lnr)
  names(out) <- c("numeric", "categoric")
  out
}