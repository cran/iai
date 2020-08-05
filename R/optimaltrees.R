#' Learner for training Optimal Classification Trees
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.0.0/OptimalTrees/reference/#IAI.OptimalTreeClassifier}{\code{IAI.OptimalTreeClassifier}}
#'
#' @usage optimal_tree_classifier(...)
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::optimal_tree_classifier()}
#'
#' @export
optimal_tree_classifier <- function(...) {
  set_obj_class(jl_func("IAI.OptimalTreeClassifier", ...))
}


#' Learner for training Optimal Regression Trees
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.0.0/OptimalTrees/reference/#IAI.OptimalTreeRegressor}{\code{IAI.OptimalTreeRegressor}}
#'
#' @usage optimal_tree_regressor(...)
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::optimal_tree_regressor()}
#'
#' @export
optimal_tree_regressor <- function(...) {
  set_obj_class(jl_func("IAI.OptimalTreeRegressor_convert", ...))
}


#' Learner for training Optimal Survival Trees
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.0.0/OptimalTrees/reference/#IAI.OptimalTreeSurvivalLearner}{\code{IAI.OptimalTreeSurvivalLearner}}
#'
#' @usage optimal_tree_survival_learner(...)
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::optimal_tree_survival_learner()}
#'
#' @export
optimal_tree_survival_learner <- function(...) {
  if (iai_version_less_than("2.0.0")) {
    set_obj_class(jl_func("IAI.OptimalTreeSurvivor_convert", ...))
  } else {
    set_obj_class(jl_func("IAI.OptimalTreeSurvivalLearner_convert", ...))
  }
}
#' Learner for training Optimal Survival Trees
#'
#' This function was deprecated and renamed to [optimal_tree_survival_learner()]
#' in iai 2.0.0. This is for consistency with the IAI v2.0.0 Julia release.
#'
#' @usage optimal_tree_survivor(...)
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::optimal_tree_survivor()}
#'
#' @export
#' @md
optimal_tree_survivor <- function(...) {
  lifecycle::deprecate_warn("2.0.0", "iai::optimal_tree_survivor()",
                            "optimal_tree_survival_learner()")
  optimal_tree_survival_learner(...)
}


#' Learner for training Optimal Prescriptive Trees where the prescriptions
#' should aim to minimize outcomes
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.0.0/OptimalTrees/reference/#IAI.OptimalTreePrescriptionMinimizer}{\code{IAI.OptimalTreePrescriptionMinimizer}}
#'
#' @usage optimal_tree_prescription_minimizer(...)
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::optimal_tree_prescription_minimizer()}
#'
#' @export
optimal_tree_prescription_minimizer <- function(...) {
  set_obj_class(jl_func("IAI.OptimalTreePrescriptionMinimizer_convert", ...))
}


#' Learner for training Optimal Prescriptive Trees where the prescriptions
#' should aim to maximize outcomes
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.0.0/OptimalTrees/reference/#IAI.OptimalTreePrescriptionMaximizer}{\code{IAI.OptimalTreePrescriptionMaximizer}}
#'
#' @usage optimal_tree_prescription_maximizer(...)
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::optimal_tree_prescription_maximizer()}
#'
#' @export
optimal_tree_prescription_maximizer <- function(...) {
  set_obj_class(jl_func("IAI.OptimalTreePrescriptionMaximizer_convert", ...))
}


#' Learner for training Optimal Policy Trees where the policy should aim to
#' minimize outcomes
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.0.0/OptimalTrees/reference/#IAI.OptimalTreePolicyMinimizer}{\code{IAI.OptimalTreePolicyMinimizer}}
#'
#' @usage optimal_tree_policy_minimizer(...)
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::optimal_tree_policy_minimizer()}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.0 or higher.
#'
#' @export
optimal_tree_policy_minimizer <- function(...) {
  requires_iai_version("2.0.0", "optimal_tree_policy_minimizer")
  set_obj_class(jl_func("IAI.OptimalTreePolicyMinimizer_convert", ...))
}


#' Learner for training Optimal Policy Trees where the policy should aim to
#' maximize outcomes
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.0.0/OptimalTrees/reference/#IAI.OptimalTreePolicyMaximizer}{\code{IAI.OptimalTreePolicyMaximizer}}
#'
#' @usage optimal_tree_policy_maximizer(...)
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::optimal_tree_policy_maximizer()}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.0 or higher.
#'
#' @export
optimal_tree_policy_maximizer <- function(...) {
  requires_iai_version("2.0.0", "optimal_tree_policy_maximizer")
  set_obj_class(jl_func("IAI.OptimalTreePolicyMaximizer_convert", ...))
}
