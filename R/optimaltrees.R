#' Learner for training Optimal Classification Trees
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.2.1/OptimalTrees/reference/#IAI.OptimalTreeClassifier}{\code{IAI.OptimalTreeClassifier}}
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
#' \href{https://docs.interpretable.ai/v3.2.1/OptimalTrees/reference/#IAI.OptimalTreeRegressor}{\code{IAI.OptimalTreeRegressor}}
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
#' \href{https://docs.interpretable.ai/v3.2.1/OptimalTrees/reference/#IAI.OptimalTreeSurvivalLearner}{\code{IAI.OptimalTreeSurvivalLearner}}
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
#' in iai 1.3.0. This is for consistency with the IAI v2.0.0 Julia release.
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::optimal_tree_survivor()}
#'
#' @export
#' @md
optimal_tree_survivor <- function(...) {
  lifecycle::deprecate_warn("1.3.0", "iai::optimal_tree_survivor()",
                            "optimal_tree_survival_learner()")
  optimal_tree_survival_learner(...)
}


#' Learner for training Optimal Prescriptive Trees where the prescriptions
#' should aim to minimize outcomes
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.2.1/OptimalTrees/reference/#IAI.OptimalTreePrescriptionMinimizer}{\code{IAI.OptimalTreePrescriptionMinimizer}}
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
#' \href{https://docs.interpretable.ai/v3.2.1/OptimalTrees/reference/#IAI.OptimalTreePrescriptionMaximizer}{\code{IAI.OptimalTreePrescriptionMaximizer}}
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
#' \href{https://docs.interpretable.ai/v3.2.1/OptimalTrees/reference/#IAI.OptimalTreePolicyMinimizer}{\code{IAI.OptimalTreePolicyMinimizer}}
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
#' \href{https://docs.interpretable.ai/v3.2.1/OptimalTrees/reference/#IAI.OptimalTreePolicyMaximizer}{\code{IAI.OptimalTreePolicyMaximizer}}
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


#' Learner for training multi-task Optimal Classification Trees
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.2.1/OptimalTrees/reference/#IAI.OptimalTreeMultiClassifier}{\code{IAI.OptimalTreeMultiClassifier}}
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::optimal_tree_multi_classifier()}
#'
#' @section IAI Compatibility:
#' Requires IAI version 3.2 or higher.
#'
#' @export
optimal_tree_multi_classifier <- function(...) {
  requires_iai_version("3.2.0", "optimal_tree_multi_classifier")
  set_obj_class(jl_func("IAI.OptimalTreeMultiClassifier_convert", ...))
}


#' Learner for training multi-task Optimal Regression Trees
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.2.1/OptimalTrees/reference/#IAI.OptimalTreeMultiRegressor}{\code{IAI.OptimalTreeMultiRegressor}}
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::optimal_tree_multi_regressor()}
#'
#' @section IAI Compatibility:
#' Requires IAI version 3.2 or higher.
#'
#' @export
optimal_tree_multi_regressor <- function(...) {
  requires_iai_version("3.2.0", "optimal_tree_multi_regressor")
  set_obj_class(jl_func("IAI.OptimalTreeMultiRegressor_convert", ...))
}


#' Refit the models in the leaves of a trained learner using the supplied data
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.2.1/OptimalTrees/reference/#IAI.refit_leaves!}{\code{IAI.refit_leaves!}}
#'
#' @param lnr The learner to refit
#' @param ... Refer to the Julia documentation for available parameters
#'
#' @examples \dontrun{iai::refit_leaves(lnr, ...)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 3.0 or higher.
#'
#' @export
refit_leaves <- function(lnr, ...) {
  requires_iai_version("3.0.0", "refit_leaves")
  set_obj_class(jl_func("IAI.refit_leaves_convert", lnr, ...))
}


#' Copy the tree split structure from one learner into another and refit the
#' models in each leaf of the tree using the supplied data
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.2.1/OptimalTrees/reference/#IAI.copy_splits_and_refit_leaves!}{\code{IAI.copy_splits_and_refit_leaves!}}
#'
#' @param new_lnr The learner to modify and refit
#' @param orig_lnr The learner from which to copy the tree split structure
#' @param ... Refer to the Julia documentation for available parameters
#'
#' @examples \dontrun{iai::copy_splits_and_refit_leaves(new_lnr, orig_lnr, ...)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 3.0 or higher.
#'
#' @export
copy_splits_and_refit_leaves <- function(new_lnr, orig_lnr, ...) {
  requires_iai_version("3.0.0", "copy_splits_and_refit_leaves")
  set_obj_class(jl_func("IAI.copy_splits_and_refit_leaves_convert", new_lnr,
                        orig_lnr, ...))
}


#' Use the trained trees in a learner along with the supplied validation data to
#' determine the best value for the `cp` parameter and then prune the trees
#' according to this value
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.2.1/OptimalTrees/reference/#IAI.prune_trees!}{\code{IAI.prune_trees!}}
#'
#' @param lnr The learner to prune
#' @param ... Refer to the Julia documentation for available parameters
#'
#' @examples \dontrun{iai::prune_trees(lnr, ...)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 3.0 or higher.
#'
#' @export
prune_trees <- function(lnr, ...) {
  requires_iai_version("3.0.0", "prune_trees")
  set_obj_class(jl_func("IAI.prune_trees_convert", lnr, ...))
}
