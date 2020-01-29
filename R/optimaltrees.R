#' @export
print.optimal_tree_learner <- function(x, ...) {
  if (to_html(x)) {
    invisible(x)
  } else {
    NextMethod()
  }
}

#' Learner for training Optimal Classification Trees
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v1.2.0/OptimalTrees/reference/#IAI.OptimalTreeClassifier}{\code{IAI.OptimalTreeClassifier}}
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
#' \href{https://docs.interpretable.ai/v1.2.0/OptimalTrees/reference/#IAI.OptimalTreeRegressor}{\code{IAI.OptimalTreeRegressor}}
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
  set_obj_class(jl_func("IAI.OptimalTreeRegressor", ...))
}


#' Learner for training Optimal Survival Trees
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v1.2.0/OptimalTrees/reference/#IAI.OptimalTreeSurvivor}{\code{IAI.OptimalTreeSurvivor}}
#'
#' @usage optimal_tree_survivor(...)
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::optimal_tree_survivor()}
#'
#' @export
optimal_tree_survivor <- function(...) {
  set_obj_class(jl_func("IAI.OptimalTreeSurvivor", ...))
}


#' Learner for training Optimal Prescriptive Trees where the prescriptions
#' should aim to minimize outcomes
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v1.2.0/OptimalTrees/reference/#IAI.OptimalTreePrescriptionMinimizer}{\code{IAI.OptimalTreePrescriptionMinimizer}}
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
  set_obj_class(jl_func("IAI.OptimalTreePrescriptionMinimizer", ...))
}


#' Learner for training Optimal Prescriptive Trees where the prescriptions
#' should aim to maximize outcomes
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v1.2.0/OptimalTrees/reference/#IAI.OptimalTreePrescriptionMaximizer}{\code{IAI.OptimalTreePrescriptionMaximizer}}
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
  set_obj_class(jl_func("IAI.OptimalTreePrescriptionMaximizer", ...))
}
