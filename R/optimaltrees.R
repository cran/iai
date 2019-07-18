#' optimal_tree_classifier
#'
#' Learner for training Optimal Classification Trees
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/OptimalTrees/stable/reference/#IAI.OptimalTreeClassifier}{\code{IAI.OptimalTreeClassifier}}
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
  jl_func("IAI.OptimalTreeClassifier", ...)
}


#' optimal_tree_regressor
#'
#' Learner for training Optimal Regression Trees
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/OptimalTrees/stable/reference/#IAI.OptimalTreeRegressor}{\code{IAI.OptimalTreeRegressor}}
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
  jl_func("IAI.OptimalTreeRegressor", ...)
}


#' optimal_tree_survivor
#'
#' Learner for training Optimal Survival Trees
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/OptimalTrees/stable/reference/#IAI.OptimalTreeSurvivor}{\code{IAI.OptimalTreeSurvivor}}
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
  jl_func("IAI.OptimalTreeSurvivor", ...)
}


#' optimal_tree_prescription_minimizer
#'
#' Learner for training Optimal Prescriptive Trees where the prescriptions
#' should aim to minimize outcomes
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/OptimalTrees/stable/reference/#IAI.OptimalTreePrescriptionMinimizer}{\code{IAI.OptimalTreePrescriptionMinimizer}}
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
  jl_func("IAI.OptimalTreePrescriptionMinimizer", ...)
}


#' optimal_tree_prescription_maximizer
#'
#' Learner for training Optimal Prescriptive Trees where the prescriptions
#' should aim to maximize outcomes
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/OptimalTrees/stable/reference/#IAI.OptimalTreePrescriptionMaximizer}{\code{IAI.OptimalTreePrescriptionMaximizer}}
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
  jl_func("IAI.OptimalTreePrescriptionMaximizer", ...)
}
