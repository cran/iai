#' impute
#'
#' Impute missing values using either a specified method or through validation
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/OptImpute/stable/reference/#IAI.impute}{\code{IAI.impute}}
#'
#' @usage impute(X, ...)
#'
#' @param X The dataframe in which to impute missing values.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @example examples/impute.R
#'
#' @export
impute <- function(X, ...) {
  jl_func("IAI.impute", X, ...)
}


#' impute_cv
#'
#' Impute missing values using cross validation
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/OptImpute/stable/reference/#IAI.impute_cv}{\code{IAI.impute_cv}}
#'
#' @usage impute_cv(X, ...)
#'
#' @param X The dataframe in which to impute missing values.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @example examples/impute_cv.R
#'
#' @export
impute_cv <- function(X, ...) {
  jl_func("IAI.impute_cv", X, ...)
}


#' imputation_learner
#'
#' Generic learner for imputing missing values
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/OptImpute/stable/reference/#IAI.ImputationLearner}{\code{IAI.ImputationLearner}}
#'
#' @usage imputation_learner(method = "opt_knn", ...)
#'
#' @param method (optional) Specifies the imputation method to use.
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::imputation_learner(method = "opt_tree")}
#'
#' @export
imputation_learner <- function(method = "opt_knn", ...) {
  jl_func("IAI.ImputationLearner", method, ...)
}


#' opt_knn_imputation_learner
#'
#' Learner for conducting optimal k-NN imputation
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/OptImpute/stable/reference/#IAI.OptKNNImputationLearner}{\code{IAI.OptKNNImputationLearner}}
#'
#' @usage opt_knn_imputation_learner(...)
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::opt_knn_imputation_learner()}
#'
#' @export
opt_knn_imputation_learner <- function(...) {
  jl_func("IAI.OptKNNImputationLearner", ...)
}


#' opt_svm_imputation_learner
#'
#' Learner for conducting optimal SVM imputation
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/OptImpute/stable/reference/#IAI.OptSVMImputationLearner}{\code{IAI.OptSVMImputationLearner}}
#'
#' @usage opt_svm_imputation_learner(...)
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::opt_svm_imputation_learner()}
#'
#' @export
opt_svm_imputation_learner <- function(...) {
  jl_func("IAI.OptSVMImputationLearner", ...)
}


#' opt_tree_imputation_learner
#'
#' Learner for conducting optimal tree-based imputation
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/OptImpute/stable/reference/#IAI.OptTreeImputationLearner}{\code{IAI.OptTreeImputationLearner}}
#'
#' @usage opt_tree_imputation_learner(...)
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::opt_tree_imputation_learner()}
#'
#' @export
opt_tree_imputation_learner <- function(...) {
  jl_func("IAI.OptTreeImputationLearner", ...)
}


#' single_knn_imputation_learner
#'
#' Learner for conducting heuristic k-NN imputation
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/OptImpute/stable/reference/#IAI.SingleKNNImputationLearner}{\code{IAI.SingleKNNImputationLearner}}
#'
#' @usage single_knn_imputation_learner(...)
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::single_knn_imputation_learner()}
#'
#' @export
single_knn_imputation_learner <- function(...) {
  jl_func("IAI.SingleKNNImputationLearner", ...)
}


#' mean_imputation_learner
#'
#' Learner for conducting mean imputation
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/OptImpute/stable/reference/#IAI.MeanImputationLearner}{\code{IAI.MeanImputationLearner}}
#'
#' @usage mean_imputation_learner(...)
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::mean_imputation_learner()}
#'
#' @export
mean_imputation_learner <- function(...) {
  jl_func("IAI.MeanImputationLearner", ...)
}


#' rand_imputation_learner
#'
#' Learner for conducting random imputation
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/OptImpute/stable/reference/#IAI.RandImputationLearner}{\code{IAI.RandImputationLearner}}
#'
#' @usage rand_imputation_learner(...)
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::rand_imputation_learner()}
#'
#' @export
rand_imputation_learner <- function(...) {
  jl_func("IAI.RandImputationLearner", ...)
}