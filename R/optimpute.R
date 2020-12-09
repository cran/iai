#' Impute missing values using either a specified method or through validation
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/OptImpute/reference/#IAI.impute}{\code{IAI.impute}}
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
  jl_func("IAI.impute_convert", X, ...)
}


#' Impute missing values using cross validation
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/OptImpute/reference/#IAI.impute_cv}{\code{IAI.impute_cv}}
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
  jl_func("IAI.impute_cv_convert", X, ...)
}


#' Generic learner for imputing missing values
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/OptImpute/reference/#IAI.ImputationLearner}{\code{IAI.ImputationLearner}}
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
  set_obj_class(jl_func("IAI.ImputationLearner_convert", method, ...))
}


#' Learner for conducting optimal k-NN imputation
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/OptImpute/reference/#IAI.OptKNNImputationLearner}{\code{IAI.OptKNNImputationLearner}}
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
  set_obj_class(jl_func("IAI.OptKNNImputationLearner_convert", ...))
}


#' Learner for conducting optimal SVM imputation
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/OptImpute/reference/#IAI.OptSVMImputationLearner}{\code{IAI.OptSVMImputationLearner}}
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
  set_obj_class(jl_func("IAI.OptSVMImputationLearner_convert", ...))
}


#' Learner for conducting optimal tree-based imputation
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/OptImpute/reference/#IAI.OptTreeImputationLearner}{\code{IAI.OptTreeImputationLearner}}
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
  set_obj_class(jl_func("IAI.OptTreeImputationLearner_convert", ...))
}


#' Learner for conducting heuristic k-NN imputation
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/OptImpute/reference/#IAI.SingleKNNImputationLearner}{\code{IAI.SingleKNNImputationLearner}}
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
  set_obj_class(jl_func("IAI.SingleKNNImputationLearner_convert", ...))
}


#' Learner for conducting mean imputation
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/OptImpute/reference/#IAI.MeanImputationLearner}{\code{IAI.MeanImputationLearner}}
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
  set_obj_class(jl_func("IAI.MeanImputationLearner_convert", ...))
}


#' Learner for conducting random imputation
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/OptImpute/reference/#IAI.RandImputationLearner}{\code{IAI.RandImputationLearner}}
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
  set_obj_class(jl_func("IAI.RandImputationLearner_convert", ...))
}
