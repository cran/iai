#' Impute missing values using either a specified method or through validation
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/OptImpute/reference/#IAI.impute}{\code{IAI.impute}}
#'
#' This function was deprecated in iai 1.7.0. This is for consistency with the
#' IAI v3.0.0 Julia release.
#'
#' @param X The dataframe in which to impute missing values.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @example examples/impute.R
#'
#' @export
impute <- function(X, ...) {
  lifecycle::deprecate_warn("1.7.0", "iai::impute()")
  jl_func("IAI.impute_convert", X, ...)
}


#' Impute missing values using cross validation
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/OptImpute/reference/#IAI.impute_cv}{\code{IAI.impute_cv}}
#'
#' This function was deprecated in iai 1.7.0. This is for consistency with the
#' IAI v3.0.0 Julia release.
#'
#' @param X The dataframe in which to impute missing values.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @example examples/impute_cv.R
#'
#' @export
impute_cv <- function(X, ...) {
  lifecycle::deprecate_warn("1.7.0", "iai::impute()")
  jl_func("IAI.impute_cv_convert", X, ...)
}


#' Generic learner for imputing missing values
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/OptImpute/reference/#IAI.ImputationLearner}{\code{IAI.ImputationLearner}}
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
#' \href{https://docs.interpretable.ai/v3.1.0/OptImpute/reference/#IAI.OptKNNImputationLearner}{\code{IAI.OptKNNImputationLearner}}
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
#' \href{https://docs.interpretable.ai/v3.1.0/OptImpute/reference/#IAI.OptSVMImputationLearner}{\code{IAI.OptSVMImputationLearner}}
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
#' \href{https://docs.interpretable.ai/v3.1.0/OptImpute/reference/#IAI.OptTreeImputationLearner}{\code{IAI.OptTreeImputationLearner}}
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
#' \href{https://docs.interpretable.ai/v3.1.0/OptImpute/reference/#IAI.SingleKNNImputationLearner}{\code{IAI.SingleKNNImputationLearner}}
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
#' \href{https://docs.interpretable.ai/v3.1.0/OptImpute/reference/#IAI.MeanImputationLearner}{\code{IAI.MeanImputationLearner}}
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
#' \href{https://docs.interpretable.ai/v3.1.0/OptImpute/reference/#IAI.RandImputationLearner}{\code{IAI.RandImputationLearner}}
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


#' Learner for conducting zero-imputation
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/OptImpute/reference/#IAI.ZeroImputationLearner}{\code{IAI.ZeroImputationLearner}}
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::zero_imputation_learner()}
#'
#' @section IAI Compatibility:
#' Requires IAI version 3.0 or higher.
#'
#' @export
zero_imputation_learner <- function(...) {
  requires_iai_version("3.0.0", "zero_imputation_learner")
  set_obj_class(jl_func("IAI.ZeroImputationLearner_convert", ...))
}


#' Fit an imputation learner with training features and create adaptive
#' indicator features to encode the missing pattern
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/OptImpute/reference/#IAI.fit_and_expand!}{\code{IAI.fit_and_expand!}}
#'
#' @param lnr The learner to use for imputation.
#' @param X The dataframe in which to impute missing values.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::fit_and_expand(lnr, X, type = "finite")}
#'
#' @section IAI Compatibility:
#' Requires IAI version 3.0 or higher.
#'
#' @export
fit_and_expand <- function(lnr, X, ...) {
  requires_iai_version("3.0.0", "fit_and_expand")
  jl_func("IAI.fit_and_expand_convert", lnr, X, ...)
}


#' Transform features with a trained imputation learner and create adaptive
#' indicator features to encode the missing pattern
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.0/OptImpute/reference/#IAI.transform_and_expand}{\code{IAI.transform_and_expand}}
#'
#' @param lnr The learner to use for imputation.
#' @param X The dataframe in which to impute missing values.
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::transform_and_expand(lnr, X, type = "finite")}
#'
#' @section IAI Compatibility:
#' Requires IAI version 3.0 or higher.
#'
#' @export
transform_and_expand <- function(lnr, X, ...) {
  requires_iai_version("3.0.0", "transform_and_expand")
  jl_func("IAI.transform_and_expand_convert", lnr, X, ...)
}
