#' Learner for conducting Reward Estimation
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.0.0/RewardEstimation/reference/#IAI.RewardEstimator}{\code{IAI.RewardEstimator}}
#'
#' @usage reward_estimator(...)
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::reward_estimator()}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.0 or higher.
#'
#' @export
reward_estimator <- function(...) {
  requires_iai_version("2.0.0", "reward_estimator")
  set_obj_class(jl_func("IAI.RewardEstimator_convert", ...))
}
