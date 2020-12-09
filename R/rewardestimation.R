#' Learner for conducting reward estimation with categorical treatments
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/RewardEstimation/reference/#IAI.CategoricalRewardEstimator}{\code{IAI.CategoricalRewardEstimator}}
#'
#' @usage categorical_reward_estimator(...)
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::categorical_reward_estimator()}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.0 or higher.
#'
#' @export
categorical_reward_estimator <- function(...) {
  requires_iai_version("2.0.0", "categorical_reward_estimator")
  if (iai_version_less_than("2.1.0")) {
    set_obj_class(jl_func("IAI.RewardEstimator_convert", ...))
  } else {
    set_obj_class(jl_func("IAI.CategoricalRewardEstimator_convert", ...))
  }
}
#' Learner for conducting reward estimation with categorical treatments
#'
#' This function was deprecated and renamed to [categorical_reward_estimator()]
#' in iai 2.0.0. This is for consistency with the IAI v2.1.0 Julia release.
#'
#' @usage reward_estimator(...)
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::reward_estimator()}
#'
#' @export
#' @md
reward_estimator <- function(...) {
  lifecycle::deprecate_warn("2.0.0", "iai::reward_estimator()",
                            "categorical_reward_estimator()")
  categorical_reward_estimator(...)
}


#' Learner for conducting reward estimation with numeric treatments
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/RewardEstimation/reference/#IAI.NumericRewardEstimator}{\code{IAI.NumericRewardEstimator}}
#'
#' @usage numeric_reward_estimator(...)
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::numeric_reward_estimator()}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @export
numeric_reward_estimator <- function(...) {
  requires_iai_version("2.1.0", "numeric_reward_estimator")
  set_obj_class(jl_func("IAI.NumericRewardEstimator_convert", ...))
}


#' Learner that estimates equal propensity for all treatments.
#'
#' For use with data from randomized experiments where treatments are known to
#' be randomly assigned.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/RewardEstimation/reference/#IAI.EqualPropensityEstimator}{\code{IAI.EqualPropensityEstimator}}
#'
#' @usage equal_propensity_estimator(...)
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::equal_propensity_estimator()}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @export
equal_propensity_estimator <- function(...) {
  requires_iai_version("2.1.0", "equal_propensity_estimator")
  set_obj_class(jl_func("IAI.EqualPropensityEstimator_convert", ...))
}


#' Fit a reward estimation model on features, treatments and outcomes and
#' return predicted counterfactual rewards for each observation, as well as the
#' score of the internal outcome estimator.
#'
#' For categorical treatments, returns the estimated reward under each
#' treatment observed in the data. For numeric treatments, returns the
#' estimated reward under each treatment candidate.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/RewardEstimation/reference/#IAI.fit_predict!}{\code{IAI.fit_predict!}}
#'
#' @usage fit_predict(lnr, X, treatments, outcomes, ...)
#'
#' @param lnr The learner or grid to use for estimation
#' @param X The features of the data.
#' @param treatments The treatment applied to each point in the data.
#' @param outcomes The outcome observed for each point in the data.
#' @param ... For numeric treatments, the treatment candidates to consider
#'
#' @examples \dontrun{iai::fit_predict(lnr, X, treatments, outcomes)}
#'
#' @export
fit_predict <- function(lnr, X, treatments, outcomes, ...) {
  out <- jl_func("IAI.fit_predict_convert", lnr, X, treatments, outcomes, ...)
  if (!iai_version_less_than("2.1.0")) {
    names(out) <- c("rewards", "score")
  }
  out
}


#' Return a dataframe containing all treatment combinations of one or more
#' treatment vectors, ready for use as treatment candidates in `fit_predict!`
#' or `predict`
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/RewardEstimation/reference/#IAI.all_treatment_combinations}{\code{IAI.all_treatment_combinations}}
#'
#' @usage all_treatment_combinations(...)
#'
#' @param ... A vector of possible options for each treatment
#'
#' @examples \dontrun{iai::all_treatment_combinations(c(1, 2, 3))}
#'
#' @export
all_treatment_combinations <- function(...) {
  requires_iai_version("2.1.0", "all_treatment_combinations")
  jl_func("IAI.all_treatment_combinations_convert", ...)
}


#' Convert `treatments` from symbol/string format into numeric values.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v2.1.0/RewardEstimation/reference/#IAI.convert_treatments_to_numeric}{\code{IAI.convert_treatments_to_numeric}}
#'
#' @usage convert_treatments_to_numeric(treatments)
#'
#' @param treatments The treatments to convert
#'
#' @examples \dontrun{iai::convert_treatments_to_numeric(c("1", "2", "3"))}
#'
#' @export
convert_treatments_to_numeric <- function(treatments) {
  requires_iai_version("2.1.0", "convert_treatments_to_numeric")
  jl_func("IAI.convert_treatments_to_numeric_convert", treatments)
}
