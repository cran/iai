#' Learner for conducting reward estimation with categorical treatments
#'
#' This function was deprecated in iai 1.6.0, and
#' [categorical_classification_reward_estimator()] or
#' [categorical_classification_reward_estimator()] should be used instead.
#'
#' This deprecation is no longer supported as of the IAI v3 release.
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::categorical_reward_estimator()}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.0, 2.1 or 2.2.
#'
#' @export
categorical_reward_estimator <- function(...) {
  if (!iai_version_less_than("2.3.0")) {
    stop("`categorical_reward_estimator` was removed in IAI v3")
  }

  requires_iai_version("2.0.0", "categorical_reward_estimator")
  lifecycle::deprecate_warn(
      "1.6.0",
      "iai::categorical_reward_estimator()",
      details = paste("Please use",
                      "`categorical_classification_reward_estimator()` or",
                      "`categorical_regression_reward_estimator()` instead")
  )
  if (iai_version_less_than("2.1.0")) {
    set_obj_class(jl_func("IAI.RewardEstimator_convert", ...))
  } else {
    set_obj_class(jl_func("IAI.CategoricalRewardEstimator_convert", ...))
  }
}
#' Learner for conducting reward estimation with categorical treatments
#'
#' This function was deprecated and renamed to [categorical_reward_estimator()]
#' in iai 1.4.0. This is for consistency with the IAI v2.1.0 Julia release.
#'
#' This deprecation is no longer supported as of the IAI v3 release.
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::reward_estimator()}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.2 or lower.
#'
#' @export
#' @md
reward_estimator <- function(...) {
  if (!iai_version_less_than("2.3.0")) {
    stop("`reward_estimator` was removed in IAI v3")
  }
  requires_iai_version("2.0.0", "reward_estimator")
  lifecycle::deprecate_warn("1.4.0", "iai::reward_estimator()",
                            "categorical_reward_estimator()")
  categorical_reward_estimator(...)
}


#' Calculate the scores for a categorical reward estimator on the given data
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/RewardEstimation/reference/#IAI.score-Tuple\%7BCategoricalRewardEstimator\%7D}{\code{IAI.score}}
#'
#' @param obj The learner or grid to evaluate.
#' @param X The features of the data.
#' @param ... Other parameters, including zero or more target vectors as
#'            required by the problem type. Refer to the Julia
#'            documentation for other available parameters.
#'
#' @examples \dontrun{iai::score(lnr, X, treatments, outcomes)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @export
score.categorical_reward_estimator <- function(obj, X, ...) {
  requires_iai_version("2.1.0", "score", "with `categorical_reward_estimator`")
  score_common(obj, X, ...)
}


#' Return counterfactual rewards estimated by a categorical reward estimator for
#' each observation in the supplied data
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/RewardEstimation/reference/#IAI.predict-Tuple\%7BCategoricalRewardEstimator\%7D}{\code{IAI.predict}}
#'
#' @param obj The learner or grid to use for estimation
#' @param X The features of the data.
#' @param ... Additional arguments depending on the treatment and outcome types.
#'            Refer to the Julia documentation for more information.
#'
#' @examples \dontrun{iai::predict(lnr, X, treatments, outcomes)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.0 or higher.
#'
#' @export
predict.categorical_reward_estimator <- function(obj, X, ...) {
  requires_iai_version("2.0.0", "predict",
                       "with `categorical_reward_estimator`")
  predict_common(obj, X, ...)
}


#' Return counterfactual rewards estimated by a categorical reward estimator for
#' each observation in the supplied data and predictions
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/RewardEstimation/reference/#IAI.predict_reward-Tuple\%7BCategoricalRewardEstimator\%7D}{\code{IAI.predict_reward}}
#'
#' @param obj The learner or grid to use for estimation
#' @param X The features of the data.
#' @param ... Additional arguments depending on the treatment and outcome types.
#'            Refer to the Julia documentation for more information.
#'
#' @examples \dontrun{iai::predict_reward(lnr, X, treatments, outcomes, predictions)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 3.0 or higher.
#'
#' @export
predict_reward.categorical_reward_estimator <- function(obj, X, ...) {
  requires_iai_version("3.0.0", "predict_reward",
                       "with `categorical_reward_estimator`")
  predict_reward_common(obj, X, ...)
}


#' Learner for conducting reward estimation with categorical treatments and
#' classification outcomes
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/RewardEstimation/reference/#IAI.CategoricalClassificationRewardEstimator}{\code{IAI.CategoricalClassificationRewardEstimator}}
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::categorical_classification_reward_estimator()}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.2 or higher.
#'
#' @export
categorical_classification_reward_estimator <- function(...) {
  requires_iai_version("2.2.0", "categorical_classification_reward_estimator")
  set_obj_class(jl_func("IAI.CategoricalClassificationRewardEstimator_convert",
                        ...))
}


#' Learner for conducting reward estimation with categorical treatments and
#' regression outcomes
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/RewardEstimation/reference/#IAI.CategoricalRegressionRewardEstimator}{\code{IAI.CategoricalRegressionRewardEstimator}}
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::categorical_regression_reward_estimator()}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.2 or higher.
#'
#' @export
categorical_regression_reward_estimator <- function(...) {
  requires_iai_version("2.2.0", "categorical_regression_reward_estimator")
  set_obj_class(jl_func("IAI.CategoricalRegressionRewardEstimator_convert",
                        ...))
}


#' Learner for conducting reward estimation with categorical treatments and
#' survival outcomes
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/RewardEstimation/reference/#IAI.CategoricalSurvivalRewardEstimator}{\code{IAI.CategoricalSurvivalRewardEstimator}}
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::categorical_survival_reward_estimator()}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.2 or higher.
#'
#' @export
categorical_survival_reward_estimator <- function(...) {
  requires_iai_version("2.2.0", "categorical_survival_reward_estimator")
  set_obj_class(jl_func("IAI.CategoricalSurvivalRewardEstimator_convert", ...))
}


#' Learner for conducting reward estimation with numeric treatments
#'
#' This function was deprecated in iai 1.6.0, and
#' [numeric_classification_reward_estimator()] or
#' [numeric_classification_reward_estimator()] should be used instead.
#'
#' This deprecation is no longer supported as of the IAI v3 release.
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::numeric_reward_estimator()}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or 2.2.
#'
#' @export
numeric_reward_estimator <- function(...) {
  if (!iai_version_less_than("2.3.0")) {
    stop("`numeric_reward_estimator` was removed in IAI v3")
  }
  requires_iai_version("2.1.0", "numeric_reward_estimator")
  lifecycle::deprecate_warn(
      "1.6.0",
      "iai::numeric_reward_estimator()",
      details = paste("Please use `numeric_classification_reward_estimator()`",
                      "or `numeric_regression_reward_estimator()` instead")
  )
  set_obj_class(jl_func("IAI.NumericRewardEstimator_convert", ...))
}


#' Calculate the scores for a numeric reward estimator on the given data
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/RewardEstimation/reference/#IAI.score-Tuple\%7BNumericRewardEstimator\%7D}{\code{IAI.score}}
#'
#' @param obj The learner or grid to evaluate.
#' @param X The features of the data.
#' @param ... Other parameters, including zero or more target vectors as
#'            required by the problem type. Refer to the Julia
#'            documentation for other available parameters.
#'
#' @examples \dontrun{iai::score(lnr, X, treatments, outcomes)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @export
score.numeric_reward_estimator <- function(obj, X, ...) {
  requires_iai_version("2.1.0", "score", "with `numeric_reward_estimator`")
  score_common(obj, X, ...)
}


#' Return counterfactual rewards estimated by a numeric reward estimator for
#' each observation in the supplied data
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/RewardEstimation/reference/#IAI.predict-Tuple\%7BNumericRewardEstimator\%7D}{\code{IAI.predict}}
#'
#' @param obj The learner or grid to use for estimation
#' @param X The features of the data.
#' @param ... Additional arguments depending on the treatment and outcome types.
#'            Refer to the Julia documentation for more information.
#'
#' @examples \dontrun{iai::predict(lnr, X, treatments, outcomes)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @export
predict.numeric_reward_estimator <- function(obj, X, ...) {
  requires_iai_version("2.1.0", "predict", "with `numeric_reward_estimator`")
  predict_common(obj, X, ...)
}


#' Return counterfactual rewards estimated by a numeric reward estimator for
#' each observation in the supplied data and predictions
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/RewardEstimation/reference/#IAI.predict_reward-Tuple\%7BNumericRewardEstimator\%7D}{\code{IAI.predict_reward}}
#'
#' @param obj The learner or grid to use for estimation
#' @param X The features of the data.
#' @param ... Additional arguments depending on the treatment and outcome types.
#'            Refer to the Julia documentation for more information.
#'
#' @examples \dontrun{iai::predict_reward(lnr, X, treatments, outcomes, predictions)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 3.0 or higher.
#'
#' @export
predict_reward.numeric_reward_estimator <- function(obj, X, ...) {
  requires_iai_version("3.0.0", "predict_reward",
                       "with `numeric_reward_estimator`")
  predict_reward_common(obj, X, ...)
}


#' Learner for conducting reward estimation with numeric treatments and
#' classification outcomes
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/RewardEstimation/reference/#IAI.NumericClassificationRewardEstimator}{\code{IAI.NumericClassificationRewardEstimator}}
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::numeric_classification_reward_estimator()}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.2 or higher.
#'
#' @export
numeric_classification_reward_estimator <- function(...) {
  requires_iai_version("2.2.0", "numeric_classification_reward_estimator")
  set_obj_class(jl_func("IAI.NumericClassificationRewardEstimator_convert",
                        ...))
}


#' Learner for conducting reward estimation with numeric treatments and
#' regression outcomes
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/RewardEstimation/reference/#IAI.NumericRegressionRewardEstimator}{\code{IAI.NumericRegressionRewardEstimator}}
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::numeric_regression_reward_estimator()}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.2 or higher.
#'
#' @export
numeric_regression_reward_estimator <- function(...) {
  requires_iai_version("2.2.0", "numeric_regression_reward_estimator")
  set_obj_class(jl_func("IAI.NumericRegressionRewardEstimator_convert", ...))
}


#' Learner for conducting reward estimation with numeric treatments and survival
#' outcomes
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/RewardEstimation/reference/#IAI.NumericSurvivalRewardEstimator}{\code{IAI.NumericSurvivalRewardEstimator}}
#'
#' @param ... Use keyword arguments to set parameters on the resulting learner.
#'            Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{lnr <- iai::numeric_survival_reward_estimator()}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.2 or higher.
#'
#' @export
numeric_survival_reward_estimator <- function(...) {
  requires_iai_version("2.2.0", "numeric_survival_reward_estimator")
  set_obj_class(jl_func("IAI.NumericSurvivalRewardEstimator_convert", ...))
}


#' Learner that estimates equal propensity for all treatments.
#'
#' For use with data from randomized experiments where treatments are known to
#' be randomly assigned.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/RewardEstimation/reference/#IAI.EqualPropensityEstimator}{\code{IAI.EqualPropensityEstimator}}
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


#' Fit a categorical reward estimator on features, treatments and
#' outcomes and return predicted counterfactual rewards for each observation,
#' under each treatment observed in the data, as well as the scores of the
#' internal estimators.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/RewardEstimation/reference/#IAI.fit_predict\%21-Tuple\%7BCategoricalRewardEstimator\%7D}{\code{IAI.fit_predict!}}
#'
#' @param obj The learner or grid to use for estimation
#' @param X The features of the data.
#' @param treatments The treatment applied to each point in the data.
#' @param ... Additional arguments depending on the treatment and outcome types.
#'            Refer to the Julia documentation for more information.
#'
#' @examples \dontrun{iai::fit_predict(obj, X, treatments, outcomes)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.0 or higher.
#'
#' @export
fit_predict.categorical_reward_estimator <- function(obj, X, treatments, ...) {
  requires_iai_version("2.0.0", "fit_predict",
                       "with `categorical_reward_estimator`")
  fit_predict_common(obj, X, treatments, ...)
}
#' Fit a numeric reward estimator on features, treatments and
#' outcomes and return predicted counterfactual rewards for each observation,
#' under each treatment candidate, as well as the scores of the internal
#' estimators.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/RewardEstimation/reference/#IAI.fit_predict\%21-Tuple\%7BNumericRewardEstimator\%7D}{\code{IAI.fit_predict!}}
#'
#' @param obj The learner or grid to use for estimation
#' @param X The features of the data.
#' @param treatments The treatment applied to each point in the data.
#' @param ... Additional arguments depending on the treatment and outcome types.
#'            Refer to the Julia documentation for more information.
#'
#' @examples \dontrun{iai::fit_predict(obj, X, treatments, outcomes)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.1 or higher.
#'
#' @export
fit_predict.numeric_reward_estimator <- function(obj, X, treatments, ...) {
  requires_iai_version("2.1.0", "fit_predict",
                       "with `numeric_reward_estimator`")
  fit_predict_common(obj, X, treatments, ...)
}


#' Return the total kernel density surrounding each treatment candidate for the
#' propensity/outcome estimation problems in a fitted learner.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/RewardEstimation/reference/#IAI.get_estimation_densities}{\code{IAI.get_estimation_densities}}
#'
#' @param lnr The learner from which to extract densities
#' @param ... Refer to the Julia documentation for other parameters
#'
#' @examples \dontrun{iai::get_estimation_densities(lnr, ...)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.2 or higher.
#'
#' @export
get_estimation_densities <- function(lnr, ...) {
  requires_iai_version("2.2.0", "get_estimation_densities")
  jl_func("IAI.get_estimation_densities_convert", lnr, ...)
}


#' Conduct the reward kernel bandwidth tuning procedure for a range of starting
#' bandwidths and return the final tuned values.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/RewardEstimation/reference/#IAI.tune_reward_kernel_bandwidth}{\code{IAI.tune_reward_kernel_bandwidth}}
#'
#' @param lnr The learner to use for tuning the bandwidth
#' @param ... Refer to the Julia documentation for other parameters
#'
#' @examples \dontrun{iai::tune_reward_kernel_bandwidth(lnr, ...)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.2 or higher.
#'
#' @export
tune_reward_kernel_bandwidth <- function(lnr, ...) {
  requires_iai_version("2.2.0", "tune_reward_kernel_bandwidth")
  jl_func("IAI.tune_reward_kernel_bandwidth_convert", lnr, ...)
}


#' Save a new reward kernel bandwidth inside a learner, and return new reward
#' predictions generated using this bandwidth for the original data used to
#' train the learner.
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/RewardEstimation/reference/#IAI.set_reward_kernel_bandwidth!}{\code{IAI.set_reward_kernel_bandwidth!}}
#'
#' @param lnr The learner to modify
#' @param ... Refer to the Julia documentation for available parameters.
#'
#' @examples \dontrun{iai::set_reward_kernel_bandwidth(lnr, ...)}
#'
#' @section IAI Compatibility:
#' Requires IAI version 2.2 or higher.
#'
#' @export
set_reward_kernel_bandwidth <- function(lnr, ...) {
  requires_iai_version("2.2.0", "set_reward_kernel_bandwidth")
  jl_func("IAI.set_reward_kernel_bandwidth_convert", lnr, ...)
}


#' Return a dataframe containing all treatment combinations of one or more
#' treatment vectors, ready for use as treatment candidates in `fit_predict!`
#' or `predict`
#'
#' Julia Equivalent:
#' \href{https://docs.interpretable.ai/v3.1.1/RewardEstimation/reference/#IAI.all_treatment_combinations}{\code{IAI.all_treatment_combinations}}
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
#' \href{https://docs.interpretable.ai/v3.1.1/RewardEstimation/reference/#IAI.convert_treatments_to_numeric}{\code{IAI.convert_treatments_to_numeric}}
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
