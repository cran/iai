context("RewardEstimation")


test_that("equal_propensity_estimator", {
  skip_on_cran()

  if (iai:::iai_version_less_than("2.1.0")) {
    expect_error(iai::equal_propensity_estimator(),
                 "requires IAI version 2.1.0")
  } else {
    lnr <- iai::equal_propensity_estimator()
    expect_true(iai:::jl_isa(lnr, "IAI.EqualPropensityEstimator"))
  }
})

test_that("`reward_estimator` is deprecated", {
  skip_on_cran()
  if (iai:::iai_version_less_than("2.0.0")) {
    expect_error(iai::reward_estimator(), "requires IAI version 2.0.0")
  } else if (iai:::iai_version_less_than("2.3.0")) {
    lifecycle::expect_deprecated(iai::reward_estimator())
  } else {
    expect_error(iai::reward_estimator(), "removed in IAI v3")
  }
})

test_that("`categorical_reward_estimator` is deprecated", {
  skip_on_cran()
  if (iai:::iai_version_less_than("2.0.0")) {
    expect_error(iai::categorical_reward_estimator(),
                 "requires IAI version 2.0.0")
  } else if (iai:::iai_version_less_than("2.3.0")) {
    lifecycle::expect_deprecated(iai::categorical_reward_estimator())
  } else {
    expect_error(iai::categorical_reward_estimator(), "removed in IAI v3")
  }
})

test_that("`numeric_reward_estimator` is deprecated", {
  skip_on_cran()
  if (iai:::iai_version_less_than("2.1.0")) {
    expect_error(iai::numeric_reward_estimator(), "requires IAI version 2.1.0")
  } else if (iai:::iai_version_less_than("2.3.0")) {
    lifecycle::expect_deprecated(iai::numeric_reward_estimator())
  } else {
    expect_error(iai::numeric_reward_estimator(), "removed in IAI v3")
  }
})


test_that("internal estimator consistency", {
  skip_on_cran()
  if (iai:::iai_version_less_than("2.2.0")) {
    expect_error(iai::categorical_classification_reward_estimator(),
                 "requires IAI version 2.2.0")
    expect_error(iai::categorical_regression_reward_estimator(),
                 "requires IAI version 2.2.0")
    expect_error(iai::numeric_classification_reward_estimator(),
                 "requires IAI version 2.2.0")
    expect_error(iai::numeric_regression_reward_estimator(),
                 "requires IAI version 2.2.0")
  } else {
    class_lnr <- iai::optimal_tree_classifier()
    reg_lnr <- iai::optimal_tree_regressor()
    surv_lnr <- iai::optimal_tree_survival_learner()

    L <- iai::categorical_classification_reward_estimator
    # propensity
    L(propensity_estimator = class_lnr)
    expect_error(L(propensity_estimator = reg_lnr))
    # outcome
    L(outcome_estimator = class_lnr)
    expect_error(L(outcome_estimator = reg_lnr))

    L <- iai::categorical_regression_reward_estimator
    # propensity
    L(propensity_estimator = class_lnr)
    expect_error(L(propensity_estimator = reg_lnr))
    # outcome
    expect_error(L(outcome_estimator = class_lnr))
    L(outcome_estimator = reg_lnr)

    L <- iai::categorical_survival_reward_estimator
    # propensity
    L(propensity_estimator = class_lnr)
    expect_error(L(propensity_estimator = reg_lnr))
    # outcome
    expect_error(L(outcome_estimator = class_lnr))
    L(outcome_estimator = surv_lnr)

    L <- iai::numeric_classification_reward_estimator
    # propensity
    expect_error(L(propensity_estimator = class_lnr))
    L(propensity_estimator = reg_lnr)
    # outcome
    L(outcome_estimator = class_lnr)
    expect_error(L(outcome_estimator = reg_lnr))

    L <- iai::numeric_regression_reward_estimator
    # propensity
    expect_error(L(propensity_estimator = class_lnr))
    L(propensity_estimator = reg_lnr)
    # outcome
    expect_error(L(outcome_estimator = class_lnr))
    L(outcome_estimator = reg_lnr)

    L <- iai::numeric_survival_reward_estimator
    # propensity
    expect_error(L(propensity_estimator = class_lnr))
    L(propensity_estimator = reg_lnr)
    # outcome
    expect_error(L(outcome_estimator = class_lnr))
    L(outcome_estimator = surv_lnr)
  }
})


test_that("all_treatment_combinations", {
  skip_on_cran()

  if (iai:::iai_version_less_than("2.1.0")) {
    expect_error(iai::all_treatment_combinations(),
                 "requires IAI version 2.1.0")
  } else {
    c1 <- seq(1, 5)
    c2 <- c(1, 2)

    out <- iai::all_treatment_combinations(c1)
    expect_true(is.data.frame(out))
    expect_equal(colnames(out), c("treatment1"))
    expect_equal(nrow(out), length(c1))
    expect_equal(ncol(out), 1)

    out <- iai::all_treatment_combinations(c1, c2)
    expect_true(is.data.frame(out))
    expect_equal(colnames(out), c("treatment1", "treatment2"))
    expect_equal(nrow(out), length(c1) * length(c2))
    expect_equal(ncol(out), 2)

    out <- iai::all_treatment_combinations(name1 = c1)
    expect_true(is.data.frame(out))
    expect_equal(colnames(out), c("name1"))
    expect_equal(nrow(out), length(c1))
    expect_equal(ncol(out), 1)

    out <- iai::all_treatment_combinations(name1 = c1, name2 = c2)
    expect_true(is.data.frame(out))
    expect_equal(colnames(out), c("name1", "name2"))
    expect_equal(nrow(out), length(c1) * length(c2))
    expect_equal(ncol(out), 2)
  }
})


test_that("numeric API", {
  skip_on_cran()

  if (iai:::iai_version_less_than("2.2.0")) {
    expect_error(iai::get_estimation_densities(), "requires IAI version 2.2.0")
    expect_error(iai::tune_reward_kernel_bandwidth(),
                 "requires IAI version 2.2.0")
    expect_error(iai::set_reward_kernel_bandwidth(),
                 "requires IAI version 2.2.0")
  } else {
    X <- iris[, 1:2]
    t <- iris[, 3]
    y <- iris[, 4]
    c <- unique(y)

    lnr <- iai::numeric_regression_reward_estimator(
        propensity_estimator = iai::random_forest_regressor(),
        outcome_estimator = iai::random_forest_regressor(),
        reward_estimator = "doubly_robust",
    )
    rewards <- iai::fit_predict(lnr, X, t, y, c)

    if (iai:::iai_version_less_than("3.0.0")) {
      densities <- iai::get_estimation_densities(lnr)
    } else {
      densities <- iai::get_estimation_densities(lnr, t, c)
    }
    expect_equal(length(densities), length(c))

    input <- c(1, 2, 3)
    if (iai:::iai_version_less_than("3.0.0")) {
      tuned <- iai::tune_reward_kernel_bandwidth(lnr, input)
    } else {
      tuned <- iai::tune_reward_kernel_bandwidth(lnr, t, y, rewards$predictions,
                                                 input)
    }
    expect_equal(length(tuned), length(input))

    if (iai:::iai_version_less_than("3.0.0")) {
      rewards2 <- iai::set_reward_kernel_bandwidth(lnr, 2)
      expect_equal(nrow(rewards2), nrow(X))
      expect_equal(ncol(rewards2), length(c))
    } else {
      rewards2 <- iai::set_reward_kernel_bandwidth(lnr, t, y,
                                                   rewards$predictions, 2)
      expect_equal(nrow(rewards2$reward), nrow(X))
      expect_equal(ncol(rewards2$reward), length(c))
      expect_equal(nrow(rewards2$propensity), nrow(X))
      expect_equal(ncol(rewards2$propensity), length(c))
      expect_equal(nrow(rewards2$outcome), nrow(X))
      expect_equal(ncol(rewards2$outcome), length(c))
    }

    if (iai:::iai_version_less_than("3.0.0")) {
      expect_error(iai::predict_reward(lnr, t, y, rewards$predictions))
    } else {
      rewards2 <- iai::predict_reward(lnr, t, y, rewards$predictions)
      expect_equal(nrow(rewards2$reward), nrow(X))
      expect_equal(ncol(rewards2$reward), length(c))
      expect_equal(nrow(rewards2$propensity), nrow(X))
      expect_equal(ncol(rewards2$propensity), length(c))
      expect_equal(nrow(rewards2$outcome), nrow(X))
      expect_equal(ncol(rewards2$outcome), length(c))
    }
  }
})


test_that("JSON", {
  skip_on_cran()

  if (!iai:::iai_version_less_than("3.0.0")) {
    lnrs <- c(iai::categorical_classification_reward_estimator(),
              iai::categorical_regression_reward_estimator(),
              iai::categorical_survival_reward_estimator(),
              iai::numeric_classification_reward_estimator(),
              iai::numeric_regression_reward_estimator(),
              iai::numeric_survival_reward_estimator(),
              iai::equal_propensity_estimator())
    for (i in seq(lnrs)) {
      # Putting learners in a vector removes their class info so add it back
      lnr <- iai:::set_obj_class(lnrs[i])

      iai::write_json("re.json", lnr)
      new_lnr <- iai::read_json("re.json")
      file.remove("re.json")
      expect_true(lnr == new_lnr)
    }
  }
})
