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
  } else {
    lifecycle::expect_deprecated(reward_estimator())
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

    out <- iai::all_treatment_combinations(name1=c1)
    expect_true(is.data.frame(out))
    expect_equal(colnames(out), c("name1"))
    expect_equal(nrow(out), length(c1))
    expect_equal(ncol(out), 1)

    out <- iai::all_treatment_combinations(name1=c1, name2=c2)
    expect_true(is.data.frame(out))
    expect_equal(colnames(out), c("name1", "name2"))
    expect_equal(nrow(out), length(c1) * length(c2))
    expect_equal(ncol(out), 2)
  }
})
