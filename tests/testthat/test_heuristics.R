context("Heuristics")


test_that("random forest classification", {
  skip_on_cran()

  if (iai:::iai_version_less_than("2.1.0")) {
    expect_error(iai::random_forest_classifier(), "requires IAI version 2.1.0")
  } else {
    X <- iris[, 1:4]
    y <- iris[, 5] == "setosa"
    lnr <- iai::random_forest_classifier()
    iai::fit(lnr, X, y)

    expect_true(is.numeric(iai::score(lnr, X, y)))
    expect_true(is.vector(iai::predict(lnr, X)))
    expect_true(is.data.frame(iai::predict_proba(lnr, X)))
  }
})


test_that("random forest regression", {
  skip_on_cran()

  if (iai:::iai_version_less_than("2.1.0")) {
    expect_error(iai::random_forest_regressor(), "requires IAI version 2.1.0")
  } else {
    X <- mtcars[, 2:11]
    y <- mtcars[, 1]
    lnr <- iai::random_forest_regressor()
    iai::fit(lnr, X, y)

    expect_true(is.numeric(iai::score(lnr, X, y)))
    expect_true(is.vector(iai::predict(lnr, X)))
  }
})


test_that("xgboost classification", {
  skip_on_cran()

  if (iai:::iai_version_less_than("2.1.0")) {
    expect_error(iai::xgboost_classifier(), "requires IAI version 2.1.0")
  } else {
    X <- iris[, 1:4]
    y <- iris[, 5] == "setosa"
    lnr <- iai::xgboost_classifier()
    iai::fit(lnr, X, y)

    expect_true(is.numeric(iai::score(lnr, X, y)))
    expect_true(is.vector(iai::predict(lnr, X)))
    expect_true(is.data.frame(iai::predict_proba(lnr, X)))

    iai::write_booster("xgb_classifier.json", lnr)
    file.remove("xgb_classifier.json")
  }
})


test_that("xgboost regression", {
  skip_on_cran()

  if (iai:::iai_version_less_than("2.1.0")) {
    expect_error(iai::xgboost_regressor(), "requires IAI version 2.1.0")
  } else {
    X <- mtcars[, 2:11]
    y <- mtcars[, 1]
    lnr <- iai::xgboost_regressor()
    iai::fit(lnr, X, y)

    expect_true(is.numeric(iai::score(lnr, X, y)))
    expect_true(is.vector(iai::predict(lnr, X)))

    iai::write_booster("xgb_regressor.json", lnr)
    file.remove("xgb_regressor.json")
  }
})


test_that("glmnetcv regression", {
  skip_on_cran()

  if (iai:::iai_version_less_than("2.1.0")) {
    expect_error(iai::glmnetcv_regressor(), "requires IAI version 2.1.0")
  } else {
    X <- mtcars[, 2:11]
    y <- mtcars[, 1]
    lnr <- iai::glmnetcv_regressor()
    iai::fit(lnr, X, y)

    expect_true(is.numeric(iai::score(lnr, X, y)))
    expect_true(is.vector(iai::predict(lnr, X)))

    s <- iai::score(lnr, X, y)
    scores <- sapply(1:iai::get_num_fits(lnr), function (i) {
      iai::score(lnr, X, y, fit_index = i)
    })
    best_index <- which(scores == s)

    expect_equal(iai::predict(lnr, X), iai::predict(lnr, X, best_index))
    expect_equal(iai::get_prediction_constant(lnr),
                 iai::get_prediction_constant(lnr, best_index))
    expect_equal(iai::get_prediction_weights(lnr),
                 iai::get_prediction_weights(lnr, best_index))
  }
})
