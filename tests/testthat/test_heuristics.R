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

    expect_equal(class(lnr), c(
        "random_forest_classifier",
        "random_forest_learner",
        "classification_learner",
        "supervised_learner",
        "learner",
        "IAIObject",
        "JuliaObject"
    ))
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

    expect_equal(class(lnr), c(
        "random_forest_regressor",
        "random_forest_learner",
        "regression_learner",
        "supervised_learner",
        "learner",
        "IAIObject",
        "JuliaObject"
    ))
  }
})


test_that("random forest survival", {
  skip_on_cran()

  if (iai:::iai_version_less_than("2.2.0")) {
    expect_error(iai::random_forest_survival_learner(),
                 "requires IAI version 2.2.0")
  } else {
    X <- mtcars[, 2:11]
    deaths <- sample(c(TRUE, FALSE), nrow(mtcars), TRUE)
    times <- mtcars[, 1]
    lnr <- iai::random_forest_survival_learner()
    iai::fit(lnr, X, deaths, times)

    expect_true(is.numeric(iai::score(lnr, X, deaths, times)))
    expect_true(is.vector(iai::predict(lnr, X, t = 1)))

    curves <- iai::predict(lnr, X)
    expect_equal(class(curves[[1]]), c(
        "survival_curve",
        "IAIObject",
        "JuliaObject"
    ))
    probs <- iai::predict(lnr, X, t = 1)
    expect_equal(class(probs[1]), c("numeric"))

    expect_equal(class(lnr), c(
        "random_forest_survival_learner",
        "random_forest_learner",
        "survival_learner",
        "supervised_learner",
        "learner",
        "IAIObject",
        "JuliaObject"
    ))
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

    if (iai:::iai_version_less_than("2.2.0")) {
      expect_error(iai::predict_shap(lnr, X), "requires IAI version 2.2.0")
    } else {
      s <- iai::predict_shap(lnr, X)
      expect_true(is.data.frame(s$features))
      expect_true(is.list(s$shap_values))
      expect_length(s$shap_values, length(unique(y)))
      expect_true(all(sapply(s$shap_values, is.matrix)))
      expect_true(all(sapply(s$shap_values, nrow) == nrow(X)))
      expect_true(all(sapply(s$shap_values, ncol) == ncol(X)))
      expect_length(s$expected_value, length(unique(y)))
      expect_setequal(s$labels, unique(y))
    }

    expect_equal(class(lnr), c(
        "xgboost_classifier",
        "xgboost_learner",
        "classification_learner",
        "supervised_learner",
        "learner",
        "IAIObject",
        "JuliaObject"
    ))
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

    if (iai:::iai_version_less_than("2.2.0")) {
      expect_error(iai::predict_shap(lnr, X), "requires IAI version 2.2.0")
    } else {
      s <- iai::predict_shap(lnr, X)
      expect_true(is.data.frame(s$features))
      expect_true(is.matrix(s$shap_values))
      expect_equal(nrow(s$shap_values), nrow(X))
      expect_equal(ncol(s$shap_values), ncol(X))
      expect_length(s$expected_value, 1)
      expect_false("labels" %in% s)
    }

    expect_equal(class(lnr), c(
        "xgboost_regressor",
        "xgboost_learner",
        "regression_learner",
        "supervised_learner",
        "learner",
        "IAIObject",
        "JuliaObject"
    ))
  }
})


test_that("xgboost survival", {
  skip_on_cran()

  if (iai:::iai_version_less_than("2.2.0")) {
    expect_error(iai::xgboost_survival_learner(),
                 "requires IAI version 2.2.0")
  } else {
    X <- mtcars[, 2:11]
    deaths <- sample(c(TRUE, FALSE), nrow(mtcars), TRUE)
    times <- mtcars[, 1]
    lnr <- iai::xgboost_survival_learner()
    iai::fit(lnr, X, deaths, times)

    expect_true(is.numeric(iai::score(lnr, X, deaths, times)))
    expect_true(is.vector(iai::predict(lnr, X, t = 1)))

    curves <- iai::predict(lnr, X)
    expect_equal(class(curves[[1]]), c(
        "survival_curve",
        "IAIObject",
        "JuliaObject"
    ))
    probs <- iai::predict(lnr, X, t = 1)
    expect_equal(class(probs[1]), c("numeric"))

    expect_equal(class(lnr), c(
        "xgboost_survival_learner",
        "xgboost_learner",
        "survival_learner",
        "supervised_learner",
        "learner",
        "IAIObject",
        "JuliaObject"
    ))
  }
})


test_that("xgboost monotone constraints", {
  skip_on_cran()

  if (!iai:::iai_version_less_than("3.1.0")) {
    X <- iris[, 1:4]
    y <- iris[, 5]
    lnr <- iai::xgboost_classifier()
    iai::fit(lnr, X, y)

    JuliaCall::julia_assign("lnr", lnr)
    expect_true(JuliaCall::julia_eval(
        "IAI.Heuristics.make_monotone_constraints(lnr) == zeros(4)"))

    for (feature_set_pair in list(
      list(R = c(1, 1), Julia = "[1, 0, 0, 0]"),
      list(R = list(3, "increasing"), Julia = "[0, 0, 1, 0]"),
      list(R = c(1, -1), Julia = "[-1, 0, 0, 0]"),
      list(R = list(3, "decreasing"), Julia = "[0, 0, -1, 0]"),
      list(R = list(Sepal_Length = 1), Julia = "[1, 0, 0, 0]"),
      list(R = list("Sepal_Length", 1), Julia = "[1, 0, 0, 0]"),

      list(R = list(list(Not = "Sepal_Length"), 1), Julia = "[0, 1, 1, 1]"),

      list(R = list(list("Sepal_Length", 1), c(2, -1)), Julia = "[1, -1, 0, 0]")
    )) {
      feature_set <- feature_set_pair$R
      feature_set_julia <- feature_set_pair$Julia

      iai::set_params(lnr, monotone_constraints = feature_set)
      expect_true(JuliaCall::julia_eval(
        paste0("IAI.Heuristics.make_monotone_constraints(lnr) == ",
               feature_set_julia)))
    }
  }
})


test_that("glmnetcv classification", {
  skip_on_cran()

  if (iai:::iai_version_less_than("3.0.0")) {
    expect_error(iai::glmnetcv_classifier(), "requires IAI version 3.0.0")
  } else {
    X <- iris[, 1:4]
    y <- iris[, 5] == "setosa"
    lnr <- iai::glmnetcv_classifier()
    iai::fit(lnr, X, y)

    expect_true(is.numeric(iai::score(lnr, X, y)))
    expect_true(is.vector(iai::predict(lnr, X)))

    s <- iai::score(lnr, X, y)
    scores <- sapply(1:iai::get_num_fits(lnr), function(i) {
      iai::score(lnr, X, y, fit_index = i)
    })
    best_index <- which(scores == s)

    expect_equal(iai::predict(lnr, X),
                 iai::predict(lnr, X, fit_index = best_index))
    expect_equal(iai::get_prediction_constant(lnr),
                 iai::get_prediction_constant(lnr, fit_index = best_index))
    expect_equal(iai::get_prediction_weights(lnr),
                 iai::get_prediction_weights(lnr, fit_index = best_index))

    expect_equal(class(lnr), c(
        "glmnetcv_classifier",
        "glmnetcv_learner",
        "glmnet_learner",
        "classification_learner",
        "supervised_learner",
        "learner",
        "IAIObject",
        "JuliaObject"
    ))

    curve_best <- iai::roc_curve(lnr, X, y)
    expect_equal(class(curve_best), c(
        "roc_curve",
        "IAIObject",
        "JuliaObject"
    ))
    curve_first <- iai::roc_curve(lnr, X, y, fit_index = 1)
    expect_equal(class(curve_best), c(
        "roc_curve",
        "IAIObject",
        "JuliaObject"
    ))
    expect_false(curve_best == curve_first)

    probs_best <- iai::predict_proba(lnr, X)
    expect_true(is.data.frame(probs_best))
    probs_first <- iai::predict_proba(lnr, X, fit_index = 1)
    expect_true(is.data.frame(probs_first))
    expect_false(isTRUE(all.equal(probs_best, probs_first)))
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
    scores <- sapply(1:iai::get_num_fits(lnr), function(i) {
      iai::score(lnr, X, y, fit_index = i)
    })
    best_index <- which(scores == s)

    if (iai:::iai_version_less_than("2.3.0")) {
      expect_equal(iai::predict(lnr, X), iai::predict(lnr, X, best_index))
      expect_equal(iai::get_prediction_constant(lnr),
                   iai::get_prediction_constant(lnr, best_index))
      expect_equal(iai::get_prediction_weights(lnr),
                   iai::get_prediction_weights(lnr, best_index))
    } else {
      expect_equal(iai::predict(lnr, X),
                   iai::predict(lnr, X, fit_index = best_index))
      expect_equal(iai::get_prediction_constant(lnr),
                   iai::get_prediction_constant(lnr, fit_index = best_index))
      expect_equal(iai::get_prediction_weights(lnr),
                   iai::get_prediction_weights(lnr, fit_index = best_index))
    }

    expect_equal(class(lnr), c(
        "glmnetcv_regressor",
        "glmnetcv_learner",
        "glmnet_learner",
        "regression_learner",
        "supervised_learner",
        "learner",
        "IAIObject",
        "JuliaObject"
    ))
  }
})


test_that("glmnetcv survival", {
  skip_on_cran()

  if (iai:::iai_version_less_than("3.0.0")) {
    expect_error(iai::glmnetcv_survival_learner(), "requires IAI version 3.0.0")
  } else {
    X <- mtcars[, 2:11]
    deaths <- sample(c(TRUE, FALSE), nrow(mtcars), TRUE)
    times <- mtcars[, 1]
    lnr <- iai::glmnetcv_survival_learner()
    iai::fit(lnr, X, deaths, times)

    expect_true(is.numeric(iai::score(lnr, X, deaths, times)))
    expect_true(is.vector(iai::predict(lnr, X, t = 1)))

    s <- iai::score(lnr, X, deaths, times)
    scores <- sapply(1:iai::get_num_fits(lnr), function(i) {
      iai::score(lnr, X, deaths, times, fit_index = i)
    })
    best_index <- which(scores == s)

    curves <- iai::predict(lnr, X)
    expect_equal(class(curves[[1]]), c(
        "survival_curve",
        "IAIObject",
        "JuliaObject"
    ))
    probs <- iai::predict(lnr, X, t = 1)
    expect_equal(class(probs[1]), c("numeric"))


    expect_equal(iai::predict(lnr, X, t = 1),
                 iai::predict(lnr, X, fit_index = best_index, t = 1))
    expect_equal(iai::predict_hazard(lnr, X),
                 iai::predict_hazard(lnr, X, fit_index = best_index))
    expect_equal(iai::predict_expected_survival_time(lnr, X),
                 iai::predict_expected_survival_time(lnr, X,
                                                     fit_index = best_index))
    expect_equal(iai::get_prediction_constant(lnr),
                 iai::get_prediction_constant(lnr, fit_index = best_index))
    expect_equal(iai::get_prediction_weights(lnr),
                 iai::get_prediction_weights(lnr, fit_index = best_index))

    expect_equal(class(lnr), c(
        "glmnetcv_survival_learner",
        "glmnetcv_learner",
        "glmnet_learner",
        "survival_learner",
        "supervised_learner",
        "learner",
        "IAIObject",
        "JuliaObject"
    ))
  }
})
