context("OptimalFeatureSelection")


test_that("classification", {
  skip_on_cran()

  if (iai:::iai_version_less_than("1.1.0")) {
    expect_error(iai::optimal_feature_selection_classifier(),
                 "requires IAI version 1.1.0")
  } else {
    X <- iris[, 1:4]
    y <- iris[, 5] == "setosa"
    lnr <- iai::optimal_feature_selection_classifier(
        random_seed = 1,
        sparsity = 3,
    )
    iai::fit(lnr, X, y)

    expect_equal(class(lnr), c(
        "optimal_feature_selection_classifier",
        "optimal_feature_selection_learner",
        "classification_learner",
        "supervised_learner",
        "learner",
        "IAIObject",
        "JuliaObject"
    ))

    expect_true(is.numeric(iai::score(lnr, X, y)))
    expect_true(is.vector(iai::predict(lnr, X)))
    expect_true(is.data.frame(iai::predict_proba(lnr, X)))

    expect_true(is.numeric(iai::get_prediction_constant(lnr)))

    weights <- iai::get_prediction_weights(lnr)
    expect_true(is.list(weights))
    expect_true(is.list(weights$numeric))
    expect_true(is.list(weights$categoric))
    expect_equal(length(weights$categoric), 0)

    if (iai:::iai_version_less_than("2.1.0")) {
      expect_error(iai::questionnaire(lnr), "requires IAI version 2.1.0")
      expect_error(iai::write_questionnaire("questionnaire.html", lnr),
                   "requires IAI version 2.1.0")
      expect_error(iai::show_questionnaire(lnr), "requires IAI version 2.1.0")
    } else {
      vis <- iai::questionnaire(lnr)
      expect_equal(class(vis), c(
          "questionnaire",
          "abstract_visualization",
          "IAIObject",
          "JuliaObject"
      ))

      iai::write_questionnaire("questionnaire.html", lnr)
      expect_true(file.exists("questionnaire.html"))
      file.remove("questionnaire.html")
    }




    grid <- iai::grid_search(lnr, sparsity = 1:2)
    iai::fit(grid, X, y)

    if (iai:::iai_version_less_than("2.2.0")) {
      expect_error(ggplot2::autoplot(grid, type = "wrong"),
                   "not supported by autoplot")
    } else {
      expect_error(ggplot2::autoplot(grid, type = "wrong"),
                   "`type` has to be")
    }
  }
})


test_that("regression", {
  skip_on_cran()

  if (iai:::iai_version_less_than("1.1.0")) {
    expect_error(iai::optimal_feature_selection_regressor(),
                 "requires IAI version 1.1.0")
  } else {
    X <- mtcars[, 2:11]
    y <- mtcars[, 1]
    lnr <- iai::optimal_feature_selection_regressor(
        random_seed = 1,
        sparsity = 3,
    )
    iai::fit(lnr, X, y)

    expect_equal(class(lnr), c(
        "optimal_feature_selection_regressor",
        "optimal_feature_selection_learner",
        "regression_learner",
        "supervised_learner",
        "learner",
        "IAIObject",
        "JuliaObject"
    ))

    expect_true(is.numeric(iai::score(lnr, X, y)))
    expect_true(is.vector(iai::predict(lnr, X)))

    expect_true(is.numeric(iai::get_prediction_constant(lnr)))

    weights <- iai::get_prediction_weights(lnr)
    expect_true(is.list(weights))
    expect_true(is.list(weights$numeric))
    expect_true(is.list(weights$categoric))
    expect_equal(length(weights$categoric), 0)
  }
})


test_that("coordinated", {
  skip_on_cran()

  if (iai:::iai_version_less_than("3.0.0")) {
    if (!iai:::iai_version_less_than("1.1.0")) {
      lnr <- iai::optimal_feature_selection_regressor()
      expect_error(iai::get_num_fits(lnr), "requires IAI version 3.0.0")
    }
  } else {
    X <- mtcars[, 2:11]
    y <- mtcars[, 1]
    cluster <- list(1:16, 17:32)
    lnr <- iai::optimal_feature_selection_regressor(
        random_seed = 1,
        sparsity = 3,
        coordinated_sparsity = TRUE
    )
    iai::fit(lnr, X, y, cluster_inds = cluster)
    expect_equal(iai::get_num_fits(lnr), length(cluster))

    expect_error(iai::score(lnr, X, y))
    expect_false(isTRUE(all.equal(
        iai::score(lnr, X, y, fit_index = 1),
        iai::score(lnr, X, y, fit_index = 2)
    )))

    expect_error(iai::predict(lnr, X))
    expect_false(isTRUE(all.equal(
        iai::predict(lnr, X, fit_index = 1),
        iai::predict(lnr, X, fit_index = 2)
    )))

    expect_error(iai::get_prediction_constant(lnr))
    expect_false(isTRUE(all.equal(
        iai::get_prediction_constant(lnr, fit_index = 1),
        iai::get_prediction_constant(lnr, fit_index = 2)
    )))

    expect_error(iai::get_prediction_weights(lnr))
    expect_false(isTRUE(all.equal(
        iai::get_prediction_weights(lnr, fit_index = 1),
        iai::get_prediction_weights(lnr, fit_index = 2)
    )))

    imp <- iai::variable_importance(lnr)
    if (!iai:::iai_version_less_than("3.1.0")) {
      imp2 <- iai::variable_importance(lnr, fit_index = 1)
      expect_false(isTRUE(all.equal(imp, imp2)))
    }
  }

})
