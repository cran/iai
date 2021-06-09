context("IAIBase")


X <- iris[, 1:4]
y <- iris$Species

test_that("Split data and fitting",  {
  skip_on_cran()

  # Test numeric indexing of list returns
  split <- iai::split_data("classification", X, y, train_proportion = 0.75)
  train_X <- split[[1]][[1]]
  train_y <- split[[1]][[2]]
  test_X <- split[[2]][[1]]
  test_y <- split[[2]][[2]]

  expect_equal(nrow(train_X) + nrow(test_X), nrow(X))
  expect_equal(length(train_y) + length(test_y), length(y))

  # Test imputation split
  split <- iai::split_data("imputation", X, train_proportion = 0.75)
  train_X <- split[[1]][[1]]
  test_X <- split[[2]][[1]]
  expect_equal(nrow(train_X) + nrow(test_X), nrow(X))

  # Test prescription names
  treatments <- y
  outcomes <- X[, 1]
  split <- iai::split_data("prescription_minimize", X, treatments, outcomes)
  train_X <- split$train$X
  train_treatments <- split$train$treatments
  train_outcomes <- split$train$outcomes
  test_X <- split$test$X
  test_treatments <- split$test$treatments
  test_outcomes <- split$test$outcomes

  expect_equal(nrow(train_X) + nrow(test_X), nrow(X))
  expect_equal(length(train_treatments) + length(test_treatments),
               length(treatments))
  expect_equal(length(train_outcomes) + length(test_outcomes), length(outcomes))
})


test_that("Split mixed data",  {
  skip_on_cran()

  # Add a mixed data column (numeric + categoric)
  tmp <- 10 * X[, 4]
  tmp[1:5] <- NA
  tmp[6:10] <- "not measured"
  X$numericmixed <- iai::as.mixeddata(tmp, c("not measured"))

  # Add another mixed data column (ordinal + categoric)
  tmp2 <- c(rep("Small", 40), rep("Medium", 60), rep("Large", 50))
  tmp2[1:5] <- "not measured"
  tmp2[6:10] <- NA
  X$ordinalmixed <- iai::as.mixeddata(tmp2, c("not measured"),
                                      c("Small", "Medium", "Large"))

  # Split into derivation and testing
  split <- iai::split_data("classification", X, y, train_proportion = 0.75)
  train_X <- split[[1]][[1]]
  train_y <- split[[1]][[2]]
  test_X <- split[[2]][[1]]
  test_y <- split[[2]][[2]]

  # Check if the combined split_data outputs are the same as original
  expect_true(all(
      c(train_X$numericmixed, test_X$numericmixed) %in% X$numericmixed))
  expect_true(all(
      X$numericmixed %in% c(train_X$numericmixed, test_X$numericmixed)))

  expect_true(all(
      c(train_X$ordinalmixed, test_X$ordinalmixed) %in% X$ordinalmixed))
  expect_true(all(
      X$ordinalmixed %in% c(train_X$ordinalmixed, test_X$ordinalmixed)))

})


test_that("grid_search", {
  skip_on_cran()

  grid <- iai::grid_search(
      iai::optimal_tree_classifier(
          random_seed = 1,
          max_depth = 1,
      ),
  )
  iai::fit(grid, X, y)

  expect_equal(iai::get_best_params(grid), list(cp = 0.25))
  lifecycle::expect_deprecated(iai::get_grid_results(grid))
  expect_true(is.data.frame(iai::get_grid_result_summary(grid)))

  if (iai:::iai_version_less_than("2.2.0")) {
    expect_error(iai::get_grid_result_details(grid),
                 "requires IAI version 2.2.0")
  } else {
    d <- iai::get_grid_result_details(grid)
    expect_true(is.list(d))
    expect_true("params" %in% names(d[[1]]))
    expect_true("valid_score" %in% names(d[[1]]))
    expect_true("rank" %in% names(d[[1]]))
    expect_true("fold_results" %in% names(d[[1]]))
    f <- d[[1]]$fold_results
    expect_true(is.list(f))
    expect_true("train_score" %in% names(f[[1]]))
    expect_true("valid_score" %in% names(f[[1]]))
    expect_true("learner" %in% names(f[[1]]))
    expect_true("optimal_tree_learner" %in% class(f[[1]]$learner))
  }

  expect_true("optimal_tree_learner" %in% class(iai::get_learner(grid)))
})


test_that("roc_curve", {
  skip_on_cran()

  lnr <- iai::optimal_tree_classifier(max_depth = 0, cp = 0)
  iai::fit(lnr, X, y == "setosa")
  roc <- iai::roc_curve(lnr, X, y == "setosa")

  expect_true("iai_visualization" %in% class(roc))

  probs <- runif(10)
  y <- rbinom(10, 1, 0.5)
  positive_label <- 1

  if (iai:::iai_version_less_than("2.0.0")) {
    expect_error(iai::roc_curve(probs, y, positive_label = positive_label),
                 "requires IAI version 2.0.0")
  } else {
    # positive_label not specified
    expect_error(iai::roc_curve(probs, y), "positive_label")

    roc <- iai::roc_curve(probs, y, positive_label = positive_label)
    expect_true("iai_visualization" %in% class(roc))
  }

  if (iai:::iai_version_less_than("2.1.0")) {
    expect_error(iai::get_roc_curve_data(roc), "requires IAI version 2.1.0")
  } else {
    data <- iai::get_roc_curve_data(roc)
    expect_true("auc" %in% names(data))
    expect_true("coords" %in% names(data))
    c <- data$coords[1]
    expect_true("tpr" %in% names(c))
    expect_true("fpr" %in% names(c))
    expect_true("threshold" %in% names(c))
  }
})


test_that("policy", {
  skip_on_cran()

  if (!iai:::iai_version_less_than("2.0.0")) {
    X <- iris[, 1:4]
    rewards <- iris[, 1:3]
    lnr <- iai::optimal_tree_policy_minimizer(max_depth = 0, cp = 0)
    iai::fit(lnr, X, rewards)
  }

  if (iai:::iai_version_less_than("2.1.0")) {
    expect_error(iai::predict_treatment_rank(), "requires IAI version 2.1.0")
    expect_error(iai::predict_treatment_outcome(), "requires IAI version 2.1.0")
  } else {
    rank <- iai::predict_treatment_rank(lnr, X)
    expect_true(is.matrix(rank))
    expect_equal(nrow(rank), nrow(rewards))
    expect_equal(ncol(rank), ncol(rewards))

    outcomes <- iai::predict_treatment_outcome(lnr, X)
    expect_true(is.data.frame(outcomes))
    expect_equal(nrow(outcomes), nrow(rewards))
    expect_equal(ncol(outcomes), ncol(rewards))
  }
})


test_that("rich output", {
  skip_on_cran()

  iai::set_rich_output_param("test", "abc")
  expect_equal(iai::get_rich_output_params(), list(test = "abc"))
  iai::delete_rich_output_param("test")
  params <- iai::get_rich_output_params()
  expect_true(is.list(params) && length(params) == 0)
})


test_that("learner params", {
  skip_on_cran()

  lnr <- iai::optimal_tree_classifier(cp = 0)
  iai::set_params(lnr, max_depth = 1)
  expect_equal(iai::get_params(lnr)$max_depth, 1)

  iai::fit(lnr, X, y)

  new_lnr <- iai::clone(lnr)
  expect_true("optimal_tree_learner" %in% class(new_lnr))
  # Clone has same params
  expect_equal(iai::get_params(new_lnr)$max_depth, 1)
  # Clone is not fitted
  expect_error(iai::predict(new_lnr))
})


test_that("add_julia_processes", {
  skip_on_cran()

  iai::add_julia_processes(1)

  # Make sure process was added
  expect_equal(JuliaCall::julia_eval("Distributed.nprocs()"), 2)

  # Make sure we can fit a model
  X <- iris[, 1:4]
  y <- iris$Species
  grid <- iai::grid_search(iai::optimal_tree_classifier(max_depth = 1))
  iai::fit(grid, X, y)

  # Make sure process is still added, then remove
  expect_equal(JuliaCall::julia_eval("Distributed.nprocs()"), 2)
  JuliaCall::julia_eval("Distributed.rmprocs(Distributed.workers())")
  expect_equal(JuliaCall::julia_eval("Distributed.nprocs()"), 1)
})
