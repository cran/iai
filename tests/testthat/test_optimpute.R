context("OptImpute")


X <- iris[, 1:4]
X[1, 1] <- NA


test_that("constructors", {
  skip_on_cran()

  lnrs <- c(iai::opt_knn_imputation_learner(),
            iai::opt_svm_imputation_learner(),
            iai::opt_tree_imputation_learner(),
            iai::single_knn_imputation_learner(),
            iai::mean_imputation_learner(),
            iai::rand_imputation_learner())
  if (iai:::iai_version_less_than("3.0.0")) {
    expect_error(iai::zero_imputation_learner(normalize_X = FALSE))
  } else {
    lnrs <- c(lnrs, iai::zero_imputation_learner(normalize_X = FALSE))
  }

  for (i in seq(lnrs)) {
    # Putting learners in a vector removes their class info so add it back
    lnr <- iai:::set_obj_class(lnrs[i])

    expect_true(is.data.frame(iai::fit_transform(lnr, X)))

    if (iai:::iai_version_less_than("1.2.0")) {
      expect_error(iai::write_json("impute.json", lnr))
    } else {
      iai::write_json("impute.json", lnr)
      new_lnr <- iai::read_json("impute.json")
      file.remove("impute.json")
      expect_true(lnr == new_lnr)
    }
  }
})


test_that("impute grid", {
  skip_on_cran()

  grid <- iai::grid_search(iai::imputation_learner())
  iai::fit_cv(grid, X)
  expect_true(is.data.frame(iai::transform(grid, X)))
  expect_true(is.data.frame(iai::fit_transform_cv(grid, X)))
})


test_that("expand", {
  skip_on_cran()

  X <- data.frame(x1 = c(1, 2, 1), x2 = c(2, 1, NA),
                 x3 = as.factor(c(NA, "A", "B")))

  X_test <- data.frame(x1 = 2, x2 = NA, x3 = as.factor(NA))

  if (iai:::iai_version_less_than("3.0.0")) {
    lnr <- iai::mean_imputation_learner()
    expect_error(iai::fit_and_expand(lnr, X, type = "finite"))
    iai::fit_transform(lnr, X)
    expect_error(iai::transform_and_expand(lnr, X, type = "finite"))
  } else {
    lnr <- iai::zero_imputation_learner(normalize_X = FALSE)
    X_expanded <- iai::fit_and_expand(lnr, X, type = "finite")
    expect_equal(X_expanded$x1, c(1, 2, 1))
    expect_equal(X_expanded$x2, c(2, 1, 0))
    expect_equal(as.vector(X_expanded$x3), c("Null Level", "A", "B"))
    expect_equal(X_expanded$x1_is_missing, c(FALSE, FALSE, FALSE))
    expect_equal(X_expanded$x2_is_missing, c(FALSE, FALSE, TRUE))
    expect_equal(X_expanded$x3_is_missing, c(TRUE, FALSE, FALSE))

    X_test_expanded <- iai::transform_and_expand(lnr, X_test, type = "finite")
    expect_equal(X_test_expanded$x1, 2)
    expect_equal(X_test_expanded$x2, 0)
    expect_equal(as.vector(X_test_expanded$x3), "Null Level")
    expect_equal(X_test_expanded$x1_is_missing, FALSE)
    expect_equal(X_test_expanded$x2_is_missing, TRUE)
    expect_equal(X_test_expanded$x3_is_missing, TRUE)
  }
})


test_that("simple API is deprecated", {
  skip_on_cran()

  lifecycle::expect_deprecated(iai::impute(iris))
  lifecycle::expect_deprecated(iai::impute_cv(iris, list(method = "opt_knn")))
})

test_that("class", {
  skip_on_cran()

  expect_equal(class(iai::mean_imputation_learner()), c(
      "mean_imputation_learner",
      "imputation_learner",
      "unsupervised_learner",
      "learner",
      "IAIObject",
      "JuliaObject"
  ))
  expect_equal(class(iai::rand_imputation_learner()), c(
      "rand_imputation_learner",
      "imputation_learner",
      "unsupervised_learner",
      "learner",
      "IAIObject",
      "JuliaObject"
  ))
  expect_equal(class(iai::single_knn_imputation_learner()), c(
      "single_knn_imputation_learner",
      "imputation_learner",
      "unsupervised_learner",
      "learner",
      "IAIObject",
      "JuliaObject"
  ))
  expect_equal(class(iai::opt_knn_imputation_learner()), c(
      "opt_knn_imputation_learner",
      "imputation_learner",
      "unsupervised_learner",
      "learner",
      "IAIObject",
      "JuliaObject"
  ))
  expect_equal(class(iai::opt_svm_imputation_learner()), c(
      "opt_svm_imputation_learner",
      "imputation_learner",
      "unsupervised_learner",
      "learner",
      "IAIObject",
      "JuliaObject"
  ))
  expect_equal(class(iai::opt_tree_imputation_learner()), c(
      "opt_tree_imputation_learner",
      "imputation_learner",
      "unsupervised_learner",
      "learner",
      "IAIObject",
      "JuliaObject"
  ))

  if (iai:::iai_version_less_than("3.0.0")) {
      expect_error(iai::zero_imputation_learner(), "requires IAI version 3.0.0")
  } else {
    expect_equal(class(iai::zero_imputation_learner()), c(
        "zero_imputation_learner",
        "imputation_learner",
        "unsupervised_learner",
        "learner",
        "IAIObject",
        "JuliaObject"
    ))
  }
})
