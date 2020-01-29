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
  for (i in seq(lnrs)) {
    lnr <- lnrs[i]
    expect_true(is.data.frame(iai::fit_transform(lnr, X)))

    if (iai:::iai_version_less_than("1.2.0")) {
      expect_error(iai::write_json("impute.json", lnr))
    } else {
      iai::write_json("impute.json", lnr)
      new_lnr <- iai::read_json("impute.json")
      file.remove("impute.json")
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
