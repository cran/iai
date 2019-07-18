context("IAIBase")


X <- iris[, 1:4]
y <- iris$Species

test_that("Split data and fitting",  {
  skip_on_cran()
  iai::iai_setup()

  test_split <- iai::split_data("classification", X, y, train_proportion = 0.75)
  deriv_X <- test_split[[1]][[1]]
  deriv_y <- test_split[[1]][[2]]
  test_X <- test_split[[2]][[1]]
  test_y <- test_split[[2]][[2]]

  expect_equal(nrow(deriv_X) + nrow(test_X), nrow(X))
  expect_equal(length(deriv_y) + length(test_y), length(y))
})


test_that("Split mixed data",  {
  skip_on_cran()
  iai::iai_setup()

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
  test_split <- iai::split_data("classification", X, y, train_proportion = 0.75)
  deriv_X <- test_split[[1]][[1]]
  deriv_y <- test_split[[1]][[2]]
  test_X <- test_split[[2]][[1]]
  test_y <- test_split[[2]][[2]]

  # Check if the combined split_data outputs are the same as original
  expect_true(all(
      c(deriv_X$numericmixed, test_X$numericmixed) %in% X$numericmixed))
  expect_true(all(
      X$numericmixed %in% c(deriv_X$numericmixed, test_X$numericmixed)))

  expect_true(all(
      c(deriv_X$ordinalmixed, test_X$ordinalmixed) %in% X$ordinalmixed))
  expect_true(all(
      X$ordinalmixed %in% c(deriv_X$ordinalmixed, test_X$ordinalmixed)))

})


test_that("grid_search", {
  skip_on_cran()
  iai::iai_setup()

  grid <- iai::grid_search(
      iai::optimal_tree_classifier(
          random_seed = 1,
          max_depth = 1,
      ),
  )
  iai::fit(grid, X, y)

  expect_equal(iai::get_best_params(grid), list(cp = 0.25))
  expect_true(is.data.frame(iai::get_grid_results(grid)))
})


test_that("rich output", {
  skip_on_cran()
  iai::iai_setup()

  iai::set_rich_output_param("test", "abc")
  expect_equal(iai::get_rich_output_params(), list(test = "abc"))
  iai::delete_rich_output_param("test")
  params <- iai::get_rich_output_params()
  expect_true(is.list(params) && length(params) == 0)
})


test_that("learner params", {
  skip_on_cran()
  iai::iai_setup()

  lnr <- iai::optimal_tree_classifier(cp = 0)
  iai::set_params(lnr, max_depth = 1)
  expect_equal(iai::get_params(lnr)$max_depth, 1)

  iai::fit(lnr, X, y)

  new_lnr = iai::clone(lnr)
  # Clone has same params
  expect_equal(iai::get_params(new_lnr)$max_depth, 1)
  # Clone is not fitted
  expect_error(iai::predict(new_lnr))
})
