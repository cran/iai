context("OptimalTrees")


test_that("classification json", {
  skip_on_cran()

  lnr <- JuliaCall::julia_eval("IAI.OptimalTrees.load_iris_tree()")

  iai::write_json("classification.json", lnr)
  new_lnr <- iai::read_json("classification.json")
  file.remove("classification.json")
  expect_true("optimal_tree_learner" %in% class(new_lnr))
})


test_that("regression json", {
  skip_on_cran()

  lnr <- JuliaCall::julia_eval("IAI.OptimalTrees.load_mtcars_tree()")

  iai::write_json("regression.json", lnr)
  new_lnr <- iai::read_json("regression.json")
  file.remove("regression.json")
  expect_true("optimal_tree_learner" %in% class(new_lnr))
})


test_that("survival json", {
  skip_on_cran()

  lnr <- JuliaCall::julia_eval("IAI.OptimalTrees.load_survival_tree()")

  iai::write_json("survival.json", lnr)
  new_lnr <- iai::read_json("survival.json")
  file.remove("survival.json")
  expect_true("optimal_tree_learner" %in% class(new_lnr))
})


test_that("prescription json", {
  skip_on_cran()

  for (sense in c("min", "max")) {
    jl_eval <- stringr::str_interp(
        "IAI.OptimalTrees.load_prescription_tree(:${sense})"
    )
    lnr <- JuliaCall::julia_eval(jl_eval)

    iai::write_json("prescription.json", lnr)
    new_lnr <- iai::read_json("prescription.json")
    file.remove("prescription.json")
    expect_true("optimal_tree_learner" %in% class(new_lnr))
  }
})


test_that("policy json", {
  skip_on_cran()

  if (iai:::iai_version_less_than("2.0.0")) {
    expect_error(iai::optimal_tree_policy_minimizer(),
                 "requires IAI version 2.0.0")
    expect_error(iai::optimal_tree_policy_maximizer(),
                 "requires IAI version 2.0.0")
  } else {
    for (sense in c("min", "max")) {
      jl_eval <- stringr::str_interp(
          "IAI.OptimalTrees.load_policy_tree(:${sense})"
      )
      lnr <- JuliaCall::julia_eval(jl_eval)

      iai::write_json("policy.json", lnr)
      new_lnr <- iai::read_json("policy.json")
      file.remove("policy.json")
      expect_true("optimal_tree_learner" %in% class(new_lnr))
    }
  }
})


test_that("`optimal_tree_survivor` is deprecated", {
  skip_on_cran()

  lifecycle::expect_deprecated(optimal_tree_survivor())
})


test_that("feature set inputs", {
  skip_on_cran()

  X <- mtcars[, 2:11]
  y <- mtcars[, 1]

  if (!iai:::iai_version_less_than("2.0.0")) {
    for (feature_set_pair in list(
      list(R = 1, Julia = "1"),
      list(R = c(1, 3, 4), Julia = "[1, 3, 4]"),
      list(R = "cyl", Julia = "\"cyl\""),
      list(R = c("cyl", "hp"), Julia = "[\"cyl\", \"hp\"]"),
      list(R = list(Not = 1), Julia = "Dict(:Not => 1)"),
      list(R = list(Not = c("disp", "drat")),
           Julia = "Dict(:Not => [\"disp\", \"drat\"])"),
      list(R = list(All = c()),
           Julia = "Dict(:All => nothing)"),
      list(R = list(Between = c(1, 5)),
           Julia = "Dict(:Between => [1, 5])"),
      list(R = list(Between = c("cyl", "drat")),
           Julia = "Dict(:Between => [\"cyl\", \"drat\"])"))) {

      feature_set <- feature_set_pair$R
      feature_set_julia <- feature_set_pair$Julia

      lnr <- iai::optimal_tree_regressor(
        hyperplane_config = list(sparsity = "all", feature_set = feature_set),
        split_features = feature_set,
        regression_features = feature_set,
        max_depth = 2,
        cp = 0
      )
      iai::fit(lnr, X, y)
      JuliaCall::julia_assign("lnr", lnr)
      expect_true(JuliaCall::julia_eval(
          paste0("lnr.split_features == ", feature_set_julia)))
      expect_true(JuliaCall::julia_eval(
          paste0("lnr.regression_features == ", feature_set_julia)))
      expect_true(JuliaCall::julia_eval(
          paste0("lnr.hyperplane_config[:feature_set] == ", feature_set_julia)))
    }
  }
})
