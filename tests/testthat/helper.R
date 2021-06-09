# Copied check from `skip_on_cran`
# https://github.com/r-lib/testthat/blob/aef7fe20f8f1a4ccd183a544eace0c35f281bc4b/R/skip.R#L129-L135
if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  try(JuliaCall::julia_eval("IAI.IAIBase.disable_progress_bar()"), silent = T)
  try(JuliaCall::julia_eval("IAI.OptimalTrees.disable_progress_bar()"), silent = T)
}
