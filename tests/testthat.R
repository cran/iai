library(testthat)
library(iai)


reporters <- c("check")


# Configure junit if JUNIT_PATH is set
junit_path <- Sys.getenv("JUNIT_PATH", unset = NA)
if (!is.na(junit_path)) {
  options(testthat.output_file = Sys.getenv("JUNIT_PATH"))
  reporters <- c(reporters, "junit")
}


test_check("iai", reporter = reporters)
