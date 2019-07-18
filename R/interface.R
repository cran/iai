#' iai_setup
#'
#' Initialize Julia and the IAI package. This needs to be done in
#' every R session before calling `iai` functions
#'
#' @usage iai_setup(...)
#'
#' @param ... All parameters are passed through to
#'            \href{https://www.rdocumentation.org/packages/JuliaCall/topics/julia_setup}{\code{JuliaCall::julia_setup}}
#'
#' @examples \dontrun{iai::iai_setup()}
#'
#' @export
iai_setup <- function(...) {
  # Run julia setup with IAI init disabled to avoid polluting stdout
  # We will instead have to init it manually later
  Sys.setenv(IAI_DISABLE_INIT = T)
  JuliaCall::julia_setup(...)
  Sys.unsetenv("IAI_DISABLE_INIT")

  # Install dependencies of IAI-R
  for (package in c("DataFrames", "CategoricalArrays")) {
    JuliaCall::julia_install_package_if_needed(package)
  }

  # Check version of IAI installed
  if (JuliaCall::julia_exists("IAISysImg")) {
    iai_version <- JuliaCall::julia_eval("IAISysImg.VERSION")
    # Run delayed IAISysImg init
    JuliaCall::julia_eval("IAISysImg.__init__()")
  } else {
    iai_version <- JuliaCall::julia_installed_package("IAI")
    if (iai_version == "nothing") {
      stop(paste("IAI is not present in your Julia installation. Please ",
                 "follow the instructions at ",
                 "https://docs.interpretable.ai/IAI-R/stable/installation",
                 sep = ""))
    }
    JuliaCall::julia_library("IAI")
  }
  if (!JuliaCall::julia_exists("IAI")) {
    # Shouldn't be possible to hit this
    stop("IAI module not found")
  }

  # Check version of IAI is recent enough
  REQUIRED_IAI_VERSION <- "0.1.0"
  jleval <- paste("Base.thispatch(v\"", iai_version, "\")",
                  " < ",
                  "Base.thispatch(v\"", REQUIRED_IAI_VERSION, "\")",
                  sep = "")
  if (JuliaCall::julia_eval(jleval)) {
    stop(paste("This version of the `iai` R package requires IAI version ",
               REQUIRED_IAI_VERSION, ". Version ", iai_version, " of IAI ",
               "modules is installed. Please upgrade your IAI installation or ",
               "downgrade to an older version of the R `iai` package.",
               sep = ""))
  }

  # Load conversion script
  if (!JuliaCall::julia_exists("IAIConvert")) {
    JuliaCall::julia_source(system.file("julia", "convert.jl", package = "iai"))
  }
}


get_kwargs <- function(...) {
  # Use list2 to handle trailing commas
  kwargs <- rlang::list2(...)

  if (length(kwargs) == 0) {
    return("")
  }

  out <- ""
  pos <- 1
  for (pos in 1:length(kwargs)) {
    key <- names(kwargs)[[pos]]
    value <- kwargs[[pos]]

    if (!is.null(key) && nchar(key) > 0) {
      # keyword arg -> add "key=" to output
      out <- paste(out, key, "=", sep = "")
    } else {
      # positional arg -> change key to position in kwargs
      key <- paste("pos_", pos, sep = "")
      pos <- pos + 1
    }
    julia_key <- paste("_iai_arg_", key, sep = "")
    out <- paste(out, julia_key, ", ", sep = "")

    # Send value to julia
    JuliaCall::julia_assign(julia_key, value)

    if (typeof(value) == "double" && all(abs(value - round(value)) < 1e-8)) {
      # Convert integer back to int
      jleval <- stringr::str_interp("${julia_key} = Int.(${julia_key})")
      JuliaCall::julia_eval(jleval)
    }

  }
  out
}


jl_func <- function(funcname, ...) {
  kwargs <- get_kwargs(...)
  jleval <- stringr::str_interp("${funcname}(${kwargs})")
  JuliaCall::julia_eval(jleval)
}
