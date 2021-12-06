pkg.env <- new.env(parent = emptyenv())
pkg.env$initialized <- FALSE
pkg.env$needs_restart <- FALSE

# IAI wrapper around `julia_setup` with correct environment settings
iai_run_julia_setup <- function(sysimage_path = NULL, ...) {
  if (pkg.env$needs_restart) {
    stop("Need to restart R after installing the IAI system image")
  }

  # Check if a system image replacement was queued on Windows
  replace_sysimg_file <- sysimage_replace_prefs_file()
  if (file.exists(replace_sysimg_file)) {
    lines <- readLines(replace_sysimg_file)
    sysimage_do_replace(lines[1], lines[2])
    file.remove(replace_sysimg_file)
  }

  if (!is.na(Sys.getenv("IAI_JULIA", unset = NA))) { # nocov start
    bindir <- normalizePath(dirname(Sys.getenv("IAI_JULIA")), mustWork = F)
    Sys.setenv(JULIA_HOME = bindir)

    # Add Julia bindir to path on windows so that DLLs can be found
    if (.Platform$OS.type == "windows") {
      Sys.setenv(PATH = paste(Sys.getenv("PATH"), bindir,
                              sep = .Platform$path.sep))
    }
  } # nocov end

  if (!is.na(Sys.getenv("IAI_SYSTEM_IMAGE", unset = NA))) { # nocov start
    sysimage_path <- Sys.getenv("IAI_SYSTEM_IMAGE")
  } # nocov end
  if (is.null(sysimage_path)) {
    sysimage_path <- sysimage_load_install_path()
  }
  if (!is.null(sysimage_path)) {
    sysimage_path <- normalizePath(sysimage_path, mustWork = F)
  }

  # Run julia setup with IAI init disabled to avoid polluting stdout
  # We will instead have to init it manually later
  Sys.setenv(IAI_DISABLE_INIT = T)

  if (Sys.info()["sysname"] == "Darwin") {
    # https://github.com/Non-Contradiction/JuliaCall/commit/5170fcf5c9c650412f2003092b085c4f2ac576b8
    julia_dll_file <- julia_dll_locate(...)
    cur_dir <- getwd()
    setwd(dirname(julia_dll_file))
    on.exit(setwd(cur_dir))
  }

  JuliaCall::julia_setup(sysimage_path = sysimage_path, ...)
  Sys.unsetenv("IAI_DISABLE_INIT")
}

#' Initialize Julia and the IAI package.
#'
#' This function is called automatically with default parameters the first time
#' any `iai` function is used in an R session. If custom parameters for Julia
#' setup are required, this function must be called in every R session before
#' calling other `iai` functions.
#'
#' @param ... All parameters are passed through to
#'            \href{https://www.rdocumentation.org/packages/JuliaCall/topics/julia_setup}{\code{JuliaCall::julia_setup}}
#'
#' @examples \dontrun{iai::iai_setup()}
#'
#' @export
iai_setup <- function(...) {
  iai_run_julia_setup(...)

  # Check version of IAI installed
  if (JuliaCall::julia_exists("IAISysImg")) {
    iai_version <- JuliaCall::julia_eval("IAISysImg.VERSION")
    # Run delayed IAISysImg init
    JuliaCall::julia_eval("IAISysImg.__init__()")
  } else {
    iai_version <- JuliaCall::julia_installed_package("IAI")
    if (iai_version == "nothing") { # nocov start
      stop(paste("IAI is not present in your Julia installation. Please ",
                 "follow the instructions at ",
                 "https://docs.interpretable.ai/stable/IAI-R/installation",
                 sep = ""))
    } # nocov end
    JuliaCall::julia_library("IAI")
  }
  # Shouldn't be possible to hit this
  stopifnot("IAI module not found" = JuliaCall::julia_exists("IAI"))

  # Check version of IAI is recent enough
  REQUIRED_IAI_VERSION <- "1.0.0"
  jleval <- paste("Base.thispatch(v\"", iai_version, "\")",
                  " < ",
                  "Base.thispatch(v\"", REQUIRED_IAI_VERSION, "\")",
                  sep = "")
  if (JuliaCall::julia_eval(jleval)) { # nocov start
    stop(paste("This version of the `iai` R package requires IAI version ",
               REQUIRED_IAI_VERSION, ". Version ", iai_version, " of IAI ",
               "modules is installed. Please upgrade your IAI installation or ",
               "downgrade to an older version of the R `iai` package.",
               sep = ""))
  } # nocov end
  pkg.env$iai_version <- iai_version

  # Install dependencies of IAI-R
  for (package in c("DataFrames", "CategoricalArrays")) {
    JuliaCall::julia_install_package_if_needed(package)
  }

  # Load conversion script
  if (!JuliaCall::julia_exists("IAIConvert")) {
    JuliaCall::julia_source(system.file("julia", "convert.jl", package = "iai"))
  }

  pkg.env$initialized <- TRUE

  # Display any license related warnings (added in IAIv3)
  if (!iai_version_less_than("3.0.0")) {
    jleval = "IAI.IAILicensing.validate_license_convert(1, 1)"
    messages <- JuliaCall::julia_eval(jleval)
    for (message in messages) {
      warning(message) # nocov
    }
  }

  invisible(TRUE)
}


### Define wrappers for JuliaCall functions that hit our init methods
### Outside of iai_setup, JuliaCall functions should NOT be used directly
iai_setup_auto <- function() {
  # Call iai_setup automatically if calls Julia and IAI is not initialized
  if (!pkg.env$initialized) {
    iai_setup()
  }
}
julia_assign <- function(...) {
  iai_setup_auto()
  JuliaCall::julia_assign(...)
}
julia_eval <- function(...) {
  iai_setup_auto()
  JuliaCall::julia_eval(...)
}
julia_library <- function(...) {
  iai_setup_auto()
  JuliaCall::julia_library(...)
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
    julia_assign(julia_key, value)

    if (typeof(value) == "double" && all(abs(value - round(value)) < 1e-8)) {
      # Convert integer back to int
      jleval <- stringr::str_interp("${julia_key} = Int.(${julia_key})")
      julia_eval(jleval)
    }

  }
  out
}


jl_func <- function(funcname, ...) {
  kwargs <- get_kwargs(...)
  jleval <- stringr::str_interp("${funcname}(${kwargs})")
  julia_eval(jleval)
}

jl_isa <- function(obj, typename) {
  julia_assign("_iai_arg_", obj)
  jleval <- stringr::str_interp("_iai_arg_ isa ${typename}")
  julia_eval(jleval)
}

set_obj_class <- function(obj) {
  if (jl_isa(obj, "IAI.OptimalTreeLearner")) {
    class(obj) <- c(
        "optimal_tree_learner",
        "learner",
        "iai_visualization",
        "JuliaObject"
    )
  } else if (!iai_version_less_than("1.1.0") &&
             jl_isa(obj, "IAI.OptimalFeatureSelectionLearner")) {
    class(obj) <- c(
        "optimal_feature_selection_learner",
        "learner",
        "iai_visualization",
        "JuliaObject"
    )
  } else if (jl_isa(obj, "IAI.Learner")) {
    class(obj) <- c(
        "learner",
        "iai_visualization",
        "JuliaObject"
    )
  } else if (jl_isa(obj, "IAI.GridSearch")) {
    class(obj) <- c(
        "grid_search",
        "learner",
        "iai_visualization",
        "JuliaObject"
    )
  } else if (jl_isa(obj, "IAI.ROCCurve")) {
    class(obj) <- c(
        "roc_curve",
        "iai_visualization",
        "JuliaObject"
    )
  } else if (!iai_version_less_than("2.2.0") &&
             jl_isa(obj, "IAI.SimilarityComparison")) {
    class(obj) <- c(
        "similarity_comparison",
        "iai_visualization",
        "JuliaObject"
    )
  } else if (!iai_version_less_than("2.2.0") &&
             jl_isa(obj, "IAI.StabilityAnalysis")) {
    class(obj) <- c(
        "stability_analysis",
        "iai_visualization",
        "JuliaObject"
    )
  } else if (!iai_version_less_than("1.1.0") &&
             jl_isa(obj, "IAI.AbstractVisualization")) {
    class(obj) <- c(
        "iai_visualization",
        "JuliaObject"
    )
  }

  obj
}


#' @export
`==.iai_visualization` <- function(e1, e2) {
  jl_func("isequal", e1, e2)
}


#' @export
print.iai_visualization <- function(x, ...) {
  if (to_html(x)) {
    invisible(x) # nocov
  } else {
    NextMethod()
  }
}


to_html <- function(obj) {
  out <- jl_func("IAI.to_html", obj)
  viewer <- getOption("viewer")
  if (!is.null(viewer) && !is.null(out)) { # nocov start
    html_file <- file.path(tempdir(), "index.html")
    write(out, file = html_file)
    viewer(html_file)
    TRUE
  } else { # nocov end
    FALSE
  }
}


iai_version_less_than <- function(version) {
  iai_setup_auto()  # Need to run setup so that `iai_version` is set
  jleval <- paste("Base.thispatch(v\"", pkg.env$iai_version, "\")",
                  " < ",
                  "Base.thispatch(v\"", version, "\")",
                  sep = "")
  julia_eval(jleval)
}


requires_iai_version <- function(required_iai_version, function_name,
                                 extra = "") {
  if (iai_version_less_than(required_iai_version)) {
    stop(paste("The function `", function_name, "` ", extra, " in this ",
               "version of the `iai` R package requires IAI version ",
               required_iai_version, ". Please upgrade your IAI installation ",
               "to use this function.",
               sep = ""))
  }
}
