APPNAME <- "InterpretableAI"

julia_default_install_dir <- function() {
  file.path(path.expand(rappdirs::user_data_dir(APPNAME)), "julia")
}

sysimage_default_install_dir <- function() {
  file.path(path.expand(rappdirs::user_data_dir(APPNAME)), "sysimage")
}


get_latest_iai_version <- function() {
  url <- "https://docs.interpretable.ai/stable_version.txt"
  file <- tempfile()
  utils::download.file(url, file)
  readChar(file, 1024)
}


iai_download_url <- function(julia_version, iai_version) {
  sysname <- Sys.info()["sysname"]
  os <- if (sysname == "Linux") {
    "linux"
  } else if (sysname == "Darwin") {
    "macos"
  } else if (sysname == "Windows") {
    "win64"
  } else {
    stop("Unknown or unsupported OS")
  }

  if (startsWith(iai_version, "v")) {
    iai_version <- substr(iai_version, 2, nchar(iai_version))
  }

  url <- if (iai_version == "dev") {
    sprintf(
      "https://iai-system-images.s3.amazonaws.com/%s/julia%s/master/latest.zip",
      os, julia_version
    )
  } else {
    sprintf(
      "https://iai-system-images.s3.amazonaws.com/%s/julia%s/v%s/sys-%s-julia%s-iai%s.zip",
      os, julia_version, iai_version, os, julia_version, iai_version
    )
  }

  return(url)
}


# IMPORTANT: these functions deliberately don't use Julia to find the depot path
#            so that the prefs path can be detected without loading Julia. This
#            is needed so that we can load prefs and process anything that needs
#            to be done before initializing Julia (eg loading the sysimage path)
julia_default_depot <- function() {
  key <- if (Sys.info()["sysname"] == "Windows") {
    "USERPROFILE"
  } else {
    "HOME"
  }
  return(file.path(Sys.getenv(key), ".julia"))
}
get_prefs_dir <- function() {
  depot <- Sys.getenv("JULIA_DEPOT_PATH", unset = julia_default_depot())
  prefs <- file.path(depot, "prefs")
  dir.create(prefs, recursive = TRUE, showWarnings = FALSE)
  prefs
}


sysimage_save_install_path <- function(path) {
  # Save sysimg path so that it can be used automatically in future
  cat(path, file = file.path(get_prefs_dir(), "IAI"))
}


sysimage_replace_command_path <- function() {
  # Temporary file to save our replace command in
  file.path(get_prefs_dir(), "IAI-replacedefault")
}
sysimage_save_replace_command <- function(image_path, target_path) {
  # Save the original path of the image and the new target path so that we can
  # process the copy during the next R session before Julia is initialized
  writeLines(c(image_path, target_path), con = sysimage_replace_command_path())
}
sysimage_do_replace <- function(image_path, target_path) {
  # Copy the image to the new path
  file.remove(target_path)
  file.copy(image_path, target_path)
  message(paste("Replacing default system image at", target_path,
              "with IAI system image"))
}


#' Download and install Julia automatically.
#'
#' @param prefix The directory where Julia will be installed. Defaults to a
#'               location determined by
#'               \href{https://www.rdocumentation.org/packages/rappdirs/topics/user_data_dir}{\code{rappdirs::user_data_dir}}.
#'
#' @examples \dontrun{iai::install_julia()}
#'
#' @export
install_julia <- function(prefix = julia_default_install_dir()) {
  JuliaCall::install_julia(prefix)
}


#' Download and install the IAI system image automatically.
#'
#' @param version The version of the IAI system image to install (e.g.
#'                \code{"2.1.0"}). Defaults to \code{"latest"}, which will
#'                install the most recent release.
#' @param replace_default Whether to replace the default Julia system image with
#'                        the downloaded IAI system image. Defaults to
#'                        \code{FALSE}.
#' @param prefix The directory where the IAI system image will be installed.
#'               Defaults to a location determined by
#'               \href{https://www.rdocumentation.org/packages/rappdirs/topics/user_data_dir}{\code{rappdirs::user_data_dir}}.
#'
#' @examples \dontrun{iai::install_system_image()}
#'
#' @export
install_system_image <- function(version = "latest", replace_default = F,
                                 prefix = sysimage_default_install_dir()) {
  if (version == "latest") {
    version <- get_latest_iai_version()
  }

  iai_run_julia_setup()
  julia_version <- JuliaCall::julia_eval("string(VERSION)")

  url <- iai_download_url(julia_version, version)

  file <- tempfile()
  tryCatch({
    utils::download.file(url, file)
  }, error = function (err) {
    stop(paste("Error downloading IAI system image v", version, " for Julia v",
               julia_version, ". ",
               "This version may not exist, or there could be network issues.",
               sep = ""))
  })

  dest <- file.path(prefix, paste("v", version, sep = ""))
  utils::unzip(file, exdir=dest)

  sysname <- Sys.info()["sysname"]
  image_name <- if (sysname == "Linux") {
    "sys.so"
  } else if (sysname == "Darwin") {
    "sys.dylib"
  } else if (sysname == "Windows") {
    "sys.dll"
  } else {
    stop("Unknown or unsupported OS")
  }
  image_path = file.path(dest, image_name)

  sysimage_save_install_path(image_path)
  message(paste("Installed IAI system image to", dest))

  if (replace_default) {
    target_path <- JuliaCall::julia_eval("unsafe_string(Base.JLOptions().image_file)")
    # Windows can't replace the current sysimg as it is loaded into this session
    # so we save a command to run later
    if (sysname == "Windows") {
      sysimage_save_replace_command(image_path, target_path)
    } else {
      sysimage_do_replace(image_path, target_path)
    }
  }

  # Need to restart R to load with the system image before IAI can be used
  pkg.env$needs_restart = T
  return(T)
}
