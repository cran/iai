APPNAME <- "InterpretableAI"

julia_default_install_dir <- function() {
  file.path(path.expand(rappdirs::user_data_dir(APPNAME)), "julia")
}


julia_latest_version <- function() {
  url <- "https://julialang-s3.julialang.org/bin/versions.json"
  file <- tempfile()
  utils::download.file(url, file)
  versions <- rjson::fromJSON(file=file)
  max(names(Filter(function(v) v$stable, versions)))
}

julia_tgz_url <- function(version) {
  arch <- "x64"
  short_version <- substr(version, 1, 3)
  sysname <- Sys.info()["sysname"]
  if (sysname == "Linux") {
    os <- "linux"
    slug <- "linux-x86_64"
    ext <- "tar.gz"
  } else if (sysname == "Darwin") {
    os <- "mac"
    slug <- "mac64"
    ext <- "dmg"
  } else if (sysname == "Windows") {
    os <- "winnt"
    slug <- "win64"
    ext <- "zip"
  }

  sprintf(
    "https://julialang-s3.julialang.org/bin/%s/%s/%s/julia-%s-%s.%s",
    os, arch, short_version, version, slug, ext
  )
}


sysimage_default_install_dir <- function() {
  file.path(path.expand(rappdirs::user_data_dir(APPNAME)), "sysimage")
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

julia_path_prefs_file <- function() {
  file.path(get_prefs_dir(), "JuliaCall")
}
julia_save_install_dir <- function(dir) {
  # Save sysimg path so that it can be used automatically in future
  cat(file.path(dir, "bin"), file = julia_path_prefs_file())
}


#' Download and install Julia automatically.
#'
#' @param version The version of Julia to install (e.g. \code{"1.6.3"}).
#'                Defaults to \code{"latest"}, which will install the most
#'                recent stable release.
#' @param prefix The directory where Julia will be installed. Defaults to a
#'               location determined by
#'               \href{https://www.rdocumentation.org/packages/rappdirs/topics/user_data_dir}{\code{rappdirs::user_data_dir}}.
#'
#' @examples \dontrun{iai::install_julia()}
#'
#' @export
install_julia <- function(version = "latest",
                          prefix = julia_default_install_dir()) {
  if (version == "latest") {
    version <- julia_latest_version() # nocov
  }
  url <- julia_tgz_url(version)

  file <- tempfile()
  tryCatch({
    utils::download.file(url, file)
  }, error = function(err) { # nocov start
    stop(paste("There was an error downloading Julia. This could be due to ",
               "network issues, and might be resolved by re-running ",
               "`install_julia`.",
               sep = ""))
  }) # nocov end

  dest <- file.path(prefix, version)
  if (dir.exists(dest)) {
    unlink(dest, recursive = TRUE) # nocov
  }

  sysname <- Sys.info()["sysname"]
  if (sysname == "Linux") {
    utils::untar(file, exdir=dest)
    subfolder <- paste("julia-", version, sep="")
  } else if (sysname == "Darwin") {
    subfolder <- install_julia_dmg(file, dest)
  } else if (sysname == "Windows") {
    utils::unzip(file, exdir = dest)
    subfolder <- paste("julia-", version, sep="")
  }
  dest <- file.path(dest, subfolder)

  julia_save_install_dir(dest)

  print(sprintf("Installed Julia to %s", dest))

  return(TRUE)
}

install_julia_dmg <- function(dmg_path, install_dir) {
  mount_root <- normalizePath(".")
  mount_name <- tools::file_path_sans_ext(basename(dmg_path))
  mount_point <- file.path(mount_root, mount_name)

  umount(mount_point)

  cmd <- paste(
      'hdiutil attach "', dmg_path, '" -mountpoint "', mount_point,
      '" -mount required -quiet',
  sep = "")

  tryCatch({
    exitcode <- system(cmd)
    stopifnot(exitcode == 0)

    appname <- list.files(mount_point, pattern = "julia*", ignore.case = T)
    src_path <- file.path(mount_point, appname)
    if (!dir.exists(install_dir)) {
      dir.create(install_dir, recursive = T)
    }
    file.copy(src_path, install_dir, recursive = T)
  },
  finally = {
    umount(mount_point)
  })

  file.path(appname, "Contents", "Resources", "julia")
}
umount <- function(mount_point) {
  if (dir.exists(mount_point)) {
    system(paste('umount "', mount_point, '"', sep = ""))
  } else {
    0
  }
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
    stop("Unknown or unsupported OS") # nocov
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


sysimage_path_prefs_file <- function() {
  file.path(get_prefs_dir(), "IAI")
}
sysimage_save_install_path <- function(path) {
  # Save sysimg path so that it can be used automatically in future
  cat(path, file = sysimage_path_prefs_file())
}
sysimage_load_install_path <- function() {
  path <- sysimage_path_prefs_file()
  if (file.exists(path)) {
    readChar(path, 256)
  } else {
    NULL
  }
}


sysimage_replace_prefs_file <- function() {
  # Temporary file to save our replace command in
  file.path(get_prefs_dir(), "IAI-replacedefault")
}
sysimage_save_replace_command <- function(image_path, target_path) {
  # Save the original path of the image and the new target path so that we can
  # process the copy during the next R session before Julia is initialized
  writeLines(c(image_path, target_path), con = sysimage_replace_prefs_file())
}
sysimage_do_replace <- function(image_path, target_path) {
  # Copy the image to the new path
  file.remove(target_path)
  file.copy(image_path, target_path)
  message(paste("Replacing default system image at", target_path,
                "with IAI system image"))
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
  }, error = function(err) { # nocov start
    stop(paste("Error downloading IAI system image ", version, " for Julia v",
               julia_version, ". ",
               "This version may not exist, or there could be network ",
               "issues. It might be resolved by re-running ",
               "`install_system_image`.",
               sep = ""))
  }) # nocov end

  if (version != "dev") {
    version = paste("v", version, sep = "")
  }
  dest <- file.path(prefix, version)
  utils::unzip(file, exdir = dest)

  sysname <- Sys.info()["sysname"]
  image_name <- if (sysname == "Linux") {
    "sys.so"
  } else if (sysname == "Darwin") {
    "sys.dylib"
  } else if (sysname == "Windows") {
    "sys.dll"
  } else {
    stop("Unknown or unsupported OS") # nocov
  }
  image_path <- file.path(dest, image_name)

  sysimage_save_install_path(image_path)
  message(paste("Installed IAI system image to", dest))

  # Run init step to fix packages to right versions (in case JuliaCall installed
  # incompatible versions before IAI was added)
  # On Windows, Julia stdout won't show in RGui/RStudio, so we need to run the
  # command using `system` from R so that the output is shown
  cmd <- paste(
      '"', JuliaCall::julia_eval("Base.julia_cmd()[1]"), '" ',
      '--sysimage="', image_path, '" ',
      '-e nothing',
  sep = "")
  exitcode <- system(cmd)
  stopifnot(exitcode == 0)

  if (replace_default) {
    target_path <- file.path(
        JuliaCall::julia_eval("unsafe_string(Base.JLOptions().julia_bindir)"),
        "../lib/julia",
        image_name
    )
    # Windows can't replace the current sysimg as it is loaded into this session
    # so we save a command to run later
    if (sysname == "Windows") {
      sysimage_save_replace_command(image_path, target_path)
    } else {
      sysimage_do_replace(image_path, target_path)
    }
  }

  # Need to restart R to load with the system image before IAI can be used
  pkg.env$needs_restart <- T
  return(T)
}


#' Remove all traces of automatic Julia/IAI installation
#'
#' Removes files created by \code{\link{install_julia}} and
#' \code{\link{install_system_image}}
#'
#' @examples \dontrun{iai::cleanup_installation()}
#'
#' @export
cleanup_installation <- function() {
  files <- c(julia_path_prefs_file(), sysimage_path_prefs_file(),
             sysimage_replace_prefs_file())
  for (f in files) {
    if (file.exists(f)) {
      file.remove(f)
    }
  }

  paths <- c(julia_default_install_dir(), sysimage_default_install_dir())
  for (p in paths) {
    if (dir.exists(p)) {
      unlink(p, recursive = TRUE) # nocov
    }
  }
}
