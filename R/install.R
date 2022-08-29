APPNAME <- "InterpretableAI"

julia_default_install_dir <- function() {
  file.path(path.expand(rappdirs::user_data_dir(APPNAME)), "julia")
}


julia_latest_version <- function() {
  url <- "https://julialang-s3.julialang.org/bin/versions.json"
  file <- tempfile()
  utils::download.file(url, file)
  versions <- rjson::fromJSON(file = file)

  iai_versions <- get_iai_version_info()

  max(intersect(
      names(Filter(function(v) v$stable, versions)),
      names(iai_versions)
  ))
}

julia_tgz_url <- function(version) {
  sysname <- Sys.info()["sysname"]
  sysmachine <- Sys.info()["machine"]

  arch <- ifelse(sysmachine == "arm64", "aarch64", "x64")
  short_version <- substr(version, 1, 3)

  if (sysname == "Linux") {
    os <- "linux"
    slug <- "linux-x86_64"
    ext <- "tar.gz"
  } else if (sysname == "Darwin") {
    os <- "mac"
    slug <- ifelse(sysmachine == "arm64", "macaarch64", "mac64")
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
    utils::untar(file, exdir = dest)
    subfolder <- paste("julia-", version, sep = "")
  } else if (sysname == "Darwin") {
    subfolder <- install_julia_dmg(file, dest)
  } else if (sysname == "Windows") {
    utils::unzip(file, exdir = dest)
    subfolder <- paste("julia-", version, sep = "")
  }
  dest <- file.path(dest, subfolder)

  julia_save_install_dir(dest)

  print(sprintf("Installed Julia to %s", dest))

  invisible(TRUE)
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

    appname <- list.files(mount_point, pattern = "julia*", ignore.case = TRUE)
    src_path <- file.path(mount_point, appname)
    if (!dir.exists(install_dir)) {
      dir.create(install_dir, recursive = TRUE)
    }
    file.copy(src_path, install_dir, recursive = TRUE)
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

#' @importFrom utils tail
get_latest_iai_version <- function(versions) {
  vs <- names(versions)
  tail(vs[vs != "dev"], n = 1)
}


iai_download_url <- function(iai_versions, version) {
  if (startsWith(version, "v")) {
    version <- substr(version, 2, nchar(version))
  }

  v <- iai_versions[[version]]
  if (is.null(v)) {
    available <- paste(names(iai_versions), collapse = ", ")
    stop(paste("IAI version ", version, " not available for this version of ",
               "Julia. Available versions are: ", available, sep = ""))
  }
  v
}


get_iai_version_info <- function() {
  url <- "https://docs.interpretable.ai/versions.json"
  file <- tempfile()
  utils::download.file(url, file)
  versions <- rjson::fromJSON(file = file)

  sysname <- Sys.info()["sysname"]
  sysmachine <- Sys.info()["machine"]

  os <- if (sysname == "Linux") {
    "linux"
  } else if (sysname == "Darwin") {
    ifelse(sysmachine == "arm64", "macos_aarch64", "macos")
  } else if (sysname == "Windows") {
    "win64"
  } else {
    stop("Unknown or unsupported OS") # nocov
  }
  return(versions[[os]])
}

get_iai_versions <- function(julia_version) {
  info <- get_iai_version_info()
  return(info[[julia_version]])
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
#' @param accept_license Set to \code{TRUE} to confirm that you agree to the
#'                       \href{https://docs.interpretable.ai/End_User_License_Agreement.pdf}{End User License Agreement}
#'                       and skip the interactive confirmation dialog.
#'
#' @examples \dontrun{iai::install_system_image()}
#'
#' @export
install_system_image <- function(version = "latest", replace_default = FALSE,
                                 prefix = sysimage_default_install_dir(),
                                 accept_license = FALSE) {
  if (!accept_license && !accept_license_prompt()) {
    stop("The license agreement was not accepted, aborting installation")
  }

  iai_run_julia_setup()
  julia_version <- JuliaCall::julia_eval("string(VERSION)")
  iai_versions <- get_iai_versions(julia_version)

  if (version == "latest") {
    version <- get_latest_iai_version(iai_versions)
  }

  url <- iai_download_url(iai_versions, version)

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
    version <- paste("v", version, sep = "")
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
  pkg.env$needs_restart <- TRUE
  invisible(TRUE)
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


#' @importFrom utils askYesNo
accept_license_prompt <- function() {
  if (interactive()) { # nocov start
    message(paste("In order to continue the installation process, please",
                  "review the license agreement."))
    invisible(readline(prompt = "Press [ENTER] to continue..."))

    url <- "https://docs.interpretable.ai/End_User_License_Agreement.md"
    file <- tempfile()
    tryCatch({
      utils::download.file(url, file, quiet = TRUE)
    }, error = function(err) {
      stop("Error downloading license agreement")
    })

    file.show(file)
    rm(file)

    isTRUE(askYesNo("Do you accept the license terms?", default = FALSE))

  } else { # nocov end
    message(paste("R is not running in interactive mode, so cannot show",
                  "license confirmation dialog. Please run in an interactive R",
                  "session, or pass `accept_license = TRUE` to",
                  "`install_system_image`."))
    return(FALSE)
  }
}
