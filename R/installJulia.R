# Inlined from JuliaCall: https://github.com/Non-Contradiction/JuliaCall/blob/c05473bea78a0197c639f7e82ab1c6f2e943e1cc/R/installJulia.R

# julia_default_install_dir <- function(){
#     if (exists("R_user_dir", asNamespace("tools"))) {
#         R_user_dir <- get("R_user_dir", envir = asNamespace("tools"))
#         return(file.path(R_user_dir("JuliaCall"), "julia"))
#     }
#     dir <- if (requireNamespace("rappdirs", quietly = TRUE)) {
#         file.path(rappdirs::user_data_dir("JuliaCall"), "julia")
#     } else {
#         NULL
#     }
#     return(dir)
# }
# julia_latest_version <- function(){
#     url <- "https://julialang-s3.julialang.org/bin/versions.json"
#     file <- tempfile()
#     utils::download.file(url, file)
#     versions <- rjson::fromJSON(file=file)

#     max(names(Filter(function(v) v$stable, versions)))
# }


julia_url <- function(version){ # nocov start
    sysmachine <- Sys.info()["machine"]
    arch <- if (sysmachine %in% c("arm64", "aarch64")) {
        "aarch64"
    } else if (.Machine$sizeof.pointer == 8) {
        "x64"
    } else {
        "x86"
    }
    short_version <- paste(strsplit(version, "\\.")[[1]][1:2], collapse = ".")
    sysname <- Sys.info()["sysname"]
    if (sysname == "Linux") {
        os <- "linux"
        slug <- "linux-x86_64"
        ext <- "tar.gz"
    } else if (sysname == "Darwin") {
        os <- "mac"
        slug <- ifelse(arch == "aarch64", "macaarch64", "mac64")
        ext <- "tar.gz"
    } else if (sysname == "Windows") {
        os <- "winnt"
        slug <- "win64"
        ext <- "zip"
    } else {
        stop("Unknown or unsupported OS")
    }
    sprintf(
        "https://julialang-s3.julialang.org/bin/%s/%s/%s/julia-%s-%s.%s",
        os, arch, short_version, version, slug, ext
    )
} # nocov end
# julia_default_depot <- function(){
#     key <- if (Sys.info()["sysname"] == "Windows") {
#         "USERPROFILE"
#     } else {
#         "HOME"
#     }
#     return(file.path(Sys.getenv(key), ".julia"))
# }
julia_save_install_dir <- function(dir){
    depot <- Sys.getenv("JULIA_DEPOT_PATH", unset = julia_default_depot())
    prefs <- file.path(depot, "prefs")
    dir.create(prefs, recursive = TRUE, showWarnings = FALSE)
    cat(file.path(dir, "bin"), file = file.path(prefs, "JuliaCall"))
}
JuliaCall_install_julia <- function(version = "latest",
                          prefix = julia_default_install_dir()){ # nocov start
    if (is.null(prefix)) {
        stop("rappdirs is not installed and prefix was not provided")
    }
    # if (version == "latest") {
    #     version <- julia_latest_version()
    # }
    url <- julia_url(version)
    file <- tempfile()
    tryCatch({
        utils::download.file(url, file)
    }, error = function(err) {
        stop(paste("There was an error downloading Julia. This could be due ",
                   "to network issues, and might be resolved by re-running ",
                   "`install_julia`.",
                   sep = ""))
    })
    dest <- file.path(prefix, version)
    if (dir.exists(dest)) {
      unlink(dest, recursive = TRUE)
    }
    sysname <- Sys.info()["sysname"]
    if (sysname == "Linux" || sysname == "Darwin") {
      utils::untar(file, exdir = dest)
    } else if (sysname == "Windows") {
      utils::unzip(file, exdir = dest)
    }
    subfolder <- paste("julia-", version, sep="")
    dest <- file.path(dest, subfolder)
    julia_save_install_dir(dest)
    print(sprintf("Installed Julia to %s", dest))
    invisible(TRUE)
} # nocov end
