# nocov start

julia_dll_locate <- function(JULIA_HOME = NULL, verbose = TRUE,
                        installJulia = FALSE,
                        install = TRUE, force = FALSE, useRCall = TRUE,
                        rebuild = FALSE, sysimage_path = NULL) {
    JULIA_HOME <- julia_locate(JULIA_HOME)

    if (is.null(JULIA_HOME)) {
        if (isTRUE(installJulia)) {
            install_julia()
            JULIA_HOME <- julia_locate(JULIA_HOME)
            if (is.null(JULIA_HOME))
                stop("Julia is not found and automatic installation failed.")
        }
        else {
            stop("Julia is not found.")
        }
    }

    .julia$bin_dir <- JULIA_HOME

    .julia$VERSION <- julia_line(c("-e", "print(VERSION)"), stdout = TRUE)

    if (newer("0.5.3", .julia$VERSION)) {
        stop(paste0("Julia version ", .julia$VERSION, " at location ", JULIA_HOME, " is found.",
                    " But the version is too old and is not supported. Please install current release julia from https://julialang.org/downloads/ to use JuliaCall"))
    }

    dll_command <- system.file("julia/libjulia.jl", package = "JuliaCall")
    .julia$dll_file <- julia_line(dll_command, stdout = TRUE)

    if (!is.character(.julia$dll_file)) {
        stop("libjulia cannot be located.")
    }

    if (!isTRUE(file.exists(.julia$dll_file))) {
        stop("libjulia located at ", .julia$dll_file, " is not a valid file.")
    }

    .julia$dll_file
}


# nocov end
