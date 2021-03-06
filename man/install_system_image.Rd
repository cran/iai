% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/install.R
\name{install_system_image}
\alias{install_system_image}
\title{Download and install the IAI system image automatically.}
\usage{
install_system_image(
  version = "latest",
  replace_default = F,
  prefix = sysimage_default_install_dir()
)
}
\arguments{
\item{version}{The version of the IAI system image to install (e.g.
\code{"2.1.0"}). Defaults to \code{"latest"}, which will
install the most recent release.}

\item{replace_default}{Whether to replace the default Julia system image with
the downloaded IAI system image. Defaults to
\code{FALSE}.}

\item{prefix}{The directory where the IAI system image will be installed.
Defaults to a location determined by
\href{https://rdrr.io/cran/rappdirs/man/user_data_dir.html}{\code{rappdirs::user_data_dir}}.}
}
\description{
Download and install the IAI system image automatically.
}
\examples{
\dontrun{iai::install_system_image()}

}
