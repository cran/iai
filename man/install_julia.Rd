% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/install.R
\name{install_julia}
\alias{install_julia}
\title{Download and install Julia automatically.}
\usage{
install_julia(version = "latest", prefix = julia_default_install_dir())
}
\arguments{
\item{version}{The version of Julia to install (e.g. \code{"1.6.3"}).
Defaults to \code{"latest"}, which will install the most
recent stable release.}

\item{prefix}{The directory where Julia will be installed. Defaults to a
location determined by
\href{https://www.rdocumentation.org/packages/rappdirs/topics/user_data_dir}{\code{rappdirs::user_data_dir}}.}
}
\description{
Download and install Julia automatically.
}
\examples{
\dontrun{iai::install_julia()}

}
