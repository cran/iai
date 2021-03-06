% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optimalfeatureselection.R
\name{get_prediction_constant}
\alias{get_prediction_constant}
\title{Return the constant term in the prediction in the trained learner}
\usage{
get_prediction_constant(lnr, ...)
}
\arguments{
\item{lnr}{The learner to query.}

\item{...}{If a GLMNet learner, the index of the fit in the path to query,
defaulting to the best fit if not supplied.}
}
\description{
Julia Equivalent:
\href{https://docs.interpretable.ai/v2.2.0/OptimalFeatureSelection/reference/#IAI.get_prediction_constant}{\code{IAI.get_prediction_constant}}
}
\section{IAI Compatibility}{

Requires IAI version 1.1 or higher.
}

\examples{
\dontrun{iai::get_prediction_constant(lnr)}

}
