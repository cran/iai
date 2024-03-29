% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iaibase.R
\name{fit.learner}
\alias{fit.learner}
\title{Fits a model to the training data}
\usage{
\method{fit}{learner}(obj, X, ...)
}
\arguments{
\item{obj}{The learner to fit.}

\item{X}{The features of the data.}

\item{...}{Other parameters, including zero or more target vectors as
required by the problem type. Refer to the Julia documentation for
available parameters.}
}
\description{
Julia Equivalent:
\href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.fit\%21-Tuple\%7BLearner\%7D}{\code{IAI.fit!}}
}
\examples{
\dontrun{iai::fit(lnr, X, y)}

}
