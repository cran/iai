% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/heuristics.R
\name{predict_proba.glmnetcv_classifier}
\alias{predict_proba.glmnetcv_classifier}
\title{Return the probabilities of class membership predicted by a
\code{\link{glmnetcv_classifier}} learner for each point in the features}
\usage{
\method{predict_proba}{glmnetcv_classifier}(obj, X, fit_index = NULL, ...)
}
\arguments{
\item{obj}{The learner or grid to use for prediction.}

\item{X}{The features of the data.}

\item{fit_index}{The index of the fit in the path to use for prediction,
defaulting to the best fit if not supplied.}

\item{...}{Additional arguments (unused)}
}
\description{
Julia Equivalent:
\href{https://docs.interpretable.ai/v3.1.1/Heuristics/reference/#IAI.predict_proba-Tuple\%7BGLMNetCVClassifier\%2C\%20Union\%7BDataFrames.AbstractDataFrame\%2C\%20AbstractMatrix\%7B\%3C\%3AReal\%7D\%7D\%7D}{\code{IAI.predict_proba}}
}
\section{IAI Compatibility}{

Requires IAI version 3.0 or higher.
}

\examples{
\dontrun{iai::predict_proba(lnr, X)}

}
