% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iaibase.R
\name{roc_curve.classification_learner}
\alias{roc_curve.classification_learner}
\title{Construct an ROC curve using a trained classification learner on the given
data}
\usage{
\method{roc_curve}{classification_learner}(obj, X, y, ...)
}
\arguments{
\item{obj}{The learner or grid to use for prediction.}

\item{X}{The features of the data.}

\item{y}{The labels of the data.}

\item{...}{Refer to the Julia documentation for available parameters.}
}
\description{
Julia Equivalent:
\href{https://docs.interpretable.ai/v3.1.1/IAIBase/reference/#IAI.ROCCurve-Tuple\%7BClassificationLearner\%2C\%20Union\%7BDataFrames.AbstractDataFrame\%2C\%20AbstractMatrix\%7B\%3C\%3AReal\%7D\%7D\%2C\%20AbstractVector\%7D}{\code{IAI.ROCCurve}}
}
\examples{
\dontrun{iai::roc_curve(lnr, X, y)}

}
