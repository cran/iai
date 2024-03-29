% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/heuristics.R
\name{xgboost_regressor}
\alias{xgboost_regressor}
\title{Learner for training XGBoost models for regression problems}
\usage{
xgboost_regressor(...)
}
\arguments{
\item{...}{Use keyword arguments to set parameters on the resulting learner.
Refer to the Julia documentation for available parameters.}
}
\description{
Julia Equivalent:
\href{https://docs.interpretable.ai/v3.1.1/Heuristics/reference/#IAI.XGBoostRegressor}{\code{IAI.XGBoostRegressor}}
}
\section{IAI Compatibility}{

Requires IAI version 2.1 or higher.
}

\examples{
\dontrun{lnr <- iai::xgboost_regressor()}

}
