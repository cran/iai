% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/heuristics.R
\name{random_forest_survival_learner}
\alias{random_forest_survival_learner}
\title{Learner for training random forests for survival problems}
\usage{
random_forest_survival_learner(...)
}
\arguments{
\item{...}{Use keyword arguments to set parameters on the resulting learner.
Refer to the Julia documentation for available parameters.}
}
\description{
Julia Equivalent:
\href{https://docs.interpretable.ai/v3.2.2/Heuristics/reference/#IAI.RandomForestSurvivalLearner}{\code{IAI.RandomForestSurvivalLearner}}
}
\section{IAI Compatibility}{

Requires IAI version 2.2 or higher.
}

\examples{
\dontrun{lnr <- iai::random_forest_survival_learner()}

}
