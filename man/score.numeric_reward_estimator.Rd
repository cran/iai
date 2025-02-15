% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rewardestimation.R
\name{score.numeric_reward_estimator}
\alias{score.numeric_reward_estimator}
\title{Calculate the scores for a numeric reward estimator on the given data}
\usage{
\method{score}{numeric_reward_estimator}(obj, X, ...)
}
\arguments{
\item{obj}{The learner or grid to evaluate.}

\item{X}{The features of the data.}

\item{...}{Other parameters, including zero or more target vectors as
required by the problem type. Refer to the Julia
documentation for other available parameters.}
}
\description{
Julia Equivalent:
\href{https://docs.interpretable.ai/v3.2.2/RewardEstimation/reference/#IAI.score-Tuple\%7BNumericRewardEstimator\%7D}{\code{IAI.score}}
}
\section{IAI Compatibility}{

Requires IAI version 2.1 or higher.
}

\examples{
\dontrun{iai::score(lnr, X, treatments, outcomes)}

}
