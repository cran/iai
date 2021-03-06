% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rewardestimation.R
\name{fit_predict}
\alias{fit_predict}
\title{Fit a reward estimation model on features, treatments and outcomes and
return predicted counterfactual rewards for each observation, as well as the
score of the internal estimators.}
\usage{
fit_predict(lnr, X, treatments, ...)
}
\arguments{
\item{lnr}{The learner or grid to use for estimation}

\item{X}{The features of the data.}

\item{treatments}{The treatment applied to each point in the data.}

\item{...}{Additional arguments depending on the treatment and outcome types.
Refer to the Julia documentation for more information.}
}
\description{
For categorical treatments, returns the estimated reward under each
treatment observed in the data. For numeric treatments, returns the
estimated reward under each treatment candidate.
}
\details{
Julia Equivalent:
\href{https://docs.interpretable.ai/v2.2.0/RewardEstimation/reference/#IAI.fit_predict!}{\code{IAI.fit_predict!}}
}
\examples{
\dontrun{iai::fit_predict(lnr, X, treatments, outcomes)}

}
