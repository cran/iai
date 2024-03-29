% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optimaltrees.R
\name{optimal_tree_policy_minimizer}
\alias{optimal_tree_policy_minimizer}
\title{Learner for training Optimal Policy Trees where the policy should aim to
minimize outcomes}
\usage{
optimal_tree_policy_minimizer(...)
}
\arguments{
\item{...}{Use keyword arguments to set parameters on the resulting learner.
Refer to the Julia documentation for available parameters.}
}
\description{
Julia Equivalent:
\href{https://docs.interpretable.ai/v3.1.1/OptimalTrees/reference/#IAI.OptimalTreePolicyMinimizer}{\code{IAI.OptimalTreePolicyMinimizer}}
}
\section{IAI Compatibility}{

Requires IAI version 2.0 or higher.
}

\examples{
\dontrun{lnr <- iai::optimal_tree_policy_minimizer()}

}
