% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optimaltrees.R
\name{optimal_tree_policy_maximizer}
\alias{optimal_tree_policy_maximizer}
\title{Learner for training Optimal Policy Trees where the policy should aim to
maximize outcomes}
\usage{
optimal_tree_policy_maximizer(...)
}
\arguments{
\item{...}{Use keyword arguments to set parameters on the resulting learner.
Refer to the Julia documentation for available parameters.}
}
\description{
Julia Equivalent:
\href{https://docs.interpretable.ai/v3.2.2/OptimalTrees/reference/#IAI.OptimalTreePolicyMaximizer}{\code{IAI.OptimalTreePolicyMaximizer}}
}
\section{IAI Compatibility}{

Requires IAI version 2.0 or higher.
}

\examples{
\dontrun{lnr <- iai::optimal_tree_policy_maximizer()}

}
