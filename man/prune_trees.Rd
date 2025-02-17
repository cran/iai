% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optimaltrees.R
\name{prune_trees}
\alias{prune_trees}
\title{Use the trained trees in a learner along with the supplied validation data to
determine the best value for the `cp` parameter and then prune the trees
according to this value}
\usage{
prune_trees(lnr, ...)
}
\arguments{
\item{lnr}{The learner to prune}

\item{...}{Refer to the Julia documentation for available parameters}
}
\description{
Julia Equivalent:
\href{https://docs.interpretable.ai/v3.2.2/OptimalTrees/reference/#IAI.prune_trees!}{\code{IAI.prune_trees!}}
}
\section{IAI Compatibility}{

Requires IAI version 3.0 or higher.
}

\examples{
\dontrun{iai::prune_trees(lnr, ...)}

}
