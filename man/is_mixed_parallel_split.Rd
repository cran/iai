% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iaitrees.R
\name{is_mixed_parallel_split}
\alias{is_mixed_parallel_split}
\title{is_mixed_parallel_split}
\usage{
is_mixed_parallel_split(lnr, node_index)
}
\arguments{
\item{lnr}{The learner or grid to query.}

\item{node_index}{The node in the tree to query.}
}
\description{
Check if a node of a tree applies a mixed parallel/categoric split
}
\details{
Julia Equivalent:
\href{https://docs.interpretable.ai/IAITrees/stable/reference/#IAI.is_mixed_parallel_split}{\code{IAI.is_mixed_parallel_split}}
}
\examples{
\dontrun{iai::is_mixed_parallel_split(lnr, 1)}

}